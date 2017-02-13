-- Regular expression matching engine using Glushkov NFA with any Semiring marks
-- inspired by: http://sebfisch.github.io/haskell-regexp/
--
-- dzchoi,
-- - Dec/10/16, [v.1] wrote initially.
-- - Jan/07/17, - Marks can take on Semirings instead of Bools.
-- -            - added "()" operator for self-recursive regular expressions.
-- - Jan/08/17, added support of zero-width assertions.
-- - Jan/14/17, raised the precedence level of |?| from 3 to 8, which was a "compiles-ok" 
--              bug.
-- - Jan/23/17, extended the logic of shift for zero-width assertions to be also to 
--              convert an m into another m.
-- - Jan/24/17, - infixl 2 |+|; infixl 3 |*|, |&|, |?|
--              - changed "Reg c m" into "Reg m c", and "Re c m" into "Re m c".
-- - Feb/04/17, added comments of ideas about lookahead and lookbehind.
-- To-do        - The current implementation of shift requires the mark type "m c" be in 
--                Eq type class, which prevents the m monad from being a useful one with 
--                a side-effect such as IO and State monads. So, for now we cannot 
--                introduce the monad under which actions run when their corresponding 
--                patterns have matched with input. I can solve it with changing the 
--                Semiring class and adding toBoolean as a method to it.
--              - If we change the type of shift into shift :: m c -> Reader c (m c -> 
--                Reg m c -> (m c, Reg m c)), we can combine regular expressions in the 
--                Reader monad, and then we can deal with regular expressions with 
--                lookbehinds as a function that takes as arguments results of each 
--                lookbehind.

-- A regular expression itself is an automaton (function) that:
-- - is a partial matcher against each input character.
-- - takes as arguments a mark and an input character to match against.
-- - returns the (previously) given mark if the regex matches up to the current input 
--   character.
-- - is mainly stateful (if containing a Rep or a Seq) for keeping a given mark from 
--   outer expression in order to use it for the next matching.
-- - otherwise, is pure and has no (mutable) states inside.
-- - is different from the Brzozowski's regular expression derivate in that the 
--   Brzozowski's derivative returns another regular expression as the partial matcher 
--   that is responsible for matching the remaining input characters, whereas the 
--   Glushkov NFA returns a mark as an indicator to successful matching up to the 
--   characters read so far - under the Brzozowski's algorithm, the regular expression 
--   itself keeps changing as input characters are read in, but the regular expression in 
--   Glushkov NFA does not change its structure, only the (mutable) states inside.
-- - can be easily implemented as a consuming coroutine that yields every match (or some 
--   monadic action based on the match by using a non-trivial underlying monad for the 
--   coroutine transformer) and keeps the stateful regular expression inside the 
--   coroutine.

-- Food for thoughts about extensions:
--
-- For lookarounds (lookahead and lookbehind), consider:
-- - Trailing lookahead does not make sense in our rtlex, because rtlex recognizes an 
--   input string in real-time, and as such, it cannot delay the match determination 
--   after reading some more characters from input. For example, "ab(?=cd)" in PCRE 
--   recognizes "ab" only before "cd", and if rtlex determines and notifies a match just 
--   when reading "ab" and if rtlex later finds that it is not followed by "cd", rtlex 
--   should change its previous determination and say sorry about it to the user code 
--   depending on the determination. Instead in such a case, the user code had better 
--   separately consider, for example, three patterns, "ab", "abcd", and "ab..", and if 
--   it is notified that "ab" is matched it prepares some action but do not take the 
--   action immediately, and later when it is told that "abcd" gets actually matched it 
--   finally takes the action, but if "ab.." is matched instead, it throws away the 
--   prepared action.
-- - Lookahead in the middle of a pattern makes sense only when the lookahead can be made 
--   determined within the whole pattern, that is, without consuming more input 
--   characters after matching with the pattern is done. This is because of the same 
--   reason above. For such a "near-sighted" lookahead, we can actually use "&" operator. 
--   For example, "(?=α)β" in PCRE can be written as "α.*&β" if α is supposed to match a 
--   shorter string than β does. Note that if α can match a longer string instead, 
--   "α.*&β" will not match immediately when "β" matches.
-- - On the other hand, because we can remember past, lookbehind does make sense and can 
--   be well defined equivalently to that of PCRE, though it is not supported currently. 
--   A lookbehind will be implemented as a separate pattern from the outer pattern 
--   containing it, and will be kept matching with input characters, but assessed at its 
--   position in the outer pattern. Then the outer pattern will be implemented as a 
--   function to a regular expression taking as arguments the matching results of 
--   lookbehinds inside it.
-- - See also 
--   http://lea.verou.me/2012/05/hacking-lookahead-to-mimic-intersection-subtraction-and-negation/
--
-- For handling left-recursive expressions, see:
-- http://osa1.net/posts/2014-03-07-parsec-left-recursion.html

{-# LANGUAGE CPP #-}

module Regex where
    -- exports ( Semiring(..), (|?|), fromBoolean, Regex, Reg(..), Re(..), shift )



-- Marks can have any (but only) type of Semiring instance.
class Semiring s where
    zero  :: s
    one   :: s
    (|+|) :: s -> s -> s
    (|*|) :: s -> s -> s
    (|&|) :: s -> s -> s  -- to support the '&' operator in regular expressions
    (|&|) = (|*|)

infixl 2 |+|            -- same as (||) for booleans
infixl 3 |*|, |&|, |?|  -- same as (&&) for booleans

(|?|) :: Semiring s => Bool -> s -> s
b |?| s = if b then s else zero
    -- Bool as the first parameter because short-circuit is supported only for the first 
    -- parameter.

fromBoolean :: Semiring s => Bool -> s
fromBoolean = (|?| one)

{- For example,
instance Semiring Bool where
    zero    = False
    one     = True
    (|+|)  = (||)
    (|*|)  = (&&)
--}



-- Representation of regular expression and its Glushkov automaton at the same time.
data Reg m c = Reg { re :: !(Re m c), nullable :: !Bool, open :: !Bool }

    -- The "m c" for some monad m represents the type of marks that flow into a regular 
    -- expression and are transferred between inner regular subexpressions, and are 
    -- returned back finally.

    -- nullable: True if this regular expression accepts an empty string. If the regex is 
    --   a zero-width assertion, nullable denotes whether the assertion is satisfied. 
    --   Otherwise, once built, it does not change.

    -- open: True if the automaton has not reached a termination state. This flag denotes 
    --   the automaton has not reached a termination state (that is, a state of always 
    --   spitting out "no match") and still has the possibility of successful matching, 
    --   for further input characters without an external mark given.
    --
    --   We can do actually without it and it is used only:
    --   - for an efficiency reason to stop matching a regex unnecessarily that has 
    --     failed and has no possibility of successful matching with further input 
    --     characters without an external mark. (Even without this flag, the Glushkov 
    --     automaton will run ok and always return no match for further input 
    --     characters.)
    --   - to enable to construct recursive regular expressions by using "()" for self 
    --     recursion, or by using non-terminal symbols for mutual recursion. (However, 
    --     left recursion will lead to an infinite loop and has to be guarded by a 
    --     symbol, just as with parser combinators like Parsec.)
    --   - to help a (smart) lexical analyzer to decide whether or not to continue 
    --     matching to search for other longer matches.
    --
    --   Technically, it is set to True iff the regex contains a middle (not final) 
    --   marked (matched) symbol inside, that is, the regex has any possibility of 
    --   successful matching without any marks from the left being passed over (e.g. in 
    --   case of full (exact) matching and the expression is the outermost one) if the 
    --   expression by itself is tried matching with further input characters. If the 
    --   regex is nothing but a symbol, `open` is always False due to no middle symbols 
    --   inside. All (nested and outermost) regular expressions are closed initially.

data Re m c = Sym (c -> Bool)                -- "c", ., [abc], or [^abc]
            | Alt (Reg m c) (Reg m c)        -- α|β
            | And (Reg m c) (Reg m c)        -- α&β
            | Rep (Reg m c) (m c)            -- α* (Kleene closure)
            | Opt (Reg m c)                  -- α? (also serving as ε)
            | Seq (Reg m c) (Reg m c) (m c)  -- αβ
#ifdef ZERO_WIDTH_ASSERTION
            -- We do not need a separate constructor for the positive closure if 
            -- nullabilities are kept constant while matching, because then it equals the 
            -- Kleene closure, only with nullable = False.
            | Pcl (Reg m c) (m c)            -- α+ (positive closure)
            | Nul                            -- ε  (matching null string)
            | Fun (Reg m c) (m c -> m c)     -- zero-width assertion
#endif

shift :: (Monad m, Semiring (m c), Eq (m c)) => m c -> c -> Reg m c -> (m c, Reg m c)
    -- It runs the automaton, so that the automaton, given a mark "m c", tries matching 
    -- the given regular expression against an input character c and returns the given 
    -- (or previously given and stored within) mark if the regular expression matches the 
    -- input character c, or returns zero (no match) otherwise.

    -- A mark in a regular expression is like an old history book that is given from 
    -- outside at first and inherited as a heritage to each successfully matching symbol 
    -- in the regular expression, and finally returned back to the outside. Depending on 
    -- the type of marks, each successfully matching symbol may write his own name on the 
    -- mark. When there is no intermediate successor of the mark (due to matching 
    -- failure) it gets lost and disappears.

    -- Thus, each symbol in the regular expression gets a right to match itself against 
    -- the current input character when it owns a mark. Without a mark, it always fails 
    -- to match. If having a mark and matching successfully the current input character, 
    -- it returns some non-zero as its result mark, which is then taken into account by 
    -- shift and is passed over to the following symbol next to it in the regular 
    -- expression.

    -- The shift can be used as a State monad (value), as it is a stateful function with 
    -- Reg m c as the state.

-- We do not touch r in case of not (open || toBoolean m).
shift m _ r @Reg { open= open } | (not open && m == zero) = (zero, r)

shift m c r @Reg { re= Sym f } = (m |*| (f c |?| return c), r)
    -- A symbol matching is tried only when a previous symbol (from the left) in the 
    -- outer expression was marked, indicated by m /= zero. Since open == False always 
    -- for a symbol, this definition of shift could be located before the definition for 
    -- the case of not (open || toBoolean m).

shift m c r @Reg { re= Alt p q }
    -- We treat both arguments of a choice of regular expressions the same.
    | (mp, p) <- shift m c p
    , (mq, q) <- shift m c q
#ifdef ZERO_WIDTH_ASSERTION
    = (mp |+| mq,
       Reg { re= Alt p q, nullable= nullable p || nullable q, open= open p || open q })
#else
    = (mp |+| mq, r { re= Alt p q, open= open p || open q })
#endif
        -- open if either p or q is open, and matched if either p or q is matched

shift m c r @Reg { re= And p q }
    | (mp, p) <- shift m c p
    , (mq, q) <- shift m c q
#ifdef ZERO_WIDTH_ASSERTION
    = (mp |&| mq,
       Reg { re= And p q, nullable= nullable p && nullable q, open= open p || open q })
#else
    = (mp |&| mq, r { re= And p q, open= open p || open q })
#endif
        -- open if either p or q is open, matched only if p and q are matched at the same 
        -- time

shift m c r @Reg { re= Rep p mp }
    -- We shift a mark into the inner expression if a previous symbol (from the left) has 
    -- been marked or a final symbol in this expression has been marked. See comments in 
    -- shift for Seq for updating the open and returning a result.
    | (mp, p) <- shift (m |+| mp) c p
    = (mp, r { re= Rep p mp, open= open p || mp /= zero })
        -- If open == False, then mp was zero, so needs no matching.

#ifdef ZERO_WIDTH_ASSERTION
shift m c r @Reg { re= Pcl p mp }
    | (mp, p) <- shift (m |+| mp) c p
    = (mp, Reg { re= Pcl p mp, nullable= nullable p, open= open p || mp /= zero })
#endif

shift m c r @Reg { re= Opt p }
    -- Considering "r?" as "ε|r" and considering that open ε == False and shift m c ε 
    -- will also result in zero (no match) for whatever m and c, we can get the following 
    -- by looking up the shift for Alt. The implementation may look like the case of just 
    -- "r", but Opt always has the True nullability, which will be taken care of by an 
    -- outer Seq.
    | (mr, p) <- shift m c p
    = (mr, r { re= Opt p, open= open p })

shift m c r @Reg { re= Seq p q mp }
    -- Sequences are tricky. The given mark m is shifted to the first part p, but we also 
    -- have to shift it to the second part q at the same time if p accepts an empty 
    -- string. Additionally, if p contains a final character we have to shift its mark 
    -- into the second part, too.

    -- A nullable is not a match (trying to "fully" match an epsilon against a non-empty 
    -- string should always fail, resulting in zero). We returns non-zero only when an 
    -- expression successfully matches one or more input characters. A nullable 
    -- expression is taken into account as being skippable by its outer expression.

    -- An inner (not final) mark can come from the first part, from the second part, or 
    -- in between. No need to keep the mark from the second part, as it will be returned 
    -- soon to the outer expression and will be handled there.

    -- A final character of the first part is also a final character of the whole 
    -- expression if the second part accepts an empty string. Of course, a final 
    -- character in the second part is always a final character of the whole expression, 
    -- as well.
#ifdef ZERO_WIDTH_ASSERTION
    | (mP, p) <- shift m c p
    , (mq, q) <- shift (nullable p |?| m |+| mp) c q
    -- To support zero-width assertions, sequencing should be either left-associative, or 
    -- defined as a postfix operator like '*' with its all left expressions taken as the 
    -- single operand. And nullable p should be updated immediately, so that it can be 
    -- accessed in shifting q.
    -- The nullable is no longer a construction-time constant and gets computed at every 
    -- match (shift). So, be careful that shifting p may change its nullability.
    = (nullable q |?| mP |+| mq,
       Reg { re= Seq p q mP,
             nullable= nullable p && nullable q,
             open= open p || mP /= zero || open q })
#else
    | (mq, q) <- shift (nullable p |?| m |+| mp) c q
    , (mp, p) <- shift m c p
    = (nullable q |?| mp |+| mq,
       r { re= Seq p q mp, open= open p || mp /= zero || open q })
#endif
        -- If open == False we do not try inner expressions, as we know they will result 
        -- in zero (no match). We should try, however, if coerced to with non-zero mark.

#ifdef ZERO_WIDTH_ASSERTION
shift _ _ r @Reg { re= Nul } = (zero, r)
    -- As the ε, r matches nothing (so we return zero), and constantly remains as 
    -- nullable r = True and open r = False.

shift m c r @Reg { re= Fun p f }
    -- Sequencing with a zero-width assertion is the same as sequencing with ε, only that 
    -- the assertion is given the current, not previous, matching result of its left 
    -- expression (that is, p), and that unlike ε, it has varying nullabilities depending 
    -- on the evaluation successfulness (that is, resulting in non-zero) of its inner 
    -- function f as well as the nullability of p.
    -- We also use zero-width assertions as a converter from m c to another m c, though 
    -- not a general mapping between different types.
    | (mp, p) <- shift m c p
        -- If p is Nul, we will get mp == zero, nullable p == True, and open p == False.
    , let mm = nullable p |?| m |+| mp
    = if mm == zero then
        -- If the assertion is not reached, it acts just like ε.
        (zero, Reg { re= Fun p f, nullable= nullable p, open= open p })
        -- nullable = nullable p if the assertion is not evaluated, since "pq{f}" is just 
        -- viewed as "pq" from the perspective of "p".
      else
        -- However, once the assertion is reached, it actively participates in converting 
        -- and matching. If it evaluates to zero, it will make the whole match always 
        -- fail regardless of whatever expressions follow it.
        let mf = f mm in
        (mf, Reg { re= Fun p f, nullable= nullable p && (mf /= zero), open= open p })

{- if f is simply of type :: c -> Bool,
    | (mp, p) <- shift m c p
        -- If p is Nul, we will get mp == zero, nullable p == True, and open p == False.
    , let nullable_q = (nullable p |?| m |+| mp) == zero || f c
    = (nullable_q |?| mp,
       Reg { re= Fun p f, nullable= nullable p && nullable_q, open= open p })
--}
#endif
