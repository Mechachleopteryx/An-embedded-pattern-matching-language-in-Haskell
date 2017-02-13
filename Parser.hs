-- Generator of regular expressions from real Strings by using Parsec
--
-- dzchoi
-- - Dec/10/16, [v.1] wrote initially.
-- - Jan/14/17, changed the result type of parseRegex from Either ParseError (Reg Char m) 
--              to Reg Char m.
-- - Jan/17/17, fixed to handle successive occurences of "*"/"+"/"?" correctly by 
--              supplementing Parsec with `options` function.
-- - Jan/22/17, [v.1.1] implemented regex as a quasi quoter, not a function any longer 
--              and added support of zero-width assertions.
-- - Jan/23/17, - now support "[regex|${}|]" as a syntax sugar of "let x = [regex|${x}|] 
--                in x".
--              - now take "${var}" instead of "{var}", and "{fun}" instead of "{{fun}}".
--              - changed the way of parsing zero-width assertions, from having parseSeq 
--                collect terms itself to using chainl1 to distinguish between when to 
--                use Seq' and when to use Fun'.
-- - Jan/24/17, Since Reg m c specifies m as a monad, we can use "return :: c -> m c" to 
--              convert c into a Semiring. So parseRegex no longer takes fromChar of type 
--              Q Exp as an argument that does that conversion.
-- - Jan/28/17, fixed lifts and parseRegex to be able to generate a general "Reg (ListT 
--              m) Char" for any monad m, rather than for a specific instance of m.
-- - Feb/04/17, "()" now represents ε for use in some academic regular expressions.
-- - Feb/05/17, added support of some simple escape sequences.
-- To-do:       - will support left-recursive reference in regular expressions. (The 
--                problem is that (&&) is lazy only on its first argument and it is eager 
--                to evaluate its first argument even if its second argument is False.)
--              - will support capturing by grouping in paratheses, and backreferencing 
--                (as if used as a parser).

-- Note, the regular expression engine in Regex.hs deals with regular expressions of any 
-- type "Reg m c", but the regex quasi quoter exported here is more specific and 
-- generates regular expressions of type "Reg (ListT m) Char" as we are more interested 
-- in String rather than individual Char. And also note, if we apply runListT to values 
-- of "ListT m Char", we will get values of "m String".

{-# LANGUAGE CPP, TemplateHaskell, InstanceSigs #-}

module Parser ( regex ) where

import Control.Monad.List (ListT(..))
import Language.Haskell.TH (Q, Exp, ExpQ, varE, mkName)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Syntax (Lift(..))
#ifdef ZERO_WIDTH_ASSERTION
import Language.Haskell.Meta.Parse (parseExp)  -- from the haskell-src-meta package
#endif
import Text.Parsec
import Regex (Semiring(..), Reg(..), Re(..))



-- As a quasi quoter, regex generates at compile-time regular expression ASTs of type:
-- (Monad m, Semiring (ListT m Char)) => Reg (ListT m) Char, for any monad m.
regex :: QuasiQuoter
regex = QuasiQuoter {
    quoteExp  = quoteRegex,
    quotePat  = notHandled "Patterns",
    quoteType = notHandled "Types",
    quoteDec  = notHandled "Declarations"
    } where
    notHandled things =
        error $ things ++ " are not handled by the regex quasi quoter."



-- Re' is an intermediate data type that will be produced by the first phase parser, 
-- parseRegex, and then it is passed over to the second phase parsing of lifting, which 
-- will finally generate a Reg (ListT m) Char for any monad m when evaluated at 
-- compile-time.
data Re' = Sym' ExpQ            -- "c", ., [abc], or [^abc]
         | Alt' Re' Re'         -- α|β
         | And' Re' Re'         -- α&β
         | Seq' Re' Re'         -- αβ
         | Rep' Re'             -- α* (Kleene closure)
         | Opt' Re'             -- α? (also serving as ε)
         | Pcl' Re'             -- α+ (positive closure)
         | Exp' ExpQ            -- reference to a Haskell expression
         | Nul'                 -- ε  (matching null string)
#ifdef ZERO_WIDTH_ASSERTION
         | Fun' Re' ExpQ        -- zero-width assertion
#endif

-- The quoteRegex encodes an embedded regular expression given as a string into a 
-- Template Haskell expression of Q Exp type.
quoteRegex :: String -> Q Exp
quoteRegex str = [|
    let result = $(case parseRegex [|result|] str of
        -- We could equivalently use "(varE 'result)" instead of "[|result|]".
          Left  error -> fail $ show error -- turns ParseError into a compile-time error.
          Right regex -> lift regex)
    in result |]  -- result :: (Monad m, Semiring (ListT m Char)) => Reg (ListT m) Char

-- The lifts below do the second phase parsing, converting Re' to Q Exp. And the Q Exp 
-- will lead to the actual Reg (ListT m) Char for any monad m when evaluated.
instance Lift Re' where
    lift :: Re' -> Q Exp

    lift (Sym' f') =
        [| Reg { re= Sym $f', nullable= False, open= False } |]

    lift (Alt' p' q') =
        [| let p = p'; q = q' in
           Reg { re= Alt p q, nullable= nullable p || nullable q, open= False } |]

    lift (And' p' q') =
        [| let p = p'; q = q' in
           Reg { re= And p q, nullable= nullable p && nullable q, open= False } |]

    lift (Seq' p' q') =
        [| let p = p'; q = q' in
           Reg { re= Seq p q zero, nullable= nullable p && nullable q, open= False } |]

    lift (Rep' r') =
        [| Reg { re= Rep r' zero, nullable= True, open= False } |]

    lift (Opt' r') =
        [| Reg { re= Opt r', nullable= True, open= False } |]

    lift (Pcl' r') =
#ifdef ZERO_WIDTH_ASSERTION
        [| let r = r' in r { re= Pcl r zero } |]
        -- inherits the nullable and the open from r.
        -- If we used [| r { re= Pcl r zero } |] instead of above, we would have the 
        -- following error:
        -- Ambiguous type variable ‘m0’ arising from a use of ‘zero’ prevents the 
        -- constraint ‘(Semiring (m0 Char))’ from being solved.
#else
        [| let r = r' in r { re= Rep r zero } |]
        -- We do not need a separate constructor for the positive closure if 
        -- nullabilities are kept constant while matching, because then it equals the 
        -- Kleene closure, only with nullable = False.
#endif

    -- We assume expQ is an expression that will result in a "Reg m Char" when evaluated, 
    -- so we just leave it as is.
    lift (Exp' expQ) = expQ

#ifdef ZERO_WIDTH_ASSERTION
    -- The Nul regular expression is generated as a zero-width assertion.
    lift Nul' =
        [| Reg { re= Nul, nullable= True, open= False } |]

    lift (Fun' r' expQ) =
        [| let r = r' in r { re= Fun r $expQ } |]
        -- Note that the nullability of an assertion depends on the resulting truth of 
        -- the assertion as matching goes on, and is initially the same as that of the 
        -- inner re, which means the assertion itself is nullable by default. This is 
        -- because "pq{<fun>}" can be viewed as just "pq" from the side of "p".
#endif



-- The parseRegex takes the role of first phase parsing and parses an embedded regular 
-- expression into an AST of Re' type.
parseRegex :: Q Exp -> String -> Either ParseError Re'
parseRegex myself str = result where
    -- myself :: Q Exp (or TExpQ (Reg (ListT m) Char), more specifically) points to the 
    -- variable that will contain the outermost regular expression that is being parsed.

    result = parse parseAlt "" str

-- LL grammar for regular expressions:
-- Regex        = ParseAlt
-- ParseAlt     = ParseAlt "|" ParseAnd | ParseAnd              (left associative)
-- ParseAnd     = ParseAnd "&" ParseSeq | ParseSeq              (left associative)
-- ParseSeq     = ParseSeq ParseTerm | ParseTerm                (left associative)
-- ParseTerm    = <a character>
--              | "."
--              | "[" ["^"] <characters> "]"
--              | "${" [<a variable>] "}"
--              | "{" <a Haskell function> "}"
--              | "(" ParseAlt ")"
--              | ParseTerm { "?" | "*" | "+" }

-- Note:
-- . Sequencing (juxtaposition), "&", and "|" are left-associative.
-- . "&" has higher precedence than "|" as VIM does with its regular expressions.
-- . <a Haskell function> should be of type :: String -> m String, where m is the monad m 
--   from the type Reg (ListT m) Char for regular expressions.
-- . "${...}" will accept any expression as well as a variable that will evaluate to a 
--   regular expression.

    parseAlt = chainl1 parseAnd $ char '|' >> return Alt'
        --or = parseAnd `sepBy1` char '|' >>= return . (foldr1 Alt')

    parseAnd = chainl1 parseSeq $ char '&' >> return And'
        --or = parseSeq `sepBy1` char '&' >>= return . (foldr1 And')

    -- parseSeq parses sequence of terms and zero-width assertions as well. It collects 
    -- and sequences terms in the left-associative way. However, when encountering a 
    -- zero-width assertion, it generates a Fun' instead of Seq', putting the sequence of 
    -- all collected terms so far into the Fun'.
    parseSeq = chainl1 parseTerm $ return $ \p q ->
        case q of
           -- Note that chainl1 gathers terms in the left-associative way, so q can be at 
           -- most a term or a term followed by "*"/"+"/"?" operators. But as Fun' cannot 
           -- be followed by those operators, if q happens to be a Fun' the inner Re' 
           -- inside it must be Nul', which therefore we can ignore safely.
            Fun' _ expQ -> Fun' p expQ
            _           -> Seq' p q

    parseTerm =
        (   do  char '.'                                                        -- "."
                return $ Sym' [| const True |]

        <|> do  c <- do  char '\\'; escapedChar                                 -- "\c"
                     <|> noneOf "[]{}()$*+?|&"                                  -- "c"
                return $ Sym' [| (== c) |]

        <|> do  between (char '[') (char ']') $ do              -- "[abc]" and "[^abc]"
                    -- To-do: support ranges inside character classes
                    f  <- option [| elem |] $ char '^' >> return [| notElem |]
                    cs <- many1 (noneOf "]")
                    return $ Sym' [| \x -> $f x cs |]  --or [| flip $f cs |]

        <|> do  between (string "${") (char '}') $ do
                    s <- many (noneOf "}")
                    if null s then                                          -- "${}"
                        -- "${}" is the same as "(?R)" or "(?0)" in PCRE.
                        return $ Exp' myself
                    else                                                    -- "${var}"
                        either fail (return . Exp' . return) (parseExp s)
                        --or return $ Exp' $ varE (mkName s), if "${...}" is supposed to 
                        -- contain only a variable name.

#ifdef ZERO_WIDTH_ASSERTION
        <|> do  between (char '{') (char '}') $ do                          -- "{fun}"
                -- We deal with a zero-width assertion as a kind of postfix operator like 
                -- "*"/"+"/"?", only that it takes as its single argument the sequence of 
                -- all the terms on its left side. (As such, it cannot be followed by 
                -- "*"/"+"/"?".) If it appears as the first term, Nul' is supposed as its 
                -- left term.
                    s <- many1 (noneOf "}")
                    -- To-do: provide a way to escape '}' in the expression string.
                    either fail (return . Fun' Nul' . apply . return)
                        -- Nul' here will be replaced with the sequence of all the left 
                        -- terms (if any) by parseSeq.
                        (parseExp s)

              <* notFollowedBy (oneOf "*+?")
                    -- We need to make sure here that we are not followed by "*"/"+"/"?", 
                    -- because besides they do not make sense with zero-width assertions, 
                    -- they will hide the Fun' from parseSeq by wrapping it in 
                    -- Rep'/Pcl'/Opt'.
#endif

#ifdef ZERO_WIDTH_ASSERTION
        <|> do  char '('
                do  char ')'; return $ Nul'                                 -- "()"
                    -- "()" represents ε that matches an empty string.
                  <|> parseAlt <* char ')'                                  -- "(r)"
#else
        <|> do  between (char '(') (char ')') parseAlt                      -- "(r)"
#endif

        ) >>= options (\re' ->
            do  char '*'; return $ Rep' re'                                 -- "r*"
        <|> do  char '+'; return $ Pcl' re'                                 -- "r+"
        <|> do  char '?'; return $ Opt' re')                                -- "r?"
        -- To-do: support specified ranges for quantification such as "{min,max}" by 
        -- using recursive template Haskell code rather than simply copying a pattern as 
        -- many times.

#ifdef ZERO_WIDTH_ASSERTION
    apply :: Q Exp -> Q Exp
    apply f = [| ListT . (>>= $f) . runListT |]
        -- We are applying here $f monadically instead of simply fmapping it over marks m 
        -- such as "fmap $f m", because by fmapping it, we cannot return zero or non-zero 
        -- as a result of zero-width assertion, based on currently matching characters 
        -- given as its argument. By using $f monadically, we can not only use it as a 
        -- converter of one string into another, but also use it as a zero-width 
        -- assertion that determines whether or not to accept the current on-going match, 
        -- although $f should be of type String -> m String instead of String -> String.
#endif

    escapedChar = do
        -- Using Text.Parsec.Language.haskell and Text.Parsec.Token.charLiteral to deal 
        -- with it as an escape sequence from Haskell is not an option, since the 
        -- characters to be parsed should be wrapped in single quotes.
        -- See also the ReadP combinator: 
        -- https://two-wrongs.com/parser-combinators-parsing-for-haskell-beginners.
        c <- anyChar
        return $ case c of
            'b' -> '\b'
            'f' -> '\f'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            _   -> c
            -- To-do: '\a', '\v', and numeric escapes?



-- This `options` is a supplement to the Parsec library and extends `option` function to 
-- be able to apply a given parser function repeatedly.
options :: Stream s m t => (a -> ParsecT s u m a) -> a -> ParsecT s u m a
-- "options p x" tries to apply parser "p x". If "p x" fails without consuming input, 
-- it returns the value x, otherwise the value returned from it is recursively fed to 
-- "p" as a new paramter "x". (If "p x" fails after consuming some input, the whole 
-- "options p x" also fails.)
options p x = (p x >>= options p) <|> return x
