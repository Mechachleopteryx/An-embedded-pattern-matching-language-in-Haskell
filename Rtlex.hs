{-# LANGUAGE FlexibleInstances, BangPatterns, LambdaCase, FunctionalDependencies #-}

module Rtlex (
    yyReturn, yyAccept, yyReject, rule, rules,
    ($$), Stream(..), stream0, stream, yyLex
    ) where

import Control.Monad.Morph (hoist, MonadTrans(..))
--import Control.Monad.Trans (lift, MonadTrans)
import Control.Monad.List (ListT(..))
--import Control.Monad.State (StateT(..), put)
import Data.List (union, intersect)
import Regex
import Coroutine  -- We need a coroutine module from scratch as the rules function below
                  -- depends on the internals of coroutine.


instance Eq c => Semiring (ListT [] c) where  -- needs FlexibleInstances
    -- We use [String] (i.e., [[c]]) rather than just String so that we can return all 
    -- possible non-duplicate matches in a list and we can also represent no match with 
    -- an empty list as well.
    -- * How about |+| choosing the longer between the two instead of returning them in a 
    --   (non-duplicate) list?
    zero    = ListT []
    one     = ListT [[]]
    as |+| bs = ListT $ union (runListT as) (runListT bs)
        -- We are not using (++) here to avoid duplicate matched strings.
        --or  = ListT .* (union `on` runListT)
    as |*| bs = ListT $ (++) <$> runListT as <*> runListT bs
    as |&| bs = ListT $ intersect (runListT as) (runListT bs)

type Regex c = Reg (ListT []) c

{- As for now, the Eq (m c) class constraint of shift in Regex.hs does not allow some 
-- useful monads for m such as IO and State. However, a redesign of the Semiring type 
-- class will mitigate the constraint and we will be able to use such a monad like:

instance Eq c => Semiring (ListT (StateT [u] []) c) where
--or Semiring (ListT (StateT [u] (ListT m)) c), to be more general
    zero  = ListT $ StateT $ const []     -- []
    one   = ListT $ do put []; return []  -- [([], [])]

    ListT (StateT aus) |+| ListT (StateT bvs) = ListT $ StateT $ \s ->
        if null $ aus s then bvs s else
        if null $ bvs s then aus s else
        do  au @(as, us) <- aus s  -- in the list monad
            bv @(bs, vs) <- bvs s
            if as == bs then
                [(as, us ++ vs)]
            else
                [au, bv]

    ListT sau |*| ListT sbv = ListT $ do  -- in the StateT [u] [] monad
        as <- sau
        bs <- sbv  -- later state dominates (??)
        return $ as ++ bs

    ListT sau |&| ListT sbv = ListT $ do  -- in the StateT [u] [] monad
        as <- sau
        bs <- sbv
        if as == bs then
            return as
        else
            lift $ return []  -- (??)

type Regex c = Reg (ListT (StateT [()] [])) c
--}



-- An action gets coupled with a regular expression by the rule function, and should have 
-- the "Action r m c a" type, which is actually a functional type that takes as the 
-- argument, a recognized string [c] from its coupled regular expression and returns a 
-- monadic action as the result. The monad of the resulting monadic action is polymorphic 
-- but will be mostly IO monad, because the IO actions can communicate with each other 
-- through IORefs.
-- * Some other way of communications between monadic actions like user state in Parsec?
type Action r m c a = [c] -> m (ActionResult r a)

-- The monadic actions of every action should return an ActionResult-typed value.
data ActionResult r a
    = Return r  -- to finish the lexical analyzer immediately with value "r"
    | Accept a  -- to accept the current match and report value "a" to lexical analyzer
    | Reject    -- to reject the current match and try other rules

-- Short-cuts
-- Note that these short-cuts are not just for the IO monad, though m will be mostly the 
-- IO monad. If m is some transformed monad of IO, these short-cuts can be used with m 
-- without need of lifting.
yyReturn :: Monad m => r -> m (ActionResult r a)
yyReturn = return . Return

yyAccept :: Monad m => a -> m (ActionResult r a)
yyAccept = return . Accept

yyReject :: Monad m => m (ActionResult r a)
yyReject = return Reject

{- For example,
import Control.Monad (when)

main :: IO Int
main =
    stream (-1) "sheerEnd"  -- main returns (-1) if stream runs out.

    $$ yyLex (const $ return ())
        -- A simple analyzer here does nothing with the resulting reports from actions 
        -- that are executed when corresponding patterns match some input.

    $$ rules [
        rule [regex|End|]   $ \s -> yyReturn 0,  -- main returns 0 if "End" is reached.
        rule [regex|sheer|] $ \s -> do putStrLn s; yyReject,
        rule [regex|she|]   $ \s -> do putStrLn s; yyReject,
        rule [regex|he|]    $ \s -> do putStrLn s; yyReject,
        rule [regex|he*r|]  $ \s -> do
            when ( s == "her" ) $
                putStrLn s
            yyReject
        ]
--}



-- We implant a regular expression in a consuming coroutine. Otherwise, we have to carry 
-- it all the way to the top level. We could also implant it in some impure state monads 
-- such as IORef, but we cannot do with the State monad because the State monad 
-- eventually requires the regular expression as an initial state.
rule :: (Eq c, Monad m) => Regex c -> Action r m c a ->
                           Consuming r m (c, Bool) ([a], Bool)
rule re action = \(c, matchOnly) ->
    let (ListT css, re') = shift one c re in  -- run the regular expression automaton

    if matchOnly then
        -- If the matchOnly flag is set, we only run the regular expression automaton and 
        -- not the corresponding action. Then we also set the result matchOnly flag when 
        -- yielding, which will cause the outer `rules` function to set all matchOnly 
        -- flags when running the other rules following this one.
        yield ([], True) >>= rule re' action

-- Thanks to the matchOnly flag, we can stop running further rules once a rule Accepts 
-- (all) the match(es) that its corresponding regular expression has recognized.

    else do  -- in the Producing monad
        results <- lift $ mapM action css  -- run the corresponding action

        -- We have here results == [] if and only if css == [], which means there were no 
        -- matches in running the regular expression automaton and so the corresponding 
        -- action was not executed. Even in such case, we have to run yield so that every 
        -- other regular expression can also have a chance for matching as is intended by 
        -- the real-time lexical analyzer.
        if null results then
            yield ([], False) >>= rule re' action

        else  -- in case the action was really invoked
            case foldr (\case
                Return r -> const $ Left r
                Accept a -> fmap $ \case Left as -> Left (a:as); Right as -> Right (a:as)
                Reject   -> fmap $ \case Right as -> Left as; lefts -> lefts
                ) (Right (Right [])) results of

            -- If any Return in results, we tie the knot using "Done r".
            Left r -> return r
            -- Note, if there are many occurences of "Left r" in results, we return only 
            -- the first occurence instead of combining those r's. (E.g., rule "b|ab" (\s 
            -- -> putStrLn s; return $ Return s) prints both "b" and "ab" when given 
            -- "ab", but will return only "b" as the result.)

            -- If any Reject in results, we collect all a's but return them as "([a], 
            -- False)".
            Right (Left as) ->
                yield (as, False) >>= rule re' action

            -- If results has only Accepts, we return all a's as "([a], True)".
            Right (Right as) ->
                yield (as, True) >>= rule re' action

-- Note that, thanks to mapM above, only actions that correspond to successfully matching 
-- regular expressions are executed. So, for example, if an action is bound with the 
-- regular expression "a*b", it is not executed until encoutering "b" from input stream.

rules :: Monad m => [Consuming r m (c, Bool) ([a], Bool)] -> Consuming r m c [a]
rules = rules' False [] [] where
    -- We cannot define rules using foldl (or foldr) because when encountering "Done r" 
    -- while running consumers, we also have to tie the knot using "Done r" without 
    -- further running remaining consumers.

    rules' !matchOnly !ys !nextConsumers [] _ =  -- in the Producing monad
        -- No more consumers to run, so we take the current input character via _.
        -- We use bang patterns here because ys and nextConsumers play the role of mere 
        -- accumulators.
        -- * We can check ys here and, if it turns out to be [], stop running further 
        --   execution of consumers by returning something instead of yielding.
        yield ys >>= rules (reverse nextConsumers)
            -- reverse is necessary as nextConsumers have been stacked reversed.

    rules' !matchOnly !ys !nextConsumers (consumer:consumers) c = do
        ps <- lift $ resume $ consumer (c, matchOnly)  -- lifts an action to Producing
        case ps of
            Done r -> return r
            Produced (as, matchOnly) consumer ->
                rules' matchOnly (ys ++ as) (consumer:nextConsumers) consumers c



-- An abstraction of a simple non-recoverable stream
class Stream s m r c | s -> r c where -- needs FunctionalDependencies
    getc :: s -> m (Either r (c, s))
    -- getc will return either r in case of an error in the stream, or (c, s) otherwise.
    -- In case s equals the Handle from the IO monad, we do not need to retain s as a 
    -- state, because the IO monad itself maintains the Handle as a stateful value.

-- stream0 wraps a Stream in a coroutine.
stream0 :: (Monad m, Stream s m r c) => s -> Producing c () (Producing c [a] m) r
stream0 s = do  -- in the "Producing c () (Producing c [a] m)" monad
    input <- lift $ lift $ getc s
    case input of
        Left r       -> return r
        Right (c, s) -> do yield c; stream0 s
        -- To-do: the result from yield c is currently ignored, but may be used to 
        -- control the Stream when needed.

-- A simple stream coroutine for lists.
stream :: Monad m => r -> [c] -> Producing c () (Producing c [a] m) r
-- We can remove the parameter r if [c] is an infinite list.
-- To-do: bytestrings as well as Strings
stream r = hoist lift . foldr (\c z -> yield c >> z) (return r)



yyLex :: Monad m => (a -> m b) -> Consuming r (Producing c [a] m) c ()
-- yyLex is a proxy between two interfaces. It communicates with a stream coroutine 
-- through outer interface and, a rules coroutine through its inner interface.
-- The f :: a -> m b is here used simply as "lift $ lift $ mapM_ f as" and f does not 
-- seem to interact with the monadic actions that come from rules, but f is actually 
-- connected with them under the same monad m and f can interact with them through, for 
-- example, IORef under the IO monad or under the "StateT u IO" monad for some user state 
-- u.
yyLex f = foreverK $ \c -> do  -- in the Producing monad
    as <- lift $ yield c  -- run the rules coroutine with character c
    lift $ lift $ mapM_ f as
    -- By using "mapM_ f as" here instead of "f as", we can hide that a rule actually 
    -- returns "[a]", not just "a", according to possibly (non-duplicate) multiple 
    -- matches that were recognized by its corresponding regular expression.
    -- Note that
    -- To-do: The monad result b is simply ignored now, but is reserved for a future use 
    -- and may be possibly used to communicate with the stream to control the stream.
    yield ()  -- read in another character from the stream coroutine
