-- Compiler option "-DZERO_WIDTH_ASSERTION" is needed if regular expressions contain 
-- "{fun}" or "()".

{-# LANGUAGE QuasiQuotes, FlexibleInstances, MultiParamTypeClasses #-}

import Parser
import Rtlex

{- Ex0-0. Template
{-# LANGUAGE QuasiQuotes #-}

import Parser
import Rtlex

f :: m r
f =
    stream r0 "string"              -- r0 :: r

    $$ yyLex yacc                   -- yacc :: a -> m b

    $$ rules [
        rule [regex|re1|] action1,  -- action :: String -> m (ActionResult r a)
        rule [regex|re2|] action2,
        ...,
        rule [regex|reN|] actionN
        ]
--}

{- Ex0-1. First example
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
-- *Main> main
-- she
-- he
-- sheer
-- 0
--}

{- Ex0-2. Counting for words
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = do  -- in the IO monad
    m <- flip execStateT Map.empty $
        -- execStateT returns the final state and discards the final value, which is () 
        -- returned from stream ().

        stream () "ha ha ho hoo hi ha"

        $$ yyLex (\s -> do  -- in the "StateT (Map String Int) IO" monad
            modify' $ Map.insertWith (+) s 1  -- stores occurrences of each word in a Map
            lift $ putStrLn s)                -- and prints the word as well.

        $$ rules [
            rule [regex|ha|ho|hi|] $ \s -> yyAccept s  -- reports each word to the yyLex.
            ]

    print $ Map.toList m  -- finally prints the counts in the Map.
-- *Main> main
-- ha
-- ha
-- ho
-- ho
-- hi
-- ha
-- [("ha",3),("hi",1),("ho",2)]
--}

{- Ex1-1. Filter
-- Selecting only "she" from a stream
main :: IO ()
main =
    stream () "shheeshehe"  -- return () on out of stream
    $$ yyLex (const $ return ())  -- do nothing with the reported "()"s from rules
    $$ rules [
        rule [regex|she|] $ \s -> do putStrLn s; yyAccept ()
    ]
-- Output will be: she
--}

{- Ex1-2. Filter out
-- Filtering out "she" from a stream
main :: IO ()
main =
    stream () "shheeshehe"  -- return () on out of stream
    $$ yyLex (const $ return ())  -- do nothing with the reported "()"s from rules
    $$ rules [
        rule [regex|she|] $ \_ -> do putStr "!"; yyAccept (),
            -- It prints "!" when encountering "she" and does not allow to run other 
            -- matched actions by using yyAccept.
        rule [regex|.|]   $ \s -> do putStr s; yyAccept ()
            -- It prints any character matched by ".".
    ]
-- Output will be: shheesh!he
-- Note that the prefix "sh" of "she" is passed to the second rule, as all rules are 
-- "simultaneously" tried to match against each input character and the first rule 
-- matches successfully after reading the whole "she".
--}

{- Ex2.
-- Detecting the start and the end of "a" in successive "a"s.
main :: IO ()
main =
    stream () "aaaaaabbbaaabb"  -- return () on out of stream
    $$ yyLex (const $ return ())  -- do nothing with the reported "()"s from rules
    $$ rules [
        -- repeated a's will be discarded
        rule [regex|aa|]        $ \_ -> yyAccept (),
            -- If it were yyReject instead of yyAccept, the following rule would also 
            -- match and run.

        -- catch the first a
        rule [regex|a|]         $ \s -> do putStr s; yyAccept (),

        -- catch the last a
        rule [regex|a[^a]|]     $ \_ -> do putStr "+"; yyReject,

        -- catch others
        rule [regex|.|]         $ \s -> do putStr s; yyAccept ()
    ]
-- Output will be: a+bbba+bb
--}

{- Ex3-1. recursive regular expressions
-- Matching the regular expression, "x = (a x b)?", i.e. {a^n b^n | n >= 0}.
main :: IO ()
main =
    stream () "aaaaaabbbaaabb"  -- return () on out of stream
    $$ yyLex (const $ return ())  -- do nothing with the reported "()"s from rules
    $$ rules [
        rule [regex|(a${}b)?|] $ \s -> do putStrLn s; yyAccept ()
        --or we could also use: rule (let x = [regex|(a${x}b)?|] in x) $ \s -> ...
    ]
-- *Main> main
-- ab
-- aabb
-- aaabbb
-- ab
-- aabb
--}

{- Ex3-2. recursive regular expressions
-- recognizes {a^n b^n c^n | n >= 1}
main :: IO ()
main =
    stream () "aaaabbbcccc"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|${abc nul}|] $ \s -> do putStrLn s; yyAccept ()
        --rule [regex|${let x = abc nul in x}|] $ \s -> do putStrLn s; yyAccept ()
    ]

    where
    nul    = [regex|()|]
    abc bc = let bc' = [regex|b${bc}c|] in
             [regex|\a(${bc'}|${abc bc'})|]
    -- To avoid left-recursion, we have used:
    -- abc | aabbcc | aaabbbccc | ... == a(bc | a(bbcc | a(bbbccc | a(...)))).
-- *Main> main
-- aaabbbccc
--}

{- Ex4-1. Zero-width assertion
main :: IO ()
main =
    stream () "A is A.\nB is B.\nC is C.\n"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|.{\s -> if s == "\n" then [""] else []}B|] $
            \s -> do putStrLn s; yyAccept ()
    ]
-- *Main> main
-- B
--}

{- Ex4-2. Substitution
main :: IO ()
main =  -- finds ".he" only if it is "she", and converts it into "SHE".
    stream () "he she and they"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|.he{\s -> if s == "she" then ["SHE"] else []}|] $
            \s -> do putStrLn s; yyAccept ()
    ]
-- *Main> main
-- SHE
--}

{- Ex4-3. Substitution
-- Converting every "he" into "she"
main :: IO ()
main =
    stream () "eheeshehe"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|she|]               $ \s -> do putStr s; yyAccept (),
        rule [regex|he{\_ -> ["SHE"]}|] $ \s -> do putStr s; yyAccept ()
        -- ignore anything else
    ]
-- *Main> main
-- SHEsheSHE
--}

import Control.Monad.State
import qualified Data.Map as Map
import Network
import System.IO
import System.IO.Error

-- Handle from IO monad as a Stream instance
instance MonadIO m => Stream Handle m Int Char where
    -- needs FlexibleInstances, MultiParamTypeClasses
    getc handle = liftIO $
        catchIOError
        (do c <- hGetChar handle; return $ Right (c, handle))
        (\e -> return $ Left $ if isEOFError e then 0 else -1)
        -- r == 0 if EOFError, -1 for other errors

main :: IO ()
main = withSocketsDo $ do  -- in the IO monad
    sock <- listenOn $ PortNumber 3001
    putStrLn "Starting server ..."
    (handle, host, port) <- accept sock

    m <- flip execStateT Map.empty $
         stream0 handle
         $$ yyLex (\s -> do  -- in the "StateT (Map String Int) IO" monad
            modify $ Map.insertWith (+) s 1  -- stores occurrences of each word in a Map
            lift $ putStrLn s)               -- and prints each word as well.
         $$ rules [
            rule [regex|ha|ho|hi|] $ \s -> yyAccept s  -- reports each word to the yyLex.
            ]

    hClose handle
    print $ Map.toList m  -- finally prints the counts in the Map.
    putStrLn "Server closed."
