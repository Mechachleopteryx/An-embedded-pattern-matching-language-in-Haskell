## A real-time pattern matching algorithm in Haskell over network stream

**Rtlex** (Real-time Lexical Analyzer) is a scanner over network stream (or any kind of real-time streams) that executes a monadic action whenever a pattern matches some text in the stream. It is intended to work on real-time streams, and so it is based on space- and time- efficient algorithm that does not backtrack while trying to match multiple patterns and as such, does not rely on the stream being recoverable by using something like `unget()`.

Rtlex uses (self- and mutual- as well) recursive regular expressions for specifying patterns to match, which are easier to write and better to fit in a lexical analyzer than the *context-free grammar*. Rtlex takes extended regular expressions into which we can embed Haskell variables to refer to other regular expressions, and we can also embed arbitrary (in-line) Haskell functions to be used as zero-width assertions such as look-behinds. Zero-width assertions are further extended and can also be used to convert a partially matched substring so far into another string, in the middle of a regular expression while matching is going on. Regular expressions in rtlex are specified in a *quasi quote* and thus compiled at compile-time.

Actions that will run when corresponding regular expressions are matched with an input string are ordinary Haskell functions under some user-specified monad such as `IO`. As the actions share the same monad, they can communicate with each other through the monad; for example, we can use some simple mutable references like `IORef` or we can use a transformed monad like `StateT u IO` for some user state type `u`. Actions are also given as an argument the whole matched string by the corresponding regular expressions.

## Why to analyze in real-time?

The legacy `lex` matches its patterns in such a way that:
- it tries to match patterns from top to bottom and one by one (or concurrently if it is "smart" enough),
- if it finds a pattern matches a string that it has read so far from input, it memorizes the matched pattern and the position of input, and then it repeatedly tries further patterns (including the currently matched pattern as well) for any possible longer matches, and
- if there are no more patterns that successfully match as characters are read from input, it finally declares a match with the last pattern that has been remembered to match successfully, and recovers the input to the position corresponding to the last match, to continue the next lexical analysis for the input.

For example, if the legacy `lex` has the following two rules,
```
%%
helloworld { printf("1st action\n"); }
hello      { printf("2nd action\n"); }
```
and if we give it the input, `"helloworld"`, it will not execute any action until it completes to read the whole input string, and then it executes only the first action. That is, even after it reads `"hello"` in the middle of the input string it does not execute the second action immediately, because at that moment it does not know whether the first pattern will match with further input characters or not. It does not execute the second action in preference for a longer match. What if we give it the input, `"helloworks"`? This time, the second action will be executed as expected, but will be executed only when it reads the character `'k'` which is the first clue of no possibility of matching with the first pattern. That means, depending on accompanied patterns, the action corresponding to a matched pattern may not be executed immediately.

Note that most lexers (including the [`flex`](https://en.wikipedia.org/wiki/Flex_(lexical_analyser_generator))) are not so "smart" enough to match multiple patterns concurrently, and when they get to know the first pattern does not match `"helloworks"` at the character `'k'` they simply try to match the second pattern back from the start of the input. (In fact, if they want to be "smart", they need to include actions in the regular expressions (regular expression ASTs, to be accurate) and then combine all the regular expressions across rules into a single regular expression, before starting to match them with input.)

So, to facilitate the analysis of real-time streams, rtlex features:
> - immediate matching of patterns, rather than lazy matching to find out any possible longer matches, and
> - concurrent matching of all patterns, rather than trying patterns one by one. (By concurrent, I do not mean that every pattern is matched through a separate thread, but that patterns are matched in such an interleaved way that there will be no backtracking when a pattern fails to match and then another pattern is tried.)

## An example

Here is a simple code that detects "sheer", "she", "he", and "he\*r" when given the input string, "sheerEnd".
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Parser
import Rtlex
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
```
It can match "she" and "he" while matching "sheer".
```
*Main> main
she
he
sheer
0
```

## Another example

By having `StateT (Map String Int) IO` instead of the `IO` monad, we can count occurrences of words with the State monad.
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Parser
import Rtlex
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = do  -- in the IO monad
    m <- flip execStateT Map.empty $
        -- execStateT returns the final state and discards the final value, which is () 
        -- returned from stream ().

        stream () "ha ha ho hoo hi ha"

        $$ yyLex (\s -> do  -- in the "StateT (Map String Int) IO" monad
            modify $ Map.insertWith (+) s 1  -- stores occurrences of each word in a Map
            lift $ putStrLn s)               -- and prints each word as well.

        $$ rules [
            rule [regex|ha|ho|hi|] $ \s -> yyAccept s  -- reports each word to the yyLex.
            ]

    print $ Map.toList m  -- finally prints the counts in the Map.
```
```
*Main> main
ha
ha
ho
ho
hi
ha
[("ha",3),("hi",1),("ho",2)]
```

## General form

Our lexical analyzer has the following general form.
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Parser
import Rtlex

analyzer :: m r
analyzer =
    stream r0 "string"              -- r0 :: r

    $$ yyLex yacc                   -- yacc :: a -> m b

    $$ rules [
        rule [regex|re1|] action1,  -- action :: String -> m (ActionResult r a)
        rule [regex|re2|] action2,
        ...
        rule [regex|reN|] actionN
        ]
```

- `QuasiQuotes` language extension is for specifying regular expressions within `[|regex|...|]`. And regular expressions are compiled (that is, encoded into regular expression ASTs) at compile-time. So, if there is an error in a regular expression, it will be reported at compile-time. The syntax and semantics for regular expressions are described below.

- `import Parser` imports the `[|regex|...|]` quasi quoter and the regular expression engine.

- `import Rtlex` imports: `stream`, `yyLex`, `rules`, `($$)`, `rule`, `yyReturn`, `yyAccept`, and `yyReject`.

- The `analyzer` consists of three sections, `stream`, `yyLex`, and `rules`, separated by `$$` operator. Each section is actually implemented as a coroutine that interacts with each other using yield (and resume), and the `$$` operator plays the role of binding those coroutines. The middle coroutine `yyLex` acts as a proxy between the upper and the lower coroutines, and reads each input character from the stream and passes it to `rules`. The `rules` tries to match each rule in its rules list with the given character, runs actions that correspond to successfully matched patterns, and reports the results from such actions to `yyLex` one by one. Then with each result from `rules`, `yyLex` calls the `yacc` that is given as its argument. Then, `yyLex` repeats the next cycle by reading another character and so on.

- `stream` introduces an input stream and takes two arguments, `r0` and a string. `r0` can be any user-determined value of type `r`, and will be returned as a result from `analyzer` when the end of stream is reached. String is a list of characters to be served as the stream. Instead of a string, a bytestring or anything from an instance of `Stream` class can be used (will be implemented later).

- `yyLex` takes a user-defined function called `yacc`, which has type of `a -> m b`. `yacc` is a monadic function, taking an `a` that results from actions when their corresponding patterns are matched, and returning `b` under the user-determined monad `m`. The monad `m` will be usually the `IO` monad or some of its transformed monads, but any monad will be ok. The monad result `b` is currently not used and can be anything, but is reserved for a future extension and may be possibly used to communicate with the stream to control the stream.

- `rules` introduces rules in a list. As such, each rule in the list must be separated with a comma, "`,`".

- `rule` combines a quasi-quoted regular expression and a user-defined `action` function into a rule. Each pattern of rules is matched as characters are read from the input stream, and if a pattern successfully matches a string up to the current character from the stream, the corresponding actions is called with the whole matched string. (More details about the matching algorithm are explained below.) Every `action` has type of `String -> m (ActionResult r a)`, where `ActionResult` type is defined as:
```haskell
data ActionResult r a
    = Return r  -- to finish the lexical analyzer immediately with value "r"
    | Accept a  -- to accept the current match and report value "a" to lexical analyzer
    | Reject    -- to reject the current match and try other actions
```

- As you see, there are two user-determined types involved, `r` and `a` that are already introduced above. `a` is for reportng a value to `yyLex` and thus `yacc`, and `r` is for stopping and exiting the `analyzer` on the fly with the return value of `r`. So, before reaching the end of stream, we can early exit from `analyzer` using the `yyReturn`. Also note that the `action` functions and `yacc` function run under the same shared monad `m`, which means they can interact with each other through the monad. To make it convenient to use those constructors under the monad, three short-cuts are provided as follows.
```haskell
yyReturn :: Monad m => r -> m (ActionResult r a)
yyReturn = return . Return

yyAccept :: Monad m => a -> m (ActionResult r a)
yyAccept = return . Accept

yyReject :: Monad m => m (ActionResult r a)
yyReject = return Reject
```

## Matching rules

Patterns are always matched whether or not ...
