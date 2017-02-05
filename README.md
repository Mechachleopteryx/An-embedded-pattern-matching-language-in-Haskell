# A real-time pattern matching algorithm in Haskell over network stream

**Rtlex** (Real-time [Lexical Analyzer](https://en.wikibooks.org/wiki/Compiler_Construction/Lexical_analysis)) is a scanner over network stream (or any kind of real-time streams) that executes a monadic action whenever a pattern matches some text in the stream. It is intended to work on real-time streams, and so it is based on space- and time- efficient algorithm that does not backtrack while trying to match multiple patterns and as such, does not rely on the stream being recoverable by using something like [`unget()`](http://www.cplusplus.com/reference/istream/istream/unget/).

Rtlex uses (self- and mutual- as well) [recursive regular expressions](http://www.regular-expressions.info/recurse.html) for specifying patterns to match, which are easier to write and better to fit in a lexical analyzer than the [*context-free grammar*](https://en.wikipedia.org/wiki/Context-free_grammar). Rtlex takes extended regular expressions into which we can embed Haskell variables and arbitrary (in-line) Haskell functions. Embedded variables can refer to other regular expressions including the regular expression containing them, and embedded Haskell functions can be used as [zero-width assertions](http://www.regular-expressions.info/lookaround.html) such as lookbehinds. Zero-width assertions are further extended and can also be used to convert a partially matched substring so far into another string, in the middle of a regular expression while is currently being matched. Regular expressions in rtlex are specified in a [*quasi quote*](https://wiki.haskell.org/Quasiquotation) and thus compiled at compile-time.

Actions that will run when corresponding regular expressions are matched with an input string are ordinary Haskell functions under some user-specified monad such as `IO`. As the actions share the same monad, they can communicate with each other through the monad; for example, we can use some simple mutable references like [`IORef`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-IORef.html) or we can use a transformed monad like [`StateT u IO`](https://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Monad-Trans-State-Lazy.html) for some user state type `u`. Actions are also given as an argument the whole matched string by the corresponding regular expressions.

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
and if we give it the input, `"helloworld"`, it will not execute any action until it completes to read the whole input string, and then it executes only the first action. That is, even after it reads `"hello"` in the middle of the input string it does not execute the second action immediately, because at that moment it does not know whether the first pattern will match with further input characters or not. It does not execute the second action in preference for a longer match. What if we give it the input, `"helloworks"`? This time, the second action will be executed as expected, but will be executed only when it reads the character `'k'` which is the first clue of no success of matching with the first pattern. That means, depending on other patterns, the action corresponding to a matched pattern may not be executed immediately.

Note that most lexers (including the [`flex`](https://en.wikipedia.org/wiki/Flex_(lexical_analyser_generator))) are not so "smart" enough to match multiple patterns concurrently, and when they get to know the first pattern does not match `"helloworks"` at the character `'k'` they simply try to match the second pattern back from the start of the input. (In fact, if they want to be "smart", they need to include actions into the regular expressions (regular expression ASTs, to be accurate) and then combine all the regular expressions across rules into a single regular expression, before starting to match them with input.)

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

## About general form

Rtlex has the following general form.
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

- `QuasiQuotes` language extension is for specifying regular expressions within `[regex|...|]`. And regular expressions are compiled (that is, encoded into regular expression ASTs) at compile-time. So, if there is an error in a regular expression, it will be reported at compile-time. The syntax and semantics for regular expressions are described below.

- `import Parser` imports the `[regex|...|]` quasi quoter and the regular expression engine.

- `import Rtlex` imports: `stream`, `yyLex`, `rules`, `($$)`, `rule`, `yyReturn`, `yyAccept`, and `yyReject`.

- The `analyzer` consists of three sections, `stream`, `yyLex`, and `rules`, separated by `$$` operator. Each section is actually implemented as a [coroutine](https://en.wikipedia.org/wiki/Coroutine) that interacts with each other using yield (and resume), and the `$$` operator plays the role of binding those coroutines. The middle coroutine `yyLex` acts as a proxy between the upper and the lower coroutines, and reads each input character from the stream and passes it to `rules`. The `rules` tries to match each rule in its rules list with the given character, runs actions that correspond to successfully matched patterns, and reports the results from such actions to `yyLex` one by one. Then with each result from `rules`, `yyLex` calls `yacc` that is given as its argument, before `yyLex` repeats the next cycle by reading another character and so on.

- `stream` introduces an input stream and takes two arguments, `r0` and a string. `r0` can be any user-determined value of type `r`, and will be returned as a result from `analyzer` when the end of stream is reached. String is a list of characters to be served as the stream. Instead of a string, a bytestring or anything from an instance of `Stream` class can be used (will be implemented later).

- `yyLex` takes a user-defined function called `yacc`, which has type of `a -> m b`. `yacc` is a monadic function, taking an `a` that results from actions when their corresponding patterns are matched, and returning `b` under the user-determined monad `m`. The monad `m` will be usually the `IO` monad or some transformed monad of `IO`, but any monad will be ok. The monad result `b` is currently not used and can be anything, but is reserved for a future extension and may be possibly used to communicate with the stream to control it.

- `rules` introduces rules in a list. As such, each rule in the list must be separated with a comma, "`,`".

- `rule` combines a quasi-quoted regular expression and a user-defined `action` function into a rule. Each pattern of rules is matched as characters are read from the input stream, and if a pattern successfully matches a string from the stream up to the current character, the corresponding action is called with the matched string by the pattern as an argument. (More details about the matching algorithm are explained below.) Every `action` has type of `String -> m (ActionResult r a)`, where `ActionResult` type is defined as:
    ```haskell
    data ActionResult r a
        = Return r  -- to finish the lexical analyzer immediately with value "r"
        | Accept a  -- to accept the current match and report value "a" to lexical analyzer
        | Reject    -- to reject the current match and try other actions
    ```

- As you see, there are two user-determined types involved, `r` and `a` that are already introduced above. `a` is for reportng a value to `yyLex` and thus `yacc`, and `r` is for stopping and exiting the `analyzer` immediately with the return value of a `r`. So, before reaching the end of stream, we can early exit from `analyzer` using the `Return`. `Accept` is used to accept the current match and report an `a` to `yyLex`, and blocks further actions that also have their patterns matched from being executed. `Reject` just passes control over to the next action having a matched pattern. (See the details below.) Note that the `action` functions and `yacc` function run under the same shared monad `m`, which means they can interact with each other through the monad. To make it convenient to use those constructors under the monad, these three short-cuts are provided:
    ```haskell
    yyReturn :: Monad m => r -> m (ActionResult r a)
    yyReturn = return . Return

    yyAccept :: Monad m => a -> m (ActionResult r a)
    yyAccept = return . Accept

    yyReject :: Monad m => m (ActionResult r a)
    yyReject = return Reject
    ```

## Regular expressions

In rtlex, patterns are written as regular expressions instead of in context-free grammar. Since the regular expressions here are extended to support the recursive regular expressions and the embedding of arbitray Haskell expressions that lead to other regular expressions, we will see these regular expressions are more powerful than context-free grammar. Moreover, the engine for matching such regular expressions is implemented with the [Glushkov NFA algorithm](http://sebfisch.github.io/haskell-regexp/), which runs efficiently in *O(nm)* where *n* is the length of the input and *m* the size of the regular expression. It also does not involve any backtracking to match every alternative in regular expressions, and so works best with real-time streams that are hard to take back characters that have already been consumed.

The [LL grammar](https://en.wikipedia.org/wiki/LL_grammar) for regular expressions that rtlex takes is:
```
Regex        = ParseAlt
ParseAlt     = ParseAlt "|" ParseAnd | ParseAnd              (left associative)
ParseAnd     = ParseAnd "&" ParseSeq | ParseSeq              (left associative)
ParseSeq     = ParseSeq ParseTerm | ParseTerm                (left associative)
ParseTerm    = <a character>
             | "."
             | "[" ["^"] <characters> "]"
             | "${" [<a variable>] "}"
             | "{" <a Haskell function> "}"
             | "(" ParseAlt ")"
             | ParseTerm { "?" | "*" | "+" }
```

- `"|"` (*alternation*): `[regex|foo|bar|]` matches "foo" and matches "bar", and `[regex|land|island|]` matches "land" and matches "island".
    ```haskell
    main :: IO ()
    main =
        stream () "island"
        $$ yyLex (const $ return ())
        $$ rules [
            rule [regex|land|island|] $ \s -> do putStrLn s; yyAccept ()
            ]
    -- Output will be:
    -- *Main> main
    -- land
    -- island
    ```

    Note, unlike the case with `[regex|foo|bar|]` that has no occurrences of matching both "foo" and "bar", `[regex|land|island|]` matches both alternatives at the moment it reads the character `"d"` from input, and in this case, the corresponding action is executed twice for each matched string. Also note that in such a case with `"|"` operator, we have no way of executing the action for one alternative over another. Whereas if we write the alternatives in separate rules we can choose one of the corresponding actions by using `yyAccept`.
    ```haskell
    main :: IO ()
    main =
        stream () "island"
        $$ yyLex (const $ return ())
        $$ rules [
            rule [regex|island|] $ \s -> do putStrLn s; yyAccept (),
            rule [regex|land|]   $ \s -> do putStrLn s; yyAccept ()
            ]
    -- Output will be:
    -- *Main> main
    -- island
    ```

- `"&"` (*and*): `[regex|α&β|]` matches a string that is matched with both α and β *at the same time*. So, `[regex|foo.\*&.\*bar|]` matches a string from input that starts with "foo" and ends with "bar".

    Among words separated by spaces, if we want to choose the words that contain a number, we can use:
    ```haskell
    main :: IO ()
    main =
        stream () " abc de fgh1 ijk 23lm "
        $$ yyLex (const $ return ())
        $$ rules [
            rule [regex|.*[0123456789].*& [^ ]+ |] $ \s -> do putStrLn s; yyAccept ()
            ]
    -- Output will be:
    -- *Main> main
    -- fgh1 
    -- 23lm 
    ```

- (*Operator precedence*): All operators are listed from the highest precedence to the lowest as:
    ```
    ?, *, +                             (postfix, same precedence)
    sequencing (juxtaposition)          (binary, left-associative)
    &                                   (binary, left-associative)
    |                                   (binary, left-associative)
    ```

- `"."`: `"."` matches any single character (including whitespaces and control characters such as a new-line). So, `[regex|a.*b|]` matches strings of any length between "a" and "b" including "a" and "b". (However, be careful in using `".*"` in a pattern, because as a real-time analyzer, rtlex matches "." with any character includig a new-line, and `".*"` will match the whole input stream if not accompanied by proper boundary expressions.)

- `"[...]"` (*character class*) and `"[^...]"` (*negated character class*): `"[...]"` matches any single character from input that is listed inside the bracket, and `"[^...]"` matches one that does not listed inside it. (As for now, the character classes do not recognize character ranges such as `"[0-9]"`, so every character should be listed literally like as `"[0123456789]"`.)

- "\\*char*" (*escaped character*): Meta characters used as regular expression operators can be escaped with a preceding backslash `"\"`, and single-character escape codes such as `"\n"` can be used the same as in the Haskell and the C languages. 

- `"*"` (*Kleene closure*), `"+"` (*positive closure*), and `"?"` (*options*): `[regex|α*|]` matches α zero or more times, `[regex|α+|]` matches α one or more times, and `[regex|α?|]` matches α zero or one time, that is, matches α once but optionally.

- `"${var}"` and `"${}"` (*reference to other regular expression*): A regular expression can contain references to other regular expressions and its own regular expression as well. `"${}"` represents the whole regular expression that is currently being defined, and it is used to make a self-recursive regular expression. `"${var}"` embeds a reference to other regular expression through a variable name. `"${var}"` actually can take on any Haskell expression that leads to a regular expression.

    We can recognize a regular language of {a^n b^n | n >= 0} using the expression, `"X = (aXb)?"`, which cannot be described with the ordinary (non-recursive) regular expressions, but only with the context-free grammar.
    ```haskell
    main :: IO ()
    main =
        stream () "aaaaaabbbaaabb"
        $$ yyLex (const $ return ())
        $$ rules [
            rule [regex|(a${}b)?|] $ \s -> do putStrLn s; yyAccept ()
            --or we could also use: rule (let x = [regex|(a${x}b)?|] in x) $ \s -> ...
        ]
    -- Output will be:
    -- *Main> main
    -- ab
    -- aabb
    -- aaabbb
    -- ab
    -- aabb
    ```

    Using a function returning regular expressions, we can even make a more powerful expression that recognizes {a^n b^n c^n | n >= 1}, which is known that it cannot be described by the context-free grammar.
    ```haskell
    main :: IO ()
    main =
        stream () "aaaabbbcccc"
        $$ yyLex (const $ return ())
        $$ rules [
            rule [regex|${abc nul}|] $ \s -> do putStrLn s; yyAccept ()
        ]

        where
        nul    = [regex|()|]  -- "()" represents ε that matches an empty string.
        abc bc = let bc' = [regex|b${bc}c|] in
                 [regex|a(${bc'}|${abc bc'})|]
        -- To avoid left-recursion, we have used:
        -- abc | aabbcc | aaabbbccc | ... == a(bc | a(bbcc | a(bbbccc | a(...)))).
    -- *Main> main
    -- aaabbbccc
    ```

    However, [left recursion](https://en.wikipedia.org/wiki/Left_recursion) should be avoided or it will lead to an infinite loop. Every recursive call has to be guarded by a (non-terminal) symbol just as with [parser combinators](https://en.wikipedia.org/wiki/Parser_combinator). For example, the expression, `"a*"` can be represented in context-free grammar as either `"X = (aX)?"` or `"X = (Xa)?"`, but the directly translated expression, `[regex|(${}a)?|]` corresponding to the latter will lead to an infinite loop. (Left recursions can be eliminated by left-factoring the grammar. See [here](https://en.wikipedia.org/wiki/Left_recursion).)

- `"{fun}"`: For a zero-width assertion, We can embed a Haskell function into regular expressions. Functions should be of type `String -> [String]`, and they are provided as an argument with a partially matched string up to each position of them inside regular expressions. As each function acts as an assertion, it can decide whether it is matched or not based on the given matched string up to its position. For example, the following code matches a "B" only after a new-line. The first symbol is "." and matches any single character, so the function inside `{...}` takes as the argument s, the single-character string matched by ".". If s is a new-line, the function declares that it is also matched by returning a non-empty list, `[""]`, otherwise it returns an empty list, which makes the current match fail and does not give a chance to the next symbol "B" to match with further character from input (The further character has to be matched from the start, with the first symbol ".").
    ```haskell
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
    ```

    The assertion function also acts as a converter, and so it can convert the partially matched string up to its position into another string. In fact, in the above example, the assertion function actually converts a new-line character into the null string, `""`, otherwise the finally matched string that is printed on the screen would be "\nB" instead of "B". Also note that it returns the converted string in a list rather than as is. Actually, an assertion function can convert a string into multiple strings, and when some (or all) of the strings finally pass through all the remaining patterns behind the assertion function, they will be fed to their corresponding action one be one.
    ```haskell
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
    ```

    Note that unlike the `yacc` function and `action` functions, assertion functions do not run under the user-specified monad. So, they cannot interact with each other, nor with `yacc` or `action` functions.

- `"(...)"`: Regular expressions can be grouped in parentheses to limit the scope of operators. The empty parentheses `"()"` represents ε that matches an empty string. So, `"α?"` is an equivalent expression to `"α|()"`.

## Details about matching rules

Patterns are always matched whether or not ...

Every possible submatch

## More interesting applications

Hoho
