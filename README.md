# A real-time pattern matching algorithm in Haskell over network stream

**Rtlex** (Real-time Lexical Analyzer) is a scanner over network stream (or any kind of real-time streams) that executes a monadic action whenever a pattern matches some text in the stream. It is intended to work on real-time streams, and so it is based on space- and time- efficient algorithm that does not backtrack while trying multiple patterns and as such, does not rely on the stream being recoverable using something like `unget()`.

Rtlex uses (self- and mutual- as well) recursive regular expressions for specifying patterns to match, which are easier to write and better to fit in a lexical analyzer than *context-free grammar*. Rtlex takes an extended regular expression into which we can embed Haskell variables to refer to other regular expressions, and arbitrary (in-line) Haskell functions to be used as zero-width assertions such as look-behinds. Zero-width assertions are further extended and can also be used to convert a partially matched substring so far into another string, in the middle of a regular expression while matching is going on. Regular expressions in rtlex are specified in a *quasi quote* and thus compiled at compile-time.

Actions that will run when corresponding regular expressions are matched with an input string are ordinary Haskell functions under some user-specified monad such as `IO`. As the actions share the same monad, they can communicate with each other through some simple mutable references like `IORef` or through a transformed monad like, for example, `StateT u IO` for some user state type `u`. Actions are also given as an argument the whole matched string by the corresponding regular expressions.

## Why to analyze in real-time?

The legacy `lex` matches its patterns in such a way that:
- it tries to match patterns from top to bottom (or concurrently if it is "smart" enough),
- if it finds a pattern matches a string that it has read so far from input, it memorizes the matched pattern and the position of input as well, and then it repeatedly tries further patterns (including the currently matched pattern as well) for any possible longer match, and
- if there are no more patterns that successfully match at some character being read from input, it finally declares a match with the last pattern that has been remembered to match successfully, and recovers the input to the position corresponding to the last match, to continue the next lexical analysis for the input.

For example, if the legacy `lex` has the following two rules,
```
%%
helloworld { printf("1st action\n"); }
hello      { printf("2nd action\n"); }
```
and if we give it the input, `"helloworld"`, it will not execute any action until it completes to read in the whole input string. That is, even after it reads `"hello"` it does not execute the second action immediately, because at that moment it does not know whether the first pattern will match with further input characters or not. After reading the whole input string, it will execute (only) the first action. What if we give it the input, `"helloworks"`? This time, the second action will be executed, but only when it reads the character `'k'`.

Also note that most `lex`s (including the [`flex`](https://en.wikipedia.org/wiki/Flex_(lexical_analyser_generator))) are not so "smart" enough to match multiple patterns concurrently, and when they get to know the first pattern does not match `"helloworks"` at the character `'k'` they simply try to match the second pattern back from the start of the input. (If they want to be "smart", they need to put an action into the regular expression ADT (algebraic data type) and then combine all regular expressions from patterns into one.) That means, depending on accompanied patterns, the action corresponding to a matched pattern may not be executed immediately.

So, to facilitate the analysis of real-time streams, rtlex features:
> - immediate matching of patterns, rather than lazy matching to find out any possible longer match, and
> - concurrent matching of all patterns, rather than trying patterns one by one. (By concurrent, I do not mean that every pattern is matched through a separate thread, but that patterns are matched in such an interleaved way that there will be no backtracking when a pattern fails to match and another pattern is tried.)

## An example

```haskell
main :: IO Int
main =
    stream (-1) "sheerEnd"  -- return -1 on out of stream
    $$ yyLex (const $ return ())  -- do nothing with the reported "()"s from rules
    $$ rules [
        rule [regex|End|]  $ \s -> yyReturn 0,
        rule [regex|he|]   $ \s -> do putStrLn s; yyReject,
        rule [regex|she|]  $ \s -> do putStrLn s; yyAccept (),
        rule [regex|he*r|] $ \s ->
            if s == "her" then do
                putStrLn s; yyAccept ()
            else
                yyReject
    ]
```
