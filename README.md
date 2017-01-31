# A real-time pattern matching algorithm in Haskell over network stream

Rtlex (Real-time Lexical Analyzer) is a scanner over network stream (or any kind of real-time streams) that executes a monadic action whenever a pattern matches a text in the stream. It is intended to work on real-time streams, and so it is based on space- and time- efficient algorithm that does not backtrack while choosing patterns and as such, does not rely on the stream being recoverable using something like `unget()`.

Rtlex takes (self- and mutual- as well) recursive regular expressions for patterns to match, which are easier to write and better to fit in a lexical analyzer than context-free grammar. The regular expression is extended to be able to embed an arbitrary (in-line) Haskell function as a zero-width assertion as well as a on-the-fly converter of (partially) matched strings. Regular expressions are specified with a quasi quote and thus compiled at compile-time.

Actions that will run when corresponding regular expressions match are ordinary Haskell functions under any monad such as IO, and are given as an argument the matched string by the corresponding regular expressions.

## Why to analyze in real-time separately?

The legacy `lex` matches its patterns in such a way that:
- it tries to match patterns from top to bottom (or concurrently if it is "smart" enough),
- if it finds a pattern matches a string that it has read so far from input, it memorizes the matched pattern and the position of input as well, and then it repeatedly tries further patterns (including the currently matched pattern as well) for any possible longer match, and
- if there are no more patterns that successfully match at some character being read from input, it finally declares a match with the last pattern that has been remembered to match successfully, and recovers the input to the position corresponding to the last match, to continue the next lexical analysis.

For example, if the legacy `lex` has the following two rules,
~~~~
%%
helloworld { printf("1st pattern\n"); }
hello      { printf("2nd pattern\n"); }
~~~~
and if we give it the input, "`helloworld`", it will not execute any action until it completes to read in the whole input string. That is, even after it reads "`hello`" it does not execute the second action immediately, because at that moment it does not know whether the first pattern will match further input characters or not. After reading the whole input string, it will execute (only) the first action. What if we give it the input, "`helloworks`"? This time, the second action will be executed, but only when it reads the character '`k`'.

Also note that most `lex`s (including the `flex`) are not so "smart" enough to match multiple patterns concurrently, and when they get to know the first pattern does not match "`helloworks`" at the character '`k`' they simply try to match the second pattern from the start of the input again. (If they want to be "smart", they need to put an action into the regular expression ADT (algebraic data type) and then combine all regular expressions from patterns into one.) That means, depending on accompanied patterns, the action corresponding to a matched pattern may not be executed immediately.

So, to facilitate the analysis of real-time streams, rtlex features:
- immediate matching of patterns, rather than lazy matching to find out any possible longer match, and
- concurrent matching of all patterns, rather than trying patterns one by one.
