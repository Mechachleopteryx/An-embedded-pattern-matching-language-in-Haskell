# A real-time pattern matching algorithm in Haskell over network stream

Rtlex (Real-time Lexical Analyzer) is a scanner over network stream (or any kind of real-time streams) that executes a monadic action whenever a pattern matches a text in the stream. It is intended to work on real-time streams, and so it is based on space- and time- efficient algorithm that does not backtrack while choosing patterns and as such, does not require the stream is recoverable using something like `unget()`.

Rtlex takes (self- and mutual- as well) recursive regular expressions for patterns to match, which are easier to write and better to fit in a lexical analyzer than context-free grammar. The regular expression is extended to be able to embed an arbitrary in-line Haskell function as a zero-width assertion as well as a converter of (partially) matched strings. Regular expressions are specified with a quasi quote and thus compiled at compile-time.

Actions that will run when corresponding regular expressions match are ordinary Haskell functions under any monad such as IO.

## Why real-time?
