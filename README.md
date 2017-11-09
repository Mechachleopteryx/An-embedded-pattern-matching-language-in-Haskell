# Rtlex: an embedded pattern-matching language in Haskell based on Glushkov's regex-matching algorithm

**Rtlex** (Real-time [Lexical 
Analyzer](https://en.wikibooks.org/wiki/Compiler_Construction/Lexical_analysis)) is a 
tool for generating real-time lexical analyzer over network stream (or any kind of 
real-time stream) in Haskell. It is basically similar to the legacy lexical analyzer 
generator such as `flex` in the syntax and the semantics; it executes a monadic action 
(that is, an action possibly having a side-effect) whenever a pattern among many 
specified patterns matches some text from the input stream. However, it differs in that 
it deals with real-time stream rather than an already saved file, it codes the lexical 
analyzer specification directly in Haskell's template language, and it extends the 
pattern so that we can embed Haskell code running on match time in it. To effectly work 
on a real-time stream, its pattern-matching engine is implemented on the basis of the 
very efficient Glushkov's 
[NFA](https://msdn.microsoft.com/en-us/library/e347654k(v=vs.110).aspx) matching 
algorithm, which can keep matching multiple patterns simultaneously and do that without 
[backtracking](https://msdn.microsoft.com/en-us/library/dsy130b4(v=vs.110).aspx), and as 
such, does not rely on the stream being recoverable (or bufferable) by using things like 
[`unget()`](http://www.cplusplus.com/reference/istream/istream/unget/).

Rtlex uses (self- and mutual- as well) [recursive regular 
expressions](http://www.regular-expressions.info/recurse.html) for specifying patterns to 
match with, which are easier to write and are better to fit in a lexical analyzer than 
the [context-free grammar](https://en.wikipedia.org/wiki/Context-free_grammar), as we 
will see below. It extends its regular expression so that we can embed Haskell variables 
and arbitrary (in-line) Haskell functions in it. We can use embedded variables the same 
as ordinary Haskell variables or we can use them to interpolate other regular expressions 
into a regular expression. With them, we can even interpolate a regular expression into 
itself to generate a recursive pattern. Moreover, we can embed a Haskell function into a 
regular expression to dynamically interpolate a regular expression, which is quite 
similar to `(??{code})` in Perl.

We can use the embedded function as a [zero-width 
assertion](http://www.regular-expressions.info/lookaround.html) that determines the 
success or failure of its match on match time, based on the (sub-)string that has been 
partially matched so far up to the position of the zero-width assertion. The embedded 
function is also used to convert the partially matched (sub-)string into another string, 
while matching the regular expression. Regular expressions in rtlex are specified as 
wrapped in [*quasi quotes*](https://wiki.haskell.org/Quasiquotation) and thus compiled at 
the compilation time of the lexical analyzer itself. That is, we don't need another 
compilation to compile it into a Haskell or C code, which is the usual process followed 
by the legacy lexer.

Actions that will run when their associated patterns are matched are ordinary Haskell 
monadic functions under any user-specified monad such as `IO`. They are given, as an 
argument, the input string matched by the associated patterns for further looking it up. 
As all actions share the same monad, they can communicate with each other through the 
monad, and we can use, for example, simple mutable reference like 
[`IORef`](https://hackage.haskell.org/package/base-4.9.1.0/docs/Data-IORef.html), or a 
transformed monad like [`StateT u 
IO`](https://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Monad-Trans-State-Lazy.html) 
with some user state type `u`.

## Why to analyze in real-time?

Legacy lexical analyzer like 
[`flex`](https://en.wikipedia.org/wiki/Flex_(lexical_analyser_generator)) basically finds 
*the earliest and the longest submatch* and it so tries to match its patterns in such a 
way that:
- It tries to match its patterns from top to bottom and one by one (or simultaneously if 
  it is "smart" enough),
- If it finds a pattern to match a string up to some point as it reads from the input, it 
  memorizes the pattern and the current point of the (ending-) match in the input, but 
  then it continues to try other patterns (and the currently matching pattern as well) 
  for finding any possible longer matches as reading more characters from the input. If 
  it can find a longer match it forgets about the previous pattern and its point of match 
  that were memorized and instead it memorizes the new matching pattern and the new point 
  of match in the input, and does this repeatedly.
- If it finally finds no more patterns to match successfully as it reads from input, it 
  declares the match with the last pattern that has been memorized and also recovers the 
  input to the last point corresponding to the match, to continue the next lexical 
  analysis with remaining input starting at the recovered point.

For example, if a legacy lexical analyzer has the following two rules,
```
%%
helloworld { printf("1st action\n"); }
hello      { printf("2nd action\n"); }
```
and if given the input, "helloworld", it will not execute either action until it 
completely read the whole input string, and after reading to the end of the input it will 
execute only the first action. In the middle of reading the input, at the moment it reads 
the first 'o' from "helloworld", it knows that the second pattern does match, but it does 
not execute the corresponding action immediately, because it wants to try to match other 
pattern with as many input characters as possible. And when it finally finds the first 
pattern does match the whole input string, it then executes the first action, ignoring 
the second action. What if we give it the input, "helloworks"? This time, the second 
action will be executed as expected, but it will be executed only when it reads the 
character 'k' from "helloworks", which is the first clue of no successful match with the 
first pattern. That means, even though a pattern matches, the correspoding action may not 
be executed immediately, depending on other patterns.

Let's add another rule to the above example to make:
```
%%
helloworld { printf("1st action\n"); }
hello      { printf("2nd action\n"); }
works      { printf("3rd action\n"); }
```
and suppse we are working on the same input "helloworks". We now expect the second and 
the third action will be executed this time, and they do for sure. As with the previous 
example, the second action is executed when the first pattern is found to no longer match 
at reading 'k' from "helloworks". Then, how does the third pattern match with the 
remaining input? The answer is backtracking. When the second action is executed, the 
input is recovered to the point of matching with the second pattern "hello", and the 
input is pushed back to become "works", which is then matched with the third pattern. And 
in this matching of the third pattern with the recovered input, the characters are 
matched one by one from the start, even though while matching with the first pattern 
previously, we could know that the first three characters "wor" are actually matching the 
first three characters of the third pattern. However, those characters are matched again 
completely and redundantly by having the input backtracked. This is because multiple 
patterns here are not considered simultaneously and only one pattern is considered at a 
time, which is the way most legacy lexical analyzers including `flex` do. (In fact, if 
they are to be "smart" enough, they need to embed actions in the middle of the associated 
patterns (i.e., regular expressions) and combine all those regular expressions across 
rules into a single regular expression, before start matching with them.)

So, to facilitate the lexical analysis with real-time stream, rtlex features:

> - immediate matching of patterns, rather than lazy matching to search for any possible 
>   longer matches, and
> - simultaneous matching of all patterns, rather than matching patterns one at a time. 
>   (By simultaneous, I do not mean that every pattern is matched through a separate 
>   thread, but I mean that patterns are matched in such an interleaved way that there 
>   will be no backtracking when a pattern fails and another pattern is tried.)

## An example

Here is a simple code that detects "sheer", "she", "he", and "he\*r" when given the input string, "sheerEnd".
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Parser
import Rtlex
import Control.Monad (when)

main :: IO Int
main =
    stream (-1) "sheerEnd"  -- main returns -1 if stream runs out.

    $$ yyLex (const $ return ())
	-- As a simple analyzer, it does nothing here with the resulting reports from
	-- actions that are executed when corresponding patterns match some input.

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
It shows that "she" and "he" is matched with the input "sheerEnd".
```
*Main> main
she
he
sheer
0
```

## Another example

By having `StateT (Map String Int) IO` instead of the `IO` monad, we can count 
occurrences of particular words with the State monad.
```haskell
{-# LANGUAGE QuasiQuotes #-}

import Parser
import Rtlex
import Control.Monad.State
import qualified Data.Map as Map

main :: IO ()
main = do  -- in the IO monad
    m <- flip execStateT Map.empty $
	-- execStateT returns the final state but discards the final value, which is () 
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

A network-stream version of this code is introduced at the [**Network streams**](https://github.com/dzchoi/Real-time-Lex/blob/master/README.md#network-streams) section near the end of this document.

## How to use it?

See `sample.hs` for an example.

Rtlex basically has the following general form.
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

- `QuasiQuotes` language extension enables for us to specify regular expressions within 
  `[regex|...|]`. The regular expressions inside are compiled (that is, encoded into 
  regular expression ASTs) at the compilation time of the source code. So, if there is an 
  error within a regular expression, it will be reported at compile-time. The syntax and 
  the semantics for regular expressions are described 
  [below](https://github.com/dzchoi/Real-time-Lex/blob/master/README.md#regular-expressions).

- `import Parser` imports the definition for `[regex|...|]` quasi quoter and the regular 
  expression engine.

- `import Rtlex` imports: `Stream`, `stream`, `stream0`, `yyLex`, `rules`, `($$)`, `rule`, `yyReturn`, `yyAccept`, and `yyReject`.

- The `analyzer` consists of three sections, `stream`, `yyLex`, and `rules`, separated by 
  `$$` operator. Each section is actually a 
  [coroutine](https://en.wikipedia.org/wiki/Coroutine) that interacts with each other by 
  sending and receiving each input character and the result of match (if successful) with 
  the given character, using `yield` and `resume`, which are not exported by default 
  because they are to be used only internally. The `$$` operator plays the role of 
  binding those three coroutines.

- The `stream` is the first and upper coroutine, which introduces an input stream to work 
  on and reads in each character from the input to hand over to the other coroutines, 
  `yyLex` and `rules`. It takes two arguments, `r0` and a string. `r0` is a result value 
  that will finally be returned by `analyzer` when the input stream is reached to the 
  end. The type of `r0` can be any user-determined type `r`, which is specified in the 
  type signature `analyzer :: m r`. The string as the second argument denotes a list of 
  characters that will be served as the input stream. If other kind of stream (such as 
  `ByteString`) is preferred than a simple list of characters, we can define any instance 
  of `Stream` class and use it with the more generic function `stream0` rather than 
  `stream`. See the [**Network 
  streams**](https://github.com/dzchoi/Real-time-Lex/blob/master/README.md#network-streams) 
  section for this usage.

- The second and middle coroutine `yyLex` acts as a proxy between the upper and the lower 
  coroutines. With the input character coming from the upper `stream`, `yyLex` just 
  passes it down to the below coroutine `rules`, but `yyLex` deals with the match result 
  returned from the `rules` if `rules` finds a pattern is matched with the input 
  character.

  `yyLex` takes as its argument a user-defined function called `yacc`, which has type of 
  `a -> m b`. `yacc` is a user-defined monadic function. It takes an `a` that is a result 
  value returned from an action in one of `rules`'s rules when the corresponding pattern 
  is matched with the current input character. It returns `m b` with the user-determined 
  monad `m`. The monad `m` will usually be the `IO` monad or some transformed monad of 
  `IO`, but any monad can be used. The monad result `b` can be anything because it is 
  currently not used for now, but it is reserved for a future extension and will be 
  possibly used to communicate with the stream to control it.

- The third and lower coroutine `rules` introduces rules in a list, each of which is 
  separated by a comma "`,`". It tries to match each rule in its rules list with the 
  passed character, and if a rule has a pattern matching with the input up to the given 
  character it runs the action of the rule and reports the result from the action to the 
  upper `yyLex`. As a coroutine, it reports each result to `yyLex` if and only if a rule 
  is matched. The type of the result should match the argument type of `yacc` function in 
  `yyLex`, though it may be implicitly inferred from the code of each action and `yacc` 
  without having a specific type signature for it.

- The `rule` in `rules` section specifies each rule as a pattern-action pair, and 
  combines a quasi-quoted regular expression for a pattern and a user-defined `action` 
  function into a rule. Each pattern is tried to match as an input character is given by 
  the above coroutine. If a pattern successfully matches a string from the input up to 
  the current input character, the corresponding action is invoked with the matched (and 
  accumulated) string so far as an argument. (The matching algorithm needs explaining 
  more details, which are 
  [below](https://github.com/dzchoi/Real-time-Lex/blob/master/README.md#details-about-matching-rules).)

- Every `action` has type of `String -> m (ActionResult r a)`, where `ActionResult` type 
  is imported from `Rtlex` like:
    ```haskell
    data ActionResult r a
        = Return r  -- to finish the lexical analyzer immediately with value "r"
	| Accept a  -- to accept the current match and report value "a" to `yyLex`
	| Reject    -- to reject the current match and continue to try other rules
    ```

    As we can see, there are two user-determined types involved, `r` and `a` that have 
    been introduced above. The `a` is for reporting a result value to `yyLex` and hence 
    `yacc`, and `r` is for the final return value from `analyzer`.

    We can use `Return r` to stop `anyalyzer` and exit immediately with the return value 
    of `r` even before reaching the end of stream. The `Accept` is used to accept the 
    current match and report an `a` to `yyLex`, blocking next rules from having a chance 
    to match their patterns. However, the `Reject` passes the control over to the next 
    rules to allow possiblity to match their patterns (See the details 
    [below](https://github.com/dzchoi/Real-time-Lex/blob/master/README.md#details-about-matching-rules).) 

    Note also that the each `action` function and `yacc` function run under the same 
    shared monad `m`, so that they can interact with each other through the same monad.

- The following three short-cuts are provided in `Rtlex` to make it easy to use those 
  `ActionResult`'s constructors:
  ```haskell
  yyReturn :: Monad m => r -> m (ActionResult r a)
  yyReturn = return . Return

  yyAccept :: Monad m => a -> m (ActionResult r a)
  yyAccept = return . Accept

  yyReject :: Monad m => m (ActionResult r a)
  yyReject = return Reject
  ```

## Regular expressions

In rtlex, patterns are written in regular expressions instead of in context-free grammar. 
Since the regular expressions here are extended to embed any Haskell expressions (as well 
as variables) that lead to other regular expressions, we will see these regular 
expressions are more powerful than context-free grammar with respect to the recognizable 
languages. Moreover, the engine for matching such regular expressions is implemented with 
the [Glushkov NFA algorithm](http://sebfisch.github.io/haskell-regexp/), which runs 
efficiently in *O(nm)* where *n* is the length of the input and *m* the size of the 
regular expression. The algorithm does not involve any backtracking to match every 
possible alternative in regular expressions, and thus works best with real-time streams 
that are hard to take back characters that have already been consumed.

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

    Note, unlike the case with `[regex|foo|bar|]` that has no occurrences of matching both "foo" and "bar", `[regex|land|island|]` matches both alternatives at the moment it reads the character 'd' from input, and in this case, the corresponding action is executed for each matched string. Also note that in such a case with `"|"` operator, we have no way of executing the action for one alternative over another. Whereas if we write the alternatives in separate rules we can choose one of the corresponding actions by using `yyAccept` (See the details [below](https://github.com/dzchoi/Real-time-Lex/blob/master/README.md#details-about-matching-rules)).
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

- `"&"` (*and*): `[regex|α&β|]` matches a string that is matched with both α and β *at the same time*. So, `[regex|foo.*&.*bar|]` matches a string from input that starts with "foo" and ends with "bar".

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

- `"${var}"` and `"${}"` (*reference to other regular expression*): A regular expression can contain references to other regular expressions and its own regular expression as well. `"${}"` represents the whole regular expression that is currently being defined, and it is used to make a self-recursive regular expression. `"${var}"` embeds a reference to other regular expression through a variable name. However, `"${var}"` actually can take on any Haskell expression that leads to a regular expression.

    We can specify a regular language of {a^n b^n | n >= 0} using the expression, `"X = (aXb)?"`, which cannot be described with the ordinary (non-recursive) regular expressions.
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

    Using a function returning a regular expression, we can even make a more powerful expression that recognizes {a^n b^n c^n | n >= 1}, which is known that it cannot be described by the context-free grammar.
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

- `"{fun}"`: For a zero-width assertion, We can embed a Haskell function into regular expressions. Functions should be of type `String -> [String]`, and they are provided as an argument with a partially matched string up to each position of them inside regular expressions. As each function acts as an assertion, it can decide whether it matches or not on its position based on the given partially matched string. For example, the following code matches a "B" only after a new-line. The first symbol in the pattern is "." and matches any single character, so the function inside `{...}` takes as the argument s, the single-character string matched by ".". If s is a new-line, the function declares that it also matches by returning a non-empty list, `[""]`, otherwise it returns an empty list, which makes the current match fail and does not give a chance to the next symbol "B" to match with further character from input (the further character has to be matched from the start, with the first symbol ".").
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

    The assertion function also acts as a converter, and so it can convert the partially matched string up to its position into another string. In fact, in the above example, the assertion function actually converts a new-line character into the null string, `""`, otherwise the finally matched string that is printed on the screen would be "\\nB" instead of "B". Also note that it returns the converted string in a list rather than as is. Actually, an assertion function can convert a string into multiple strings, and when some (or all) of the strings finally pass through all the remaining patterns behind the assertion function, they will be fed to their corresponding action one be one.
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

    Note that unlike the `yacc` function and `action` functions, assertion functions are pure and do not run under the user-specified monad. So, they cannot interact with each other, nor with `yacc` or `action` functions.

- `"(...)"`: Regular expressions can be grouped in parentheses to limit the scope of operators. The empty parentheses `"()"` represents ε that matches an empty string. So, `"α?"` is an equivalent expression to `"α|()"`.

- For optimal code generation, zero-width assertions `"{fun}"` and ε pattern `"()"` are supported only when the `"-DZERO_WIDTH_ASSERTION"` [pre-processor constant](https://downloads.haskell.org/~ghc/7.8.2/docs/html/users_guide/options-phases.html#c-pre-processor) is defined in `ghc` or `ghci`.

## Details about matching rules

#### Every possible submatch rather than the earliest and the longest submatch

Whereas legacy lexical analyzers are eager to find *the earliest and the longest submatch*, rtlex tries to find *every possible submatch*. For that, rtlex keeps matching all patterns constantly and simultaneously. *Constantly*, because every time rtlex reads an input character from a stream rtlex tries to match all patterns with it, thinking as if the character can start a new match with any of the patterns. *Simultaneously*, because rtlex considers the possibilities of successful matching for all the patterns at a time, so that it does not need to backtrack and reconsider some unconsidered patterns later.

Here, while reading the second occurrence of "aba" from input, rtlex considers it as a new match with the `[regex|abac|]` pattern, even though rtlex is in the middle of considering the first occurrence of "aba" with the same pattern.
```haskell
main :: IO ()
main =
    stream () "ababac"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|abac|] $ \s -> do putStrLn s; yyAccept ()
    ]

-- *Main> main
-- abac
```

In the code below, rtlex considers the two patterns simultaneously while reading "ab" from input.
```haskell
main :: IO ()
main =
    stream () "abd"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|abc|] $ \s -> do putStrLn s; yyAccept (),
        rule [regex|abd|] $ \s -> do putStrLn s; yyAccept ()
    ]

-- *Main> main
-- abd
```

Using the nature of finding "every possible submatch", we can detect the start and the end of a pattern in real-time when the pattern occurs consecutively.
```haskell
main :: IO ()
main =
    stream () "aaabbbbbbccc"
    $$ yyLex (const $ return ())
    $$ rules [
        -- repeated b's are discarded
        rule [regex|bb|]    $ \s -> yyAccept (),

        -- catch the first b
        rule [regex|b|]     $ \s -> do putStrLn "Start of b's"; yyAccept (),

        -- catch the last b
        rule [regex|b[^b]|] $ \s -> do putStrLn "End of b's"; yyAccept ()
    ]

-- *Main> main
-- Start of b's
-- End of b's
```

#### Actions are executed from top to bottom, but selectively.

The `rule` combines a quasi-quoted regular expression and a user-defined action into a rule. Each pattern of rules is matched as characters are read from the input stream, and if a pattern successfully matches a string from the stream up to the current character, the associated action is called with the matched string by the pattern as an argument. Every action has type of `String -> m (ActionResult r a)`, where `ActionResult` type is defined as:
```haskell
data ActionResult r a
    = Return r  -- to finish the lexical analyzer immediately with value "r"
    | Accept a  -- to accept the current match and report value "a" to lexical analyzer
    | Reject    -- to reject the current match and try other actions

yyReturn :: Monad m => r -> m (ActionResult r a)
yyReturn = return . Return

yyAccept :: Monad m => a -> m (ActionResult r a)
yyAccept = return . Accept

yyReject :: Monad m => m (ActionResult r a)
yyReject = return Reject
```

However, unlike patterns are tried to match constantly and simultaneously, not all matched actions (that is, actions associated with matched patterns) are executed always. As monads, actions are executed in the order of top-to-bottom, and each action is executed only if its all previous actions give it a way by returning `Reject`. In other words, if an action returns `Accept`, the actions below it are not executed. (Note, however, returning `Reject` or `Accept` affects only the execution of actions, and has nothing to do with matching patterns; all patterns are tried matching always!)
```haskell
main :: IO ()
main =
    stream () "she"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|she|] $ \s -> do putStrLn s; yyReject,
        rule [regex|he|]  $ \s -> do putStrLn s; yyAccept ()
    ]

-- *Main> main
-- she
-- he
```
```haskell
main :: IO ()
main =
    stream () "she"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|she|] $ \s -> do putStrLn s; yyAccept (),
        rule [regex|he|]  $ \s -> do putStrLn s; yyAccept ()
    ]

-- *Main> main
-- she
```

#### A pattern can pass multiple strings to the corresponding action in a match.

The last example above can be rewritten with a single pattern as follows. As the `[regex|he|she|]` pattern can match "he" and "she" at the same time when reading 'e' from the input string, "she", it passes both of them over to its action, calling the action with each of them as the argument. And in this case, we cannot control the multiple executions of the same action using `Accept` or `Reject`, as they only affect the execution of actions that come below.
```haskell
main :: IO ()
main =
    stream () "she"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|he|she|] $ \s -> do putStrLn s; yyAccept ()
    ]

-- *Main> main
-- he
-- she
```

Be careful when using quantification operators, `"*"` and `"+"`, that rtlex will match all the possible (sub-)strings from input stream.
```haskell
main :: IO ()
main =
    stream () "aaa"
    $$ yyLex (const $ return ())
    $$ rules [
        rule [regex|a*|] $ \s -> do putStrLn s; yyAccept ()
    ]

-- *Main> main
-- a
-- a
-- aa
-- a
-- aa
-- aaa
```
The results from the example above may look redundant, but if we think of the input string as <code>"a<sub>1</sub>a<sub>2</sub>a<sub>3</sub>"</code> they correspond to:
<pre>a<sub>1</sub>
a<sub>2</sub>
a<sub>1</sub>a<sub>2</sub>
a<sub>3</sub>
a<sub>2</sub>a<sub>3</sub>
a<sub>1</sub>a<sub>2</sub>a<sub>3</sub></pre>

Then, we might have a question, if an action is called multiple times in a match, for each matched string by the corresponding pattern, and if the action returns `Accept` for some of the strings and `Reject` for others, how does it affect the action that follows it? The answer is that the action behaves just the same when it is called separately for different matches from its pattern, so we do not need to consider this case specially.
```haskell
import Data.Char (toUpper)
main :: IO ()
main =
    stream () "abc"
    $$ yyLex putStrLn 
    $$ rules [
        rule [regex|ab|b|] $ \s -> if s == "b" then yyAccept s else yyReject,
        rule [regex|.b{\s -> [map toUpper s]}|] $ \s -> yyAccept s
    ]

-- *Main> main
-- b
-- AB
```
In the above example, at the moment the character 'b' is reached, the first pattern `[regex|ab|b|]` matches both "ab" and "b" at the same time, and the corresponding action returns `yyAccept "b"` for "b" and `yyReject` for "ab". Although this case is processed in a rather complex way internally, we can easily think that "b" is accepted and gets passed to `putStrLn` through `yyLex`, and at the same time, since "ab" is ignored by the first action the second action is executed and converts it into its capitalized string, which is then passed over to `putStrLn` as is `yyAccept`ed.

So, we can just think that an action is called for each match regardless of whether the matches occur at the same time in a pattern or not.

#### `yyLex` is also called for each match, but with some user-determined value than the matched string.

The `yyLex` is called whenever a matched action returns something with `yyAccept`, and then `yyLex` calls the `yacc` that is specified as its argument and feeds the returned result from the action to `yacc`. So, whenever there is a match, a corresponding action is called and then `yacc` is also called. But, whereas action is given the matched string as its argument, `yacc` is given the result that is returned by such an action. In a sense, we can think of actions as converters from String into a value of a user-determined type `a`.
```
action :: String -> m (ActionResult r a)
yacc   :: a -> m b
```

The `yacc` returns `m b`, a value wrapped in a user-determined monad `m`. However, since `yacc` is called each time there is a match from a pattern, and since it is not called at once with all the matches together, the intermediate results of `m b` cannot be used outside of `yacc`, and they are simply ignored (actually, reserved for future use) outside of `yacc`. That's actually what `yacc` is for. If we want to process the result from an action each time a pattern matches, we put the procedure into `yacc`. However, if we want the results from actions at once at the end of lexical analysis, we need to gather those results using some monad.
```haskell
import Control.Monad.State

gather :: IO [String]
gather =  -- in the IO monad
    flip execStateT [] $
    -- execStateT returns the final state and discards the final value, which is () 
    -- returned from stream ().

    stream () "ha ha ho ho hi ha"

    $$ yyLex (\s -> do  -- in the "StateT [String] IO" monad
        modify $ (s:))  -- stores each word in a list

    $$ rules [
        rule [regex|ha|ho|hi|] $ \s -> yyAccept s  -- reports each word to the yyLex.
        ]

-- *Main> gather
-- ["ha","hi","ho","ho","ha","ha"]
```

#### About `r`

The `r` is the type of the final result from our lexical analyzer. It can be returned by either `stream` or an action. `stream` returns an `r` when it has reached the end of its stream. An action can use `yyReturn` to return an `r`, making our lexical analyzer stop immediately. If our lexical analyzer needs to keep running to the end of a stream, chances are we don't need to `yyReturn` it in any action unless there is an exceptional case in the stream. But, if we expect our lexical analyzer to exit early when encountering a certain pattern in the stream, we can make use of it.

## Network streams

Instead of `stream`, a more generic stream-reading function `stream0` is provided to work with any stream of an instance of `Stream` class, which is very simple because it is not assumed to be recoverable. And, by defining a proper `getc` method for a custom stream, we can use it with `stream0`.
```haskell
class Stream s m r c | s -> r c where -- needs FunctionalDependencies
    getc :: s -> m (Either r (c, s))
    -- getc will return either r in case of an error in the stream, or (c, s) otherwise.
```

For example, we can make an example server program that was introduced earlier to count occurrences of some words.
```haskell
{-# LANGUAGE QuasiQuotes, FlexibleInstances, MultiParamTypeClasses #-}

import Parser
import Rtlex
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
```

When we run the above program and run some client program, we will get things like:
```
$ nc localhost 3001
ha ha ho hoo hi ha
^D
```
```
*Main> main
Starting server ...
ha
ha
ho
ho
hi
ha
[("ha",3),("hi",1),("ho",2)]
Server closed.
```
