class: center, middle, heading-black

# Haskell's bear traps

Tim Humphries

Formation

<a href="https://twitter.com/thumphriees">@thumphriees</a>
<a href="https://teh.id.au">teh.id.au</a>

???

Hi all, name is Tim, work at Formation right now, etc.

This is a talk about all the things I pay attention to when reviewing
Haskell code.

Sorry it's so half-baked!

---

class: center, middle

# Too much stuff

???

The general theme of this talk is "there's too much".

How can we cut down on it?

---

class: center, middle

# Part 1: Failure

???

I'd like to start by talking a bit about the building blocks of
failure.

Failure you can see.

---

# `undefined`

a.k.a. *bottom*, the root of all runtime errors

```haskell
undefined :: forall a. a
```


???

Let's start with the bottom.

Haskell expressions don't always successfully evaluate.
There are two mechanisms for runtime errors: functions are
either **partial** or they **throw exceptions**.

`undefined` is the simplest partial function. It always typechecks,
and blows up whenever it is evaluated.


--

```sh
λ> length undefined
*** Exception: Prelude.undefined
```

???

--

```haskell
important :: Thing -> ValidatedThing
important =
  undefined -- TODO implement
```

World's worst task management system

???

Particularly nefarious when you use it to unblock yourself.

Slot it in as a TODO, your code compiles, you can keep moving.

This code should probably never be merged.

It can be merged (no errors) and can make it all the way to
production. Can disrupt production systems.

---

# `error`

How thoughtful

```haskell
error :: forall a. String -> a
```

???

Basically the same thing, but it takes a message.

You get unstuck, the user gets slightly more information, everyone is
happy.

--

```sh
λ> length (error "unimplemented")
*** Exception: unimplemented
```

--

```haskell
important :: Thing -> ValidatedThing
important =
  error "TODO implement important"
```

(At least we can grep for it - but who called it?!)

???
You don't really know which part of your program called it.
CallStack can help a bit, but not much.

This has all the problems that undefined has - can make it into
production.

The teams I've been on have been dogmatic about **keeping partial
functions out of production code.**

---

# Partial function JIRA

### Alternative 1: Typed holes

```haskell
important :: Thing -> ValidatedThing
important =
  _important
```

`-fdefer-type-errors` for seamless development

???

If you do happen to be using partial functions as a task tracker,
there are a few quick remedies.

Typed holes achieve a similar thing, but create a warning instead.

Makes it a bit easier to go back and fix it, and you can keep them out of production by enforcing Wall in CI.

---

# `read`

Parse or die

???

These are pretty obvious when they're visible.

The scary thing about partial functions is they are typically
hidden several layers deep.

You don't know where they'll be called from.

The only thing you can guarantee is the errors will be _garbage_.

The classic example is `read`.

--

```haskell
read :: Read a => String -> a
```

--

```haskell
λ> read "true" :: Bool
*** Exception: Prelude.read: no parse
```

???

Unfortunately, the partial functions are not just coming from inside
the house.

They're pretty much everywhere.

Base, Hackage, etc.

---

# `read`

### Solution 1: `readMaybe`

```haskell
readMaybe :: Read a => String -> Maybe a
```

---

# `read`

### Solution 2: Real parser

- Attoparsec (fast, worse errors)
- Parsec, Megaparsec (better errors)
- Alex / Happy (parser generators)
- Earley (embedded, more flexible than Parsec family)

... plus probably many, many others

???

The more robust solution is to write a real parser that you control
fully.

Unfortunately, there are about 10 ways to do this.

Lots of flexibility, but

**this is characteristic** of the Haskell ecosystem.

The obvious choice is not particularly wise in most applications.

The superior options have a lot of conceptual overhead.

We can't be surprised people stick to the easy thing.

---

# `head`, `tail`, etc

Partial functions that are always avoidable in practice

```haskell
head :: [a] -> a
tail :: [a] -> [a]
```

???

Let's talk a bit about avoiding partial functions.

We can break this down a couple of ways.

--

```haskell
myMaximum :: [Int] -> Int
myMaximum =
  head . sort
```

--

```haskell
λ> myMaximum [1..10]
10
λ> myMaximum []
*** Exception: Prelude.head: empty list
```

???

`base` contains _lots_ of partial functions.

---

# `head`, `tail`, etc

### Solution 1: Restrict input

```haskell
myMaximum :: NonEmpty Int -> Int
myMaximum =
  NE.head . NE.sort
```

???

---

# `head`, `tail`, etc

### Solution 2: Loosen output

```haskell
myMaximum :: [Int] -> Maybe Int
myMaximum xs =
  case sort xs of
    [] ->
	  Nothing
	(y:ys) ->
	  Just y
```

???

Usually I like to use Solution 1 whenever it's easy to do so.
If it takes more than 25 minutes to be constructive, give up.

There's *so many* of these in base.

I'm not going to go into them in detail.

---

# Text encoding

Is that trusted input?

```haskell
decodeUtf8 :: ByteString -> Text
decodeUtf8' :: ByteString -> Either UnicodeException Text
```

???

Spotting dangerous functions is a real art.

This one comes up really often.

It's a function in the Text package for decoding raw UTF-8.

If you don't trust the input, it'll fail.

--

```sh
λ> decodeUtf8 untrustedInput
*** Exception: Cannot decode input
```

???

Just to illustrate that it's not just `base`, here's one I hit every
day.

There's a real problem if we're going to be building reliable systems.

Libraries are able to throw these context-free errors.
the output is not necessarily actionable.

---

# Record accessors

Fine until they aren't

```haskell
data Thing =
    This { thisFirst :: Text, thisSecond :: Text }
```

--

```haskell
λ> :t thisFirst
Thing -> Text
```

--

```haskell
λ> thisFirst (This "a" "b")
"a"
```

---

# Record accessors

```haskell
data Thing =
    This { thisFirst :: Text, thisSecond :: Text }
  | That { thatFirst :: Text, thatSecond :: Int }
```

--

```haskell
λ> :t thatFirst
thatFirst :: Thing -> Text
```

--

```haskell
λ> thisFirst (That "a" 5)
*** Exception: No match in record selector thisFirst
```

---

# Record accessors

### Solution 1: Don't

Break down into additional datatypes. Low overhead

```haskell
data Thing =
    XThis This
  | XThat That

data This = This { ... }

data That = That { ... }
```

???

Low conceptual overhead.

High syntactic overhead, as we need to unbox.

---

# Record accessors

### Solution 2: Lenses

Generate lenses and prisms. High complexity overhead

???

High conceptual overhead - user must understand lenses and prisms.

Syntax can be quite concise and you gain access to the expressive
power of `lens`.

---

# `fail`

Which instance are you using?

--

```haskell
import System.IO

prompt :: IO ()
prompt = do
  "yes" <- getLine
  putStrLn "ok"
```

--

```haskell
> prompt
yes
ok
```

--

```haskell
> prompt
oij
*** Exception: user error (Pattern match failure in
    do expression at File.hs:30:15-19)
```

---

# `fail`

Which instance are you using?

--

```haskell
import Data.Attoparsec.ByteString

bool :: Parser Bool
bool = do
  str <- takeWhile isAlpha
  case str of
    "true" ->
	  pure True
    "false" ->
      pure False
    _ ->
      fail "Invalid boolean"
```

???

MonadFail affects falsifiable patterns inside do-notation,
as well as explicit `fail` calls.



---

# `fail`

### Mitigation: verify

You won't often abstract over `MonadFail`

Just check which instance you're using

---

---

class: center, middle

# Part 2: Wrong Answer

???

So thus far this has been about program crashes waiting to happen.

This section is about stuff that compiles, runs, and does the wrong
thing.


---

# `fromIntegral`

Basically `auto_cast` for Haskell integrals

```haskell
fromIntegral :: (Num b, Integral a) => a -> b
```

???

Something I look out for in code reviews.

`fromIntegral` is the "just make it compile" operator.

--

```haskell
λ> (fromIntegral :: Int32 -> Int64) 41
41
```

--

```haskell
λ> (fromIntegral :: Int64 -> Int32) maxBound
-1
λ> (fromIntegral :: Int64 -> Int32) maxBound
0
```

???

fromIntegral is dangerous because it looks innocuous!

We have to use it all the time - it's the "default coercion"

... but when going from a "larger" type to a "smaller" type,
it silently corrupts, which is rarely what we want.


---

# `filter`

Uses a predicate to do... *something*

```haskell
filter :: (a -> Bool) -> [a] -> [a]
```

???

Does it filter in, or filter out?!

inverted filters show up surprisingly often.

---

# `filter`

```haskell
accept :: (a -> Bool) -> [a] -> [a]
accept = filter

reject :: (a -> Bool) -> [a] -> [a]
reject p = filter (not . p)
```

???

Alternative names for your custom prelude

---

# `void`

Whatever you got, I don't want it

```haskell
void :: Functor f => f a -> f ()
```

--

```haskell
main :: IO ()
main =
  void worker

worker :: IO ()
worker =
  forever $ do
    ... things
```

???

Useful until the `a` changes.

---

# `void`

Whatever you got, I don't want it

```haskell
void :: Functor f => f a -> f ()
```

```haskell
main :: IO ()
main =
  void worker

worker :: IO (IO ())
worker =
  pure (print "nah")
```

**Second-order bug**: two correct changes broke the system

???

Useful until the `a` changes.

Second-order bug: a sequence of innocuous diffs broke your system.

---

# `void`

### Alternative 1: Type annotations

```haskell
main :: IO ()
main =
  (_ :: ()) <- void worker 
```

???

Couple of ways we can avoid this

--

### Alternative 2: Monomorphic shim

```haskell
main :: IO ()
main =
  runWorker worker
  
runWorker :: IO () -> IO ()
runWorker = id
```

---

# Show

A free, meaningless serialisation

```haskell
renderWorkers :: Int -> String
renderWorkers workers =
  show workers <> " workers"
```

```sh
λ> renderWorkers 5
"5 workers"
```

???

Show is the typeclass we get for free, on everything.

It's a fairly convenient facility for debugging. But, we often don't
want the polymorphism!

---

# Show

A free, meaningless serialisation

```haskell
renderWorkers :: Workers -> String
renderWorkers workers =
  show workers <> " workers"

newtype Workers = Workers Int
  deriving (Show)  
```

--

```sh
λ> renderWorkers (Workers 5)
"Workers 5 workers"
```

--

Worst example: Read / Show in optparse-applicative (`auto`)

???

So, months later, we decide to add some nice newtypes to improve our
code.

We change the outer call sites, then follow the errors until the
compiler stops complaining.

Problem: too much stuff keeps compiling.

---

# Show

A free, meaningless serialisation

### Alternative: Monomorphic functions

```haskell
renderInt :: Int -> String
renderInt = printf "%d"
```

(... or just keep Show out of databases, parsers, APIs)

---

# Lazy IO

Spooky action-at-a-distance

--

```haskell
import System.IO

cat :: FilePath -> IO ()
cat file =
  input <- withFile file ReadMode hGetContents
  hPutStr stdout input
```

--

```sh
echo "foo" > tmp
λ> cat "tmp"
*** Exception: tmp: hGetContents: illegal operation (delayed read on closed handle)
```

???

---

# Lazy IO

Solution 1: Know what is going on

```haskell
import System.IO

cat :: FilePath -> IO ()
cat file =
  withFile file ReadMode $ \handle -> do
    stuff <- hGetContents handle
	hPutStr stdout stuff
```

---

# Lazy IO

Solution 2: Use more explicit techniques

Streaming or block-at-a-time

---

# Generic programming

It always compiles

???

Generic traversals are extremely polymorphic, and thus often
insufficiently brittle.

Changes in data representation are unlikely to create compile-time
errors in your code.

Since it feels like a very advanced technique, people tend to test
these things a bit less, for whatever reason.

---

# Code too stable

It still compiles

--

```haskell
data MyRecord = MyRecord {
    firstField :: Stuff
  , secondField :: Things
  , thirdField :: New
  }

thing :: MyRecord -> IO ()
thing MyRecord{..} =
  process firstField
  process secondField
```

---

# Code too brittle

Broke everybody

--

```haskell
data Extensible =
    A
  | B
  | C

isA :: Extensible -> Bool
isA ext =
  case ext of
    A -> True
	B -> False
	C -> False
```

???

Much worse for libraries that are consumed in a variety of projects.

Usually not too painful in an application.

---

class: center, middle

# Part 3: Operational hell

---

# My process died

It worked on my laptop

--

- Space leaks
- Unbounded memory use
- Deadlock
- Too many / not enough capabilities
- Suboptimal garbage collector settings

Mostly normal software noise

---

# Strictness

Not particularly easy

- Thunks accumulate on the _outside_ of your data
    - Learn when to use `$!` and bang patterns and `seq`

---

# Strictness

Not particularly easy

- Thunks accumulate on the _inside_ of your data
    - e.g. Unread fields in a record
	- e.g. Incrementing `Maybe Int` without forcing it
	- `-XStrictData` or `!` on fields

---

# Strictness

Not particularly easy

- Libraries (and `base`) tend not to export strict data
    - Enjoy interop between your strict Maybe and everyone else's

---

# WriterT

Banned at most companies

???

WriterT leaks space always

Reimplement it in terms of StateT

How do we all know this? Blog posts and reddit

---

# Your process died

It printed a bunch of trash

--

- Terrible error messages from most of the ecosystem
- `Show` is bad for operators
- `error "impossible" -- not possible`
    - The impossible usually happens

--

Mitigations: careful explicit error handling, exit codes, design
failure messages for humans

---

class: center, middle

# Part 4: Navigating culture

---

# Effect systems

- `transformers`
- `mtl`
- `freer`
- `fused-effects`

... and about 20 other varieties

---

# Streaming

More than 10 libraries

---

# String types

`[Char], Text, ByteString` in common use

Alternatives in `foundation` and other custom cores

---

# Mixed idioms

- Deriving serialisation vs explicit
- Lens vs fclabels vs direct accessors

---

# Dialects

- Exceptions vs value-level errors
- Choice of effect system etc
- Policy on partial functions
- Policy on data strictness
- Ground types: Text / ByteString or your own thing
- Alternative compilers (SCB)

---

class: center, middle

# Part X: Coping Strategies

--

Understand

???

Understand - Haskell is very old

Many stakeholders with many different use cases

Styles over time have changed a lot -

functional styles have gone from mimicking OO to being mimicked in
many cases

--

Standardise

???

At a certain organisational scale, it is worth investing in a common core

As a team, try to cut down on decision fatigue.

Make choices and recommendations.

Make sure juniors don't have to learn too much, if you're going to
hire lots of them.

If you're not, maybe you want to go wild with advanced profunctor optics.

--

Contribute upstream

???

If you're using an interesting combination of base libraries,
you will have to write new things to make it all work.

Share them.

--

Audit

???

Always be reading the source and the diffs.

People on Hackage could smuggle partial functions into your code at
any time.

--

Talk about it

???

When I was learning Haskell I thought I'd have to know how to use
GADTs, like they were a daily thing.

The Haskell I write right now could easily be Go or Ocaml or Rust.

The teams I've been on have kept things simple.

It's useful for people to hear about this stuff.

---

# Fin

Sorry!
