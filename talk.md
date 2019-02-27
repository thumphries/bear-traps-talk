class: center, middle, heading-black

# Haskell's bear traps

Tim Humphries

Formation

<a href="https://twitter.com/thumphriees">@thumphriees</a>
<a href="https://teh.id.au">teh.id.au</a>

???

Hi all, name is Tim, work at Formation right now, etc.

I've been writing Haskell for work my whole career.

This is a talk about the reams and reams of nonsense I have
absorbed that make it feasible to use Haskell for real things.

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

--

```sh
λ> length undefined
*** Exception: Prelude.undefined
```

???

Let's start with the bottom.

Haskell expressions don't always successfully evaluate.
There are two mechanisms for runtime errors: functions are
either **partial** or they **throw exceptions**.

`undefined` is the simplest partial function. It always typechecks,
and blows up whenever it is evaluated.

---

# `error`

???

Partial functions don't have to be completely useless; they can also
be mostly useless!

error lets us attach a string, but otherwise just typechecks wherever
you put it and blows your program up at runtime.


---

# `read`

Parse or die

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

The fun really starts when 

---

# `read`

### Solution 1: `readMaybe`

```haskell

```

---

# `read`

### Solution 2: Real parser

- Attoparsec (fast, worse errors)
- Parsec, Megaparsec (better errors)
- Alex / Happy (parser generators)
- Earley (embedded, more flexible than Parsec family)

... plus probably many, many others

---

# `head`, `tail`, etc

Partial functions that are always avoidable

```haskell
head :: [a] -> a
tail :: [a] -> [a]
```

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


---

# `fromIntegral`

Basically `auto_cast` for Haskell integrals

```haskell
fromIntegral :: (Num b, Integral a) => a -> b
```

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

# Show

A free, meaningless serialisation

```haskell
renderWorkers :: Int -> String
renderWorkers workers =
  show workers <> " workers"
```

```
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

```
λ> renderWorkers (Workers 5)
"Workers 5 workers"
```

???

So, months later, we decide to add some nice newtypes to improve our
code.

We change the outer call sites, then follow the errors until the
compiler stops complaining.

Problem: too much stuff keeps compiling.

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

insert properly CPS'd thing here

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

---

# Your process died

It printed a bunch of trash

--

- Terrible error messages from most of the ecosystem
- `Show` is bad for operators
- `error "impossible" -- not possible`

---

class: center, middle

# Part 4: Navigating culture

---

# Dialects

---

# One dozen effect systems

---

# String types

---

---

class: center, middle

# Part X: Strategies

---

???

Dialects and tribes - many use cases

Layering - explicit libraries with few expectations,
more libraries built on top

Defaults are for applications

At a certain scale, it is worth investing in a common core
