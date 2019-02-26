class: center, middle, heading-black

# Haskell's bear traps

Tim Humphries

Formation

<a href="https://twitter.com/thumphriees">@thumphriees</a>
<a href="https://teh.id.au">teh.id.au</a>

???

---

class: center, middle

# Part 1: Failure


---

# `undefined`

???

Let's start with the bottom.

Haskell expressions don't always successfully evaluate.
There are two mechanisms for runtime errors: functions are
either **partial** or they **throw exceptions**.

`undefined` is the simplest partial function. It always typechecks,
and blows up whenever it is evaluated.

--

a.k.a. *bottom*, the root of all runtime errors

```haskell
undefined :: forall a. a
```

--

```sh
λ> length undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
  undefined, called at <interactive>:2:8 in interactive:Ghci1
```

--

```haskell
important :: Thing -> Other Thing
important thing =
  fmap undefined (unpack thing)
```

---

# `error`

---

# `read`

---

# Record accessors

---
