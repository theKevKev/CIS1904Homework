# Polymorphism

## Polymoprphic data types

Recall this data type from the algebraic data type reading:

```Haskell
data FailableDouble
  = Failure
  | OK Double
```

We have on several occasions seen situations like this,
where we either return a value or some sort of failure.
It turns out Haskell has an abstraction for
this pattern:

```Haskell
data Maybe a
  = Nothing
  | Just a
```

A `Maybe a` is, possibly, an `a`. It's an abstraction that helps us represent
_partial computation_, i.e., computations that may have no result on some
inputs. (For those familiar with `option` in OCaml, it is the same idea.)

Instead of `FailableDouble`, we could use `Maybe Double` to indicate that when
a computation succeeds, it will produce a `Double`,
but it may also fail on a given input, resulting in `Nothing`.

We discussed in the algebraic data type material that `data` is a keyword for
defining _type constructors_. Most of the type constructors we have seen so far
have had no arguments, so we just called them types. `Maybe`, however, is a
type constructor with an argument, `a`; `Maybe` on its own is not a type. To
make it a type, we must supply it with another type, like `Double`. We can
think of this as similar to function application: when we supply `Maybe` with
a argument, we simply replace all uses of `a` in `Maybe`’s definition with
the argument, so a `Maybe Double` is either `Nothing` or `Just x` for some
`x :: Double`.
We've seen many examples of substituting terms in for
other terms, for example with data constructors. Here we just apply this same
principle to types.

Here is some sample code using `Maybe`:

```Haskell
toInt :: Maybe Int -> Int -> Int
toInt (Just n) _ = n
toInt Nothing default = default

safeDiv :: Double -> Double -> Maybe Double
safeDiv _ 0 = Nothing
safeDiv m n = Just (m / n)
```

Let’s look at another example of a type constructor with an argument:

```Haskell
data List a
  = Nil
  | Cons a (List a)
```

Given a type `a`, a `List a` consists of either the constructor `Nil`, or the
constructor `Cons` along with a value of type `a` and another `List a`.
Here is an example:

```Haskell
sumInts :: List Int -> Int
sumInts Nil = 0
sumInts (Cons h tl) = h + sumInts tl
```

This `List` type we have defined here is exactly like Haskell's standard
library list type, only without special
syntax. In fact, when you say `[Int]` in a type, that really means `[] Int`
— allowing you to put the brackets around the `Int` is just nice syntactic
sugar that gets converted to `[] Int` during compilation.

## Polymorphic functions

A symbol is polymorphic if it can have any of a variety of different types.
There are two main forms of polymorphism in Haskell: _parametric polymorphism_
and _ad-hoc polymorphism_. (Other forms of polymorphism exist, such as
subtyping, but these are less directly supported in Haskell.) We will talk
about ad-hoc polymorphism in our typeclasses lecture, so here we focus on
parametric polymorphism.

In parametric polymorphism, we have type _parameters_ that can have any type
substituted in for them. If a function expects a `List a`, we can pass it
a `List Int`, `List String`, `List (List Nat)`, and so on. This means that the
function cannot consider type-specific information about its input. For
example, suppose we have a function `f :: a -> a`. This function can only do
thing: return its input! Any other behavior would require knowing what `a` is,
and we do not in `f`. Similarly, there are no meaningful terms of type `a`;
there is no meaningful term that has every type! (We specify "meaningful"
because there are various error terms that have this type: `undefined`, for
example, is defined as having every type.)

Let’s say we want to retrieve the first element of a list, but we need to be
able to return something if we have an empty list. The idiomatic way of doing
this in Haskell involves a `Maybe`.
The type of the list elements does not matter here, so we can give this
function the type `[a] -> Maybe a`. `safeHead` is thus a polymorphic function,
because it manipulates polymorphic data.

```Haskell
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x : _) = Just x
```

## Total and partial functions

Unfortunately, the version of this function in Prelude, called `head`, instead
has type `[a] -> a`. It crashes when given an empty list!

`head` is a _partial function_: there are certain inputs for which `head` will
crash. Functions that recurse infinitely on some inputs are also considered
partial. Functions which are well-defined (i.e., produce an output)
on all possible inputs are called _total functions_.

Avoiding partial functions is good practice in any programming language (and
for the rest of this class, you may not use partial functions unless specified
otherwise). In Haskell, structures like `Maybe` make this easier.

In addition to `head`, other Prelude partial functions include `tail`, `init`,
`last`, and `(!!)`. You should avoid using partial functions.
What to do instead?

Often uses of partial functions like `head`, `tail`, and so on can be replaced
by pattern-matching. Consider the following two definitions:

```Haskell
doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1 : x2 : _) = x1 + x2
```

These functions compute exactly the same result, and they are both total. The
second, however, is easier to read, and because it contains no partial
functions, we can be sure that it will not crash even if we accidentally
mix up our cases.

We can also use alternatives like `safeHead` if we truly need the functionality.
Why is this a good idea?

1. `safeHead` will never crash.
2. The type of `safeHead` makes it obvious that some inputs conceptually have no
    output for this function, hence the need for `Maybe`. The type of `head`,
    conversely, gives no indication that it may fail.
3. The type system ensures that users of `safeHead` must appropriately check
    the return value of `safeHead` to see whether they got a value or
    `Nothing`.
