# Monads

## Motivation

The concept of monad started out as an abstract bit of mathematics that
turned out to be incredibly useful for functional programming. In Haskell,
this concept takes the form of a typeclass.

A monad is handy whenever a programmer wants to sequence actions. (Sometimes it
is useful to think of each monad as corresponding to a type of _side effect_;
other times this perspective may feel contrived, so don't worry about it
too much.) We’ve already learned about `Applicative`, which runs two actions in
parallel and combines their results. `Monad` allows the second action to depend
on the result value of the first.

The best way to really understand monads is to work with them for a
while. After programming using several different monads, you’ll be able to
abstract away the essence of what a monad really is.

## Monad

The `Monad` type class is defined as follows:

```Haskell
class Monad m where
  return :: a -> m a

  (>>=) :: m a -> (a -> m b) -> m b

  (>>) :: m a -> m b -> m b
  m1 >> m2 = m1 >>= \_ -> m2
```

`>>=` is pronounced "bind", and `>>` is often pronounced "and then". (`(>>)` is
just a specialized version of `(>>=)` that discards the result value. It is
included in the `Monad` class in case some instance wants to provide a more
efficient implementation, but usually the default implementation is just fine.)

`(>>=)` is where all the action is. Let’s think carefully about its type:
 `m a -> (a -> m b) -> m b`.

`(>>=)` takes two arguments. The first one is a value of type `m a`. (Note that
from this Haskell can infer that `m` must be a type constructors with one
argument.) These are sometimes called monadic values, or computations. The one
thing we must not call them is "monads," since that is a kind error: the type
constructor `m` is a monad. The idea is that an action of type `m a` represents
a computation which results in a value (or several values, or no values) of type
`a`, and may also have some sort of "side effect":

-   `c1 :: Maybe a `is a computation which might fail but produces an `a` if it
    succeeds.
-   `c2 :: [a]` is a computation which produces 0 or more `a`s. This models
    nondeterminism. (Of course, this does not reflect how we often
    conceptualize lists; that is ok! The important thing is to notice how
    `return` and `bind` can be useful when working with lists.)
-   `c3 :: IO a` is a computation which potentially has some I/O effects and
    then produces an `a`.
-   `c4 :: Gen a` is a computation which has access to a source of
    pseudo-randomness and produces an `a`.

And so on. Now, what about the second argument to `(>>=)`? It is a function of
type `(a -> m b)`. That means it can take the result(s) of the previous
computation and use them to construct the next. This is precisely what it means
for a monad to be a kind of computation that can be sequenced.

All `(>>=)` really does is put together two actions to produce a larger one,
which, when executed, first runs one and then the other, returning the result
of the second one. The all-important twist is that we get to decide which
action to run second based on the output from the first.

The default implementation of `(>>)` should make sense now: `m1 >> m2` simply
does `m1` and then `m2`, ignoring the result of `m1`.

What about `return`? We can think of it as similar to (though distinct from)
`const`: it takes a value and returns a computation that produces that value.
Put another way, it takes a value and returns a representation of that value
in the monad.

## Examples

Let’s start by writing a `Monad` instance for `Maybe`:

```Haskell
instance Monad Maybe where
  return  = Just

  Nothing >>= _ = Nothing
  Just x  >>= k = k x
```

`return` is `Just`, because `Just x` is a representation of a computation that
successfully produces `x`. `(>>=)` has two cases: if its first argument is
`Nothing` (i.e., the previous computation failed), then the whole computation
fails (i.e., produces `Nothing`). Otherwise, if its first argument is `Just x`,
we apply the second argument to `x` to decide what to do next.

It is conventional, but not necessary, to use the letter `k` for the second
argument of `(>>=)`.

Some examples:

```Haskell
checkLt10 :: Int -> Maybe Int
checkLt10 n
  | n < 10 = Just n
  | otherwise = Nothing

halve :: Int -> Maybe Int
halve n
  | even n = Just (n `div` 2)
  | otherwise = Nothing

ex1 :: Maybe Int
ex1 = check 7 >>= halve

ex2 :: Maybe Int
ex2 = check 12 >>= halve
```

We can also write this with `do` notation:

```Haskell
ex1' = do
  checked <- check 7
  halve checked

ex2' = do
  checked <- check 12
  halve checked
```

(Think about what these examples should evaluate to!)

How about a `Monad` instance for the list constructor `[]`?

```Haskell
instance Monad [] where
  return x = [x]
  xs >>= k = concat (map k xs)
```

If we view lists as a model of nondeterminism, then `[x]` is a computation that
deterministically produces `x`. `>>=` is a bit more complicated; it helps to
remember that we are modelling all the possible paths a nondeterministic
computation can take. From any given `x` in `xs`, when we apply `k` we get a
list of values that can appear following that `x` in the computation tree. If
we want to know all possible values we could have at this point in the
computation, not just those preceded by that `x`, we concatenate all those
lists together.

A simple example:

```Haskell
addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

ex3 = do
  num <- [10, 20, 30]
  addOneOrTwo num
```

`num` is non-deterministically selected from `[10, 20, 30]`, and then we
non-deterministically add `1` or `2` to it. The result is a list of six
elements representing all possible results.

This non-determinism can be made even more apparent through the use of the
function guard, which cuts off a computation path if the argument to `guard`
evaluates to `False` along that path:

```Haskell
ex4 = do
  num <- [1..20]
  guard (even num)
  guard (num `mod` 3 == 0)
  return num
```

Here, we can think of choosing `num` from the range `1` through `20`, and then
checking if it is even and divisible by `3`.

The full type of guard is `MonadPlus m => Bool -> m ()`. `MonadPlus` is another
type class that characterizes monads that have a possibility of failure. These
include `Maybe` and `[]`. `guard` takes a boolean value and either fails or
succeeds but produces no useful result. That’s why its return type is `m ()` –
no new data comes out from it, it just determines whether the computation cuts
off or continues.

## Monad combinators

One nice thing about the Monad class is that, using only `return` and `(>>=)`,
we can build up a lot of nice general combinators for programming with monads.

For example, `sequence` takes a list of monadic values and produces a single
monadic value which collects the results. What this means depends on the
particular monad. For example, in the case of `Maybe` it means that the entire
computation succeeds only if all the individual ones do; in the case of `IO` it
means to run all the computations in sequence; and so on.

```Haskell
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma : mas) =
  ma >>= \a ->
  sequence mas >>= \as ->
  return (a : as)
```

We'll see several more examples of monad combinators in class and in the
homework.

## List comprehensions

The monad for lists gives us a new notation for list building that turns out to
be quite convenient. Building lists using monad-like operations is so useful
that Haskell has a special syntax for it, called list comprehensions. It is
best shown by examples:

```Haskell
evensUpTo100 :: [Int]
evensUpTo100 = [n | n <- [1..100], even n]

-- inefficient, but it works
primes :: [Int]
primes = [p | p <- [2..],
              all ((/= 0) . (p `mod`)) [2..p-1] ]
```

List comprehensions work just like set-builder notation you may have learned in
a math class. In a list comprehension, the statements to the right of the `|`
are run in order. A statement with a `<-` selects an
element from a list. Statements without `<-` are boolean expressions; if the
expression is `False`, then the current choice of element is discarded.

There is a straightforward translation between list comprehensions and `do`
notation:

```Haskell
[a | b <- c, d, e, f <- g, h]
```

is exactly equivalent to

```Haskell
do
  b <- c
  guard d
  guard e
  f <- g
  guard h
  return a
```

Note that, in the translation, lists aren’t mentioned anywhere! With the GHC
language extension `MonadComprehensions`, you can actually use list
comprehension notation for any monad, but in practice it is mostly used for
lists.
