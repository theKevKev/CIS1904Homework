# Functor and Foldable

In this unit, we will see how to map and fold over more than just lists, with
the help of the type class machinery that we introduced previously.

## A brief digression on kinds

Just as every expression has a type, types themselves have “types,” called
kinds. (In Haskell, there is no further level beyond kinds, i.e., kinds have
kinds, rather than a third categorization.) In GHCi, we can ask about the kinds
of types using `:kind` or `:k`. For example, let’s ask for the kind of `Int`:

```Haskell
ghci> :k Int
Int :: *
```

We see that `Int` has kind `*`. (`*` is just nice syntax for `Data.Kind.Type`.)
In fact, every type which can actually serve as the type of regular Haskell
values has kind `*`.

```Haskell
ghci> :k Bool
Bool :: *
ghci> :k Char
Char :: *
ghci> :k Maybe Int
Maybe Int :: *
```

If `Maybe Int` has kind `*`, then what about `Maybe`? Notice that there are no
values of type `Maybe`. There are values of type `Maybe Int`, and of type
`Maybe Bool`, but not of type `Maybe`. However, as a type constructor,
`Maybe` certainly is "type-like" to have a kind. So what is it?
Let’s ask `GHCi`.

```Haskell
ghci> :k Maybe
Maybe :: * -> *
```

Type constructors like `Maybe` are, in a sense, functions on types.
`Maybe` takes as input types of kind `*`, and produces another type of
kind `*`. For example, it can take as input `Int :: *` and produce the new type
`Maybe Int :: *`. (We use the syntax `::` both to say that a given value
has a certain type and that a given type (or kind) has a certain kind.)

Are there other type constructors with kind `* -> *`? Sure. For example, the
list type constructor, written `[]`, and `Tree` both have kind `* -> *`. In
general, a kind either is `*` or has the form `k1 -> k2` for some kinds
`k1` and `k2`, so we could have type constructors of kind `* -> * -> *` (as
usual, `->` is right associative, so this is equivalent to `* -> (* -> *)`),
`(* -> *) -> *`, `(* -> (* -> *)) -> *`, and so on. In practice, you will
rarely encounter particularly complex kinds: we most often see types of
kind `*` and type constructors of kind `* -> *`, with the occasional type
constructor of kind `* -> * -> *`.

## Functor

Suppose we want to generalize `map`, so that it works not only for lists:
```Haskell
(a -> b) -> [a] -> [b]
```
but also `Maybe`s:
```Haskell
(a -> b) -> Maybe a -> Maybe b
```
and `Tree`s:
```Haskell
(a -> b) -> Tree a -> Tree b
```

The essence of this mapping pattern is a higher-order function with a type like

```Haskell
(a -> b) -> f a -> f b
```
where `f` is a variable standing in for some type constructor of kind `* -> *`.
Can we write a function of this type once and for all, rather than
reimplementing it for every possible `f`?

Yes and no. There’s not much we can do if we don’t know what `f` is, so we
cannot make one implementation that works for every `f`. We can, however,
make a type class that allows us to use the same function name across every
type constructor that has such a function. More importantly, this also allows
us to define other functions that are restricted to types with a mapping
function, i.e., those built with type constructors that implement our
typeclass. Following the mathematical concept, this type class is called
`Functor`:

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

Now we can just implement this class for each particular `f`.

Note that the `Functor` class abstracts over types of kind `* -> *`. It
would make no sense to write

```Haskell
instance Functor Int where
  fmap = ...
```

This is what we want, because it makes no sense to map over an `Int`.
We only want to consider types that are in some way containers of data
of another type, such as `List`, `Maybe`, and `Tree`.

What does the typeclass instance for `Maybe` look like? Let's follow
the structure of the type and the intuition that `Maybe` is a
"container" for the data that we actually want to operate on:

```Haskell
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing = Nothing
  fmap f (Just a) = Just (f a)
```

How about lists?

```Haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap _ [] = []
  fmap f (x : xs) = f x : fmap f xs
  -- or just
  -- fmap = map
```

## Foldable

Analogously, we might want to generalize `foldr` beyond lists to any type that
represents some kind of container with lots of data inside that we can combine
together. We can accomplish this generalization with the `Foldable` type class.
`Foldable` has many required functions, but with the help of default
definitions we can define them all in terms of just one:

```Haskell
class Foldable t where
  foldr :: (a -> b -> b) -> b -> t a -> b
```

Notice that this `foldr` has almost the same type signature that we're used to
with lists, but it's generalized over any `t`. The type signature uses `t a` as
a type, so `t` must be a type constructor with kind `* -> *`. (In general, a
typeclass must be defined for types or for type constructors of one specific
kind, but that kind can be anything. If Haskell cannot infer the kind from the
signatures of the typeclass methods, it will default to treating the typeclass
as defined for types.)

We can write a `Foldable` instance for lists:

```Haskell
instance Foldable [] where
  foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr = -- same implementation we've seen previously
```

Or for trees:

```Haskell
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf = z
  foldr f z (Branch l x r) = foldr f (f x (foldr f z r)) l
```

Here, we first fold the right subtree `r`, and then combine the result with the
node value `x`, and then pass that result as the starting accumulator to fold
the left subtree `l`.

Using `Foldable`, we can write general functions like the following, which
transforms a value of *any* type that is an instance of `Foldable` into a list:

```Haskell
toList :: Foldable t => t a -> [a]
toList = foldr (:) []
```

Notice the use of the type class constraint `Foldable t =>` to enforce that `t`
implements `Foldable`.
