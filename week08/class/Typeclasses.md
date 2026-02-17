# Introduction to Type Classes

In our polymorphism unit, we discussed _parametric polymorphism_.
We will briefly review that here before diving into
_ad-hoc polymorphism_ and one of the main forms it takes in
Haskell: type classes.

## Parametricity

Consider the type `a -> a -> a`. Remember that `a` is a type variable
which can stand for any type. What sorts of functions have this type?

Consider this:

```Haskell
f :: a -> a -> a
f x y = x && y
```

The syntax looks valid, but this does not type check.
We get an error message that type `a` can't be matched with type `Bool`.

The reason this doesn’t work is that the _caller_ of a polymorphic function
gets to choose the type. Here we, as the _implementor_, have tried to choose a
specific type (namely, `Bool`), but we may be given `String`, or `Int`, or
even a custom type, which we can’t possibly know about in advance.
In other words, you can read the type `a -> a -> a` as a promise that a
function with this type will work no matter what type the caller chooses, but
`f` only works if the type is Bool, so we get an error.

Another version of this function we could imagine is

```Haskell
f a1 a2 = case (typeOf a1) of
    Int  -> a1 + a2
    Bool -> a1 && a2
    _    -> a1
```

where `f` exhibits specific behaviors for specific types. After all, we can
easily implement this in Java using `instanceof`. In Haskell, however, there is
no equivalent of `instanceof`, so we can't write `f` this way.

The style of polymorphism we have seen so far in Haskell is
_parametric polymorphism_. We say that a function like `f :: a -> a -> a`
is _parametric_ in the type `a`, i.e., `a` is a _type parameter_ of `f`.
This means that `f` must work uniformly for any type chosen by the caller.

So, what functions actually could have the type `a -> a -> a`?
Actually, there are only two!

```Haskell
f1 :: a -> a -> a
f1 x y = x

f2 :: a -> a -> a
f2 x y = y
```

It turns out that the type `a -> a -> a` really conveys a lot of information
about `f`. Parametricity is part of the reason that just looking at the type
of a Haskell function can tell you so much about what it does.

## Type classes

Parametric polymorphism is useful in many situations, but sometimes we really
do want to be able to decide what to do based on types! For example, what about
addition? We’ve already seen that addition is polymorphic (it works on both
`Int` and `Double`, for example), but it does not work for every type,
and it has to know what type of numbers it is adding to decide what to do:
adding two `Int`s works in a different way than adding two `Double`s.
That is, unlike parametric polymorphism, it both does not have a definition
for every input type and may have different definitions for each input type
for which it is defined. This is called _ad-hoc polymorphism_, because
we define addition ad-hoc as needed for specific types.

How do we manage this?
Let's look at the type of `(+)` in GHCi:

```Haskell
Prelude> :t (+)
(+) :: Num a => a -> a -> a
```

Notice the `Num a =>` at the front. In fact, `(+)` isn’t the only standard
library function with a double-arrow in its type. Here are a few others:

```Haskell
(==) :: Eq a => a -> a -> Bool
(<) :: Ord a => a -> a -> Bool
show :: Show a => a -> String
```

`Num`, `Eq`, `Ord`, and `Show` are _type classes_, and we say that functions
like `(+)`, `(==)`, `(<)`, and `show` are "type class polymorphic."
(Note: Some people write typeclasses as one word; others write it as two.
Either spelling is fine.) Intuitively, type classes correspond to sets of types
which have certain operations defined for them, and type class polymorphic
functions work only for types which are instances of the type class(es) in
question. (In some ways, this is similar to interfaces in a language like
Java; in other ways, interfaces are more similar to ADTs. More on this below.)

As an example, let’s look in detail at the `Eq` type class.

```Haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

We can read this as follows: `Eq` is declared to be a type class with a single
type parameter `a`. Any type `a` which wants to be an instance of `Eq` must
define two functions, `(==)` and `(/=)`, with the indicated type signatures.

Consider a function like `elem`, which tests if an element is present in a list
and can be implemented as follows:

```Haskell
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem x (y : ys) = x == y || elem x ys
```

The `Eq a` that comes before the `=>` is a _type class constraint_. Having it
there allows us to use `(==)` in the body of the function because we know from
this constraint that `a` must be an instance of `Eq`, so it must have an
implementation for `(==)`. It is a type error to call the function `elem` for
a list of some type `a` which is not an instance of `Eq`, since we wouldn't
know what to do for `(==)`.

If a parametrically polymorphic type is a promise that the function will work
for whatever type the caller chooses, a type class polymorphic function is a
_constrained_ promise that the function will work for any type the caller
chooses, as long as the chosen type is an instance of the required type
class(es). (If we want to have multiple typeclass constraints on a function,
we can put them in parentheses, e.g., `foo :: (Eq a, Show a) => ... `.)
When `(==)` (or any type class method) is used, the compiler can figure out
which implementation of `(==)` should be chosen based on the inferred types
of its arguments. Note that this happens at compile time rather than at
run time!

To get a better handle on how this works in practice, let’s make our own
type and declare an instance of `Eq` for it.

```Haskell
data Foo = A Int | B Char

instance Eq Foo where
  (A i1) == (A i2) = i1 == i2
  (B c1) == (B c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)
```

It’s a bit annoying that we have to define both `(==)` and `(/=)`! Luckily,
it turns out that we actually don't need to. Type classes can give default
implementations of methods in terms of other methods, which should be used
whenever an instance declaration does not provide its own definition.
In fact, the `Eq` class is actually declared like this:

```Haskell
class Eq a where
  (==), (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```

This means that when we make an instance of `Eq`, we can define either `(==)`
or `(/=)`, whichever is more convenient; the other one will be automatically
defined in terms of the one we specify. (However, we have to be careful: if we
don’t specify either one in a given instance declaration, we get infinite
recursion and will crash any program that calls `(==)` or `(/=)` for our type.
For this reason, if you end up designing a type class, carefully consider
which if any functions to give default definitions for.)

As it turns out, `Eq` (along with a few other standard type classes) is
special: GHC is able to automatically generate instances of `Eq` for us based
on the structure of the type. Like so:

```Haskell
data Foo = A Int | B Char
  deriving (Eq, Ord, Show)
```

This tells GHC to automatically derive instances of the `Eq`, `Ord`, and `Show`
type classes for our data type `Foo`. In most cases, the derived instance
definitions do exactly what you'd expect: they traverse the data structure,
comparing constructors and calling the function being defined on subexpressions
exactly as we did in our `Eq` instance declaration for `Foo`.

## vs. Java interfaces

Type classes have a lot of similarities to Java interfaces. Both define a set
of types/classes which implement a specified list of operations. However, there
are a couple of important ways in which type classes are more general than Java
interfaces:

1. Any interfaces a Java class implements must be declared when it is defined.
    Type class instances, on the other hand, are declared separately from the
    definition of the corresponding types and can even be put in a separate
    module.

2. The types of type class methods can be more general and flexible than the
    signatures for Java interface methods, especially when multi-parameter type
    classes enter the picture. For example, consider a hypothetical type class:

    ```Haskell
    class Foo a b where
      foo :: a -> b -> Bool
    ```

    Which implementation of `foo` the compiler choses depends on both `a` and
    `b`. There is no easy way to do this in Java. Haskell type classes can
    also easily handle binary (or ternary, or...) methods, as in

    ```Haskell
    class Num a where
      (+) :: a -> a -> a
      ...
    ```

    There is no nice way to do this in Java: for one thing, one of the two
    arguments would have to be the "privileged" one which is actually getting
    the `(+)` method invoked on it, and this asymmetry is awkward.

## Standard type classes

Here are some other standard type classes you should know about:

-   `Ord` is for types whose elements can be totally ordered; that is, where
    any two elements can be compared to see which is less than the other. It
    provides comparison operations like `(<)` and `(<=)`, and also the
    `compare` function.

-   `Num` is for "numeric" types, which support things like addition,
    subtraction, and multipication. Literals that look like integers
    are actually also type class polymorphic:

    ```Haskell
    Prelude> :t 5
    5 :: Num a => a
    ```

    This means that literals like `5` can be used as `Ints`, `Doubles`, or any
    other type which is an instance of `Num`.

-   `Show` defines the method `show`, which is used to convert values into
    `String`s.

-   `Read` is the dual of `Show`; `read` converts `String`s into values.
