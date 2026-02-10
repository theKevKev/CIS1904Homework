# Higher-Order Patterns

## Function composition, operator sections, and eta reduction

We have discussed how functions are _first-class_ in Haskell, i.e., they can be
passed around as data. A function that takes in or returns another function is
said to be _higher-order_. We have briefly looked at functions such as `map`
that take other functions as inputs; in this unit, we will focus on functions
that output other functions.

Note: the example below includes an _anonymous_ function, where we _bind_ a
a variable `x` with the syntax `\x` and then use `->` to inducate that what
follows is the function _body_. This corresponds to the syntax `fun x -> ...`
in OCaml. (For multi-argument functions we write `\x y z -> ...`.)
For historical reasons (look up the lambda calculus if you're
curious!), we read the backslash before the variable as "lambda".

Consider this implementation of composition for unary integer operators
(i.e., functions of type `Int -> Int`).


```Haskell
compose :: (Int -> Int) -> (Int -> Int) -> (Int -> Int)
compose f g = \x -> f (g x)
```

Note how this function both takes in and outputs functions.

In the standard library, we use the shorthand `(.)` for function
composition. That is, if `f` and `g` are functions, then `f . g` is the
function which first does `g` and then `f`. This shorthand and function
composition in general can help programmers write concise, elegant code. It
fits well into the “wholemeal” programming style encouraged by Haskell's
design, where we think about composing together successive high-level
transformations of an entire data structure, rather than incrementally
changing one small piece of the structure at a time.

Consider the following function:

```Haskell
myTest :: [Int] -> Bool
myTest xs = even (length (filter (\x -> x == 0) xs))
```

We can rewrite this as:

```Haskell
myTest' :: [Int] -> Bool
myTest' xs = (even . length . filter (\x -> x == 0)) xs
```

This version makes it clear that `myTest'` is reall just a “pipeline” composed
of three smaller functions. (We will see in a moment how writing it this way
also allows us to remove parentheses, reducing visual clutter.)

Next, we can simplify `myTest'` further using _operator sections_.

```Haskell
myTest'' :: [Int] -> Bool
myTest'' xs = (even . length . filter (== 0)) xs
```

For any binary infix operator `?` (i.e., a function `?` that takes two
arguments and is written between its arguments), we can use `(? y)` as
a shorthand for `\x -> x ? y` and `(y ?)` as a shorthand for
`\x -> y ? x`. (The function must be binary and infix so we can distinguish
where in the argument order the omitted argument is. For example, if
`?` were prefix, we could not distinguish `\x -> ? x y` from `\x -> ? y x`
when writing it as an operator section.)

Finally, notice how both the function we defined, `myTest''`, and its
definition, `(even . length . filter (== 0))` appear in the same position
relative to the argument. That is, given any integer list `xs`, `myTest''`
and `(even . length . filter (== 0))` do the same thing to `xs`, by
definition. This means that we can actually omit the argument and define
our function directly in terms of `(even . length . filter (== 0))`:

```Haskell
myTest''' :: [Int] -> Bool
myTest''' = even . length . filter (== 0)
```

This process of removing extraneous arguments is called _eta reduction_.
Another way of looking at it is that `f x = ...` is always just syntactic
sugar for `f = \x -> ...`, so if we have something of the form
`f = \x -> g x`, we might as well write `f = g` instead.

## Partial application

Consider a function like this:

```Haskell
f :: Int -> Int -> Int
f x y = 2 * x + y
```

The type of multi-argument functions in Haskell may seem strange compared to
many other languages. Why all the arrows, instead of something like
`Int Int -> Int`? The reason is actually fundamental to the nature of functions
in Haskell: all functions in Haskell take exactly one argument!

How can the function `f` shown above take two arguments then?
The answer is that it actually doesn’t: it takes one argument (of type `Int`)
and outputs a function (of type `Int -> Int`); that function then takes one
argument and returns the final answer. When we refer to a function as
having multiple arguments in Haskell, we actually mean that it returns a
function.

In fact, we can equivalently write `f`’s type like this:

```Haskell
f :: Int -> (Int -> Int)
```

Note that function arrows associate to the right, that is, `W -> X -> Y -> Z`
is equivalent to `W -> (X -> (Y -> Z))`. We can always add or remove
parentheses around the rightmost top-level arrow in a type.

Function application, in turn, is left-associative. That is, `f 3 4` is really
shorthand for `(f 3) 4`. This makes sense given what we said previously: we
apply `f` to an argument `3`, which returns a function that takes an `Int`
and adds `4` to it.

The “multi-argument” anonymous function `\x y z -> ...` is really just
syntactic sugar for `\x -> (\y -> (\z -> ...))`.

The type of our composition function can equivalently be written as
`(Int -> Int) -> (Int -> Int) -> Int -> Int`, and we can write `x` on the other
side of the `=`:

```Haskell
compose' :: (Int -> Int) -> (Int -> Int) -> Int -> Int
compose' f g x = f (g x)
```

This idea of representing multi-argument functions as one-argument functions
returning other functions is known as _currying_. (The opposite direction,
where we take a one-argument function that returns another function and
convert it to a function that takes a tuple, e.g. converting
`f :: Int -> Int -> Int` to `f' :: (Int, Int) -> Int`, is called _uncurrying_.)

The fact that functions in Haskell are curried makes _partial application_
particularly easy. We _partially apply_ a function when we supply some of its
arguments and get a function as the result.
For example, suppose we want a function that filters a list of integers for
elements greater than 100. We can partially apply the standard library
list filter function by supplying just its first argument:

```Haskell
greaterThan100 :: [Int] -> Int
greaterThan100 = filter (> 100)
```

Because we did not supply any list argument, the result is a function
that takes a list of integers and filters it for elements that are greater
than 100, as desired.

In Haskell, it is very natural to partially apply functions to their first
argument or first few arguments in this way, but if we want to partially apply
a function out of order (e.g., by supplying its second argument without
supplying its first argument), it becomes much
trickier. (The one exception is infix operators, which as we’ve seen, can
be partially applied to either of their two arguments using an operator
section.) This means that when defining multi-argument functions, it is
important to consider which arguments it would be most useful to partially
apply the function to and put those arguments first. For example, there are
many situations where we might want to define a filter function for a specific
criterion that can be used on many lists, as above. It is far less common to
need a filter function on one list that can be used with many filter criteria,
so it makes sense for `filter`'s function argument to come first.

## Another example

Let's tie some of the things we've learned together. Consider

```Haskell
foobar :: [Int] -> Int
foobar [] = 0
foobar (x : xs)
  | x > 3     = (7 * x) + foobar xs
  | otherwise = foobar xs
```

This function compiles just fine but it is not good Haskell style. First,
it is doing too many things at the same time, so it can be difficult to
determine at a glance what it is supposed to do. Second, it is operating
at too low a level, modifying individual list elements instead of transforming
the whole input at once. Here’s a much more idiomatic implementation, using
partial application, operator sections, and composition via `.`:

```Haskell
foobar :: [Int] -> Int
foobar = sum . map (7 *) . filter (> 3)
```

This defines `foobar` as a “pipeline” of three functions: first, we throw away
all elements from the list which are not greater than three; next, we apply an
arithmetic operation to every element of the remaining list; finally, we su
the results. Not only is the code more concise, but it is much easier to tell
at a glance what it does.
