# Introduction to Testing

We have discussed in past lectures the importance of code correctness. Speed,
efficient use of memory, and other program properties are important, but they
make little difference if the code does not do what we want it to.
Haskell in particular is designed with correctness in mind.
Consider, for example:
* Functional code tends to look similar to math and is often close to its own
  specification
* Functional code helps enforce modularity and define easily testable units of
  code
* Pure code makes for easier testing because we need only consider inputs and
  outputs, eliminating many kinds of bugs (e.g. concurrency bugs)
* Static typing helps eliminate large classes of errors by encoding invariants
  in the types, preventing some types of bad inputs and giving the developer
  feedback on many categories of buggy program at compile time, sometimes
  even as the developer types their code in an Integrated Development
  Environment (IDE) such as VSCode.

(Laziness is the exception here, in that
it can make programs harder to reason about in exchange for improved efficiency
and the ability to work with infinite data structures.)

As helpful as these
design features are, however, they do not eliminate the need for tests. In this
unit, we consider how to write effective tests in Haskell.

## HUnit
HUnit is Haskell's primary unit testing library, modelled after Java's JUnit.
HUnit contains a datatype called
[`Test`](https://hackage.haskell.org/package/HUnit-1.6.2.0/docs/Test-HUnit-Base.html#t:Test).

We will talk more about datatypes next
week, but for now, let's look at an example of a `Test` from the previous homework.

```Haskell
exercise2 :: Test
exercise2 =
  "toRevDigits"
    ~: [ toRevDigits 1234 ~?= [4, 3, 2, 1],
         toRevDigits 0 ~?= []
       ]
```

In the example above, `toRevDigits 1234 ~?= [4, 3, 2, 1]` and
`toRevDigits 0 ~?= []` are each themselves `Test`s.
The `~?=` operator takes
two elements of a type, an "actual value" on the left and an "expected value"
on the right, and makes a test comparing the two. The `~:` operator can, among
other things, take
a `String` label and either a single `Test` or a list of `Test`s and output a labelled `Test`. (While not
strictly necessary for writing tests, we highly recommend labels to keep test
output readable.)

How can such distinct looking terms all be `Test`s? The full answer will have to wait until we've learned about algebraic datatypes and typeclasses, but for now, we can think of a `Test` in two ways. Under the hood, a `Test` is a tree whose leaves are test cases (of type `Assertion`) with optional labels at each node. Practically speaking, we can think of a `Test` as the basic unit that we can run
in the terminal and get a failure report for, and we can think of each term of the form `x ~?= y` as corresponding to one test case.

How do we do these tests? With the HUnit
function `runTestTT`.
In the terminal, run `stack ghci Exercises.hs`. In Exercises.hs,
we have defined the following example:

```Haskell
testExample1 :: Test
testExample1 =
  "example1"
    ~: [ "four" ~: example1 4 ~?= 16,
         "zero" ~: example1 0 ~?= 0
       ]
```

In the REPL (i.e., next to `ghci>`),
we can type `runTestTT testExample1` and get
results printed to the terminal.
If all the tests pass, they will look something like this:

`Cases: 2  Tried: 2  Errors: 0  Failures: 0`.

An error indicates that an exception was raised. A failure indicates
that a given test case simply did not pass, for example, if an actual
value and the corresponding expected value were not equal.

In the case of failure, the results will give further information, for example, something like this:

```
### Failure in: 0:example1:zero
expected: 0
 but got: 7
```

`example1:zero` uses the labels we have provided to tell us which case failed.

You may have noticed the following code in Homework 1:

```Haskell
main :: IO ()
main = do
  _ <-
    runTestTT $
      TestList
        [ exercise1,
          exercise2,
          exercise3,
          exercise4,
          exercise5,
          exercise6
        ]
  return ()
```
where `exercise1`, ..., `exercise1` are all `Test`s.
This is another perfectly reasonable way to organize a list
of tests. To run this, you would simply type `main` in the
REPL instead of `runTestTT <test name>`.

## Property-Based Testing
HUnit provides tooling for _unit testing_, i.e., tests that look at
how a function behaves on specific example inputs.
It turns out that Haskell is actually ideal for another form of testing:
_property-based testing_ (PBT). PBT originated with the
Haskell library [QuickCheck](https://hackage-content.haskell.org/package/QuickCheck-2.17.1.0/docs/Test-QuickCheck.html), and PBT libraries have since been developed for Python, Java, C, JavaScript, and many other languages. It generalizes the idea of unit testing by considering _properties_ that should hold for all input/output pairs. For example, suppose we are developing a library for binary search trees (BSTs), which have the invariant that all elements are distinct, and
we want to test our `delete` function. It should always be true that, after
we delete an element, it is no longer in the tree. We can write this as a QuickCheck property as follows (properties should always start with `prop_`):

```Haskell
prop_delete :: BST -> Int -> Bool
prop_delete t i = notElem i (delete t i)
```
QuickCheck will then use _generators_ for BSTs and  for Ints
to randomly generate many pairs of BSTs and Ints and check that this
property holds for all of them. (The exact number of random input values
produced is configurable; by default it is 100.)

This is very useful for many categories of test. For example,
we can use it for checking postconditions, as above. We can use it
to check that two functions are inverses of each other (e.g., we could
look at how `insert` and `delete` interact when composed); properties that test how multiple functions interact are often called _metamorphic properties_. We often use it to check particularly "mathy" properties,
such a testing that a function is _involutive_ (its own inverse) or _idempotent_ (applying it multiple times produces the same output as applying it once).  We can also
check invariants of our data structures. For example, if instead of a
custom `BST` type, we were using the regular `Tree` type, we might
want to check that the output of all our library functions always satisfies some `isValidBST` function:
```Haskell
prop_deleteValid :: Tree -> Int -> Bool
prop_deleteValid t i = isValidBST (delete t i)
```
However, if we run this, we will encounter an issue: this property
only holds if the randomly generated input is a valid BST. Most of the
time, it will not be. To address this, we can add a _precondition_:
```Haskell
prop_deleteValid :: Tree -> Int -> Bool
prop_deleteValid t i = isValidBST t ==> isValidBST (delete t i)
```
This way, QuickCheck will discard any input trees that are not
already valid BSTs. Unfortunately, because most trees are not
BSTs, it will waste a lot of time generating and discarding trees
before it manages to successfully test the desired number of valid inputs. We will discuss this issue more in a later lecture.

One common form of PBT is _model-based testing_. Suppose we have
a simple, intuitive implementation of a function `foo`, and we
would like to optimize it, but the optimized version, `foo'`, is convoluted and
we are not sure we got it right. We can check the property
`foo x === foo' x` to ensure that for all inputs, the optimized version
behaves the same as the model `foo`.

How do we use PBT in Haskell? For a prop `prop_Foo`, we run
`quickCheck prop_Foo` in GHCi. (Note that QuickCheck must be imported.) We will either get a success message:
```
ghci> quickCheck prop_Foo
+++ Ok, passed 100 tests.
```
or a failure message:
```
ghci> quickCheck prop_Foo
*** Failed! Falsified (after 9 tests and 2 shrinks):
0
```
where `0` is a counterexample it found to `prop_Foo`. We will discuss in a later lecture more advanced QuickCheck options, as well as what `shrinks` are.

## Choosing the right kind of testing

Unit testing and property-based testing each have their strengths.
Property-based testing efficiently covers far more examples than a
programmer could cover by hand. It also provides a clear specification:
the program is correct if it satisfies these properties. It can be
great for testing invariants of a custom data structure, or for specifying "math-like" properties such as two functions being inverses of each other. It can help us check a highly optimized, convoluted version of a function against an intuitive reference implementation. It's quite natural for functional programming --- it was actually first developed in Haskell. On the other hand, sometimes it can be difficult to think of properties that are not just a repeat of the program itself (for example, if we wanted to test a function that squares an input number and did not have a reference
implementation). Additionally, in this unit, we only looked at properties of lists,
but with more complicated datatypes we would have had to manually write
a function to generate arbitrary inputs. (The standard library
already has lists covered for us.) This can be quite tedious.

On the unit testing side, we have to manually write every example,
and we may easily miss edge cases. On the other hand, we can write
them quite quickly, so they can be good for the beginning stages of
a project. They are also good for situations where we really want
to focus on a specific example --- perhaps there was a previous bug
for one input that we caught, and we want to make sure it isn't
reintroduced (regression testing), or maybe we want to make sure
the behavior is correct for a specific edge case (say, an empty list).

Ultimately, programmers often use a combination of both in practice. In the rest of this class, we will continue refining our testing skills through a combination of both techniques.