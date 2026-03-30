{-# LANGUAGE InstanceSigs #-}

module Exercises where

import Test.HUnit
import Prelude hiding (unzip, (<$))

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

-- Functor

{-
Write a Functor instance for Tree.
-}

instance Functor Tree where
  fmap _ Leaf = Leaf
  fmap f (Node l y r) = Node (fmap f l) (f y) (fmap f r)

{-
Use fmap to implement a function that adds three to every
element in the tree. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become Branch (Branch Leaf 4 Leaf) 5 (Branch Leaf 6 Leaf)

Your solution should work for more than just Tree Int!
-}

add3tree :: (Num a) => Tree a -> Tree a
add3tree = fmap (+ 3)

{-
To practice with the `Functor` type class, implement `fconst` and `unzip`.

Any (safe) implementation that type checks will be correct!
-}

fconst :: (Functor f) => b -> f a -> f b
fconst c = fmap . const c

unzip :: (Functor f) => f (a, b) -> (f a, f b)
unzip x = (fmap fst x, fmap snd x)

-- Foldable

{-
Write a Foldable instance for Tree.
-}

instance Foldable Tree where
  foldr _ z Leaf = z
  foldr f z (Node l x r) = foldr f z (f x (foldr f z r)) l

{-
Use foldr to implement a function that flattens a
tree into a list. For example,

Branch (Branch Leaf 1 Leaf) 2 (Branch Leaf 3 Leaf)
should become [1, 2, 3]
-}

flatten :: Tree a -> [a]
flatten x = foldr (:) [] x

-- Rose trees

{-
Up until now, the various trees we have worked with have been binary trees. A
alternative data structure is the rose tree, where each node instead has a list
of children. e.g.

```Haskell
data RoseTree a = Node a [RoseTree a]
```

Implement a `Functor` instance for `RoseTree`.

Constraint: You may **not** pattern-match over the list `ts`. Instead, you
should use the `fmap` (or equivalently, `map`) for lists.
-}

data RoseTree a = NodeR a [RoseTree a]
  deriving (Eq, Show)

instance Functor RoseTree where
  fmap f (NodeR x children) = NodeR (f x) (map (fmap f) children)

{-
Implement a `Foldable` instance for `RoseTree`.

We strongly recommend that you make use of typed holes (via underscores) to
\*incrementally* write this function. For example, we should have

```Haskell
foldr f b (Node a ts) = f _ _
```

Figure out what the first argument to `f` should be: What is its type? What do
we have of that type? Then, figure out the second argument: What is its type?
How can we (again, incrementally!) build something of that type?

Constraint: You may **not** pattern-match over the list `ts`. Instead, you
should use the `foldr` for lists.
-}

instance Foldable RoseTree where
  foldr = error "unimplemented"

{-
We can further generalize `RoseTree`. Modify `RoseTreeG` to take another type
parameter `t`, so that `NodeG`s contain not a list of children but a `t` of
children. (Note that while normal rose trees are a common and useful data
structure, this exercise is just for fun.)

Copy and paste your `Foldable` instance, and adapt it for `RoseTreeG`. You
should be able to do this in a way where you're only modifying the types and not
the implementation.
-}

-- Modify this definition.
data RoseTreeG a = NodeG a [RoseTreeG a]

-- Copy and paste your Foldable instance, and modify it for RoseTreeG.

-- Uncomment this test. It should type check and pass.
-- testRoseTreeG :: Test
-- testRoseTreeG =
--   "general-rosetree"
--     ~: [ -- a tree of Maybes
--          sum (NodeG 1 Nothing) ~?= 1,
--          sum (NodeG 1 (Just (NodeG 2 (Just (NodeG 3 Nothing))))) ~?= 6,
--          -- a tree of lists
--          foldr (:) [] (NodeG 1 [NodeG 2 [NodeG 3 []], NodeG 4 []]) ~?= [1, 2, 3, 4]
--        ]