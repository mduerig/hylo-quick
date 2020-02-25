{-# LANGUAGE DeriveFunctor #-}

module HyloQuick
    ( qSort
    ) where

import Data.List
import Data.Functor.Foldable ( hylo )

type Algebra f c = f c -> c
type CoAlgebra f c = c -> f c

data TreeF a r = Leaf | Node a r r
  deriving Functor

split :: Ord a => CoAlgebra (TreeF a) [a]
split [] = Leaf
split (a:as) = Node a l r
  where (l, r) = partition (< a) as

join :: Algebra (TreeF a) [a]
join Leaf = []
join (Node a l r) = l ++ [a] ++ r

qSort :: Ord a => [a] -> [a]
qSort = hylo join split