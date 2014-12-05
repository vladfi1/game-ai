module Main where

import Discrete

data Tree = Node {
  value :: Double,
  leaves :: Discrete Double Tree
}

tree = Node {
  value = 1,
  leaves = sample [(tree, 0.5), (tree, 0.5)]
}

f 0 t = value t
f n t = expectation $ fmap (f (n - 1)) (leaves t)



main = choose 50 [0..20000]
