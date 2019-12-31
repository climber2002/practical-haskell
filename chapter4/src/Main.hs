module Main where

import qualified Data.Map as M
import Data.Tree

main :: IO ()
main = putStrLn "Hello, Haskell!"

preOrder :: (a -> b) -> Tree a -> [b]
preOrder f (Node v subTrees)
  = let subtreesTraversed = concat $ map (preOrder f) subTrees
    in f v : subtreesTraversed
