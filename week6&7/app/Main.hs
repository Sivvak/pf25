module Main (main) where

import MapTraversal (traverseMap)
import System.Environment (getArgs)

main :: IO ()
-- 1) main = traverseMap (0, 0)
-- 2) main = do
--      args <- getArgs
--      putStrLn $ unlines args
main = if cond then print "True" else print "False"
  where
    cond = nondec ([] :: [Int])

nondec :: (Ord a) => [a] -> Bool
nondec xs = and (map leq pairs)
  where
    leq (x, y) = x <= y
    pairs = zip xs (tail xs)