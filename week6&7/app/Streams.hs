module Streams where

nats :: [Int]
nats = nats' 0 []
  where
    nats' :: Int -> [Int] -> [Int]
    nats' n ns = n : nats' (n + 1) ns