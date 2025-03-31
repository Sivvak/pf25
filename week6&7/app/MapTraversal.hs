module MapTraversal (traverseMap) where

move :: (Int, Int) -> String -> (Int, Int)
move (x, y) command = case command of
  "n" -> (x, y - 1)
  "e" -> (x + 1, y)
  "w" -> (x - 1, y)
  "s" -> (x, y + 1)
  _ -> error ("Invalid command: '" ++  command ++ "'") 

traverseMap :: (Int, Int) -> IO ()
traverseMap (x, y) = do
  command <- getLine
  let output = move (x, y) command
  if output == (3, 3)
    then putStrLn "Treasure reached!"
    else do
      putStrLn ("Coords: (" ++ show (fst output) ++ ", " ++ show (snd output) ++ ")")
      traverseMap output