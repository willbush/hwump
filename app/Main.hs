module Main where

import Game

main :: IO ()
main = do
  putStrLn "enter q to quit"
  putStrLn "otherwise enter a number to move the player to."
  loopGame mkGame

loopGame :: Game -> IO ()
loopGame game = do
    print game
    line <- getLine
    print line
    if line == "q"
      then return ()
      else loopGame $ movePlayer (read line :: Int) game
