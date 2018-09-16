module Main where

import Game

main :: IO ()
main = do
  putStrLn "enter q to quit"
  putStrLn "enter u to increment and d to decrement the player's room number."
  loopGame mkInitialGame

mkInitialGame :: Game
mkInitialGame =
  Game
    { _player = Player {_playerRoom = 0, arrowCount = 10}
    }

loopGame :: Game -> IO ()
loopGame g = do
    print g
    line <- getLine
    print line
    if line == "q"
      then return ()
      else loopGame $ updateGame g line

updateGame :: Game -> String -> Game
updateGame g "u" = mvPlayerUp g
updateGame g "d" = mvPlayerDown g
updateGame g _ = g
