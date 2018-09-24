{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Monad.Random as R
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E
import           Game
import           Text.Read            (readMaybe)

data Command = Shoot | Move | Quit

main :: IO ()
main = R.evalRandIO makeGame >>= loopGame

loopGame :: Game -> IO ()
loopGame g = do
  game <- R.evalRandIO $ update g
  print game
  case eval game of
    GameOver FellInPit -> putStrLn "YYYIIIIEEEE... fell in a pit!"
    GameOver DeathByWumpus -> putStrLn "Tsk tsk tsk - wumpus got you!"
    BumpWumpus -> do
      putStrLn "...Oops! Bumped a wumpus!"
      loopGame $ awakenWumpus game
    SuperBatSnatch -> do
      newRoom <- R.evalRandIO $ R.getRandomR (minRoom, maxRoom)
      putStrLn "Zap--Super Bat snatch! Elsewhereville for you!"
      loopGame $ movePlayer newRoom game
    GameOn -> do
      printAdjacentRooms game
      command <- promptForCommand
      case command of
        Quit -> return ()
        Shoot -> return ()
        Move -> do
          room <- promptForRoom $ getPlayerRoom game
          loopGame $ movePlayer room game

printAdjacentRooms :: Game -> IO ()
printAdjacentRooms game = do
  let adjRooms = getCurrentAdjRooms game
  putStrLn $
    concat
      [ "Tunnel leads to "
      , (show . firstRoom) adjRooms
      , " "
      , (show . secondRoom) adjRooms
      , " "
      , (show . thirdRoom) adjRooms
      ]

promptForCommand :: IO Command
promptForCommand = do
  putStrLn "Shoot, Move or Quit(S - M - Q)? "
  strippedUpperLine <- T.toUpper . T.strip . E.decodeUtf8 <$> B.getLine
  case strippedUpperLine of
    "S" -> return Shoot
    "M" -> return Move
    "Q" -> return Quit
    _   -> promptForCommand

promptForRoom :: Room -> IO Room
promptForRoom current = do
  putStrLn "Where to?"
  line <- getLine
  case readAdjacentTo current line of
    Just next -> return next
    Nothing -> do
      putStr "Not Possible - "
      promptForRoom current

readAdjacentTo :: Room -> String -> Maybe Room
readAdjacentTo current line =
  case readMaybe line of
    Just next | isAdjacent current next  -> Just next
    _                                   -> Nothing
