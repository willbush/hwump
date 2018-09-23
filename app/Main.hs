{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           Game
import qualified System.Random      as R
import           Text.Read          (readMaybe)

data Command = Shoot | Move | Quit

main :: IO ()
main = do
  gen <- mkGen
  loopGame $ makeGame gen

loopGame :: Game -> IO ()
loopGame g = do
  game <- update g <$> mkGen
  print game
  case eval game of
    GameOver FellInPit -> putStrLn "YYYIIIIEEEE... fell in a pit!"
    GameOver DeathByWumpus -> putStrLn "Tsk tsk tsk - wumpus got you!"
    BumpWumpus -> do
      putStrLn "...Oops! Bumped a wumpus!"
      loopGame $ awakenWumpus game
    SuperBatSnatch -> do
      gen' <- mkGen
      let (newRoom, _) = R.randomR (minRoom, maxRoom) gen'
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

mkGen :: IO R.StdGen
mkGen = R.mkStdGen <$> R.randomIO

printAdjacentRooms :: Game -> IO ()
printAdjacentRooms game = do
  let adjRooms = getCurrentAdjRooms game
      a = firstRoom adjRooms
      b = secondRoom adjRooms
      c = thirdRoom adjRooms
  putStrLn $ concat ["Tunnel leads to ", show a, " ", show b, " ", show c]

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
    Just next | isAdjacent next current -> Just next
    _                                   -> Nothing
