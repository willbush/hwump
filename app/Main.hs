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
  game <- makeGame . R.mkStdGen <$> R.randomIO
  loopGame game

loopGame :: Game -> IO ()
loopGame game = do
  print game
  gen <- R.mkStdGen <$> R.randomIO
  case eval (update game gen) of
    GameOver FellInPit -> putStrLn "YYYIIIIEEEE... fell in a pit!"
    GameOver DeathByWumpus -> putStrLn "Tsk tsk tsk - wumpus got you!"
    BumpWumpus -> do
      putStrLn "...Oops! Bumped a wumpus!"
      loopGame $ awakenWumpus game
    SuperBatSnatch -> do
      g <- R.mkStdGen <$> R.randomIO
      let (newRoom, _) = R.randomR (minRoom, maxRoom) g
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
