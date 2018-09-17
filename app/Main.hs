{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.Encoding as E
import           Game
import           Text.Read          (readMaybe)

data Command = Shoot | Move | Quit

main :: IO ()
main = loopGame makeGame

loopGame :: Game -> IO ()
loopGame game = do
  print game
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
  strippedUpperLine <- (T.toUpper . T.strip . E.decodeUtf8) <$> B.getLine
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
    Just next ->
      if isAdjacent next current
        then Just next
        else Nothing
    Nothing -> Nothing
