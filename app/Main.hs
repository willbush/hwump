{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad        (when)
import qualified Control.Monad.Random as R
import qualified Data.ByteString      as B
import           Data.Foldable        (forM_)
import           Data.Maybe           (catMaybes)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as E
import           Game
import           System.Environment   (getArgs)
import           Text.Read            (readMaybe)

data Command = Shoot | Move | Quit

main :: IO ()
main = do
  args <- getArgs
  R.evalRandIO mkGame >>= loopGame (isCheatingFlag args)

isCheatingFlag :: [String] -> Bool
isCheatingFlag (arg:_) = arg == "cheat"
isCheatingFlag _       = False

loopGame :: Bool -> Game -> IO ()
loopGame isCheating = go
  where
    go g = do
      game <- R.evalRandIO $ updateWumpus g
      when isCheating $ print game
      printWarnings game
      putStrLn $ "You are in room " ++ show (_player game)
      maybeGame <- update game
      forM_ maybeGame go

update :: Game -> IO (Maybe Game)
update game =
  case eval game of
    GameOver FellInPit -> do
      putStrLn "YYYIIIIEEEE... fell in a pit!"
      return Nothing
    GameOver DeathByWumpus -> do
      putStrLn "Tsk tsk tsk - wumpus got you!"
      return Nothing
    GameOver OutOfArrows -> do
      putStrLn "You've run out of arrows!"
      return Nothing
    BumpWumpus -> do
      putStrLn "...Oops! Bumped a wumpus!"
      return $ Just (awakenWumpus game)
    SuperBatSnatch -> do
      newRoom <- R.evalRandIO $ R.getRandomR (minRoom, maxRoom)
      putStrLn "Zap--Super Bat snatch! Elsewhereville for you!"
      return $ Just (movePlayer newRoom game)
    GameOn -> do
      printAdjacentRooms game
      command <- promptForCommand
      processCommand game command

processCommand :: Game -> Command -> IO (Maybe Game)
processCommand game command =
  case command of
    Quit -> return Nothing
    Move -> do
      room <- promptForRoom $ _player game
      return $ Just (movePlayer room game)
    Shoot -> do
      rooms <- promptForRoomsToShoot
      ArrowTrip maybeHit traversed <- R.evalRandIO $ shoot rooms game
      mapM_ print traversed
      case maybeHit of
        HitWumpus -> do
          putStrLn $
            "Aha! You got the Wumpus!\n" ++
            "Hee hee hee - the Wumpus'll getcha next time!!"
          return Nothing
        HitPlayer -> putStrLn "Ouch! Arrow got you!" >> return Nothing
        Miss -> do
          putStrLn "Miss"
          return $ Just $ (awakenWumpIfFirstArrow . decrementArrowCount) game

printWarnings :: Game -> IO ()
printWarnings game = do
  let rs = getCurrentAdjRooms game
      wr = _wumpus game
      b1 = bat1 game
      b2 = bat2 game
      p1 = pit1 game
      p2 = pit2 game
  when (isNear b1 rs || isNear b2 rs) $ putStrLn "Bats nearby!"
  when (isNear p1 rs || isNear p2 rs) $ putStrLn "I feel a draft!"
  when (isNear wr rs) $ putStrLn "I Smell a Wumpus."

isNear :: Room -> AdjRooms -> Bool
isNear r (AdjRooms a b c) = a == r || b == r || c == r

printAdjacentRooms :: Game -> IO ()
printAdjacentRooms game =
  let AdjRooms a b c = getCurrentAdjRooms game
   in putStrLn $ concat ["Tunnel leads to ", show a, " ", show b, " ", show c]

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
    Just next | isAdjacent current next -> Just next
    _                                   -> Nothing

promptForRoomsToShoot :: IO [Room]
promptForRoomsToShoot = do
  putStrLn "Enter up to 5 space separated rooms to shoot: "
  line <- getLine
  let maybeRooms = map readMaybe $ words line
  if Nothing `elem` maybeRooms
    then do
      putStrLn "The given list of rooms contains one or more invalid numbers."
      promptForRoomsToShoot
    else do
      let rooms = catMaybes maybeRooms
      if anyTooCrooked rooms
        then do
          putStrLn "Arrows aren't that crooked - try another room sequence!"
          promptForRoomsToShoot
        else return rooms
