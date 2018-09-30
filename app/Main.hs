{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad        (when)
import qualified Control.Monad.Random as R
import qualified Data.ByteString      as B
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
isCheatingFlag (arg:_) = arg == "cheating"
isCheatingFlag _ = False

loopGame :: Bool -> Game -> IO ()
loopGame isCheating = go
  where
    go g = do
      game <- R.evalRandIO $ updateWumpus g
      when isCheating $ print game
      case eval game of
        GameOver FellInPit -> putStrLn "YYYIIIIEEEE... fell in a pit!"
        GameOver DeathByWumpus -> putStrLn "Tsk tsk tsk - wumpus got you!"
        GameOver OutOfArrows -> putStrLn "You've run out of arrows!"
        BumpWumpus -> do
          putStrLn "...Oops! Bumped a wumpus!"
          go $ awakenWumpus game
        SuperBatSnatch -> do
          newRoom <- R.evalRandIO $ R.getRandomR (minRoom, maxRoom)
          putStrLn "Zap--Super Bat snatch! Elsewhereville for you!"
          go $ movePlayer newRoom game
        GameOn -> do
          printAdjacentRooms game
          command <- promptForCommand
          case command of
            Quit -> return ()
            Move -> do
              room <- promptForRoom $ getPlayerRoom game
              go $ movePlayer room game
            Shoot -> do
              rooms <- promptForRoomsToShoot
              ArrowTrip maybeHit traversed <- R.evalRandIO $ shoot rooms game
              mapM_ print traversed
              case maybeHit of
                HitWumpus ->
                  putStrLn $
                  "Aha! You got the Wumpus!\n" ++
                  "Hee hee hee - the Wumpus'll getcha next time!!"
                HitPlayer -> putStrLn "Ouch! Arrow got you!"
                Miss -> do
                  putStrLn "Miss"
                  go $ decrementArrowCount game

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
