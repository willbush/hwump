{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game(..)
  , Player(..)
  , Wumpus(..)
  , AdjRooms(..)
  , EvalResult(..)
  , DeathType(..)
  , ShootResult(..)
  , Room
  , isAdjacent
  , movePlayer
  , decrementArrowCount
  , moveWumpus
  , awakenWumpus
  , getPlayerRoom
  , getCurrentAdjRooms
  , makeGame
  , maxRoom
  , minRoom
  , eval
  , update
  , shoot
  ) where

import           Control.Lens          (makeLenses, set)
import qualified Control.Monad.Random  as R
import qualified Data.Vector           as V
import           System.Random.Shuffle (shuffleM)

type Room = Int

data AdjRooms = AdjRooms !Room !Room !Room

data Game = Game
  { _player :: !Player
  , _wumpus :: !Wumpus
  , pit1    :: !Room
  , pit2    :: !Room
  , bat1    :: !Room
  , bat2    :: !Room
  } deriving (Show, Eq)

data Player = Player
  { _playerRoom :: !Room
  , _arrowCount :: !Int
  } deriving (Show, Eq)

data Wumpus = Wumpus
  { _wumpusRoom :: !Room
  , _isSleeping :: !Bool
  } deriving (Show, Eq)

data EvalResult
  = GameOver DeathType
  | BumpWumpus
  | SuperBatSnatch
  | GameOn
  deriving (Show, Eq)

data DeathType
  = FellInPit
  | DeathByWumpus
  | OutOfArrows
  deriving (Show, Eq)

data ShootResult
  = HitWumpus
  | Suicide
  | Miss
  deriving (Show, Eq)

makeLenses ''Game

makeLenses ''Player

makeLenses ''Wumpus

makeGame :: (R.RandomGen g) => R.Rand g Game
makeGame = do
  shuffledRooms <- shuffleM [minRoom .. maxRoom]
  let randRooms = V.fromList $ take 6 shuffledRooms
  return
    Game
      { _player = Player {_playerRoom = randRooms V.! 0, _arrowCount = 5}
      , _wumpus = Wumpus {_wumpusRoom = randRooms V.! 1, _isSleeping = True}
      , pit1 = randRooms V.! 2
      , pit2 = randRooms V.! 3
      , bat1 = randRooms V.! 4
      , bat2 = randRooms V.! 5
      }

-- | Evaluates the current state of the game
eval :: Game -> EvalResult
eval game
  | pr == pit1 game || pr == pit2 game = GameOver FellInPit
  | pr == bat1 game || pr == bat2 game = SuperBatSnatch
  | not wumpIsSleeping && pr == wr     = GameOver DeathByWumpus
  | wumpIsSleeping && pr == wr         = BumpWumpus
  | (_arrowCount . _player) game == 0  = GameOver OutOfArrows
  | otherwise                          = GameOn
  where
    pr = getPlayerRoom game
    wr = (_wumpusRoom . _wumpus) game
    wumpIsSleeping = wumpusIsSleeping game

update :: (R.RandomGen g) => Game -> R.Rand g Game
update game = do
  n <- R.getRandomR (1 :: Int, 4)
  let wumpusFeelsLikeMoving = n > 1
  if not (wumpusIsSleeping game) && wumpusFeelsLikeMoving
    then do
      r <- getRandAdjRoomToWumpus game
      return $ moveWumpus r game
    else return game

shoot :: [Room] -> Game -> ShootResult
shoot [] _ = Miss
shoot (room:rooms) game
  | room == wr = HitWumpus
  | room == pr = Suicide
  | otherwise  = shoot rooms game
  where
    wr = (_wumpusRoom . _wumpus) game
    pr = getPlayerRoom game

getRandAdjRoomToWumpus :: (R.RandomGen g) => Game -> R.Rand g Room
getRandAdjRoomToWumpus game = do
  let AdjRooms a b c = getAdjRoomsTo $ (_wumpusRoom . _wumpus) game
  shuffledRooms <- shuffleM [a, b, c]
  return $ head shuffledRooms

-- | The game map in Hunt the Wumpus is laid out as a dodecahedron. The vertices of
-- the dodecahedron are considered rooms, and each room has 3 adjacent rooms. A
-- room is adjacent if it has a line segment directly from one vertex to another.
-- Here we have a vector of adjacent rooms where the element at an index contains
-- the adjacent rooms to the Room = index + 1. I just hard coded some valid room
-- values here for ease.
gameMap :: V.Vector AdjRooms
gameMap =
  V.fromList
    [ AdjRooms 2  5  8
    , AdjRooms 1  3  10
    , AdjRooms 2  4  12
    , AdjRooms 3  5  14
    , AdjRooms 1  4  6
    , AdjRooms 5  7  15
    , AdjRooms 6  8  17
    , AdjRooms 1  7  9
    , AdjRooms 8  10 18
    , AdjRooms 2  9  11
    , AdjRooms 10 12 19
    , AdjRooms 3  11 13
    , AdjRooms 12 14 20
    , AdjRooms 4  13 15
    , AdjRooms 6  14 16
    , AdjRooms 15 17 20
    , AdjRooms 7  16 18
    , AdjRooms 9  17 19
    , AdjRooms 11 18 20
    , AdjRooms 13 16 19
    ]

minRoom :: Room
minRoom = 1;

maxRoom :: Room
maxRoom = 20;

getCurrentAdjRooms :: Game -> AdjRooms
getCurrentAdjRooms = getAdjRoomsTo . getPlayerRoom

getPlayerRoom :: Game -> Room
getPlayerRoom = _playerRoom . _player

movePlayer :: Room -> Game -> Game
movePlayer = set (player . playerRoom)

decrementArrowCount :: Game -> Game
decrementArrowCount game =
  let current = (_arrowCount . _player) game
   in set (player . arrowCount) (current - 1) game

moveWumpus :: Room -> Game -> Game
moveWumpus = set (wumpus . wumpusRoom)

awakenWumpus :: Game -> Game
awakenWumpus = set (wumpus . isSleeping) False

wumpusIsSleeping :: Game -> Bool
wumpusIsSleeping = _isSleeping . _wumpus

isAdjacent :: Room -> Room -> Bool
isAdjacent r1 r2 = isInBounds r1 && isInBounds r2 && isAdj r1 r2
  where
    isInBounds x = x >= minRoom && x <= maxRoom
    isAdj x y =
      let AdjRooms a b c = getAdjRoomsTo x
       in a == y || b == y || c == y

getAdjRoomsTo :: Room -> AdjRooms
getAdjRoomsTo r = gameMap V.! (r - 1)
