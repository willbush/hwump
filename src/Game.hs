{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game(..)
  , Player(..)
  , Room
  , isAdjacent
  , movePlayer
  , mkGame
  ) where

import Control.Lens (makeLenses, over, set)
import Control.Monad.Trans.State
import qualified Data.Vector as V
import System.Random

type Room = Int

data AdjacentRooms = AdjacentRooms
  { firstRoom  :: {-# UNPACK #-}!Room
  , secondRoom :: {-# UNPACK #-}!Room
  , thirdRoom  :: {-# UNPACK #-}!Room
  } deriving (Show)

newtype Game = Game
  { _player :: Player
  } deriving (Show, Eq)

data Player = Player
  { _playerRoom :: {-# UNPACK #-}!Room
  , arrowCount  :: {-# UNPACK #-}!Int
  } deriving (Show, Eq)

makeLenses ''Game

makeLenses ''Player

mkGame :: Game
mkGame = do
  let s = mkStdGen 0
      (room, _) = randomR (1, 6) s
  Game {_player = Player {_playerRoom = room, arrowCount = 10}}

-- | The game map in Hunt the Wumpus is laid out as a dodecahedron. The vertices of
-- the dodecahedron are considered rooms, and each room has 3 adjacent rooms. A
-- room is adjacent if it has a line segment directly from one vertex to another.
-- Here we have a vector of adjacent rooms where the element at an index contains
-- the adjacent rooms to the Room = index + 1. I just hard coded some valid room
-- values here for ease.
gameMap :: V.Vector AdjacentRooms
gameMap =
  V.fromList
    [ AdjacentRooms 2  5  8
    , AdjacentRooms 1  3  10
    , AdjacentRooms 2  4  12
    , AdjacentRooms 3  5  14
    , AdjacentRooms 1  4  6
    , AdjacentRooms 5  7  15
    , AdjacentRooms 6  8  17
    , AdjacentRooms 1  7  9
    , AdjacentRooms 8  10 18
    , AdjacentRooms 2  9  11
    , AdjacentRooms 10 12 19
    , AdjacentRooms 3  11 13
    , AdjacentRooms 12 14 20
    , AdjacentRooms 4  13 15
    , AdjacentRooms 6  14 16
    , AdjacentRooms 15 17 20
    , AdjacentRooms 7  16 18
    , AdjacentRooms 9  17 19
    , AdjacentRooms 11 18 20
    , AdjacentRooms 13 16 19
    ]

movePlayer :: Room -> Game -> Game
movePlayer room game =
  if isAdjacent room currentRoom
    then set (player . playerRoom) room game
    else game
  where
    currentRoom = (_playerRoom . _player) game

isAdjacent :: Room -> Room -> Bool
isAdjacent a b = isInBounds a && isInBounds b && isAdj a b
  where
    minRoom = 1
    maxRoom = V.length gameMap
    isInBounds x = x >= minRoom && x <= maxRoom
    isAdj x y =
      let adjRooms = gameMap V.! (x - 1)
       in firstRoom adjRooms == y
          || secondRoom adjRooms == y
          || thirdRoom adjRooms == y
