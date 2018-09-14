{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game(..)
  , Wumpus(..)
  , Player(..)
  , mvPlayerUp
  , mvPlayerDown
  ) where

import Control.Lens (makeLenses, over)
import Control.Monad.Trans.State
import qualified Data.Vector as V
import System.Random

type RoomNum = Int

data AdjacentRooms =
  AdjacentRooms {-# UNPACK #-}!RoomNum
                {-# UNPACK #-}!RoomNum
                {-# UNPACK #-}!RoomNum

data Game = Game
  { _player :: Player
  , _wumpus :: Wumpus
  } deriving (Show)

data Player = Player
  { _playerRoom :: RoomNum
  , arrowCount :: Int
  } deriving (Show)

data Wumpus = Wumpus
  { _wumpusRoom :: RoomNum
  , isAsleep :: Bool
  } deriving (Show)

makeLenses ''Game

makeLenses ''Player

makeLenses ''Wumpus

mvPlayerUp :: Game -> Game
mvPlayerUp = over (player . playerRoom) (+ 1)

mvPlayerDown :: Game -> Game
mvPlayerDown = over (player . playerRoom) (subtract 1)

mkGame :: Game
mkGame = do
  let s = mkStdGen 0
      (room, _) = randomR (1, 6) s
  Game
    { _player = Player {_playerRoom = room, arrowCount = 10}
    , _wumpus = Wumpus {_wumpusRoom = 1, isAsleep = True}
    }

randRoom :: State StdGen RoomNum
randRoom = state $ randomR (1, 6)

-- | The game map in Hunt the Wumpus is laid out as a dodecahedron. The vertices of
-- the dodecahedron are considered rooms, and each room has 3 adjacent rooms. A
-- room is adjacent if it has a line segment directly from one vertex to another.
-- Here we have a vector of adjacent rooms where the element at an index contains
-- the adjacent rooms to the RoomNum = index + 1. I just hard coded some valid room
-- values here for ease.
map :: V.Vector AdjacentRooms
map =
  V.fromList
    [ AdjacentRooms 2 5 8
    , AdjacentRooms 1 3 10
    , AdjacentRooms 2 4 12
    , AdjacentRooms 3 5 14
    , AdjacentRooms 1 4 6
    , AdjacentRooms 5 7 15
    , AdjacentRooms 6 8 17
    , AdjacentRooms 1 7 9
    , AdjacentRooms 8 10 18
    , AdjacentRooms 2 9 11
    , AdjacentRooms 10 12 19
    , AdjacentRooms 3 11 13
    , AdjacentRooms 12 14 20
    , AdjacentRooms 4 13 15
    , AdjacentRooms 6 14 16
    , AdjacentRooms 15 17 20
    , AdjacentRooms 7 16 18
    , AdjacentRooms 9 17 19
    , AdjacentRooms 11 18 20
    , AdjacentRooms 13 16 19
    ]
