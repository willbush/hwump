{-# LANGUAGE TemplateHaskell #-}

module Game
  ( Game(..)
  , Player(..)
  , AdjacentRooms(..)
  , Room
  , isAdjacent
  , movePlayer
  , getPlayerRoom
  , getCurrentAdjRooms
  , makeGame
  ) where

import           Control.Lens          (makeLenses, set)
import qualified Data.Vector           as V
import qualified System.Random         as R
import           System.Random.Shuffle (shuffle')

type Room = Int

data AdjacentRooms = AdjacentRooms
  { firstRoom  :: {-# UNPACK #-} !Room
  , secondRoom :: {-# UNPACK #-} !Room
  , thirdRoom  :: {-# UNPACK #-} !Room
  } deriving (Show)

data Game = Game
  { _player :: {-# UNPACK #-} !Player
  , _pit1   :: {-# UNPACK #-} !Room
  , _pit2   :: {-# UNPACK #-} !Room
  } deriving (Show, Eq)

data Player = Player
  { _playerRoom :: {-# UNPACK #-} !Room
  , arrowCount  :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

makeLenses ''Game

makeLenses ''Player

makeGame :: R.StdGen -> Game
makeGame g =
  let maxRoom = V.length gameMap
      randRooms = V.fromList $ take 3 $ shuffle' [1 .. maxRoom] maxRoom g
   in Game
        { _player = Player {_playerRoom = randRooms V.! 0, arrowCount = 10}
        , _pit1 = randRooms V.! 1
        , _pit2 = randRooms V.! 2
        }

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

getCurrentAdjRooms :: Game -> AdjacentRooms
getCurrentAdjRooms = getAdjRoomsTo . getPlayerRoom

getPlayerRoom :: Game -> Room
getPlayerRoom = _playerRoom . _player

movePlayer :: Room -> Game -> Game
movePlayer = set (player . playerRoom)

isAdjacent :: Room -> Room -> Bool
isAdjacent a b = isInBounds a && isInBounds b && isAdj a b
  where
    minRoom = 1
    maxRoom = V.length gameMap
    isInBounds x = x >= minRoom && x <= maxRoom
    isAdj x y =
      let adjRooms = getAdjRoomsTo x
       in firstRoom adjRooms == y
          || secondRoom adjRooms == y
          || thirdRoom adjRooms == y

getAdjRoomsTo :: Room -> AdjacentRooms
getAdjRoomsTo r = gameMap V.! (r - 1)
