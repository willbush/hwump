{-# LANGUAGE TemplateHaskell #-}

module Game
  ( AdjRooms(..)
  , ArrowTrip(..)
  , DeathType(..)
  , EvalResult(..)
  , Game(..)
  , MaybeHit(..)
  , Player(..)
  , Room
  , Wumpus(..)
  , anyTooCrooked
  , awakenWumpIfFirstArrow
  , awakenWumpus
  , decrementArrowCount
  , eval
  , getCurrentAdjRooms
  , getPlayerRoom
  , getWumpusRoom
  , isAdjacent
  , maxRoom
  , minRoom
  , mkGame
  , movePlayer
  , moveWumpus
  , shoot
  , updateWumpus
  ) where

import           Control.Lens          (makeLenses, set)
import qualified Control.Monad.Random  as R
import           Data.List             (delete)
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

data ArrowTrip = ArrowTrip MaybeHit [Room]
  deriving (Show, Eq)

data MaybeHit
  = HitWumpus
  | HitPlayer
  | Miss
  deriving (Show, Eq)

makeLenses ''Game

makeLenses ''Player

makeLenses ''Wumpus

initialArrowCount :: Int
initialArrowCount = 5

-- | Make a randomly initialized game with non-overlapping game entities.
mkGame :: (R.RandomGen g) => R.Rand g Game
mkGame = do
  shuffledRooms <- shuffleM [minRoom .. maxRoom]
  let randRooms = V.fromList $ take 6 shuffledRooms
  return
    Game
      { _player = Player {_playerRoom = randRooms V.! 0, _arrowCount = initialArrowCount}
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
    wr = getWumpusRoom game
    wumpIsSleeping = wumpusIsSleeping game

updateWumpus :: (R.RandomGen g) => Game -> R.Rand g Game
updateWumpus game = do
  n <- R.getRandomR (1 :: Int, 4)
  let wumpusFeelsLikeMoving = n > 1
  if not (wumpusIsSleeping game) && wumpusFeelsLikeMoving
    then do
      shuffledRooms <- getShuffledAdjRoomsTo $ getWumpusRoom game
      return $ moveWumpus (head shuffledRooms) game
    else return game

-- | This function shoots an arrow from the first room in the given list through
-- the rest of the valid rooms from left to right. When an invalid room
-- traversal is encountered the rest of the rooms are replaced by a valid random
-- traversal. See mkValidTraversal for more info.
shoot :: (R.RandomGen g) => [Room] -> Game -> R.Rand g ArrowTrip
shoot rooms game = do
  validTraversal <- mkValidTraversal (getPlayerRoom game) rooms
  return $ go validTraversal game []
  where
    wr = getWumpusRoom game
    pr = getPlayerRoom game
    go [] _ acc = ArrowTrip Miss $ reverse acc
    go (r:rs) g acc
      | r == wr = ArrowTrip HitWumpus $ reverse $ r : acc
      | r == pr = ArrowTrip HitPlayer $ reverse $ r : acc
      | otherwise = go rs g $ r : acc

-- | This function makes a valid arrow traversal.
--
-- A valid arrow traversal has the following properties:
--
-- * Each room in the traversal are rooms the arrow travels through.
-- * A traversal is considered to flow from left to right in the list.
-- * Each room adjacent in the list is adjacent in the map.
-- * A traversal does not violate A-B-A "arrow too crooked" restriction. (i.e.
--   The arrow never goes to an adjacent room and back). This function assumes
--   the given list of rooms does not contain a A-B-A path, which should be
--   prevented when the user is inputing the rooms. However, when adding
--   randomly traversed rooms this function will not violate A-B-A.
--
-- As soon as a non-adjacent room is encountered, when scanning from left to
-- right, the remaining rooms are disregarded and a random valid traversal
-- constructed.
--
-- Note, the first room to shoot is a special case because if its not adjacent
-- to the player, then we need to get any random adjacent room. This is the only
-- case where we don't have to worry about creating an A-B-A path.
mkValidTraversal :: (R.RandomGen g) => Room -> [Room] -> R.Rand g [Room]
mkValidTraversal _ [] = return []
mkValidTraversal playerR roomsToShoot@(r:rs) =
  if isAdjacent playerR r
    then go (playerR : roomsToShoot) False [r]
    else do
      shuffledRooms <- getShuffledAdjRoomsTo playerR
      let fstValidRoom = head shuffledRooms
      go (playerR : fstValidRoom : rs) True [fstValidRoom]
  where
    go (previous:current:next:rooms) isDisjoint acc =
      if isDisjoint || not (isAdjacent current next)
        then do
          shuffledRooms <- getShuffledAdjRoomsTo current
          let randNext = head $ delete previous shuffledRooms
          go (current : randNext : rooms) True (randNext : acc)
        else go (current : next : rooms) False (next : acc)
    go _ _ acc = return $ reverse acc

getShuffledAdjRoomsTo :: (R.RandomGen g) => Room -> R.Rand g [Room]
getShuffledAdjRoomsTo room =
  let AdjRooms a b c = getAdjRoomsTo room
   in shuffleM [a, b, c]

getCurrentAdjRooms :: Game -> AdjRooms
getCurrentAdjRooms = getAdjRoomsTo . getPlayerRoom

getPlayerRoom :: Game -> Room
getPlayerRoom = _playerRoom . _player

getWumpusRoom :: Game -> Room
getWumpusRoom = _wumpusRoom . _wumpus

movePlayer :: Room -> Game -> Game
movePlayer = set (player . playerRoom)

decrementArrowCount :: Game -> Game
decrementArrowCount game =
  let current = (_arrowCount . _player) game
   in set (player . arrowCount) (current - 1) game

awakenWumpIfFirstArrow :: Game -> Game
awakenWumpIfFirstArrow game =
  if (_arrowCount . _player) game < initialArrowCount
    then awakenWumpus game
    else game

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

anyTooCrooked :: [Room] -> Bool
anyTooCrooked rs@(a:_:c:_) = a == c || anyTooCrooked (drop 1 rs)
anyTooCrooked _            = False

minRoom :: Room
minRoom = 1;

maxRoom :: Room
maxRoom = 20;

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
