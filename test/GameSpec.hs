module GameSpec
  ( spec
  ) where

import qualified Control.Monad.Random as R
import           Game
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "is adjacent" $ do
    it "returns false if any given room is out of bounds of the map" $ do
      isAdjacent 20 21 `shouldBe` False
      isAdjacent 21 20 `shouldBe` False
      isAdjacent 0 1 `shouldBe` False
      isAdjacent 1 0 `shouldBe` False

    it "returns true if the two given expectedTraversal are adjacent" $
      isAdjacent 1 2 `shouldBe` True

    it "returns true for room number n and n + 1 and n in bounds" $
      property moveToNextRoomNum

    it "returns true for room number n and n - 1 and n in bounds" $
      property moveToPrevRoomNum

  describe "evaluate function" $ do
    it "never returns game over from a newly made game" $
      property newGameIsNeverGameOver

    it "returns game over if the player fell into a pit" $ do
      let pitRoom = pit1 gameTemplate
          game = movePlayer pitRoom gameTemplate
      eval game `shouldBe` GameOver FellInPit

    it "returns game over if the player runs into the awake wumpus" $ do
      let wumpusRoom = getWumpusRoom gameTemplate
          game = movePlayer wumpusRoom $ awakenWumpus gameTemplate
      eval game `shouldBe` GameOver DeathByWumpus

    it "returns bat snatch if the player went into a room with a bat." $ do
      let batRoom = bat1 gameTemplate
          game = movePlayer batRoom gameTemplate
      eval game `shouldBe` SuperBatSnatch

    it "returns game over if the player runs out of arrows." $ do
      let zeroArrowGame = decrementArrowCount gameTemplate
      eval zeroArrowGame `shouldBe` GameOver OutOfArrows

  describe "shoot function" $ do
    it "can miss when shooting nothing." $ do
      trip <- R.evalRandIO $ shoot [] gameTemplate
      trip `shouldBe` ArrowTrip Miss []

    it "can miss." $ do
      let playerRoom = 8
          game = movePlayer playerRoom gameTemplate
          roomsToShoot = [9, 10, 11, 12, 13]
      trip <- R.evalRandIO $ shoot roomsToShoot game
      trip `shouldBe` ArrowTrip Miss roomsToShoot

    it "can hit player." $ do
      let roomsToShoot = [5, 6, 7, 8, 1]
      trip <- R.evalRandIO $ shoot roomsToShoot gameTemplate
      trip `shouldBe` ArrowTrip HitPlayer roomsToShoot

    it "can hit the Wumpus." $ do
      let roomsToShoot = [2]
      trip <- R.evalRandIO $ shoot roomsToShoot gameTemplate
      trip `shouldBe` ArrowTrip HitWumpus roomsToShoot

    it "never shoots a too crooked with random traversal." $
      property randTraveralIsNeverTooCrooked

    it "never creates a disjoint path with random traversal." $
      property randTraveralIsNeverDisjoint

-- | A simple game template to use in tests.
gameTemplate :: Game
gameTemplate =
  Game
    { _player = Player {_playerRoom = 1, _arrowCount = 1}
    , _wumpus = Wumpus {_wumpusRoom = 2, _isSleeping = True}
    , pit1 = 3
    , pit2 = 4
    , bat1 = 5
    , bat2 = 6
    }

-- | The current room and the current room plus one should always be
-- adjacent unless either are out of bounds.
moveToNextRoomNum :: Room -> Bool
moveToNextRoomNum x =
  let isAdj = isAdjacent x (x + 1)
   in if x > 0 && x < 20
        then isAdj
        else not isAdj

-- | The current room and the current room minus one should always be
-- adjacent unless either are out of bounds.
moveToPrevRoomNum :: Room -> Bool
moveToPrevRoomNum x =
  let isAdj = isAdjacent x (x - 1)
   in if x >= 2 && x <= 20
        then isAdj
        else not isAdj

-- | A newly created game should never evaluate to a game over state.
newGameIsNeverGameOver :: Int -> Bool
newGameIsNeverGameOver n =
  let evalResult = eval $ R.evalRand mkGame (R.mkStdGen n)
   in evalResult == GameOn

randTraveralIsNeverTooCrooked :: Int -> Bool
randTraveralIsNeverTooCrooked seed = not $ anyTooCrooked $ getRandTraversal seed

randTraveralIsNeverDisjoint :: Int -> Bool
randTraveralIsNeverDisjoint seed = not $ anyDisjoint $ getRandTraversal seed

getRandTraversal :: Int -> [Room]
getRandTraversal seed =
  let (playerRoom, g) = R.randomR (1 :: Room, 20) $ R.mkStdGen seed
      game = movePlayer playerRoom gameTemplate
      ArrowTrip _ traversal = R.evalRand (shoot invalidTraversal game) g
   in traversal

  -- | the first room 20 is not adjacent to the player room 1.
invalidTraversal :: [Room]
invalidTraversal = [20, 13, 12, 11, 10]

anyDisjoint :: [Room] -> Bool
anyDisjoint rs@(a:b:_) = not (isAdjacent a b) || anyDisjoint (drop 1 rs)
anyDisjoint _          = False
