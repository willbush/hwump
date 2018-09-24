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

    it "returns true if the two given rooms are adjacent" $
      isAdjacent 1 2 `shouldBe` True

    it "returns true for room number n and n + 1 and n in bounds" $
      property moveToNextRoomNum

    it "returns true for room number n and n - 1 and n in bounds" $
      property moveToPrevRoomNum

  describe "evaluate function" $ do
    it "never returns game over from a newly made game" $
      property newGameIsNeverGameOver

    it "returns game over if the player fell into a pit" $ do
      let game =
            Game
              { _player = Player {_playerRoom = 1, arrowCount = 1}
              , _wumpus = Wumpus {_wumpusRoom = 20, _isSleeping = True}
              , pit1 = 1
              , pit2 = 2
              , bat1 = 3
              , bat2 = 4
              }
      eval game `shouldBe` GameOver FellInPit

    it "returns game over if the player runs into the awake wumpus" $ do
      let game =
            Game
              { _player = Player {_playerRoom = 1, arrowCount = 1}
              , _wumpus = Wumpus {_wumpusRoom = 1, _isSleeping = False}
              , pit1 = 2
              , pit2 = 3
              , bat1 = 4
              , bat2 = 5
              }
      eval game `shouldBe` GameOver DeathByWumpus

    it "returns bat snatch if the player went into a room with a bat." $ do
      let game =
            Game
              { _player = Player {_playerRoom = 1, arrowCount = 1}
              , _wumpus = Wumpus {_wumpusRoom = 20, _isSleeping = True}
              , bat1 = 1
              , bat2 = 2
              , pit1 = 3
              , pit2 = 4
              }
      eval game `shouldBe` SuperBatSnatch

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
  let evalResult = eval $ R.evalRand makeGame (R.mkStdGen n)
   in evalResult == GameOn
