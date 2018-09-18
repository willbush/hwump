module GameSpec
  ( spec
  ) where

import           Game
import qualified System.Random   as R
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

  describe "analyze" $ do
    it "returns game over if the player fell into a pit" $ do
      let game =
            Game
              { _player = Player {_playerRoom = 1, arrowCount = 1}
              , _pit1 = 1
              , _pit2 = 2
              }
      eval game `shouldBe` GameOver FellInPit

    it "never returns game over from a newly made game" $
      property newGameIsNeverGameOver

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
  let evalResult = eval $ makeGame $ R.mkStdGen n
   in evalResult == GameOn
