module GameSpec
  ( spec
  ) where

import Game
import Test.Hspec
import Test.QuickCheck

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
      property moveToNextRoomNumProperty

    it "returns true for room number n and n - 1 and n in bounds" $
      property moveToPrevRoomNumProperty

  describe "move player" $ do
    it "moves the player to given room if adjacent" $
      let game = Game {_player = Player {_playerRoom = 1, arrowCount = 5}}
          expected = Game {_player = Player {_playerRoom = 2, arrowCount = 5}}
       in movePlayer 2 game `shouldBe` expected

    it "does not move the player to given room if not adjacent" $
      let game = Game {_player = Player {_playerRoom = 1, arrowCount = 5}}
       in movePlayer 20 game `shouldBe` game

-- | The current room and the current room plus one should always be
-- adjacent unless either are out of bounds.
moveToNextRoomNumProperty :: Room -> Bool
moveToNextRoomNumProperty x =
  let isAdj = isAdjacent x (x + 1)
   in if x > 0 && x < 20
        then isAdj
        else not isAdj

-- | The current room and the current room minus one should always be
-- adjacent unless either are out of bounds.
moveToPrevRoomNumProperty :: Room -> Bool
moveToPrevRoomNumProperty x =
  let isAdj = isAdjacent x (x - 1)
   in if x >= 2 && x <= 20
        then isAdj
        else not isAdj
