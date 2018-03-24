import Test.Hspec
import Dastards.Types

main :: IO ()
main = hspec $ do
    describe "makeMove" $ do
        it "banish from applications" $ makeMove beforeBanishFromApps (Banish (Green, Scientist)) `shouldBe` afterBanishFromApps
        it "banish from palace" $ makeMove beforeBanishFromPal (Banish (Green, Scientist)) `shouldBe` afterBanishFromPal

beforeBanishFromApps :: GameState
beforeBanishFromApps = read "GameState {gsPlayerStates = fromList [(Green,PlayerState {psMoney = 32, psReserveScholars = fromList [(Green,Doctor),(Green,Priest),(Green,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Red,PlayerState {psMoney = 32, psReserveScholars = fromList [(Red,Scientist),(Red,Doctor),(Red,Priest),(Red,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Violet,PlayerState {psMoney = 32, psReserveScholars = fromList [(Violet,Scientist),(Violet,Doctor),(Violet,Priest),(Violet,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList [(Green,Scientist)]})], gsIsland = [], gsCurrentPlayer = Violet, gsTurnCount = 0}"
afterBanishFromApps :: GameState
afterBanishFromApps = read "GameState {gsPlayerStates = fromList [(Green,PlayerState {psMoney = 32, psReserveScholars = fromList [(Green,Doctor),(Green,Priest),(Green,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Red,PlayerState {psMoney = 32, psReserveScholars = fromList [(Red,Scientist),(Red,Doctor),(Red,Priest),(Red,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Violet,PlayerState {psMoney = 32, psReserveScholars = fromList [(Violet,Scientist),(Violet,Doctor),(Violet,Priest),(Violet,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []})], gsIsland = [(Green, Scientist)], gsCurrentPlayer = Violet, gsTurnCount = 0}"

beforeBanishFromPal :: GameState
beforeBanishFromPal = read "GameState {gsPlayerStates = fromList [(Green,PlayerState {psMoney = 32, psReserveScholars = fromList [(Green,Doctor),(Green,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Red,PlayerState {psMoney = 32, psReserveScholars = fromList [(Red,Scientist),(Red,Doctor),(Red,Priest),(Red,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Violet,PlayerState {psMoney = 32, psReserveScholars = fromList [(Violet,Scientist),(Violet,Doctor),(Violet,Priest),(Violet,Clerk)], psPalace = Palace (Just(Green, Scientist)) Nothing Nothing (Just(Green, Priest)), psApplications = fromList []})], gsIsland = [], gsCurrentPlayer = Violet, gsTurnCount = 0}"
afterBanishFromPal :: GameState
afterBanishFromPal = read "GameState {gsPlayerStates = fromList [(Green,PlayerState {psMoney = 32, psReserveScholars = fromList [(Green,Doctor),(Green,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Red,PlayerState {psMoney = 32, psReserveScholars = fromList [(Red,Scientist),(Red,Doctor),(Red,Priest),(Red,Clerk)], psPalace = Palace Nothing Nothing Nothing Nothing, psApplications = fromList []}),(Violet,PlayerState {psMoney = 32, psReserveScholars = fromList [(Violet,Scientist),(Violet,Doctor),(Violet,Priest),(Violet,Clerk)], psPalace = Palace Nothing Nothing Nothing (Just(Green, Priest)), psApplications = fromList []})], gsIsland = [(Green, Scientist)], gsCurrentPlayer = Violet, gsTurnCount = 0}"
