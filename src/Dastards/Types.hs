module Dastards.Types where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set


type Money = Int
data ScholarType = Scientist | Doctor | Priest | Clerk
    deriving (Show, Eq, Ord, Read)

data Player = Blue | Yellow | Green | Red | Violet
    deriving (Show, Eq, Ord, Read)

type SalArea = Maybe(Scholar)

data Palace = Palace SalArea SalArea SalArea SalArea
    deriving (Show, Eq, Read)

type Island = [Scholar]

type Scholar = (Player, ScholarType)

data PlayerState = PlayerState
    { psMoney :: Money
    , psReserveScholars :: Set Scholar
    , psPalace :: Palace
    , psApplications :: Set Scholar
    }
    deriving (Show, Eq, Read)
data GameState = GameState
    { gsPlayerStates :: Map Player PlayerState
    , gsIsland :: Island
    , gsCurrentPlayer :: Player
    , gsTurnCount :: Int
    }
    deriving (Show, Eq, Read)


data Move
     = AcceptApplication Scholar Int
     | Bribe Player Money
     | Banish Scholar
     | SubmitApplication Scholar Player


initialSalArea :: SalArea
initialSalArea = Nothing

initialPlayerState :: Player -> PlayerState
initialPlayerState pl = PlayerState
    { psMoney = 32
    , psReserveScholars = Set.fromList $ zip (repeat pl) [Scientist, Scientist, Doctor, Doctor, Priest, Priest, Clerk, Clerk]
    , psPalace = Palace initialSalArea initialSalArea initialSalArea initialSalArea
    , psApplications = Set.empty
    }

initialGameState :: [Player] -> GameState
initialGameState pls = GameState
    { gsPlayerStates = Map.fromList $ map ( \pl -> (pl, initialPlayerState pl) ) pls
    , gsIsland = []
    , gsCurrentPlayer = head pls
    , gsTurnCount = 0
    }

makeMove :: GameState -> Move -> GameState
makeMove gs (Banish scholar) = GameState
    { gsPlayerStates = Map.adjust removeScholar (gsCurrentPlayer gs) (gsPlayerStates gs)
    , gsIsland = scholar : gsIsland gs
    , gsCurrentPlayer = gsCurrentPlayer gs
    , gsTurnCount = gsTurnCount gs
    }
    where
        removeScholar :: PlayerState -> PlayerState
        removeScholar pl = PlayerState
            { psMoney = psMoney pl
            , psReserveScholars = psReserveScholars pl
            , psPalace = removeFromPalace ( psPalace pl )
            , psApplications = Set.delete scholar (psApplications pl)
            }
        removeFromPalace :: Palace -> Palace
        removeFromPalace (Palace s1 s2 s3 s4) = Palace (removeFromSalArea s1) (removeFromSalArea s2) (removeFromSalArea s3) (removeFromSalArea s4)
        removeFromSalArea :: SalArea -> SalArea
        removeFromSalArea Nothing = Nothing
        removeFromSalArea (Just scholar1) = if scholar1 == scholar then Nothing else Just scholar1


            -- isEqual <- (==) scholar <$> sA
            -- if isEqual then Nothing else sA
