module Dastards.Types where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type Money = Int
data ScholarType = Scientist | Doctor | Priest | Clerk
    deriving (Show, Eq)

data Player = Blue | Yellow | Green | Red | Violet
    deriving (Show, Eq, Ord)

data SalArea = SalArea (Maybe(Scholar))
    deriving (Show, Eq)

data Palace = Palace SalArea SalArea SalArea SalArea
    deriving (Show, Eq)

type Island = [Scholar]

type Scholar = (Player, ScholarType)

data PlayerState = PlayerState
    { psMoney :: Money
    , psReserveScholars :: [Scholar]
    , psPalace :: Palace
    , psApplications :: [Scholar]
    }
    deriving (Show, Eq)
data GameState = GameState
    { gsPlayerStates :: Map Player PlayerState
    , gsIsland :: Island
    , gsCurrentPlayer :: Player
    , gsTurnCount :: Int
    }
    deriving (Show, Eq)


data Move
     = AcceptApplication Scholar Int
     | Bribe Player Money
     | Banish Scholar
     | SubmitApplication Scholar Player


initialSalArea :: SalArea
initialSalArea = SalArea Nothing

initialPlayerState :: Player -> PlayerState
initialPlayerState pl = PlayerState
    { psMoney = 32
    , psReserveScholars = zip (repeat pl) [Scientist, Scientist, Doctor, Doctor, Priest, Priest, Clerk, Clerk]
    , psPalace = Palace initialSalArea initialSalArea initialSalArea initialSalArea
    , psApplications = []
    }

initialGameState :: [Player] -> GameState
initialGameState pls = GameState
    { gsPlayerStates = Map.fromList $ map ( \pl -> (pl, initialPlayerState pl) ) pls
    , gsIsland = []
    , gsCurrentPlayer = head pls
    , gsTurnCount = 0
    }

-- makeMove :: GameState -> Move -> GameState
-- makeMove gs (Banish s) = GameState
--     { gsPlayerStates = Map.fromList $ map ( \pl -> (pl, initialPlayerState pl) ) pls
--     , gsIsland = s : gsIsland gs
--     , gsCurrentPlayer = gsCurrentPlayer gs
--     , gsTurnCount = gsTurnCount gs
--     }
