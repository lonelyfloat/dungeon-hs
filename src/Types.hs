module Types where

data Room = Room {
    roomPos :: (Int, Int),
    roomBounds :: (Int, Int),
    connectors :: [ConnectorNode],
    generation :: Int
} deriving Show

data Direction = North | South | West | East deriving (Eq, Ord, Enum, Show)

data ConnectorNode = ConnectorNode {
    connectRoom :: Room,
    connectDir :: Direction,
    sideOffset :: Int,
    roomOffset :: Int,
    connectLength :: Int
} deriving (Show)

data ConnectorSettings = ConnectorSettings {
    csDir :: Direction,
    csOffset :: Int,
    csrOffset :: Int,
    csLength :: Int
} deriving (Show)

data RoomSettings = RoomSettings {
    connector :: ConnectorNode, 
    bounds :: (Int, Int),
    sGen :: Int
}

data WorldConfig = WorldConfig {
    wBounds :: (Int, Int),
    hBounds :: (Int, Int),
    lenBounds :: (Int, Int),
    totalGenerations :: Int
}

data GenerationData = GenerationData {
    generationConnectors :: [[ConnectorSettings]],
    generationRooms :: [RoomSettings]
}


