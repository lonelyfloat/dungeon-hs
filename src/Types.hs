module Types where

-- Base 'Room' type given back
data Room = Room {
    roomPos :: (Int, Int),
    roomBounds :: (Int, Int),
    connectors :: [ConnectorNode],
    generation :: Int
} deriving Show

-- Direction used for ConnectorNodes.
data Direction = North | South | West | East deriving (Eq, Ord, Enum, Show)

-- Node that represents a path that may or may not connect rooms
data ConnectorNode = ConnectorNode {
    connectRoom :: Room,
    connectDir :: Direction,
    sideOffset :: Int,
    roomOffset :: Int,
    connectLength :: Int
} deriving (Show)

-- Connector settings to be passed to the random IO things.
data ConnectorSettings = ConnectorSettings {
    csDir :: Direction,
    csOffset :: Int,
    csrOffset :: Int,
    csLength :: Int
} deriving (Show)

-- Room settings to be passed to the random IO things.
data RoomSettings = RoomSettings {
    connector :: ConnectorNode, 
    bounds :: (Int, Int),
    sGen :: Int
}

-- Configuration type passed by user controlling procedural generation
data WorldConfig = WorldConfig {
    wBounds :: (Int, Int),
    hBounds :: (Int, Int),
    lenBounds :: (Int, Int),
    totalGenerations :: Int
}

-- Total data generated from random generation, later converted into a [Room].
data GenerationData = GenerationData {
    generationConnectors :: [[ConnectorSettings]],
    generationRooms :: [RoomSettings]
}


