module DungeonGenerator where

import Data.Time.Clock
import Data.Maybe

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
    sGen :: Int,
    willSpawn :: Bool    
}

data WorldConfig = WorldConfig {
    xBounds :: (Int, Int),
    yBounds :: (Int, Int),
    wBounds :: (Int, Int),
    hBounds :: (Int, Int),
    roomSpawnPercent :: Int,
    connectorSpawnPercent :: Int,
    totalGenerations :: Int
}

-- Utilities

roomsColliding :: Room -> Room -> Bool
roomsColliding roomA roomB =
   (fst (roomPos roomA) < fst (roomPos roomB) + fst (roomBounds roomB)) &&
   (fst (roomPos roomA) + fst (roomBounds roomA) > fst (roomPos roomB)) &&
   (snd (roomPos roomA) < snd (roomPos roomB) + snd (roomBounds roomB)) &&
   (snd (roomPos roomA) + snd (roomBounds roomA) > snd (roomPos roomB))

-- Actual logic

addConnector :: ConnectorSettings -> Room -> Room
addConnector (ConnectorSettings dir offset roffset len) room = room {connectors = ConnectorNode room dir offset roffset len : connectors room}

addConnectors :: [ConnectorSettings] -> Room -> Room
addConnectors settings room = foldr addConnector room settings

roomCollidingPlural :: Room -> [Room] -> Bool
roomCollidingPlural room = foldr ((&&) . roomsColliding room) True

roomFromRoomSettings :: RoomSettings -> Room
roomFromRoomSettings (RoomSettings connect bnds sGen True) = Room {roomPos = getRoomSpawnPos connect, roomBounds = bnds, connectors=[], generation=sGen}
roomFromRoomSettings (RoomSettings _ _ _ False) = error "Cannot create false room settings"

addRoom :: [Room] -> RoomSettings -> [Room]
addRoom rooms roomS = if willSpawn roomS && not (roomCollidingPlural (roomFromRoomSettings roomS) rooms) then roomFromRoomSettings roomS : rooms else rooms 

getRoomSpawnPos :: ConnectorNode -> (Int, Int)
getRoomSpawnPos (ConnectorNode room dir offset roffset length)
    | dir == North = (fst (roomPos room) + offset - roffset, snd (roomPos room) - length)
    | dir == South = (fst (roomPos room) + offset - roffset, snd (roomPos room) + snd (roomBounds room) + length)
    | dir == West  = (fst (roomPos room) - length, snd (roomPos room) + offset - roffset)
    | dir == East  = (fst (roomPos room) + fst (roomBounds room) + length, snd (roomPos room) + offset - roffset)
    | otherwise = error "Unreachable"

-- Takes in all rooms, returns all rooms with new generation applied.
addGeneration :: [Room] -> [[ConnectorSettings]] -> [RoomSettings] -> Int -> [Room]
addGeneration rooms settings roomSettings currentGeneration = zipWith addConnectors settings (foldl addRoom (filter ((currentGeneration-1==) . generation) rooms) roomSettings) ++
    filter ((currentGeneration-1/=) . generation) rooms 

-- Randoms
getRandom :: Int -> Int -> IO Int
getRandom min max = do
    t <- getCurrentTime
    let time = fromIntegral $ diffTimeToPicoseconds $ utctDayTime t
    return ((((time * 2189104) `div` 2135902) `mod` max) + min)


-- Drawing
drawRoom :: Room -> IO ()
drawRoom room = error "Room drawing not implemented"

drawConnector :: ConnectorNode -> IO ()
drawConnector connect = error "Connector drawing not implemented"
