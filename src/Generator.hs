module Generator where

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

-- Utilities

roomColliding :: Room -> Room -> Bool
roomColliding roomA roomB =
   (fst (roomPos roomA) < fst (roomPos roomB) + fst (roomBounds roomB)) &&
   (fst (roomPos roomA) + fst (roomBounds roomA) > fst (roomPos roomB)) &&
   (snd (roomPos roomA) < snd (roomPos roomB) + snd (roomBounds roomB)) &&
   (snd (roomPos roomA) + snd (roomBounds roomA) > snd (roomPos roomB))

roomsColliding :: Room -> [Room] -> Bool
roomsColliding room = foldr ((&&) . roomColliding room) True

roomFromRoomSettings :: RoomSettings -> Room
roomFromRoomSettings (RoomSettings connect bnds sGen) = Room {roomPos = getRoomSpawnPos connect, roomBounds = bnds, connectors=[], generation=sGen}


-- Actual logic

addConnector :: ConnectorSettings -> Room -> Room
addConnector (ConnectorSettings dir offset roffset len) room = room {connectors = ConnectorNode room dir offset roffset len : connectors room}

addConnectors :: [ConnectorSettings] -> Room -> Room
addConnectors settings room = foldr addConnector room settings

addRoom :: [Room] -> RoomSettings -> [Room]
addRoom rooms roomS = if not (roomsColliding (roomFromRoomSettings roomS) rooms) then roomFromRoomSettings roomS : rooms else rooms 

getRoomSpawnPos :: ConnectorNode -> (Int, Int)
getRoomSpawnPos (ConnectorNode room dir offset roffset length)
    | dir == North = (fst (roomPos room) + offset - roffset, snd (roomPos room) - length)
    | dir == South = (fst (roomPos room) + offset - roffset, snd (roomPos room) + snd (roomBounds room) + length)
    | dir == West  = (fst (roomPos room) - length, snd (roomPos room) + offset - roffset)
    | dir == East  = (fst (roomPos room) + fst (roomBounds room) + length, snd (roomPos room) + offset - roffset)
    | otherwise = error "Unreachable"

-- Takes in all rooms, returns all rooms with new generation applied.
addGeneration :: [Room] -> GenerationData -> Int -> [Room]
addGeneration rooms (GenerationData settings roomSettings) currentGeneration = zipWith addConnectors settings (foldl addRoom (filter ((currentGeneration-1==) . generation) rooms) roomSettings) ++
    filter ((currentGeneration-1/=) . generation) rooms 





