module DungeonGenerator where

import Data.Time.Clock
import Data.Maybe
import Control.Monad

-- TODO: MAKE MORE FILES (pure and non-pure, etc. ) to better organize! 

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
roomFromRoomSettings (RoomSettings connect bnds sGen) = Room {roomPos = getRoomSpawnPos connect, roomBounds = bnds, connectors=[], generation=sGen}

addRoom :: [Room] -> RoomSettings -> [Room]
addRoom rooms roomS = if not (roomCollidingPlural (roomFromRoomSettings roomS) rooms) then roomFromRoomSettings roomS : rooms else rooms 

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

-- RANDGEN

getRandom :: Int -> Int -> IO Int
getRandom min max = do
    t <- getCurrentTime
    let time = fromIntegral $ diffTimeToPicoseconds $ utctDayTime t
    return ((((time * 2189104) `div` 2135902) `mod` max) + min)
getRandomListElem :: [a] -> IO a
getRandomListElem xs = getRandom 0 (length xs - 1) >>= (return . (xs!!) )

getRandomTuple :: (Int, Int) -> (Int, Int) -> IO (Int, Int)
getRandomTuple (x1, y1) (x2, y2) = do
    t1 <- getRandom x1 y1
    t2 <- getRandom x2 y2
    return (t1, t2)

genMult :: a -> a -> (a -> a -> IO b) -> Int -> [IO b]
genMult b1 b2 f 1 = [f b1 b2]
genMult b1 b2 f len = f b1 b2 : genMult b1 b2 f (len-1)

genConnectorS :: (Int, Int) -> [(Int, Int)] -> IO ConnectorSettings
genConnectorS lenB offB = do
    dir <- getRandom 0 3 >>= (return . toEnum) :: IO Direction
    len <- uncurry getRandom lenB
    let getCorrect = if dir == North || dir == South then head offB else offB!!1
    off <- uncurry getRandom getCorrect
    roff <- uncurry getRandom getCorrect
    return ConnectorSettings{csDir=dir, csLength=len, csOffset=off, csrOffset=roff}

getRandGeneration :: WorldConfig -> Int -> [[ConnectorNode]] -> IO GenerationData
getRandGeneration config generation nodes = do
    -- Create room settings (invalid created rooms are weeded out later by checking if they collide with anything)
    n <- mapM getRandomListElem nodes
    bounds <- sequence (genMult (wBounds config) (hBounds config) getRandomTuple (length n) )
    let roomS = zipWith (\ b a -> RoomSettings b a generation) n bounds
    -- now just to make the random connectors...
    let fs = (\_ -> genConnectorS (lenBounds config) [hBounds config, wBounds config])
    let ffs = (\x -> mapM fs (replicate x () ) )
    cnts <- sequence $ genMult 0 4 getRandom (length roomS)
    connectorS <- mapM ffs cnts
    return (GenerationData{generationRooms=roomS, generationConnectors=connectorS})

generateRooms :: WorldConfig -> Room -> IO [Room] 
generateRooms config initialRoom = generateRooms' config (totalGenerations config) [initialRoom] 

generateRooms' :: WorldConfig -> Int -> [Room] -> IO [Room]
generateRooms' config currGen rooms = do
        let roots = map connectors $ filter ((currGen-1==) . generation) rooms
        rand <- getRandGeneration config currGen roots
        let newGen = addGeneration rooms rand currGen
        if currGen /= 0 then generateRooms' config (currGen - 1) newGen else return newGen

-- Drawing
drawRoom :: Room -> IO ()
drawRoom room = error "Room drawing not implemented"

drawConnector :: ConnectorNode -> IO ()
drawConnector connect = error "Connector drawing not implemented"
