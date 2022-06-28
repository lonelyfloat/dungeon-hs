module RandomGen where

import Generator
import Types
import Data.Time.Clock

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
    -- and pack everything into a GenerationData struct
    return (GenerationData{generationRooms=roomS, generationConnectors=connectorS})

generateRooms :: WorldConfig -> Room -> IO [Room] 
generateRooms config initialRoom = generateRooms' config (totalGenerations config) [initialRoom] 

generateRooms' :: WorldConfig -> Int -> [Room] -> IO [Room]
generateRooms' config currGen rooms = do
        let roots = map connectors $ filter ((currGen-1==) . generation) rooms
        rand <- getRandGeneration config currGen roots
        let newGen = addGeneration rooms rand currGen
        if currGen /= 0 then generateRooms' config (currGen - 1) newGen else return newGen


