module Graphics where

import Types

type Canvas = [[Char]]


roomChar :: Char
roomChar = '#' 

connectorChar :: Char
connectorChar = 'O'

getRoomChar :: (Int, Int) -> Room -> Canvas -> Char
getRoomChar (x,y) room canvas
    | (x == fst (roomPos room) || x == fst (roomPos room) + snd (roomBounds room) ) && (y - snd (roomPos room) < snd (roomBounds room) && y < snd (roomPos room)) = roomChar
    | (y == snd (roomPos room) || y == snd (roomPos room) + fst (roomBounds room) ) && (x - fst (roomPos room) < fst (roomBounds room) && x < fst (roomPos room)) = roomChar
    | otherwise = (canvas !! x) !! y

drawRoom :: Room -> Canvas -> Canvas
drawRoom room currCanvas = do
    let newCan = [[getRoomChar (x,y) room currCanvas | x <- [0..length (head currCanvas) - 1]] | y <- [0..length currCanvas - 1]]
    newCan
    --foldr drawConnector newCan (connectors room)

drawConnector :: ConnectorNode -> Canvas -> Canvas
drawConnector connect canvas = error "not implemented"
