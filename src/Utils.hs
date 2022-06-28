module Utils where

import Types

-- Takes in a node and returns a room's spawn position as defined by that node.
getRoomSpawnPos :: ConnectorNode -> (Int, Int)
getRoomSpawnPos (ConnectorNode room dir offset roffset length)
    | dir == North = (fst (roomPos room) + offset - roffset, snd (roomPos room) - length)
    | dir == South = (fst (roomPos room) + offset - roffset, snd (roomPos room) + snd (roomBounds room) + length)
    | dir == West  = (fst (roomPos room) - length, snd (roomPos room) + offset - roffset)
    | dir == East  = (fst (roomPos room) + fst (roomBounds room) + length, snd (roomPos room) + offset - roffset)
    | otherwise = error "Unreachable"

-- Takes in two rooms and returns if they are colliding (AABB collision)
roomColliding :: Room -> Room -> Bool
roomColliding roomA roomB =
   (fst (roomPos roomA) < fst (roomPos roomB) + fst (roomBounds roomB)) &&
   (fst (roomPos roomA) + fst (roomBounds roomA) > fst (roomPos roomB)) &&
   (snd (roomPos roomA) < snd (roomPos roomB) + snd (roomBounds roomB)) &&
   (snd (roomPos roomA) + snd (roomBounds roomA) > snd (roomPos roomB))

-- Takes in a room 'roomA' and a list of rooms and returns if any rooms are colliding with 'roomA'.
roomsColliding :: Room -> [Room] -> Bool
roomsColliding room = foldr ((&&) . roomColliding room) True

-- Converts a RoomSettings type into a Room type.
roomFromRoomSettings :: RoomSettings -> Room
roomFromRoomSettings (RoomSettings connect bnds sGen) = Room {roomPos = getRoomSpawnPos connect, roomBounds = bnds, connectors=[], generation=sGen}


