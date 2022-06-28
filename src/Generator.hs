module Generator where

import Types
import Utils

-- Takes in a connector settings type and a room and attaches that connector to that room.
addConnector :: ConnectorSettings -> Room -> Room
addConnector (ConnectorSettings dir offset roffset len) room = room {connectors = ConnectorNode room dir offset roffset len : connectors room}

-- Takes in list of connector settings and a room and adds those connectors to that room.
addConnectors :: [ConnectorSettings] -> Room -> Room
addConnectors settings room = foldr addConnector room settings

-- Takes in a generation of rooms and a new room setting and appends that room onto the list.
addRoom :: [Room] -> RoomSettings -> [Room]
addRoom rooms roomS = if not (roomsColliding (roomFromRoomSettings roomS) rooms) then roomFromRoomSettings roomS : rooms else rooms 

-- Takes in all rooms, returns all rooms with new generation applied.
addGeneration :: [Room] -> GenerationData -> Int -> [Room]
addGeneration rooms (GenerationData settings roomSettings) currentGeneration = zipWith addConnectors settings (foldl addRoom (filter ((currentGeneration-1==) . generation) rooms) roomSettings) ++
    filter ((currentGeneration-1/=) . generation) rooms 





