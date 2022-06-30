module Main where

import Generator
import Graphics
import Types 
import RandomGen
import Data.List

config :: WorldConfig
config = WorldConfig (3,5) (3,5) (2,6) 5

initialRoom :: Room
initialRoom = Room (5,5) (4,4) [] 0

main :: IO ()
main = do
    rooms <- generateRooms config initialRoom
    let canvas = foldr drawRoom (replicate 10 (replicate 10 ' ') ) rooms
    putStrLn (intercalate "\n" canvas)
