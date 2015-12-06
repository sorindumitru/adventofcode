{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 as AT
import qualified Data.Set as Set
import qualified Data.Map as Map

data Command =
	TurnOn [(Int,Int)] |
	TurnOff [(Int,Int)] |
	Toggle [(Int,Int)]
	deriving (Show)

commandParser = choice [ turnOnCommand, turnOffCommand, toggleCommand]

turnOnCommand = do
	AT.string "turn on "
	startX <- AT.many1 AT.digit
	AT.char ','
	startY <- AT.many1 AT.digit
	AT.string " through "
	stopX <- AT.many1 AT.digit
	AT.char ','
	stopY <- AT.many1 AT.digit
	return $ (TurnOn [ (x,y) | x <- [(read startX::Int)..(read stopX::Int)], y <- [(read startY::Int)..(read stopY::Int)]])

turnOffCommand = do
	AT.string "turn off "
	startX <- AT.many1 AT.digit
	AT.char ','
	startY <- AT.many1 AT.digit
	AT.string " through "
	stopX <- AT.many1 AT.digit
	AT.char ','
	stopY <- AT.many1 AT.digit
	return $ (TurnOff [ (x,y) | x <- [(read startX::Int)..(read stopX::Int)], y <- [(read startY::Int)..(read stopY::Int)]])

toggleCommand = do
	AT.string "toggle "
	startX <- AT.many1 AT.digit
	AT.char ','
	startY <- AT.many1 AT.digit
	AT.string " through "
	stopX <- AT.many1 AT.digit
	AT.char ','
	stopY <- AT.many1 AT.digit
	return $ (Toggle [ (x,y) | x <- [(read startX::Int)..(read stopX::Int)], y <- [(read startY::Int)..(read stopY::Int)]])

commandListParser = AT.many1 $ commandParser <* AT.endOfLine

toggle set (light:lights)
	| Set.member light set = toggle (Set.delete light set) lights
	| otherwise = toggle (Set.insert light set) lights
toggle set [] = set

processCommands set ((TurnOn lights):commands) = processCommands (Set.union set (Set.fromList lights)) commands
processCommands set ((TurnOff lights):commands) = processCommands (Set.difference set (Set.fromList lights)) commands
processCommands set ((Toggle lights):commands) = processCommands (toggle set lights) commands
processCommands set [] = set

increaseBrightness quantity map (light:lights)
	| Map.member light map = increaseBrightness quantity (Map.insertWith (+) light quantity map) lights
	| otherwise = increaseBrightness quantity (Map.insert light quantity map) lights
increaseBrightness quantity map [] = map

decreaseMin a b = max 0 (b - a)

decreaseBrighness quantity map (light:lights)
	| Map.member light map = decreaseBrighness quantity (Map.insertWith decreaseMin light quantity map) lights
	| otherwise = decreaseBrighness quantity map lights
decreaseBrighness quantity map [] = map

processBrightnessCommands map ((TurnOn lights):commands) = processBrightnessCommands (increaseBrightness 1 map lights) commands
processBrightnessCommands map ((TurnOff lights):commands) = processBrightnessCommands (decreaseBrighness 1 map lights) commands
processBrightnessCommands map ((Toggle lights):commands) = processBrightnessCommands (increaseBrightness 2 map lights) commands
processBrightnessCommands map [] = map

main = do
	input <- getContents
	let pinput = BS.pack input
	let comms = AT.parseOnly commandListParser pinput
	let commands = case comms of
			(Left error) -> [(TurnOn [(0,0)])]
			(Right xs) -> xs
	let lights = processCommands Set.empty commands
	putStrLn $ show $ Set.size lights
	let (brightness, _) = Map.mapAccum (\x y -> (x + y, y)) 0 $ processBrightnessCommands Map.empty commands
	putStrLn $ show $ brightness
	return ()


