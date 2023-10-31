module Interpreter where

import Prelude hiding (lex)
import Data.Char
import Graphics.Gloss

import DataTypes
import LevelHelper

-- This is the file that will interpret user commands


-- after all passes there shouldn't be any tokens of type Digit or Special
data Token = W String | Pos (Int, Int) | N Int | Digit Int | Special Char
                deriving (Show, Eq) 

                
type Command = [Token]

specialChars :: [Char]
specialChars = [',', '(', ')']

 
-- helper functions
rmFSpaces :: String -> String 
rmFSpaces  = dropWhile (== ' ') 

rmESpaces :: String -> String 
rmESpaces input | (last input == ' ') = rmESpaces (init input)
                | otherwise = input

rmDSpaces :: String -> String
rmDSpaces [] = []
rmDSpaces [x] = [x]
rmDSpaces (x1:x2:xs) 
    | x1 == ' ' && x2 == ' ' = rmDSpaces (x2:xs)
    | otherwise =  x1 : rmDSpaces (x2:xs) 



-- makes input into standardized format 
preprocess :: String -> [String]
preprocess input = words $ toLower <$> (rmFSpaces . rmESpaces . rmDSpaces $ input)


-- This pass makes strings into word tokens, but leaves the other characters alone
wordPass :: [String] -> [Token]
wordPass [] = []
wordPass ("" :ws) = wordPass ws
wordPass (word@(prefix:rest) : ws) 

    | isLetter prefix = W word : (wordPass ws)
    | isDigit prefix = Digit (digitToInt prefix) : wordPass ( rest:ws )
    | prefix `elem `specialChars = Special prefix : wordPass (rest:ws)
    
    -- ignore anything else
    | otherwise = wordPass ws



-- This pass merges all digits into numbers 
mergePass :: [Token] -> [Token]
mergePass [] = []
mergePass tokens = 
    case tokens of 
        (Digit a : Digit b : rest) -> mergePass (N (a * 10 + b):rest) 
        (N aa : Digit b : rest) -> mergePass (N (aa * 10 + b ):rest)
        (Digit a : rest) -> N a : (mergePass rest)

        (x:rest) -> x:(mergePass rest)



-- Final pass : greedy combination of as many number clusters into positions
-- Also eliminate any special characters that don't fit into anything
posPass :: [Token] -> Command 
posPass [] = []
posPass tokens = 
    case tokens of 
        (Special '(':N x:Special ',':N y: Special ')':rest) -> Pos (x,y):posPass rest
        (Special c:rest) -> posPass rest

        (x:rest) -> x:(posPass rest)


-- student makes WORST lexer EVER, asked to LEAVE CSE301
interpret :: String -> GameState -> GameState
interpret "" gs = gs
interpret input gs =
    let
        command = posPass . mergePass. wordPass. preprocess $ input
    in
        case command of
            [W "move", W "to", Pos newPos] -> 
                    gs {rooms = map (`movedToRoomState` newPos) (rooms gs)  }   

            [W "add", W "spike", W "at", Pos newPos] ->
                    gs {rooms = map (`addSpike` newPos) (rooms gs)}
            
            [W "add", W "box", W "at", Pos newPos] ->
                    gs {rooms = map (`addWall` newPos) (rooms gs)}

            [W "dance"] ->
                    gs {isDancing = not $ isDancing gs }
            
            [W "stop", W "dancing"] ->
                    gs {isDancing = False}

            [W "die"] ->
                    (backup gs) {gameOver = True}

            _ -> gs {debugText = "Command not recognized: " ++ (show $ mergePass. wordPass. preprocess $ input)}


