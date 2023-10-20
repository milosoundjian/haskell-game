module Interpreter where

import Prelude hiding (lex)
import Data.Char
import Graphics.Gloss

import DataTypes
import Levels

-- This is the file that will interpret user commands

-- All of the constant definitions start here
data Operation = ADD | SUB | MOVE  deriving (Show, Eq)
data Prep = BY | AT | TO deriving (Show, Eq)

-- after all passes there shouldn't be any tokens of type Digit or Ch
data Token = OP Operation | Prep Prep | Special Char | Ch Char | Digit Int | Name String | Number Int |
             Pos Position
                deriving (Show, Eq) 

                
type Command = [Token]

ignoredChars :: [Char]
ignoredChars = [' ', '.', '\t',  '\r', '\n', '\v', '\f']

specialChars :: [Char]
specialChars = [',', '(', ')']
-- All of the constant definitions end here

 
-- helper functions for pre-processing
rmFSpaces :: String -> String 
rmFSpaces  = dropWhile (== ' ') 

rmESpaces :: String -> String 
rmESpaces input | (last input == ' ') = init input
                | otherwise = input


-- currently, just converts the input string to lowercase
preprocess :: String -> String
preprocess input = toLower <$> (rmFSpaces . rmESpaces $ input)

-- student makes WORST parser EVER, asked to LEAVE CSE301
namePass :: String -> [Token]
namePass input = 
    case input of 
        "" -> []

        -- recognizing operation names ONLY if they're isolated 
        ('a':'d':'d':rest) -> OP ADD : namePass rest
        ('s':'u':'b':rest) -> OP SUB : namePass rest
        ('m':'o':'v':'e':rest) -> OP MOVE : namePass rest

        -- recognizing preposition names
        ('a':'t':rest) -> Prep AT : namePass rest
        ('b':'y':rest) -> Prep BY : namePass rest
        ('t':'o':rest) -> Prep TO : namePass rest

        -- skipping empty characters for anything else
        (c:rest) | elem c ignoredChars -> namePass rest


        --sorting anything else into chars and digits
        (d:rest) | isDigit d -> Digit (digitToInt d) : namePass rest
        (c:rest) -> if (elem c specialChars) then 
                        (Special c : namePass rest) 
                    else 
                        (Ch c: namePass rest)



-- This pass merges all of the lonely chars and lonely digits together 
mergePass :: [Token] -> [Token]
mergePass [] = []

mergePass (Ch a:Ch b:rest) = mergePass $ Name [a,b]: rest
mergePass (Name a:Ch b:rest) = mergePass $ Name (a ++ [b]) : rest

mergePass (Digit a:Digit b:rest) = mergePass $ Number (a*10 + b) : rest
mergePass (Number a:Digit b:rest) = mergePass $ Number (a * 10 + b) : rest

mergePass (Ch a :rest) = Name [a] :mergePass rest
mergePass (Digit a : rest) = Number a : mergePass rest
mergePass (other : rest) = other:mergePass rest



-- Final pass : greedy combination of as many number clusters into positions
-- Also eliminate any special characters that don't fit into anything
posPass :: [Token] -> Command 
posPass [] = []

posPass (Special '(':Number x:Special ',':Number y: Special ')':rest) = Pos (x,y):posPass rest
posPass (Special c : rest) = posPass rest
posPass (x:rest) = x:posPass rest


interpret :: String -> GameState -> GameState
interpret "" gs = gs
interpret input gs =
    let
        command = posPass . mergePass. namePass. preprocess $ input

    in
        case command of
            [OP MOVE, Prep TO, Pos newPos] -> 
                    gs {rooms = map (`movedToRoomState` newPos) (rooms gs)  }   

            [OP ADD, Name "spike", Prep AT, Pos newPos] ->
                    gs {rooms = map (`addSpike` newPos) (rooms gs)}
            
            [OP ADD, Name "water", Prep AT, Pos newPos] ->
                    gs {rooms = map (`addWater` newPos) (rooms gs)}

            _ -> gs {debugText = "Command not recognized: " ++ (show command)}


