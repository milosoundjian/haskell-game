module Interpreter where

import Data.Char
import Graphics.Gloss

import DataTypes

-- This is the file that will interpret user commands
data Token = OP String | Pos Position | Name String
type Command = [Token]

ignoredChars = [Char]
ignoredChars = [' ',  '\t',  '\r', '\n', '\v', '\f']
 


-- currently, just converts the input string to lowercase
preprocess :: String -> String
preprocess input = toLower <$> (rmFSpaces . rmESpaces $ input)

-- transforms our input string into a list of tokens
parse :: String -> Command
parse input =
    -- 1h30 spent on this : i can't do this anymore  

    -- case input of 
    --     "" -> []
    --     (c:rest) | (elem c ignoredChars) -> parse rest
    --     ('a':'d':'d':rest) -> (OP "add") : parse rest
    --     ('s':'u':'d':rest) -



      



interpret :: String -> GameState -> GameState
interpret input gameState =
    let
        command = preprocess input
    in
        gameState


