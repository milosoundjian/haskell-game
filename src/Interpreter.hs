module Interpreter where

import Prelude hiding (lex)
import Data.Char
import Graphics.Gloss

import DataTypes

-- This is the file that will interpret user commands
data Token = OP String | Pos Position | Name String 
             deriving Show
                
type Command = [Token]

ignoredChars :: [Char]
ignoredChars = [' ',  '\t',  '\r', '\n', '\v', '\f']
 
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
lex :: String -> Command
lex input = 
    case input of 
        "" -> []
        (c:rest) | elem c ignoredChars -> lex rest

        --parsing operations 
        ('a':'d':'d':rest) -> OP "add" : lex rest
        ('s':'u':'b':rest) -> OP "sub" : lex rest

        --parsing tuples (terrible code) (TODO : should use multi-pass approach)
        ('(':x:',':y:')':rest) | isDigit x && isDigit y -> 
            Pos (digitToInt x, digitToInt y) : lex rest
        ('(':x1:x2:',':y:')':rest) | and (isDigit <$> [x1, x2, y]) -> 
            Pos (digitToInt x1 * 10 + digitToInt x2, digitToInt y) : lex rest
        ('(':x:',':y1:y2:')':rest) | and (isDigit <$> [x, y1, y2]) -> 
            Pos (digitToInt x, digitToInt y1 * 10 + digitToInt y2) : lex rest
        ('(':x1:x2:',':y1:y2:')':rest) | and (isDigit <$> [x1, x2, y1, y2]) -> 
            Pos (digitToInt x1 * 10 + digitToInt x2, digitToInt y1 * 10 + digitToInt y2) : lex rest
        
        --all lonely characters will be merged eventually
        (c:rest) -> Name (c:[]) : lex rest

mergeNames :: Command -> Command 
mergeNames [] = []
mergeNames (Name a:Name b:rest) = mergeNames (Name (a ++ b):rest)
mergeNames (x:xs) = x:(mergeNames xs)





interpret :: String -> GameState -> GameState
interpret input gameState =
    let
        command = mergeNames . lex . preprocess $ input
    in
        gameState {debugText = show command }


