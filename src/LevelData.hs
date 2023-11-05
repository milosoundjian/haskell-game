module LevelData where

import DataTypes
import Constants

import Zippers

-- eidolon room (use as base for all room creations)
roomBase :: RoomState 
roomBase = 
    RoomState 
    {
        character = (-100, -100),
        charRot = 180,
        isDoubleSpeed = False,

        walls = [],
        spikes = [],

        isMini = False,
        isTerminal = False,
        specialPos = (-200, -200),
        rGameOver = False
    } 

-- DEFINIING ROOMS HERE
-- ideally each room should have a name of the form
-- room[ID] where [ID] = (roomLevelNumber)([A..Z]*)
-- only exception is roomDebug 
roomDebug :: RoomState
roomDebug  = 
    roomBase 
    {
        walls = 
            [(x, 1) | x <- [1..8]] ++
            [(x, 2) | x <- [1..8]] ++
            [(1, 3), (2, 3), --(4, 3), (5, 3),
            (7, 3), (8, 3),

            (1, 6), (2, 6), (1, 5), (2, 5),
            (7, 6), (8, 6), (7, 5), (8, 5)
            ]

    }

room0A :: RoomState
room0A = 
    roomBase 
    {
        character= (1, 1),
        charRot = 180,

        walls = [(0, 7), (0, 6), (0, 5), (0, 4), (1, 7), (1, 6), (1, 5),
                    (2, 7), (2, 6), (2, 5), (3, 7), (3, 6), (3, 0), (4, 7),
                    (4, 6), (4, 1), (4, 0), (5, 7), (5, 6), (5, 2), (5, 1),
                    (5, 0), (6, 7), (6, 3), (6, 2), (6, 1), (6, 0), (7, 3),
                    (7, 2), (7, 1), (7, 0), (8, 4), (8, 3), (8, 2), (8, 1),
                    (8, 0), (9, 5), (9, 4), (9, 3), (9, 2), (9, 1), (9, 0)],

        isTerminal = True,
        specialPos = (8, 6)
    }

room1A :: RoomState
room1A = 
    roomBase 
    {
        character = (9, 1),
        charRot = 180,

        walls = 
             [(0, 7), (0, 6), (0, 5), (2, 7), (2, 6), (2, 5), (2, 4),
                    (2, 3), (2, 2), (3, 2), (3, 1), (4, 4), (4, 1), (5, 5),
                    (5, 3), (6, 4), (6, 3), (6, 2), (6, 1), (6, 0), (7, 7),
                    (7, 3), (7, 2), (7, 1), (7, 0), (8, 7), (8, 6), (8, 0),
                    (9, 6), (9, 5), (9, 0)],
            
        spikes = [(0, 0), (1, 7), (3, 7), (3, 6), (4, 7), (5, 4), (9, 7)],

        isTerminal = True,
        specialPos = (1, 6)
    }




room2A, room2B, room2C :: RoomState 
room2A = 
    roomBase 
    {
        character = (0, 4),
        charRot = -90,

        walls = [(4, 7), (4, 6), (4, 3), (4, 2), (4, 1), (4, 0), (5, 7),
                    (5, 6), (5, 3), (5, 2), (5, 1), (5, 0)],
        spikes = [(0, 7), (0, 0), (1, 6), (1, 1), (2, 7), (2, 2), (3, 6),
                      (3, 3), (6, 6), (6, 3), (7, 7), (7, 2), (8, 6), (8, 1),
                      (9, 7), (9, 0)],

        isTerminal = False,
        specialPos = (9, 4)
    }

room2B = 
    roomBase 
    {
        character = (6, 4),
        charRot = -90,

        walls = [(0, 6), (1, 6), (1, 5), (1, 4), (1, 3), (1, 2), (1, 1),
                    (2, 7), (2, 3), (3, 7), (3, 3), (3, 2), (3, 1), (4, 7),
                    (4, 1), (5, 0), (6, 6),  (6, 0), (7, 0), (8, 5),
                    (9, 4), (9, 3), (9, 0)],
        spikes = [(0, 2), (0, 0), (1, 0), (2, 6), (2, 4), (2, 2), (2, 0),
                      (4, 2), (5, 1), (6, 3), (6, 7), (7, 5), (7, 1), (8, 0), (9, 2),
                      (9, 1), (5, 4), (4, 5)],
        isTerminal = True,
        specialPos = (7, 4)
    }

room2C = 
    roomBase 
    {
        character= (7, 3),
        charRot = 90,

        walls = [(0, 7), (0, 6), (0, 5), (0, 4), (0, 3), (0, 2), (0, 1),
                    (0, 0), (1, 7), (1, 6), (1, 5), (1, 4), (1, 3), (1, 2),
                    (1, 1), (1, 0), (2, 7), (2, 6), (2, 5), (2, 4), (2, 3),
                    (2, 2), (2, 1), (3, 7), (3, 6), (3, 5), (3, 4), (3, 3),
                    (3, 2), (4, 7), (4, 6), (5, 7), (6, 7), (6, 6), (8, 5),
                    (9, 5), (9, 4)],
        spikes =  [(4, 5), (4, 4), (4, 3), (4, 2), (5, 1), (6, 4), (6, 2),
                      (6, 1), (7, 7), (7, 0), (8, 6), (8, 3), (8, 2), (8, 1),
                      (8, 4)],

        isTerminal = True,
        specialPos = (6, 3)
    }

room3A = 
    roomBase 
    {
        character= (1, 1),
        charRot = -90,

        walls = [(0, 2), (0, 1), (0, 0), (1, 2), (1, 0), (2, 5), (2, 2),
                    (2, 0), (3, 5), (3, 2), (3, 0), (4, 5), (4, 2), (4, 0),
                    (5, 7), (5, 5), (5, 3), (5, 2), (5, 0), (6, 6), (6, 5),
                    (6, 4), (6, 2), (6, 0), (7, 5), (7, 2), (7, 0), (8, 2),
                    (8, 0), (9, 2), (9, 1), (9, 0)],


        specialPos = (8, 1)
    }

room3B = 
    roomBase 
    {
        character= (4, 4),
        charRot = -90,

        walls = [(3, 5), (3, 3), (5, 5), (5, 3)],
        spikes =  [(3, 4), (4, 5), (4, 3)],

        isTerminal = True,
        specialPos = (5, 4)
    }

room3C = 
    roomBase 
    {
        character= (4, 4),
        charRot = 0,

        walls = [(3, 5), (3, 2), (5, 2), (6, 5), (6, 3), (6, 2)],
        spikes =  [(3, 4), (3, 3), (4, 5), (4, 2), (5, 5), (5, 3), (6, 4)],

        isTerminal = True,
        specialPos = (4, 3)
    }

room3D = 
    roomBase 
    {
        character= (4, 4),
        charRot = 90,

        walls = [(4, 5), (4, 3), (5, 5), (5, 4), (5, 3)],
        spikes =  [(3, 5), (3, 3)],

        isTerminal = True,
        specialPos = (3, 4)
    }

room3E = 
    roomBase 
    {
        character= (4, 4),
        charRot = 180,

        walls = [(2, 6), (2, 5), (2, 4), (2, 3), (2, 2), (3, 6), (3, 2),
                    (4, 6), (5, 6), (5, 2), (6, 6), (6, 5), (6, 3), (6, 2)],
        spikes =  [(3, 5), (3, 4), (3, 3), (4, 2), (5, 5), (5, 3), (6, 4)],

        isTerminal = True,
        specialPos = (4, 5)
    }

room4A = 
    roomBase 
    {
        character= (1, 3),
        charRot = -90,
        isDoubleSpeed = True ,

        walls = [(0, 7), (0, 6), (0, 5), (0, 4), (0, 3), (0, 2), (0, 1),
                    (0, 0), (1, 7), (1, 6), (1, 5), (1, 4), (1, 0), (2, 7),
                    (2, 6), (2, 5), (2, 1), (2, 0), (3, 7), (3, 6), (3, 5),
                    (3, 1), (3, 0), (4, 7), (4, 5), (4, 1), (5, 7), (5, 0),
                    (6, 7), (6, 0), (7, 7), (7, 5), (8, 7), (8, 6), (8, 5),
                    (8, 1), (8, 0), (9, 7), (9, 6), (9, 5), (9, 4), (9, 1),
                    (9, 0)],

        spikes = [(1, 1), (2, 4), (3, 4), (4, 6), (4, 4), (4, 0), (5, 6),
                      (6, 6), (7, 6), (7, 4), (7, 1), (7, 0), (8, 4), (9, 3),
                      (9, 2)],

        specialPos = (8, 3)
    }

--eidolon game state 
gsBase :: GameState
gsBase = GameState 
    {
        userText = "",
        debugText = "Debug output :",
        isCursorVisible = True,

        paused = False, 

        currLevelInitScreen = (Hole, gameScreenTree), 
        screenPointer = (Hole, gameScreenTree),

        elapsedFrames = 0,
        gameOver = False,
        rooms = [],
        moveHistory = [],

        isDancing = False
    }


-- ScreenWrap eidola
startScreen, midScreen :: ScreenWrap 
startScreen = ScreenWrap{
    screen = [roomDebug],
    isNewLevel = True,
    solved = False,
    leftSolved = False, 
    rightSolved = False,
    visited = False,
    active = False,
    title = ""
}

midScreen = ScreenWrap {
    screen = [roomDebug],
    isNewLevel = False,
    solved = False,
    leftSolved = False, 
    rightSolved = False,
    active = False,
    visited = False,
    title = ""
}

-- We define the entire game as a succession of screens == room lists
-- Every "level" consists of a set amount of screens
level0 :: Level
level0 = [
    
    startScreen {
        screen = [room0A]
    }

    ]

level1 :: Level
level1 = [

    startScreen{
        screen = [room1A]
    }

    ]

level2 :: Level 
level2 = 
    [
        startScreen{
            screen = [room2A]
        },
        midScreen {
            screen = [room2B, room2C]
        }
    ]


level3 :: Level
level3 = 
    [
        startScreen{
            screen = [room3A]
        },
        midScreen {
            screen = [room3B, room3C, room3D, room3E]
        }
    ]

level4 :: Level
level4 = 
    [
        startScreen {
            screen = [room4A]
        }
    ]

-- Combining everything
levelsData:: LevelData
levelsData = level0 ++ level1 ++ level2 ++ level3 ++ level4

titles :: [Title]
titles = ["0 : Initiation", 
          "1 : The Pursuit of Happiness", 
          "2 : Roundabout",
          "3 : 4 Player Co-op",
          "4 : Supersonic Sciurine"
          
          ]

addTitlesTL :: [Title] -> [ScreenWrap] -> [ScreenWrap]
addTitlesTL ts = addTitles (headItem ("dummy" : ts))

addTitles :: ListZip Title -> [ScreenWrap] -> [ScreenWrap]
addTitles _ [] = []
addTitles tp (sw:sws) = sw' : addTitles tp' sws where
    tp' = if isNewLevel sw then movR tp else tp
    sw' = sw{title = value tp'}



bisect :: [a] -> ([a], [a])
bisect lst =
    let
        x = (length lst) `div` 2
    in
        splitAt x lst


treeify :: LevelData -> ScreenTree
treeify [] = Leaf
treeify (sw : sws) =
    let
        (l, r) = bisect sws
    in
        B sw (treeify l) (treeify r)

gameScreenTree :: ScreenTree    -- The final tree to be traversed in the game 
gameScreenTree = treeify $ addTitlesTL (titles ++ titles ++ titles) (levelsData ++ levelsData ++ levelsData) -- reuse levels for testing
