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

        waters = [],
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
        waters = 
            [(x, 1) | x <- [1..6]] ++
            [(1, 2), (3, 2), (4, 2), (6, 2),
            (1, 4), (6, 4)]
    }

room0A :: RoomState
room0A = 
    roomBase 
    {
        character = (0, 5),
        charRot = 0,

        waters = [
            (0, 1), (0, 2), (1, 1), (1, 0), 
            (2, 0), (3, 0), (4, 0), (5, 0),
            (5, 1), (6, 1), (6, 2), (7, 3), 
            (6, 4), (5, 4), (4, 4), (4, 3), 
            (2, 4), (2, 5), (1, 5), (3, 3),
            (2, 3)
            ],
        spikes = [],

        isTerminal = False,
        specialPos = (6, 3)
    }


room0B :: RoomState
room0B = 
    roomBase
    {
        character = (0, 1),
        charRot = -90,

        spikes = [(1, 0), (2, 0), (3, 0), (1, 2), (2, 2), (3, 2)],

        isMini = True,
        isTerminal = True,
        specialPos = (3, 1) 
    }

--eidolon game state 
gsBase :: GameState
gsBase = GameState 
    {
        userText = "",
        debugText = "Debug output :",
        isCursorVisible = True,

        toInit = True,

        currLevelInitScreen = headItem levelsData, 
        screenPointer = headItem levelsData,
        titlePointer = headItem titles,

        elapsedFrames = 0,
        gameOver = False,
        rooms = [],
        moveHistory = []
    }


-- ScreenWrap eidola
startScreen, midScreen :: ScreenWrap 
startScreen = ScreenWrap{
    screen = [roomDebug],
    isNewLevel = True
}

midScreen = ScreenWrap {
    screen = [roomDebug],
    isNewLevel = False
}

-- We define the entire game as a succession of screens == room lists
-- Every "level" consists of a set amount of screens
level0 :: Level
level0 = [
    
    startScreen {
        screen = [room0B]
    }

    ]

level1 :: Level
level1 = [

    startScreen{
        screen = [room0B]
    },

    midScreen{
        screen = [room0A]
    }

    ]

-- Combining everything
levelsData:: LevelData
levelsData = level0 ++ level1

titles :: [Title]
titles = ["0 : Getting Started", "1 : Binary Bifurcation Branch"]
