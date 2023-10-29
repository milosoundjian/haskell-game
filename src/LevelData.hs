module LevelData where

import DataTypes
import Constants

-- defining helper types 
type Screen = [RoomState]
type Level = (GameState, [Screen])
type LevelData = [Level]

-- eidolon room (use as base for all room creations)
roomBase :: RoomState 
roomBase = 
    RoomState 
    {
        character = (0, 0),
        charRot = 180,

        waters = [],
        spikes = [],

        isMini = False,
        isTerminal = False,
        specialPos = (cols -1, rows - 1),
        rGameOver = False
    } 

-- DEFINIING ROOMS HERE
-- ideally each room should have a name of the form
-- roomID where ID = (roomLevelNumber)([A..Z]*)
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
        userText = "Level X",
        debugText = "Debug output :",
        isCursorVisible = True,

        elapsedFrames = 0,
        levelIndex = 0, 
        screenIndex = 0,
        gameOver = False,
        rooms = [],
        moveHistory = []
    }


-- HERE is where we define the actual levels using the rooms above
levelZero :: Level
levelZero = (
    --starting gs of the level
    gsBase
    { 
      userText = "Level 0 : Getting Started !",
      levelIndex = 0
    },

    --succession of "screens" == "lists of roomstates"
    [
        [room0B]
    ]

    )

levelOne :: Level
levelOne = (

    gsBase 
    {
        userText = "Level 1 : Binary Bifurcation Branch",
        levelIndex = 1
    },

    [
        [room0A]
    ]

    )

-- Combining everythings
levelsData:: LevelData
levelsData = [levelZero, levelOne]