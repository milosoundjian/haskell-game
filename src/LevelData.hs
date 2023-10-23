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
        character = (0, 0),

        waters = [(2, 3), (3, 3), (3, 4)],
        spikes = [(0, 5), (7, 4)],

        isTerminal = False,
        specialPos = (5, 5)
    }

room0B = 
    roomBase
    {
        character = (5, 5),

        waters = [(x, 3) | x <- [0..cols-1]],

        isTerminal = True,
        specialPos = (0, 0) 
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
        [room0A],
        [room0B, room0B, room0B]
    ]

    )


-- Combining everythings
levelsData:: LevelData
levelsData = [levelZero]