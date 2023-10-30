module LevelData where

import DataTypes
import Constants

import Zippers

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


-- ScreenWrap eidolon
screenEidolon :: ScreenWrap
screenEidolon = ScreenWrap{
    screen = [],
    isNewLevel = False
}

-- HERE is where we define the actual levels using the rooms above.
-- Just define a succession of screens as follows and mark each new level by a True in the wrapper

screenZero :: ScreenWrap
screenZero = screenEidolon{
    screen = [room0B],
    isNewLevel = True
}


screenOne :: ScreenWrap
screenOne = screenEidolon{
    screen = [room0A],
    isNewLevel = True
}

-- Combining everything
levelsData:: LevelData
levelsData = [screenZero, screenOne]

titles :: [Title]
titles = ["Level 0", "Level 1 : Binary Bifurcation Branch"]
