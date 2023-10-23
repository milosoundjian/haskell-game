module LevelData where

import DataTypes

-- defining helper types 
type Level = [GameState]
type LevelData = [Level]

-- defining rooms here
debugRoom :: RoomState
debugRoom = 
    RoomState 
    {
        character = (0, 0),
        charRot = 0,

        waters = [(2, 3), (3, 3), (3, 4)],
        spikes = [(0, 5), (7, 4)],

        isTerminal = False,
        specialPos = (5, 5),

        rGameOver = False
    }


levelZero :: Level
levelZero = [

    GameState
    { 
      userText = "Level 0 : Getting Started !",
      debugText = "Debug output :",
      isCursorVisible = True,

      elapsedFrames = 0,
      levelIndex = 0,
      gsIndex = 0, -- index of the game state within the level
      gameOver = False,

      rooms = [debugRoom],
      moveHistory = []
    }

    ]


-- Combining everythings
levelsData:: LevelData
levelsData = [levelZero]