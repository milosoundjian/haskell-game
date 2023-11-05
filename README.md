# haskell-game

Enjoy the trials and tribulations of life as a squirrel in Haskell.

A game written in Haskell using the [Gloss](https://hackage.haskell.org/package/gloss) library.

Project hosted at : https://github.com/milosoundjian/haskell-game 

## Installation

Either stack or cabal is required to turn the project into an executable format.
Compilation has been tested on Windows 10 as well as a select few lab machines.  

### Stack

1. Install [Stack](https://docs.haskellstack.org/en/stable/) or through ghcup
2. Clone this repo
3. Run `stack build` in the repo directory
4. Run `stack run` in the repo directory

Note: If you have trouble with the `stack run` command, try `stack exec haskell-game-exe`

Note: Tested with Stack 2.13.1

### Cabal

1. Install [Cabal](https://www.haskell.org/cabal/) or through ghcup
2. Clone this repo
3. Run `cabal build` in the repo directory
4. Run `cabal run` in the repo directory

Note: If you have trouble with the `cabal run` command, try `cabal exec haskell-game-exe`

Note: Tested with Cabal 3.10.1.0

## Controls

* Arrow Keys: Move
* Left Shift: Undo
* Left Ctrl: Restart level
* Tab : Pause the game
* Esc: Close the game
* Every other letter: Type into the game "console" (bottom)

## Console Actions

* `add box at (x , y)` : creates a box at the given grid coordinates (sentence spacing doesn't matter)
* `add spike at (x, y)` : same as the previous command but with a spike instead
* `move to (x, y)` : teleports the squirrel to the given grid coordinates (only works if the specified tile is bereft of obstacles)
* `dance` / `stop dancing`: make the squirrel dance (spin around)
* `die`: kill the squirrel (game over)
There is a small debug display in the top left of the screen that shows the lexxed version of any inputed command. 


## Project Structure 

The game is composed of a cluster of helper files in the `src/` folder, and a main file at `app/Main.hs` which puts
together all of the components into a coherent Gestalt program. 
The compartmentalization is both helpful to preserve readability (no one file becomes too crowded) and to create clear delineations between functionalities that shouldn't interact.

Note : The full documentation of the GLOSS framework is available [here](https://hackage.haskell.org/package/gloss).

## Individual Files Breakdown (warning : big wall of text)

* `app/Main.hs` : As stated above, the file imports almost every other helper file and serves as the middleman between our code and GLOSS (our 2D graphics framework). Mainly, it defines the `render` function which is called every frame to display the game to the user, and the input handlers, which are functions called every time a specific key is inputted on the keyboard.
* `src/Constants.hs` : A static file (not dependent on any other project file) which defines a few basic variables, in order to easily tweak them for experimentation. 
* `src/DataTypes.hs` : Another static file (only dependent on `Constants.hs`) that defines the data abstractions and aliases that we use heavily throughout the project. 2 notable examples are  `RoomState` - which encodes the information that represents a single room of gameplay - and `GameState`, which contains all of the information relevant to the state of the application at any point during its execution.   
* `src/Graphics.hs` : A list of functions and objects which relate in one way or another to the `Picture` type provided by GLOSS. A `Picture` is how images are represented in the framework, it can contain anything from a simple colored rectangle to a complex bitmap. Note : position information is also part of the Picture  type, it can be modified indirectly by using the `translate` GLOSS function. 
*  `src/Interpreter.hs` : The interpreter file is named that way because it is in charge of taking the user "console" input and turning it into tangible modifications of the gamestate. It does so by first performing a rudimentary lexxing pass, followed by pattern matching on the lexer tokens.
* `src/LevelData.hs` : One of the longest files in the project, `LevelData` is where we store the information for every individual level in the game. Most of the data consists of position tuples for objects such as spikes, boxes, or the character itself. To avoid having to enter most of that data manually, we created a very short helper script (in python \:P) that lets us convert pixel art of a level into a list of positions which we can then plug into `LevelData.hs`.
* `src/LevelHelper.hs` : We frequently have to manipulate levels in some way throughout the course of the game : reset the current level, undo the last move, move on to the nexst level, etc... To avoid writing all of that logic in main and clogging everything, `LevelHelper.hs` served as our compendium for all functions that could be described as "operations" on a level or room.   
* `src/PauseScreen.hs` : A short snippet of code that converts our internal game tree into a helpful visual displayed in the pause menu.
* `src/Zippers.hs` : Our homebrew implementation of zippers, and what allows us to represent the flow of the game as a game "tree". The use of zippers here notably helps us avoid having to wrangle with list indices everywhere, instead relying on the more abstracted `isRoot`, `goDown`, `goRight`, and `goLeft`.   
