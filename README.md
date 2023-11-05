# haskell-game

Enjoy the trials and tribulations of life as a squirrel in Haskell.

A game written in Haskell using the [Gloss](https://hackage.haskell.org/package/gloss) library.

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
