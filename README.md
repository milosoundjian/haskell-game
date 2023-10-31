# haskell-game

Enjoy the trials and tribulations of life as a squirrel in Haskell

An game written in Haskell using the [Gloss](https://hackage.haskell.org/package/gloss) library.

## Installation

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
* Shift: Undo
* Ctrl: Restart level
* Esc: Exit
* Letters: Type into the "console" (bottom)

## Console Commands

* `/dance`: make the squirrel dance (spin around dance)
* `/skin`: change the squirrel skin
* `/kms`: kill yourself (lose the game)
* `/win`: win the game (no satisfaction tho)
* `/credits` opens credits
