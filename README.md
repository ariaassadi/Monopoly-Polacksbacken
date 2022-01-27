# Monopoly Polacksbacken

## Necessary installations (Ubuntu)
ghci:		sudo apt-get install haskell-platform
cabal:		sudo install cabal-install
HUnit:		cabal install HUnit
gloss: 		cabal install gloss

## How to play the game
	ghci
	:l Main.hs
	run
#### Move the terminal to the right side of the screen and dont touch the graphics window

## How to play the game (terminal only)
	ghci
	:l Monopoly.hs
	run

## How to play the game (graphics only)
	ghci
	:l Graphics.hs
	run
#### Use "W,A,S,D" keys to move a player around.
#### Use "1,2,3,4" to switch between player A, B, C and D
