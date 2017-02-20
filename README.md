INSTRUCTIONS FOR DOWNLOADING RANDOM:
run the following command in Terminal
$ cabal install random


The following is a simplified version where the player always plays first.

Game starts with the following state:
	Player points: 0
	Computer points: 0
	Chosen card: x
	Remaining cards: 51 cards remaining (52 cards - x)

Player chooses an action:
	Available actions:
		Over: >
		Under: <
		Same: =

Game will take action, player points, computer points, chosen card, and remaining cards.

Game calls choose card:
	Choose card will return a tuple of (x, remaining cards)

Game takes result of choose card, player action, and chosen card to determine result.

Result will take current card, new card, action and return bool using the action as an operator.

Game takes the result (bool) and determines the new score.
	For whoever's turn it is, increase their score if bool == true, else nothing
	Check if the incremented score == 5
		If true, return EndOfGame 1
		Else return continue game state	
