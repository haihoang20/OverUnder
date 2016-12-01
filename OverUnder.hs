-- CPSC 312 - 2016 - Games in Haskell
module OverUnder where

import System.Random

-- To run it, try:
-- ghci
-- :load OverUnder

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq,Ord,Enum,Bounded,Show,Read)

allCardValues = [minBound..maxBound] :: [CardValue]

-- state our score, their score, last chosen card
type Score = Int -- Score = 

data AMove = Over | Under | Same
  deriving (Eq, Show, Read)
-- type AMove = Int                  -- a move for a player
-- type State = ([AMove], [AMove])   -- (mine,other's)

type State = (Score, Score, (CardValue, [CardValue])) -- my score, their score, previously chosen card value, remaining cards

data Action = Move AMove State   -- do AMove in State
            | Start              -- returns starting state

data Result = EndOfGame Int        -- end of game
            | ContinueGame State   -- continue with new state
         deriving (Eq, Show)

type Game = Action -> Result

type Player = Game -> Result -> AMove

------ The Over Under Game -------
overunder :: Game
overunder Start = ContinueGame (0, 0, (choose_card (allCardValues++allCardValues++allCardValues++allCardValues)))
overunder (Move move (myscore, theirscore, (previous_card, the_deck)))
    | length the_deck  == 0                 = EndOfGame 0      -- the deck is empty, draw
    | otherwise =
         overunder_helper (Move move (myscore, theirscore, (previous_card, new_deck))) chosen_card
         where
            (chosen_card, new_deck) = choose_card the_deck

overunder_helper (Move move (myscore, theirscore, (previous_card, the_deck))) chosen_card
    | new_score == 5                    = EndOfGame 1      -- agent wins
    | otherwise                         =
          ContinueGame (theirscore, new_score, (chosen_card, the_deck))
          where 
              new_score = (update_score move previous_card chosen_card myscore)


-- updates the score based on move made, previous card, current card, and current score
update_score Over previous current score
  | current > previous = score + 1
  | otherwise = score
update_score Under previous current score
  | current < previous = score + 1
  | otherwise = score
update_score Same previous current score
  | current == previous = score + 1
  | otherwise = score

-- TODO: make choose_card be random! for now just choosing the first card in the list!
-- USE THE BELOW FUNCTION (getrandomelementfromtlist)
choose_card (h:t) = (h, t)

-- returns a random element from any list
getrandomelementfromlist :: [a] -> IO a
getrandomelementfromlist xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

-- Remove element from list
removeElem _ [] = []
removeElem e (h:t)
  | h==e = t
  | otherwise = h:removeElem e t



------- A Player -------
simple_player :: Player
simple_player _ (ContinueGame (_, _, (_, remaining))) = Over



----------------------------------------------------------------------------------
--------------------------------- Smart Computer ---------------------------------
----------------------------------------------------------------------------------
-- given a card and the remaining deck, this will return the next best choice
-- where under=0, same=1, over=2
bestchoice c d = 
  calculatebestchoice 
    (countcards (>) c d) 
    (countcards (==) c d) 
    (countcards (<) c d) 
    (fromIntegral (length d))

-- returns the number of cards counted based on an operator op, the current card to compare c,
-- and the rest of the deck d
-- example: countcards (<) 1 [1,2,3]
countcards op c d = countcardshelper op c d 0

countcardshelper _ _ [] n = n
countcardshelper op c (h:t) n
  | op c h = countcardshelper op c t (n+1)
  | otherwise = countcardshelper op c t n

-- determines the best choice (lower=0, same=1, higher=2) based on the number of cards that are
-- stil in the deck that are lower than the current card l, the same as the current card s, 
-- higher h, and the number of cards that remain in the deck c
calculatebestchoice l s h c
  | ((probability l c) >= (probability s c)) && ((probability l c) >= (probability h c)) = 0
  | ((probability s c) >= (probability l c)) && ((probability s c) >= (probability h c)) = 1
  | otherwise = 2

probability numcardstocompare numcardsleftindeck = numcardstocompare / numcardsleftindeck
--------------------------------------------------------------------------------------
--------------------------------- End Smart Computer ---------------------------------
--------------------------------------------------------------------------------------



-- Test cases
-- overunder Start
-- overunder (Move 6 ([5,3],[2,7]))
-- overunder (Move 3 ([5,7],[2,9]))
