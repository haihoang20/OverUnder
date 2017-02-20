module OverUnder where

import System.Random

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq,Ord,Enum,Bounded,Show,Read)

allCardValues = [minBound..maxBound] :: [CardValue]

-- state our score, their score, last chosen card
type Score = Int -- Score = 

data AMove = Over | Under | Same
  deriving (Eq, Show, Read)
-- type AMove = Int                  -- a move for a player
-- type State = ([AMove], [AMove])   -- (mine,other's)

type State = (Score, Score, (CardValue, [CardValue]), [Int]) -- my score, their score, previously chosen card value, remaining cards, infinite list of random numbers

data Action init = Move AMove State   -- do AMove in State
            | Start init             -- returns starting state

data Result init = EndOfGame Int init    -- end of game
            | ContinueGame State   -- continue with new state
         deriving (Eq, Show)

type Game init = Action init -> Result init

type Player init = Game init -> Result init -> AMove

------ The Over Under Game -------
overunder :: Game [Int]
overunder (Start (rh:rt)) = ContinueGame (0, 0, (choose_card (allCardValues++allCardValues++allCardValues++allCardValues) (rh:rt)), rt)
overunder (Move move (myscore, theirscore, (previous_card, the_deck), (rh:rt)))
    | length the_deck  == 0                 = EndOfGame 0 (rh:rt)      -- the deck is empty, draw
    | otherwise =
         overunder_helper (Move move (myscore, theirscore, (previous_card, new_deck), (rh:rt))) chosen_card
         where
           (chosen_card, new_deck) = choose_card the_deck (rh:rt)

overunder_helper (Move move (myscore, theirscore, (previous_card, the_deck), (rh:rt))) chosen_card
    | new_score == 15                   = EndOfGame 1 (rh:rt)      -- agent wins
    | otherwise                         =
          ContinueGame (theirscore, new_score, (chosen_card, the_deck), rt)
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

choose_card deck (rh:rt) = (x, (removeElem x deck))
  where
    x = deck !! (rh `mod` (length deck))

-- Remove element from list
removeElem _ [] = []
removeElem e (h:t)
  | h==e = t
  | otherwise = h:removeElem e t

------- Simple Player - This player just choses randomely -------
simple_player :: Player init
simple_player _ (ContinueGame (_, _, (_, remaining), (rh:rt))) = [Over, Under, Same] !! (rh `mod` 3)


------- Optimal Player - This player chooses based on calculating probability -------
optimal_player :: Player init
optimal_player _ (ContinueGame (_, _, (card, deck), _)) = bestchoice card deck
      
----------------------------------------------------------------------------------
--------------------------------- Smart Computer Calculator ----------------------
----------------------------------------------------------------------------------
-- given a card and the remaining deck, this will return the next best choice
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

-- determines the best choice (Over, Under, or Same)  based on the number of cards that are
-- stil in the deck that are lower than the current card l, the same as the current card s, 
-- higher h, and the number of cards that remain in the deck c
calculatebestchoice l s h c
  | ((probability l c) >= (probability s c)) && ((probability l c) >= (probability h c)) = Under
  | ((probability s c) >= (probability l c)) && ((probability s c) >= (probability h c)) = Same
  | otherwise = Over

probability numcardstocompare numcardsleftindeck = numcardstocompare / numcardsleftindeck
