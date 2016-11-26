-- CPSC 312 - 2016 - Games in Haskell
module OverUnder where

-- To run it, try:
-- ghci
-- :load OverUnder

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq,Ord,Enum,Bounded,Show,Read)

allCardValues = [minBound..maxBound] :: [CardValue]

-- state our score, their score, last chosen card
type Score = Int -- Score = 

data AMove = Over | Under | Same
-- type AMove = Int                  -- a move for a player
-- type State = ([AMove], [AMove])   -- (mine,other's)

type State = (Score, Score, (CardValue, [CardValue])) -- my score, their score, previously chosen card value, remaining cards

data Action = Move AMove State   -- do AMove in State
            | Start              -- returns starting state

data Result = EndOfGame Int        -- end of game
            | ContinueGame State [AMove]   -- continue with new state, and list of possible moves
         deriving (Eq, Show)

type Game = Action -> Result

type Player = Game -> Result -> AMove

------ The Over Under Game -------

overunder :: Game
overunder (Move move (myscore, theirscore, (previous_card, the_deck)))
    -- (chosen, remaining) <- choose_card the_deck
    -- | chosen move previous_card             = EndOfGame 1      -- agent wins
    | length deck  == 0                 = EndOfGame 0      -- the deck is empty, draw
    | otherwise
         overunder_helper (Move move (myscore, theirscore, (previous_card, new_deck))) chosen_card
    where
       (chosen_card, new_deck) = choose_card the_deck

overunder_helper (Move move (myscore, theirscore, (previous_card, the_deck))) chosen_card
    | myscore == 5                      = EndOfGame 1      -- agent wins
    | otherwise                         =
          ContinueGame (theirscore, (update_score move previous_card chosen_card myscore), (chosen_card, the_deck))

overunder Start = ContinueGame (0, 0, (choose_card (allCardValues++allCardValues++allCardValues++allCardValues)))

update_score :: Int
update_score move previous current score =
  | current move previous = score+1
  | otherwise = score

-- TODO: make choose_card be random! for now just choosing the first card in the list!
choose_card h:t = (h, t)

------- A Player -------
simple_player :: Player
-- this player has an ordering of the moves, and chooses the fist one available
simple_player _ (ContinueGame _ avail) = head [e | e <- [5,6,4,2,8,1,3,7,9], e `elem` avail]


-- Test cases
-- overunder Start
-- overunder (Move 6 ([5,3],[2,7]))
-- overunder (Move 3 ([5,7],[2,9]))
