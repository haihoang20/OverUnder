-- CPSC 312 - 2016 - Games in Haskell
module OverUnder where

-- To run it, try:
-- ghci
-- :load OverUnder

type AMove = Int                  -- a move for a player
type State = ([AMove], [AMove])   -- (mine,other's)

data Action = Move AMove State   -- do AMove in State
            | Start              -- returns starting state

data Result = EndOfGame Int        -- end of game
            | ContinueGame State [AMove]   -- continue with new state, and list of possible moves
         deriving (Eq, Show)

type Game = Action -> Result

type Player = Game -> Result -> AMove

------ The Magic Sum Game -------

overunder :: Game
overunder (Move move (mine, others))
    | win move mine                     = EndOfGame 1      -- agent wins
    | length mine + length others == 8  = EndOfGame 0      -- no more moves, draw
    | otherwise                         =
          ContinueGame (others, move:mine)
                 [act | act <- [1..9], not (act `elem` move:mine++others)]

overunder Start = ContinueGame ([],[]) [1..9]

-- win n ns = the agent wins if it selects n given it has already selected ns
win n ns  = or [n+x+y==15 | x <- ns, y <- ns, x/=y]

------- A Player -------

simple_player :: Player
-- this player has an ordering of the moves, and chooses the fist one available
simple_player _ (ContinueGame _ avail) = head [e | e <- [5,6,4,2,8,1,3,7,9], e `elem` avail]


-- Test cases
-- overunder Start
-- overunder (Move 6 ([5,3],[2,7]))
-- overunder (Move 3 ([5,7],[2,9]))
