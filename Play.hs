-- CPSC 312 - 2016 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play

import System.IO
import OverUnder
import System.Random
import qualified Data.Char as Char

type TournammentState = (Int,Int,Int)   -- wins, losses, ties

-- capitalizes the first letter, and puts the rest into lowercase
capitalized :: String -> String
capitalized (head:tail) = Char.toUpper head : map Char.toLower tail
capitalized [] = []

play :: Game init -> Result init -> Player init -> TournammentState -> IO TournammentState

play game start opponent tournament_state =
  let (wins, losses,ties) = tournament_state in
   do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      putStrLn(" ")
      line <- getLine
      if (read line :: Int)==0
      then
            person_play game start opponent tournament_state
      else if (read line :: Int)==1
           then
               computer_play game start opponent tournament_state
            else
               return tournament_state

person_play :: Game init -> Result init -> Player init -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play
person_play game (EndOfGame 1 init) opponent (wins,losses,ties) =
   do
      putStrLn "Computer won!"
      putStrLn(" ")
      play game (game (Start init)) opponent (wins,losses+1,ties)
person_play game (EndOfGame 0 init) opponent (wins,losses,ties) =
   do
      putStrLn "I't a draw"
      putStrLn(" ")
      play game (game (Start init)) opponent (wins,losses,ties+1)
person_play game (ContinueGame (s1, s2, (card, deck), init)) opponent tournament_state =
   do
      putStrLn ("The currently displayed card is "++show card++".")
      putStrLn("Score is player: "++show s1++ ", computer: "++show s2) 
      putStrLn("Choose one of Over, Under, Same")
      line <- getLine
      computer_play game (game (Move (read (capitalized line) :: AMove) (s1, s2, (card, deck), init))) opponent tournament_state

computer_play :: Game init -> Result init -> Player init -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent tournament_state
-- person has played, the computer must now play
computer_play game (EndOfGame 1 init) opponent (wins,losses,ties) =
   do
      putStrLn "You won!"
      putStrLn(" ")
      play game (game (Start init)) opponent (wins+1,losses,ties)
computer_play game (EndOfGame 0 init) opponent (wins,losses,ties) =
   do
      putStrLn "I't a draw"
      putStrLn(" ")
      play game (game (Start init)) opponent (wins,losses,ties+1)
      
computer_play game (ContinueGame (s1, s2, (card, deck), init)) opponent tournament_state =

      let result = (ContinueGame (s1, s2, (card, deck), init))
          ContinueGame state = result
          opponent_move = opponent game result
        in
          do
            putStrLn(" ")
            putStrLn ("Flipping next card... the card is "++show card)
            putStrLn ("**  Computer's turn. The computer chose "++show opponent_move)
            putStrLn ("Flipping card...")
            putStrLn(" ")
            person_play game (game (Move opponent_move state)) opponent tournament_state

playOU player = do
  g <- getStdGen 
  play overunder (overunder (Start (randoms g :: [Int]))) player (0,0,0)


-- To play:
-- playOU simple_player
-- playOU optimal_player

