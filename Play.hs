-- CPSC 312 - 2016 - Games in Haskell
module Play where

-- To run it, try:
-- ghci
-- :load Play

import System.IO
import OverUnder

type TournammentState = (Int,Int,Int)   -- wins, losses, ties

play :: Game -> Result -> Player -> TournammentState -> IO TournammentState

play game start opponent tournament_state =
  let (wins, losses,ties) = tournament_state in
   do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
      putStrLn "Who starts? 0=you, 1=computer, 2=exit."
      line <- getLine
      if (read line :: Int)==0
      then
            person_play game start opponent tournament_state
      else if (read line :: Int)==1
           then
               computer_play game start opponent tournament_state
            else
               return tournament_state

person_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- opponent has played, the person must now play
person_play game (EndOfGame 1) opponent (wins,losses,ties) =
   do
      putStrLn "Computer won!"
      play game (game Start) opponent (wins,losses+1,ties)
person_play game (EndOfGame 0) opponent (wins,losses,ties) =
   do
      putStrLn "I't a draw"
      play game (game Start) opponent (wins,losses,ties+1)
person_play game (ContinueGame (s1, s2, (card, deck))) opponent tournament_state =
   do
      putStrLn ("Previous card is "++show card++". Score is player: "++show s1++ ", computer: "++show s2++" Choose one of Over, Under, Same")
      line <- getLine
      computer_play game (game (Move (read line :: AMove) (s1, s2, (card, deck)))) opponent tournament_state

computer_play :: Game -> Result -> Player -> TournammentState -> IO TournammentState
-- computer_play game current_result opponent tournament_state
-- person has played, the computer must now play
computer_play game (EndOfGame 1) opponent (wins,losses,ties) =
   do
      putStrLn "You won!"
      play game (game Start) opponent (wins+1,losses,ties)
computer_play game (EndOfGame 0) opponent (wins,losses,ties) =
   do
      putStrLn "I't a draw"
      play game (game Start) opponent (wins,losses,ties+1)
      
computer_play game result opponent tournament_state =
      let ContinueGame state = result
          opponent_move = opponent game result
        in
          do
            putStrLn ("The computer chose "++show opponent_move)
            person_play game (game (Move opponent_move state)) opponent tournament_state
      

-- play overunder (overunder Start) simple_player (0,0,0)
