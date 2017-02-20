module RandomTest where

import System.Random

main = do
   x <- randomIO :: IO Int
   print (x `mod` 2)


getrandom = do
   x <- randomRIO (0,10) :: IO Int
   print x


pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)


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
  | ((probability l c) > (probability s c)) && ((probability l c) > (probability h c)) = 0
  | ((probability s c) > (probability l c)) && ((probability s c) > (probability h c)) = 1
  | otherwise = 2

probability numcardstocompare numcardsleftindeck = numcardstocompare / numcardsleftindeck


testwhere a b = x+1
    where
        x = a + b
