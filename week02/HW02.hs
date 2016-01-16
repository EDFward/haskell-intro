{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches actual guess = sum $ map fromEnum $ zipWith (==) actual guess

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = map (\color -> length $ filter (color==) code) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches c1 c2 = sum $ zipWith min (countColors c1) (countColors c2)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess =
  Move guess exact (matches actual guess - exact)
  where exact = exactMatches actual guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exact nonexact) code =
  exactMatches guess code == exact && matches guess code - exact == nonexact

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter $ isConsistent move

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes len
  | len <= 0 = []
  | len == 1 = map (:[]) colors
  | otherwise =
    concatMap (\color -> map (color:) $ allCodes (len - 1)) colors

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve secretCode =
  guess (initMove, allCodes len, [])
  where
    len = length secretCode
    initMove = getMove secretCode $ replicate len Red
    guess :: (Move, [Code], [Move]) -> [Move]
    guess (move@(Move guessCode _ _), candidates, history)
      | secretCode == guessCode = reverse newHistory
      | otherwise = guess (nextMove, newCandidates, newHistory)
      where
        newHistory = move : history
        newCandidates = filter (isConsistent move) candidates
        nextMove = getMove secretCode (head newCandidates)

-- Bonus ----------------------------------------------

-- Omitted.

fiveGuess :: Code -> [Move]
fiveGuess = undefined
