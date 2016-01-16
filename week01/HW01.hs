{-# OPTIONS_GHC -Wall #-}

module HW01 where

import           Data.Char

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith (\i v -> v + v * mod i 2) [0..]

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
toDigits :: Integer -> [Int]
toDigits n
  | n <= 0 = []
  | otherwise = map digitToInt . show $ n

sumDigits :: [Integer] -> Integer
sumDigits = toInteger . sum . map (sum . toDigits)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = mod (sumDigits . doubleEveryOther . toRevDigits $ n) 10 == 0

-- Exercise 6 -----------------------------------------

-- Omitted.
