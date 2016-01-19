{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns #-}
module HW06 where

import           Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 10 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons val s) = val : streamToList s

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons val s) = Cons (f val) (fmap f s)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat val = Cons val (sRepeat val)

sIterate :: (a -> a) -> a -> Stream a
sIterate f val = Cons val (sIterate f $ f val)

sInterleave :: Stream a -> Stream a -> Stream a
-- Note that the second argument should not be destructured. Otherwise ruler
-- stream will not work.
sInterleave (Cons val s) s2 = Cons val (sInterleave s2 s)

sTake :: Int -> Stream a -> [a]
sTake n = take n . streamToList

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (1+) 0

ruler :: Stream Integer
ruler = sInterleave (sRepeat 0) ((1+) <$> ruler)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand seed = Cons next (rand next)
  where next = 1103515245 * seed + 12345 `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 223 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = foldl' compare' Nothing
  where
    compare' Nothing v = Just (v, v)
    -- Use bang to force strict evaluation.
    compare' (Just (!prevMin, !prevMax)) v = Just(min v prevMin, max v prevMax)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

-- Omitted.

fastFib :: Int -> Integer
fastFib = undefined
