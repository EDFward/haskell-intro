{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module HW04 where

import           Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [0, 1]

-- Exercise 2 ----------------------------------------

instance (Num a, Eq a) => Eq (Poly a) where
  poly1@(P ls1) == poly2@(P ls2)
    | len1 == len2 = ls1 == ls2
    | len1 < len2 =
        (take len1 ls2 == ls1) && all (==0) (drop len1 ls2)
    | otherwise = poly2 == poly1
    where len1 = length ls1; len2 = length ls2

-- Exercise 3 -----------------------------------------

showDegree :: Int -> String
showDegree 0 = ""
showDegree 1 = "x"
showDegree n = "x^" ++ show n

instance (Num a, Eq a, Show a) => Show (Poly a) where
  show (P []) = "0"
  show (P coefs) = intercalate " + "
                 . reverse
                 . filter (not . null)
                 $ zipWith render coefs ([0..] :: [Int])
    where
      render 0 _ = ""
      render coef 0 = show coef
      render 1 deg = showDegree deg
      render (-1) deg = '-' : showDegree deg
      render coef deg = show coef ++ showDegree deg

-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus poly1@(P coefs1) poly2@(P coefs2)
  | len1 == len2 = P commonSum
  | len1 < len2 =
      P $ commonSum ++ drop len1 coefs2
  | otherwise = plus poly2 poly1
  where
    len1 = length coefs1; len2 = length coefs2
    commonSum = zipWith (+) coefs1 coefs2

-- Exercise 5 -----------------------------------------

times :: Num a => Poly a -> Poly a -> Poly a
times (P coefs1) (P coefs2) =
  sum $ zipWith shiftAndTimes coefs1 ([0..] :: [Int])
  where
    shiftAndTimes coef1 shift = P $ map (coef1*) $ replicate shift 0 ++ coefs2

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
  (+) = plus
  (*) = times
  negate (P coefs) = P $ map negate coefs
  fromInteger i = P [fromInteger i]
  -- No meaningful definitions exist
  abs    = undefined
  signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P coefs) toApply =
  sum $ zipWith (\coef deg -> toApply ^ deg * coef) coefs ([0..] :: [Int])

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
  deriv  :: a -> a
  nderiv :: Int -> a -> a
  nderiv n d
    | n <= 0 = error "Cannot differentiate with n <= 0"
    | n == 1 = deriv d
    | otherwise = nderiv (n - 1) (deriv d)

-- Exercise 9 -----------------------------------------

instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P coefs) = P $ zipWith (*) (drop 1 coefs) [1..]
