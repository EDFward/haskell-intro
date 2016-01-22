{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TupleSections       #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import           Cards
import           Prelude              hiding (mapM)

import           Control.Monad        hiding (liftM, mapM)
import           Control.Monad.Random
import           Data.Monoid
import           Data.Vector          (Vector, (!), (!?), (//))

import qualified Data.Vector          as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f m = m >>= return . f

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v =
  liftM3 (\vi vj v' -> v' // [(i, vj), (j, vi)]) (v !? i) (v !? j) (Just v)

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts indices v = mapM (v!?) indices

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = (v!?) <$> getRandomR (0, length v - 1)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec len = sequence $ V.replicate len getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR len = sequence . V.replicate len . getRandomR

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = do
  switchPairs <- mapM (\i -> liftM (i,) (getRandomR (0, i))) . reverse $ indices
  return $ foldr swap v switchPairs
  where
    indices = [1 .. length v - 1]
    swap (i, j) v' = v' // [(i, v' ! j), (j, v' ! i)]
-- A slightly faster way: https://github.com/ROKT-CIS-194/cis-194-homework/blob/52460da5d8d16d56c03f74bb0c990dc33419b92f/src/CIS194/BenS/HW07.hs
-- shuffle v' = foldM go v' ixs
--   where
--     ixs = [V.length v' - 1, V.length v' - 2 .. 0]
--     go v i = do
--       j <- getRandomR (0, i)
--       return (v // [(i, v ! j), (j, v ! i)])

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = (lt, p, ge)
  where
    p = v ! i
    (lt, ge) = V.unstablePartition (<p) (V.take i v V.++ V.drop (i + 1) v)

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.null v = V.empty
  | otherwise = qsort [ y | y <- xs, y < x ]
                <> V.cons x (qsort [ y | y <-xs, y >= x ])
  where
    x = V.head v
    xs = V.tail v

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.null v = return V.empty
  | otherwise = do
      (lt, p, ge) <- partitionAt v <$> getRandomR (0, length v - 1)
      sortedLt <- qsortR lt
      sortedGe <- qsortR ge
      return $ sortedLt <> V.cons p sortedGe

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select n v
  | n < 0 || n >= length v = return Nothing
  | otherwise = do
    (lt, p, ge) <- partitionAt v <$> getRandomR (0, length v - 1)
    let len = length lt
    case compare n len of
      LT -> select n lt
      EQ -> return $ Just p
      GT -> select (n - len - 1) ge

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [ Card l s | l <- labels, s <- suits ]
-- A clever way of fmap: https://github.com/ROKT-CIS-194/cis-194-homework/blob/52460da5d8d16d56c03f74bb0c990dc33419b92f/src/CIS194/BenS/HW07.hs
-- allCards = flip Card <$> suits <*> labels

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.null d = Nothing
  | otherwise = Just (V.head d, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards 0 d = Just ([], d)
getCards n d = do
  (c, d') <- nextCard d
  (cs, d'') <- getCards (n - 1) d'
  return (c : cs, d'')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
