{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module HW05 where

import           Data.Bits            (xor)
import           Data.List            (sortBy)
import           Data.Ord             (comparing)
import qualified Data.Set             as Set

import           Data.ByteString.Lazy (ByteString)
import           Data.Map.Strict      (Map)
import           System.Environment   (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict      as Map

import           Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret f1 f2 = do
  bytes1 <- BS.unpack <$> BS.readFile f1
  bytes2 <- BS.unpack <$> BS.readFile f2
  return $ BS.pack $ filter (/=0) $ zipWith xor bytes1 bytes2

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey secret f = do
  bytes <- BS.unpack <$> BS.readFile f
  let decrypted = BS.pack $ zipWith xor bytes (cycle . BS.unpack $ secret)
  -- Remove ".enc" extension.
  let newF = take (length f - 4) f
  BS.writeFile newF decrypted

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile f = do
  s <- BS.readFile f
  return $ decode s

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimF tF = do
  victims <- parseFile victimF :: IO (Maybe [TId])
  ts <- parseFile tF :: IO (Maybe [Transaction])
  let victimSet = maybe Set.empty Set.fromList victims
  if Set.null victimSet
    then return $ Just []
    else return $ filter (\t -> Set.member (tid t) victimSet) <$> ts

-- Exercise 5 -----------------------------------------

-- Update balance with a list of transactions. Accounts with 0
-- balance are excluded.
updateBalaceFromTs :: Map String Integer -> [Transaction] -> Map String Integer
updateBalaceFromTs balance ts = Map.filter (/=0) $ foldr handleT balance ts
  where
    update :: Integer -> Maybe Integer -> Maybe Integer
    update delta = Just . maybe delta (delta+)
    handleT :: Transaction -> Map String Integer -> Map String Integer
    handleT t = Map.alter (update . amount $ t) (to t) .
                Map.alter (update . negate . amount $ t) (from t)

getFlow :: [Transaction] -> Map String Integer
getFlow = updateBalaceFromTs Map.empty

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = maybe (error "Money flow is empty!") fst
            . Map.foldrWithKey compareMoney Nothing
  where
    compareMoney k v Nothing = Just (k, v)
    compareMoney name money (Just prev@(_, prevMoney)) =
      Just $ if money > prevMoney then (name, money) else prev

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs balance tIds
  | Map.null balance = []
  | otherwise = newTs ++ undoTs newBalance (drop (length newTs) tIds)
  where
    (payerMap, payeeMap) = Map.partition (>0) balance
    payers = sortBy (flip $ comparing snd) (Map.toList payerMap)
    payees = sortBy (flip $ comparing snd) (Map.toList payeeMap)
    newT :: (String, Integer) -> (String, Integer) -> TId -> Transaction
    newT (from, owes) (to, owed) tid = Transaction { from = from
                                                   , to = to
                                                   , amount = min owes (-owed)
                                                   , tid = tid
                                                   }
    newTs = zipWith3 newT payers payees tIds
    newBalance = updateBalaceFromTs balance newTs

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON f = BS.writeFile f . encode

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  let decryptedVict = take (length vict - 4) vict
  mts <- getBadTs decryptedVict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json.enc"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
