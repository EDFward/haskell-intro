{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Parser ( encode
              , decode
              , Transaction(..)
              , TId
              , FromJSON(..)
              , ToJSON(..)
              ) where

import           Control.Applicative
import           Data.Aeson
import           Data.Monoid

type TId = String

data Transaction = Transaction { from   :: String
                               , to     :: String
                               , amount :: Integer
                               , tid    :: TId
                               }
                   deriving (Show, Eq)

instance FromJSON Transaction where
    parseJSON (Object v) = Transaction   <$>
                           v .: "from"   <*>
                           v .: "to"     <*>
                           v .: "amount" <*>
                           v .: "tid"
    parseJSON _ = mempty

instance ToJSON Transaction where
    toJSON Transaction{..} = object [ "from"   .= from
                                    , "to"     .= to
                                    , "amount" .= amount
                                    , "tid"    .= tid
                                    ]
