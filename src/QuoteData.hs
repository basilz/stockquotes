{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module QuoteData where

import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField (..))

data QuoteData = QuoteData
  { day :: Day,
    volume :: Int,
    open :: Double,
    close :: Double,
    high :: Double,
    low :: Double
  } deriving (Generic, FromNamedRecord)

instance FromField Day where
  parseField = parseTimeM False defaultTimeLocale "%Y-%m-%d" . unpack

data QField = Open | Close | High | Low | Volume deriving (Eq, Ord, Show, Enum, Bounded)

field2Fun :: QField -> QuoteData -> Double
field2Fun Open = open
field2Fun Close = close
field2Fun High = high
field2Fun Low = low
field2Fun Volume = fromIntegral . volume


