{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module StatReport where

import Colonnade
import Data.Foldable (maximumBy, minimumBy)
import Data.Ord (comparing)
import Data.Time (diffDays)
import Fmt
import QuoteData

decimalPlacesFloating = 2

data StatValue = StatValue {decimalPlaces :: Int, value :: Double}

data StatEntry = StatEntry
  { qField :: QField,
    meanVal :: StatValue,
    minVal :: StatValue,
    maxVal :: StatValue,
    daysBetweenMinMax :: Int
  }

mean :: (Fractional a, Foldable t) => t a -> a
mean xs = sum xs / fromIntegral (length xs)

computeMinMaxDays :: (Ord a, Foldable t) => (QuoteData -> a) -> t QuoteData -> (a, a, Int)
computeMinMaxDays get qs = (get minQ, get maxQ, days)
  where
    cmp = comparing get
    minQ = minimumBy cmp qs
    maxQ = maximumBy cmp qs
    days = fromIntegral $ abs $ diffDays (day minQ) (day maxQ)

statInfo :: (Functor t, Foldable t) => t QuoteData -> [StatEntry]
statInfo qs = qFieldStatInfo <$> [minBound .. maxBound]
  where
    decimalPlacesByQField Volume = 0
    decimalPlacesByQField _ = decimalPlacesFloating

    qFieldStatInfo qField =
      let get = field2Fun qField
          (mn, mx, daysBetweenMinMax) = computeMinMaxDays get qs
          decPlaces = decimalPlacesByQField qField
          meanVal = StatValue decimalPlacesFloating (mean $ get <$> qs)
          minVal = StatValue decPlaces mn
          maxVal = StatValue decPlaces mx
       in StatEntry {..}

instance Buildable StatValue where
  build sv = fixedF (decimalPlaces sv) (value sv)

showPrice :: Double -> Builder
showPrice = fixedF decimalPlacesFloating

instance Buildable StatEntry where
  build StatEntry {..} =
    "Stats for " +|| qField ||+ ": "
      +| meanVal |+ " (mean), "
      +| minVal |+ " (min), "
      +| maxVal |+ " (max), "
      +| daysBetweenMinMax |+ " (days) "

textReport :: [StatEntry] -> String
textReport = ascii colStats
  where
    colStats =
      mconcat
        [ headed "Quote Field" (show . qField),
          headed "Mean" (pretty . meanVal),
          headed "Min" (pretty . minVal),
          headed "Max" (pretty . maxVal),
          headed "Days between Min/Max" (pretty . daysBetweenMinMax)
        ]