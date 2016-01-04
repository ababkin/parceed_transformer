{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Types.Schedule where

import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (join, (>=>), msum)
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe, isJust)
import Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
  toNamedRecord, (.=))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Text.Read (readMaybe)

import Types.Util



type CallScheduleTable = [(Int, CallSchedule)]

data WorkSchedule = WorkSchedule {
    wsAvgTimeOnDutyFirstYear              :: Int
  , wsMaxConsecutiveHoursOnDutyFirstYear  :: Int
  , wsAvgNumOfOffDutyDaysPerWeekFirstYear :: Double
  , wsAllowsMoonlighting                  :: Bool
  , wsNightFloat                          :: Bool
  , wsOffersAwareness                     :: Bool
  } deriving Show

instance FromJSON (Maybe WorkSchedule) where
  parseJSON = runMaybeT . ( 
      parseTab "work-schedule" >=> 
      parseArticles >=>
      lookupByTitle "Work schedule" >=>
      parseWorkSchedule
    )

parseWorkSchedule :: Value -> MaybeT Parser WorkSchedule
parseWorkSchedule (Object ws)   = do
    is <- lift $ ws .: "items"
    WorkSchedule 
      <$> lookupParsedIntValue     "Avg hrs/wk on duty during first year (excluding beeper call)" is
      <*> lookupParsedIntValue     "Maximum consecutive hours on duty during first year (excluding beeper call)" is
      <*> lookupParsedDoubleValue  "Average number of 24-hour off duty periods per week during first year" is
      <*> lookupParsedBoolValue    "Program allows moonlighting (beyond GY1)" is
      <*> lookupParsedBoolValue    "Night float system            (Residents participate during first year)" is
      <*> lookupParsedBoolValue    "Offers awareness and management of fatigue in residents/fellows" is




data Schedule = NA | NGA | OTH | Num Int | Str Text
instance FromJSON Schedule where
  parseJSON (String "NA") = pure NA
  parseJSON (String "NGA") = pure NGA
  parseJSON (String "OTH") = pure OTH
  parseJSON (String s) = pure $
    case readMaybe $ T.unpack s of
      Just n -> Num n
      Nothing -> Str s
  parseJSON o = typeMismatch "Schedule" o
instance Show Schedule where
  show NA = "NA"
  show NGA= "NGA"
  show OTH = "OTH"
  show (Num n) = show n
  show (Str t) = T.unpack t
  

data CallSchedule = CallSchedule {
    csMostTaxing  :: Schedule
  , csBeeper      :: Schedule
  } deriving Show

instance FromJSON (Maybe CallScheduleTable) where
  parseJSON = runMaybeT . (
      parseTab "work-schedule" >=> 
      parseArticles >=>
      msum . map parseTables >=>
      lookupByTitle "Call schedule" >=>
      parseColumns >=>
      parseCallScheduleTable
    )

parseCallScheduleTable :: [Value] -> MaybeT Parser CallScheduleTable
parseCallScheduleTable cs = do
    (Object yo)    <- lookupByTitle "Year" cs
    ys         <- lift $ yo .: "values"
    
    (Object mto)   <- lookupByTitle "Most taxing schedule and frequency per year" cs
    mts    <- lift $ mto .: "values"
    
    (Object bo)    <- lookupByTitle "Beeper or home call (weeks/year)" cs
    bs        <- lift $ bo .: "values"

    mapM (\(String y, mt, b) -> 
          lift ( (readText y, ) <$> (CallSchedule <$> parseJSON mt <*> parseJSON b) )
        ) $ zip3 ys mts bs

readText = read . T.unpack


workPrefix = "Work schedule - Work schedule - "
callPrefix = "Work schedule - Call schedule - "

scheduleFields 
  :: Maybe WorkSchedule
  ->  Maybe CallScheduleTable
  -> [(ByteString, ByteString)]
scheduleFields work call = prefixWith workPrefix [
    "Avg hrs/wk on duty during first year (excluding beeper call)"                .= showJust wsAvgTimeOnDutyFirstYear work
  , "Work schedule - Work schedule - Maximum consecutive hours on duty during first year (excluding beeper call)" .= showJust wsMaxConsecutiveHoursOnDutyFirstYear work
  , "Work schedule - Work schedule - Average number of 24-hour off duty periods per week during first year"       .= showJust wsAvgNumOfOffDutyDaysPerWeekFirstYear work
  , "Work schedule - Work schedule - Program allows moonlighting (beyond GY1)"                                    .= showJust wsAllowsMoonlighting work
  , "Work schedule - Work schedule - Night float system"                                                          .= showJust wsNightFloat work
  , "Work schedule - Work schedule - Offers awareness and management of fatigue in residents/fellows"             .= showJust wsOffersAwareness work
  ] ++ prefixWith callPrefix [
    "Most taxing schedule and frequency per year - Year 1"  .= showJustJust (fmap csMostTaxing .lookup 1) call
  , "Most taxing schedule and frequency per year - Year 2"  .= showJustJust (fmap csMostTaxing .lookup 2) call
  , "Most taxing schedule and frequency per year - Year 3"  .= showJustJust (fmap csMostTaxing .lookup 3) call
  , "Most taxing schedule and frequency per year - Year 4"  .= showJustJust (fmap csMostTaxing .lookup 4) call
  , "Beeper or home call (weeks/year) - Year 1"             .= showJustJust (fmap csBeeper . lookup 1) call
  , "Beeper or home call (weeks/year) - Year 2"             .= showJustJust (fmap csBeeper . lookup 2) call
  , "Beeper or home call (weeks/year) - Year 3"             .= showJustJust (fmap csBeeper . lookup 3) call
  , "Beeper or home call (weeks/year) - Year 4"             .= showJustJust (fmap csBeeper . lookup 4) call
  ]



