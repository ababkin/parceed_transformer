{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (join)
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
  toNamedRecord, (.=))
import Data.Traversable (sequenceA)

type CallSchedules = [(Int, CallSchedule)]

data Program = Program {
    pid       :: Text
  , pTitle    :: Text
  , pInfo     :: Maybe Info
  , pFaculty        :: Maybe Faculty
  , pWorkSchedule   :: Maybe WorkSchedule
  , pCallSchedules  :: Maybe CallSchedules
  } deriving Show

data Info = Info {
    iLengthOfTraining   :: Int
  , iRequiredLength     :: Int
  , iAccepting20162017  :: Bool
  , iAccepting20172018  :: Bool
  , iProgramStarts      :: Text
  , iEras               :: Bool
  , iAffiliatedUsGov    :: Bool
  } deriving Show


instance FromJSON Program where
  parseJSON obj@(Object o)   = Program
    <$> o .: "id"
    <*> o .: "title"
    <*> o .: "information"
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
  parseJSON o = typeMismatch "Program" o

instance FromJSON Info where
  parseJSON (Object o)   = do
    is <- o .: "items"
    Info
      <$> is `lookupParsedIntValue` "Accredited length of training"
      <*> is `lookupParsedIntValue` "Required length"
      <*> is `lookupParsedBoolValue` "Accepting applications for training that begins in 2016-2017"
      <*> is `lookupParsedBoolValue` "Will be accepting applications for training that begins in 2017-2018"
      <*> is `lookupValue` "Program start dates"
      <*> is `lookupParsedBoolValue` "Participates in ERAS"
      <*> is `lookupParsedBoolValue` "Affiliated with US government"
  parseJSON o = typeMismatch "Info" o


lookupValue 
  :: FromJSON a 
  => [Value] 
  -> Text 
  -> Parser a
lookupValue os k = (head . catMaybes) <$> mapM (\(Object o) -> o .:? k) os

lookupParsedIntValue 
  :: [Value] 
  -> Text 
  -> Parser Int
lookupParsedIntValue os k = read <$> lookupValue os k

lookupParsedDoubleValue 
  :: [Value] 
  -> Text 
  -> Parser Double
lookupParsedDoubleValue os k = read <$> lookupValue os k

lookupParsedBoolValue 
  :: [Value] 
  -> Text 
  -> Parser Bool
lookupParsedBoolValue os k = toBool <$> lookupValue os k

toBool :: Text -> Bool
toBool "Yes"  = True
toBool "No"   = False



data Faculty = Faculty {
    fFullTimePhysician    :: Maybe Int
  , fFullTimeNonPhysician :: Maybe Int
  , fPartTimePhysician    :: Maybe Int
  , fPartTimeNonPhysician :: Maybe Int
  , fPercentageFemalePaid :: Maybe Text
  , fRatioFullTime        :: Maybe Text
  } deriving Show

instance FromJSON Faculty where
  parseJSON (Object o)   = do
    facultyTable    <- o .: "faculty table"
    (Object t):_    <- facultyTable .: "tables"
    rs <- t .: "rows"
    dict <- catMaybes <$> mapM parseFacultyRow rs
    let fulltime = lookup "Full-time paid" dict
        parttime = lookup "Part-time paid" dict
    
    programFaculty  <- o .: "program faculty"
    l <- programFaculty .: "list"
    is <- l .: "items"

    Faculty (join (fst <$> fulltime)) (join (snd <$> fulltime))
            (join (fst <$> parttime)) (join (snd <$> parttime))
      <$> is `lookupValue` "Percentage of full-time paid female physician faculty"
      <*> is `lookupValue` "Ratio of full-time equivalent paid faculty to positions"

  parseJSON o = typeMismatch "Faculty" o

parseFacultyRow :: Value -> Parser (Maybe (Text, (Maybe Int, Maybe Int)))
parseFacultyRow (Object o) = do
  maybeFt <- o .:? "Faculty type" 
  case maybeFt of
    Just (ft :: Text) -> do
      vals <- (,) 
        <$> fmap (fmap read) (o .:? "Physician") 
        <*> fmap (fmap read) (o .:? "Non-physician")
      return $ Just (ft, vals)
    Nothing -> return Nothing

parseFacultyRow x = typeMismatch "Faculty Row" x


data WorkSchedule = WorkSchedule {
    wsAvgTimeOnDutyFirstYear :: Int
  , wsMaxConsecutiveHoursOnDutyFirstYear :: Int
  , wsAvgNumOfOffDutyDaysPerWeekFirstYear :: Double
  , wsAllowsMoonlighting :: Bool
  , wsNightFloat :: Bool
  , wsOffersAwareness :: Bool
  } deriving Show

instance FromJSON (Maybe WorkSchedule) where
  parseJSON (Object o) =
    (sequenceA . fmap parseWorkSchedule) =<< (o .:? "work schedule")

parseWorkSchedule :: Value -> Parser WorkSchedule
parseWorkSchedule (Object o)   = do
    (Object l)    <- o .: "list"
    is            <- l .: "items"
    WorkSchedule
      <$> is `lookupParsedIntValue`     "Avg hrs/wk on duty during first year (excluding beeper call)"
      <*> is `lookupParsedIntValue`     "Maximum consecutive hours on duty during first year (excluding beeper call)"
      <*> is `lookupParsedDoubleValue`  "Average number of 24-hour off duty periods per week during first year"
      <*> is `lookupParsedBoolValue`    "Program allows moonlighting (beyond GY1)"
      <*> is `lookupParsedBoolValue`    "Night float system"
      <*> is `lookupParsedBoolValue`    "Offers awareness and management of fatigue in residents/fellows"




data Schedule = NA | NGA | OTH | Num Int 
instance FromJSON Schedule where
  parseJSON (String "NA") = pure NA
  parseJSON (String "NGA") = pure NGA
  parseJSON (String "OTH") = pure OTH
  parseJSON (String n) = pure . Num . read $ T.unpack n
  parseJSON o = typeMismatch "Schedule" o
instance Show Schedule where
  show NA = "NA"
  show NGA= "NGA"
  show OTH = "OTH"
  show (Num n) = show n
  

data CallSchedule = CallSchedule {
    csMostTaxing  :: Schedule
  , csBeeper      :: Schedule
  } deriving Show

instance FromJSON (Maybe CallSchedules) where
  parseJSON (Object o)   = do
    maybeScheduleTable <- o .:? "work schedule"
    case maybeScheduleTable of
      Just scheduleTable -> do
        (Object t1)   <- scheduleTable .: "tables"
        (Object t2):_ <- t1 .: "tables"
        rs            <- t2 .: "rows"
        Just . catMaybes <$> mapM parseCallScheduleRow rs
      Nothing ->
        return Nothing
  parseJSON o = typeMismatch "Maybe CallSchedules" o

parseCallScheduleRow :: Value -> Parser (Maybe (Int, CallSchedule))
parseCallScheduleRow (Object o) = do
  maybeI <- o .:? "Year"
  maybe
    (return Nothing)
    (\i -> do
      mostTaxing  <- o .: "Most taxing schedule and frequency per year"
      beeper      <- o .: "Beeper or home call (weeks/year)"
      return $ Just (i, CallSchedule mostTaxing beeper)
    )
    (read <$> maybeI)

parseCallScheduleRow x = typeMismatch "Maybe (Int, CallSchedule)" x


instance ToNamedRecord Program 
  where
    toNamedRecord Program{pid, pTitle, pInfo, pFaculty, pWorkSchedule, pCallSchedules} = namedRecord [
        "ProgramId"               .= pid
      , "ProgramTitle"            .= pTitle
      
      , "InfoLengthOfTraining"    .= showJust iLengthOfTraining pInfo
      , "InfoRequiredLength"      .= showJust iRequiredLength pInfo
      , "InfoAccepting20162017"   .= showJust iAccepting20162017 pInfo
      , "InfoAccepting20172018"   .= showJust iAccepting20172018 pInfo
      , "InfoProgramStartDates"   .= just iProgramStarts pInfo
      , "InfoParticipatesInEras"  .= showJust iEras pInfo
      , "InfoAffiliatedWithUsGov" .= showJust iAffiliatedUsGov pInfo

      , "FacultyFullTimePhysician"    .= showJustJust fFullTimePhysician pFaculty
      , "FacultyFullTimeNonPhysician" .= showJustJust fFullTimeNonPhysician pFaculty
      , "FacultyPartTimePhysician"    .= showJustJust fPartTimePhysician pFaculty
      , "PercentageFullTimePaidFemalePhysicianFaculty"  .= joinJust fPercentageFemalePaid pFaculty
      , "RatioFullTimeEquivalentPaidFacultyToPositions" .= joinJust fRatioFullTime pFaculty

      , "AvgTimeOnDutyDuringFirstYearExcludingBeeperCall"                             .= showJust wsAvgTimeOnDutyFirstYear pWorkSchedule
      , "Maximum consecutive hours on duty during first year (excluding beeper call)" .= showJust wsMaxConsecutiveHoursOnDutyFirstYear pWorkSchedule
      , "Average number of 24-hour off duty periods per week during first year"       .= showJust wsAvgNumOfOffDutyDaysPerWeekFirstYear pWorkSchedule
      , "Program allows moonlighting (beyond GY1)"                                    .= showJust wsAllowsMoonlighting pWorkSchedule
      , "Night float system"                                                          .= showJust wsNightFloat pWorkSchedule
      , "Offers awareness and management of fatigue in residents/fellows"             .= showJust wsOffersAwareness pWorkSchedule

      , "Most taxing schedule and frequency per year - Year 1"  .= showJustJust (fmap csMostTaxing .lookup 1) pCallSchedules
      , "Most taxing schedule and frequency per year - Year 2"  .= showJustJust (fmap csMostTaxing .lookup 2) pCallSchedules
      , "Most taxing schedule and frequency per year - Year 3"  .= showJustJust (fmap csMostTaxing .lookup 3) pCallSchedules
      , "Most taxing schedule and frequency per year - Year 4"  .= showJustJust (fmap csMostTaxing .lookup 4) pCallSchedules
      , "Beeper or home call (weeks/year) - Year 1"             .= showJustJust (fmap csBeeper . lookup 1) pCallSchedules
      , "Beeper or home call (weeks/year) - Year 2"             .= showJustJust (fmap csBeeper . lookup 2) pCallSchedules
      , "Beeper or home call (weeks/year) - Year 3"             .= showJustJust (fmap csBeeper . lookup 3) pCallSchedules
      , "Beeper or home call (weeks/year) - Year 4"             .= showJustJust (fmap csBeeper . lookup 4) pCallSchedules
      ]


showJustJust a = maybe "" (maybe "" show . a)
showJust a = maybe "" (show . a)
just = maybe ""
joinJust f = maybe "" (fromMaybe "" . f) 

