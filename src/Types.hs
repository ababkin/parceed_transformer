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
  , pFacultyTable   :: Maybe FacultyTable
  , pProgramFaculty :: Maybe ProgramFaculty
  , pWorkSchedule   :: Maybe WorkSchedule
  , pCallSchedules  :: Maybe CallSchedules
  , pEducationalEnvironment  :: Maybe EducationalEnvironment
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



data FacultyTable = FacultyTable {
    fFullTimePhysician    :: Maybe Int
  , fFullTimeNonPhysician :: Maybe Int
  , fPartTimePhysician    :: Maybe Int
  , fPartTimeNonPhysician :: Maybe Int
  } deriving Show

data ProgramFaculty = ProgramFaculty {
    fPercentageFemalePaid :: Maybe Text
  , fRatioFullTime        :: Maybe Text
  } deriving Show


instance FromJSON (Maybe FacultyTable) where
  parseJSON (Object o) =
    (sequenceA . fmap parseFacultyTable) =<< o .:? "faculty table"
  parseJSON o = typeMismatch "Maybe FacultyTable" o

parseFacultyTable facultyTable = do
    (Object t):_    <- facultyTable .: "tables"
    rs <- t .: "rows"
    dict <- catMaybes <$> mapM parseFacultyRow rs
    let fulltime = lookup "Full-time paid" dict
        parttime = lookup "Part-time paid" dict
    
    return $ FacultyTable (join (fst <$> fulltime)) (join (snd <$> fulltime))
                          (join (fst <$> parttime)) (join (snd <$> parttime))


instance FromJSON (Maybe ProgramFaculty) where
  parseJSON (Object o) =
    (sequenceA . fmap parseProgramFaculty) =<< o .:? "program faculty"
  parseJSON o = typeMismatch "Maybe ProgramFaculty" o

parseProgramFaculty programFaculty = do
    l <- programFaculty .: "list"
    is <- l .: "items"

    ProgramFaculty
      <$> is `lookupValue` "Percentage of full-time paid female physician faculty"
      <*> is `lookupValue` "Ratio of full-time equivalent paid faculty to positions"





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



data EducationalEnvironment = EducationalEnvironment {
    eeAvgScheduledLecturesFirstYear :: Int
  , eeTrainingFirstYear             :: Text
  , eeTrainingNonHospitalFirstYear  :: Text
  } deriving Show
instance FromJSON (Maybe EducationalEnvironment) where
  parseJSON (Object o) =
    (sequenceA . fmap parse) =<< (o .:? "education environment")

    where
      parse :: Value -> Parser EducationalEnvironment
      parse (Object o)   = do
          (Object l):_  <- o .: "lists"
          is            <- l .: "items"
          EducationalEnvironment
            <$> is `lookupParsedIntValue`     "Avg hours/week of regularly scheduled lectures/conferences during 1st year"
            <*> is `lookupValue`     "Training at hospital outpatient clinics during 1st year"
            <*> is `lookupValue`  "Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics"

instance ToNamedRecord Program 
  where
    toNamedRecord Program{pid, pTitle, pInfo, pFacultyTable, pProgramFaculty, pWorkSchedule, pCallSchedules, pEducationalEnvironment} = namedRecord [
        "Program Id"               .= pid
      , "Program Title"            .= pTitle
      
      , "Basic Information - Information - Accredited length of training"                                         .= showJust iLengthOfTraining pInfo
      , "Basic Information - Information - Required length"                                                       .= showJust iRequiredLength pInfo
      , "Basic Information - Information - Accepting applications for training that begins in 2016-2017"          .= showJust iAccepting20162017 pInfo
      , "Basic Information - Information - Will be accepting applications for training that begins in 2017-2018"  .= showJust iAccepting20172018 pInfo
      , "Basic Information - Information - Program start dates"                                                   .= just iProgramStarts pInfo
      , "Basic Information - Information - Participates in ERAS"                                                  .= showJust iEras pInfo
      , "Basic Information - Information - Affiliated with US government"                                         .= showJust iAffiliatedUsGov pInfo

      , "Faculty & Trainees - Program Faculty - Full-time paid - Physician"     .= showJustJust fFullTimePhysician pFacultyTable
      , "Faculty & Trainees - Program Faculty - Full-time paid - Non-physician" .= showJustJust fFullTimeNonPhysician pFacultyTable
      , "Faculty & Trainees - Program Faculty - Part-time paid - Physician"     .= showJustJust fPartTimePhysician pFacultyTable
      , "Faculty & Trainees - Program Faculty - Part-time paid - Non-physician" .= showJustJust fPartTimeNonPhysician pFacultyTable
      , "Faculty & Trainees - Program Faculty - Percentage of full-time paid female physician faculty"   .= joinJust fPercentageFemalePaid pProgramFaculty
      , "Faculty & Trainees - Program Faculty - Ratio of full-time equivalent paid faculty to positions" .= joinJust fRatioFullTime pProgramFaculty

      , "Work schedule - Work schedule - Avg hrs/wk on duty during first year (excluding beeper call)"                .= showJust wsAvgTimeOnDutyFirstYear pWorkSchedule
      , "Work schedule - Work schedule - Maximum consecutive hours on duty during first year (excluding beeper call)" .= showJust wsMaxConsecutiveHoursOnDutyFirstYear pWorkSchedule
      , "Work schedule - Work schedule - Average number of 24-hour off duty periods per week during first year"       .= showJust wsAvgNumOfOffDutyDaysPerWeekFirstYear pWorkSchedule
      , "Work schedule - Work schedule - Program allows moonlighting (beyond GY1)"                                    .= showJust wsAllowsMoonlighting pWorkSchedule
      , "Work schedule - Work schedule - Night float system"                                                          .= showJust wsNightFloat pWorkSchedule
      , "Work schedule - Work schedule - Offers awareness and management of fatigue in residents/fellows"             .= showJust wsOffersAwareness pWorkSchedule

      , "Work schedule - Call schedule - Most taxing schedule and frequency per year - Year 1"  .= showJustJust (fmap csMostTaxing .lookup 1) pCallSchedules
      , "Work schedule - Call schedule - Most taxing schedule and frequency per year - Year 2"  .= showJustJust (fmap csMostTaxing .lookup 2) pCallSchedules
      , "Work schedule - Call schedule - Most taxing schedule and frequency per year - Year 3"  .= showJustJust (fmap csMostTaxing .lookup 3) pCallSchedules
      , "Work schedule - Call schedule - Most taxing schedule and frequency per year - Year 4"  .= showJustJust (fmap csMostTaxing .lookup 4) pCallSchedules
      , "Work schedule - Call schedule - Beeper or home call (weeks/year) - Year 1"             .= showJustJust (fmap csBeeper . lookup 1) pCallSchedules
      , "Work schedule - Call schedule - Beeper or home call (weeks/year) - Year 2"             .= showJustJust (fmap csBeeper . lookup 2) pCallSchedules
      , "Work schedule - Call schedule - Beeper or home call (weeks/year) - Year 3"             .= showJustJust (fmap csBeeper . lookup 3) pCallSchedules
      , "Work schedule - Call schedule - Beeper or home call (weeks/year) - Year 4"             .= showJustJust (fmap csBeeper . lookup 4) pCallSchedules

      , "Educational environment - Educational Environment - Avg hours/week of regularly scheduled lectures/conferences during 1st year"                                              .= showJust eeAvgScheduledLecturesFirstYear pEducationalEnvironment
      , "Educational environment - Educational Environment - Training at hospital outpatient clinics during 1st year"                                                                 .= showJust eeTrainingFirstYear pEducationalEnvironment
      , "Educational environment - Educational Environment - Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics"  .= showJust eeTrainingNonHospitalFirstYear pEducationalEnvironment
      ]


showJustJust a = maybe "" (maybe "" show . a)
showJust a = maybe "" (show . a)
just = maybe ""
joinJust f = maybe "" (fromMaybe "" . f) 

