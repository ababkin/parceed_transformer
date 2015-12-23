{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (join)
import GHC.Generics
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import           Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
                                               toNamedRecord, (.=))

data Program = Program {
    pid       :: Text
  , pTitle    :: Text
  , pInfo     :: Maybe Info
  , pFaculty  :: Maybe Faculty
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



instance ToNamedRecord Program 
  where
    toNamedRecord Program{pid, pTitle, pInfo, pFaculty} = namedRecord [
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
      ]

showJustJust a = maybe "" (maybe "" show . a)
showJust a = maybe "" (show . a)
just = maybe ""
joinJust f = maybe "" (fromMaybe "" . f) 

