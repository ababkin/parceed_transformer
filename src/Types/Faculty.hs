{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Faculty where

import Control.Monad.Trans.Class (lift)
import Data.ByteString (ByteString)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (join, (>=>), msum)
import GHC.Generics
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (fromJust, catMaybes, listToMaybe, fromMaybe)
import Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
  toNamedRecord, (.=))
import Data.Traversable (sequenceA)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

import Types.Util


data FacultyTable = FacultyTable {
    fFullTimePhysician    :: Maybe Int
  , fFullTimeNonPhysician :: Maybe Int
  , fPartTimePhysician    :: Maybe Int
  , fPartTimeNonPhysician :: Maybe Int
  } deriving Show


instance FromJSON (Maybe FacultyTable) where
  parseJSON = runMaybeT . ( 
      parseTab "faculty-info" >=>
      parseArticles >=>
      msum . map parseTables >=>
      lookupByTitle "Program Faculty" >=>
      parseColumns >=>
      parseFacultyTable
    )


parseFacultyTable :: [Value] -> MaybeT Parser FacultyTable
parseFacultyTable cs = do
    (Object p)    <- lookupByTitle "Physician" cs
    (String ftp):(String ptp):_ <- lift $ p .: "values"
    (Object np)   <- lookupByTitle "Non-physician" cs
    (String ftnp):(String ptnp):_ <- lift $ np .: "values"
    
    return $ FacultyTable (readText ftp) (readText ftnp) (readText ptp) (readText ptnp)

readText = readMaybe . T.unpack



data ProgramFaculty = ProgramFaculty {
    fPercentageFemalePaid :: Text
  , fRatioFullTime        :: Text
  } deriving Show

instance FromJSON (Maybe ProgramFaculty) where
  parseJSON = runMaybeT . (
      parseTab "faculty-info" >=>
      parseArticles >=>
      (\as -> ProgramFaculty
        <$> lookupValueInArticles "Percentage of full-time paid female physician faculty" as
        <*> lookupValueInArticles "Ratio of full-time equivalent paid faculty to positions" as
      )
    )


      
      
prefix = "Faculty & Trainees - Program Faculty - "

facultyFields 
  :: Maybe FacultyTable
  -> Maybe ProgramFaculty
  -> [(ByteString, ByteString)]
facultyFields facultyTable programFaculty = prefixWith prefix [
    "Full-time paid - Physician"     .= showJustJust fFullTimePhysician facultyTable
  , "Full-time paid - Non-physician" .= showJustJust fFullTimeNonPhysician facultyTable
  , "Part-time paid - Physician"     .= showJustJust fPartTimePhysician facultyTable
  , "Part-time paid - Non-physician" .= showJustJust fPartTimeNonPhysician facultyTable
  , "Percentage of full-time paid female physician faculty"   .= just fPercentageFemalePaid programFaculty
  , "Ratio of full-time equivalent paid faculty to positions" .= just fRatioFullTime programFaculty
  ]


