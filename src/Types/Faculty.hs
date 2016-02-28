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


instance {-# OVERLAPPING #-} FromJSON (Maybe FacultyTable) where
  parseJSON = runMaybeT . ( 
      parseTab "faculty-info" >=>
      parseArticles >=>
      fmap (concat . catMaybes) . mapM parseTables >=>
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




data CharacteristicsTable = CharacteristicsTable {
    cUSMD   :: Maybe Double
  , cIMG    :: Maybe Double
  , cDO     :: Maybe Double 
  , cFemale :: Maybe Double
  , cMale   :: Maybe Double
  } deriving Show


instance {-# OVERLAPPING #-} FromJSON (Maybe CharacteristicsTable) where
  parseJSON = runMaybeT . ( 
      parseTab "faculty-info" >=>
      parseArticles >=>
      fmap (concat . catMaybes) . mapM parseTables >=>
      lookupByTitle "Characteristics of trainees" >=>
      parseColumns >=>
      parseCharacteristicsTable
    )


parseCharacteristicsTable :: [Value] -> MaybeT Parser CharacteristicsTable
parseCharacteristicsTable cs = do
    (Object usmdc)    <- lookupByTitle "% USMD" cs
    (String usmdv):_  <- lift $ usmdc .: "values"
    (Object imgc)     <- lookupByTitle "% IMG" cs
    (String imgv):_   <- lift $ imgc .: "values"
    (Object doc)      <- lookupByTitle "% DO" cs
    (String dov):_    <- lift $ doc .: "values"
    (Object femalec)  <- lookupByTitle "% Female" cs
    (String femalev):_  <- lift $ femalec .: "values"
    (Object malec)      <- lookupByTitle "% Male" cs
    (String malev):_    <- lift $ malec .: "values"
    
    return $ CharacteristicsTable (readText usmdv) (readText imgv) (readText dov) (readText femalev) (readText malev)


readText :: Read a => Text -> Maybe a
readText = readMaybe . T.unpack




data ProgramFaculty = ProgramFaculty {
    fPercentageFemalePaid :: Text
  , fRatioFullTime        :: Text
  } deriving Show

instance {-# OVERLAPPING #-} FromJSON (Maybe ProgramFaculty) where
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
  -> Maybe CharacteristicsTable
  -> Maybe ProgramFaculty
  -> [(ByteString, ByteString)]
facultyFields facultyTable characteristicsTable programFaculty = prefixWith prefix [
    "Full-time paid - Physician"     .= showJustJust fFullTimePhysician facultyTable
  , "Full-time paid - Non-physician" .= showJustJust fFullTimeNonPhysician facultyTable
  , "Part-time paid - Physician"     .= showJustJust fPartTimePhysician facultyTable
  , "Part-time paid - Non-physician" .= showJustJust fPartTimeNonPhysician facultyTable
  , "% USMD"    .= showJustJust cUSMD characteristicsTable
  , "% IMG"     .= showJustJust cIMG characteristicsTable
  , "% DO"      .= showJustJust cDO characteristicsTable
  , "% Female"  .= showJustJust cFemale characteristicsTable
  , "% Male"    .= showJustJust cMale characteristicsTable
  , "Percentage of full-time paid female physician faculty"   .= just fPercentageFemalePaid programFaculty
  , "Ratio of full-time equivalent paid faculty to positions" .= just fRatioFullTime programFaculty
  ]

