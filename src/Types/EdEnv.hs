{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.EdEnv where

import Data.Monoid ((<>))
import Control.Monad.Trans.Class (lift)
import Control.Monad (join, (>=>), msum)
import Control.Applicative (pure, (<$>), (<*>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
  toNamedRecord, (.=))
import Data.Traversable (sequenceA)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

import Types.Util

data ProgramEvaluation = ProgramEvaluation {
    peProgramGradRates      :: Text
  , peResidentAssessment    :: Text
  , peInTrainingScores      :: Text
  , pePerfAssessmentScores  :: Text
  } deriving Show

data EducationalEnvironment = EducationalEnvironment {
    eeAvgScheduledLecturesFirstYear :: Text
  , eeTrainingFirstYear             :: Text
  , eeTrainingNonHospitalFirstYear  :: Text
  , eeProgramEvaluation :: ProgramEvaluation
  } deriving Show
instance FromJSON (Maybe EducationalEnvironment) where
  parseJSON = runMaybeT . ( 
      parseTab "educational-environment" >=>
      parseArticles >=>
      (\as -> EducationalEnvironment
        <$> lookupValueInArticles "Avg hours/week of regularly scheduled lectures/conferences during 1st year" as
        <*> lookupValueInArticles "Training at hospital outpatient clinics during 1st year" as
        <*> lookupValueInArticles "Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics" as
        <*> (parseProgramEvaluation =<< lookupByTitle "Program evaluation" as)
      )
    )


parseProgramEvaluation :: Value -> MaybeT Parser ProgramEvaluation
parseProgramEvaluation (Object pe)   = do
    is <- lift $ pe .: "items"
    ProgramEvaluation
      <$> lookupValue "Program graduation rates" is
      <*> lookupValue "Resident assessment of curriculum" is
      <*> lookupValue "In-training examination scores" is
      <*> lookupValue "Performance-based assessment scores (eg, OSCE)" is

prefix = "Educational environment - " :: ByteString
eePrefix = prefix <> "Educational Environment - " :: ByteString
pePrefix = prefix <> "Program evaluation - " :: ByteString


edEnvFields 
  :: Maybe EducationalEnvironment
  -> [(ByteString, ByteString)]
edEnvFields edEnv = 
  let
    pe = eeProgramEvaluation <$> edEnv
  in
    prefixWith eePrefix [
    "Avg hours/week of regularly scheduled lectures/conferences during 1st year"                                              .= just eeAvgScheduledLecturesFirstYear edEnv
  , "Training at hospital outpatient clinics during 1st year"                                                                 .= just eeTrainingFirstYear edEnv
  , "Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics"  .= just eeTrainingNonHospitalFirstYear edEnv

  ] ++ prefixWith pePrefix [
    "Program graduation rates"                        .= just peProgramGradRates pe
  , "Resident assessment of curriculum"               .= just peResidentAssessment pe
  , "In-training examination scores"                  .= just peInTrainingScores pe
  , "Performance-based assessment scores (eg, OSCE)"  .= just pePerfAssessmentScores pe
  ]
  
