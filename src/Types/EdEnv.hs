{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.EdEnv where

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


data EducationalEnvironment = EducationalEnvironment {
    eeAvgScheduledLecturesFirstYear :: Text
  , eeTrainingFirstYear             :: Text
  , eeTrainingNonHospitalFirstYear  :: Text
  } deriving Show
instance FromJSON (Maybe EducationalEnvironment) where
  parseJSON = runMaybeT . ( 
      parseTab "educational-environment" >=>
      parseArticles >=>
      (\as -> EducationalEnvironment
        <$> lookupValueInArticles "Avg hours/week of regularly scheduled lectures/conferences during 1st year" as
        <*> lookupValueInArticles "Training at hospital outpatient clinics during 1st year" as
        <*> lookupValueInArticles "Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics" as
      )
    )

prefix = "Educational environment - Educational Environment - "

edEnvFields 
  :: Maybe EducationalEnvironment
  -> [(ByteString, ByteString)]
edEnvFields edEnv = prefixWith prefix [
    "Avg hours/week of regularly scheduled lectures/conferences during 1st year"                                              .= just eeAvgScheduledLecturesFirstYear edEnv
  , "Training at hospital outpatient clinics during 1st year"                                                                 .= just eeTrainingFirstYear edEnv
  , "Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics"  .= just eeTrainingNonHospitalFirstYear edEnv
  ]
  
