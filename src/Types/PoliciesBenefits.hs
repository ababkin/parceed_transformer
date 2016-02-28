{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.PoliciesBenefits where

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

data MajorMedicalBenefits = MajorMedicalBenefits {
    mmbInsuranceResidence   :: Text
  , mmbInsuranceDependents  :: Text
  , mmbInsuranceDomestic    :: Text
  , mmbOutpatient           :: Text
  , mmbInpatient            :: Text
  , mmbGroupLifeInsurance   :: Text
  , mmbDentalInsurance      :: Text
  , mmbDisabilityInsurance  :: Text
  , mmbDisabilityInsuranceHIV :: Text
  , mmbCoverageBegins       :: Text
  } deriving Show




data PoliciesBenefits = PoliciesBenefits {
    pbMajorMedicalBenefits    :: MajorMedicalBenefits
  } deriving Show
instance {-# OVERLAPPING #-} FromJSON (Maybe PoliciesBenefits) where
  parseJSON = runMaybeT . ( 
      parseTab "educational-environment" >=>
      parseArticles >=>
      (\as -> PoliciesBenefits
        <$> (parseMajorMedicalBenefits  =<< parseSubTable =<< lookupByTitle "Major medical benefits" as)
      )
    )

parseSubTable :: Value -> MaybeT Parser [Value]
parseSubTable (Object pe) = lift $ pe .: "items"

parseMajorMedicalBenefits :: [Value] -> MaybeT Parser MajorMedicalBenefits
parseMajorMedicalBenefits is = 
  MajorMedicalBenefits
    <$> lookupValue "Major medical insurance for residents" is
    <*> lookupValue "Major medical insurance for dependents" is
    <*> lookupValue "Major medical insurance for domestic partners" is
    <*> lookupValue "Outpatient mental health insurance" is
    <*> lookupValue "Inpatient mental health insurance" is
    <*> lookupValue "Group life insurance" is
    <*> lookupValue "Dental insurance" is
    <*> lookupValue "Disability insurance" is
    <*> lookupValue "Disability insurance for occupationally-acquired HIV" is
    <*> lookupValue "Medical insurance coverage begins" is


prefix = "Policies and Benefits - "             :: ByteString
mmbPrefix = prefix <> "Major medical benefits - " :: ByteString


policiesBenefitsFields 
  :: Maybe PoliciesBenefits
  -> [(ByteString, ByteString)]
policiesBenefitsFields pb = 
  let
    mmb = pbMajorMedicalBenefits  <$> pb
  in
    prefixWith mmbPrefix [
      "Major medical insurance for residents"         .= just mmbInsuranceResidence mmb
    , "Major medical insurance for dependents"        .= just mmbInsuranceDependents mmb
    , "Major medical insurance for domestic partners" .= just mmbInsuranceDomestic mmb
    , "Outpatient mental health insurance"            .= just mmbOutpatient mmb
    , "Inpatient mental health insurance"             .= just mmbInpatient mmb
    , "Group life insurance"                          .= just mmbGroupLifeInsurance mmb
    , "Dental insurance"                              .= just mmbDentalInsurance mmb
    , "Disability insurance"                          .= just mmbDisabilityInsurance mmb
    , "Disability insurance for occupationally-acquired HIV"  .= just mmbDisabilityInsuranceHIV mmb
    , "Medical insurance coverage begins"             .= just mmbCoverageBegins mmb
    ]


