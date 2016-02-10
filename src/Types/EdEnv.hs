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

data ResidentEvaluation = ResidentEvaluation {
    reYearlySpecialtyRequired :: Text
  , rePatientSurveys          :: Text
  , rePortfolioSystem         :: Text
  , re360Evaluations          :: Text
  , reObjectiveExaminations   :: Text
  } deriving Show

data EducationalFeatures = EducationalFeatures {
    efAdditionalTraining  :: Text
  , efPrimaryCare         :: Text
  , efRuralTrack          :: Text
  , efWomensTrack         :: Text
  , efHospitalistTrack    :: Text
  , efResearchFellowship  :: Text
  , efAnotherTrack        :: Text
  } deriving Show

data EducationalBenefits = EducationalBenefits {
    ebPhysicianImpairment  :: Text
  , ebIntegrativeMedicine         :: Text
  , ebDebtManagement          :: Text
  , ebFormalProgramSkills         :: Text
  , ebFormalMentoring    :: Text
  , ebFormalProgramTeamwork  :: Text
  , ebContinuousTraining        :: Text
  , ebInternationalExperience        :: Text
  , ebResidentRetreats        :: Text
  , ebOffCampus        :: Text
  , ebHospice        :: Text
  , ebCulturalAwareness        :: Text
  , ebInstructionInSpanish        :: Text
  , ebAlternativeCurriculum        :: Text
  , ebEconomicsCurriculum        :: Text
  , ebMPH        :: Text
  , ebResearchRotation        :: Text
  } deriving Show




data EducationalEnvironment = EducationalEnvironment {
    eeAvgScheduledLecturesFirstYear :: Text
  , eeTrainingFirstYear             :: Text
  , eeTrainingNonHospitalFirstYear  :: Text
  , eeProgramEvaluation             :: ProgramEvaluation
  , eeResidentEvaluation            :: ResidentEvaluation
  , eeEducationalFeatures           :: EducationalFeatures
  , eeEducationalBenefits           :: EducationalBenefits
  } deriving Show
instance FromJSON (Maybe EducationalEnvironment) where
  parseJSON = runMaybeT . ( 
      parseTab "educational-environment" >=>
      parseArticles >=>
      (\as -> EducationalEnvironment
        <$> lookupValueInArticles "Avg hours/week of regularly scheduled lectures/conferences during 1st year" as
        <*> lookupValueInArticles "Training at hospital outpatient clinics during 1st year" as
        <*> lookupValueInArticles "Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics" as
        <*> (parseProgramEvaluation   =<< parseSubTable =<< lookupByTitle "Program evaluation" as)
        <*> (parseResidentEvaluation  =<< parseSubTable =<< lookupByTitle "Resident evaluation" as)
        <*> (parseEducationalFeatures =<< parseSubTable =<< lookupByTitle "Educational features" as)
        <*> (parseEducationalBenefits =<< parseSubTable =<< lookupByTitle "Educational benefits" as)
      )
    )

parseSubTable :: Value -> MaybeT Parser [Value]
parseSubTable (Object pe) = lift $ pe .: "items"

parseProgramEvaluation :: [Value] -> MaybeT Parser ProgramEvaluation
parseProgramEvaluation is =
  ProgramEvaluation
    <$> lookupValue "Program graduation rates" is
    <*> lookupValue "Resident assessment of curriculum" is
    <*> lookupValue "In-training examination scores" is
    <*> lookupValue "Performance-based assessment scores (eg, OSCE)" is

parseResidentEvaluation :: [Value] -> MaybeT Parser ResidentEvaluation
parseResidentEvaluation is =
  ResidentEvaluation
    <$> lookupValue "Yearly specialty in-service examination required" is
    <*> lookupValue "Patient surveys" is
    <*> lookupValue "Portfolio system" is
    <*> lookupValue "360 degree evaluations" is
    <*> lookupValue "Objective structured clinical examinations (OSCE)" is

parseEducationalFeatures :: [Value] -> MaybeT Parser EducationalFeatures
parseEducationalFeatures is =
  EducationalFeatures
    <$> lookupValue "Offers additional training or educational experience beyond accredited length" is
    <*> lookupValue "Offers a primary care track" is
    <*> lookupValue "Offers a rural track" is
    <*> lookupValue "Offers a women's health track" is
    <*> lookupValue "Offers a hospitalist track" is
    <*> lookupValue "Offers a research track/nonaccredited fellowship" is
    <*> lookupValue "Offers another track" is

parseEducationalBenefits :: [Value] -> MaybeT Parser EducationalBenefits
parseEducationalBenefits is =
  EducationalBenefits
    <$> lookupValue "Physician impairment prevention curriculum" is
    <*> lookupValue "Integrative medicine curriculum" is
    <*> lookupValue "Debt management/financial counseling" is
    <*> lookupValue "Formal program to develop teaching skills" is
    <*> lookupValue "Formal mentoring program" is
    <*> lookupValue "Formal program to foster interdisciplinary teamwork" is
    <*> lookupValue "Continuous quality improvement training" is
    <*> lookupValue "International experience" is
    <*> lookupValue "Resident/fellow retreats" is
    <*> lookupValue "Off-campus electives" is
    <*> lookupValue "Hospice/home care experience" is
    <*> lookupValue "Cultural competence awareness" is
    <*> lookupValue "Instruction in medical Spanish or other non-English language" is
    <*> lookupValue "Alternative/complementary medicine curriculum" is
    <*> lookupValue "Economics of health care systems curriculum" is
    <*> lookupValue "MPH/MBA or PhD training" is
    <*> lookupValue "Research rotation" is



prefix = "Educational environment - "             :: ByteString
eePrefix = prefix <> "Educational Environment - " :: ByteString
pePrefix = prefix <> "Program evaluation - "      :: ByteString
rePrefix = prefix <> "Resident evaluation - "     :: ByteString
efPrefix = prefix <> "Educational features - "    :: ByteString
ebPrefix = prefix <> "Educational benefits - "    :: ByteString


edEnvFields 
  :: Maybe EducationalEnvironment
  -> [(ByteString, ByteString)]
edEnvFields edEnv = 
  let
    pe = eeProgramEvaluation    <$> edEnv
    re = eeResidentEvaluation   <$> edEnv
    ef = eeEducationalFeatures  <$> edEnv
    eb = eeEducationalBenefits  <$> edEnv
  in
    prefixWith eePrefix [
      "Avg hours/week of regularly scheduled lectures/conferences during 1st year"                                              .= just eeAvgScheduledLecturesFirstYear edEnv
    , "Training at hospital outpatient clinics during 1st year"                                                                 .= just eeTrainingFirstYear edEnv
    , "Training during 1st year in ambulatory non-hospital community-based settings, eg, physician offices, community clinics"  .= just eeTrainingNonHospitalFirstYear edEnv
    ] ++ 
    prefixWith pePrefix [
      "Program graduation rates"                          .= just peProgramGradRates pe
    , "Resident assessment of curriculum"                 .= just peResidentAssessment pe
    , "In-training examination scores"                    .= just peInTrainingScores pe
    , "Performance-based assessment scores (eg, OSCE)"    .= just pePerfAssessmentScores pe
    ] ++ 
    prefixWith rePrefix [
      "Yearly specialty in-service examination required"  .= just reYearlySpecialtyRequired re
    , "Patient surveys"                                   .= just rePatientSurveys re
    , "Portfolio system"                                  .= just rePortfolioSystem re
    , "360 degree evaluations"                            .= just re360Evaluations re
    , "Objective structured clinical examinations (OSCE)" .= just reObjectiveExaminations re
    ] ++
    prefixWith efPrefix [
      "Offers additional training or educational experience beyond accredited length"  .= just efAdditionalTraining ef
    , "Offers a primary care track"                       .= just efPrimaryCare ef
    , "Offers a rural track"                              .= just efRuralTrack ef
    , "Offers a women's health track"                     .= just efWomensTrack ef
    , "Offers a hospitalist track"                        .= just efHospitalistTrack ef
    , "Offers a research track/nonaccredited fellowship"  .= just efResearchFellowship ef
    , "Offers another track"                              .= just efAnotherTrack ef
    ] ++
    prefixWith ebPrefix [
      "Physician impairment prevention curriculum"                    .= just ebPhysicianImpairment eb
    , "Integrative medicine curriculum"                               .= just ebIntegrativeMedicine eb
    , "Debt management/financial counseling"                          .= just ebDebtManagement eb
    , "Formal program to develop teaching skills"                     .= just ebFormalProgramSkills eb
    , "Formal mentoring program"                                      .= just ebFormalMentoring eb
    , "Formal program to foster interdisciplinary teamwork"           .= just ebFormalProgramTeamwork eb
    , "Continuous quality improvement training"                       .= just ebContinuousTraining eb
    , "International experience"                                      .= just ebInternationalExperience eb
    , "Resident/fellow retreats"                                      .= just ebResidentRetreats eb
    , "Off-campus electives"                                          .= just ebOffCampus eb
    , "Hospice/home care experience"                                  .= just ebHospice eb
    , "Cultural competence awareness"                                 .= just ebCulturalAwareness eb
    , "Instruction in medical Spanish or other non-English language"  .= just ebInstructionInSpanish eb
    , "Alternative/complementary medicine curriculum"                 .= just ebAlternativeCurriculum eb
    , "Economics of health care systems curriculum"                   .= just ebEconomicsCurriculum eb
    , "MPH/MBA or PhD training"                                       .= just ebMPH eb
    , "Research rotation"                                             .= just ebResearchRotation eb
    ]


