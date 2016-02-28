{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Control.Applicative ((<$>), (<*>))
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
  toNamedRecord, (.=))


import Types.Info
import Types.GeneralInfo
import Types.Faculty
import Types.Schedule
import Types.EdEnv
import Types.PoliciesBenefits
import Types.Comp
import Types.Util

data Program = Program {
    pid               :: Text
  , pTitle            :: Text
  , pInfo             :: Maybe Info
  , pFacultyTable     :: Maybe FacultyTable
  , pCharacteristicsTable :: Maybe CharacteristicsTable
  , pProgramFaculty   :: Maybe ProgramFaculty
  , pWorkSchedule     :: Maybe WorkSchedule
  , pCallSchedules    :: Maybe CallScheduleTable
  , pEducationalEnvironment     :: Maybe EducationalEnvironment
  , pGeneralInfo      :: Maybe GeneralInfo
  , pPoliciesBenefits :: Maybe PoliciesBenefits
  {- , pCompensationAndLeaveTable  :: Maybe CompensationAndLeaveTable -}
  } deriving Show

instance FromJSON Program where
  parseJSON obj@(Object o)   = Program
    <$> o .: "id"
    <*> o .: "title"
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
    <*> parseJSON obj
  parseJSON o = typeMismatch "Program" o


instance ToNamedRecord Program 
  where
    toNamedRecord Program{pid, pTitle, pInfo, pFacultyTable, pCharacteristicsTable, pProgramFaculty, pWorkSchedule, 
                          pCallSchedules, pEducationalEnvironment, pGeneralInfo, pPoliciesBenefits} = namedRecord $ [
        "Program Id"               .= pid
      , "Program Title"            .= pTitle]
      ++ infoFields pInfo
      ++ facultyFields pFacultyTable pCharacteristicsTable pProgramFaculty
      ++ scheduleFields pWorkSchedule pCallSchedules
      ++ edEnvFields pEducationalEnvironment
      ++ generalInfoFields pGeneralInfo
      ++ policiesBenefitsFields pPoliciesBenefits
      ++ [
      {- , "Employment Policies & Benefits - Compensation and leave - Salary compensation - Grad year 2" .= showJustJust (fmap clSalaryCompensation .lookup 2) pCompensationAndLeave -}
      {- , "Employment Policies & Benefits - Compensation and leave - Vacation days - Grad year 2"       .= showJustJust (fmap clVacationsDays .lookup 2) pCompensationAndLeave -}
      {- , "Employment Policies & Benefits - Compensation and leave - Sick days - Grad year 2"           .= showJustJust (fmap clSickDays .lookup 2) pCompensationAndLeave -}
      ]




