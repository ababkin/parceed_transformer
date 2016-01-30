{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Types.GeneralInfo where

import Control.Monad.Trans.Class (lift)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (join, (>=>), msum, join, sequence, mzero, MonadPlus, mplus)
import Control.Arrow
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
  toNamedRecord, (.=))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)

import Types.Util

type TotalProgramSizeTable = [(Int, Int)]

data USMLEReqs = USMLEReqs {
    u1Required :: Text
  , u1MinScore :: Text
  , u2Required :: Text
  , u2MinScore :: Text
  } deriving Show

data IMGs = IMGs {
    isCert          :: Text
  , isCitizenship   :: Text
  , isResident      :: Text
  , isJ1            :: Text
  , isH1B           :: Text
  , isF1            :: Text
  , isUnrestricted  :: Text
  } deriving Show

data GeneralInfo = GeneralInfo {
    giSize      :: TotalProgramSizeTable
  , giSteps     :: USMLEReqs
  , giAvgStep1  :: Text
  , giLevels    :: USMLEReqs
  , giAvgLevel1 :: Text
  , giIMGs      :: IMGs
  , giPrimarySite       :: Text
  , giProgramDescribed  :: Text
  , giReqPrevGME  :: Text
  , giOffersPos  :: Text
  , giParticipatesMainMatch  :: Text
  , giParticipatesAdvanced  :: Text
  , giParticipatesSF  :: Text
  , giParticipatesAnother  :: Text
  , giInterviewsLast  :: Text
  , giReqLetters  :: Text
  , giLatestDate20162017  :: Text
  , giEarliestDate20172018  :: Text
  , giLatestDate20172018  :: Text
  } deriving Show


instance FromJSON (Maybe GeneralInfo) where
  parseJSON = runMaybeT . (
      parseTab "general-info" >=> 
      parseArticles >=>
      (\as -> do
        ts <- concat . catMaybes <$> mapM parseTables as
        GeneralInfo
          <$> (parseTotalProgramSizeTable =<< parseColumns =<< lookupByTitle "Total program size" ts)
          <*> (parseUSMLESteps =<< parseColumns =<< lookupByTitle "USMLE Step 1 and Step 2 requirements for interview consideration" ts)
          <*> (getFirstVal =<< lookupByTitle "Average Step 1 score (range) of current residents/fellows" =<< fmap concat (mapM parseColumns ts) ) 
          <*> (parseUSMLESteps =<< parseColumns =<< lookupByTitle "COMLEX Level 1 and 2 requirements for interview consideration (DOs only)" ts)
          <*> (getFirstVal =<< lookupByTitle "Average Level 1 score (range) of current residents/fellows" =<< fmap concat (mapM parseColumns ts) ) 
          <*> parseIMGs as
          <*> lookupValueInArticles "Primary teaching site" as
          <*> lookupValueInArticles "Program best described as" as
          <*> lookupValueInArticles "Requires previous GME" as
          <*> lookupValueInArticles "Offers preliminary positions" as
          <*> lookupValueInArticles "Participates in the Main Match of the National Resident Matching Program (NRMP)" as
          <*> lookupValueInArticles "Participates in the Advanced or Fellowship Match of the National Resident Matching Program (NRMP)" as
          <*> lookupValueInArticles "Participant in San Francisco match" as
          <*> lookupValueInArticles "Participant in another matching program" as
          <*> lookupValueInArticles "Interviews conducted last year for first year positions" as
          <*> lookupValueInArticles "Required letters of recommendation" as
          <*> lookupValueInArticles "Latest date for applications for 2016-2017" as
          <*> lookupValueInArticles "Earliest date for applications for 2017-2018" as
          <*> lookupValueInArticles "Latest date for applications for 2017-2018" as
      )
    )


{- parseTotalProgramSizeTable :: [Value] -> MaybeT Parser TotalProgramSizeTable -}
parseTotalProgramSizeTable cs = do
    (Object yo)   <- lookupByTitle "Year" cs
    ys            <- lift $ yo .: "values"
    
    (Object tpso)   <- lookupByTitle "Positions" cs
    tpss            <- lift $ tpso .: "values"

    return (mapTuple readText <$> zip ys tpss)
  where
    mapTuple = join (***)
    readText = read . T.unpack


parseUSMLESteps :: [Value] -> MaybeT Parser USMLEReqs
parseUSMLESteps cs = do
  [s1r, s1m, s2r, s2m] <- mapM getFirstVal cs
  return $ USMLEReqs s1r s1m s2r s2m

parseIMGs :: [Value] -> MaybeT Parser IMGs
parseIMGs as = do
  (Object o) <- lookupByTitle "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information." as
  is <- lift $ o .: "items"
  IMGs 
    <$> lookupValue "Current ECFMG certification" is
    <*> lookupValue "US citizenship" is
    <*> lookupValue "US permanent resident" is
    <*> lookupValue "J-1 visa" is
    <*> lookupValue "H1-B visa" is
    <*> lookupValue "F-1 visa" is
    <*> lookupValue "Unrestricted state medical license for this state" is

getFirstVal :: Value -> MaybeT Parser Text
getFirstVal (Object o) = head <$> lift (o .: "values")


prefix = "General Information - " :: ByteString

generalInfoFields 
  :: Maybe GeneralInfo
  -> [(ByteString, ByteString)]
generalInfoFields gi = 
  let
    usmleStepReqs = giSteps <$> gi
    usmleLevelReqs = giLevels <$> gi
    imgs = giIMGs <$> gi
  in
    prefixWith prefix [
      "Total program size - Year 1"  .= showJustJust (lookup 1 . giSize) gi
    , "Total program size - Year 2"  .= showJustJust (lookup 2 . giSize) gi
    , "Total program size - Year 3"  .= showJustJust (lookup 3 . giSize) gi
    , "Total program size - Year 4"  .= showJustJust (lookup 4 . giSize) gi

    , "USMLE Step 1 and Step 2 requirements for interview consideration - Step 1 Required"       .= just u1Required usmleStepReqs
    , "USMLE Step 1 and Step 2 requirements for interview consideration - Step 1 Minimum Score"  .= just u1MinScore usmleStepReqs
    , "USMLE Step 1 and Step 2 requirements for interview consideration - Step 2 Required"       .= just u2Required usmleStepReqs
    , "USMLE Step 1 and Step 2 requirements for interview consideration - Step 2 Minimum Score"  .= just u2MinScore usmleStepReqs
    , "Average Step 1 score (range) of current residents/fellows"  .= just giAvgStep1 gi

    , "COMLEX Level 1 and 2 requirements for interview consideration (DOs only) - Level 1 Required"       .= just u1Required usmleLevelReqs
    , "COMLEX Level 1 and 2 requirements for interview consideration (DOs only) - Level 1 Minimum Score"  .= just u1MinScore usmleLevelReqs
    , "COMLEX Level 1 and 2 requirements for interview consideration (DOs only) - Level 2 Required"       .= just u2Required usmleLevelReqs
    , "COMLEX Level 1 and 2 requirements for interview consideration (DOs only) - Level 2 Minimum Score"  .= just u2MinScore usmleLevelReqs
    , "Average Level 1 score (range) of current residents/fellows"  .= just giAvgLevel1 gi


    , "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information. - Current ECFMG certification"  .= just isCert imgs
    , "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information. - US citizenship"               .= just isCitizenship imgs
    , "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information. - US permanent resident"        .= just isResident imgs
    , "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information. - J-1 visa"                     .= just isJ1 imgs
    , "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information. - H1-B visa"                    .= just isH1B imgs
    , "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information. - F-1 visa"                     .= just isF1 imgs
    , "IMGs should have, among other qualifications, one or more of the following.  Contact the program for additional information. - Unrestricted state medical license for this state"  .= just isUnrestricted imgs

    , "Primary teaching site"       .= just giPrimarySite gi
    , "Program best described as"   .= just giProgramDescribed gi
    , "Requires previous GME"   .= just giReqPrevGME gi
    , "Offers preliminary positions"   .= just giOffersPos gi
    , "Participates in the Main Match of the National Resident Matching Program (NRMP)"   .= just giParticipatesMainMatch gi
    , "Participates in the Advanced or Fellowship Match of the National Resident Matching Program (NRMP)"   .= just giParticipatesAdvanced gi
    , "Participant in San Francisco match"   .= just giParticipatesSF gi
    , "Participant in another matching program"   .= just giParticipatesAnother gi
    , "Interviews conducted last year for first year positions"   .= just giInterviewsLast gi
    , "Required letters of recommendation"   .= just giReqLetters gi
    , "Latest date for applications for 2016-2017"   .= just giLatestDate20162017 gi
    , "Earliest date for applications for 2017-2018"   .= just giEarliestDate20172018 gi
    , "Latest date for applications for 2017-2018"   .= just giLatestDate20172018 gi
    ]

