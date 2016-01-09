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

data USMLESteps = USMLESteps {
    usStep1Required :: Text
  , usStep1MinScore :: Text
  , usStep2Required :: Text
  , usStep2MinScore :: Text
  } deriving Show

data GeneralInfo = GeneralInfo {
    giSize      :: TotalProgramSizeTable
  , giUSMLEReqs :: USMLESteps
  } deriving Show


instance FromJSON (Maybe GeneralInfo) where
  parseJSON = runMaybeT . (
      parseTab "general-info" >=> 
      parseArticles >=>
      fmap (concat . catMaybes) . mapM parseTables >=>
      (\ts ->
        GeneralInfo
          <$> (parseTotalProgramSizeTable =<< parseColumns =<< lookupByTitle "Total program size" ts)
          <*> (parseUSMLESteps =<< parseColumns =<< lookupByTitle "USMLE Step 1 and Step 2 requirements for interview consideration" ts)
      )
    )



parseTotalProgramSizeTable :: [Value] -> MaybeT Parser TotalProgramSizeTable
parseTotalProgramSizeTable cs = do
    (Object yo)   <- lookupByTitle "Year" cs
    ys            <- lift $ yo .: "values"
    
    (Object tpso)   <- lookupByTitle "Positions" cs
    tpss            <- lift $ tpso .: "values"

    return (mapTuple readText <$> zip ys tpss)
  where
    mapTuple = join (***)
    readText = read . T.unpack


parseUSMLESteps :: [Value] -> MaybeT Parser USMLESteps
parseUSMLESteps cs = do
  [s1r, s1m, s2r, s2m] <- lift $ mapM getFirstVal cs
  return $ USMLESteps s1r s1m s2r s2m

  where
    getFirstVal (Object o) = head <$> o .: "values"


prefix = "General Information - Total program size - " :: ByteString

generalInfoFields 
  :: Maybe GeneralInfo
  -> [(ByteString, ByteString)]
generalInfoFields gi = 
  let
    usmlReqs = giUSMLEReqs <$> gi
  in
    prefixWith prefix [
      "Year 1"  .= showJustJust (lookup 1 . giSize) gi
    , "Year 2"  .= showJustJust (lookup 2 . giSize) gi
    , "Year 3"  .= showJustJust (lookup 3 . giSize) gi
    , "Year 4"  .= showJustJust (lookup 4 . giSize) gi

    , "Step 1 Required"       .= just usStep1Required usmlReqs
    , "Step 1 Minimum Score"  .= just usStep1MinScore usmlReqs
    , "Step 2 Required"       .= just usStep2Required usmlReqs
    , "Step 2 Minimum Score"  .= just usStep2MinScore usmlReqs
    ]


