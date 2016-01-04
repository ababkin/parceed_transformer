{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Types.Info where

import Control.Monad.Trans.Class (lift)
import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (join, (>=>))
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

data Info = Info {
    iLengthOfTraining   :: Int
  , iRequiredLength     :: Int
  , iAccepting20162017  :: Bool
  , iAccepting20172018  :: Bool
  , iProgramStarts      :: Text
  , iEras               :: Bool
  , iAffiliatedUsGov    :: Bool
  } deriving Show

instance FromJSON (Maybe Info) where
  parseJSON = runMaybeT . (
      parseTab "basic-info" >=> 
      parseArticles >=>
      lookupByTitle "Information" >=>
      parseInfo
    )
 

parseInfo :: Value -> MaybeT Parser Info
parseInfo (Object info) = do
  is <- lift $ info .: "items"
  Info
    <$> lookupParsedIntValue   "Accredited length of training" is
    <*> lookupParsedIntValue   "Required length" is
    <*> lookupParsedBoolValue  "Accepting applications for training that begins in 2016-2017" is
    <*> lookupParsedBoolValue  "Will be accepting applications for training that begins in 2017-2018" is
    <*> lookupValue            "Program start dates" is
    <*> lookupParsedBoolValue  "Participates in ERAS" is
    <*> lookupParsedBoolValue  "Affiliated with US government" is
parseInfo o = lift $ typeMismatch "Info" o


prefix = "Basic Information - Information - " :: ByteString

infoFields 
  :: Maybe Info
  -> [(ByteString, ByteString)]
infoFields info = prefixWith prefix [
    "Accredited length of training"                                         .= showJust iLengthOfTraining info
  , "Required length"                                                       .= showJust iRequiredLength info
  , "Accepting applications for training that begins in 2016-2017"          .= showJust iAccepting20162017 info
  , "Will be accepting applications for training that begins in 2017-2018"  .= showJust iAccepting20172018 info
  , "Program start dates"                                                   .= just iProgramStarts info
  , "Participates in ERAS"                                                  .= showJust iEras info
  , "Affiliated with US government"                                         .= showJust iAffiliatedUsGov info
  ]

