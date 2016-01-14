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
    iWebsite            :: Text
  , iLengthOfTraining   :: Int
  , iRequiredLength     :: Int
  , iAccepting20162017  :: Text
  , iAccepting20172018  :: Text
  , iProgramStarts      :: Text
  , iEras               :: Text
  , iAffiliatedUsGov    :: Text
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
    <$> lookupValue           "Web Address" is
    <*> lookupParsedIntValue  "Accredited length of training" is
    <*> lookupParsedIntValue  "Required length" is
    <*> lookupValue           "Accepting applications for training that begins in 2016-2017" is
    <*> lookupValue           "Will be accepting applications for training that begins in 2017-2018" is
    <*> lookupValue           "Program start dates" is
    <*> lookupValue           "Participates in ERAS" is
    <*> lookupValue           "Affiliated with US government" is
parseInfo o = lift $ typeMismatch "Info" o


prefix = "Basic Information - Information - " :: ByteString

infoFields 
  :: Maybe Info
  -> [(ByteString, ByteString)]
infoFields info = prefixWith prefix [
    "Web Address"                                                           .= just iWebsite info
  , "Accredited length of training"                                         .= showJust iLengthOfTraining info
  , "Required length"                                                       .= showJust iRequiredLength info
  , "Accepting applications for training that begins in 2016-2017"          .= just iAccepting20162017 info
  , "Will be accepting applications for training that begins in 2017-2018"  .= just iAccepting20172018 info
  , "Program start dates"                                                   .= just iProgramStarts info
  , "Participates in ERAS"                                                  .= just iEras info
  , "Affiliated with US government"                                         .= just iAffiliatedUsGov info
  ]

