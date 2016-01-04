{-# LANGUAGE OverloadedStrings #-}

module Types.Comp where

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (join)
import GHC.Generics
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe)
import Data.CSV.Conduit.Conversion  (ToNamedRecord (..), namedRecord,
  toNamedRecord, (.=))
import Data.Traversable (sequenceA)

import Types.Util


{- type CompensationAndLeaveTable = [(Int, CompensationAndLeave)] -}

{- data CompensationAndLeave = CompensationAndLeave { -}
    {- clSalaryCompensation  :: Int -}
  {- , clVacationsDays       :: Int -}
  {- , clSickDays            :: Int -}
  {- } -}

{- instance FromJSON CompensationAndLeaveTable where -}

