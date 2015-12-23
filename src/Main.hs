{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad.IO.Class (liftIO)
import           Control.Applicative        ((<$>))
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (decode)
import System.Environment (getArgs)
import           Control.Monad.Trans.Resource (MonadResource, runResourceT)
import           Data.Conduit
import           Data.Conduit.Binary          (sourceFile, sinkFile)
import qualified Data.Conduit.List            as CL
import           Data.CSV.Conduit
import Data.Maybe (fromJust)
import           Data.CSV.Conduit.Conversion  (toNamedRecord)

import Types


main :: IO ()
main = do
  [jsonFilename, csvFilename] <- getArgs
  runResourceT (sourceFile jsonFilename =$ CL.map (fromJust . decode . BL.fromStrict) $$ toCSV csvFilename)

  where
    toCSV :: MonadResource m => String -> Sink Program m ()
    toCSV csvFilename =
          CL.map toNamedRecord
      =$  (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
      =$  sinkFile csvFilename



