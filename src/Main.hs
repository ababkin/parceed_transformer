{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}



import Database.MongoDB
import Data.Aeson.Bson (toAeson)

import Control.Applicative ((<$>))
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans (liftIO, lift)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Aeson (fromJSON, Result(Success, Error), Value(Object))
import System.Environment (getArgs)
import Data.Conduit ((=$), ($$), Source, Sink, yield)
import Data.Conduit.Binary (sourceFile, sinkFile)
import qualified Data.Conduit.List            as CL
import Data.CSV.Conduit
import Data.Maybe (fromJust)
import Data.CSV.Conduit.Conversion (toNamedRecord)
import Data.Conduit.Lift (readerC)


import Types


main :: IO ()
main = do
  pipe <- connect (Host "127.0.0.1" $ PortNumber 4321)
  e <- runResourceT . access pipe master "parceed" $ do
    cursor <- find $ select [] "items"
    fromMongo cursor $$ toCSV "result.csv"

  close pipe

  where
    fromMongo 
      :: (MonadIO m, MonadBaseControl IO m) 
      => Cursor 
      -> Source (Action m) Program
    fromMongo cursor = do
      maybeDocument <- lift $ next cursor
      case maybeDocument of
        Just doc -> do
          case fromJSON . Object . toAeson . exclude ["_id"] $ doc of
            Success p -> yield p
            Error err -> fail err
          fromMongo cursor
        Nothing -> return ()

    toCSV 
      :: MonadResource m 
      => String 
      -> Sink Program (Action m) ()
    toCSV csvFilename = readerC . const $
          CL.map toNamedRecord
      =$  (writeHeaders defCSVSettings >> fromCSV defCSVSettings)
      =$  sinkFile csvFilename




