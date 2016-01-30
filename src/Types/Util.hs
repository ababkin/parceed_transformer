{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Types.Util where

import Control.Monad (msum)
import Control.Applicative (pure, (<$>), (<*>))
import Data.Text (Text)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON),
  Value (..), object, (.:), encode, decode, (.:?))
import Data.Aeson.Types (typeMismatch, Parser, Object)
import Data.Maybe (catMaybes, listToMaybe, fromMaybe, listToMaybe)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Control.Error.Util (hoistMaybe)
import Control.Monad.Trans.Class (lift)
import Text.Read (readMaybe)

lookupValue 
  :: FromJSON a 
  => Text 
  -> [Value] 
  -> MaybeT Parser a
lookupValue k os = do
  mrs <- lift $ mapM (\(Object o) -> o .:? k) os
  case catMaybes mrs of
    [r] -> MaybeT $ return r
    []  -> fail $ "Could not find value for key: " ++ T.unpack k
    _   -> fail $ "Found multiple key value pairs in the list of objects for key: " ++ T.unpack k

lookupParsedIntValue 
  :: Text 
  -> [Value] 
  -> MaybeT Parser Int
lookupParsedIntValue k os = read <$> lookupValue k os

lookupParsedDoubleValue 
  :: Text 
  -> [Value] 
  -> MaybeT Parser Double
lookupParsedDoubleValue k os = read <$> lookupValue k os

lookupParsedBoolValue 
  :: Text 
  -> [Value] 
  -> MaybeT Parser Bool
lookupParsedBoolValue k os = toBool <$> lookupValue k os

toBool :: Text -> Bool
toBool "Yes"  = True
toBool "No"   = False



showJustJust a = maybe "" (maybe "" show . a)
showJust a = maybe "" (show . a)
just = maybe ("" :: Text)
joinJust f = maybe "" (fromMaybe "" . f) 


prefixWith prefix = map (\(x,y) -> (prefix <> x, y))


parseTab
  :: Text
  -> Value
  -> MaybeT Parser Value
parseTab tab (Object o) =
  MaybeT $ o .:? tab
parseTab _ o = lift $ typeMismatch "Tab" o


parseArticles
  :: Value
  -> MaybeT Parser [Value]
parseArticles (Object o) =
  MaybeT $ o .:? "articles"
parseArticles o = lift $ typeMismatch "Articles" o


parseTables
  :: Value
  -> MaybeT Parser (Maybe [Value])
parseTables (Object o) =
  MaybeT ( Just <$> o .:? "tables" )
parseTables o = lift $ typeMismatch "Tables" o


parseColumns
  :: Value
  -> MaybeT Parser [Value]
parseColumns (Object o) =
  MaybeT $ o .:? "columns"
parseColumns o = lift $ typeMismatch "Columns" o


lookupByTitle 
  :: Text 
  -> [Value]
  -> MaybeT Parser Value
lookupByTitle title is =
     msum (map finder is)
  where
    finder obj@(Object o) =
      (\(String t) -> if t == title then return obj else hoistMaybe Nothing) =<< MaybeT (o .:? "title")
    finder o = fail $ "undexpected non-object type: " ++ show o


lookupValueInArticles 
  :: Text 
  -> [Value] 
  -> MaybeT Parser Text
lookupValueInArticles key = 
  msum . map (lookupValueInArticle key)

  where
    lookupValueInArticle key (Object o) =
      lookupValue key =<< MaybeT (o .:? "items")
