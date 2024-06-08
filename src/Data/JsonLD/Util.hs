{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}

module Data.JsonLD.Util where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Maybe

isArray, isObject, isString, isEmptyObject, isNull, notNull :: Value -> Bool
isArray = \case
  Array _ -> True
  _ -> False

isObject = \case
  Object _ -> True
  _ -> False

isString = \case
  String _ -> True
  _ -> False

isEmptyObject = \case
  Object o -> KM.size o == 0
  _ -> False

isNull = \case
  Null -> True
  _ -> False

notNull = not . isNull

withObject :: Value -> (Object -> a) -> Maybe a
withObject v f = case v of
  Object o -> Just $ f o
  _ -> Nothing

isWithText :: (Text -> Bool) -> Value -> Bool
isWithText f = \case
  String txt -> f txt
  _ -> False

objectKey :: Key -> Value -> Maybe Value
objectKey k = \case
  Object o -> KM.lookup k o
  _ -> Nothing

toArrayText :: Value -> [Maybe Text]
toArrayText = \case
  String txt -> [Just txt]
  Array mp -> valueToText <$> toList mp
  _ -> []

valueToText :: Value -> Maybe Text
valueToText = \case
  String txt -> Just txt
  _ -> Nothing

toTextList :: KM.KeyMap Value -> [(Text, Value)]
toTextList km = first toText <$> KM.toList km

toArrayValue :: Value -> Value
toArrayValue v = case v of
  Array ls -> Array ls
  _ -> Array $ V.fromList [v]

-- | Should unwrap if it's alrady a list?
fromValueList :: [Value] -> Value
fromValueList = Array . V.fromList

toArray :: Value -> [Value]
toArray v = case v of
  Array ls -> toList ls
  _ -> [v]

mapToValue :: Map Text Value -> Value
mapToValue = Object . KM.fromMap . Map.mapKeys fromText

addValueMaybe :: Text -> Maybe Value -> [(Text, Value)] -> [(Text, Value)]
addValueMaybe k mv ls = case mv of
  Nothing -> ls
  Just v -> (k, v) : ls

isKeyword :: Text -> Bool
isKeyword k = k `elem` keywords

keywords :: [Text]
keywords =
  [ "@base"
  , "@container"
  , "@context"
  , "@default"
  , "@direction"
  , "@embed"
  , "@explicit"
  , "@graph"
  , "@id"
  , "@included"
  , "@index"
  , "@json"
  , "@language"
  , "@list"
  , "@nest"
  , "@none"
  , "@omitDefault"
  , "@prefix"
  , "@preserve"
  , "@protected"
  , "@requireAll"
  , "@reverse"
  , "@set"
  , "@type"
  , "@value"
  , "@version"
  , "@vocab"
  ]

-- | TODO: This is wrong. Use regex ^@[a-zA-Z]+$
isKeywordLike :: Text -> Bool
isKeywordLike txt = case Text.uncons txt of
  Just ('@', _) -> True
  _ -> False

-- | TODO: fix. Use regex.
isAbsoluteIRI :: Text -> Bool
isAbsoluteIRI _ = False

getValidTypeValue :: Value -> Maybe [Text]
getValidTypeValue = \case
  String txt -> Just [txt]
  Array vs | all isString vs -> Just $ mapMaybe valueToText (V.toList vs)
  _ -> Nothing

-- | TODO: Fix. Use BCP47 regex
isLanuageText :: Text -> Bool
isLanuageText _ = True
