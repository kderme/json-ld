{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module Data.JsonLD.Expand where

import Control.Monad.Extra
import Data.Aeson
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types
import Data.Foldable
import Data.JsonLD.Context
import Data.JsonLD.Util
import Data.Maybe
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)
import qualified Data.Map as Map

expand :: Context -> Maybe Text -> Value -> Parser ([Value], Context)
expand ctx mproperty element = case element of
  Null -> pure ([Null], ctx)
  Array ls -> (, ctx) <$> expandArray ctx mproperty ls
  Object o -> do
    mExpandedProp <- mExpandIri ctx False True False mproperty
    let ctx' = getCtxmKeyTypeProcess mproperty "@context" ctx
    ctx'' <- case KM.lookup "@context" o of
      Nothing -> pure ctx'
      Just elemCtx -> pure $ processCtx ctx $ valueToContext elemCtx
    let sortedKV = List.sortOn fst $ toTextList o
    (ctx''', typeKey) <- foldM (expandType ctx'') (ctx'', Nothing) sortedKV
    expandedKeyMap <- expandObject ctx''' mproperty mExpandedProp sortedKV typeKey ctx''
    expandedKeyMap' <- handleExpandedObject expandedKeyMap
    pure ([mapToValue expandedKeyMap'], ctx)
  _scalar -> do
      v <- expandValue ctx mproperty element
      pure ([v], ctx)
    -- TODO: check for 
  where
    expandType cctx (actx, typeKey) (k,v) = do
      expanded <- expandIri actx False True False k
      if expanded == Just "@type" then do
        let vals = toArrayText v -- returns Nothing for values of keys which are not String or Array
        actx' <- foldM (processTypeCtx cctx) actx vals
        pure (actx', Just $ fromMaybe k typeKey)
      else pure (actx, typeKey)
    processTypeCtx cctx actx mtp = pure $
      case getCtxmKeyType mtp "@context" cctx of
        Nothing -> actx
        Just actx' -> processCtx actx actx'

    handleExpandedObject :: Map Text Value -> Parser (Map Text Value)
    handleExpandedObject mp
      | Just valueValue <- Map.lookup "@value" mp = handlevalueObject mp valueValue
      | Just typeValue <- Map.lookup "@type" mp = pure $ Map.update (Just . toArrayValue) "@type" mp
      | Map.member "@list" mp || Map.member "@set" mp, Map.size mp > 1, Map.size mp /= 2 || Map.notMember "@index" mp =
          fail "InvalidJsonLDSyntax: An element with @list or @set can have at most one property and that is @index"
      -- TODO: replace @set with array
      | Map.member "@language" mp, Map.size mp == 1 = pure Map.empty
      | otherwise = pure mp

    -- json-ld specs: 9.5 [Value Objects]
    handlevalueObject :: Map Text Value -> Value -> Parser (Map Text Value)
    handlevalueObject mp valueValue
      | Map.member "@type" mp, Map.member "@language" mp || Map.member "@direction" mp =
          fail "InvalidJsonLDSyntax: an element containing @value cannot contain both @type and @langauge or @direction"
      | not (all (`elem`  valueCompatWords) (Map.keys mp)) =
          fail $ Text.unpack $ Text.unwords $
           "InvalidJsonLDSyntax: an element containing @value can only contain" : valueCompatWords
      | Just ["@json"] <- mtypes = pure mp
      | valueValue == Null = pure Map.empty -- TODO: Ward for null @value which is dropped.
      | not (all (\v -> isString v || isEmptyObject v) (toArray valueValue)) && Map.member "@language" mp =
          fail "InvalidJsonLDSyntax: The value of @value has to be a string when @language is present"
      | Just types <- mtypes, not $
          all
          (\tv -> isWithText (\txt -> isAbsoluteIRI txt && not (Text.isPrefixOf "_:" txt) ) tv || isEmptyObject tv)
          types = fail "InvalidJsonLDSyntax: The value of @type must be an absolute IRI in elements that contain @value."
      | otherwise = pure mp
      where
        valueCompatWords :: [Text]
        valueCompatWords = ["@type", "@index", "@language", "@direction"]

        mtypes = toArray <$> Map.lookup "@type" mp


-- Cannot be Null, Array, Object
expandValue :: Context -> Maybe Text -> Value -> Parser Value
expandValue ctx mproperty element = do
  mExpandedProp <- mExpandIri ctx True False False mproperty
  if
    | mExpandedProp == Just "@id" -> mTextToValue <$> expandIriScalar ctx True False False element
    | mExpandedProp == Just "@type" -> mTextToValue <$> expandIriScalar ctx False True False element
    | Just expandedProp <- mExpandedProp, isKeyword expandedProp -> pure element
    | otherwise -> do
        let mtp = getCtxmKeyTypeTxt mproperty "@type" ctx -- ##getContextValue
        if
          | mtp `elem` ["@id", "@graph"], valuetxt <- valueToText element-> do
            mexpandedValue <- expandIriScalar ctx True False False element
            case mexpandedValue of
              Nothing ->  -- TODO: Warn reserved @id value
                pure $ toJSON $ Map.fromList [("@id" :: Text, Null)]
              Just expandedValue ->
                pure $ toJSON $ Map.fromList [("@id" :: Text, String expandedValue)]
          | mtp == "@vocab", valuetxt <- valueToText element -> do
            mexpandedValue <- expandIriScalar ctx True True False element
            pure $ toJSON [("@id" :: Text, mTextToValue mexpandedValue)]
          | mtp `notElem` ["@id", "@vocab", "@none"] -> do
            pure $ toJSON [("@type" :: Text, String mtp), ("@value", element)]
          | otherwise -> do
              let ls = [("@value", element)]
              let lang = getCtxmKeyTypeTxt mproperty "@language" ctx
              let dir = getCtxmKeyTypeTxt mproperty "@direction" ctx
              let ls' = addValueMaybe "@language" (Just $ String lang) ls
              let ls'' = addValueMaybe "@direction" (Just $ String dir) ls
              pure $ toJSON ls''
  where
    mTextToValue = \case
      Nothing -> Null
      Just txt -> String txt

expandObject ::  Context -> Maybe Text -> Maybe Text -> [(Text, Value)] -> Maybe Text -> Context -> Parser (Map Text Value)
expandObject context mproperty mExpandedProp sortedKV mtypeKey typeCtx = do
  isJson <- findIsJson
  (ctx', mp) <- foldM (expandKV isJson) (context, Map.empty) sortedKV
  undefined
  where
    findIsJson
      | Just typeKey <- mtypeKey, Just (Just txt : _) <- toArrayText <$> List.lookup typeKey sortedKV = do
          expandedTypeValue <- expandIri context False True True txt
          pure $ expandedTypeValue == Just "@json"
      | otherwise = pure False

    expandKV :: Bool -> (Context, Map Text Value) -> (Text, Value) -> Parser (Context, Map Text Value)
    expandKV _ v ("@context", _) = pure v
    expandKV _isJson (ctx, acc) (k,v) = do
      mexpandedKey <- expandIri ctx False True False k
      go mexpandedKey
      where
        go Nothing = skip -- warning non expanded property
        go (Just expandedKey)
          | not (isAbsoluteIRI expandedKey || isKeyword expandedKey) = skip -- warning non expanded property
          | isKeyword expandedKey, mExpandedProp == Just "@reverse" =
                fail "InvalidJsonLDSyntax: @reverse cannot be used as a property"
          | isKeyword expandedKey, Map.member expandedKey acc, expandedKey /= "@included", expandedKey /= "@type" =
                fail "InvalidJsonLDSyntax: coliding keywords"
          | expandedKey == "@id" = if
            | Nothing <- valueToText v -> fail "InvalidJsonLDSyntax: @id value must be a string" -- TODO: support framing
            | Just stringV <- valueToText v -> do
                mExpandedIdVal <- expandIri ctx True False False stringV
                if
                  | Nothing <- mExpandedIdVal -> fail "null @id value found."
                  | Just expandedIdVal <- mExpandedIdVal, not (isAbsoluteIRI expandedKey)->
                      fail "@id not expanded"
                  | Just expandedIdVal <- mExpandedIdVal -> pure (ctx, Map.insert "@id" (toArrayValue $ String expandedIdVal) acc)
          | expandedKey == "@type" = if
            | Just typeList <- getValidTypeValue v -> do
                acc' <- foldM updateWithTypeValue acc typeList
                pure (ctx, acc')

            | otherwise -> skip
          | expandedKey == "@graph", not (isObject v || isArray v) = fail "InvalidJsonLDSyntax: @graph value must be object or array."
          | expandedKey == "@value" = pure (ctx, Map.insert "@value" v acc) -- TODO: check for isJson
          | expandedKey == "@language", Nothing <- valueToText "@language" = fail "InvalidJsonLDSyntax: @language value must be a string"
          | expandedKey == "@language", Just langTxt <- valueToText "@language", isLanuageText langTxt =
              pure (ctx, Map.insert "@language" (toArrayValue $ String langTxt) acc)
          | expandedKey == "@language" = skip --warn that @language value must be a valid BCP47
          | expandedKey == "@index", isString v = pure (ctx, Map.insert "@index" v acc)
          | expandedKey == "@index" = fail "InvalidJsonLDSyntax: @index value must be string"
          | expandedKey `elem` ["@included", "@direcion", "@reverse", "@nest"] = fail $ "Unimplemented error: " <> Text.unpack expandedKey
          | otherwise = do
              let termContext = processCtx ctx (getCtxKeyType k "@context" ctx)
              let isList = expandedKey == "@list"
              let newProperty = if isList || mExpandedProp == Just "@graph" then Nothing else mproperty
              (expandedvals, ctx') <- if
                | isList || expandedKey == "@set" -> do
                  expand termContext newProperty v
                | False -> pure ([toJSON $ Map.fromList [("@type" :: Text, String "@json"), ("@value", v)]], ctx)
                | otherwise -> expand termContext mproperty v
              pure (ctx', Map.insert expandedKey (fromValueList expandedvals) acc)

        skip = pure (ctx, acc)

        updateWithTypeValue mp typeValStr = do
          mexpandedTyperVal <- expandIri typeCtx True True True typeValStr
          if
            | Just expandedTyperVal <- mexpandedTyperVal , expandedTyperVal == "@json" || isAbsoluteIRI expandedTyperVal ->
                pure $ Map.insert "@type" (toArrayValue $ String expandedTyperVal) mp
            | otherwise -> pure mp -- warn about wrong @type value

-- | TODO: Check is inside @container @list
-- TODO: does ctx change?
expandArray :: Context -> Maybe Text -> Array -> Parser [Value]
expandArray ctx mproperty arr = concat <$> mapM (fmap fst . expand ctx mproperty) (toList arr) -- TODO: Handle new 'expand' context

expandIriScalar ::  Context -> Bool -> Bool -> Bool -> Value -> Parser (Maybe Text)
expandIriScalar ctx isBase isVocab typeExpansion = \case
  String txt -> expandIri ctx isBase isVocab typeExpansion txt
  _ -> fail "Unimplemented error: Expand non String"

-- | TODO: Possibly we need an additional base.
expandIri :: Context -> Bool -> Bool -> Bool -> Text -> Parser (Maybe Text)
expandIri ctx isBase isVocab typeExpansion property
  | isKeyword property = pure $ Just property
  | isKeywordLike property = pure Nothing --ignore property
  -- Create a term when used for context creation
  | isVocab, isNothing mpropertyMapInCtx = pure Nothing
  | isVocab, Just propMap <- mpropertyMapInCtx, isObject propMap = pure Nothing
  | isVocab, Just propMap <- mpropertyMapInCtx, Just vl' <- objectKey (fromText "@id") propMap = pure $ valueToText vl'
  | Just (pref, suff) <- colonSeparatedProp, pref == "_" || Text.isPrefixOf "//" suff = pure $ Just property
  | Just (pref, suff) <- colonSeparatedProp, False = pure Nothing -- when used for context creation define new property. alterContext
  | Just (pref, suff) <- colonSeparatedProp, Just prefixId <- getCtx pref ctx = pure $ Just $ prefixId <> suff -- TODO: should we add '/' here?
  | Just (pref, suff) <- colonSeparatedProp, isAbsoluteIRI property = pure $ Just property
  | isVocab, Just vocabValue <- getCtx "@vocab" ctx = pure $ Just $ vocabValue <> property -- TODO: should we add '/' here?
  | isBase, Just _ <- getCtx property ctx = fail "Unimplemented error: @base context key not supported yet." -- TODO: prepend base value.
  | isBase = pure $ Just property -- prepend a default base here.
  | otherwise = pure Nothing -- Nothing or propert?
  where
    colonSeparatedProp = case Text.break (== ':') property of
      (pref, colsuff) | Just (':', suff) <- Text.uncons colsuff  -> Just (pref,suff)
      _ -> Nothing

    propertyKey = fromText property
    mpropertyMapInCtx = getCtxMapping property ctx

-- | TODO: Possibly we need an additional base.
mExpandIri :: Context -> Bool -> Bool -> Bool -> Maybe Text -> Parser (Maybe Text)
mExpandIri ctx isBase isVocab typeExpansion = \case
  Just property -> expandIri ctx isBase isVocab typeExpansion property
  Nothing -> pure Nothing
