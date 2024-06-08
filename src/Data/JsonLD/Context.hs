module Data.JsonLD.Context where

import Data.Aeson
import Data.JsonLD.Util
import Data.Text (Text)

data Context = Context

processCtx :: Context -> Context -> Context
processCtx currentCtx newCtx = undefined

valueToContext :: Value -> Context
valueToContext = undefined

getCtxMapping :: Text -> Context -> Maybe Value
getCtxMapping = undefined

getCtx :: Text -> Context -> Maybe Text
getCtx = undefined

getCtxmKeyTypeProcess :: Maybe Text -> Text -> Context -> Context
getCtxmKeyTypeProcess mkey tp ctx = case mkey of
  Nothing -> ctx
  Just k -> processCtx ctx (getCtxKeyType k tp ctx)

getCtxmKeyTypeTxt :: Maybe Text -> Text -> Context -> Text
getCtxmKeyTypeTxt = undefined

getCtxmKeyType :: Maybe Text -> Text -> Context -> Maybe Context
getCtxmKeyType mkey tp ctx = case mkey of
  Nothing -> Nothing
  Just k -> Just $ getCtxKeyType k tp ctx

-- | get context value
getCtxKeyType :: Text -> Text -> Context -> Context
getCtxKeyType _key _type _ = undefined
