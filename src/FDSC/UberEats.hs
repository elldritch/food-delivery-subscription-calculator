module FDSC.UberEats (getOrdersSince, Order (..), SessionID(..)) where

import Data.Aeson (FromJSON, Value)
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Network.HTTP.Req (MonadHttp, NoReqBody (..), POST (..), ReqBodyJson (..), header, https, jsonResponse, req, responseBody, (/:), runReq, defaultHttpConfig, JsonResponse, HttpBody)
import Relude

data Order = Order {}
  deriving (Show)

-- data UberEatsResponse = UberEatsResponse {}
--   deriving (Generic)

type UberEatsResponse = Value

-- instance FromJSON UberEatsResponse

newtype SessionID = SessionID Text

newtype OrderID = OrderID Text

-- getOrdersSince takes a session ID cookie and a timestamp, and returns all
-- orders that were created after that timestamp.
getOrdersSince :: SessionID -> UTCTime -> IO [Order]
getOrdersSince sid since = runReq defaultHttpConfig $ getOrdersSince' sid since

getOrdersSince' :: (MonadHttp m) => SessionID -> UTCTime -> m [Order]
getOrdersSince' auth since = fst <$> getOrdersPage auth Nothing

-- getOrdersPage takes a session ID cookie and possibly the last order ID from a
-- previous page and returns the next page of orders, and another order ID if
-- there exist remaining pages.
getOrdersPage :: forall m. (MonadHttp m) => SessionID -> Maybe OrderID -> m ([Order], Maybe OrderID)
getOrdersPage (SessionID auth) lastOrderID =
  parsePage . responseBody
    <$> ( case lastOrderID of
            Just (OrderID lastOrderUUID) -> reqWithBody $ ReqBodyJson (Map.singleton @Text "lastWorkflowUUID" lastOrderUUID)
            Nothing -> reqWithBody NoReqBody
        )
  where
    headers = header "x-csrf-token" "x" <> header "cookie" ("sid=" <> encodeUtf8 auth)

    reqWithBody :: (HttpBody body) => body -> m (JsonResponse UberEatsResponse)
    reqWithBody body = req POST (https "www.ubereats.com" /: "api" /: "getPastOrdersV1") body jsonResponse headers

    parsePage :: UberEatsResponse -> ([Order], Maybe OrderID)
    parsePage res = traceShow res ([], Nothing)
