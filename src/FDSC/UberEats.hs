module FDSC.UberEats (getOrdersSince, Order (..), SessionID (..)) where

import Data.Aeson (FromJSON (..), Object, Value, withObject, (.:))
import Data.Aeson.Types (Parser)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Network.HTTP.Req (HttpBody, JsonResponse, MonadHttp, NoReqBody (..), POST (..), ReqBodyJson (..), defaultHttpConfig, header, https, jsonResponse, req, responseBody, runReq, (/:))
import Relude

data Order = Order
  { orderID :: OrderID,
    store :: Store,
    fare :: Fare
  }
  deriving (Show)

data Store = Store
  { title :: Text
  }
  deriving (Show)

data Fare = Fare
  { total :: Double
  }
  deriving (Show)

data UberEatsAPIResponse = UberEatsAPIResponse
  { orders :: [Order],
    lastOrderID :: OrderID,
    hasMore :: Bool
  }
  deriving (Show)

instance FromJSON UberEatsAPIResponse where
  parseJSON = withObject "UberEats API response" $ \v -> do
    status :: Text <- v .: "status"
    unless (status == "success") $ case status of
      "failure" -> do
        d <- v .: "data"
        m <- d .: "message"
        fail m
      _ -> fail "unknown UberEats API response status"
    d <- v .: "data"

    ordersMap <- d .: "ordersMap"
    let pairs = HashMap.toList ordersMap
    orders <- mapM parseOrder pairs

    orderUUIDs <- d .: "orderUuids"
    let lastOrderID = OrderID $ last orderUUIDs

    meta <- d .: "meta"
    hasMore <- meta .: "hasMore"

    return UberEatsAPIResponse {orders, lastOrderID, hasMore}
    where
      parseOrder :: (Text, Value) -> Parser Order
      parseOrder (orderID, obj) = withObject "UberEats order" (parseOrder' orderID) obj

      parseOrder' :: Text -> Object -> Parser Order
      parseOrder' orderID v = do
        store <- v .: "storeInfo"
        storeTitle <- store .: "title"

        fare <- v .: "fareInfo"
        fareTotal <- fare .: "totalPrice"

        return
          Order
            { orderID = OrderID orderID,
              store = Store {title = storeTitle},
              fare = Fare {total = fareTotal}
            }

newtype SessionID = SessionID Text

newtype OrderID = OrderID Text
  deriving (Eq, Ord, Show)

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

    reqWithBody :: (HttpBody body) => body -> m (JsonResponse UberEatsAPIResponse)
    reqWithBody body = req POST (https "www.ubereats.com" /: "api" /: "getPastOrdersV1") body jsonResponse headers

    parsePage :: UberEatsAPIResponse -> ([Order], Maybe OrderID)
    parsePage res = traceShow res ([], Nothing)
