module FDSC.UberEats (getOrdersSince, Order (..), SessionID (..), render) where

import Data.Aeson (FromJSON (..), Value, withArray, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Foldable (foldrM)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Time (UTCTime)
import Network.HTTP.Req
  ( HttpBody,
    JsonResponse,
    MonadHttp,
    NoReqBody (..),
    POST (..),
    ReqBodyJson (..),
    defaultHttpConfig,
    header,
    https,
    jsonResponse,
    req,
    responseBody,
    runReq,
    (/:),
  )
import Relude

data Order = Order
  { orderID :: OrderID,
    created :: UTCTime,
    status :: OrderStatus,
    items :: [Item],
    store :: Store,
    fare :: Fare
  }
  deriving (Show)

data OrderStatus
  = InProgress
  | Completed UTCTime
  | Cancelled UTCTime
  deriving (Show)

data Item = Item
  { itemID :: Text,
    title :: Text,
    price :: Double,
    quantity :: Int,
    customizations :: [Customization]
  }
  deriving (Show)

data Customization = Customization
  { customizationID :: Text,
    price :: Double,
    quantity :: Int,
    categoryTitle :: Text,
    selectionTitle :: Text
  }
  deriving (Show)

data Store = Store
  { title :: Text,
    address :: Text
  }
  deriving (Show)

data Fare = Fare
  { total :: Double,
    breakdown :: Map Text FareItem
  }
  deriving (Show)

data FareItem = FareItem
  { label :: Text,
    fareType :: Text,
    value :: Double
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
      "failure" -> v .: "data" >>= (.: "message") >>= fail
      _ -> fail "unknown UberEats API response status"
    d <- v .: "data"

    ordersMap :: HashMap Text Value <- d .: "ordersMap"
    orders <- HashMap.elems <$> traverse parseOrder ordersMap

    orderUUIDs <- d .: "orderUuids"
    let lastOrderID = OrderID $ last orderUUIDs

    meta <- d .: "meta"
    hasMore <- meta .: "hasMore"

    pure UberEatsAPIResponse {orders, lastOrderID, hasMore}
    where
      parseOrder :: Value -> Parser Order
      parseOrder = withObject "UberEats order" $ \v -> do
        baseOrder <- v .: "baseEaterOrder"
        orderID <- baseOrder .: "uuid"
        items <- baseOrder .: "shoppingCart" >>= (.: "items") >>= parseItems
        (created, status) <- baseOrder .: "orderStateChanges" >>= parseStatus

        store <- v .: "storeInfo"
        storeTitle <- store .: "title"
        address <- store .: "location" >>= parseAddress

        fare <- v .: "fareInfo"
        fareTotal <- fare .: "totalPrice"
        breakdown <- fare .: "checkoutInfo" >>= parseCheckoutInfo

        pure
          Order
            { orderID = OrderID orderID,
              created,
              status,
              items,
              store = Store {title = storeTitle, address},
              fare = Fare {total = fareTotal, breakdown}
            }

      parseStatus :: Value -> Parser (UTCTime, OrderStatus)
      parseStatus = withArray "order state changes" $ \xs -> do
        (created, status) <- foldrM (flip parseFoldState) (Nothing, InProgress) xs
        case created of
          Just createTime -> pure (createTime, status)
          Nothing -> fail "invalid order state: never created"
        where
          parseFoldState ::
            (Maybe UTCTime, OrderStatus) ->
            Value ->
            Parser (Maybe UTCTime, OrderStatus)
          parseFoldState prev@(created, status) =
            withObject "order state change" $ \v -> do
              changeType :: Text <- v .: "type"
              time <- v .: "stateChangeTime"
              case changeType of
                "CREATED" -> case created of
                  Just _ -> fail "invalid order state: order created twice"
                  Nothing -> pure (Just time, status)
                "OFFERED" -> pure prev
                "ASSIGNED" -> pure prev
                "COMPLETED" -> case status of
                  InProgress -> pure (created, Completed time)
                  Completed _ ->
                    fail "invalid order state: order completed twice"
                  Cancelled _ ->
                    fail "invalid order state: order completed after cancel"
                _ -> fail "invalid order state: unknown change type"

      parseItems :: Value -> Parser [Item]
      parseItems = withArray "shopping cart" $ traverse parseItem . toList
        where
          parseItem :: Value -> Parser Item
          parseItem = withObject "shopping cart item" $ \i -> do
            itemID <- i .: "uuid"
            title <- i .: "title"
            price <- i .: "price"
            quantity <- i .: "quantity"
            customizations <- i .: "customizations" >>= parseCustomizations
            pure Item {itemID, title, price, quantity, customizations}

          parseCustomizations :: Value -> Parser [Customization]
          parseCustomizations =
            withArray "customizations" $
              (concat <$>) . traverse parseCustomization . toList
            where
              parseCustomization :: Value -> Parser [Customization]
              parseCustomization = withObject "customization" $ \v -> do
                categoryTitle <- v .: "title"
                v .: "childOptions"
                  >>= (.: "options")
                  >>= parseOptions categoryTitle

              parseOptions :: Text -> Value -> Parser [Customization]
              parseOptions categoryTitle =
                withArray "options" $
                  traverse (parseOption categoryTitle) . toList

              parseOption :: Text -> Value -> Parser Customization
              parseOption categoryTitle = withObject "option" $ \v -> do
                price <- v .: "price"
                quantity <- v .: "quantity"
                selectionTitle <- v .: "title"
                customizationID <- v .: "uuid"
                pure
                  Customization
                    { customizationID,
                      price,
                      quantity,
                      categoryTitle,
                      selectionTitle
                    }

      parseAddress :: Value -> Parser Text
      parseAddress = withObject "store location" $ \location -> do
        address <- location .: "address"
        address1 <- address .: "address1"
        aptOrSuite <- address .:? "aptOrSuite"
        city <- address .: "city"
        region <- address .: "region"
        postalCode <- address .: "postalCode"
        pure
          ( address1
              <> " "
              <> maybe "" (" " <>) aptOrSuite
              <> ", "
              <> city
              <> ", "
              <> region
              <> " "
              <> postalCode
          )

      parseCheckoutInfo :: Value -> Parser (Map Text FareItem)
      parseCheckoutInfo =
        withArray "fare" $
          (Map.fromList <$>) . traverse parseFareItem . toList
        where
          parseFareItem :: Value -> Parser (Text, FareItem)
          parseFareItem = withObject "fare item" $ \v -> do
            key <- v .: "key"
            fareType <- v .: "type"
            label <- v .: "label"
            value <- v .: "rawValue"
            pure (key, FareItem {fareType, label, value})

newtype SessionID = SessionID Text

newtype OrderID = OrderID Text
  deriving (Eq, Ord, Show)

-- getOrdersSince takes a session ID cookie and a timestamp, and returns all
-- orders that were created after that timestamp.
getOrdersSince :: SessionID -> UTCTime -> IO [Order]
getOrdersSince sid since = runReq defaultHttpConfig $ getOrdersSince' sid since

-- TODO: implement pagination.
getOrdersSince' :: (MonadHttp m) => SessionID -> UTCTime -> m [Order]
getOrdersSince' sid since = fst <$> getOrdersPage sid Nothing

-- getOrdersPage takes a session ID cookie and possibly the last order ID from a
-- previous page and returns the next page of orders, and another order ID if
-- there exist remaining pages.
getOrdersPage ::
  forall m.
  (MonadHttp m) =>
  SessionID ->
  Maybe OrderID ->
  m ([Order], Maybe OrderID)
getOrdersPage (SessionID sid) lastOrderID = parsePage . responseBody <$> request
  where
    headers =
      header "x-csrf-token" "x" <> header "cookie" ("sid=" <> encodeUtf8 sid)

    reqWithBody ::
      (HttpBody body) => body -> m (JsonResponse UberEatsAPIResponse)
    reqWithBody body =
      req
        POST
        (https "www.ubereats.com" /: "api" /: "getPastOrdersV1")
        body
        jsonResponse
        headers

    request :: m (JsonResponse UberEatsAPIResponse)
    request = case lastOrderID of
      Just (OrderID lastOrderUUID) ->
        reqWithBody $
          ReqBodyJson $ Map.singleton @Text "lastWorkflowUUID" lastOrderUUID
      Nothing -> reqWithBody NoReqBody

    parsePage :: UberEatsAPIResponse -> ([Order], Maybe OrderID)
    parsePage UberEatsAPIResponse {orders, lastOrderID = l, hasMore} =
      (orders, if hasMore then Just l else Nothing)

data Hypotheticals = Hypotheticals
  { original :: Order,
    noCouponNoSub :: Fare,
    couponNoSub :: Fare,
    noCouponSub :: Fare,
    couponSub :: Fare
  }
  deriving (Show)

render :: Order -> String
render order = show order

alternativePrices :: Order -> Hypotheticals
alternativePrices original = undefined
