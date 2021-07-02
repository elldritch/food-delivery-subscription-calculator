module FDSC.UberEats (getOrdersSince, Order (..), SessionID (..), render) where

import Data.Aeson (FromJSON (..), Value, withArray, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Foldable (foldrM)
import Data.HashMap.Strict qualified as HashMap
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (UTCTime)
import FDSC.App (MonadApp, log)
import Network.HTTP.Req
  ( HttpBody,
    JsonResponse,
    MonadHttp,
    NoReqBody (..),
    POST (..),
    ReqBodyJson (..),
    header,
    https,
    jsonResponse,
    req,
    responseBody,
    (/:),
  )
import Relude
import Text.Printf (printf)

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

newtype Fare = Fare (Map Text FareItem)
  deriving (Show)

fareKeysBase :: [Text]
fareKeysBase =
  [ fareKeyBaseSubtotal,
    "eats_fare.tip",
    "eats.tax.base"
  ]

fareKeyBaseSubtotal :: Text
fareKeyBaseSubtotal = "eats_fare.subtotal"

fareKeysFees :: [Text]
fareKeysFees =
  [ fareKeyFeeDelivery,
    "eats.mp.charges.bag_fee",
    "eats.mp.charges.basket_dependent_fee",
    "eats.mp.charges.mpf_cap_dependent_fee"
  ]

fareKeyFeeDelivery :: Text
fareKeyFeeDelivery = "eats.mp.charges.booking_fee"

fareKeysPromos :: [Text]
fareKeysPromos =
  [ "eats.discounts.promotion",
    "eats.discounts.store_promotion",
    "eats.item_discount.promotion_total",
    "eats.mp.discounts.bdf_discount_percentage",
    "eats.mp.discounts.item_level_uber_funded",
    "eats.mp.discounts.no_rush_delivery_discount",
    "eats.mp.discounts.uber_funded.restaurant_food_promo"
  ]

fareKeysSubs :: [Text]
fareKeysSubs =
  [ fareKeySubDiscount,
    fareKeySubDelivery
  ]

fareKeySubDiscount :: Text
fareKeySubDiscount = "eats.mp.discounts.subscription_basket_dependent_discount"

fareKeySubDelivery :: Text
fareKeySubDelivery = "eats.mp.discounts.subscription_delivery_fee_discount"

fareKeysCorrections :: [Text]
fareKeysCorrections =
  [ "eats.mp.eater.base.delivery_fee_correction.exclusive",
    "eats.mp.eater.base.order_correction.exclusive",
    "eats.mp.eater.base.order_correction.tax.exclusive"
  ]

fareKeyTotal :: Text
fareKeyTotal = "eats_fare.total"

supportedFareKeys :: Set Text
supportedFareKeys =
  Set.fromList
    ( fareKeysBase
        <> fareKeysFees
        <> fareKeysPromos
        <> fareKeysSubs
        <> fareKeysCorrections
        <> [ fareKeyTotal,
             "" -- Not sure why this occurs - seems to be one-off bug?
           ]
    )

sumFare :: Fare -> Double
sumFare (Fare breakdown) = foldr (\FareItem {value} -> (+) value) 0.0 breakdown

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

        unless
          (fareTotal `approx` (sumFare (Fare breakdown) * 100))
          $ fail "invalid fare: components do not sum to total"

        pure
          Order
            { orderID = OrderID orderID,
              created,
              status,
              items,
              store = Store {title = storeTitle, address},
              fare = Fare breakdown
            }

      parseStatus :: Value -> Parser (UTCTime, OrderStatus)
      parseStatus = withArray "order state changes" $ \xs -> do
        (created, status) <-
          foldrM (flip parseFoldState) (Nothing, InProgress) xs
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
      parseCheckoutInfo v = do
        breakdown <- parser v

        -- Validate that sums match up.
        let FareItem {value = total} = (Map.!) breakdown fareKeyTotal
        let components = Map.delete fareKeyTotal breakdown
        let computedTotal = sumFare (Fare components)
        unless
          (total `approx` computedTotal)
          $ fail "invalid fare: components do not sum to total key"

        -- Return price components without sums.
        return components
        where
          parser :: Value -> Parser (Map Text FareItem)
          parser =
            withArray "fare" $
              (Map.fromList <$>) . traverse parseFareItem . toList

          parseFareItem :: Value -> Parser (Text, FareItem)
          parseFareItem = withObject "fare item" $ \i -> do
            key <- i .: "key"
            unless
              (key `Set.member` supportedFareKeys)
              $ fail $
                "unsupported fare item key: "
                  ++ show key
                  ++ " in value: "
                  ++ show v
            fareType <- i .: "type"
            label <- i .: "label"
            value <- i .: "rawValue"
            pure (key, FareItem {fareType, label, value})

      approx :: Double -> Double -> Bool
      approx a b = abs (a - b) < 0.01

newtype SessionID = SessionID Text

newtype OrderID = OrderID Text
  deriving (Eq, Ord, Show)

-- getOrdersSince takes a session ID cookie and a timestamp, and returns all
-- orders that were created after that timestamp.
getOrdersSince :: (MonadApp m) => SessionID -> UTCTime -> m [Order]
getOrdersSince sid since = sortOn created <$> getPagesRecurse Nothing
  where
    -- Base cases:
    -- 1. Empty page.
    -- 2. All orders on page are before `since`.
    -- 3. No remaining pages (`nextOrderID` is `Nothing`)
    getPagesRecurse :: (MonadApp m) => Maybe OrderID -> m [Order]
    getPagesRecurse orderID = do
      log $ "Loading page for orderID: " ++ show orderID
      (orders, nextOrderID) <- getOrdersPage sid orderID
      log $
        "Oldest order in page: "
          ++ show (created <$> viaNonEmpty head (sortOn created orders))
      let withinWindow = filter (\Order {created} -> created >= since) orders
      if length withinWindow < length orders || null withinWindow
        then return withinWindow
        else case nextOrderID of
          Just _ -> do
            nextOrders <- getPagesRecurse nextOrderID
            return $ withinWindow ++ nextOrders
          Nothing -> return withinWindow

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

render :: Order -> String
render Order {items, created, store = Store {title}, fare} =
  "Store: " ++ toString title ++ "\n"
    ++ "Date:  "
    ++ show created
    ++ "\n"
    ++ "Items:\n"
    ++ showItems items
    ++ "Fare:\n"
    ++ showFare fare
    ++ "Hypothetical prices:\n"
    ++ "- No  promos, no  sub: "
    ++ showMoney
      ( sumFare $ removeKeyGroup (fareKeysPromos ++ fareKeysSubs) fare
      )
    ++ "\n"
    ++ "- Yes promos, no  sub: "
    ++ showMoney (sumFare $ removeKeyGroup fareKeysSubs fare)
    ++ "\n"
    ++ "- No  promos, yes sub: "
    ++ showMoney (sumFare $ withSub $ removeKeyGroup fareKeysPromos fare)
    ++ "\n"
    ++ "- Yes promos, yes sub: "
    ++ showMoney (sumFare $ withSub fare)
    ++ "\n"
    ++ "Conclusions:\n"
    ++ "- Actual price:                    "
    ++ showMoney (sumFare fare)
    ++ "\n"
    ++ "- Money saved with sub:            "
    ++ showMoney
      ( sumFare (removeKeyGroup fareKeysSubs fare) - sumFare (withSub fare)
      )
    ++ "\n"
    ++ "- Money saved with sub, no promos: "
    ++ showMoney
      ( sumFare (removeKeyGroup fareKeysSubs fare)
          - sumFare (withSub $ removeKeyGroup fareKeysPromos fare)
      )
    ++ "\n"
  where
    showItems :: [Item] -> String
    showItems = mconcat . fmap showItem

    showItem :: Item -> String
    showItem Item {title = t, quantity, price} =
      "- " ++ toString t ++ ": "
        ++ "("
        ++ show quantity
        ++ "x "
        ++ showMoney (price / 100)
        ++ ")\n"

    showMoney :: Double -> String
    showMoney amount = printf "$%1.2f" amount

    showFare :: Fare -> String
    showFare (Fare breakdown) =
      "- Base:\n"
        ++ showFareGroup fareKeysBase
        ++ "- Fees:\n"
        ++ showFareGroup fareKeysFees
        ++ "- Promotions:\n"
        ++ showFareGroup fareKeysPromos
        ++ "- Subscription:\n"
        ++ showFareGroup fareKeysSubs
        ++ "- Corrections:\n"
        ++ showFareGroup fareKeysCorrections
        ++ "- Total:\n"
        ++ showFareItem fareKeyTotal
      where
        showFareGroup :: [Text] -> String
        showFareGroup = concatMap showFareItem

        showFareItem :: Text -> String
        showFareItem key = case Map.lookup key breakdown of
          Just FareItem {label, value} ->
            "  - " ++ toString label ++ ": " ++ showMoney value ++ "\n"
          Nothing -> ""

    removeKeyGroup :: [Text] -> Fare -> Fare
    removeKeyGroup keyGroup (Fare breakdown) =
      Fare $ foldr Map.delete breakdown keyGroup

    withSub :: Fare -> Fare
    withSub (Fare breakdown) =
      Fare $
        Map.alter (const basketDiscount) fareKeySubDiscount $
          Map.alter (const deliveryDiscount) fareKeySubDelivery breakdown
      where
        deliveryDiscount :: Maybe FareItem
        deliveryDiscount = case Map.lookup fareKeyFeeDelivery breakdown of
          Just FareItem {value} ->
            Just $
              FareItem
                { label = "Delivery Discount",
                  fareType = "debit",
                  value = -1.0 * value
                }
          Nothing -> Nothing

        basketDiscount :: Maybe FareItem
        basketDiscount =
          if subtotal > 15
            then
              Just $
                FareItem
                  { label = "Discount",
                    fareType = "debit",
                    value = -1.0 * subtotal * 0.05
                  }
            else Nothing
          where
            FareItem {value = subtotal} = (Map.!) breakdown fareKeyBaseSubtotal
