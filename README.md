# food-delivery-subscription-calculator

`fdsc` helps you determine whether you would save money by subscribing to Uber Eats Pass by calculating what you would have saved in previous months given your order history.

## Usage

### Getting an authentication token

`fdsc` works by using a web client authentication token, and querying the `getPastOrders` API of the Uber Eats web app.

To get an authentication token, sign in to the Uber Eats web application, and examine the `sid` cookie from domain `.ubereats.com` after your dashboard loads. You can find this `sid` value by examining `Application > Storage > Cookies > https://www.ubereats.com` in the Chrome Inspector.

### Building `fdsc`

To build a binary, run:

```sh
# Builds the binary
cabal build fdsc

# Tells you where the built binary is
cabal list-bin fdsc
```

Alternatively, to run the command from within the repo:

```sh
cabal run fdsc -- ARGS
```

For those getting started with Haskell, use [ghcup](https://www.haskell.org/ghcup/) to install Cabal.

### Running `fdsc`

Running `fdsc` will print a table of your last 6 months worth of orders, with costs broken down by fee type. It also shows the impact of promotions (coupon codes, store discounts, buy-one-get-one-frees, etc.) and subscription discounts.

Here's an example output with one order:

```sh
$ fdsc --service=uber-eats --auth=YOUR_AUTH_TOKEN

Store: Toppu Ramen and Dim Sum House
Date:  2021-06-23 04:34:03 UTC
Items:
- D7. Steamed Taro Bun 芋茸包: (1x $5.98)
- D5. Steamed BBQ Pork Bun 蠔汁叉燒包: (1x $5.98)
- D10. Steamed Rice Noodle Rolls 路邊豬腸粉: (1x $6.98)
- A14. Chicken Karaage 炸日式雞塊: (1x $7.98)
- D3. Steamed Pork Dumpling 鮮蝦干蒸燒賣 : (1x $5.98)
- R2. Tonkotsu Miso Ramen 濃豚骨味噌湯拉麵: (1x $13.98)
Fare:
- Base:
  - Subtotal: $49.88
  - Tip: $9.01
  - Tax: $4.24
- Fees:
  - Delivery Fee: $0.99
  - Service Fee: $5.00
  - MPF cap dependent fee: $2.00
- Promotions:
  - Promotion: $-12.00
- Subscription:
- Corrections:
- Total:
Hypothetical prices:
- No  promos, no  sub: $71.12
- Yes promos, no  sub: $59.12
- No  promos, yes sub: $67.64
- Yes promos, yes sub: $55.64
Conclusions:
- Full price:                        $71.12
- Actual price:                      $59.12
- Money saved with sub:              $3.48
- Money saved with promos:           $12.00
- Money saved with sub if no promos: $-8.52

# Other orders elided...
```

Examining this table for the last couple months should help you work out whether you should buy a food delivery subscription.

### `fdsc` output FAQ

#### What is "money saved with sub if no promos"?

This field calculates how much money you would have saved _with_ a subscription but _without_ any applied promotions. This is because I've heard (but not confirmed) that food delivery services stop sending you discounts after you subscribe.

This number can be negative, which means you would have spent extra money if you had a subscription but because of the subscription did not receive the coupon code that you used.

#### How are promo savings calculated? Do you know about promos that I missed?

Promotions savings are retrieved from your historical order data - you'll see them if you actually used that promotion for a particular order.

## Contributing

Use `ormolu` to format code. Keep lines shorter than 80 columns. Make imports explicit.

See `TODO.md` for things that still need to get done.

## License

This project is released under the term of the Apache 2 license.
