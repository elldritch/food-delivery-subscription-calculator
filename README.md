# food-delivery-subscription-calculator

Help you figure out whether to subscribe to Uber Eats Pass by calculating what you would have saved in previous months given your order history.

## Usage

```
$ fdsc --service=uber-eats --auth=foo # auth = cookie named "sid" after signing in online

TL;DR: Yes, you should subscribe. Over the last 6 months, you would have saved $X total.

Uber Eats Pass costs $X/month and saves you delivery fees + 5% over $15

Jan 2021:
Ordered X times
Did not have Eats Pass
Spent $Y without Eats Pass
Would have spent $Z with Eats Pass
Net difference: save $N this month

Orders:
- Date: Restaurant Name ($AMT)
  - $X subtotal
  - $X delivery fee (would be $X with Eats Pass)
  - $X service fee (would be $X with Eats Pass)
  - -$X coupon

Feb 2021

Mar 2021

Apr 2021

May 2021
Jun 2021

```
