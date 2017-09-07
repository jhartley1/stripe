{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Sku where

import           Data.Either   (Either(..), isRight)
import           Test.Hspec
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Sku

skuTests :: StripeSpec
skuTests stripe = return ()
