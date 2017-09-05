{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Transfer where

import           Data.Maybe
import           Data.Either
import           Data.String
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Transfer

------------------------------------------------------------------------------
-- the tests

transferTests :: StripeSpec
transferTests stripe =
  describe "Transfer tests" $ do
    it "Retrieves transfers" $ do
      result <- stripe $ do t <- getTransfers
                            return t
      result `shouldSatisfy` isRight
    it "Retrieves transfers expandable" $ do
      result <- stripe $ do t <- getTransfers -&- ExpandParams
                                   [ "data.balance_transaction"
                                   ]
                            return t
      result `shouldSatisfy` isRight
    -- it "Can't Cancel a committed transfer" $ do
    --   result <- stripe $ do
    --     Recipient { recipientId = rid } <-
    --       createRecipient
    --         name
    --         Individual
    --         -&- bankinfo
    --     Transfer { transferId = tid }
    --        <- createTransfer rid (Amount 100) USD
    --     t <- cancelTransfer tid
    --     void $ deleteRecipient rid
    --     return t
    --   result `shouldSatisfy` isLeft

