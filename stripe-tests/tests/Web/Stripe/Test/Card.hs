{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax  #-}
{-# LANGUAGE RecordWildCards   #-}
module Web.Stripe.Test.Card where

import           Data.Either
import           Data.Maybe
import           Test.Hspec
import           Web.Stripe.Card
import           Web.Stripe.Customer
import           Web.Stripe.StripeRequest (Expandable (Id))
import           Web.Stripe.Test.Prelude
import           Web.Stripe.Token

cardTests :: StripeSpec
cardTests stripe = do
    describe "Card tests" $ do
      it "Can create a Customer and add a Card by CardNumber" $ do
        result <- stripe $ do
          Customer { customerId = cid } <- createCustomer
          card <- createCustomerCard cid cardinfo
          void $ deleteCustomer cid
          return card
        result `shouldSatisfy` isRight

      it "Can create a Customer Card by TokenId" $ do
        result <- stripe $ do
          Token    { tokenId = tkid   } <- createCardToken (Just cardinfo)
          Customer { customerId = cid } <- createCustomer
          card <- createCustomerCardByToken cid tkid
          void $ deleteCustomer cid
          return card
        result `shouldSatisfy` isRight

      {-
      it "Can retrieve a Customer Card" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   , customerSources = StripeList { list = [ Card { cardId = cardid } ] }
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCard customerid cardid
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardLastFour `shouldBe` "4242"
        cardExpMonth `shouldBe` em
        cardExpYear `shouldBe` ey
      -}

      {-
      it "Can retrieve a Customer's Card with expansion" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   , customerSources = StripeList { list = [ Card { cardId = cardid } ] }
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCard customerid cardid -&- ExpandParams ["customer"]
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardLastFour `shouldBe` "4242"
        cardExpMonth `shouldBe` em
        cardExpYear `shouldBe` ey
      -}

      it "Can retrieve a Customer's Cards" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCards customerid
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight

      it "Can retrieve a Customer's Cards with Expansion" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   } <- createCustomer -&- cardinfo
          card <- getCustomerCards customerid -&- ExpandParams ["data.customer"]
          void $ deleteCustomer customerid
          return card
        result `shouldSatisfy` isRight

      it "Can delete a Customer's Cards" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                    , customerDefaultSource = Just (Id cardid)
                   } <- createCustomer -&- cardinfo
          result <- deleteCustomerCard customerid cardid
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight

      it "Can update a Customer's Card" $ do
        result <- stripe $ do
          Customer { customerId = customerid
                   , customerDefaultSource = Just (Id cardid)
                   } <- createCustomer -&- cardinfo
          result <- updateCustomerCard customerid cardid
                       -&- cardname
                       -&- cardcity
                       -&- cardcountry
                       -&- cardaddressOne
                       -&- cardaddressTwo
                       -&- cardaddressState
                       -&- cardzip
          void $ deleteCustomer customerid
          return result
        result `shouldSatisfy` isRight
        let Right Card{..} = result
        cardName           `shouldBe` (Just cardname)
        cardAddressCity    `shouldBe` (Just cardcity)
        cardAddressCountry `shouldBe` (Just cardcountry)
        cardAddressLine1   `shouldBe` (Just cardaddressOne)
        cardAddressLine2   `shouldBe` (Just cardaddressTwo)
        cardAddressState   `shouldBe` (Just cardaddressState)
        cardAddressZip     `shouldBe` (Just cardzip)

  where
    cardinfo = (mkNewCard credit em ey) { newCardCVC = Just cvc }
    debitinfo = (mkNewCard debit em ey) { newCardCVC = Just cvc }
    credit = CardNumber "4242424242424242"
    debit  = CardNumber "4000056655665556"
    em  = ExpMonth 12
    ey  = ExpYear 2020
    cvc = CVC "123"
    country = Country "US"
    routingnumber = RoutingNumber "110000000"
    accountnumber = AccountNumber "000123456789"
    name = Name "David Johnson"
    cardname = Name "cardName"
    cardcity         = AddressCity "Chicago"
    cardcountry      = AddressCountry "US"
    cardaddressOne   = AddressLine1 "123 Fake Street"
    cardaddressTwo   = AddressLine2 "456 Fake Street"
    cardaddressState = AddressState "IL"
    cardzip          = AddressZip "60610"
