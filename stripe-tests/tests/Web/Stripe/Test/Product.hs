{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Product where

import           Data.Either   (Either(..), isRight)
import           Test.Hspec
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Product

productTests :: StripeSpec
productTests stripe = do
  describe "Product tests" $ do
    it "Successfully creates a Product" $ do
      result <- stripe $ do
        p <- createProduct (ProductName "sandwich")
        void $ deleteProduct (productId p)
        return p
      result `shouldSatisfy` isRight
    it "Successfully creates a Product with options" $ do
      result <- stripe $ do
        p <- createProduct (ProductName "balogna")
               -&- (ProductActive False)
               -&- (Description "Full of balogna goodness")
               -&- (Shippable False)
               -&- (Caption "Delicious")
        void $ deleteProduct (productId p)
        return p
      result `shouldSatisfy` isRight
      let Right Product { productName = pname
                        , productActive = pactive
                        , productDescription = Just pdesc
                        , productShippable = pship
                        , productCaption = Just pcapt
                        } = result
      pname `shouldBe` (ProductName "balogna")
      pactive `shouldBe` (ProductActive False)
      pdesc `shouldBe` (Description "Full of balogna goodness")
      pship `shouldBe` (Shippable False)
      pcapt `shouldBe` (Caption "Delicious")
    it "Successfully updates a Product" $ do
      result <- stripe $ do
        Product { productId = pid } <-
            createProduct (ProductName "balogna")
               -&- (Description "Full of balogna goodness")
        r <- updateProduct pid
               -&- (ProductActive False)
               -&- (Description "Empty of balogna goodness")
               -&- (Shippable False)
               -&- (Caption "Tasty?")
        void $ deleteProduct pid
        return r
      result `shouldSatisfy` isRight
      let Right Product { productName = pname
                        , productActive = pactive
                        , productDescription = Just pdesc
                        , productShippable = pship
                        , productCaption = Just pcapt
                        } = result
      pname `shouldBe` (ProductName "balogna")
      pactive `shouldBe` (ProductActive False)
      pdesc `shouldBe` (Description "Empty of balogna goodness")
      pship `shouldBe` (Shippable False)
      pcapt `shouldBe` (Caption "Tasty?")
