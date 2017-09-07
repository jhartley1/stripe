{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
module Web.Stripe.Product
    ( -- * API
      CreateProduct
    , createProduct
    , GetProduct
    , getProduct
    , UpdateProduct
    , updateProduct
    , GetProducts
    , getProducts
    , DeleteProduct
    , deleteProduct
      -- * Types
    , Product            (..)
    , ProductId          (..)
    , ProductName        (..)
    , ProductURL         (..)
    , ProductAttributes  (..)
    , Shippable          (..)
    , StartingAfter      (..)
    , EndingBefore       (..)
    , Limit              (..)
    , MetaData           (..)
    , Description        (..)
    , StripeList         (..)
    , StripeDeleteResult (..)
    , ProductActive      (..)
    , Caption            (..)
    ) where

import           Data.Text                (Text)
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest(..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Types         (Product(..), ProductId(..),
                                           ProductName(..), ProductActive(..),
                                           ProductURL(..), ProductAttributes(..),
                                           Shippable(..), StartingAfter(..),
                                           EndingBefore(..), Limit(..), MetaData(..),
                                           Description(..), StripeList(..),
                                           StripeDeleteResult(..), Caption(..))
import           Web.Stripe.Util          ((</>))

------------------------------------------------------------------------------
-- | Create a `Product`
createProduct
    :: ProductName              -- ^ Name of `Product` to be displayed to user
    -> StripeRequest CreateProduct
createProduct name = request
  where request = mkStripeRequest POST url params
        url     = "products"
        params  = toStripeParam name []

data CreateProduct
type instance StripeReturn CreateProduct = Product
instance StripeHasParam CreateProduct ProductId
instance StripeHasParam CreateProduct ProductActive
instance StripeHasParam CreateProduct ProductAttributes
instance StripeHasParam CreateProduct Caption
instance StripeHasParam CreateProduct Description
instance StripeHasParam CreateProduct MetaData
instance StripeHasParam CreateProduct Shippable
instance StripeHasParam CreateProduct ProductURL

------------------------------------------------------------------------------
-- | Get a `Product`
getProduct
    :: ProductId                -- ^ The ID of the `Product` to retreive
    -> StripeRequest GetProduct
getProduct (ProductId productid) = request
  where request = mkStripeRequest GET url params
        url     = "products" </> productid
        params  = []

data GetProduct
type instance StripeReturn GetProduct = Product

------------------------------------------------------------------------------
-- | Update a `Product`
updateProduct
    :: ProductId                -- ^ The ID of the `Product` to update
    -> StripeRequest UpdateProduct
updateProduct (ProductId productid) = request
  where request = mkStripeRequest POST url params
        url     = "products" </> productid
        params  = []

data UpdateProduct
type instance StripeReturn UpdateProduct = Product
instance StripeHasParam UpdateProduct ProductActive
instance StripeHasParam UpdateProduct Caption
instance StripeHasParam UpdateProduct Description
instance StripeHasParam UpdateProduct MetaData
instance StripeHasParam UpdateProduct ProductName
instance StripeHasParam UpdateProduct Shippable
instance StripeHasParam UpdateProduct ProductURL

------------------------------------------------------------------------------
-- | Retrieve all `Product`s
getProducts :: StripeRequest GetProducts
getProducts = request
  where request = mkStripeRequest GET url params
        url     = "products"
        params  = []

data GetProducts
type instance StripeReturn GetProducts = (StripeList Product)
instance StripeHasParam GetProducts (EndingBefore ProductId)
instance StripeHasParam GetProducts Limit
instance StripeHasParam GetProducts (StartingAfter ProductId)
instance StripeHasParam GetProducts Shippable
instance StripeHasParam GetProducts ProductURL

------------------------------------------------------------------------------
-- | Delete a `Product`
deleteProduct
    :: ProductId                -- ^ The ID of the `Product` to delete
    -> StripeRequest DeleteProduct
deleteProduct (ProductId productid) = request
  where request = mkStripeRequest DELETE url params
        url     = "products" </> productid
        params  = []

data DeleteProduct
type instance StripeReturn DeleteProduct = StripeDeleteResult

