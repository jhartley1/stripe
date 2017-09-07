{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
module Web.Stripe.Sku
    ( -- * API
      CreateSku
    , createSku
    , GetSku
    , getSku
    , UpdateSku
    , updateSku
    , GetSkus
    , getSkus
    , DeleteSku
    , deleteSku
      -- * Types
    , Sku     (..)
    , SkuId   (..)
    ) where

import           Data.Text                (Text)
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest(..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Types         (Sku(..), SkuId(..), Limit(..),
                                           StartingAfter(..), EndingBefore(..))
import           Web.Stripe.Util          ((</>))

------------------------------------------------------------------------------
-- | Create a `Sku`
createSku
    :: Currency
    -> Price
    -> ProductId
    -> StripeRequest CreateSku
createSku
  currency                      -- ^ Currency `Sku`s price is specified in
  price                         -- ^ Price of the `Sku` to create
  (ProductId productid)         -- ^ ID of `Product` the `Sku` will be associated with
      = request
  where request = mkStripeRequest POST url params
        url     = "skus"
        params  = toStripeParam currency $
                  toStripeParam price $
                  toStripeParam (Param "product", productid) $
                  []

data CreateSku
type instance StripeReturn CreateSku = Sku
instance StripeHasParam CreateSku SkuId
instance StripeHasParam CreateSku SkuActive
instance StripeHasParam CreateSku SkuAttributes
instance StripeHasParam CreateSku SkuImage
instance StripeHasParam CreateSku MetaData

------------------------------------------------------------------------------
-- | Get a `Sku`
getSku
    :: SkuId                    -- ^ The ID of the `Sku` to retreive
    -> StripeRequest GetSku
getSku (SkuId skuid) = request
  where request = mkStripeRequest GET url params
        url     = "skus" </> skuid
        params  = []

data GetSku
type instance StripeReturn GetSku = Sku

------------------------------------------------------------------------------
-- | Update a `Sku`
updateSku
    :: SkuId                -- ^ The ID of the `Sku` to update
    -> StripeRequest UpdateSku
updateSku (SkuId skuid) = request
  where request = mkStripeRequest POST url params
        url     = "skus" </> skuid
        params  = []

data UpdateSku
type instance StripeReturn UpdateSku = Sku
instance StripeHasParam UpdateSku SkuActive
instance StripeHasParam UpdateSku Currency
instance StripeHasParam UpdateSku SkuImage
instance StripeHasParam UpdateSku MetaData
instance StripeHasParam UpdateSku Price

------------------------------------------------------------------------------
-- | Retrieve all `Sku`s
getSkus :: StripeRequest GetSkus
getSkus = request
  where request = mkStripeRequest GET url params
        url     = "skus"
        params  = []

data GetSkus
type instance StripeReturn GetSkus = (StripeList Sku)
instance StripeHasParam GetSkus (EndingBefore SkuId)
instance StripeHasParam GetSkus Limit
instance StripeHasParam GetSkus (StartingAfter SkuId)

------------------------------------------------------------------------------
-- | Delete a `Sku`
deleteSku
    :: SkuId                -- ^ The ID of the `Sku` to delete
    -> StripeRequest DeleteSku
deleteSku (SkuId skuid) = request
  where request = mkStripeRequest DELETE url params
        url     = "skus" </> skuid
        params  = []

data DeleteSku
type instance StripeReturn DeleteSku = StripeDeleteResult
