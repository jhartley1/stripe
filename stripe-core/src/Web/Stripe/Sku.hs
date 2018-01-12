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
    , StartingAfter      (..)
    , EndingBefore       (..)
    , Limit              (..)
    , MetaData           (..)
    , StripeList         (..)
    , StripeDeleteResult (..)
    , SkuActive          (..)
    , Sku                (..)
    , SkuId              (..)
    , SkuPrice           (..)
    , Currency           (..)
    , ProductId          (..)
    , SkuAttributes      (..)
    , SkuImage           (..)
    ) where

import           Data.Text                (Text)
import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE), Param(..),
                                           StripeHasParam, StripeRequest(..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Types         (Sku(..), SkuId(..), SkuActive(..),
                                           StartingAfter(..), EndingBefore(..),
                                           Limit(..), MetaData(..), StripeList(..),
                                           StripeDeleteResult(..), Currency(..),
                                           SkuPrice(..), ProductId(..),
                                           SkuAttributes(..), SkuImage(..))
import           Web.Stripe.Util          ((</>))

------------------------------------------------------------------------------
-- | Create a `Sku`
createSku
    :: Currency
    -> SkuPrice                 -- ^ Currency 'Sku' price is specified in
    -> ProductId                -- ^ Price of the 'Sku' to create
    -> StripeRequest CreateSku  -- ^ ID of 'Product' the 'Sku' will be associated with
createSku
  currency                      
  price                         
  (ProductId productid)         
      = request
  where request = mkStripeRequest POST url params
        url     = "skus"
        params  = toStripeParam currency $
                  toStripeParam price $
                  toStripeParam (Param ("product" :: Text, productid)) $
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
instance StripeHasParam UpdateSku SkuPrice

------------------------------------------------------------------------------
-- | Retrieve all `Sku`s
getSkus :: StripeRequest GetSkus
getSkus = request
  where request = mkStripeRequest GET url params
        url     = "skus"
        params  = []

data GetSkus
type instance StripeReturn GetSkus = (StripeList Sku)
instance StripeHasParam GetSkus SkuActive
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
