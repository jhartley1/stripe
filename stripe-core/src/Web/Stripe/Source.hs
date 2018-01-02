{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Web.Stripe.Source
    ( -- * API
      customerAttachSource
    , CustomerAttachSource
    , getCustomerSources
    , GetCustomerSources
      -- * Types
    , Source (..)
    , SourceId (..)
    , SourceValue (..)
    , SourceUsage (..)
    , SourceStatus (..)
    , SourceOwner (..)
    , SourceAuthFlow (..)
    ) where

import           Web.Stripe.StripeRequest (Method (GET, POST, DELETE),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn, ToStripeParam(..),
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         ( Source (..)
                                          , SourceId (..)
                                          , SourceValue (..)
                                          , SourceUsage (..)
                                          , SourceStatus (..)
                                          , SourceOwner (..)
                                          , SourceAuthFlow (..)
                                          , CustomerId (..)
                                          , StripeList (..))
import           Web.Stripe.Types.Util    (getCustomerId)

customerAttachSource :: CustomerId
                     -> SourceId
                     -> StripeRequest CustomerAttachSource
customerAttachSource
  customerId
  sourceId      = request
  where request = mkStripeRequest POST url params
        url     = "customers" </> (getCustomerId customerId) </> "sources"
        params  = toStripeParam sourceId []

data CustomerAttachSource
type instance StripeReturn CustomerAttachSource = Source

------------------------------------------------------------------------------
-- | Retrieve all sources associated with a `Customer`
getCustomerSources
    :: CustomerId
    -> StripeRequest GetCustomerSources
getCustomerSources customerId = request
    where request = mkStripeRequest GET url params
          url     = "customers" </> (getCustomerId customerId) </> "sources"
          params  = []

data GetCustomerSources
type instance StripeReturn GetCustomerSources = (StripeList Source)
