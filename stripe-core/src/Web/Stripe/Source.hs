{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Web.Stripe.Source
    ( -- * API
      customerAttachSource
    , CustomerAttachSource
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
                                          , CustomerId (..))
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
