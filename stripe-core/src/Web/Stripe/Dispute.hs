{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
-------------------------------------------
-- |
-- Module      : Web.Stripe.Dispute
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- < https:/\/\stripe.com/docs/api#diputes >
--
-- @
-- {-\# LANGUAGE OverloadedStrings \#-}
-- import Web.Stripe
-- import Web.Stripe.Charge
-- import Web.Stripe.Dispute
--
-- main :: IO ()
-- main = do
--   let config = StripeConfig (StripeKey "secret_key")
--   result <- stripe config $ getCharge (ChargeId "charge_id")
--   case result of
--     (Left stripeError) -> print stripeError
--     (Right (Charge { chargeDispute = dispute })) ->
--       case dispute of
--        (Just dispute) -> print dispute
--        Nothing        -> print "no dispute on this charge"
-- @
module Web.Stripe.Dispute
    ( -- * API
      GetDispute
    , getDispute
    , UpdateDispute
    , updateDispute
    , CloseDispute
    , closeDispute
      -- * Types
    , ChargeId        (..)
    , Dispute         (..)
    , DisputeId       (..)
    , DisputeReason   (..)
    , DisputeStatus   (..)
    , DisputeEvidence (..)
    , MetaData        (..)
    , mkDisputeEvidence
    ) where

import           Web.Stripe.StripeRequest (Method (POST, GET),
                                           StripeHasParam, StripeRequest (..),
                                           StripeReturn,
                                           mkStripeRequest)
import           Web.Stripe.Util          ((</>))
import           Web.Stripe.Types         (ChargeId (..), Dispute (..),
                                           DisputeId (..),
                                           DisputeReason (..),
                                           DisputeStatus (..),
                                           DisputeEvidence (..), MetaData(..),
                                           mkDisputeEvidence)
import           Web.Stripe.Types.Util    (getChargeId)

------------------------------------------------------------------------------
-- | Retrieve a `Dispute`
getDispute
    :: DisputeId      -- ^ The ID of the Dispute to be retrieved
    -> StripeRequest GetDispute
getDispute
    (DisputeId disputeid) = request
  where request = mkStripeRequest GET url params
        url = "disputes" </> disputeid
        params = []

data GetDispute
type instance StripeReturn GetDispute = Dispute

------------------------------------------------------------------------------
-- | `Dispute` to be updated
updateDispute
    :: ChargeId        -- ^ The ID of the Charge being disputed
    -> StripeRequest UpdateDispute
updateDispute
  chargeId = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute"
        params  = []

data UpdateDispute
type instance StripeReturn UpdateDispute = Dispute
instance StripeHasParam UpdateDispute DisputeEvidence
instance StripeHasParam UpdateDispute MetaData

------------------------------------------------------------------------------
-- | `Dispute` to be closed
closeDispute
    :: ChargeId  -- ^ The ID of the Charge being disputed
    -> StripeRequest CloseDispute
closeDispute
    chargeId = request
  where request = mkStripeRequest POST url params
        url     = "charges" </> getChargeId chargeId </> "dispute" </> "close"
        params  = []

data CloseDispute
type instance StripeReturn CloseDispute = Dispute
