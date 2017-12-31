{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
module Web.Stripe.Source
    ( -- * API
      -- * Types
      Source (..)
    , SourceId (..)
    ) where

import Web.Stripe.Types ( Source (..)
                        , SourceId (..))
import Web.Stripe.Types.Util
