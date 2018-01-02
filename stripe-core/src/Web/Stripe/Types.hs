{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.Stripe.Types
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.Stripe.Types where
------------------------------------------------------------------------------
import           Control.Applicative (pure, (<$>), (<*>), (<|>))
import           Control.Monad       (mzero)
import           Data.Aeson          (FromJSON (parseJSON), ToJSON(..),
                                      Value (String, Object, Bool), (.:),
                                      (.:?), (.!=), withObject, withText)
import           Data.Data           (Data, Typeable)
import qualified Data.HashMap.Strict as H
import           Data.Ratio          ((%))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Numeric             (fromRat, showFFloat)
import           Text.Read           (lexP, pfail)
import qualified Text.Read           as R
import           Web.Stripe.Util     (fromSeconds)
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | `Expandable` values
--    maps from an id to an object, e.g. `CardId` to `Card`
type family ExpandsTo id :: *

-- | a wrapper for fields which can either be an id or an expanded object
data Expandable id
  = Id id -- ^ an id such as `CardId`, `AccountId`, `CustomerId`, etc
  | Expanded (ExpandsTo id) -- ^ expanded object such as `Card`, `Account`, `Customer`, etc
    deriving (Typeable)

deriving instance (Data id, Data (ExpandsTo id)) => Data (Expandable id)
deriving instance (Show id, Show (ExpandsTo id)) => Show (Expandable id)
deriving instance (Read id, Read (ExpandsTo id)) => Read (Expandable id)
deriving instance (Eq   id, Eq   (ExpandsTo id)) => Eq   (Expandable id)
deriving instance (Ord  id, Ord  (ExpandsTo id)) => Ord  (Expandable id)

type instance ExpandsTo AccountId       = Account
type instance ExpandsTo CardId          = Card
type instance ExpandsTo ChargeId        = Charge
type instance ExpandsTo CustomerId      = Customer
type instance ExpandsTo DisputeId       = Dispute
type instance ExpandsTo InvoiceId       = Invoice
type instance ExpandsTo InvoiceItemId   = InvoiceItem
type instance ExpandsTo TransactionId   = BalanceTransaction

------------------------------------------------------------------------------
-- | JSON Instance for `Expandable`
instance (FromJSON id,  FromJSON (ExpandsTo id)) =>
         FromJSON (Expandable id) where
  parseJSON v = (Id <$> parseJSON v) <|> (Expanded <$> parseJSON v)

------------------------------------------------------------------------------
-- | specify a `TimeRange`
-- FIXME: this is a little awkward to use. How can we make it moar better?
data TimeRange a = TimeRange
    { gt  :: Maybe a
    , gte :: Maybe a
    , lt  :: Maybe a
    , lte :: Maybe a
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Time range with all values set to `Nothing`
emptyTimeRange :: TimeRange a
emptyTimeRange = TimeRange
    { gt  = Nothing
    , gte = Nothing
    , lt  = Nothing
    , lte = Nothing
    }

------------------------------------------------------------------------------
-- | `AvailableOn`
newtype AvailableOn = AvailableOn UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Created`
newtype Created = Created UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Date`
newtype Date = Date UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `ChargeId` associated with a `Charge`
newtype ChargeId
  = ChargeId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `ChargeId`
instance FromJSON ChargeId where
    parseJSON = withText "charge id" (pure . ChargeId)

------------------------------------------------------------------------------
-- | `StatementDescriptor` to be added to a `Charge`
newtype StatementDescriptor =
  StatementDescriptor Text deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON StatementDescriptor where
    parseJSON = withText "statement_descriptor" (pure . StatementDescriptor)

------------------------------------------------------------------------------
-- | `Charge` object in `Stripe` API
data Charge = Charge {
      chargeId                   :: ChargeId
    , chargeObject               :: Text
    , chargeCreated              :: UTCTime
    , chargeLiveMode             :: Bool
    , chargePaid                 :: Bool
    , chargeAmount               :: Amount
    , chargeCurrency             :: Currency
    , chargeRefunded             :: Bool
    , chargeSource               :: Maybe Card
    , chargeCaptured             :: Bool
    , chargeRefunds              :: StripeList Refund
    , chargeBalanceTransaction   :: Maybe (Expandable TransactionId)
    , chargeFailureMessage       :: Maybe Text
    , chargeFailureCode          :: Maybe Text
    , chargeAmountRefunded       :: Int
    , chargeCustomerId           :: Maybe (Expandable CustomerId)
    , chargeInvoice              :: Maybe (Expandable InvoiceId)
    , chargeDescription          :: Maybe Description
    , chargeDispute              :: Maybe (Expandable DisputeId)
    , chargeMetaData             :: MetaData
    , chargeStatementDescriptor  :: Maybe StatementDescriptor
    , chargeReceiptEmail         :: Maybe Text
    , chargeReceiptNumber        :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Charge`
instance FromJSON Charge where
    parseJSON =
      withObject "charge" $ \o ->
        Charge <$> (ChargeId <$> o .: "id")
               <*> o .: "object"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "livemode"
               <*> o .: "paid"
               <*> (Amount <$> o .: "amount")
               <*> o .: "currency"
               <*> o .: "refunded"
               <*> o .:? "source"
               <*> o .: "captured"
               <*> o .: "refunds"
               <*> o .:? "balance_transaction"
               <*> o .:? "failure_message"
               <*> o .:? "failure_code"
               <*> o .: "amount_refunded"
               <*> o .:? "customer"
               <*> o .:? "invoice"
               <*> o .:? "description"
               <*> o .:? "dispute"
               <*> o .: "metadata"
               <*> o .:? "statement_descriptor"
               <*> o .:? "receipt_email"
               <*> o .:? "receipt_number"

------------------------------------------------------------------------------
-- | Capture for `Charge`
newtype Capture = Capture { getCapture :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `RefundId` for `Refund`
newtype RefundId =
  RefundId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Refund` Object
data Refund = Refund {
      refundId                 :: RefundId
    , refundAmount             :: Int
    , refundCurrency           :: Currency
    , refundCreated            :: UTCTime
    , refundObject             :: Text
    , refundCharge             :: Expandable ChargeId
    , refundBalanceTransaction :: Expandable TransactionId
    , refundMetaData           :: MetaData
    , refundReason             :: Maybe RefundReason
    , refundReceiptNumber      :: Maybe Text
    , refundStatus             :: RefundStatus
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Refund`
instance FromJSON Refund where
   parseJSON =
     withObject "refund" $ \o ->
        Refund <$> (RefundId <$> o .: "id")
               <*> o .: "amount"
               <*> o .: "currency"
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "object"
               <*> o .: "charge"
               <*> o .: "balance_transaction"
               <*> o .: "metadata"
               <*> o .: "reason"
               <*> o .: "receipt_number"
               <*> o .: "status"

------------------------------------------------------------------------------
-- | `RefundApplicationFee`
newtype RefundApplicationFee =
  RefundApplicationFee { getRefundApplicationFee :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | status for a `Refund`
data RefundStatus
    = RefundSucceeded
    | RefundPending
    | RefundFailed
    | RefundCancelled
      deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `RefundStatus`
instance FromJSON RefundStatus where
    parseJSON (String "succeeded") = pure RefundSucceeded
    parseJSON (String "pending") = pure RefundPending
    parseJSON (String "failed") = pure RefundFailed
    parseJSON (String "cancelled") = pure RefundCancelled
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `RefundReason`
data RefundReason
  = RefundDuplicate
  | RefundFraudulent
  | RefundRequestedByCustomer
  deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `RefundReason`
instance FromJSON RefundReason where
    parseJSON (String "duplicate") = pure RefundDuplicate
    parseJSON (String "fraudulent") = pure RefundFraudulent
    parseJSON (String "requested_by_customer") = pure RefundRequestedByCustomer
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `CustomerId` for a `Customer`
newtype CustomerId = CustomerId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `CustomerId`
instance FromJSON CustomerId where
    parseJSON = withText "customer id" (pure . CustomerId)

------------------------------------------------------------------------------
-- | `Customer` object
data Customer = Customer {
      customerObject         :: Text
    , customerCreated        :: UTCTime
    , customerId             :: CustomerId
    , customerLiveMode       :: Bool
    , customerDescription    :: Maybe Description
    , customerEmail          :: Maybe Email
    , customerDelinquent     :: Bool
    , customerSubscriptions  :: StripeList Subscription
    , customerDiscount       :: Maybe Discount
    , customerAccountBalance :: Int
    , customerSources        :: StripeList Card
    , customerCurrency       :: Maybe Currency
    , customerDefaultSource  :: Maybe (Expandable CardId)
    , customerMetaData       :: MetaData
    } | DeletedCustomer {
      deletedCustomer   :: Maybe Bool
    , deletedCustomerId :: CustomerId
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Customer`
instance FromJSON Customer where
    parseJSON =
      withObject "customer" $ \o ->
        (Customer
         <$> o .: "object"
         <*> (fromSeconds <$> o .: "created")
         <*> (CustomerId <$> o .: "id")
         <*> o .: "livemode"
         <*> o .:? "description"
         <*> (fmap Email <$> o .:? "email")
         <*> o .: "delinquent"
         <*> o .: "subscriptions"
         <*> o .:? "discount"
         <*> o .: "account_balance"
         <*> o .: "sources"
         <*> o .:? "currency"
         <*> o .:? "default_source"
         <*> o .: "metadata"
         <|> DeletedCustomer -- FIXME?
         <$> o .: "deleted"
         <*> (CustomerId <$> o .: "id"))
         <|> DeletedCustomer
         <$> o .:? "deleted"
         <*> (CustomerId <$> o .: "id")

------------------------------------------------------------------------------
-- | AccountBalance for a `Customer`
newtype AccountBalance = AccountBalance Int
  deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------------------------------------
-- | CardId for a `Customer`
newtype CardId = CardId Text
  deriving (Eq, Ord, Read, Show, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `CardId`
instance FromJSON CardId where
    parseJSON = fmap CardId . parseJSON

------------------------------------------------------------------------------
-- | Number associated with a `Card`
newtype CardNumber     = CardNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Expiration Month for a `Card`
newtype ExpMonth       = ExpMonth Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Expiration Year for a `Card`
newtype ExpYear        = ExpYear Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | CVC for a `Card`
newtype CVC            = CVC Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | City address for a `Card`
newtype AddressCity    = AddressCity Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Country address for a `Card`
newtype AddressCountry = AddressCountry Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address Line One for a `Card`
newtype AddressLine1   = AddressLine1 Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address Line Two for a `Card`
newtype AddressLine2   = AddressLine2 Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address State for a `Card`
newtype AddressState   = AddressState Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Address Zip Code for a `Card`
newtype AddressZip     = AddressZip Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Credit / Debit Card Brand
data Brand = Visa
           | AMEX
           | MasterCard
           | Discover
           | JCB
           | DinersClub
           | Unknown
             deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Brand`
instance FromJSON Brand where
   parseJSON (String "American Express") = pure AMEX
   parseJSON (String "MasterCard") = pure MasterCard
   parseJSON (String "Discover") = pure Discover
   parseJSON (String "JCB") = pure JCB
   parseJSON (String "Visa") = pure Visa
   parseJSON (String "DinersClub") = pure DinersClub
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Card` Object
data Card = Card {
      cardId                :: CardId
    , cardObject            :: Text
    , cardLastFour          :: Text
    , cardBrand             :: Brand
    , cardFunding           :: Text
    , cardExpMonth          :: ExpMonth
    , cardExpYear           :: ExpYear
    , cardFingerprint       :: Text
    , cardCountry           :: Maybe Text
    , cardName              :: Maybe Name
    , cardAddressLine1      :: Maybe AddressLine1
    , cardAddressLine2      :: Maybe AddressLine2
    , cardAddressCity       :: Maybe AddressCity
    , cardAddressState      :: Maybe AddressState
    , cardAddressZip        :: Maybe AddressZip
    , cardAddressCountry    :: Maybe AddressCountry
    , cardCVCCheck          :: Maybe Text
    , cardAddressLine1Check :: Maybe Text
    , cardAddressZipCheck   :: Maybe Text
    , cardCustomerId        :: Maybe (Expandable CustomerId)
    , cardMetaData          :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Card`
instance FromJSON Card where
    parseJSON =
      withObject "card" $ \o ->
        Card <$> (CardId <$> o .: "id")
             <*> o .: "object"
             <*> o .: "last4"
             <*> o .: "brand"
             <*> o .: "funding"
             <*> (ExpMonth <$> o .: "exp_month")
             <*> (ExpYear <$> o .: "exp_year")
             <*> o .: "fingerprint"
             <*> o .:? "country"
             <*> o .:? "name"
             <*> (fmap AddressLine1 <$> o .:? "address_line1")
             <*> (fmap AddressLine2 <$> o .:? "address_line2")
             <*> (fmap AddressCity <$> o .:? "address_city")
             <*> (fmap AddressState <$> o .:? "address_state")
             <*> (fmap AddressZip <$> o .:? "address_zip")
             <*> (fmap AddressCountry <$> o .:? "address_country")
             <*> o .:? "cvc_check"
             <*> o .:? "address_line1_check"
             <*> o .:? "address_zip_check"
             <*> o .:? "customer"
             <*> o .: "metadata"

------------------------------------------------------------------------------
-- | `NewCard` contains the data needed to create a new `Card`
data NewCard = NewCard
    { newCardCardNumber     :: CardNumber
    , newCardExpMonth       :: ExpMonth
    , newCardExpYear        :: ExpYear
    , newCardCVC            :: Maybe CVC
    , newCardName           :: Maybe Name
    , newCardAddressLine1   :: Maybe AddressLine1
    , newCardAddressLine2   :: Maybe AddressLine2
    , newCardAddressCity    :: Maybe AddressCity
    , newCardAddressZip     :: Maybe AddressZip
    , newCardAddressState   :: Maybe AddressState
    , newCardAddressCountry :: Maybe AddressCountry
    }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | create a `NewCard` with only the required fields
mkNewCard
    :: CardNumber
    -> ExpMonth
    -> ExpYear
    -> NewCard
mkNewCard
    cardNumber
    expMonth
    expYear
    = NewCard
    { newCardCardNumber     = cardNumber
    , newCardExpMonth       = expMonth
    , newCardExpYear        = expYear
    , newCardCVC            = Nothing
    , newCardName           = Nothing
    , newCardAddressLine1   = Nothing
    , newCardAddressLine2   = Nothing
    , newCardAddressCity    = Nothing
    , newCardAddressZip     = Nothing
    , newCardAddressState   = Nothing
    , newCardAddressCountry = Nothing
    }

------------------------------------------------------------------------------
-- | set the `DefaultCard`
data DefaultCard = DefaultCard { getDefaultCard :: CardId }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `SubscriptionId` for a `Subscription`
newtype SubscriptionId = SubscriptionId { getSubscriptionId :: Text }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `SubscriptionId`
instance FromJSON SubscriptionId where
    parseJSON = withText "subscription id" (pure . SubscriptionId)

------------------------------------------------------------------------------
-- | Subscription Object
data Subscription = Subscription {
      subscriptionId                    :: SubscriptionId
    , subscriptionPlan                  :: Plan
    , subscriptionObject                :: Text
    , subscriptionStart                 :: UTCTime
    , subscriptionStatus                :: SubscriptionStatus
    , subscriptionCustomerId            :: Expandable CustomerId
    , subscriptionCancelAtPeriodEnd     :: Bool
    , subscriptionCurrentPeriodStart    :: UTCTime
    , subscriptionCurrentPeriodEnd      :: UTCTime
    , subscriptionEndedAt               :: Maybe UTCTime
    , subscriptionTrialStart            :: Maybe UTCTime
    , subscriptionTrialEnd              :: Maybe UTCTime
    , subscriptionCanceledAt            :: Maybe UTCTime
    , subscriptionQuantity              :: Quantity
    , subscriptionApplicationFeePercent :: Maybe Double
    , subscriptionDiscount              :: Maybe Discount
    , subscriptionMetaData              :: MetaData
    , subscriptionTaxPercent            :: Maybe Double
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Subscription`
instance FromJSON Subscription where
   parseJSON =
     withObject "subscription" $ \o ->
       Subscription <$> (SubscriptionId <$> o .: "id")
                    <*> o .: "plan"
                    <*> o .: "object"
                    <*> (fromSeconds <$> o .: "start")
                    <*> o .: "status"
                    <*> o .: "customer"
                    <*> o .: "cancel_at_period_end"
                    <*> (fromSeconds <$> o .: "current_period_start")
                    <*> (fromSeconds <$> o .: "current_period_end")
                    <*> (fmap fromSeconds <$> o .:? "ended_at")
                    <*> (fmap fromSeconds <$> o .:? "trial_start")
                    <*> (fmap fromSeconds <$> o .:? "trial_end")
                    <*> (fmap fromSeconds <$> o .:? "canceled_at")
                    <*> (Quantity <$> o .:  "quantity")
                    <*> o .:? "application_fee_percent"
                    <*> o .:? "discount"
                    <*> o .: "metadata"
                    <*> o .:? "tax_percent"

------------------------------------------------------------------------------
-- | Status of a `Subscription`
data SubscriptionStatus =
          Trialing
        | Active
        | PastDue
        | Canceled
        | UnPaid
        deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `SubscriptionStatus`
instance FromJSON SubscriptionStatus where
   parseJSON (String "trialing") = pure Trialing
   parseJSON (String "active")   = pure Active
   parseJSON (String "past_due") = pure PastDue
   parseJSON (String "canceled") = pure Canceled
   parseJSON (String "unpaid")   = pure UnPaid
   parseJSON _                   = mzero

------------------------------------------------------------------------------
-- | `TaxPercent` for a `Subscription`
newtype TaxPercent = TaxPercent Double deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `PlanId` for a `Plan`
newtype PlanId = PlanId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Plan object
data Plan = Plan {
      planInterval            :: Interval
    , planName                :: Text
    , planCreated             :: UTCTime
    , planAmount              :: Int
    , planCurrency            :: Currency
    , planId                  :: PlanId
    , planObject              :: Text
    , planLiveMode            :: Bool
    , planIntervalCount       :: Maybe Int -- optional, max of 1 year intervals allowed, default 1
    , planTrialPeriodDays     :: Maybe Int
    , planMetaData            :: MetaData
    , planStatementDescriptor :: Maybe StatementDescriptor
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Plan`
instance FromJSON Plan where
   parseJSON =
     withObject "plan" $ \o ->
        Plan <$> o .: "interval"
             <*> o .: "name"
             <*> (fromSeconds <$> o .: "created")
             <*> o .: "amount"
             <*> o .: "currency"
             <*> (PlanId <$> o .: "id")
             <*> o .: "object"
             <*> o .: "livemode"
             <*> o .:? "interval_count"
             <*> o .:? "trial_period_days"
             <*> o .: "metadata"
             <*> o .:? "statement_descriptor"

------------------------------------------------------------------------------
-- | `TrialPeriod` for a Plan
newtype TrialPeriod = TrialPeriod UTCTime deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `TrialEnd` for a Plan
newtype TrialEnd = TrialEnd UTCTime
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Interval for `Plan`s
data Interval = Day | Week | Month | Year deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Interval`
instance FromJSON Interval where
   parseJSON (String "day") = pure Day
   parseJSON (String "week") = pure Week
   parseJSON (String "month") = pure Month
   parseJSON (String "year") = pure Year
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Show` instance for `Interval`
instance Show Interval where
    show Day   = "day"
    show Week  = "week"
    show Month = "month"
    show Year  = "year"

------------------------------------------------------------------------------
-- | `Read` instance for `Interval`
instance Read Interval where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "day"   -> return Day
         "week"  -> return Week
         "month" -> return Month
         "year"  -> return Year
         _       -> pfail

------------------------------------------------------------------------------
-- | `Coupon` Duration
data Duration = Forever | Once | Repeating deriving (Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Show` instance for `Duration`
instance Show Duration where
    show Forever   = "forever"
    show Once      = "once"
    show Repeating = "repeating"

------------------------------------------------------------------------------
-- | `Read` instance for `Duration`
instance Read Duration where
  readPrec =
    do (R.String s) <- lexP
       case s of
         "forever"   -> return Forever
         "once"      -> return Once
         "repeating" -> return Repeating
         _           -> pfail

------------------------------------------------------------------------------
-- | JSON Instance for `Duration`
instance FromJSON Duration where
   parseJSON (String x)
       | x == "forever"   = pure Forever
       | x == "once"      = pure Once
       | x == "repeating" = pure Repeating
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Coupon` Object
data Coupon = Coupon {
      couponId               :: CouponId
    , couponCreated          :: UTCTime
    , couponPercentOff       :: Maybe Int
    , couponAmountOff        :: Maybe Int
    , couponCurrency         :: Maybe Currency
    , couponLiveMode         :: Bool
    , couponDuration         :: Duration
    , couponRedeemBy         :: Maybe UTCTime
    , couponMaxRedemptions   :: Maybe Int
    , couponTimesRedeemed    :: Maybe Int
    , couponDurationInMonths :: Maybe Int
    , couponValid            :: Bool
    , couponMetaData         :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Coupon`
instance FromJSON Coupon where
   parseJSON =
     withObject "coupon" $ \o ->
        Coupon <$> (CouponId <$> o .: "id")
               <*> (fromSeconds <$> o .: "created")
               <*> o .: "percent_off"
               <*> o .:? "amount_off"
               <*> o .:? "currency"
               <*> o .: "livemode"
               <*> o .: "duration"
               <*> (fmap fromSeconds <$> o .:? "redeem_by")
               <*> o .:? "max_redemptions"
               <*> o .:? "times_redeemed"
               <*> o .:? "duration_in_months"
               <*> o .: "valid"
               <*> o .: "metadata"

------------------------------------------------------------------------------
-- | `CouponId` for a `Coupon`
newtype CouponId = CouponId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `AmountOff` for a `Coupon`
newtype AmountOff = AmountOff Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `MaxRedemptions` for a `Coupon`
newtype MaxRedemptions = MaxRedemptions Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `PercentOff` for a `Coupon`
newtype PercentOff = PercentOff Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `RedeemBy` date for a `Coupon`
newtype RedeemBy = RedeemBy UTCTime deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `DurationInMonths` for a `Coupon`
newtype DurationInMonths = DurationInMonths Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `IntervalCount` for a `Coupon`
newtype IntervalCount   = IntervalCount Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `TrialPeriodDays` for a `Coupon`
newtype TrialPeriodDays = TrialPeriodDays Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Amount representing a monetary value.
-- Stripe represents pennies as whole numbers
-- i.e. 100 = $1
newtype Amount = Amount { getAmount :: Int }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Discount` for `Coupon`
data Discount = Discount {
      discountCoupon       :: Coupon
    , discountStart        :: UTCTime
    , discountEnd          :: Maybe UTCTime
    , discountCustomer     :: Expandable CustomerId
    , discountObject       :: Text
    , discountSubscription :: Maybe SubscriptionId
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Discount`
instance FromJSON Discount where
    parseJSON =
      withObject "discount" $ \o ->
        Discount <$> o .: "coupon"
                 <*> (fromSeconds <$> o .: "start")
                 <*> (fmap fromSeconds <$> o .:? "end")
                 <*> o .: "customer"
                 <*> o .: "object"
                 <*> (fmap SubscriptionId <$> o .:? "subscription")

------------------------------------------------------------------------------
-- | `Invoice` for a `Coupon`
newtype InvoiceId = InvoiceId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceId`
instance FromJSON InvoiceId where
    parseJSON = withText "invoice id" (pure . InvoiceId)

------------------------------------------------------------------------------
-- | `Invoice` Object
data Invoice = Invoice {
      invoiceDate                 :: UTCTime
    , invoiceId                   :: Maybe InvoiceId -- ^ If upcoming no ID will exist
    , invoicePeriodStart          :: UTCTime
    , invoicePeriodEnd            :: UTCTime
    , invoiceLineItems            :: StripeList InvoiceLineItem
    , invoiceSubTotal             :: Int
    , invoiceTotal                :: Int
    , invoiceCustomer             :: Expandable CustomerId
    , invoiceObject               :: Text
    , invoiceAttempted            :: Bool
    , invoiceClosed               :: Bool
    , invoiceForgiven             :: Bool
    , invoicePaid                 :: Bool
    , invoiceLiveMode             :: Bool
    , invoiceAttemptCount         :: Int
    , invoiceAmountDue            :: Int
    , invoiceCurrency             :: Currency
    , invoiceStartingBalance      :: Int
    , invoiceEndingBalance        :: Maybe Int
    , invoiceNextPaymentAttempt   :: Maybe UTCTime
    , invoiceWebHooksDeliveredAt  :: Maybe UTCTime
    , invoiceCharge               :: Maybe (Expandable ChargeId)
    , invoiceDiscount             :: Maybe Discount
    , invoiceApplicateFee         :: Maybe FeeId
    , invoiceSubscription         :: Maybe SubscriptionId
    , invoiceStatementDescriptor  :: Maybe StatementDescriptor
    , invoiceDescription          :: Maybe Description
    , invoiceMetaData             :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Invoice`
instance FromJSON Invoice where
   parseJSON =
     withObject "invoice" $ \o ->
       Invoice <$> (fromSeconds <$> o .: "date")
               <*> (fmap InvoiceId <$> o .:? "id")
               <*> (fromSeconds <$> o .: "period_start")
               <*> (fromSeconds <$> o .: "period_end")
               <*> o .: "lines"
               <*> o .: "subtotal"
               <*> o .: "total"
               <*> o .: "customer"
               <*> o .: "object"
               <*> o .: "attempted"
               <*> o .: "closed"
               <*> o .: "forgiven"
               <*> o .: "paid"
               <*> o .: "livemode"
               <*> o .: "attempt_count"
               <*> o .: "amount_due"
               <*> o .: "currency"
               <*> o .: "starting_balance"
               <*> o .:? "ending_balance"
               <*> (fmap fromSeconds <$> o .:? "next_payment_attempt")
               <*> (fmap fromSeconds <$> o .: "webhooks_delivered_at")
               <*> o .:? "charge"
               <*> o .:? "discount"
               <*> (fmap FeeId <$> o .:? "application_fee")
               <*> (fmap SubscriptionId <$> o .: "subscription")
               <*> o .:? "statement_descriptor"
               <*> o .:? "description"
               <*> o .: "metadata"

------------------------------------------------------------------------------
-- | Whether a `InvoiceLineItem` or `InvoiceItem` is discountable
newtype Discountable = Discountable Bool
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `InvoiceItemId` for `InvoiceItem`
newtype InvoiceItemId = InvoiceItemId Text
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `InvoiceItem` object
data InvoiceItem = InvoiceItem {
      invoiceItemObject       :: Text
    , invoiceItemId           :: InvoiceItemId
    , invoiceItemDate         :: UTCTime
    , invoiceItemAmount       :: Int
    , invoiceItemLiveMode     :: Bool
    , invoiceItemProration    :: Bool
    , invoiceItemCurrency     :: Currency
    , invoiceItemDiscountable :: Discountable
    , invoiceItemCustomer     :: Expandable CustomerId
    , invoiceItemDescription  :: Maybe Description
    , invoiceItemInvoice      :: Maybe (Expandable InvoiceId)
    , invoiceItemQuantity     :: Maybe Quantity
    , invoiceItemSubscription :: Maybe SubscriptionId
    , invoiceItemMetaData     :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceItem`
instance FromJSON InvoiceItem where
   parseJSON =
     withObject "invoice_item" $ \o ->
       InvoiceItem <$> o .: "object"
                   <*> (InvoiceItemId <$> o .: "id")
                   <*> (fromSeconds <$> o .: "date")
                   <*> o .: "amount"
                   <*> o .: "livemode"
                   <*> o .: "proration"
                   <*> o .: "currency"
                   <*> (Discountable <$> o .: "discountable")
                   <*> o .: "customer"
                   <*> o .:? "description"
                   <*> o .:? "invoice"
                   <*> (fmap Quantity <$> o .:? "quantity")
                   <*> o .:? "subscription"
                   <*> o .: "metadata"

------------------------------------------------------------------------------
-- | `InvoiceLineItemId` for an `InvoiceLineItem`
newtype InvoiceLineItemId =
    InvoiceLineItemId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Type of `InvoiceItem`
data InvoiceLineItemType
    = InvoiceItemType |
     SubscriptionItemType
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceLineItemType`
instance FromJSON InvoiceLineItemType where
   parseJSON (String "invoiceitem")  = pure InvoiceItemType
   parseJSON (String "subscription") = pure SubscriptionItemType
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `InvoiceLineItem` Object
data InvoiceLineItem = InvoiceLineItem {
      invoiceLineItemId           :: InvoiceLineItemId
    , invoiceLineItemObject       :: Text
    , invoiceLineItemType         :: InvoiceLineItemType
    , invoiceLineItemLiveMode     :: Bool
    , invoiceLineItemAmount       :: Int
    , invoiceLineItemCurrency     :: Currency
    , invoiceLineItemDiscountable :: Discountable
    , invoiceLineItemProration    :: Bool
    , invoiceLineItemPeriod       :: Period
    , invoiceLineItemQuantity     :: Maybe Quantity
    , invoiceLineItemPlan         :: Maybe Plan
    , invoiceLineItemDescription  :: Maybe Description
    , invoiceLineItemMetaData     :: MetaData
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Period for an `InvoiceLineItem`
data Period = Period {
      start :: UTCTime
    , end   :: UTCTime
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Period`
instance FromJSON Period where
   parseJSON =
     withObject "period" $ \o ->
       Period <$> (fromSeconds <$> o .: "start")
              <*> (fromSeconds <$> o .: "end")

------------------------------------------------------------------------------
-- | JSON Instance for `InvoiceLineItem`
instance FromJSON InvoiceLineItem where
   parseJSON =
     withObject "invoice_line_item" $ \o ->
       InvoiceLineItem <$> (InvoiceLineItemId <$> o .: "id")
                       <*> o .: "object"
                       <*> o .: "type"
                       <*> o .: "livemode"
                       <*> o .: "amount"
                       <*> o .: "currency"
                       <*> (Discountable <$> o .: "discountable")
                       <*> o .: "proration"
                       <*> o .: "period"
                       <*> (fmap Quantity <$> o .:? "quantity")
                       <*> o .:? "plan"
                       <*> o .:? "description"
                       <*> o .: "metadata"

------------------------------------------------------------------------------
-- | `Closed` - invoice closed or not
newtype Closed =
  Closed { getClosed :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Forgiven` - invoice forgiven or not
newtype Forgiven =
  Forgiven { getForgiven :: Bool }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Status of a `Dispute`
data DisputeStatus
    = WarningNeedsResponse
    | WarningUnderReview
    | WarningClosed
    | NeedsResponse
    | UnderReview
    | ChargeRefunded
    | Won
    | Lost
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `DisputeReason`
instance FromJSON DisputeReason where
   parseJSON (String "duplicate") = pure Duplicate
   parseJSON (String "fraudulent") = pure Fraudulent
   parseJSON (String "subscrption_canceled") = pure SubscriptionCanceled
   parseJSON (String "product_unacceptable") = pure ProductUnacceptable
   parseJSON (String "product_not_received") = pure ProductNotReceived
   parseJSON (String "credit_not_processed") = pure CreditNotProcessed
   parseJSON (String "general") = pure General
   parseJSON (String "incorrect_account_details") = pure IncorrectAccountDetails
   parseJSON (String "insufficient_funds") = pure InsufficientFunds
   parseJSON (String "bank_cannot_process") = pure BankCannotProcess
   parseJSON (String "debit_not_authorized") = pure DebitNotAuthorized
   parseJSON (String "customer_initiated") = pure CustomerInitiated
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Reason of a `Dispute`
data DisputeReason
    = Duplicate
    | Fraudulent
    | SubscriptionCanceled
    | ProductUnacceptable
    | ProductNotReceived
    | Unrecognized
    | CreditNotProcessed
    | General
    | IncorrectAccountDetails
    | InsufficientFunds
    | BankCannotProcess
    | DebitNotAuthorized
    | CustomerInitiated
      deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `DisputeStatus`
instance FromJSON DisputeStatus where
   parseJSON (String "needs_response") = pure NeedsResponse
   parseJSON (String "warning_needs_response") = pure WarningNeedsResponse
   parseJSON (String "warning_under_review") = pure WarningUnderReview
   parseJSON (String "warning_closed") = pure WarningClosed
   parseJSON (String "under_review") = pure UnderReview
   parseJSON (String "charge_refunded") = pure ChargeRefunded
   parseJSON (String "won") = pure Won
   parseJSON (String "lost") = pure Lost
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Dispute` Object
data Dispute = Dispute {
      disputeId                  :: DisputeId
    , disputeChargeId            :: Expandable ChargeId
    , disputeAmount              :: Int
    , disputeCreated             :: UTCTime
    , disputeStatus              :: DisputeStatus
    , disputeLiveMode            :: Bool
    , disputeCurrency            :: Currency
    , disputeObject              :: Text
    , disputeReason              :: DisputeReason
    , disputeIsChargeRefundable  :: Bool
    , disputeBalanceTransactions :: [BalanceTransaction]
    , disputeEvidenceDetails     :: Maybe EvidenceDetails
    , disputeEvidence            :: DisputeEvidence
    , disputeMetaData            :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `DisputeId` for a `Dispute`
newtype DisputeId =
    DisputeId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `DisputeId`
instance FromJSON DisputeId where
    parseJSON = fmap DisputeId . parseJSON

------------------------------------------------------------------------------
-- | `DisputeEvidence` associated with a `Dispute`
data DisputeEvidence = DisputeEvidence {
      disputeEvidenceAccessActivityLog            :: Maybe Text
    , disputeEvidenceBillingAddress               :: Maybe Text
    , disputeEvidenceCancellationPolicy           :: Maybe Text -- newtype this
    , disputeEvidenceCancellationPolicyDisclosure :: Maybe Text
    , disputeEvidenceCancellationRebuttal         :: Maybe Text
    , disputeEvidenceCustomerCommunication        :: Maybe Text -- newtype this
    , disputeEvidenceCustomerEmailAddress         :: Maybe Text
    , disputeEvidenceCustomerName                 :: Maybe Text
    , disputeEvidenceCustomerPurchaseIp           :: Maybe Text
    , disputeEvidenceCustomerSignature            :: Maybe Text -- newtype this
    , disputeEvidenceDuplicateChargeDocumentation :: Maybe Text -- newtype this
    , disputeEvidenceDuplicateChargeExplanation   :: Maybe Text
    , disputeEvidenceDuplicateChargeId            :: Maybe Text
    , disputeEvidenceProductDescription           :: Maybe Text
    , disputeEvidenceReceipt                      :: Maybe Text -- newtype this
    , disputeEvidenceRefundPolicy                 :: Maybe Text -- newtype this
    , disputeEvidenceRefundPolicyDisclosure       :: Maybe Text
    , disputeEvidenceRefundRefusalExplanation     :: Maybe Text
    , disputeEvidenceServiceDate                  :: Maybe Text
    , disputeEvidenceServiceDocumentation         :: Maybe Text
    , disputeEvidenceShippingAddress              :: Maybe Text
    , disputeEvidenceShippingDate                 :: Maybe Text
    , disputeEvidenceShippingDocumentation        :: Maybe Text -- newtype this
    , disputeEvidenceShippingTrackingNumber       :: Maybe Text
    , disputeEvidenceUncategorizedFile            :: Maybe Text -- newtype this
    , disputeEvidenceUncategorizedText            :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | create an empty `DisputeEvidence`
mkDisputeEvidence :: DisputeEvidence
mkDisputeEvidence = DisputeEvidence {
      disputeEvidenceAccessActivityLog = Nothing
    , disputeEvidenceBillingAddress = Nothing
    , disputeEvidenceCancellationPolicy = Nothing
    , disputeEvidenceCancellationPolicyDisclosure = Nothing
    , disputeEvidenceCancellationRebuttal = Nothing
    , disputeEvidenceCustomerCommunication = Nothing
    , disputeEvidenceCustomerEmailAddress = Nothing
    , disputeEvidenceCustomerName = Nothing
    , disputeEvidenceCustomerPurchaseIp = Nothing
    , disputeEvidenceCustomerSignature = Nothing
    , disputeEvidenceDuplicateChargeDocumentation = Nothing
    , disputeEvidenceDuplicateChargeExplanation = Nothing
    , disputeEvidenceDuplicateChargeId = Nothing
    , disputeEvidenceProductDescription = Nothing
    , disputeEvidenceReceipt = Nothing
    , disputeEvidenceRefundPolicy = Nothing
    , disputeEvidenceRefundPolicyDisclosure = Nothing
    , disputeEvidenceRefundRefusalExplanation = Nothing
    , disputeEvidenceServiceDate = Nothing
    , disputeEvidenceServiceDocumentation = Nothing
    , disputeEvidenceShippingAddress = Nothing
    , disputeEvidenceShippingDate = Nothing
    , disputeEvidenceShippingDocumentation = Nothing
    , disputeEvidenceShippingTrackingNumber = Nothing
    , disputeEvidenceUncategorizedFile = Nothing
    , disputeEvidenceUncategorizedText = Nothing
    }

------------------------------------------------------------------------------
-- | `EvidenceDetails` associated with a `Dispute`
data EvidenceDetails = EvidenceDetails {
      evidenceDetailsDueBy           :: UTCTime
    , evidenceDetailsSubmissionCount :: Int
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Dispute`
instance FromJSON Dispute where
    parseJSON =
      withObject "dispute" $ \o ->
        Dispute <$> (DisputeId <$> o .: "id")
                <*> o .: "charge"
                <*> o .: "amount"
                <*> (fromSeconds <$> o .: "created")
                <*> o .: "status"
                <*> o .: "livemode"
                <*> o .: "currency"
                <*> o .: "object"
                <*> o .: "reason"
                <*> o .: "is_charge_refundable"
                <*> o .: "balance_transactions"
                <*> o .: "evidence_details"
                <*> o .: "evidence"
                <*> o .: "metadata"

------------------------------------------------------------------------------
-- | JSON Instance for `DisputeEvidence`
instance FromJSON DisputeEvidence where
    parseJSON =
      withObject "dispute_evidence" $ \o ->
        DisputeEvidence <$> o .:? "access_activity_log"
                        <*> o .:? "billing_address"
                        <*> o .:? "cancellation_policy"
                        <*> o .:? "cancellation_policy_disclosure"
                        <*> o .:? "cancellation_rebuttal"
                        <*> o .:? "customer_communication"
                        <*> o .:? "customer_email_address"
                        <*> o .:? "customer_name"
                        <*> o .:? "customer_purchase_ip"
                        <*> o .:? "customer_signature"
                        <*> o .:? "duplicate_charge_documentation"
                        <*> o .:? "duplicate_charge_explanation"
                        <*> o .:? "duplicate_charge_id"
                        <*> o .:? "product_description"
                        <*> o .:? "receipt"
                        <*> o .:? "refund_policy"
                        <*> o .:? "refund_policy_disclosure"
                        <*> o .:? "refund_refusal_explanation"
                        <*> o .:? "service_date"
                        <*> o .:? "service_documentation"
                        <*> o .:? "shipping_address"
                        <*> o .:? "shipping_date"
                        <*> o .:? "shipping_documentation"
                        <*> o .:? "shipping_tracking_number"
                        <*> o .:? "uncategorized_file"
                        <*> o .:? "uncategorized_text"

------------------------------------------------------------------------------
-- | JSON Instance for `EvidenceDetails`
instance FromJSON EvidenceDetails where
    parseJSON =
      withObject "evidence_details" $ \o ->
        EvidenceDetails <$> (fromSeconds <$> o .: "due_by")
                        <*> o .: "submission_count"

------------------------------------------------------------------------------
-- | `TransferId`
newtype TransferId =
  TransferId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Type of a `Transfer`
data TransferType =
    CardTransfer
  | BankAccountTransfer
  | BitcoinReceiverTransfer
  | AlipayAccountTransfer
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TransferType`
instance FromJSON TransferType where
    parseJSON (String "card")             = pure CardTransfer
    parseJSON (String "bank_account")     = pure BankAccountTransfer
    parseJSON (String "bitcoin_receiver") = pure BitcoinReceiverTransfer
    parseJSON (String "alipay_account")   = pure AlipayAccountTransfer
    parseJSON _                           = mzero

------------------------------------------------------------------------------
-- | `Transfer` Object
data Transfer = Transfer {
      transferId                   :: TransferId
    , transferObject               :: Text
    , transferCreated              :: UTCTime
    , transferLiveMode             :: Bool
    , transferAmount               :: Int
    , transferCurrency             :: Currency
    , transferType                 :: TransferType
    , transferBalanceTransaction   :: Expandable TransactionId
    , transferDescription          :: Maybe Description
    , transferBankAccount          :: Maybe BankAccount
    , transferFailureMessage       :: Maybe Text
    , transferFailureCode          :: Maybe Text
    , transferStatementDescriptor  :: Maybe StatementDescriptor
    , transferMetaData             :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Transfer`
instance FromJSON Transfer where
    parseJSON =
      withObject "transfer" $ \o ->
        Transfer <$> (TransferId <$> o .: "id")
                    <*> o .: "object"
                    <*> (fromSeconds <$> o .: "created")
                    <*> o .: "livemode"
                    <*> o .: "amount"
                    <*> o .: "currency"
                    <*> o .: "source_type"
                    <*> o .: "balance_transaction"
                    <*> o .:? "description"
                    <*> o .:? "bank_account"
                    <*> o .:? "failure_message"
                    <*> o .:? "failure_code"
                    <*> o .:? "statement_descriptor"
                    <*> o .: "metadata"

------------------------------------------------------------------------------
-- | `BankAccount` Object
data BankAccount = BankAccount {
      bankAccountId                 :: BankAccountId
    , bankAccountObject             :: Text
    , bankAccountAccount            :: Maybe Text
    , bankAccountHolderName         :: Text
    , bankAccountHolderType         :: BankAccountHolderType
    , bankAccountBankName           :: Text
    , bankAccountCountry            :: Country
    , bankAccountCurrency           :: Currency
    , bankAccountCustomer           :: Text
    , bankAccountDefaultForCurrency :: Maybe Bool
    , bankAccountFingerprint        :: Text
    , bankAccountLast4              :: Text
    , bankAccountMetaData           :: MetaData
    , bankAccountRoutingNumber      :: Text
    , bankAccountStatus             :: BankAccountStatus
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccount` JSON Instance
instance FromJSON BankAccount where
   parseJSON =
     withObject "bank_account" $ \o ->
       BankAccount <$> (BankAccountId <$> o .: "id")
                   <*> o .: "object"
                   <*> o .:? "account"
                   <*> o .: "account_holder_name"
                   <*> o .: "account_holder_type"
                   <*> o .: "bank_name"
                   <*> (Country <$> o .: "country")
                   <*> o .: "currency"
                   <*> o .: "customer"
                   <*> o .:? "default_for_currency"
                   <*> o .: "fingerprint"
                   <*> o .: "last4"
                   <*> o .: "metadata"
                   <*> o .: "routing_number"
                   <*> o .: "status"

------------------------------------------------------------------------------
-- | `BankAccountHolderType` for `BankAccount`
data BankAccountHolderType =
  BankAccountHolderIndividual | BankAccountHolderCompany
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccountHolderType` JSON instance
instance FromJSON BankAccountHolderType where
    parseJSON (String "individual") = pure BankAccountHolderIndividual
    parseJSON (String "company") = pure BankAccountHolderCompany
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `BankAccountId` for `BankAccount`
newtype BankAccountId = BankAccountId Text
                        deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccountStatus` Object
data BankAccountStatus =
  New | Validated | Verified | VerificationFailed | Errored
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `BankAccountStatus` JSON instance
instance FromJSON BankAccountStatus where
   parseJSON (String "new") = pure New
   parseJSON (String "validated") = pure Validated
   parseJSON (String "verified") = pure Verified
   parseJSON (String "verification_failed") = pure VerificationFailed
   parseJSON (String "errored") = pure Errored
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Routing Number for Bank Account
newtype RoutingNumber =
  RoutingNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Country
newtype Country       =
  Country Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Account Number of a Bank Account
newtype AccountNumber =
  AccountNumber Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | create a new `BankAccount`
data NewBankAccount = NewBankAccount
    { newBankAccountAccountNumber :: AccountNumber
    , newBankAccountCountry       :: Country
    , newBankAccountCurrency      :: Currency
    , newBankAccountRoutingNumber :: Maybe RoutingNumber
    , newBankAccountHolderName    :: Maybe Text
    , newBankAccountHolderType    :: Maybe BankAccountHolderType
    }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `PlanId` for a `Plan`
newtype ApplicationFeeId = ApplicationFeeId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | ApplicationFee Object
data ApplicationFee = ApplicationFee {
      applicationFeeId                 :: ApplicationFeeId
    , applicationFeeObjecet            :: Text
    , applicationFeeCreated            :: UTCTime
    , applicationFeeLiveMode           :: Bool
    , applicationFeeAmount             :: Int
    , applicationFeeCurrency           :: Currency
    , applicationFeeRefunded           :: Bool
    , applicationFeeAmountRefunded     :: Int
    , applicationFeeRefunds            :: StripeList Refund
    , applicationFeeBalanceTransaction :: Expandable TransactionId
    , applicationFeeAccountId          :: Expandable AccountId
    , applicationFeeApplicationId      :: ApplicationId
    , applicationFeeChargeId           :: Expandable ChargeId
    , applicationFeeMetaData           :: MetaData
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | ApplicationFeePercent
newtype ApplicationFeePercent = ApplicationFeePercent Double
  deriving (Read, Show, Eq, Ord, Data, Typeable)


------------------------------------------------------------------------------
-- | ApplicationFeeAmount
newtype ApplicationFeeAmount = ApplicationFeeAmount Integer
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `ApplicationId` object
newtype ApplicationId = ApplicationId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `ApplicationId`
instance FromJSON ApplicationId where
    parseJSON = withText "application id" (pure . ApplicationId)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFee`
instance FromJSON ApplicationFee where
   parseJSON (Object o) =
       ApplicationFee <$> (ApplicationFeeId <$> o .: "id")
                      <*> o .: "object"
                      <*> (fromSeconds <$> o .: "created")
                      <*> o .: "livemode"
                      <*> o .: "amount"
                      <*> o .: "currency"
                      <*> o .: "refunded"
                      <*> o .: "amount_refunded"
                      <*> o .: "refunds"
                      <*> o .: "balance_transaction"
                      <*> o .: "account"
                      <*> (ApplicationId <$> o .: "application")
                      <*> o .: "charge"
                      <*> o .: "metadata"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `FeeId` for objects with Fees
newtype FeeId = FeeId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Application Fee Refunds
data ApplicationFeeRefund = ApplicationFeeRefund {
       applicationFeeRefundId                 :: RefundId
     , applicationFeeRefundAmount             :: Int
     , applicationFeeRefundCurrency           :: Currency
     , applicationFeeRefundCreated            :: UTCTime
     , applicationFeeRefundObject             :: Text
     , applicationFeeRefundBalanceTransaction :: Maybe (Expandable TransactionId)
     , applicationFeeRefundFee                :: FeeId
     , applicationFeeRefundMetaData           :: MetaData
     } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `ApplicationFeeRefund`
instance FromJSON ApplicationFeeRefund where
    parseJSON =
      withObject "application_fee_refund" $ \o ->
        ApplicationFeeRefund <$> (RefundId <$> o .: "id")
                             <*> o .: "amount"
                             <*> o .: "currency"
                             <*> (fromSeconds <$> o .: "created")
                             <*> o .: "object"
                             <*> o .:? "balance_transaction"
                             <*> (FeeId <$> o .: "fee")
                             <*> o .: "metadata"

------------------------------------------------------------------------------
-- | `AccountId` of an `Account`
newtype AccountId = AccountId Text
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `AccountId`
instance FromJSON AccountId where
  parseJSON = withText "account id" (pure . AccountId)

------------------------------------------------------------------------------
-- | `Account` Object
data Account = Account {
      accountId                   :: AccountId
    , accountEmail                :: Email
    , accountDisplayName          :: Maybe Text
    , accountDetailsSubmitted     :: Bool
    , accountChargesEnabled       :: Bool
    , accountDefaultCurrency      :: Currency
    , accountCountry              :: Text
    , accountObject               :: Text
    , accountBusinessName         :: Maybe Text
    , accountBusinessURL          :: Maybe Text
    , accountBusinessLogo         :: Maybe Text
    , accountStatementDescriptor  :: Maybe StatementDescriptor
    , accountSupportEmail         :: Maybe Text
    , accountSupportPhone         :: Maybe Text
    , accountTimeZone             :: Text
    , accountType                 :: AccountType
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Account`
instance FromJSON Account where
   parseJSON =
     withObject "account" $ \o ->
       Account <$> (AccountId <$> o .:  "id")
               <*> (Email <$> o .:  "email")
               <*> o .:   "display_name"
               <*> o .:   "details_submitted"
               <*> o .:   "charges_enabled"
               <*> o .:   "default_currency"
               <*> o .:   "country"
               <*> o .:   "object"
               <*> o .:?  "business_name"
               <*> o .:?  "business_url"
               <*> o .:?  "business_logo"
               <*> o .:?  "statement_descriptor"
               <*> o .:?  "support_email"
               <*> o .:?  "support_phone"
               <*> o .:   "timezone"
               <*> o .:   "type"

------------------------------------------------------------------------------
-- | type for an `Account`
data AccountType
    = AccountTypeStandard
    | AccountTypeExpress
    | AccountTypeCustom
      deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `AccountType`
instance FromJSON AccountType where
    parseJSON (String "standard") = pure AccountTypeStandard
    parseJSON (String "express") = pure AccountTypeExpress
    parseJSON (String "custom") = pure AccountTypeCustom
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Balance` Object
data Balance = Balance {
      balancePending   :: [BalanceAmount]
    , balanceAvailable :: [BalanceAmount]
    , balanceLiveMode  :: Bool
    , balanceObject    :: Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Balance`
instance FromJSON Balance where
   parseJSON =
     withObject "balance" $ \o ->
       Balance <$> o .: "pending"
               <*> o .: "available"
               <*> o .: "livemode"
               <*> o .: "object"

------------------------------------------------------------------------------
-- | `BalanceAmount` Object
data BalanceAmount = BalanceAmount {
      balanceAmount   :: Int
    , balanceCurrency :: Currency
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `BalanceAmount`
instance FromJSON BalanceAmount where
   parseJSON =
     withObject "balance_amount" $ \o ->
       BalanceAmount <$> o .: "amount"
                     <*> o .: "currency"

------------------------------------------------------------------------------
-- | `BalanceTransaction` Object
data BalanceTransaction = BalanceTransaction {
      balanceTransactionId             :: TransactionId
    , balanceTransactionObject         :: Text
    , balanceTransactionAmount         :: Int
    , balanceTransactionCurrency       :: Currency
    , balanceTransactionNet            :: Int
    , balanceTransactionType           :: TransactionType
    , balanceTransactionCreated        :: UTCTime
    , balanceTransactionAvailableOn    :: UTCTime
    , balanceTransactionStatus         :: Text
    , balanceTransactionFee            :: Int
    , balanceTransactionFeeDetails     :: [FeeDetails]
    , balanceTransactionFeeSource      :: Expandable ChargeId -- todo
    , balanceTransactionFeeDescription :: Maybe Description
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `BalanceTransaction`
instance FromJSON BalanceTransaction where
   parseJSON =
     withObject "balance_transaction" $ \o ->
       BalanceTransaction <$> (TransactionId <$> o .: "id")
                          <*> o .: "object"
                          <*> o .: "amount"
                          <*> o .: "currency"
                          <*> o .: "net"
                          <*> o .: "type"
                          <*> (fromSeconds <$> o .: "created")
                          <*> (fromSeconds <$> o .: "available_on")
                          <*> o .: "status"
                          <*> o .: "fee"
                          <*> o .: "fee_details"
                          <*> o .: "source"
                          <*> o .:? "description"

------------------------------------------------------------------------------
-- | `TransactionId` of a `Transaction`
newtype TransactionId = TransactionId Text
                   deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TransactionId`
instance FromJSON TransactionId where
    parseJSON = withText "transaction id" (pure . TransactionId)

------------------------------------------------------------------------------
-- | `FeeDetails` Object
data FeeDetails = FeeDetails {
      feeDetailsAmount   :: Int
    , feeDetailsCurrency :: Currency
    , feeType            :: Text
    , feeDescription     :: Maybe Description
    , feeApplication     :: Maybe Text
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `FeeDetails`
instance FromJSON FeeDetails where
   parseJSON =
     withObject "fee_details" $ \o ->
       FeeDetails <$> o .: "amount"
                  <*> o .: "currency"
                  <*> o .: "type"
                  <*> o .: "description"
                  <*> o .:? "application"

------------------------------------------------------------------------------
-- | `BalanceSource` used for filtering `Balance` transactions. It should contain
-- an object Id such as a `ChargeId`
newtype BalanceSource a = BalanceSource { getSource :: a }
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Source` object
data Source = Source {
      sourceId                  :: SourceId
    , sourceAmount              :: Maybe Int
    , sourceClientSecret        :: Text
    , sourceCreated             :: UTCTime
    , sourceCurrency            :: Maybe Currency
    , sourceFlow                :: SourceAuthFlow
    , sourceLiveMode            :: Bool
    , sourceMetaData            :: MetaData
    , sourceOwner               :: SourceOwner
    , sourceStatementDescriptor :: Maybe Text
    , sourceStatus              :: SourceStatus
    , sourceValue               :: SourceValue
    , sourceUsage               :: SourceUsage
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `Source`
instance FromJSON Source where
    parseJSON = withObject "source" $ \o -> do
      sourceId <- o .: "id"
      sourceAmount <- o .:? "amount"
      sourceClientSecret <- o .: "client_secret"
      sourceCreated <- fmap fromSeconds (o .: "created")
      sourceCurrency <- o .:? "currency"
      sourceLiveMode <- o .: "livemode"
      sourceMetaData <- o .: "metadata"
      sourceOwner <- o .: "owner"
      sourceStatementDescriptor <- o .:? "statement_descriptor"
      sourceStatus <- o .: "status"
      sourceUsage <- o .: "usage"

      String valType <- o .: "type"
      sourceValue <-
          case valType of
            "card" -> pure SourceValueCard
            "three_d_secure" -> pure SourceValueThreeDSecure
            "giropay" -> pure SourceValueGiropay
            "sepa_debit" -> pure SourceValueSepaDebit
            "ideal" -> pure SourceValueIdeal
            "sofort" -> pure SourceValueSofort
            "bancontact" -> pure SourceValueBancontact
            "alipay" -> pure SourceValueAlipay
            "bitcoin" -> SourceValueBitcoin <$> o .: "bitcoin"
            "p24" -> pure SourceValueP24

      String flow <- o .: "flow"
      sourceFlow <-
          case flow of
            "redirect" -> SourceAuthFlowRedirect <$> o .: "redirect"
            "receiver" -> SourceAuthFlowReceiver <$> o .: "receiver"
            "code_verification" ->
                SourceAuthFlowCodeVerification <$> o .: "code_verification"
            "none" -> pure SourceAuthFlowNone
      return Source{..}

------------------------------------------------------------------------------
-- | Id for a `Source` object
newtype SourceId = SourceId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `SourceId`
instance FromJSON SourceId where
    parseJSON = withText "source id" (pure . SourceId)

------------------------------------------------------------------------------
-- | Owner for a `Source` object
data SourceOwner = SourceOwner {
      ownerAddress :: Maybe Text
    , ownerEmail :: Maybe Text
    , ownerName :: Maybe Text
    , ownerPhone :: Maybe Text
    , ownerVerifiedAddress :: Maybe Text
    , ownerVerifiedEmail :: Maybe Text
    , ownerVerifiedName :: Maybe Text
    , ownerVerifiedPhone :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for an `Owner`
instance FromJSON SourceOwner where
    parseJSON =
      withObject "owner" $ \o ->
        SourceOwner <$> o .:? "address"
                    <*> o .:? "email"
                    <*> o .:? "name"
                    <*> o .:? "phone"
                    <*> o .:? "verified_address"
                    <*> o .:? "verified_email"
                    <*> o .:? "verified_name"
                    <*> o .:? "verified_phone"

------------------------------------------------------------------------------
-- | type for a `Source` object
data SourceValue
  = SourceValueCard
  | SourceValueThreeDSecure
  | SourceValueGiropay
  | SourceValueSepaDebit
  | SourceValueIdeal
  | SourceValueSofort
  | SourceValueBancontact
  | SourceValueAlipay
  | SourceValueBitcoin SourceBitcoin
  | SourceValueP24
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | bitcoin `Source` data
data SourceBitcoin = SourceBitcoin {
      sourceBitcoinAddress :: Text
    , sourceBitcoinAmount :: Int
    , sourceBitcoinAmountCharged :: Int
    , sourceBitcoinAmountReceived :: Int
    , sourceBitcoinAmountReturned :: Int
    , sourceBitcoinUri :: Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SourceBitcoin`
instance FromJSON SourceBitcoin where
    parseJSON =
      withObject "bitcoin" $ \o ->
        SourceBitcoin <$> o .: "address"
                      <*> o .: "amount"
                      <*> o .: "amount_charged"
                      <*> o .: "amount_received"
                      <*> o .: "amount_returned"
                      <*> o .: "uri"

------------------------------------------------------------------------------
-- | receiver for a `Source` object
data SourceFlowReceiver = SourceFlowReceiver {
      sourceFlowReceiverAddress        :: Text
    , sourceFlowReceiverAmountCharged  :: Int
    , sourceFlowReceiverAmountReceived :: Int
    , sourceFlowReceiverAmountReturned :: Int
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `SourceFlowReceiver`
instance FromJSON SourceFlowReceiver where
    parseJSON =
      withObject "receiver" $ \o ->
        SourceFlowReceiver <$> o .: "address"
                           <*> o .: "amount_charged"
                           <*> o .: "amount_received"
                           <*> o .: "amount_returned"

------------------------------------------------------------------------------
-- | flow for a `Source` object
data SourceAuthFlow
  = SourceAuthFlowRedirect SourceFlowRedirect
  | SourceAuthFlowReceiver SourceFlowReceiver
  | SourceAuthFlowCodeVerification SourceFlowCodeVerification
  | SourceAuthFlowNone
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | code_verification object for a `Source` object
data SourceFlowCodeVerification = SourceFlowCodeVerification {
      sourceFlowCodeVerificationAttemptsRemaining :: Int
    , sourceFlowCodeVerificationStatus :: Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SourceCodeVerification`
instance FromJSON SourceFlowCodeVerification where
    parseJSON =
      withObject "code_verification" $ \o ->
        SourceFlowCodeVerification <$> o .: "attempts_remaining"
                                   <*> o .: "status"

------------------------------------------------------------------------------
-- | redirect for a `Source` object
data SourceFlowRedirect = SourceFlowRedirect {
      sourceFlowRedirectReturnURL :: Text
    , sourceFlowRedirectStatus    :: Text
    , sourceFlowRedirectURL       :: Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SourceFlowRedirect`
instance FromJSON SourceFlowRedirect where
    parseJSON =
      withObject "redirect" $ \o ->
        SourceFlowRedirect <$> o .: "return_url"
                           <*> o .: "status"
                           <*> o .: "url"

------------------------------------------------------------------------------
-- | status of a `Source` object
data SourceStatus
  = SourceCanceled
  | SourceChargeable
  | SourceConsumed
  | SourceFailed
  | SourcePending
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `SourceStatus`
instance FromJSON SourceStatus where
    parseJSON (String "canceled") = pure SourceCanceled
    parseJSON (String "chargeable") = pure SourceChargeable
    parseJSON (String "consumed") = pure SourceConsumed
    parseJSON (String "failed") = pure SourceFailed
    parseJSON (String "pending") = pure SourcePending
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | whether a `Source` object is single use or reusable
data SourceUsage
  = SourceSingleUse
  | SourceReusable
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `SourceUsage`
instance FromJSON SourceUsage where
    parseJSON (String "single_use") = pure SourceSingleUse
    parseJSON (String "reusable") = pure SourceReusable
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | type for an `Order` object
data Order = Order {
      orderId             :: OrderId
    , orderAmount         :: Int
    , orderAmountReturned :: Maybe Int
    , orderApplication    :: Maybe ApplicationId
    , orderApplicationFee :: Maybe Int
    , orderCharge         :: Maybe ChargeId
    , orderCreated        :: UTCTime
    , orderCurrency       :: Currency
    , orderCustomer       :: Maybe CustomerId
    , orderEmail          :: Maybe Text
    , orderItems          :: [OrderItem]
    , orderLiveMode       :: Bool
    , orderMetaData       :: MetaData
    , orderUpdated        :: UTCTime
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for an `Order`
instance FromJSON Order where
    parseJSON =
      withObject "order" $ \o ->
        Order <$> o .:  "id"
              <*> o .:  "amount"
              <*> o .:? "amount_returned"
              <*> o .:? "application"
              <*> o .:? "application_fee"
              <*> o .:? "charge"
              <*> fmap fromSeconds (o .: "created")
              <*> o .:  "currency"
              <*> o .:? "customer"
              <*> o .:? "email"
              <*> o .:  "items"
              <*> o .:  "livemode"
              <*> o .:  "metadata"
              <*> fmap fromSeconds (o .: "updated")

------------------------------------------------------------------------------
-- | Id for an `Order` object
newtype OrderId = OrderId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for `OrderId`
instance FromJSON OrderId where
    parseJSON = withText  "order id" (pure . OrderId)

------------------------------------------------------------------------------
-- | OrderItem for an `Order` object
data OrderItem = OrderItem {
      orderItemAmount :: Int
    , orderItemCurrency :: Currency
    , orderItemDescription :: Text
    , orderItemQuantity :: Maybe Int
    , orderItemType :: Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for an `OrderItem`
instance FromJSON OrderItem where
    parseJSON =
      withObject "order_item" $ \o ->
        OrderItem <$> o .:  "amount"
                  <*> o .:  "currency"
                  <*> o .:  "description"
                  <*> o .:? "quantity"
                  <*> o .:  "type"

------------------------------------------------------------------------------
-- | type for a `Review`
data Review = Review {
      reviewId :: ReviewId
    , reviewCharge :: ChargeId
    , reviewCreated :: UTCTime
    , reviewLiveMode :: Bool
    , reviewOpen :: Bool
    , reviewReason :: ReviewReason
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `Review`
instance FromJSON Review where
    parseJSON =
      withObject "review" $ \o ->
        Review <$> o .: "id"
               <*> o .: "charge"
               <*> fmap fromSeconds (o .: "created")
               <*> o .: "livemode"
               <*> o .: "open"
               <*> o .: "reason"

------------------------------------------------------------------------------
-- | Id for a `Review`
newtype ReviewId = ReviewId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for  a `ReviewId`
instance FromJSON ReviewId where
    parseJSON = withText "review id" (pure . ReviewId)

------------------------------------------------------------------------------
-- | Reason for a `Review`
data ReviewReason
    = ReasonRule
    | ReasonManual
    | ReasonApproved
    | ReasonRefunded
    | ReasonRefundedAsFraud
    | ReasonDisputed
      deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `ReviewReason`
instance FromJSON ReviewReason where
    parseJSON (String "rule") = pure ReasonRule
    parseJSON (String "manual") = pure ReasonManual
    parseJSON (String "approved") = pure ReasonApproved
    parseJSON (String "refunded") = pure ReasonRefunded
    parseJSON (String "refunded_as_fraud") = pure ReasonRefundedAsFraud
    parseJSON (String "disputed") = pure ReasonDisputed
    parseJSON _ = mzero

------------------------------------------------------------------------------
-- | type for a `Payout`
data Payout = Payout {
      payoutId             :: PayoutId
    , payoutAmount         :: Int
    , payoutArrivalDate    :: UTCTime
    , payoutCreated        :: UTCTime
    , payoutCurrency       :: Currency
    , payoutDescripiton    :: Text
    , payoutFailureMessage :: Maybe Text
    , payoutLiveMode       :: Bool
    , payoutMetaData       :: MetaData
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `Payout`
instance FromJSON Payout where
    parseJSON =
      withObject "payout" $ \o ->
        Payout <$> o .: "id"
               <*> o .: "amount"
               <*> fmap fromSeconds (o .: "arrival_date")
               <*> fmap fromSeconds (o .: "created")
               <*> o .: "currency"
               <*> o .: "description"
               <*> o .:? "failure_message"
               <*> o .: "livemode"
               <*> o .: "metadata"

------------------------------------------------------------------------------
-- | Id for a `Payout`
newtype PayoutId = PayoutId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `PayoutId`
instance FromJSON PayoutId where
    parseJSON = fmap PayoutId . parseJSON

------------------------------------------------------------------------------
-- | `Caption` type
newtype Caption = Caption Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `Caption`
instance FromJSON Caption where
    parseJSON = fmap Caption . parseJSON

------------------------------------------------------------------------------
-- | Whether a `Product` is active
newtype ProductActive = ProductActive Bool
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `ProductActive`
instance FromJSON ProductActive where
    parseJSON = fmap ProductActive . parseJSON

------------------------------------------------------------------------------
-- | URL for a `Product`
newtype ProductURL = ProductURL Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `ProductUrl`
instance FromJSON ProductURL where
    parseJSON = fmap ProductURL . parseJSON

------------------------------------------------------------------------------
-- | Attributes for a `Product`
newtype ProductAttributes = ProductAttributes [Text]
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `ProductAttributes`
instance FromJSON ProductAttributes where
    parseJSON = fmap ProductAttributes . parseJSON

------------------------------------------------------------------------------
-- | type for a `Product`
data Product = Product {
      productId :: ProductId
    , productActive :: ProductActive
    , productAttributes :: ProductAttributes
    , productCaption :: Maybe Caption
    , productCreated :: UTCTime
    , productDescription :: Maybe Description
    , productImages :: [Text]
    , productLiveMode :: Bool
    , productMetaData :: MetaData
    , productName :: ProductName
    , productShippable :: Shippable
    , productUpdated :: UTCTime
    , productURL :: Maybe ProductURL
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `Product`
instance FromJSON Product where
    parseJSON =
      withObject "product" $ \o ->
        Product <$> o .: "id"
                <*> o .: "active"
                <*> o .: "attributes"
                <*> o .:? "caption"
                <*> fmap fromSeconds (o .: "created")
                <*> o .:? "description"
                <*> o .: "images"
                <*> o .: "livemode"
                <*> o .: "metadata"
                <*> o .: "name"
                <*> o .: "shippable"
                <*> fmap fromSeconds (o .: "updated")
                <*> o .:? "url"

------------------------------------------------------------------------------
-- | Id for a `Product`
newtype ProductId = ProductId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `ProductId`
instance FromJSON ProductId where
    parseJSON = fmap ProductId . parseJSON

------------------------------------------------------------------------------
-- | Name for a `Product`
newtype ProductName = ProductName Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `ProductName`
instance FromJSON ProductName where
    parseJSON = fmap ProductName . parseJSON

------------------------------------------------------------------------------
-- | Whether a `Product` is Shippable
newtype Shippable = Shippable Bool
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `Shippable`
instance FromJSON Shippable where
    parseJSON = fmap Shippable . parseJSON

------------------------------------------------------------------------------
-- | type for a `Sku`
data Sku = Sku {
      skuId         :: SkuId
    , skuActive     :: SkuActive
    , skuAttributes :: SkuAttributes
    , skuCreated    :: UTCTime
    , skuCurrency   :: Currency
    , skuImage      :: Maybe SkuImage
    , skuLiveMode   :: Bool
    , skuMetaData   :: MetaData
    , skuPrice      :: SkuPrice
    , skuProduct    :: ProductId
    , skuUpdated    :: UTCTime
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `Sku`
instance FromJSON Sku where
    parseJSON =
      withObject "sku" $ \o ->
        Sku <$> o .: "id"
            <*> o .: "active"
            <*> o .: "attributes"
            <*> fmap fromSeconds (o .: "created")
            <*> o .: "currency"
            <*> o .:? "image"
            <*> o .: "livemode"
            <*> o .: "metadata"
            <*> o .: "price"
            <*> o .: "product"
            <*> fmap fromSeconds (o .: "updated")

------------------------------------------------------------------------------
-- | Id for a `Sku`
newtype SkuId = SkuId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SkuId`
instance FromJSON SkuId where
    parseJSON = fmap SkuId . parseJSON

------------------------------------------------------------------------------
-- | Whether a `Sku` is active
newtype SkuActive = SkuActive Bool
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SkuActive`
instance FromJSON SkuActive where
    parseJSON = fmap SkuActive . parseJSON

------------------------------------------------------------------------------
-- | Price for a `Sku`
newtype SkuPrice = SkuPrice Int
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SkuPrice`
instance FromJSON SkuPrice where
    parseJSON = fmap SkuPrice . parseJSON

------------------------------------------------------------------------------
-- | Image for a `Sku`
newtype SkuImage = SkuImage Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SkuImage`
instance FromJSON SkuImage where
    parseJSON = fmap SkuImage . parseJSON

------------------------------------------------------------------------------
-- | Attributes for a `Sku`
newtype SkuAttributes = SkuAttributes [ (Text, Text) ]
    deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for a `SkuAttributes`
instance FromJSON SkuAttributes where
    parseJSON j = (SkuAttributes . H.toList) <$> (parseJSON j)

------------------------------------------------------------------------------
-- | transaction type for `BalanceTransaction`
data TransactionType
  = ChargeTxn
  | RefundTxn
  | AdjustmentTxn
  | ApplicationFeeTxn
  | ApplicationFeeRefundTxn
  | TransferTxn
  | TransferCancelTxn
  | TransferFailureTxn
  | PaymentTxn
  | PaymentFailureRefundTxn
  | PaymentRefundTxn
  | TransferRefundTxn
  | PayoutTxn
  | PayoutCancelTxn
  | PayoutFailureTxn
  | ValidationTxn
    deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON TransactionType where
  parseJSON (String "charge")           = pure ChargeTxn
  parseJSON (String "refund")           = pure RefundTxn
  parseJSON (String "adjustment")       = pure AdjustmentTxn
  parseJSON (String "application_fee")  = pure ApplicationFeeTxn
  parseJSON (String "application_fee_refund") = pure ApplicationFeeRefundTxn
  parseJSON (String "transfer")         = pure TransferTxn
  parseJSON (String "transfer_cancel")  = pure TransferCancelTxn
  parseJSON (String "transfer_failure") = pure TransferFailureTxn
  parseJSON (String "payment")          = pure PaymentTxn
  parseJSON (String "payment_failure_refund") = pure PaymentFailureRefundTxn
  parseJSON (String "payment_refund")   = pure PaymentRefundTxn
  parseJSON (String "transfer_refund")  = pure TransferRefundTxn
  parseJSON (String "payout")           = pure PayoutTxn
  parseJSON (String "payout_cancel")    = pure PayoutCancelTxn
  parseJSON (String "payout_failure")   = pure PayoutFailureTxn
  parseJSON (String "validation")       = pure ValidationTxn
  parseJSON _                           = mzero

instance ToJSON TransactionType where
  toJSON ChargeTxn          = String "charge"
  toJSON RefundTxn          = String "refund"
  toJSON AdjustmentTxn      = String "adjustment"
  toJSON ApplicationFeeTxn  = String "application_fee"
  toJSON ApplicationFeeRefundTxn = String "application_fee_refund"
  toJSON TransferTxn        = String "transfer"
  toJSON TransferCancelTxn  = String "transfer_cancel"
  toJSON TransferFailureTxn = String "transfer_failure"
  toJSON PaymentTxn         = String "payment"
  toJSON PaymentFailureRefundTxn = String "payment_failure_refund"
  toJSON PaymentRefundTxn   = String "payment_refund"
  toJSON TransferRefundTxn  = String "transfer_refund"
  toJSON PayoutTxn          = String "payout"
  toJSON PayoutCancelTxn    = String "payout_cancel"
  toJSON PayoutFailureTxn   = String "payout_failure"
  toJSON ValidationTxn      = String "validation"

------------------------------------------------------------------------------
-- | `Event` Types
data EventType =
    AccountUpdatedEvent
  | AccountApplicationDeauthorizedEvent
  | AccountExternalCreatedEvent
  | AccountExternalDeletedEvent
  | AccountExternalUpdatedEvent
  | ApplicationFeeCreatedEvent
  | ApplicationFeeRefundedEvent
  | BalanceAvailableEvent
  | BitcoinReceiverCreatedEvent
  | BitcoinReceiverFilledEvent
  | BitcoinReceiverUpdatedEvent
  | BitcoinReceiverTransactionCreatedEvent
  | ChargeSucceededEvent
  | ChargeFailedEvent
  | ChargeRefundedEvent
  | ChargeCapturedEvent
  | ChargePendingEvent
  | ChargeUpdatedEvent
  | ChargeDisputeCreatedEvent
  | ChargeDisputeUpdatedEvent
  | ChargeDisputeClosedEvent
  | ChargeDisputeFundsWithdrawnEvent
  | ChargeDisputeFundsReinstatedEvent
  | ChargeRefundUpdatedEvent
  | CustomerCreatedEvent
  | CustomerUpdatedEvent
  | CustomerDeletedEvent
  | CustomerSourceCreatedEvent
  | CustomerSourceUpdatedEvent
  | CustomerSourceDeletedEvent
  | CustomerSubscriptionCreatedEvent
  | CustomerSubscriptionUpdatedEvent
  | CustomerSubscriptionDeletedEvent
  | CustomerSubscriptionTrialWillEndEvent
  | CustomerDiscountCreatedEvent
  | CustomerDiscountUpdatedEvent
  | CustomerDiscountDeletedEvent
  | InvoiceCreatedEvent
  | InvoiceUpdatedEvent
  | InvoicePaymentSucceededEvent
  | InvoicePaymentFailedEvent
  | InvoiceUpcomingEvent
  | InvoiceItemCreatedEvent
  | InvoiceItemUpdatedEvent
  | InvoiceItemDeletedEvent
  | PayoutCanceledEvent
  | PayoutCreatedEvent
  | PayoutFailedEvent
  | PayoutPaidEvent
  | PayoutUpdatedEvent
  | PlanCreatedEvent
  | PlanUpdatedEvent
  | PlanDeletedEvent
  | ProductCreatedEvent
  | ProductDeletedEvent
  | ProductUpdatedEvent
  | CouponCreatedEvent
  | CouponUpdatedEvent
  | CouponDeletedEvent
  | OrderCreatedEvent
  | OrderPaymentFailedEvent
  | OrderPaymentSucceededEvent
  | OrderUpdatedEvent
  | OrderReturnCreatedEvent
  | ReviewClosedEvent
  | ReviewOpenedEvent
  | SkuCreatedEvent
  | SkuDeletedEvent
  | SkuUpdatedEvent
  | SourceCanceledEvent
  | SourceChargeableEvent
  | SourceFailedEvent
  | SourceTransactionCreatedEvent
  | TransferCreatedEvent
  | TransferUpdatedEvent
  | TransferReversedEvent
  | PingEvent
  | UnknownEvent
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Event Types JSON Instance
instance FromJSON EventType where
   parseJSON (String "account.updated") = pure AccountUpdatedEvent
   parseJSON (String "account.application.deauthorized") = pure AccountApplicationDeauthorizedEvent
   parseJSON (String "account.external_account.created") = pure AccountExternalCreatedEvent
   parseJSON (String "account.external_account.deleted") = pure AccountExternalDeletedEvent
   parseJSON (String "account.external_account.updated") = pure AccountExternalUpdatedEvent
   parseJSON (String "application_fee.created") = pure ApplicationFeeCreatedEvent
   parseJSON (String "application_fee.refunded") = pure ApplicationFeeRefundedEvent
   parseJSON (String "balance.available") = pure BalanceAvailableEvent
   parseJSON (String "bitcoin.receiver.created") = pure BitcoinReceiverCreatedEvent
   parseJSON (String "bitcoin.receiver.filled") = pure BitcoinReceiverFilledEvent
   parseJSON (String "bitcoin.receiver.updated") = pure BitcoinReceiverUpdatedEvent
   parseJSON (String "bitcoin.receiver.transaction.created") = pure BitcoinReceiverTransactionCreatedEvent
   parseJSON (String "charge.succeeded") = pure ChargeSucceededEvent
   parseJSON (String "chage.failed") = pure ChargeFailedEvent
   parseJSON (String "charge.refunded") = pure ChargeRefundedEvent
   parseJSON (String "charge.captured") = pure ChargeCapturedEvent
   parseJSON (String "charge.updated") = pure ChargeUpdatedEvent
   parseJSON (String "charge.pending") = pure ChargePendingEvent
   parseJSON (String "charge.dispute.created") = pure ChargeDisputeCreatedEvent
   parseJSON (String "charge.dispute.updated") = pure ChargeDisputeUpdatedEvent
   parseJSON (String "charge.dispute.closed") = pure ChargeDisputeClosedEvent
   parseJSON (String "charge.dispute.funds_withdrawn") = pure ChargeDisputeFundsWithdrawnEvent
   parseJSON (String "charge.dispute.funds_reinstated") = pure ChargeDisputeFundsReinstatedEvent
   parseJSON (String "charge.refund.updated") = pure ChargeRefundUpdatedEvent
   parseJSON (String "customer.created") = pure CustomerCreatedEvent
   parseJSON (String "customer.updated") = pure CustomerUpdatedEvent
   parseJSON (String "customer.deleted") = pure CustomerDeletedEvent
   parseJSON (String "customer.source.created") = pure CustomerSourceCreatedEvent
   parseJSON (String "customer.source.updated") = pure CustomerSourceUpdatedEvent
   parseJSON (String "customer.source.deleted") = pure CustomerSourceDeletedEvent
   parseJSON (String "customer.subscription.created") = pure CustomerSubscriptionCreatedEvent
   parseJSON (String "customer.subscription.updated") = pure CustomerSubscriptionUpdatedEvent
   parseJSON (String "customer.subscription.deleted") = pure CustomerSubscriptionDeletedEvent
   parseJSON (String "customer.subscription.trial_will_end") = pure CustomerSubscriptionTrialWillEndEvent
   parseJSON (String "customer.discount.created") = pure CustomerDiscountCreatedEvent
   parseJSON (String "customer.discount.updated") = pure CustomerDiscountUpdatedEvent
   parseJSON (String "invoice.created") = pure InvoiceCreatedEvent
   parseJSON (String "invoice.updated") = pure InvoiceUpdatedEvent
   parseJSON (String "invoice.payment_succeeded") = pure InvoicePaymentSucceededEvent
   parseJSON (String "invoice.payment_failed") = pure InvoicePaymentFailedEvent
   parseJSON (String "invoice.upcoming") = pure InvoiceUpcomingEvent
   parseJSON (String "invoiceitem.created") = pure InvoiceItemCreatedEvent
   parseJSON (String "invoiceitem.updated") = pure InvoiceItemUpdatedEvent
   parseJSON (String "invoiceitem.deleted") = pure InvoiceItemDeletedEvent
   parseJSON (String "payout.canceled") = pure PayoutCanceledEvent
   parseJSON (String "payout.created") = pure PayoutCreatedEvent
   parseJSON (String "payout.failed") = pure PayoutFailedEvent
   parseJSON (String "payout.paid") = pure PayoutPaidEvent
   parseJSON (String "payout.updated") = pure PayoutUpdatedEvent
   parseJSON (String "plan.created") = pure PlanCreatedEvent
   parseJSON (String "plan.updated") = pure PlanUpdatedEvent
   parseJSON (String "plan.deleted") = pure PlanDeletedEvent
   parseJSON (String "product.created") = pure ProductCreatedEvent
   parseJSON (String "product.deleted") = pure ProductDeletedEvent
   parseJSON (String "product.updated") = pure ProductUpdatedEvent
   parseJSON (String "coupon.created") = pure CouponCreatedEvent
   parseJSON (String "coupon.updated") = pure CouponUpdatedEvent
   parseJSON (String "coupon.deleted") = pure CouponDeletedEvent
   parseJSON (String "order.created") = pure OrderCreatedEvent
   parseJSON (String "order.payment_failed") = pure OrderPaymentFailedEvent
   parseJSON (String "order.payment_succeeded") = pure OrderPaymentSucceededEvent
   parseJSON (String "order.updated") = pure OrderUpdatedEvent
   parseJSON (String "order_return.created") = pure OrderReturnCreatedEvent
   parseJSON (String "review.closed") = pure ReviewClosedEvent
   parseJSON (String "review.opened") = pure ReviewOpenedEvent
   parseJSON (String "sku.created") = pure SkuCreatedEvent
   parseJSON (String "sku.deleted") = pure SkuDeletedEvent
   parseJSON (String "sku.updated") = pure SkuUpdatedEvent
   parseJSON (String "source.canceled") = pure SourceCanceledEvent
   parseJSON (String "source.chargeable") = pure SourceChargeableEvent
   parseJSON (String "source.failed") = pure SourceFailedEvent
   parseJSON (String "source.transaction.created") = pure SourceTransactionCreatedEvent
   parseJSON (String "transfer.created") = pure TransferCreatedEvent
   parseJSON (String "transfer.updated") = pure TransferUpdatedEvent
   parseJSON (String "transfer.reversed") = pure TransferReversedEvent
   parseJSON (String "ping") = pure PingEvent
   parseJSON _ = pure UnknownEvent

------------------------------------------------------------------------------
-- | `EventId` of an `Event`
newtype EventId = EventId Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | EventData
data EventData =
    TransferEvent Transfer
  | AccountEvent Account
  | AccountApplicationEvent ConnectApp
  | ApplicationFeeEvent ApplicationFee
  | InvoiceEvent Invoice
  | PayoutEvent Payout
  | PlanEvent Plan
  | CouponEvent Coupon
  | BalanceEvent Balance
  | BitcoinReceiverEvent BitcoinReceiver
  | ChargeEvent Charge
  | RefundEvent Refund
  | DisputeEvent Dispute
  | CustomerEvent Customer
  | SubscriptionEvent Subscription
  | CustomerSourceEvent (Either Card BankAccount)
  | SourceEvent Source
  | OrderEvent Order
  | ProductEvent Product
  | ReviewEvent Review
  | SkuEvent Sku
  | DiscountEvent Discount
  | InvoiceItemEvent InvoiceItem
  | UnknownEventData
  | Ping
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Event` Object
data Event = Event {
      eventId              :: Maybe EventId
    , eventCreated         :: UTCTime
    , eventLiveMode        :: Bool
    , eventType            :: EventType
    , eventData            :: EventData
    , eventObject          :: Text
    , eventPendingWebHooks :: Int
    , eventRequest         :: Maybe EventRequest
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | request identifier object for an `Event`
data EventRequest = EventRequest {
      eventRequestId :: Text
    , eventRequestIdempotencyKey :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

-- | JSON instance for an `EventRequest`
instance FromJSON EventRequest where
    parseJSON =
      withObject "request" $ \o ->
        EventRequest <$> o .: "id"
                     <*> o .: "idempotency_key"

------------------------------------------------------------------------------
-- | JSON Instance for `Event`
instance FromJSON Event where
   parseJSON (Object o) = do
     eventId <- fmap EventId <$> o .:? "id"
     eventCreated <- fromSeconds <$> o .: "created"
     eventLiveMode <- o .: "livemode"
     eventType <- o .: "type"
     String etype <- o .: "type"
     obj <- o .: "data"
     eventData <-
       case etype of
        "account.updated" -> AccountEvent <$> obj .: "object"
        "account.application.deauthorized" -> AccountApplicationEvent <$> obj .: "object"
        "application_fee.created" -> ApplicationFeeEvent <$> obj .: "object"
        "application_fee.refunded" -> ApplicationFeeEvent <$> obj .: "object"
        "balance.available" -> BalanceEvent <$> obj .: "object"
        "bitcoin.receiver.created" -> BitcoinReceiverEvent <$> obj .: "object"
        "bitcoin.receiver.filled" -> BitcoinReceiverEvent <$> obj .: "object"
        "bitcoin.receiver.updated" -> BitcoinReceiverEvent <$> obj .: "object"
        "bitcoin.receiver.transaction.created" -> BitcoinReceiverEvent <$> obj .: "object"
        "charge.succeeded" -> ChargeEvent <$> obj .: "object"
        "charge.failed" -> ChargeEvent <$> obj .: "object"
        "charge.refunded" -> ChargeEvent <$> obj .: "object"
        "charge.captured" -> ChargeEvent <$> obj .: "object"
        "charge.updated" -> ChargeEvent <$> obj .: "object"
        "charge.pending" -> ChargeEvent <$> obj .: "object"
        "charge.dispute.created" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.updated" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.closed" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.funds_withdrawn" -> DisputeEvent <$> obj .: "object"
        "charge.dispute.funds_reinstated" -> DisputeEvent <$> obj .: "object"
        "charge.refund.updated" -> RefundEvent <$> obj .: "object"
        "customer.created" -> CustomerEvent <$> obj .: "object"
        "customer.updated" -> CustomerEvent <$> obj .: "object"
        "customer.deleted" -> CustomerEvent <$> obj .: "object"
        "customer.source.created" -> do
                            rawObj <- obj .: "object"
                            case H.lookup "object" (rawObj :: H.HashMap String Value) of
                              Nothing -> pure UnknownEventData
                              Just (String "card") -> CustomerSourceEvent <$> Left <$> obj .: "object"
                              Just (String "bank_account") -> CustomerSourceEvent <$> Right <$> obj .: "object"
                              Just _ -> pure UnknownEventData
        "customer.source.deleted" -> SourceEvent <$> obj .: "object"
        "customer.source.updated" -> SourceEvent <$> obj .: "object"
        "customer.subscription.created" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.updated" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.deleted" -> SubscriptionEvent <$> obj .: "object"
        "customer.subscription.trial_will_end" -> SubscriptionEvent <$> obj .: "object"
        "customer.discount.created" -> DiscountEvent <$> obj .: "object"
        "customer.discount.updated" -> DiscountEvent <$> obj .: "object"
        "customer.discount.deleted" -> DiscountEvent <$> obj .: "object"
        "invoice.created" -> InvoiceEvent <$> obj .: "object"
        "invoice.updated" -> InvoiceEvent <$> obj .: "object"
        "invoice.payment_succeeded" -> InvoiceEvent <$> obj .: "object"
        "invoice.payment_failed" -> InvoiceEvent <$> obj .: "object"
        "invoice.upcoming" -> InvoiceEvent <$> obj .: "object"
        "invoiceitem.created" -> InvoiceItemEvent <$> obj .: "object"
        "invoiceitem.updated" -> InvoiceItemEvent <$> obj .: "object"
        "invoiceitem.deleted" -> InvoiceItemEvent <$> obj .: "object"
        "order.created" -> OrderEvent <$> obj .: "object"
        "order.payment_failed" -> OrderEvent <$> obj .: "object"
        "order.payment_succeeded" -> OrderEvent <$> obj .: "object"
        "order.updated" -> OrderEvent <$> obj .: "object"
        "payout.canceled" -> PayoutEvent <$> obj .: "object"
        "payout.created" -> PayoutEvent <$> obj .: "object"
        "payout.failed" -> PayoutEvent <$> obj .: "object"
        "payout.paid" -> PayoutEvent <$> obj .: "object"
        "payout.updated" -> PayoutEvent <$> obj .: "object"
        "plan.created" -> PlanEvent <$> obj .: "object"
        "plan.updated" -> PlanEvent <$> obj .: "object"
        "plan.deleted" -> PlanEvent <$> obj .: "object"
        "product.created" -> ProductEvent <$> obj .: "object"
        "product.deleted" -> ProductEvent <$> obj .: "object"
        "product.updated" -> ProductEvent <$> obj .: "object"
        "review.opened" -> ReviewEvent <$> obj .: "object"
        "review.closed" -> ReviewEvent <$> obj .: "object"
        "sku.created" -> SkuEvent <$> obj .: "object"
        "sku.deleted" -> SkuEvent <$> obj .: "object"
        "sku.updated" -> SkuEvent <$> obj .: "object"
        "coupon.created" -> CouponEvent <$> obj .: "object"
        "coupon.updated" -> CouponEvent <$> obj .: "object"
        "coupon.deleted" -> CouponEvent <$> obj .: "object"
        "source.canceled" -> SourceEvent <$> obj .: "object"
        "source.chargeable" -> SourceEvent <$> obj .: "object"
        "source.failed" -> SourceEvent <$> obj .: "object"
        "transfer.created" -> TransferEvent <$> obj .: "object"
        "transfer.updated" -> TransferEvent <$> obj .: "object"
        "transfer.reversed" -> TransferEvent <$> obj .: "object"
        "ping" -> pure Ping
        _        -> pure UnknownEventData
     eventObject <- o .: "object"
     eventPendingWebHooks <- o .: "pending_webhooks"
     eventRequest <- o .:? "request"
     return Event {..}
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Connect Application
data ConnectApp = ConnectApp {
      connectAppId     :: Maybe Text
    , connectAppObject :: Text
    , connectAppName   :: Text
  } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Connect Application JSON instance
instance FromJSON ConnectApp where
   parseJSON =
     withObject "connect_application" $ \o ->
       ConnectApp <$> o .:? "id"
                  <*> o .: "object"
                  <*> o .: "name"

------------------------------------------------------------------------------
-- | `TokenId` of a `Token`
newtype TokenId = TokenId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Type of `Token`
data TokenType = TokenCard
               | TokenBankAccount
                 deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `TokenType`
instance FromJSON TokenType where
   parseJSON (String "bank_account") = pure TokenBankAccount
   parseJSON (String "card") = pure TokenCard
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | `Token` Object
data Token a = Token {
      tokenId       :: TokenId
    , tokenLiveMode :: Bool
    , tokenCreated  :: UTCTime
    , tokenUsed     :: Bool
    , tokenObject   :: Text
    , tokenType     :: TokenType
    , tokenData     :: a
} deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `Token`
instance FromJSON a => FromJSON (Token a) where
   parseJSON =
       withObject "token" $ \o -> do
         tokenId <- TokenId <$> o .: "id"
         Bool tokenLiveMode <- o .: "livemode"
         tokenCreated <- fromSeconds <$> o .: "created"
         Bool tokenUsed <- o .: "used"
         String tokenObject <- o .: "object"
         String typ <- o .: "type"
         tokenType <- pure $ case typ of
                               "card"         -> TokenCard
                               "bank_account" -> TokenBankAccount
                               _ -> error "unspecified type"
         tokenData <-
             case typ of
               "bank_account" -> o .: "bank_account"
               "card"         -> o .: "card"
               _              -> mzero
         return Token {..}

------------------------------------------------------------------------------
-- | Generic handling of Stripe JSON arrays
data StripeList a = StripeList {
      list       :: [a]
    , stripeUrl  :: Text
    , object     :: Text
    , totalCount :: Maybe Int
    , hasMore    :: Bool
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `StripeList`
instance FromJSON a => FromJSON (StripeList a) where
    parseJSON =
      withObject "stripe_list" $ \o ->
        StripeList <$> o .:  "data"
                   <*> o .:  "url"
                   <*> o .:  "object"
                   <*> o .:? "total_count"
                   <*> o .:? "has_more" .!= False

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype Limit = Limit Int
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype StartingAfter a = StartingAfter a
 deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Pagination Option for `StripeList`
newtype EndingBefore a = EndingBefore a
 deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON returned from a `Stripe` deletion request
data StripeDeleteResult = StripeDeleteResult {
      deleted   :: Bool
    , deletedId :: Maybe Text
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | JSON Instance for `StripeDeleteResult`
instance FromJSON StripeDeleteResult where
   parseJSON (Object o) =
       StripeDeleteResult <$> o .: "deleted"
                          <*> o .:? "id"
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Type of MetaData for use on `Stripe` objects
newtype MetaData = MetaData [ (Text,Text) ]
  deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON MetaData where
  parseJSON j = (MetaData . H.toList) <$> (parseJSON j)

------------------------------------------------------------------------------
-- | Type of Expansion Parameters for use on `Stripe` objects
newtype ExpandParams = ExpandParams { getExpandParams :: [Text] }
  deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Generic ID for use in constructing API Calls
type ID    = Text

------------------------------------------------------------------------------
-- | Generic URL for use in constructing API Calls
type URL   = Text

------------------------------------------------------------------------------
-- | a cardholder's full name
newtype Name  = Name { getName :: Text }
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Name where
  parseJSON v = Name <$> parseJSON v

------------------------------------------------------------------------------
-- | a plan name
newtype PlanName  = PlanName { getPlanName :: Text }
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON PlanName where
  parseJSON v = PlanName <$> parseJSON v

------------------------------------------------------------------------------
-- | Generic Description for use in constructing API Calls
newtype Description = Description Text
   deriving (Read, Show, Eq, Ord, Data, Typeable)

instance FromJSON Description where
  parseJSON v = Description <$> parseJSON v

------------------------------------------------------------------------------
-- | Generic `Quantity` type to be used with `Customer`,
-- `Subscription` and `InvoiceLineItem` API requests
newtype Quantity = Quantity Int deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Prorate
newtype Prorate = Prorate Bool deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | A flag that if set to true will delay the cancellation of the
-- subscription until the end of the current period.
newtype AtPeriodEnd = AtPeriodEnd Bool deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Email` associated with a `Customer` or `Charge`
newtype Email = Email Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Email` to send receipt to
newtype ReceiptEmail = ReceiptEmail Text deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Stripe supports 138 currencies
data Currency =
    AED -- ^  United Arab Emirates Dirham
  | AFN -- ^  Afghan Afghani
  | ALL -- ^  Albanian Lek
  | AMD -- ^  Armenian Dram
  | ANG -- ^  Netherlands Antillean Gulden
  | AOA -- ^  Angolan Kwanza
  | ARS -- ^  Argentine Peso
  | AUD -- ^  Australian Dollar
  | AWG -- ^  Aruban Florin
  | AZN -- ^  Azerbaijani Manat
  | BAM -- ^  Bosnia & Herzegovina Convertible Mark
  | BBD -- ^  Barbadian Dollar
  | BDT -- ^  Bangladeshi Taka
  | BGN -- ^  Bulgarian Lev
  | BIF -- ^  Burundian Franc
  | BMD -- ^  Bermudian Dollar
  | BND -- ^  Brunei Dollar
  | BOB -- ^  Bolivian Boliviano
  | BRL -- ^  Brazilian Real
  | BSD -- ^  Bahamian Dollar
  | BWP -- ^  Botswana Pula
  | BZD -- ^  Belize Dollar
  | CAD -- ^  Canadian Dollar
  | CDF -- ^  Congolese Franc
  | CHF -- ^  Swiss Franc
  | CLP -- ^  Chilean Peso
  | CNY -- ^  Chinese Renminbi Yuan
  | COP -- ^  Colombian Peso
  | CRC -- ^  Costa Rican Colón
  | CVE -- ^  Cape Verdean Escudo
  | CZK -- ^  Czech Koruna
  | DJF -- ^  Djiboutian Franc
  | DKK -- ^  Danish Krone
  | DOP -- ^  Dominican Peso
  | DZD -- ^  Algerian Dinar
  | EEK -- ^  Estonian Kroon
  | EGP -- ^  Egyptian Pound
  | ETB -- ^  Ethiopian Birr
  | EUR -- ^  Euro
  | FJD -- ^  Fijian Dollar
  | FKP -- ^  Falkland Islands Pound
  | GBP -- ^  British Pound
  | GEL -- ^  Georgian Lari
  | GIP -- ^  Gibraltar Pound
  | GMD -- ^  Gambian Dalasi
  | GNF -- ^  Guinean Franc
  | GTQ -- ^  Guatemalan Quetzal
  | GYD -- ^  Guyanese Dollar
  | HKD -- ^  Hong Kong Dollar
  | HNL -- ^  Honduran Lempira
  | HRK -- ^  Croatian Kuna
  | HTG -- ^  Haitian Gourde
  | HUF -- ^  Hungarian Forint
  | IDR -- ^  Indonesian Rupiah
  | ILS -- ^  Israeli New Sheqel
  | INR -- ^  Indian Rupee
  | ISK -- ^  Icelandic Króna
  | JMD -- ^  Jamaican Dollar
  | JPY -- ^  Japanese Yen
  | KES -- ^  Kenyan Shilling
  | KGS -- ^  Kyrgyzstani Som
  | KHR -- ^  Cambodian Riel
  | KMF -- ^  Comorian Franc
  | KRW -- ^  South Korean Won
  | KYD -- ^  Cayman Islands Dollar
  | KZT -- ^  Kazakhstani Tenge
  | LAK -- ^  Lao Kip
  | LBP -- ^  Lebanese Pound
  | LKR -- ^  Sri Lankan Rupee
  | LRD -- ^  Liberian Dollar
  | LSL -- ^  Lesotho Loti
  | LTL -- ^  Lithuanian Litas
  | LVL -- ^  Latvian Lats
  | MAD -- ^  Moroccan Dirham
  | MDL -- ^  Moldovan Leu
  | MGA -- ^  Malagasy Ariary
  | MKD -- ^  Macedonian Denar
  | MNT -- ^  Mongolian Tögrög
  | MOP -- ^  Macanese Pataca
  | MRO -- ^  Mauritanian Ouguiya
  | MUR -- ^  Mauritian Rupee
  | MVR -- ^  Maldivian Rufiyaa
  | MWK -- ^  Malawian Kwacha
  | MXN -- ^  Mexican Peso
  | MYR -- ^  Malaysian Ringgit
  | MZN -- ^  Mozambican Metical
  | NAD -- ^  Namibian Dollar
  | NGN -- ^  Nigerian Naira
  | NIO -- ^  Nicaraguan Córdoba
  | NOK -- ^  Norwegian Krone
  | NPR -- ^  Nepalese Rupee
  | NZD -- ^  New Zealand Dollar
  | PAB -- ^  Panamanian Balboa
  | PEN -- ^  Peruvian Nuevo Sol
  | PGK -- ^  Papua New Guinean Kina
  | PHP -- ^  Philippine Peso
  | PKR -- ^  Pakistani Rupee
  | PLN -- ^  Polish Złoty
  | PYG -- ^  Paraguayan Guaraní
  | QAR -- ^  Qatari Riyal
  | RON -- ^  Romanian Leu
  | RSD -- ^  Serbian Dinar
  | RUB -- ^  Russian Ruble
  | RWF -- ^  Rwandan Franc
  | SAR -- ^  Saudi Riyal
  | SBD -- ^  Solomon Islands Dollar
  | SCR -- ^  Seychellois Rupee
  | SEK -- ^  Swedish Krona
  | SGD -- ^  Singapore Dollar
  | SHP -- ^  Saint Helenian Pound
  | SLL -- ^  Sierra Leonean Leone
  | SOS -- ^  Somali Shilling
  | SRD -- ^  Surinamese Dollar
  | STD -- ^  São Tomé and Príncipe Dobra
  | SVC -- ^  Salvadoran Colón
  | SZL -- ^  Swazi Lilangeni
  | THB -- ^  Thai Baht
  | TJS -- ^  Tajikistani Somoni
  | TOP -- ^  Tongan Paʻanga
  | TRY -- ^  Turkish Lira
  | TTD -- ^  Trinidad and Tobago Dollar
  | TWD -- ^  New Taiwan Dollar
  | TZS -- ^  Tanzanian Shilling
  | UAH -- ^  Ukrainian Hryvnia
  | UGX -- ^  Ugandan Shilling
  | USD -- ^  United States Dollar
  | UYU -- ^  Uruguayan Peso
  | UZS -- ^  Uzbekistani Som
  | VND -- ^  Vietnamese Đồng
  | VUV -- ^  Vanuatu Vatu
  | WST -- ^  Samoan Tala
  | XAF -- ^  Central African Cfa Franc
  | XCD -- ^  East Caribbean Dollar
  | XOF -- ^  West African Cfa Franc
  | XPF -- ^  Cfp Franc
  | YER -- ^  Yemeni Rial
  | ZAR -- ^  South African Rand
  | ZMW -- ^  Zambian Kwacha
  | UnknownCurrency -- ^  Unknown Currency
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | `Currency` JSON instances
instance FromJSON Currency where
   parseJSON (String "aed") = pure AED
   parseJSON (String "afn") = pure AFN
   parseJSON (String "all") = pure ALL
   parseJSON (String "amd") = pure AMD
   parseJSON (String "ang") = pure ANG
   parseJSON (String "aoa") = pure AOA
   parseJSON (String "ars") = pure ARS
   parseJSON (String "aud") = pure AUD
   parseJSON (String "awg") = pure AWG
   parseJSON (String "azn") = pure AZN
   parseJSON (String "bam") = pure BAM
   parseJSON (String "bbd") = pure BBD
   parseJSON (String "bdt") = pure BDT
   parseJSON (String "bgn") = pure BGN
   parseJSON (String "bif") = pure BIF
   parseJSON (String "bmd") = pure BMD
   parseJSON (String "bnd") = pure BND
   parseJSON (String "bob") = pure BOB
   parseJSON (String "brl") = pure BRL
   parseJSON (String "bsd") = pure BSD
   parseJSON (String "bwp") = pure BWP
   parseJSON (String "bzd") = pure BZD
   parseJSON (String "cad") = pure CAD
   parseJSON (String "cdf") = pure CDF
   parseJSON (String "chf") = pure CHF
   parseJSON (String "clp") = pure CLP
   parseJSON (String "cny") = pure CNY
   parseJSON (String "cop") = pure COP
   parseJSON (String "crc") = pure CRC
   parseJSON (String "cve") = pure CVE
   parseJSON (String "czk") = pure CZK
   parseJSON (String "djf") = pure DJF
   parseJSON (String "dkk") = pure DKK
   parseJSON (String "dop") = pure DOP
   parseJSON (String "dzd") = pure DZD
   parseJSON (String "eek") = pure EEK
   parseJSON (String "egp") = pure EGP
   parseJSON (String "etb") = pure ETB
   parseJSON (String "eur") = pure EUR
   parseJSON (String "fjd") = pure FJD
   parseJSON (String "fkp") = pure FKP
   parseJSON (String "gbp") = pure GBP
   parseJSON (String "gel") = pure GEL
   parseJSON (String "gip") = pure GIP
   parseJSON (String "gmd") = pure GMD
   parseJSON (String "gnf") = pure GNF
   parseJSON (String "gtq") = pure GTQ
   parseJSON (String "gyd") = pure GYD
   parseJSON (String "hkd") = pure HKD
   parseJSON (String "hnl") = pure HNL
   parseJSON (String "hrk") = pure HRK
   parseJSON (String "htg") = pure HTG
   parseJSON (String "huf") = pure HUF
   parseJSON (String "idr") = pure IDR
   parseJSON (String "ils") = pure ILS
   parseJSON (String "inr") = pure INR
   parseJSON (String "isk") = pure ISK
   parseJSON (String "jmd") = pure JMD
   parseJSON (String "jpy") = pure JPY
   parseJSON (String "kes") = pure KES
   parseJSON (String "kgs") = pure KGS
   parseJSON (String "khr") = pure KHR
   parseJSON (String "kmf") = pure KMF
   parseJSON (String "krw") = pure KRW
   parseJSON (String "kyd") = pure KYD
   parseJSON (String "kzt") = pure KZT
   parseJSON (String "lak") = pure LAK
   parseJSON (String "lbp") = pure LBP
   parseJSON (String "lkr") = pure LKR
   parseJSON (String "lrd") = pure LRD
   parseJSON (String "lsl") = pure LSL
   parseJSON (String "ltl") = pure LTL
   parseJSON (String "lvl") = pure LVL
   parseJSON (String "mad") = pure MAD
   parseJSON (String "mdl") = pure MDL
   parseJSON (String "mga") = pure MGA
   parseJSON (String "mkd") = pure MKD
   parseJSON (String "mnt") = pure MNT
   parseJSON (String "mop") = pure MOP
   parseJSON (String "mro") = pure MRO
   parseJSON (String "mur") = pure MUR
   parseJSON (String "mvr") = pure MVR
   parseJSON (String "mwk") = pure MWK
   parseJSON (String "mxn") = pure MXN
   parseJSON (String "myr") = pure MYR
   parseJSON (String "mzn") = pure MZN
   parseJSON (String "nad") = pure NAD
   parseJSON (String "ngn") = pure NGN
   parseJSON (String "nio") = pure NIO
   parseJSON (String "nok") = pure NOK
   parseJSON (String "npr") = pure NPR
   parseJSON (String "nzd") = pure NZD
   parseJSON (String "pab") = pure PAB
   parseJSON (String "pen") = pure PEN
   parseJSON (String "pgk") = pure PGK
   parseJSON (String "php") = pure PHP
   parseJSON (String "pkr") = pure PKR
   parseJSON (String "pln") = pure PLN
   parseJSON (String "pyg") = pure PYG
   parseJSON (String "qar") = pure QAR
   parseJSON (String "ron") = pure RON
   parseJSON (String "rsd") = pure RSD
   parseJSON (String "rub") = pure RUB
   parseJSON (String "rwf") = pure RWF
   parseJSON (String "sar") = pure SAR
   parseJSON (String "sbd") = pure SBD
   parseJSON (String "scr") = pure SCR
   parseJSON (String "sek") = pure SEK
   parseJSON (String "sgd") = pure SGD
   parseJSON (String "shp") = pure SHP
   parseJSON (String "sll") = pure SLL
   parseJSON (String "sos") = pure SOS
   parseJSON (String "srd") = pure SRD
   parseJSON (String "std") = pure STD
   parseJSON (String "svc") = pure SVC
   parseJSON (String "szl") = pure SZL
   parseJSON (String "thb") = pure THB
   parseJSON (String "tjs") = pure TJS
   parseJSON (String "top") = pure TOP
   parseJSON (String "try") = pure TRY
   parseJSON (String "ttd") = pure TTD
   parseJSON (String "twd") = pure TWD
   parseJSON (String "tzs") = pure TZS
   parseJSON (String "uah") = pure UAH
   parseJSON (String "ugx") = pure UGX
   parseJSON (String "usd") = pure USD
   parseJSON (String "uyu") = pure UYU
   parseJSON (String "uzs") = pure UZS
   parseJSON (String "vnd") = pure VND
   parseJSON (String "vuv") = pure VUV
   parseJSON (String "wst") = pure WST
   parseJSON (String "xaf") = pure XAF
   parseJSON (String "xcd") = pure XCD
   parseJSON (String "xof") = pure XOF
   parseJSON (String "xpf") = pure XPF
   parseJSON (String "yer") = pure YER
   parseJSON (String "zar") = pure ZAR
   parseJSON (String "zmw") = pure ZMW
   parseJSON _ = pure UnknownCurrency

------------------------------------------------------------------------------
-- | BTC ReceiverObject
data BitcoinReceiver = BitcoinReceiver {
       btcId                    :: BitcoinReceiverId
    ,  btcObject                :: Text
    ,  btcCreated               :: UTCTime
    ,  btcLiveMode              :: Bool
    ,  btcActive                :: Bool
    ,  btcAmount                :: Integer
    ,  btcAmountReceived        :: Integer
    ,  btcBitcoinAmount         :: Integer
    ,  btcBitcoinAmountReceived :: Integer
    ,  btcBitcoinUri            :: Text
    ,  btcCurrency              :: Currency
    ,  btcFilled                :: Bool
    ,  btcInboundAddress        :: Text
    ,  btcUncapturedFunds       :: Bool
    ,  btcDescription           :: Maybe Text
    ,  btcEmail                 :: Text
    ,  btcMetadata              :: MetaData
    ,  btcRefundAddress         :: Maybe Text
    ,  btcTransactions          :: Maybe Transactions
    ,  btcPayment               :: Maybe PaymentId
    ,  btcCustomer              :: Maybe CustomerId
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | FromJSON for `BitcoinReceiver`
instance FromJSON BitcoinReceiver where
   parseJSON =
     withObject "bitcoin_receiver" $ \o ->
       BitcoinReceiver <$> (BitcoinReceiverId <$> o .: "id")
                       <*> o .: "object"
                       <*> (fromSeconds <$> o .: "created")
                       <*> o .: "livemode"
                       <*> o .: "active"
                       <*> o .: "amount"
                       <*> o .: "amount_received"
                       <*> o .: "bitcoin_amount"
                       <*> o .: "bitcoin_amount_received"
                       <*> o .: "bitcoin_uri"
                       <*> o .: "currency"
                       <*> o .: "filled"
                       <*> o .: "inbound_address"
                       <*> o .: "uncaptured_funds"
                       <*> o .:? "description"
                       <*> o .: "email"
                       <*> o .: "metadata"
                       <*> o .:? "refund_address"
                       <*> o .:? "transactions"
                       <*> (fmap PaymentId <$> o .:? "payment")
                       <*> (fmap CustomerId <$> o .:? "customer")

------------------------------------------------------------------------------
-- | Bitcoin Transactions
data Transactions = Transactions {
      transactionsObject     :: Text
    , transactionsTotalCount :: Integer
    , transactionsHasMore    :: Bool
    , transactionsURL        :: Text
    , transactions           :: [BitcoinTransaction]
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | Bitcoin Transactions data
instance FromJSON Transactions where
   parseJSON =
     withObject "transactions" $ \o ->
       Transactions <$> o .: "object"
                    <*> o .: "total_count"
                    <*> o .: "has_more"
                    <*> o .: "url"
                    <*> o .: "data"

------------------------------------------------------------------------------
-- | Bitcoin Transaction
data BitcoinTransaction = BitcoinTransaction {
         btcTransactionId            :: BitcoinTransactionId
       , btcTransactionObject        :: Text
       , btcTransactionCreated       :: UTCTime
       , btcTransactionAmount        :: Integer
       , btcTransactionBitcoinAmount :: Integer
       , btcTransactionCurrency      :: Currency
       , btcTransactionReceiver      :: BitcoinReceiverId
    } deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | FromJSON BitcoinTransaction
instance FromJSON BitcoinTransaction where
   parseJSON =
     withObject "bitcoin_transaction" $ \o ->
       BitcoinTransaction <$> o .: "id"
                          <*> o .: "object"
                          <*> (fromSeconds <$> o .: "created")
                          <*> o .: "amount"
                          <*> o .: "bitcoin_amount"
                          <*> o .: "currency"
                          <*> o .: "receiver"

------------------------------------------------------------------------------
-- | BitcoinTransactionId
newtype BitcoinTransactionId = BitcoinTransactionId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | FromJSON BitcoinTransactionId
instance FromJSON BitcoinTransactionId where
    parseJSON = withText "bitcoin transaction id" (pure . BitcoinTransactionId)

------------------------------------------------------------------------------
-- | BTC ReceiverId
newtype BitcoinReceiverId = BitcoinReceiverId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | FromJSON for BitcoinReceiverId
instance FromJSON BitcoinReceiverId where
    parseJSON = withText "bitcoin receiver id" (pure . BitcoinReceiverId)

------------------------------------------------------------------------------
-- | BTC PaymentId
newtype PaymentId = PaymentId Text
    deriving (Read, Show, Eq, Ord, Data, Typeable)

------------------------------------------------------------------------------
-- | FromJSON for PaymentId
instance FromJSON PaymentId where
    parseJSON = withText "payment id" (pure . PaymentId)

------------------------------------------------------------------------------
-- | Show an amount accounting for zero currencies
--
-- https:\/\/support.stripe.com\/questions\/which-zero-decimal-currencies-does-stripe-support
showAmount
  :: Currency -- ^ `Currency`
  -> Int      -- ^ `Amount`
  -> String
showAmount cur amt =
  case cur of
   USD -> "$" ++ show2places (currencyDivisor cur amt)
   _   -> show2places (currencyDivisor cur amt) ++ " " ++ show cur
  where
    show2places v = showFFloat (Just 2) v ""

------------------------------------------------------------------------------
-- currency division funtion accounting for zero currencies
--
-- https:\/\/support.stripe.com\/questions\/which-zero-decimal-currencies-does-stripe-support
currencyDivisor
    :: Currency -- ^ `Currency`
    -> (Int -> Float) -- ^ function to convert amount to a float
currencyDivisor cur =
  case cur of
    BIF -> zeroCurrency
    CLP -> zeroCurrency
    DJF -> zeroCurrency
    GNF -> zeroCurrency
    JPY -> zeroCurrency
    KMF -> zeroCurrency
    KRW -> zeroCurrency
    MGA -> zeroCurrency
    PYG -> zeroCurrency
    RWF -> zeroCurrency
    VND -> zeroCurrency
    VUV -> zeroCurrency
    XAF -> zeroCurrency
    XOF -> zeroCurrency
    XPF -> zeroCurrency
    EUR -> hundred
    USD -> hundred
    _   -> error $ "please submit a patch to currencyDivisor for this currency: " ++ show cur
  where
    zeroCurrency = fromIntegral
    hundred v    = fromRat $ (fromIntegral v) % (100 :: Integer)
