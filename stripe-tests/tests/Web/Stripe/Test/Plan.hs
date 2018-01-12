{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
module Web.Stripe.Test.Plan where

import           Data.Either   (Either(..), isRight)
import           Test.Hspec
import           Web.Stripe.Test.Util     (makePlanId)
import           Web.Stripe.Test.Prelude

import           Web.Stripe.Plan
import           Web.Stripe.Customer

planTests :: StripeSpec
planTests stripe = do
  describe "Plan tests" $ do
    it "Successfully creates a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        p <- createPlan planid
                        (Amount 0) -- free plan
                        USD
                        Month
                        (PlanName "sample plan")
        void $ deletePlan planid
        return p
      result `shouldSatisfy` isRight
    it "Successfully deletes a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        Plan { planId = pid } <-
          createPlan planid
          (Amount 0) -- free plan
          USD
          Month
          (PlanName "sample plan")
        void $ deletePlan pid
      result `shouldSatisfy` isRight
    it "Successfully updates a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        Plan { planId = pid } <-
          createPlan planid
          (Amount 0) -- free plan
          USD
          Month
          (PlanName "sample plan")
        r <- updatePlan pid
               -&- (PlanName "cookie")
               -&- (StatementDescriptor "test")
               -&- MetaData [("key","value")]
        void $ deletePlan pid
        return r
      result `shouldSatisfy` isRight
      let Right Plan { planMetaData = pm
                     , planName = pname
                     , planStatementDescriptor = pdesc
                     } = result
      pm `shouldBe` (MetaData [("key", "value")])
      pname `shouldBe` "cookie"
      pdesc `shouldBe` (Just $ StatementDescriptor "test")
    it "Successfully retrieves a Plan" $ do
      planid <- makePlanId
      result <- stripe $ do
        Plan { planId = pid } <-
          createPlan planid
          (Amount 0) -- free plan
          USD
          Month
          (PlanName "sample plan")
        r <- getPlan pid
        void $ deletePlan pid
        return r
      result `shouldSatisfy` isRight
    it "Successfully retrieves a list of Plans" $ do
      result <- stripe $ void $ getPlans
      result `shouldSatisfy` isRight
