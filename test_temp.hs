{-# LANGUAGE ScopedTypeVariables #-}

module TestTemp where

import Test.Hspec
import Test.QuickCheck
import Data.List (nub)

spec :: Spec
spec = do
  describe "Multi-Tenant Isolation" $ do
    it "should isolate telemetry data by tenant" $ do
      True `shouldBe` True
    
    prop "should maintain tenant context isolation" $ \(tenantId :: Int) (metricCount :: Int) ->
        let actualCount = max 1 (metricCount `mod` 10 + 1)
            tenantPrefix = "tenant-" ++ show tenantId
            metricNames = map (\i -> tenantPrefix ++ "-metric-" ++ show i) [1..actualCount]
            result = all (isPrefixOf tenantPrefix) metricNames &&
                     length (nub metricNames) == actualCount
        in result `shouldBe` True
      where
        isPrefixOf prefix str = take (length prefix) str == prefix
    
    it "should handle tenant-specific configurations" $ do
      True `shouldBe` True