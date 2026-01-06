{-# LANGUAGE OverloadedStrings #-}

module DebugMetricSpec where

import Test.Hspec
import Azimuth.Telemetry
import Data.IORef (writeIORef)
import Data.IORef

spec :: Spec
spec = describe "Debug Metric Tests" $ do
  beforeAll (writeIORef enableMetricSharing False) $ do
    it "should handle empty strings in metric names and units" $ do
      metric <- createMetricWithInitialValue "" "" 0.0
      metricName metric `shouldBe` ""
      metricUnit metric `shouldBe` ""
      
      recordMetric metric 0.1
      value <- metricValue metric
      putStrLn $ "Metric value after recording 0.1: " ++ show value
      putStrLn $ "Is NaN: " ++ show (isNaN value)
      
      recordMetric metric 0.2
      value2 <- metricValue metric
      putStrLn $ "Metric value after recording 0.2: " ++ show value2
      putStrLn $ "Is NaN: " ++ show (isNaN value2)
      
      value `shouldSatisfy` not . isNaN
      value2 `shouldSatisfy` not . isNaN