{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TextHandlingSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Data.Text (pack, unpack, Text, null, length)
import qualified Data.Text as Text
import Data.Char (isAscii, isControl, isSpace)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Text Handling Tests" $ do
  
  -- 测试ASCII字符处理
  describe "ASCII Character Handling" $ do
    it "should handle ASCII characters in metric names" $ property $
      \asciiString ->
        let filteredString = filter isAscii (take 100 asciiString)
            asciiText = pack filteredString
            result = unsafePerformIO $ do
              metric <- createMetric asciiText "ascii-unit"
              return (metricName metric == asciiText)
        in result
    
    it "should handle ASCII characters in metric units" $ property $
      \asciiString ->
        let filteredString = filter isAscii (take 50 asciiString)
            asciiText = pack filteredString
            result = unsafePerformIO $ do
              metric <- createMetric "ascii-metric" asciiText
              return (metricUnit metric == asciiText)
        in result
    
    it "should handle ASCII characters in logger names" $ property $
      \asciiString ->
        let filteredString = filter isAscii (take 100 asciiString)
            asciiText = pack filteredString
            result = unsafePerformIO $ do
              logger <- createLogger asciiText Info
              return (loggerName logger == asciiText)
        in result
    
    it "should handle ASCII characters in span names" $ property $
      \asciiString ->
        let filteredString = filter isAscii (take 100 asciiString)
            asciiText = pack filteredString
            result = unsafePerformIO $ do
              span <- createSpan asciiText
              return (spanName span == asciiText)
        in result
    
    it "should handle ASCII characters in log messages" $ property $
      \asciiString ->
        let filteredString = filter isAscii (take 200 asciiString)
            asciiText = pack filteredString
            result = unsafePerformIO $ do
              logger <- createLogger "ascii-logger" Info
              result <- (try $ logMessage logger Info asciiText) :: IO (Either SomeException ())
              return (isSuccess result)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试Unicode字符处理
  describe "Unicode Character Handling" $ do
    it "should handle Unicode characters in metric names" $ property $
      \unicodeString ->
        let unicodeText = pack (take 100 unicodeString)
            result = unsafePerformIO $ do
              metric <- createMetric unicodeText "unicode-unit"
              return (metricName metric == unicodeText)
        in result
    
    it "should handle Unicode characters in metric units" $ property $
      \unicodeString ->
        let unicodeText = pack (take 50 unicodeString)
            result = unsafePerformIO $ do
              metric <- createMetric "unicode-metric" unicodeText
              return (metricUnit metric == unicodeText)
        in result
    
    it "should handle Unicode characters in logger names" $ property $
      \unicodeString ->
        let unicodeText = pack (take 100 unicodeString)
            result = unsafePerformIO $ do
              logger <- createLogger unicodeText Info
              return (loggerName logger == unicodeText)
        in result
    
    it "should handle Unicode characters in span names" $ property $
      \unicodeString ->
        let unicodeText = pack (take 100 unicodeString)
            result = unsafePerformIO $ do
              span <- createSpan unicodeText
              return (spanName span == unicodeText)
        in result
    
    it "should handle Unicode characters in log messages" $ property $
      \unicodeString ->
        let unicodeText = pack (take 200 unicodeString)
            result = unsafePerformIO $ do
              logger <- createLogger "unicode-logger" Info
              result <- (try $ logMessage logger Info unicodeText) :: IO (Either SomeException ())
              return (isSuccess result)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试控制字符处理
  describe "Control Character Handling" $ do
    it "should handle control characters in metric names" $ property $
      \controlString ->
        let controlText = pack (take 100 controlString)
            result = unsafePerformIO $ do
              metric <- createMetric controlText "control-unit"
              return (metricName metric == controlText)
        in result
    
    it "should handle control characters in metric units" $ property $
      \controlString ->
        let controlText = pack (take 50 controlString)
            result = unsafePerformIO $ do
              metric <- createMetric "control-metric" controlText
              return (metricUnit metric == controlText)
        in result
    
    it "should handle control characters in logger names" $ property $
      \controlString ->
        let controlText = pack (take 100 controlString)
            result = unsafePerformIO $ do
              logger <- createLogger controlText Info
              return (loggerName logger == controlText)
        in result
    
    it "should handle control characters in span names" $ property $
      \controlString ->
        let controlText = pack (take 100 controlString)
            result = unsafePerformIO $ do
              span <- createSpan controlText
              return (spanName span == controlText)
        in result
    
    it "should handle control characters in log messages" $ property $
      \controlString ->
        let controlText = pack (take 200 controlString)
            result = unsafePerformIO $ do
              logger <- createLogger "control-logger" Info
              result <- (try $ logMessage logger Info controlText) :: IO (Either SomeException ())
              return (isSuccess result)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试空字符串处理
  describe "Empty String Handling" $ do
    it "should handle empty metric names" $ do
      let result = unsafePerformIO $ do
            metric <- createMetric "" "empty-unit"
            return (metricName metric == "" && metricUnit metric == "empty-unit")
      result `shouldBe` True
    
    it "should handle empty metric units" $ do
      let result = unsafePerformIO $ do
            metric <- createMetric "empty-metric" ""
            return (metricName metric == "empty-metric" && metricUnit metric == "")
      result `shouldBe` True
    
    it "should handle empty logger names" $ do
      let result = unsafePerformIO $ do
            logger <- createLogger "" Info
            return (loggerName logger == "")
      result `shouldBe` True
    
    it "should handle empty span names" $ do
      let result = unsafePerformIO $ do
            span <- createSpan ""
            return (spanName span == "")
      result `shouldBe` True
    
    it "should handle empty log messages" $ do
      let isSuccess (Right _) = True
          isSuccess (Left _) = False
          result = unsafePerformIO $ do
            logger <- createLogger "empty-logger" Info
            result <- (try $ logMessage logger Info "") :: IO (Either SomeException ())
            return (isSuccess result)
      result `shouldBe` True
  
  -- 测试长字符串处理
  describe "Long String Handling" $ do
    it "should handle long metric names" $ property $
      \baseString ->
        let longString = take 10000 baseString
            longText = pack longString
            result = unsafePerformIO $ do
              metric <- createMetric longText "long-unit"
              return (metricName metric == longText)
        in result
    
    it "should handle long metric units" $ property $
      \baseString ->
        let longString = take 1000 baseString
            longText = pack longString
            result = unsafePerformIO $ do
              metric <- createMetric "long-metric" longText
              return (metricUnit metric == longText)
        in result
    
    it "should handle long logger names" $ property $
      \baseString ->
        let longString = take 10000 baseString
            longText = pack longString
            result = unsafePerformIO $ do
              logger <- createLogger longText Info
              return (loggerName logger == longText)
        in result
    
    it "should handle long span names" $ property $
      \baseString ->
        let longString = take 10000 baseString
            longText = pack longString
            result = unsafePerformIO $ do
              span <- createSpan longText
              return (spanName span == longText)
        in result
    
    it "should handle long log messages" $ property $
      \baseString ->
        let longString = take 100000 baseString
            longText = pack longString
            result = unsafePerformIO $ do
              logger <- createLogger "long-logger" Info
              result <- (try $ logMessage logger Info longText) :: IO (Either SomeException ())
              return (isSuccess result)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试特殊字符处理
  describe "Special Character Handling" $ do
    it "should handle special characters in metric names" $ do
      let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
          result = unsafePerformIO $ do
            metric <- createMetric (pack specialChars) "special-unit"
            return (metricName metric == pack specialChars)
      result `shouldBe` True
    
    it "should handle special characters in metric units" $ do
      let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
          result = unsafePerformIO $ do
            metric <- createMetric "special-metric" (pack specialChars)
            return (metricUnit metric == pack specialChars)
      result `shouldBe` True
    
    it "should handle special characters in logger names" $ do
      let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
          result = unsafePerformIO $ do
            logger <- createLogger (pack specialChars) Info
            return (loggerName logger == pack specialChars)
      result `shouldBe` True
    
    it "should handle special characters in span names" $ do
      let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
          result = unsafePerformIO $ do
            span <- createSpan (pack specialChars)
            return (spanName span == pack specialChars)
      result `shouldBe` True
    
    it "should handle special characters in log messages" $ do
      let specialChars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
          isSuccess (Right _) = True
          isSuccess (Left _) = False
          result = unsafePerformIO $ do
            logger <- createLogger "special-logger" Info
            result <- (try $ logMessage logger Info (pack specialChars)) :: IO (Either SomeException ())
            return (isSuccess result)
      result `shouldBe` True
  
  -- 测试空格和制表符处理
  describe "Whitespace Handling" $ do
    it "should handle whitespace in metric names" $ property $
      \baseString ->
        let whitespaceString = "  \t\n\r  " ++ take 50 baseString ++ "  \t\n\r  "
            whitespaceText = pack whitespaceString
            result = unsafePerformIO $ do
              metric <- createMetric whitespaceText "whitespace-unit"
              return (metricName metric == whitespaceText)
        in result
    
    it "should handle whitespace in metric units" $ property $
      \baseString ->
        let whitespaceString = "  \t\n\r  " ++ take 20 baseString ++ "  \t\n\r  "
            whitespaceText = pack whitespaceString
            result = unsafePerformIO $ do
              metric <- createMetric "whitespace-metric" whitespaceText
              return (metricUnit metric == whitespaceText)
        in result
    
    it "should handle whitespace in logger names" $ property $
      \baseString ->
        let whitespaceString = "  \t\n\r  " ++ take 50 baseString ++ "  \t\n\r  "
            whitespaceText = pack whitespaceString
            result = unsafePerformIO $ do
              logger <- createLogger whitespaceText Info
              return (loggerName logger == whitespaceText)
        in result
    
    it "should handle whitespace in span names" $ property $
      \baseString ->
        let whitespaceString = "  \t\n\r  " ++ take 50 baseString ++ "  \t\n\r  "
            whitespaceText = pack whitespaceString
            result = unsafePerformIO $ do
              span <- createSpan whitespaceText
              return (spanName span == whitespaceText)
        in result
    
    it "should handle whitespace in log messages" $ property $
      \baseString ->
        let whitespaceString = "  \t\n\r  " ++ take 100 baseString ++ "  \t\n\r  "
            whitespaceText = pack whitespaceString
            result = unsafePerformIO $ do
              logger <- createLogger "whitespace-logger" Info
              result <- (try $ logMessage logger Info whitespaceText) :: IO (Either SomeException ())
              return (isSuccess result)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试文本编码一致性
  describe "Text Encoding Consistency" $ do
    it "should maintain text encoding consistency across operations" $ property $
      \text ->
        let originalText = pack (take 100 text)
            result = unsafePerformIO $ do
              -- 创建度量
              metric <- createMetric originalText originalText
              
              -- 创建日志记录器
              logger <- createLogger originalText Info
              
              -- 创建span
              span <- createSpan originalText
              
              -- 记录日志消息
              logMessage logger Info originalText
              
              return (metricName metric == originalText &&
                      metricUnit metric == originalText &&
                      loggerName logger == originalText &&
                      spanName span == originalText)
        in result
    
    it "should handle text round-trip consistently" $ property $
      \text ->
        let originalText = pack (take 100 text)
            result = unsafePerformIO $ do
              -- 创建度量
              metric <- createMetric originalText "round-trip-unit"
              
              -- 获取度量名称
              let retrievedName = metricName metric
              
              -- 使用获取的名称创建新度量
              newMetric <- createMetric retrievedName "new-unit"
              
              return (metricName newMetric == originalText &&
                      metricName newMetric == retrievedName)
        in result