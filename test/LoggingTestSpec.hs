{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LoggingTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (pack, unpack)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (replicateM, replicateM_, when)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Logging Tests" $ do
  
  -- 1. 测试Logger创建
  describe "Logger Creation" $ do
    it "should create loggers with correct names and levels" $ do
      logger <- createLogger "test-logger" Info
      loggerName logger `shouldBe` "test-logger"
      loggerLevel logger `shouldBe` Info
    
    it "should create loggers with different levels" $ do
      debugLogger <- createLogger "debug-logger" Debug
      infoLogger <- createLogger "info-logger" Info
      warnLogger <- createLogger "warn-logger" Warn
      errorLogger <- createLogger "error-logger" Error
      
      loggerLevel debugLogger `shouldBe` Debug
      loggerLevel infoLogger `shouldBe` Info
      loggerLevel warnLogger `shouldBe` Warn
      loggerLevel errorLogger `shouldBe` Error
  
  -- 2. 测试日志消息记录
  describe "Log Message Recording" $ do
    it "should record log messages without errors" $ do
      logger <- createLogger "message-test" Info
      
      -- 记录不同级别的消息
      logMessage logger Debug "debug message"
      logMessage logger Info "info message"
      logMessage logger Warn "warn message"
      logMessage logger Error "error message"
      
      -- 如果没有异常，测试通过
      True `shouldBe` True
    
    it "should handle empty log messages" $ do
      logger <- createLogger "empty-message-test" Info
      logMessage logger Info ""
      
      -- 如果没有异常，测试通过
      True `shouldBe` True
    
    it "should handle long log messages" $ do
      logger <- createLogger "long-message-test" Info
      let longMessage = pack $ replicate 1000 'x'
      logMessage logger Info longMessage
      
      -- 如果没有异常，测试通过
      True `shouldBe` True
  
  -- 3. 测试并发日志记录
  describe "Concurrent Logging" $ do
    it "should handle concurrent logging safely" $ do
      let numThreads = 10
          messagesPerThread = 50
      
      threads <- mapM (\threadId -> forkIO $ do
        logger <- createLogger (pack $ "concurrent-logger-" ++ show threadId) Info
        replicateM_ messagesPerThread $ do
          logMessage logger Info $ pack $ "message from thread " ++ show threadId
        ) [1..numThreads]
      
      -- 等待所有线程完成
      threadDelay 1000000  -- 1秒
      mapM_ killThread threads
      
      -- 如果没有异常，测试通过
      True `shouldBe` True
  
  -- 4. 测试日志级别比较
  describe "Log Level Comparison" $ do
    it "should compare log levels correctly" $ do
      Debug `shouldSatisfy` (< Info)
      Info `shouldSatisfy` (< Warn)
      Warn `shouldSatisfy` (< Error)
      
      Debug `shouldBe` Debug
      Info `shouldBe` Info
      Warn `shouldBe` Warn
      Error `shouldBe` Error
  
  -- 5. 测试Unicode消息处理
  describe "Unicode Message Handling" $ do
    it "should handle unicode characters in messages" $ do
      logger <- createLogger "unicode-logger" Info
      
      -- 测试各种Unicode字符
      logMessage logger Info "测试中文消息"
      logMessage logger Info "Тест русского сообщения"
      logMessage logger Info "Test de message français"
      logMessage logger Info "テスト日本語メッセージ"
      logMessage logger Info "🚀 Rocket emoji message"
      
      -- 如果没有异常，测试通过
      True `shouldBe` True
  
  -- 6. 测试QuickCheck属性
  describe "QuickCheck Properties" $ do
    it "should create loggers with consistent properties" $ property $
      \(name :: String) (levelInt :: Int) ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            loggerNameText = pack name
        in unsafePerformIO $ do
          logger <- createLogger loggerNameText level
          return (loggerName logger == loggerNameText && loggerLevel logger == level)
    
    it "should handle all log levels consistently" $ property $
      \(name :: String) ->
        let levels = [Debug, Info, Warn, Error]
            loggerNameText = pack name
        in unsafePerformIO $ do
          loggers <- mapM (\level -> createLogger loggerNameText level) levels
          let loggerNames = map loggerName loggers
              loggerLevels = map loggerLevel loggers
          return (all (== loggerNameText) loggerNames && loggerLevels == levels)
    
    it "should handle arbitrary message content" $ property $
      \(message :: String) ->
        let messageText = pack message
        in unsafePerformIO $ do
          logger <- createLogger "property-test" Info
          logMessage logger Info messageText
          return True  -- 如果没有异常，测试通过
    
    it "should maintain logger level ordering" $ property $
      \(level1Int :: Int) (level2Int :: Int) ->
        let levels = [Debug, Info, Warn, Error]
            level1 = levels !! (abs level1Int `mod` 4)
            level2 = levels !! (abs level2Int `mod` 4)
            expectedOrder = fromEnum level1 <= fromEnum level2
            actualOrder = level1 <= level2
        in expectedOrder == actualOrder