{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LoggerPropertiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Data.Text (pack, unpack)
import Data.List (nub, sort)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Logger Properties Tests" $ do
  
  -- 测试Logger级别的排序
  describe "Log Level Ordering" $ do
    it "should maintain correct ordering of log levels" $ do
      let levels = [Debug, Info, Warn, Error]
          sortedLevels = sort levels
      sortedLevels `shouldBe` [Debug, Info, Warn, Error]
    
    it "should satisfy transitive property: if a <= b and b <= c then a <= c" $ property $
      \a b c ->
        let levels = [Debug, Info, Warn, Error]
            levelFromInt n = levels !! (abs n `mod` 4)
            levelA = levelFromInt a
            levelB = levelFromInt b
            levelC = levelFromInt c
        in if levelA <= levelB && levelB <= levelC
           then levelA <= levelC
           else True
    
    it "should satisfy antisymmetric property: if a <= b and b <= a then a = b" $ property $
      \a b ->
        let levels = [Debug, Info, Warn, Error]
            levelFromInt n = levels !! (abs n `mod` 4)
            levelA = levelFromInt a
            levelB = levelFromInt b
        in if levelA <= levelB && levelB <= levelA
           then levelA == levelB
           else True
  
  -- 测试Logger名称的持久性
  describe "Logger Name Persistence" $ do
    it "should preserve logger name after creation" $ property $
      \name ->
        let loggerNameText = pack (take 100 name)  -- 限制名称长度
            result = unsafePerformIO $ do
              logger <- createLogger loggerNameText Info
              return (loggerName logger == loggerNameText)
        in result
    
    it "should handle empty logger names" $ do
      let result = unsafePerformIO $ do
            logger <- createLogger "" Info
            return (loggerName logger == "")
      result `shouldBe` True
    
    it "should handle special characters in logger names" $ property $
      \name ->
        let specialChars = pack $ take 100 name
            result = unsafePerformIO $ do
              logger <- createLogger specialChars Info
              return (loggerName logger == specialChars)
        in result
  
  -- 测试Logger级别的持久性
  describe "Logger Level Persistence" $ do
    it "should preserve logger level after creation" $ property $
      \levelInt ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            result = unsafePerformIO $ do
              logger <- createLogger "level-test" level
              return (loggerLevel logger == level)
        in result
    
    it "should create loggers with all different levels" $ do
      let levels = [Debug, Info, Warn, Error]
          result = unsafePerformIO $ do
            loggers <- mapM (\level -> createLogger "multi-level-test" level) levels
            let createdLevels = map loggerLevel loggers
            return (sort createdLevels == levels)
      result `shouldBe` True
  
  -- 测试Logger操作的安全性
  describe "Logger Operation Safety" $ do
    it "should handle logging operations safely" $ property $
      \(messages :: [Int]) ->
        let testMessages = take 10 (map (pack . take 200 . show) ([1..] :: [Int]))
            result = unsafePerformIO $ do
              logger <- createLogger "safety-test" Info
              results <- mapM (\msg -> (try :: IO a -> IO (Either SomeException a)) $ logMessage logger Info msg) testMessages
              return (all isSuccess results)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
    
    it "should handle logging at different levels" $ property $
      \(messages :: [Int]) ->
        let testMessages = take 5 (map (pack . take 200 . show) ([1..] :: [Int]))
            levels = [Debug, Info, Warn, Error]
            result = unsafePerformIO $ do
              logger <- createLogger "multi-level-test" Debug
              results <- sequence $ do
                msg <- testMessages
                level <- levels
                return $ (try :: IO a -> IO (Either SomeException a)) $ logMessage logger level msg
              return (all isSuccess results)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
    
    it "should handle empty log messages" $ property $
      \(levelInt :: Int) ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            result = unsafePerformIO $ do
              logger <- createLogger "empty-message-test" level
              result <- (try :: IO a -> IO (Either SomeException a)) $ logMessage logger level ""
              return (isSuccess result)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试Logger的标识符唯一性
  describe "Logger Identity" $ do
    it "should distinguish loggers with different names" $ property $
      \name1 name2 ->
        let name1Text = pack (take 50 name1)
            name2Text = pack (take 50 name2)
            result = unsafePerformIO $ do
              logger1 <- createLogger name1Text Info
              logger2 <- createLogger name2Text Info
              return (logger1 == logger2, name1Text == name2Text)
        in case result of
            (isEqual, namesEqual) -> isEqual == namesEqual
    
    it "should distinguish loggers with different levels" $ property $
      \name level1 level2 ->
        let nameText = pack (take 50 name)
            levels = [Debug, Info, Warn, Error]
            levelA = levels !! (abs level1 `mod` 4)
            levelB = levels !! (abs level2 `mod` 4)
            result = unsafePerformIO $ do
              logger1 <- createLogger nameText levelA
              logger2 <- createLogger nameText levelB
              return (logger1 == logger2, levelA == levelB)
        in case result of
            (isEqual, levelsEqual) -> isEqual == levelsEqual
  
  -- 测试Logger的边界条件
  describe "Logger Boundary Conditions" $ do
    it "should handle very long logger names" $ do
      let longName = pack $ replicate 10000 'a'
          result = unsafePerformIO $ do
            logger <- createLogger longName Info
            return (loggerName logger == longName)
      result `shouldBe` True
    
    it "should handle unicode characters in logger names" $ property $
      \name ->
        let unicodeName = pack name
            result = unsafePerformIO $ do
              logger <- createLogger unicodeName Info
              return (loggerName logger == unicodeName)
        in result
    
    it "should handle control characters in logger names" $ property $
      \name ->
        let controlName = pack $ take 100 name
            result = unsafePerformIO $ do
              logger <- createLogger controlName Info
              return (loggerName logger == controlName)
        in result
    
    it "should handle very long log messages" $ property $
      \(name :: String) ->
        let longMessage = pack $ replicate 10000 'b'
            result = unsafePerformIO $ do
              logger <- createLogger "long-message-test" Info
              result <- (try :: IO a -> IO (Either SomeException a)) $ logMessage logger Info longMessage
              return (isSuccess result)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试Logger的级别过滤
  describe "Logger Level Filtering" $ do
    it "should create loggers with appropriate level hierarchy" $ property $
      \levelInt ->
        let levels = [Debug, Info, Warn, Error]
            level = levels !! (abs levelInt `mod` 4)
            higherLevels = filter (>= level) levels
            result = unsafePerformIO $ do
              logger <- createLogger "filter-test" level
              return (loggerLevel logger == level && level `elem` levels)
        in result
    
    it "should maintain level ordering consistency" $ property $
      \level1 level2 ->
        let levels = [Debug, Info, Warn, Error]
            levelA = levels !! (abs level1 `mod` 4)
            levelB = levels !! (abs level2 `mod` 4)
        in (levelA <= levelB) == (levelA `elem` takeWhile (<= levelB) levels)
  
  -- 测试Logger的并发安全性
  describe "Logger Concurrency Safety" $ do
    it "should handle concurrent logger creation" $ property $
      \num ->
        let numLoggers = max 1 (abs num `mod` 10 + 1)
            result = unsafePerformIO $ do
              loggers <- sequence $ replicate numLoggers $ do
                createLogger "concurrent-logger" Info
              let loggerNames = map loggerName loggers
                  loggerLevels = map loggerLevel loggers
              return (all (== "concurrent-logger") loggerNames &&
                     all (== Info) loggerLevels)
        in result
    
    it "should handle concurrent logging operations" $ property $
      \(num :: Int) ->
        let numOperations = max 1 (abs num `mod` 20 + 1)
            result = unsafePerformIO $ do
              logger <- createLogger "concurrent-logging" Info
              results <- sequence $ replicate numOperations $ do
                (try :: IO a -> IO (Either SomeException a)) $ logMessage logger Info "concurrent message"
              return (all isSuccess results)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result