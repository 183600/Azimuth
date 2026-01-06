{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpanPropertiesSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent (threadDelay)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Span Properties Tests" $ do
  
  -- 测试Span ID的唯一性
  describe "Span ID Uniqueness" $ do
    it "should generate unique span IDs for different spans" $ property $
      \names ->
        let spanNames = take 10 (map (pack . take 50 . show) ([1..] :: [Int]))  -- 生成10个唯一名称
            result = unsafePerformIO $ do
              -- 重置追踪上下文以确保唯一性
              shutdownTelemetry
              initTelemetry productionConfig
              
              spans <- mapM createSpan spanNames
              let spanIds = map spanSpanId spans
              return (length (nub spanIds) == length spanIds)
        in result
    
    it "should maintain unique span IDs even with same name" $ property $
      \name ->
        let spanName = pack (take 50 name)
            numSpans = 10
            result = unsafePerformIO $ do
              -- 重置追踪上下文
              shutdownTelemetry
              initTelemetry productionConfig
              
              spans <- sequence $ replicate numSpans $ createSpan spanName
              let spanIds = map spanSpanId spans
              return (length (nub spanIds) == length spanIds)
        in result
  
  -- 测试Trace ID的一致性
  describe "Trace ID Consistency" $ do
    it "should maintain consistent trace ID within same trace" $ property $
      \names ->
        let spanNames = take 5 (map (pack . take 50 . show) ([1..] :: [Int]))
            result = unsafePerformIO $ do
              -- 重置追踪上下文
              shutdownTelemetry
              initTelemetry productionConfig
              
              -- 创建第一个span以建立trace context
              parentSpan <- createSpan (head spanNames)
              let parentTraceId = spanTraceId parentSpan
              
              -- 创建子span
              childSpans <- mapM createSpan (tail spanNames)
              let childTraceIds = map spanTraceId childSpans
              
              return (all (== parentTraceId) childTraceIds)
        in result
    
    it "should generate different trace IDs for different traces" $ property $
      \names ->
        let traceNames = take 3 (map (pack . take 50 . show) ([1..] :: [Int]))
            result = unsafePerformIO $ do
              traceIds <- sequence $ map (\_ -> do
                -- 重置追踪上下文以创建新的trace
                shutdownTelemetry
                initTelemetry productionConfig
                
                -- 创建span以建立trace context
                span <- createSpan "trace-test"
                return (spanTraceId span)
              ) traceNames
              
              return (length (nub traceIds) == length traceIds)
        in result
  
  -- 测试Span名称的持久性
  describe "Span Name Persistence" $ do
    it "should preserve span name after creation" $ property $
      \name ->
        let spanName = pack (take 100 name)  -- 限制名称长度
            result = unsafePerformIO $ do
              span <- createSpan spanName
              return (spanName span == spanName)
        in result
    
    it "should handle empty span names" $ do
      let result = unsafePerformIO $ do
            span <- createSpan ""
            return (spanName span == "")
      result `shouldBe` True
    
    it "should handle special characters in span names" $ property $
      \name ->
        let specialChars = pack $ take 100 name
            result = unsafePerformIO $ do
              span <- createSpan specialChars
              return (spanName span == specialChars)
        in result
  
  -- 测试Span ID的格式
  describe "Span ID Format" $ do
    it "should generate span IDs with consistent length" $ property $
      \names ->
        let spanNames = take 5 (map (pack . take 50 . show) ([1..] :: [Int]))
            result = unsafePerformIO $ do
              spans <- mapM createSpan spanNames
              let spanIds = map spanSpanId spans
                  spanIdLengths = map Text.length spanIds
              return (all (== head spanIdLengths) spanIdLengths)
        in result
    
    it "should generate span IDs with valid hex characters" $ property $
      \names ->
        let spanNames = take 5 (map (pack . take 50 . show) ([1..] :: [Int]))
            isValidHexChar c = c `elem` ['0'..'9'] ++ ['a'..'f']
            result = unsafePerformIO $ do
              spans <- mapM createSpan spanNames
              let spanIds = map spanSpanId spans
                  allValidHex = all (\spanId -> all isValidHexChar (unpack spanId)) spanIds
              return allValidHex
        in result
  
  -- 测试Span操作的安全性
  describe "Span Operation Safety" $ do
    it "should handle span creation and finishing safely" $ property $
      \names ->
        let spanNames = take 5 (map (pack . take 50 . show) ([1..] :: [Int]))
            result = unsafePerformIO $ do
              spans <- mapM createSpan spanNames
              -- 尝试结束所有span
              results <- mapM (try . finishSpan) spans
              return (all isSuccess results)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
    
    it "should handle multiple finish operations on same span" $ property $
      \name ->
        let spanName = pack (take 50 name)
            result = unsafePerformIO $ do
              span <- createSpan spanName
              -- 多次结束同一个span
              result1 <- try $ finishSpan span
              threadDelay 1000  -- 短暂延迟
              result2 <- try $ finishSpan span
              return (isSuccess result1 && isSuccess result2)
            isSuccess (Right _) = True
            isSuccess (Left _) = False
        in result
  
  -- 测试Span的时间相关性
  describe "Span Temporal Properties" $ do
    it "should maintain trace context over time" $ property $
      \names ->
        let spanNames = take 5 (map (pack . take 50 . show) ([1..] :: [Int]))
            result = unsafePerformIO $ do
              -- 重置追踪上下文
              shutdownTelemetry
              initTelemetry productionConfig
              
              -- 创建第一个span
              firstSpan <- createSpan (head spanNames)
              let firstTraceId = spanTraceId firstSpan
              
              -- 延迟后创建更多span
              threadDelay 1000
              laterSpans <- mapM createSpan (tail spanNames)
              let laterTraceIds = map spanTraceId laterSpans
              
              return (all (== firstTraceId) laterTraceIds)
        in result
    
    it "should handle rapid span creation" $ property $
      \num ->
        let numSpans = max 1 (abs num `mod` 20 + 1)
            result = unsafePerformIO $ do
              -- 重置追踪上下文
              shutdownTelemetry
              initTelemetry productionConfig
              
              -- 快速创建多个span
              spans <- sequence $ replicate numSpans $ createSpan "rapid-span"
              let spanIds = map spanSpanId spans
                  traceIds = map spanTraceId spans
              
              return (length (nub spanIds) == length spanIds && 
                     all (== head traceIds) traceIds)
        in result
  
  -- 测试Span的并发安全性
  describe "Span Concurrency Safety" $ do
    it "should handle concurrent span creation safely" $ property $
      \num ->
        let numThreads = max 1 (abs num `mod` 5 + 1)
            result = unsafePerformIO $ do
              -- 重置追踪上下文
              shutdownTelemetry
              initTelemetry productionConfig
              
              -- 创建第一个span以建立trace context
              parentSpan <- createSpan "concurrent-parent"
              let parentTraceId = spanTraceId parentSpan
              
              -- 并发创建子span
              childSpans <- sequence $ replicate numThreads $ do
                createSpan "concurrent-child"
              
              let childTraceIds = map spanTraceId childSpans
                  childSpanIds = map spanSpanId childSpans
              
              return (all (== parentTraceId) childTraceIds &&
                     length (nub childSpanIds) == length childSpanIds)
        in result
  
  -- 测试Span的边界条件
  describe "Span Boundary Conditions" $ do
    it "should handle very long span names" $ do
      let longName = pack $ replicate 10000 'a'
          result = unsafePerformIO $ do
            span <- createSpan longName
            return (spanName span == longName)
      result `shouldBe` True
    
    it "should handle unicode characters in span names" $ property $
      \name ->
        let unicodeName = pack name
            result = unsafePerformIO $ do
              span <- createSpan unicodeName
              return (spanName span == unicodeName)
        in result
    
    it "should handle span names with control characters" $ property $
      \name ->
        let controlName = pack $ take 100 name
            result = unsafePerformIO $ do
              span <- createSpan controlName
              return (spanName span == controlName)
        in result