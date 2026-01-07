{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SpanTracingTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Text (pack, unpack)
import Data.List (nub)
import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Monad (replicateM, replicateM_, when)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Span Tracing Tests" $ do
  
  -- 1. 测试Span创建
  describe "Span Creation" $ do
    it "should create spans with correct names" $ do
      span1 <- createSpan "test-span-1"
      spanName span1 `shouldBe` "test-span-1"
      
      span2 <- createSpan "test-span-2"
      spanName span2 `shouldBe` "test-span-2"
    
    it "should generate valid trace IDs" $ do
      span <- createSpan "trace-id-test"
      let traceId = spanTraceId span
      
      -- 验证trace ID不为空
      unpack traceId `shouldNotBe` ""
      
      -- 验证trace ID是有效的十六进制字符串
      all (`elem` ['0'..'9'] ++ ['a'..'f']) (unpack traceId) `shouldBe` True
    
    it "should generate valid span IDs" $ do
      span <- createSpan "span-id-test"
      let spanId = spanSpanId span
      
      -- 验证span ID不为空
      unpack spanId `shouldNotBe` ""
      
      -- 验证span ID是有效的十六进制字符串
      all (`elem` ['0'..'9'] ++ ['a'..'f']) (unpack spanId) `shouldBe` True
  
  -- 2. 测试Trace ID传播
  describe "Trace ID Propagation" $ do
    it "should propagate the same trace ID across spans" $ do
      -- 创建第一个span
      parentSpan <- createSpan "parent"
      let parentTraceId = spanTraceId parentSpan
      
      -- 创建子span
      childSpan1 <- createSpan "child1"
      childSpan2 <- createSpan "child2"
      
      -- 验证所有span都有相同的trace ID
      spanTraceId childSpan1 `shouldBe` parentTraceId
      spanTraceId childSpan2 `shouldBe` parentTraceId
    
    it "should generate new trace ID after shutdown" $ do
      -- 创建第一个span
      span1 <- createSpan "before-shutdown"
      let traceId1 = spanTraceId span1
      
      -- 关闭并重新初始化
      shutdownTelemetry
      initTelemetry defaultConfig
      
      -- 创建新span
      span2 <- createSpan "after-shutdown"
      let traceId2 = spanTraceId span2
      
      -- 验证trace ID不同
      traceId2 `shouldNotBe` traceId1
  
  -- 3. 测试Span ID唯一性
  describe "Span ID Uniqueness" $ do
    it "should generate unique span IDs" $ do
      spans <- replicateM 10 $ createSpan "unique-test"
      let spanIds = map spanSpanId spans
      
      -- 验证所有span ID都是唯一的
      length (nub spanIds) `shouldBe` length spanIds
  
  -- 4. 测试并发Span创建
  describe "Concurrent Span Creation" $ do
    it "should handle concurrent span creation safely" $ do
      let numThreads = 10
          spansPerThread = 5
      
      threads <- mapM (\threadId -> forkIO $ do
        replicateM_ spansPerThread $ do
          span <- createSpan $ pack $ "concurrent-span-" ++ show threadId
          return span
        ) [1..numThreads]
      
      -- 等待所有线程完成
      threadDelay 1000000  -- 1秒
      mapM_ killThread threads
  
  -- 5. 测试Span创建辅助函数
  describe "Span Creation Helpers" $ do
    it "should create spans and return IDs correctly" $ do
      (traceId, spanId) <- createSpanWithIds "helper-test"
      
      -- 验证返回的ID是有效的
      unpack traceId `shouldNotBe` ""
      unpack spanId `shouldNotBe` ""
      
      -- 验证ID是有效的十六进制字符串
      all (`elem` ['0'..'9'] ++ ['a'..'f']) (unpack traceId) `shouldBe` True
      all (`elem` ['0'..'9'] ++ ['a'..'f']) (unpack spanId) `shouldBe` True
  
  -- 6. 测试QuickCheck属性
  describe "QuickCheck Properties" $ do
    it "should generate unique span IDs for different spans" $ property $
      \(names :: [String]) ->
        let spanNames = take 5 (filter (not . null) names)
            nonEmpty = not (null spanNames)
        in if nonEmpty
           then unsafePerformIO $ do
             spans <- mapM (\name -> createSpan (pack name)) spanNames
             let spanIds = map spanSpanId spans
             return (length (nub spanIds) == length spanIds)
           else True
    
    it "should propagate trace ID correctly across spans" $ property $
      \(names :: [String]) ->
        let spanNames = take 3 (filter (not . null) names)
            nonEmpty = not (null spanNames)
        in if nonEmpty
           then unsafePerformIO $ do
             -- 创建第一个span
             parentSpan <- createSpan (pack (head spanNames))
             let parentTraceId = spanTraceId parentSpan
             
             -- 创建子span
             childSpans <- mapM (\name -> createSpan (pack name)) (tail spanNames)
             
             let childTraceIds = map spanTraceId childSpans
                 allSameTraceId = all (== parentTraceId) childTraceIds
             
             return allSameTraceId
           else True
    
    it "should handle empty span names gracefully" $ property $
      \() -> unsafePerformIO $ do
        span <- createSpan ""
        spanName span `shouldBe` ""
        unpack (spanTraceId span) `shouldNotBe` ""
        unpack (spanSpanId span) `shouldNotBe` ""
        return True