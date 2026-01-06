{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RealWorldCabalTestSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (try, SomeException, evaluate)
import Control.Concurrent (forkIO, threadDelay, killThread, MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (replicateM, when, void, unless, sequence_, forM)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)
import Data.Text (pack, unpack)
import qualified Data.Text as Text
import Data.List (nub, sort, group, intercalate, take, drop, replicate)
import Prelude hiding (id)

import Azimuth.Telemetry

spec :: Spec
spec = describe "Real World Cabal Test Suite" $ do
  
  -- 1. Web应用监控场景
  describe "Web Application Monitoring" $ do
    it "should handle typical web application metrics" $ do
      initTelemetry productionConfig
      
      -- 创建Web应用度量
      httpRequests <- createMetric "http_requests_total" "count"
      responseTime <- createMetric "http_response_time_seconds" "seconds"
      activeConnections <- createMetric "active_connections" "count"
      errorRate <- createMetric "error_rate_percent" "percent"
      
      -- 创建日志器
      accessLogger <- createLogger "access" Info
      errorLogger <- createLogger "errors" Error
      
      -- 模拟Web请求
      let requests = [
            ("GET", "/api/users", 200, 0.12),
            ("POST", "/api/users", 201, 0.25),
            ("GET", "/api/users/123", 404, 0.08),
            ("GET", "/api/products", 200, 0.15),
            ("POST", "/api/orders", 500, 2.3)
            ]
      
      sequence_ $ flip map requests $ \(method, path, status, time) -> do
        recordMetric httpRequests 1.0
        recordMetric responseTime time
        recordMetric activeConnections 1.0
        
        logMessage accessLogger Info (pack $ method ++ " " ++ path ++ " " ++ show status)
        
        when (status >= 400) $ do
          recordMetric errorRate 1.0
          logMessage errorLogger Error (pack $ method ++ " " ++ path ++ " returned " ++ show status)
        
        recordMetric activeConnections (-1.0)
      
      -- 验证度量
      totalRequests <- metricValue httpRequests
      avgResponseTime <- metricValue responseTime
      activeConns <- metricValue activeConnections
      errors <- metricValue errorRate
      
      shutdownTelemetry
      
      totalRequests `shouldBe` 5.0
      avgResponseTime `shouldBe` 2.9  -- 0.12 + 0.25 + 0.08 + 0.15 + 2.3
      activeConns `shouldBe` 0.0
      errors `shouldBe` 2.0  -- 404和500错误
    
    it "should handle user session tracking" $ property $
      \sessionCount ->
        let actualSessions = max 1 (abs sessionCount `mod` 100 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          activeSessions <- createMetric "active_sessions" "count"
          sessionDuration <- createMetric "session_duration_seconds" "seconds"
          pageViews <- createMetric "page_views_total" "count"
          
          userLogger <- createLogger "user_activity" Info
          
          -- 模拟用户会话
          sequence_ $ forM [1..fromIntegral actualSessions] $ \sessionId -> do
            recordMetric activeSessions 1.0
            logMessage userLogger Info (pack $ "Session " ++ show sessionId ++ " started")
            
            -- 模拟页面浏览
            sequence_ $ forM [1..5] $ \pageId -> do
              recordMetric pageViews 1.0
              logMessage userLogger Info (pack $ "Session " ++ show sessionId ++ " viewed page " ++ show pageId)
            
            recordMetric sessionDuration 300.0  -- 5分钟会话
            recordMetric activeSessions (-1.0)
            logMessage userLogger Info (pack $ "Session " ++ show sessionId ++ " ended")
          
          -- 验证会话度量
          totalSessions <- metricValue activeSessions
          totalDuration <- metricValue sessionDuration
          totalPageViews <- metricValue pageViews
          
          shutdownTelemetry
          
          totalSessions `shouldBe` 0.0  -- 所有会话都已结束
          totalDuration `shouldBe` fromIntegral actualSessions * 300.0
          totalPageViews `shouldBe` fromIntegral actualSessions * 5.0
  
  -- 2. 微服务架构监控
  describe "Microservices Architecture Monitoring" $ do
    it "should handle distributed tracing across services" $ do
      initTelemetry productionConfig
      
      -- 创建服务度量
      apiGatewayRequests <- createMetric "api_gateway_requests" "count"
      userServiceRequests <- createMetric "user_service_requests" "count"
      productServiceRequests <- createMetric "product_service_requests" "count"
      orderServiceRequests <- createMetric "order_service_requests" "count"
      
      serviceLogger <- createLogger "services" Info
      
      -- 模拟分布式请求
      apiGatewaySpan <- createSpan "api_gateway_request"
      recordMetric apiGatewayRequests 1.0
      logMessage serviceLogger Info "API Gateway: Processing request"
      
      -- 用户服务调用
      userServiceSpan <- createSpan "user_service_call"
      recordMetric userServiceRequests 1.0
      logMessage serviceLogger Info "User Service: Authenticating user"
      finishSpan userServiceSpan
      
      -- 产品服务调用
      productServiceSpan <- createSpan "product_service_call"
      recordMetric productServiceRequests 1.0
      logMessage serviceLogger Info "Product Service: Fetching product data"
      finishSpan productServiceSpan
      
      -- 订单服务调用
      orderServiceSpan <- createSpan "order_service_call"
      recordMetric orderServiceRequests 1.0
      logMessage serviceLogger Info "Order Service: Creating order"
      finishSpan orderServiceSpan
      
      finishSpan apiGatewaySpan
      
      -- 验证服务度量
      apiGatewayCount <- metricValue apiGatewayRequests
      userServiceCount <- metricValue userServiceRequests
      productServiceCount <- metricValue productServiceRequests
      orderServiceCount <- metricValue orderServiceRequests
      
      shutdownTelemetry
      
      apiGatewayCount `shouldBe` 1.0
      userServiceCount `shouldBe` 1.0
      productServiceCount `shouldBe` 1.0
      orderServiceCount `shouldBe` 1.0
    
    it "should handle service mesh communication" $ property $
      \serviceCount ->
        let actualServices = max 2 (abs serviceCount `mod` 5 + 2)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建服务网格度量
          interServiceCalls <- createMetric "inter_service_calls" "count"
          serviceLatency <- createMetric "service_latency_ms" "ms"
          circuitBreakerTrips <- createMetric "circuit_breaker_trips" "count"
          
          meshLogger <- createLogger "service_mesh" Info
          
          -- 模拟服务间通信
          sequence_ $ forM [1..fromIntegral actualServices] $ \serviceId -> do
            entrySpan <- createSpan (pack $ "service_" ++ show serviceId ++ "_entry")
            
            sequence_ $ forM [1..3] $ \callId -> do
              callSpan <- createSpan (pack $ "call_" ++ show callId)
              recordMetric interServiceCalls 1.0
              recordMetric serviceLatency (50.0 + fromIntegral callId * 10.0)
              
              -- 模拟熔断器触发
              when (callId == 2 && serviceId `mod` 3 == 0) $ do
                recordMetric circuitBreakerTrips 1.0
                logMessage meshLogger Warn (pack $ "Circuit breaker tripped for service " ++ show serviceId)
              
              finishSpan callSpan
            
            finishSpan entrySpan
          
          -- 验证服务网格度量
          totalCalls <- metricValue interServiceCalls
          totalLatency <- metricValue serviceLatency
          totalTrips <- metricValue circuitBreakerTrips
          
          shutdownTelemetry
          
          let expectedCalls = fromIntegral actualServices * 3
              expectedLatency = sum [50.0 + fromIntegral callId * 10.0 | 
                                   _ <- [1..actualServices], callId <- [0..2]]
              expectedTrips = fromIntegral (actualServices `div` 3)
          
          return (totalCalls == expectedCalls &&
                  totalLatency == expectedLatency &&
                  totalTrips == expectedTrips)
  
  -- 3. 数据库性能监控
  describe "Database Performance Monitoring" $ do
    it "should handle database operation metrics" $ do
      initTelemetry productionConfig
      
      -- 创建数据库度量
      dbConnections <- createMetric "db_connections" "count"
      queryTime <- createMetric "query_time_ms" "ms"
      slowQueries <- createMetric "slow_queries" "count"
      dbErrors <- createMetric "db_errors" "count"
      
      dbLogger <- createLogger "database" Debug
      
      -- 模拟数据库操作
      let queries = [
            ("SELECT * FROM users", 25, False),
            ("SELECT * FROM orders", 150, True),  -- 慢查询
            ("INSERT INTO users", 45, False),
            ("UPDATE products", 80, False),
            ("DELETE FROM sessions", 200, True),   -- 慢查询
            ("SELECT * FROM products", 500, False) -- 错误
            ]
      
      sequence_ $ flip map queries $ \(query, time, isSlow) -> do
        recordMetric dbConnections 1.0
        recordMetric queryTime (fromIntegral time)
        
        logMessage dbLogger Debug (pack $ "Executing: " ++ query)
        
        when isSlow $ do
          recordMetric slowQueries 1.0
          logMessage dbLogger Warn (pack $ "Slow query detected: " ++ query ++ " (" ++ show time ++ "ms)")
        
        when (time > 400) $ do
          recordMetric dbErrors 1.0
          logMessage dbLogger Error (pack $ "Database error in: " ++ query)
        
        recordMetric dbConnections (-1.0)
      
      -- 验证数据库度量
      activeConnections <- metricValue dbConnections
      totalQueryTime <- metricValue queryTime
      totalSlowQueries <- metricValue slowQueries
      totalErrors <- metricValue dbErrors
      
      shutdownTelemetry
      
      activeConnections `shouldBe` 0.0
      totalQueryTime `shouldBe` 1000.0  -- 25 + 150 + 45 + 80 + 200 + 500
      totalSlowQueries `shouldBe` 2.0
      totalErrors `shouldBe` 1.0
  
  -- 4. 缓存性能监控
  describe "Cache Performance Monitoring" $ do
    it "should handle cache hit/miss metrics" $ property $
      \requestCount ->
        let actualRequests = max 10 (abs requestCount `mod` 1000 + 10)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          cacheHits <- createMetric "cache_hits" "count"
          cacheMisses <- createMetric "cache_misses" "count"
          cacheEvictions <- createMetric "cache_evictions" "count"
          cacheSize <- createMetric "cache_size_bytes" "bytes"
          
          cacheLogger <- createLogger "cache" Debug
          
          -- 模拟缓存操作
          sequence_ $ forM [1..fromIntegral actualRequests] $ \requestId -> do
            let isHit = requestId `mod` 3 /= 0  -- 约67%命中率
                isEviction = requestId `mod` 20 == 0
            
            if isHit
              then do
                recordMetric cacheHits 1.0
                logMessage cacheLogger Debug (pack $ "Cache hit for request " ++ show requestId)
              else do
                recordMetric cacheMisses 1.0
                logMessage cacheLogger Debug (pack $ "Cache miss for request " ++ show requestId)
                
                when isEviction $ do
                  recordMetric cacheEvictions 1.0
                  recordMetric cacheSize (-1024.0)  -- 减少缓存大小
                  logMessage cacheLogger Debug (pack $ "Cache eviction for request " ++ show requestId)
                
                recordMetric cacheSize 1024.0  -- 增加缓存大小
          
          -- 验证缓存度量
          totalHits <- metricValue cacheHits
          totalMisses <- metricValue cacheMisses
          totalEvictions <- metricValue cacheEvictions
          finalCacheSize <- metricValue cacheSize
          
          shutdownTelemetry
          
          let expectedHits = fromIntegral (actualRequests - actualRequests `div` 3)
              expectedMisses = fromIntegral (actualRequests `div` 3)
              expectedEvictions = fromIntegral (actualRequests `div` 20)
              expectedCacheSize = (expectedMisses - expectedEvictions) * 1024.0
          
          return (totalHits == expectedHits &&
                  totalMisses == expectedMisses &&
                  totalEvictions == expectedEvictions &&
                  finalCacheSize == expectedCacheSize)
  
  -- 5. 消息队列监控
  describe "Message Queue Monitoring" $ do
    it "should handle message queue metrics" $ do
      initTelemetry productionConfig
      
      -- 创建消息队列度量
      messagesPublished <- createMetric "messages_published" "count"
      messagesConsumed <- createMetric "messages_consumed" "count"
      queueDepth <- createMetric "queue_depth" "count"
      processingTime <- createMetric "processing_time_ms" "ms"
      
      queueLogger <- createLogger "queue" Info
      
      -- 模拟消息队列操作
      let messages = [
            ("user.created", 50),
            ("order.placed", 120),
            ("payment.processed", 80),
            ("order.shipped", 30),
            ("user.updated", 40)
            ]
      
      sequence_ $ flip map messages $ \(messageType, processTime) -> do
        recordMetric messagesPublished 1.0
        recordMetric queueDepth 1.0
        
        logMessage queueLogger Info (pack $ "Published message: " ++ messageType)
        
        -- 模拟消费
        recordMetric messagesConsumed 1.0
        recordMetric processingTime (fromIntegral processTime)
        recordMetric queueDepth (-1.0)
        
        logMessage queueLogger Info (pack $ "Consumed message: " ++ messageType ++ " in " ++ show processTime ++ "ms")
      
      -- 验证消息队列度量
      totalPublished <- metricValue messagesPublished
      totalConsumed <- metricValue messagesConsumed
      finalQueueDepth <- metricValue queueDepth
      totalProcessingTime <- metricValue processingTime
      
      shutdownTelemetry
      
      totalPublished `shouldBe` 5.0
      totalConsumed `shouldBe` 5.0
      finalQueueDepth `shouldBe` 0.0
      totalProcessingTime `shouldBe` 320.0  -- 50 + 120 + 80 + 30 + 40
  
  -- 6. API网关监控
  describe "API Gateway Monitoring" $ do
    it "should handle API gateway routing metrics" $ do
      initTelemetry productionConfig
      
      -- 创建API网关度量
      gatewayRequests <- createMetric "gateway_requests" "count"
      routeLatency <- createMetric "route_latency_ms" "ms"
      authenticationFailures <- createMetric "auth_failures" "count"
      rateLimitHits <- createMetric "rate_limit_hits" "count"
      
      gatewayLogger <- createLogger "gateway" Info
      
      -- 模拟API网关请求
      let routes = [
            ("/api/v1/users", 50, True, False),   -- 认证成功
            ("/api/v1/products", 80, True, False), -- 认证成功
            ("/api/v1/orders", 120, False, False), -- 认证失败
            ("/api/v1/users", 60, True, True),     -- 限流
            ("/api/v1/products", 70, True, False)  -- 认证成功
            ]
      
      sequence_ $ flip map routes $ \(route, latency, authSuccess, rateLimited) -> do
        recordMetric gatewayRequests 1.0
        recordMetric routeLatency (fromIntegral latency)
        
        logMessage gatewayLogger Info (pack $ "Routing request to: " ++ route)
        
        unless authSuccess $ do
          recordMetric authenticationFailures 1.0
          logMessage gatewayLogger Warn (pack $ "Authentication failed for: " ++ route)
        
        when rateLimited $ do
          recordMetric rateLimitHits 1.0
          logMessage gatewayLogger Warn (pack $ "Rate limit hit for: " ++ route)
      
      -- 验证API网关度量
      totalRequests <- metricValue gatewayRequests
      totalLatency <- metricValue routeLatency
      totalAuthFailures <- metricValue authenticationFailures
      totalRateLimitHits <- metricValue rateLimitHits
      
      shutdownTelemetry
      
      totalRequests `shouldBe` 5.0
      totalLatency `shouldBe` 380.0  -- 50 + 80 + 120 + 60 + 70
      totalAuthFailures `shouldBe` 1.0
      totalRateLimitHits `shouldBe` 1.0
  
  -- 7. 容器化应用监控
  describe "Containerized Application Monitoring" $ do
    it "should handle container resource metrics" $ property $
      \containerCount ->
        let actualContainers = max 1 (abs containerCount `mod` 10 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建容器资源度量
          cpuUsage <- createMetric "container_cpu_usage_percent" "percent"
          memoryUsage <- createMetric "container_memory_usage_mb" "mb"
          networkIO <- createMetric "container_network_io_mb" "mb"
          diskIO <- createMetric "container_disk_io_mb" "mb"
          
          containerLogger <- createLogger "containers" Info
          
          -- 模拟容器资源使用
          sequence_ $ forM [1..fromIntegral actualContainers] $ \containerId -> do
            let cpu = 25.0 + fromIntegral (containerId `mod` 75)  -- 25-100% CPU
                memory = 128.0 + fromIntegral (containerId `mod` 512)  -- 128-640MB 内存
                network = 10.0 + fromIntegral (containerId `mod` 100)  -- 10-110MB 网络
                disk = 5.0 + fromIntegral (containerId `mod` 50)  -- 5-55MB 磁盘
            
            recordMetric cpuUsage cpu
            recordMetric memoryUsage memory
            recordMetric networkIO network
            recordMetric diskIO disk
            
            logMessage containerLogger Info (pack $ "Container " ++ show containerId ++ 
                                              " - CPU: " ++ show cpu ++ "%, " ++
                                              "Memory: " ++ show memory ++ "MB")
          
          -- 验证容器资源度量
          totalCpu <- metricValue cpuUsage
          totalMemory <- metricValue memoryUsage
          totalNetwork <- metricValue networkIO
          totalDisk <- metricValue diskIO
          
          shutdownTelemetry
          
          let expectedCpu = sum [25.0 + fromIntegral (i `mod` 75) | i <- [0..actualContainers-1]]
              expectedMemory = sum [128.0 + fromIntegral (i `mod` 512) | i <- [0..actualContainers-1]]
              expectedNetwork = sum [10.0 + fromIntegral (i `mod` 100) | i <- [0..actualContainers-1]]
              expectedDisk = sum [5.0 + fromIntegral (i `mod` 50) | i <- [0..actualContainers-1]]
          
          return (totalCpu == expectedCpu &&
                  totalMemory == expectedMemory &&
                  totalNetwork == expectedNetwork &&
                  totalDisk == expectedDisk)
  
  -- 8. 移动应用监控
  describe "Mobile Application Monitoring" $ do
    it "should handle mobile app metrics" $ property $
      \userCount ->
        let actualUsers = max 1 (abs userCount `mod` 100 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建移动应用度量
          appLaunches <- createMetric "app_launches" "count"
          screenViews <- createMetric "screen_views" "count"
          userActions <- createMetric "user_actions" "count"
          crashes <- createMetric "app_crashes" "count"
          
          mobileLogger <- createLogger "mobile_app" Info
          
          -- 模拟移动应用使用
          sequence_ $ forM [1..fromIntegral actualUsers] $ \userId -> do
            recordMetric appLaunches 1.0
            logMessage mobileLogger Info (pack $ "User " ++ show userId ++ " launched app")
            
            -- 模拟屏幕浏览
            sequence_ $ forM [1..5] $ \screenId -> do
              recordMetric screenViews 1.0
              logMessage mobileLogger Debug (pack $ "User " ++ show userId ++ " viewed screen " ++ show screenId)
            
            -- 模拟用户操作
            sequence_ $ forM [1..10] $ \actionId -> do
              recordMetric userActions 1.0
              logMessage mobileLogger Debug (pack $ "User " ++ show userId ++ " performed action " ++ show actionId)
            
            -- 模拟应用崩溃
            when (userId `mod` 20 == 0) $ do
              recordMetric crashes 1.0
              logMessage mobileLogger Error (pack $ "App crashed for user " ++ show userId)
          
          -- 验证移动应用度量
          totalLaunches <- metricValue appLaunches
          totalScreenViews <- metricValue screenViews
          totalActions <- metricValue userActions
          totalCrashes <- metricValue crashes
          
          shutdownTelemetry
          
          let expectedLaunches = fromIntegral actualUsers
              expectedScreenViews = fromIntegral actualUsers * 5
              expectedActions = fromIntegral actualUsers * 10
              expectedCrashes = fromIntegral (actualUsers `div` 20)
          
          return (totalLaunches == expectedLaunches &&
                  totalScreenViews == expectedScreenViews &&
                  totalActions == expectedActions &&
                  totalCrashes == expectedCrashes)
  
  -- 9. IoT设备监控
  describe "IoT Device Monitoring" $ do
    it "should handle IoT device telemetry" $ property $
      \deviceCount ->
        let actualDevices = max 1 (abs deviceCount `mod` 50 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建IoT设备度量
          deviceMessages <- createMetric "device_messages" "count"
          sensorReadings <- createMetric "sensor_readings" "count"
          deviceErrors <- createMetric "device_errors" "count"
          batteryLevel <- createMetric "battery_level_percent" "percent"
          
          iotLogger <- createLogger "iot_devices" Info
          
          -- 模拟IoT设备遥测
          sequence_ $ forM [1..fromIntegral actualDevices] $ \deviceId -> do
            let messages = 10 + deviceId `mod` 20
                readings = 50 + deviceId `mod` 100
                errors = if deviceId `mod` 10 == 0 then 1 else 0
                battery = 100.0 - fromIntegral (deviceId `mod` 50)
            
            recordMetric deviceMessages (fromIntegral messages)
            recordMetric sensorReadings (fromIntegral readings)
            recordMetric deviceErrors (fromIntegral errors)
            recordMetric batteryLevel battery
            
            logMessage iotLogger Info (pack $ "Device " ++ show deviceId ++ 
                                          " - Messages: " ++ show messages ++ 
                                          ", Readings: " ++ show readings ++
                                          ", Battery: " ++ show battery ++ "%")
            
            when (errors > 0) $ do
              logMessage iotLogger Warn (pack $ "Device " ++ show deviceId ++ " reported errors")
          
          -- 验证IoT设备度量
          totalMessages <- metricValue deviceMessages
          totalReadings <- metricValue sensorReadings
          totalErrors <- metricValue deviceErrors
          avgBattery <- metricValue batteryLevel
          
          shutdownTelemetry
          
          let expectedMessages = sum [fromIntegral (10 + i `mod` 20) | i <- [0..actualDevices-1]]
              expectedReadings = sum [fromIntegral (50 + i `mod` 100) | i <- [0..actualDevices-1]]
              expectedErrors = fromIntegral (actualDevices `div` 10)
              expectedBattery = sum [100.0 - fromIntegral (i `mod` 50) | i <- [0..actualDevices-1]]
          
          return (totalMessages == expectedMessages &&
                  totalReadings == expectedReadings &&
                  totalErrors == expectedErrors &&
                  avgBattery == expectedBattery)
  
  -- 10. 游戏服务器监控
  describe "Game Server Monitoring" $ do
    it "should handle game server metrics" $ property $
      \playerCount ->
        let actualPlayers = max 1 (abs playerCount `mod` 100 + 1)
        in unsafePerformIO $ do
          initTelemetry productionConfig
          
          -- 创建游戏服务器度量
          activePlayers <- createMetric "active_players" "count"
          gameSessions <- createMetric "game_sessions" "count"
          playerActions <- createMetric "player_actions" "count"
          serverLatency <- createMetric "server_latency_ms" "ms"
          
          gameLogger <- createLogger "game_server" Info
          
          -- 模拟游戏服务器活动
          sequence_ $ forM [1..fromIntegral actualPlayers] $ \playerId -> do
            recordMetric activePlayers 1.0
            recordMetric gameSessions 1.0
            
            logMessage gameLogger Info (pack $ "Player " ++ show playerId ++ " joined game")
            
            -- 模拟玩家操作
            sequence_ $ forM [1..20] $ \actionId -> do
              recordMetric playerActions 1.0
              recordMetric serverLatency (10.0 + fromIntegral (actionId `mod` 50))
            
            recordMetric activePlayers (-1.0)
            logMessage gameLogger Info (pack $ "Player " ++ show playerId ++ " left game")
          
          -- 验证游戏服务器度量
          finalActivePlayers <- metricValue activePlayers
          totalSessions <- metricValue gameSessions
          totalActions <- metricValue playerActions
          avgLatency <- metricValue serverLatency
          
          shutdownTelemetry
          
          finalActivePlayers `shouldBe` 0.0  -- 所有玩家都已离开
          totalSessions `shouldBe` fromIntegral actualPlayers
          totalActions `shouldBe` fromIntegral actualPlayers * 20
          
          let expectedLatency = sum [10.0 + fromIntegral (i `mod` 50) | 
                                   _ <- [1..fromIntegral actualPlayers], i <- [1..20]]
          
          return (avgLatency == expectedLatency)