{-# LANGUAGE OverloadedStrings #-}

module Azimuth.Telemetry
    ( -- * Telemetry API
      TelemetryConfig(..)
    , defaultConfig
    , initTelemetry
    , shutdownTelemetry
    , -- * Metrics
      Metric(..)
    , createMetric
    , createMetricWithInitialValue
    , recordMetric
    , metricValue
    , unsafeMetricValue
    , -- * Tracing
      Span(..)
    , createSpan
    , finishSpan
    , -- * Logging
      LogLevel(..)
    , Logger(..)
    , loggerName
    , loggerLevel
    , createLogger
    , logMessage
    ) where

import Data.Text (Text, pack)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import System.Random (randomIO)
import Data.Char (intToDigit)
import Numeric (showHex)
import Control.Concurrent.MVar
import Control.Concurrent (myThreadId, ThreadId)
import Data.Hashable (hash)
import Control.Monad (when)
import Prelude hiding (id)

-- | Global trace context storage
{-# NOINLINE traceContext #-}
traceContext :: MVar (Maybe Text)
traceContext = unsafePerformIO (newMVar Nothing)

-- | Global span counter
{-# NOINLINE spanCounter #-}
spanCounter :: IORef Int
spanCounter = unsafePerformIO (newIORef 0)

-- | Global configuration reference
{-# NOINLINE globalConfig #-}
globalConfig :: IORef TelemetryConfig
globalConfig = unsafePerformIO (newIORef defaultConfig)

-- | Global log counter for rate limiting
{-# NOINLINE logCounter #-}
logCounter :: IORef Int
logCounter = unsafePerformIO (newIORef 0)

-- | Configuration for telemetry system
data TelemetryConfig = TelemetryConfig
    { serviceName :: Text
    , serviceVersion :: Text
    , enableMetrics :: Bool
    , enableTracing :: Bool
    , enableLogging :: Bool
    , enableDebugOutput :: Bool
    } deriving (Show, Eq)

-- | Default telemetry configuration
defaultConfig :: TelemetryConfig
defaultConfig = TelemetryConfig
    { serviceName = "azimuth-service"
    , serviceVersion = "0.1.0"
    , enableMetrics = True
    , enableTracing = True
    , enableLogging = True
    , enableDebugOutput = True
    }

-- | Initialize telemetry system
initTelemetry :: TelemetryConfig -> IO ()
initTelemetry config = do
    when (enableDebugOutput config) $
        putStrLn $ "Initializing telemetry for service: " ++ show (serviceName config)
    -- Save configuration globally
    writeIORef globalConfig config
    -- Clear trace context on initialization
    modifyMVar_ traceContext (\_ -> return Nothing)
    -- Reset span counter
    writeIORef spanCounter 0
    -- Reset log counter
    writeIORef logCounter 0
    -- Implementation would go here

-- | Shutdown telemetry system
shutdownTelemetry :: IO ()
shutdownTelemetry = do
    config <- readIORef globalConfig
    when (enableDebugOutput config) $
        putStrLn "Shutting down telemetry system"
    -- Clear trace context on shutdown
    modifyMVar_ traceContext (\_ -> return Nothing)
    -- Reset span counter
    writeIORef spanCounter 0

-- | Generate a random hex string
generateRandomHex :: Int -> IO Text
generateRandomHex len = do
    let chars = ['0'..'9'] ++ ['a'..'f']
    randomInts <- sequence $ replicate len $ randomIO :: IO [Int]
    return $ pack $ map (\i -> chars !! (i `mod` 16)) randomInts

-- | Helper function to hash thread ID
hashThreadId :: ThreadId -> Int
hashThreadId = hash

-- | Generate a unique span ID
generateSpanId :: IO Text
generateSpanId = do
    counter <- readIORef spanCounter
    modifyIORef spanCounter (+1)
    threadId <- myThreadId
    let threadHash = hashThreadId threadId `mod` 10000
    return $ pack $ showHex counter "" ++ showHex threadHash ""

-- | Metric data type
data Metric = Metric
    { metricName :: Text
    , metricValueRef :: MVar Double
    , metricUnit :: Text
    } 

-- | Get the current value of a metric
metricValue :: Metric -> IO Double
metricValue metric = readMVar (metricValueRef metric)

-- | Show instance for Metric (for debugging)
instance Show Metric where
    show metric = show (metricName metric) ++ "=" ++ "<value>" ++ " " ++ show (metricUnit metric)

-- | Eq instance for Metric (compares by name and unit)
instance Eq Metric where
    m1 == m2 = metricName m1 == metricName m2 && metricUnit m1 == metricUnit m2

-- | Create a new metric
createMetric :: Text -> Text -> IO Metric
createMetric name unit = do
    valueRef <- newMVar 0.0
    return $ Metric name valueRef unit

-- | Create a new metric with initial value (for testing)
createMetricWithInitialValue :: Text -> Text -> Double -> IO Metric
createMetricWithInitialValue name unit initialValue = do
    valueRef <- newMVar initialValue
    return $ Metric name valueRef unit

-- | Record a metric value
recordMetric :: Metric -> Double -> IO ()
recordMetric metric value = do
    config <- readIORef globalConfig
    when (enableDebugOutput config) $ do
        count <- atomicModifyIORef' logCounter (\c -> (c + 1, c + 1))
        -- Only log every 100th operation to reduce output in high-frequency scenarios
        when (count `mod` 100 == 0) $
            putStrLn $ "Recording metric: " ++ show (metricName metric) ++ " = " ++ show value ++ " (count: " ++ show count ++ ")"
    modifyMVar_ (metricValueRef metric) (\currentValue -> return (currentValue + value))
    return ()

-- | Unsafe get metric value (for testing only)
unsafeMetricValue :: Metric -> Double
unsafeMetricValue metric = unsafePerformIO (readMVar (metricValueRef metric))

-- | Span data type for tracing
data Span = Span
    { spanName :: Text
    , spanTraceId :: Text
    , spanSpanId :: Text
    } deriving (Show, Eq)

-- | Create a new span
createSpan :: Text -> IO Span
createSpan name = do
    -- Get current trace context or create a new one
    currentTraceId <- modifyMVar traceContext $ \maybeTraceId -> 
        case maybeTraceId of
            Just traceId -> return (maybeTraceId, traceId)
            Nothing -> do
                -- Generate a new trace ID for the first span in a trace
                newTraceId <- generateRandomHex 8
                return (Just newTraceId, newTraceId)
    
    -- Generate a unique span ID for each span
    spanId <- generateSpanId
    
    return $ Span name currentTraceId spanId

-- | Finish a span
finishSpan :: Span -> IO ()
finishSpan span = do
    config <- readIORef globalConfig
    when (enableDebugOutput config) $ do
        count <- atomicModifyIORef' logCounter (\c -> (c + 1, c + 1))
        -- Only log every 100th operation to reduce output in high-frequency scenarios
        when (count `mod` 100 == 0) $
            putStrLn $ "Finishing span: " ++ show (spanName span) ++ " (count: " ++ show count ++ ")"
    -- Implementation would go here

-- | Log levels
data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq)

-- | Logger data type
data Logger = Logger
    { loggerName :: Text
    , loggerLevel :: LogLevel
    } deriving (Show, Eq)

-- | Create a new logger
createLogger :: Text -> LogLevel -> IO Logger
createLogger name level = return $ Logger name level

-- | Log a message
logMessage :: Logger -> LogLevel -> Text -> IO ()
logMessage logger level message = do
    config <- readIORef globalConfig
    when (enableDebugOutput config) $ do
        count <- atomicModifyIORef' logCounter (\c -> (c + 1, c + 1))
        -- Only log every 100th operation to reduce output in high-frequency scenarios
        when (count `mod` 100 == 0) $
            putStrLn $ "[" ++ show (loggerLevel logger) ++ "] " ++ show (loggerName logger) ++ ": " ++ show message ++ " (count: " ++ show count ++ ")"
    -- Implementation would go here