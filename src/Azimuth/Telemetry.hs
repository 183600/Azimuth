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
    , -- * Simple Metrics (for testing)
      SimpleMetric(..)
    , createSimpleMetric
    , recordSimpleMetric
    , simpleMetricValue
    , simpleToMetric
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
    , -- * Internal state (for testing)
      metricRegistry
    , metricCache
    ) where

import Data.Text (Text, pack, null)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef
import System.Random (randomIO)
import Data.Char (intToDigit)
import Numeric (showHex)
import Control.Concurrent.MVar
import Control.Concurrent (myThreadId, ThreadId)
import Data.Hashable (hash)
import Control.Monad (when)
import qualified Data.Map as Map
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

-- | Global metric registry for sharing values by name
{-# NOINLINE metricRegistry #-}
metricRegistry :: MVar (Map.Map (Text, Text) (MVar Double))
metricRegistry = unsafePerformIO (newMVar Map.empty)

-- | Global metric cache for testing with unsafePerformIO
{-# NOINLINE metricCache #-}
metricCache :: IORef (Map.Map (Text, Text) Double)
metricCache = unsafePerformIO (newIORef Map.empty)

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
    -- Clear metric registry
    modifyMVar_ metricRegistry (\_ -> return Map.empty)
    -- Clear metric cache
    writeIORef metricCache Map.empty

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
    let threadHash = hashThreadId threadId `mod` 65536  -- Use larger range for better uniqueness
        -- Pad counter to 8 hex digits and threadHash to 4 hex digits for consistent length
        spanId = padHex 8 counter ++ padHex 4 threadHash
    return $ pack spanId
  where
    padHex n value = let hex = showHex value ""
                        in replicate (n - length hex) '0' ++ hex

-- | Metric data type
data Metric = Metric
    { metricName :: Text
    , metricValueRef :: MVar Double
    , metricUnit :: Text
    } 

-- | Get the current value of a metric
metricValue :: Metric -> IO Double
metricValue metric = do
    -- First try to get the value from the global cache
    let key = (metricName metric, metricUnit metric)
    cache <- readIORef metricCache
    case Map.lookup key cache of
        Just cachedValue -> return cachedValue
        Nothing -> do
            -- If not in cache, fall back to MVar
            mvarValue <- readMVar (metricValueRef metric)
            -- Update the cache with the MVar value
            atomicModifyIORef' metricCache (\c -> (Map.insert key mvarValue c, ()))
            return mvarValue

-- | Show instance for Metric (for debugging)
instance Show Metric where
    show metric = show (metricName metric) ++ "=" ++ "<value>" ++ " " ++ show (metricUnit metric)

-- | Eq instance for Metric (compares by name and unit)
instance Eq Metric where
    m1 == m2 = metricName m1 == metricName m2 && metricUnit m1 == metricUnit m2

-- | Create a new metric
createMetric :: Text -> Text -> IO Metric
createMetric name unit = do
    -- Check if a metric with this name and unit already exists in the registry
    registry <- readMVar metricRegistry
    case Map.lookup (name, unit) registry of
        Just existingValueRef -> do
            -- Ensure the metric is in the cache
            let key = (name, unit)
            cache <- readIORef metricCache
            when (not $ Map.member key cache) $ do
                mvarValue <- readMVar existingValueRef
                atomicModifyIORef' metricCache (\c -> (Map.insert key mvarValue c, ()))
            -- Return a metric with the existing value reference
            return $ Metric name existingValueRef unit
        Nothing -> do
            -- Create a new value reference and register it
            newValueRef <- newMVar 0.0
            modifyMVar_ metricRegistry (\reg -> return $ Map.insert (name, unit) newValueRef reg)
            -- Also add to cache
            let key = (name, unit)
            atomicModifyIORef' metricCache (\c -> (Map.insert key 0.0 c, ()))
            return $ Metric name newValueRef unit

-- | Create a new metric with initial value (for testing)
createMetricWithInitialValue :: Text -> Text -> Double -> IO Metric
createMetricWithInitialValue name unit initialValue = do
    -- Check if a metric with this name and unit already exists in the registry
    registry <- readMVar metricRegistry
    case Map.lookup (name, unit) registry of
        Just existingValueRef -> do
            -- Set the initial value for the existing metric
            modifyMVar_ existingValueRef (\_ -> return initialValue)
            -- Also update the cache
            let key = (name, unit)
            atomicModifyIORef' metricCache (\c -> (Map.insert key initialValue c, ()))
            return $ Metric name existingValueRef unit
        Nothing -> do
            -- Create a new value reference with initial value and register it
            newValueRef <- newMVar initialValue
            modifyMVar_ metricRegistry (\reg -> return $ Map.insert (name, unit) newValueRef reg)
            -- Also add to cache
            let key = (name, unit)
            atomicModifyIORef' metricCache (\c -> (Map.insert key initialValue c, ()))
            return $ Metric name newValueRef unit

-- | Record a metric value
recordMetric :: Metric -> Double -> IO ()
recordMetric metric value = do
    config <- readIORef globalConfig
    when (enableDebugOutput config) $ do
        count <- atomicModifyIORef' logCounter (\c -> (c + 1, c + 1))
        -- Only log every 10000th operation to reduce output in high-frequency scenarios
        when (count `mod` 10000 == 0) $
            putStrLn $ "Recording metric: " ++ show (metricName metric) ++ " = " ++ show value ++ " (count: " ++ show count ++ ")"
    
    -- Update the metric value atomically
    modifyMVar_ (metricValueRef metric) (\currentValue -> return (currentValue + value))
    
    -- Also update the global cache for testing with unsafePerformIO
    let key = (metricName metric, metricUnit metric)
    atomicModifyIORef' metricCache (\cache -> 
        case Map.lookup key cache of
            Just cachedValue -> (Map.insert key (cachedValue + value) cache, ())
            Nothing -> (Map.insert key value cache, ()))
    
    return ()

-- | Simple metric for testing (pure functional)
data SimpleMetric = SimpleMetric
    { smName :: Text
    , smUnit :: Text
    , smValue :: Double
    } deriving (Show, Eq)

-- | Create a simple metric for testing
createSimpleMetric :: Text -> Text -> Double -> SimpleMetric
createSimpleMetric name unit value = SimpleMetric name unit value

-- | Record a value to a simple metric (pure function)
recordSimpleMetric :: SimpleMetric -> Double -> SimpleMetric
recordSimpleMetric metric value = 
    metric { smValue = smValue metric + value }

-- | Get the value of a simple metric
simpleMetricValue :: SimpleMetric -> Double
simpleMetricValue metric = smValue metric

-- | Convert a simple metric to a regular metric (for integration)
simpleToMetric :: SimpleMetric -> IO Metric
simpleToMetric simpleMetric = do
    metric <- createMetricWithInitialValue (smName simpleMetric) (smUnit simpleMetric) (smValue simpleMetric)
    return metric

-- | Record a metric value (unsafe version for testing with unsafePerformIO)
unsafeRecordMetric :: Metric -> Double -> Double
unsafeRecordMetric metric value = unsafePerformIO $ do
    recordMetric metric value
    metricValue metric

-- | Unsafe get metric value (for testing only)
unsafeMetricValue :: Metric -> Double
unsafeMetricValue metric = unsafePerformIO (metricValue metric)

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
        -- Only log every 10000th operation to reduce output in high-frequency scenarios
        when (count `mod` 10000 == 0) $
            putStrLn $ "Finishing span: " ++ show (spanName span) ++ " (count: " ++ show count ++ ")"
    -- Implementation would go here

-- | Log levels
data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq, Ord, Enum)

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
        -- Only log every 10000th operation to reduce output in high-frequency scenarios
        when (count `mod` 10000 == 0) $
            putStrLn $ "[" ++ show (loggerLevel logger) ++ "] " ++ show (loggerName logger) ++ ": " ++ show message ++ " (count: " ++ show count ++ ")"
    -- Implementation would go here