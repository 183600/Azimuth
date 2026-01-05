// Azimuth Telemetry System - New Comprehensive Test Suite
// This file contains comprehensive test cases for various telemetry system features

// Test 1: Data Compression and Decompression
test "data compression and decompression" {
  let original_data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
  
  // Test compression
  let compressed_data = Compression::compress(original_data)
  assert_true(compressed_data.length() < original_data.length())
  
  // Test decompression
  let decompressed_data = Compression::decompress(compressed_data)
  assert_eq(original_data, decompressed_data)
  
  // Test with empty data
  let empty_data = ""
  let compressed_empty = Compression::compress(empty_data)
  let decompressed_empty = Compression::decompress(compressed_empty)
  assert_eq(empty_data, decompressed_empty)
}

// Test 2: Network Communication with Telemetry
test "network communication with telemetry" {
  let client = NetworkClient::new()
  let telemetry_context = TelemetryContext::new("test_operation")
  
  // Test connection establishment
  let connection = NetworkClient::connect(client, "https://api.example.com")
  assert_true(NetworkClient::is_connected(connection))
  
  // Test request with telemetry
  let headers = [
    ("Content-Type", "application/json"),
    ("X-Trace-ID", TelemetryContext::trace_id(telemetry_context))
  ]
  let request = NetworkRequest::new("POST", "/api/v1/data", headers, Some("{\"test\": \"data\"}"))
  let response = NetworkClient::send(connection, request)
  
  // Verify telemetry was recorded
  assert_eq(NetworkResponse::status_code(response), 200)
  assert_true(TelemetryContext::has_span(telemetry_context, "network_request"))
  
  // Test connection cleanup
  NetworkClient::disconnect(connection)
  assert_false(NetworkClient::is_connected(connection))
}

// Test 3: Advanced Error Handling with Recovery
test "advanced error handling with recovery" {
  let error_handler = ErrorHandler::new()
  let retry_policy = RetryPolicy::exponential_backoff(3, 1000) // 3 retries, 1s initial delay
  
  // Test successful operation
  let result = ErrorHandler::execute_with_retry(error_handler, retry_policy, fn() {
    return "success"
  })
  match result {
    Ok(value) => assert_eq(value, "success")
    Err(_) => assert_true(false)
  }
  
  // Test operation that fails then succeeds
  let mut attempt_count = 0
  let retry_result = ErrorHandler::execute_with_retry(error_handler, retry_policy, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      return Error("temporary_failure")
    } else {
      return "success_after_retry"
    }
  })
  match retry_result {
    Ok(value) => assert_eq(value, "success_after_retry")
    Err(_) => assert_true(false)
  }
  assert_eq(attempt_count, 3)
  
  // Test operation that always fails
  let always_fail_result = ErrorHandler::execute_with_retry(error_handler, retry_policy, fn() {
    return Error("permanent_failure")
  })
  match always_fail_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "permanent_failure")
  }
}

// Test 4: Resource Management with Pooling
test "resource management with pooling" {
  let resource_pool = ResourcePool::new(5) // Maximum 5 resources
  let mut created_resources = []
  
  // Test resource acquisition
  for i in 0..=4 {
    let resource = ResourcePool::acquire(resource_pool)
    match resource {
      Some(r) => {
        created_resources = created_resources + [r]
        assert_true(Resource::is_valid(r))
      }
      None => assert_true(false)
    }
  }
  
  // Test pool exhaustion
  let exhausted_resource = ResourcePool::acquire(resource_pool)
  match exhausted_resource {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test resource release
  let released_resource = created_resources[0]
  ResourcePool::release(resource_pool, released_resource)
  
  // Test resource acquisition after release
  let new_resource = ResourcePool::acquire(resource_pool)
  match new_resource {
    Some(r) => assert_true(Resource::is_valid(r))
    None => assert_true(false)
  }
  
  // Test cleanup
  for resource in created_resources {
    ResourcePool::release(resource_pool, resource)
  }
}

// Test 5: Time Series Data Processing
test "time series data processing" {
  let time_series_processor = TimeSeriesProcessor::new()
  
  // Create sample time series data
  let data_points = [
    TimeSeriesDataPoint::new(1000L, 10.5),
    TimeSeriesDataPoint::new(2000L, 15.3),
    TimeSeriesDataPoint::new(3000L, 12.7),
    TimeSeriesDataPoint::new(4000L, 18.9),
    TimeSeriesDataPoint::new(5000L, 14.2)
  ]
  
  // Test aggregation
  let aggregated = TimeSeriesProcessor::aggregate(time_series_processor, data_points, 2000L, "average")
  assert_eq(aggregated.length(), 2)
  assert_eq(aggregated[0].value, 12.9) // Average of first 3 points
  assert_eq(aggregated[1].value, 16.55) // Average of last 2 points
  
  // Test downsampling
  let downsampled = TimeSeriesProcessor::downsample(time_series_processor, data_points, 3000L)
  assert_eq(downsampled.length(), 2)
  assert_eq(downsampled[0].timestamp, 1000L)
  assert_eq(downsampled[1].timestamp, 4000L)
  
  // Test anomaly detection
  let anomalies = TimeSeriesProcessor::detect_anomalies(time_series_processor, data_points, 2.0)
  assert_eq(anomalies.length(), 1) // 18.9 is an anomaly (> 2 std dev)
  assert_eq(anomalies[0].timestamp, 4000L)
}

// Test 6: Context Propagation Across Services
test "context propagation across services" {
  let original_context = Context::with_value(
    Context::root(),
    ContextKey::new("user_id"),
    "user123"
  )
  let trace_context = TraceContext::new("trace123", "span456", true)
  let enriched_context = Context::with_value(
    original_context,
    ContextKey::new("trace_context"),
    trace_context
  )
  
  // Test context extraction
  let user_id = Context::get(enriched_context, ContextKey::new("user_id"))
  match user_id {
    Some(id) => assert_eq(id, "user123")
    None => assert_true(false)
  }
  
  let extracted_trace = Context::get(enriched_context, ContextKey::new("trace_context"))
  match extracted_trace {
    Some(ctx) => {
      match ctx {
        TraceContext(trace_id, span_id, sampled) => {
          assert_eq(trace_id, "trace123")
          assert_eq(span_id, "span456")
          assert_true(sampled)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test context serialization
  let serialized = ContextSerializer::serialize(enriched_context)
  let deserialized = ContextSerializer::deserialize(serialized)
  
  let deserialized_user_id = Context::get(deserialized, ContextKey::new("user_id"))
  match deserialized_user_id {
    Some(id) => assert_eq(id, "user123")
    None => assert_true(false)
  }
}

// Test 7: Metrics Collection with Dynamic Dimensions
test "metrics collection with dynamic dimensions" {
  let metrics_collector = MetricsCollector::new()
  
  // Test counter with dynamic dimensions
  let counter = MetricsCollector::create_counter(metrics_collector, "requests_total")
  MetricsCollector::increment(counter, [
    ("method", "GET"),
    ("status", "200"),
    ("service", "api")
  ])
  MetricsCollector::increment(counter, [
    ("method", "POST"),
    ("status", "201"),
    ("service", "api")
  ])
  
  // Test histogram with dynamic dimensions
  let histogram = MetricsCollector::create_histogram(metrics_collector, "response_duration_ms")
  MetricsCollector::record(histogram, 150.5, [
    ("endpoint", "/api/users"),
    ("method", "GET")
  ])
  MetricsCollector::record(histogram, 89.2, [
    ("endpoint", "/api/users"),
    ("method", "GET")
  ])
  
  // Test gauge with dynamic dimensions
  let gauge = MetricsCollector::create_gauge(metrics_collector, "active_connections")
  MetricsCollector::set(gauge, 42, [
    ("server", "web-01"),
    ("environment", "production")
  ])
  
  // Test metrics retrieval
  let all_metrics = MetricsCollector::get_all(metrics_collector)
  assert_true(all_metrics.length() >= 3)
  
  // Test dimensional queries
  let api_metrics = MetricsCollector::filter_by_dimension(metrics_collector, "service", "api")
  assert_eq(api_metrics.length(), 2)
}

// Test 8: Configuration Management with Validation
test "configuration management with validation" {
  let config_manager = ConfigurationManager::new()
  
  // Define configuration schema
  let schema = ConfigSchema::new()
  ConfigSchema::add_string_field(schema, "service.name", true, None) // Required
  ConfigSchema::add_int_field(schema, "service.port", true, Some(8080)) // Required with default
  ConfigSchema::add_bool_field(schema, "debug.enabled", false, Some(false)) // Optional with default
  ConfigSchema::add_string_field(schema, "database.url", true, None) // Required
  
  // Test valid configuration
  let valid_config = [
    ("service.name", "azimuth-api"),
    ("service.port", "9090"),
    ("debug.enabled", "true"),
    ("database.url", "postgresql://localhost:5432/azimuth")
  ]
  
  let validated_config = ConfigurationManager::validate_and_load(config_manager, schema, valid_config)
  match validated_config {
    Ok(config) => {
      assert_eq(ConfigurationManager::get_string(config, "service.name"), "azimuth-api")
      assert_eq(ConfigurationManager::get_int(config, "service.port"), 9090)
      assert_true(ConfigurationManager::get_bool(config, "debug.enabled"))
      assert_eq(ConfigurationManager::get_string(config, "database.url"), "postgresql://localhost:5432/azimuth")
    }
    Err(_) => assert_true(false)
  }
  
  // Test invalid configuration (missing required field)
  let invalid_config = [
    ("service.name", "azimuth-api"),
    ("debug.enabled", "true")
    // Missing service.port and database.url
  ]
  
  let invalid_result = ConfigurationManager::validate_and_load(config_manager, schema, invalid_config)
  match invalid_result {
    Ok(_) => assert_true(false)
    Err(errors) => assert_true(errors.length() >= 2)
  }
}

// Test 9: Log Correlation and Aggregation
test "log correlation and aggregation" {
  let log_processor = LogProcessor::new()
  let trace_id = "trace123"
  let span_id = "span456"
  
  // Create correlated log entries
  let log_entries = [
    LogEntry::new(Info, "Starting operation", Some(trace_id), Some(span_id)),
    LogEntry::new(Debug, "Processing data", Some(trace_id), Some(span_id)),
    LogEntry::new(Warning, "Slow operation detected", Some(trace_id), Some(span_id)),
    LogEntry::new(Info, "Operation completed", Some(trace_id), Some(span_id))
  ]
  
  // Test log correlation
  let correlated_logs = LogProcessor::correlate_by_trace(log_processor, trace_id)
  assert_eq(correlated_logs.length(), 4)
  
  for log in correlated_logs {
    assert_eq(LogEntry::trace_id(log), Some(trace_id))
    assert_eq(LogEntry::span_id(log), Some(span_id))
  }
  
  // Test log aggregation by severity
  let aggregated_by_severity = LogProcessor::aggregate_by_severity(log_processor, log_entries)
  assert_eq(aggregated_by_severity.get(Info), Some(2))
  assert_eq(aggregated_by_severity.get(Debug), Some(1))
  assert_eq(aggregated_by_severity.get(Warning), Some(1))
  
  // Test log pattern matching
  let pattern_logs = LogProcessor::match_pattern(log_processor, log_entries, "operation")
  assert_eq(pattern_logs.length(), 2) // "Starting operation" and "Operation completed"
}

// Test 10: Performance Benchmarking with Telemetry
test "performance benchmarking with telemetry" {
  let benchmark = PerformanceBenchmark::new()
  let telemetry_recorder = TelemetryRecorder::new()
  
  // Define benchmark scenarios
  let scenarios = [
    BenchmarkScenario::new("memory_allocation", fn() {
      let mut data = []
      for i in 0..=1000 {
        data = data + [i]
      }
      return data.length()
    }),
    BenchmarkScenario::new("string_processing", fn() {
      let result = ""
      for i in 0..=100 {
        result = result + "test"
      }
      return result.length()
    }),
    BenchmarkScenario::new("mathematical_operations", fn() {
      let mut result = 0.0
      for i in 0..=10000 {
        result = result + (i.to_float() * 1.5).sqrt()
      }
      return result
    })
  ]
  
  // Execute benchmarks with telemetry
  let benchmark_results = []
  for scenario in scenarios {
    let result = PerformanceBenchmark::execute_with_telemetry(benchmark, scenario, telemetry_recorder)
    benchmark_results = benchmark_results + [result]
    
    // Verify telemetry was recorded
    assert_true(TelemetryRecorder::has_metric(telemetry_recorder, BenchmarkScenario::name(scenario)))
    assert_true(TelemetryRecorder::has_span(telemetry_recorder, BenchmarkScenario::name(scenario)))
  }
  
  // Test performance analysis
  let analysis = PerformanceBenchmark::analyze_results(benchmark_results)
  assert_true(analysis.length() == 3)
  
  // Test baseline comparison
  let baseline = [
    ("memory_allocation", 1000), // 1000 microseconds
    ("string_processing", 500),
    ("mathematical_operations", 2000)
  ]
  
  let comparison = PerformanceBenchmark::compare_with_baseline(benchmark_results, baseline)
  for (scenario_name, result) in comparison {
    assert_true(result.within_threshold(0.2)) // Within 20% of baseline
  }
}