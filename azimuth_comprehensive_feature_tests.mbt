// Azimuth Telemetry System - Comprehensive Feature Tests
// This file contains comprehensive test cases covering various aspects of the telemetry system

// Test 1: Distributed Tracing
test "distributed tracing functionality" {
  let tracer = Tracer::new("azimuth-service")
  
  // Test span creation
  let root_span = Tracer::start_span(tracer, "root-operation")
  assert_true(Tracer::is_active(root_span))
  assert_eq(Tracer::get_span_name(root_span), "root-operation")
  
  // Test span context propagation
  let context = Tracer::get_span_context(root_span)
  assert_true(context.trace_id.length() > 0)
  assert_true(context.span_id.length() > 0)
  
  // Test child span creation
  let child_span = Tracer::start_child_span(tracer, "child-operation", root_span)
  assert_eq(Tracer::get_parent_span_id(child_span), context.span_id)
  assert_eq(Tracer::get_trace_id(child_span), context.trace_id)
  
  // Test span attributes
  Tracer::set_attribute(tracer, child_span, "user.id", "12345")
  Tracer::set_attribute(tracer, child_span, "operation.type", "database")
  
  let attributes = Tracer::get_attributes(tracer, child_span)
  assert_eq(attributes.get("user.id"), Some("12345"))
  assert_eq(attributes.get("operation.type"), Some("database"))
  
  // Test span events
  Tracer::add_event(tracer, child_span, "query.start", [("query", "SELECT * FROM users")])
  Tracer::add_event(tracer, child_span, "query.complete", [("rows", "42")])
  
  let events = Tracer::get_events(tracer, child_span)
  assert_eq(events.length(), 2)
  assert_eq(events[0].name, "query.start")
  assert_eq(events[1].name, "query.complete")
  
  // Test span status
  Tracer::set_status(tracer, child_span, SpanStatus::Ok, "Operation completed successfully")
  assert_eq(Tracer::get_status(child_span), SpanStatus::Ok)
  assert_eq(Tracer::get_status_message(child_span), "Operation completed successfully")
  
  // Test span completion
  Tracer::end_span(tracer, child_span)
  Tracer::end_span(tracer, root_span)
  
  assert_false(Tracer::is_active(child_span))
  assert_false(Tracer::is_active(root_span))
  
  // Test span export
  let exporter = SpanExporter::new("jaeger")
  let exported_spans = SpanExporter::export_finished_spans(exporter)
  assert_eq(exported_spans.length(), 2)
}

// Test 2: Performance Monitoring
test "performance monitoring capabilities" {
  let monitor = PerformanceMonitor::new()
  
  // Test metric registration
  PerformanceMonitor::register_counter(monitor, "requests.total", "Total number of requests")
  PerformanceMonitor::register_histogram(monitor, "request.duration", "Request duration in milliseconds")
  PerformanceMonitor::register_gauge(monitor, "memory.usage", "Current memory usage")
  
  assert_true(PerformanceMonitor::metric_exists(monitor, "requests.total"))
  assert_true(PerformanceMonitor::metric_exists(monitor, "request.duration"))
  assert_true(PerformanceMonitor::metric_exists(monitor, "memory.usage"))
  
  // Test counter operations
  PerformanceMonitor::counter_add(monitor, "requests.total", 1)
  PerformanceMonitor::counter_add(monitor, "requests.total", 5)
  
  let counter_value = PerformanceMonitor::get_counter_value(monitor, "requests.total")
  assert_eq(counter_value, 6)
  
  // Test histogram operations
  PerformanceMonitor::histogram_record(monitor, "request.duration", 120.5)
  PerformanceMonitor::histogram_record(monitor, "request.duration", 85.3)
  PerformanceMonitor::histogram_record(monitor, "request.duration", 200.1)
  
  let histogram_stats = PerformanceMonitor::get_histogram_stats(monitor, "request.duration")
  assert_eq(histogram_stats.count, 3)
  assert_eq(histogram_stats.sum, 405.9)
  assert_eq(histogram_stats.min, 85.3)
  assert_eq(histogram_stats.max, 200.1)
  
  // Test gauge operations
  PerformanceMonitor::gauge_set(monitor, "memory.usage", 1024.0)
  PerformanceMonitor::gauge_set(monitor, "memory.usage", 1536.0)
  
  let gauge_value = PerformanceMonitor::get_gauge_value(monitor, "memory.usage")
  assert_eq(gauge_value, 1536.0)
  
  // Test metric aggregation
  let metrics = PerformanceMonitor::collect_all_metrics(monitor)
  assert_eq(metrics.length(), 3)
  
  // Test metric export
  let exporter = MetricsExporter::new("prometheus")
  let exported_metrics = MetricsExporter::format_metrics(exporter, metrics)
  assert_true(exported_metrics.contains("requests_total 6"))
  assert_true(exported_metrics.contains("memory_usage 1536"))
}

// Test 3: Data Aggregation
test "data aggregation functionality" {
  let aggregator = DataAggregator::new()
  
  // Test time series data creation
  let time_series = TimeSeries::new("cpu.usage")
  TimeSeries::add_point(time_series, 1640995200, 45.2)  // 2022-01-01 00:00:00
  TimeSeries::add_point(time_series, 1640995260, 48.7)  // 2022-01-01 00:01:00
  TimeSeries::add_point(time_series, 1640995320, 52.1)  // 2022-01-01 00:02:00
  TimeSeries::add_point(time_series, 1640995380, 49.8)  // 2022-01-01 00:03:00
  TimeSeries::add_point(time_series, 1640995440, 46.3)  // 2022-01-01 00:04:00
  
  assert_eq(TimeSeries::get_point_count(time_series), 5)
  
  // Test average aggregation
  let avg_result = DataAggregator::average(aggregator, time_series)
  assert_eq(avg_result, 48.42)
  
  // Test max/min aggregation
  let max_result = DataAggregator::max(aggregator, time_series)
  let min_result = DataAggregator::min(aggregator, time_series)
  assert_eq(max_result, 52.1)
  assert_eq(min_result, 45.2)
  
  // Test sum aggregation
  let sum_result = DataAggregator::sum(aggregator, time_series)
  assert_eq(sum_result, 242.1)
  
  // Test time window aggregation
  let window_start = 1640995200
  let window_end = 1640995380  // 3 minutes window
  let windowed_series = DataAggregator::filter_by_time_window(aggregator, time_series, window_start, window_end)
  
  assert_eq(TimeSeries::get_point_count(windowed_series), 4)
  
  // Test downsampling
  let downsampled = DataAggregator::downsample(aggregator, time_series, 120)  // 2-minute intervals
  assert_eq(TimeSeries::get_point_count(downsampled), 3)
  
  // Test percentiles
  let p95 = DataAggregator::percentile(aggregator, time_series, 95)
  let p50 = DataAggregator::percentile(aggregator, time_series, 50)
  
  assert_eq(p50, 48.7)  // Median
  assert_eq(p95, 52.1)  // 95th percentile
  
  // Test trend analysis
  let trend = DataAggregator::calculate_trend(aggregator, time_series)
  assert_true(trend.slope > -1.0 and trend.slope < 1.0)  // Should be relatively stable
  
  // Test anomaly detection
  let anomaly_series = TimeSeries::new("memory.usage")
  TimeSeries::add_point(anomaly_series, 1640995200, 1024.0)
  TimeSeries::add_point(anomaly_series, 1640995260, 1056.0)
  TimeSeries::add_point(anomaly_series, 1640995320, 1088.0)
  TimeSeries::add_point(anomaly_series, 1640995380, 5120.0)  // Anomaly
  TimeSeries::add_point(anomaly_series, 1640995440, 1120.0)
  
  let anomalies = DataAggregator::detect_anomalies(aggregator, anomaly_series, 2.0)  // 2 standard deviations
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0].timestamp, 1640995380)
  assert_eq(anomalies[0].value, 5120.0)
}

// Test 4: Error Recovery
test "error recovery mechanisms" {
  let recovery_manager = ErrorRecoveryManager::new()
  
  // Test error registration
  ErrorRecoveryManager::register_error_type(recovery_manager, "database.connection", ErrorSeverity::High)
  ErrorRecoveryManager::register_error_type(recovery_manager, "network.timeout", ErrorSeverity::Medium)
  ErrorRecoveryManager::register_error_type(recovery_manager, "validation.failure", ErrorSeverity::Low)
  
  // Test error occurrence tracking
  ErrorRecoveryManager::record_error(recovery_manager, "database.connection", "Connection pool exhausted")
  ErrorRecoveryManager::record_error(recovery_manager, "database.connection", "Connection timeout")
  ErrorRecoveryManager::record_error(recovery_manager, "network.timeout", "Request timeout after 30s")
  
  let db_error_count = ErrorRecoveryManager::get_error_count(recovery_manager, "database.connection")
  let network_error_count = ErrorRecoveryManager::get_error_count(recovery_manager, "network.timeout")
  
  assert_eq(db_error_count, 2)
  assert_eq(network_error_count, 1)
  
  // Test recovery strategy registration
  ErrorRecoveryManager::register_recovery_strategy(recovery_manager, "database.connection", RecoveryStrategy::Retry, 3)
  ErrorRecoveryManager::register_recovery_strategy(recovery_manager, "network.timeout", RecoveryStrategy::CircuitBreaker, 5)
  
  // Test error recovery execution
  let db_recovery_result = ErrorRecoveryManager::execute_recovery(recovery_manager, "database.connection")
  assert_true(db_recovery_result.attempted)
  assert_eq(db_recovery_result.strategy, RecoveryStrategy::Retry)
  assert_eq(db_recovery_result.max_attempts, 3)
  
  // Test circuit breaker functionality
  let circuit_breaker = CircuitBreaker::new("database.service", 5, 60000)  // 5 failures, 60s timeout
  
  // Initial state should be closed
  assert_eq(CircuitBreaker::get_state(circuit_breaker), CircuitBreakerState::Closed)
  
  // Record failures to trigger circuit breaker
  for i in 1..=5 {
    CircuitBreaker::record_failure(circuit_breaker)
  }
  
  // Circuit should now be open
  assert_eq(CircuitBreaker::get_state(circuit_breaker), CircuitBreakerState::Open)
  
  // Test that calls are blocked when circuit is open
  let call_result = CircuitBreaker::execute(circuit_breaker, fn() {
    return "success"
  })
  
  assert_false(call_result.success)
  assert_eq(call_result.reason, CallFailureReason::CircuitOpen)
  
  // Test retry mechanism
  let retry_config = RetryConfig::new(3, 1000, 2.0)  // 3 attempts, 1s initial delay, 2x backoff
  let attempt_count = { mut count: 0 }
  
  let retry_result = Retry::execute(retry_config, fn() {
    attempt_count.count = attempt_count.count + 1
    if attempt_count.count < 3 {
      return Err("temporary failure")
    } else {
      return Ok("success after retries")
    }
  })
  
  match retry_result {
    Ok(value) => assert_eq(value, "success after retries")
    Err(_) => assert_true(false)
  }
  
  assert_eq(attempt_count.count, 3)
  
  // Test fallback mechanism
  let fallback_result = Fallback::execute(
    fn() { Err("primary operation failed") },
    fn() { Ok("fallback operation succeeded") }
  )
  
  match fallback_result {
    Ok(value) => assert_eq(value, "fallback operation succeeded")
    Err(_) => assert_true(false)
  }
  
  // Test error recovery statistics
  let stats = ErrorRecoveryManager::get_statistics(recovery_manager)
  assert_eq(stats.total_errors, 3)
  assert_eq(stats.recovery_attempts, 1)
  assert_true(stats.recovery_rate > 0.0)
}

// Test 5: Configuration Management
test "configuration management system" {
  let config_manager = ConfigurationManager::new()
  
  // Test configuration loading
  let config_data = [
    ("service.name", "azimuth-telemetry"),
    ("service.version", "1.0.0"),
    ("service.port", "8080"),
    ("database.host", "localhost"),
    ("database.port", "5432"),
    ("database.name", "azimuth_db"),
    ("logging.level", "info"),
    ("logging.format", "json")
  ]
  
  for (key, value) in config_data {
    ConfigurationManager::set(config_manager, key, value)
  }
  
  // Test configuration retrieval
  assert_eq(ConfigurationManager::get(config_manager, "service.name"), Some("azimuth-telemetry"))
  assert_eq(ConfigurationManager::get(config_manager, "service.version"), Some("1.0.0"))
  assert_eq(ConfigurationManager::get(config_manager, "service.port"), Some("8080"))
  assert_eq(ConfigurationManager::get(config_manager, "database.host"), Some("localhost"))
  
  // Test configuration type conversion
  let port = ConfigurationManager::get_int(config_manager, "service.port")
  assert_eq(port, Some(8080))
  
  let db_port = ConfigurationManager::get_int(config_manager, "database.port")
  assert_eq(db_port, Some(5432))
  
  // Test configuration with defaults
  let missing_with_default = ConfigurationManager::get_with_default(config_manager, "missing.key", "default_value")
  assert_eq(missing_with_default, "default_value")
  
  // Test configuration validation
  let validation_rules = [
    ValidationRule::required("service.name"),
    ValidationRule::required("service.version"),
    ValidationRule::port_range("service.port", 1024, 65535),
    ValidationRule::enum_value("logging.level", ["debug", "info", "warn", "error"])
  ]
  
  let validation_result = ConfigurationManager::validate(config_manager, validation_rules)
  assert_true(validation_result.valid)
  
  // Test configuration with invalid data
  ConfigurationManager::set(config_manager, "service.port", "invalid_port")
  
  let invalid_validation_result = ConfigurationManager::validate(config_manager, validation_rules)
  assert_false(invalid_validation_result.valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // Restore valid port
  ConfigurationManager::set(config_manager, "service.port", "8080")
  
  // Test configuration watching
  let change_events = { mut events: [] }
  let watcher = ConfigurationManager::watch(config_manager, "service.name", fn(old_value, new_value) {
    change_events.events = change_events.events.push((old_value, new_value))
  })
  
  ConfigurationManager::set(config_manager, "service.name", "updated-service")
  
  assert_eq(change_events.events.length(), 1)
  assert_eq(change_events.events[0], ("azimuth-telemetry", "updated-service"))
  
  // Test configuration groups
  let service_config = ConfigurationManager::get_group(config_manager, "service")
  assert_eq(service_config.length(), 3)
  assert_true(service_config.contains(("name", "updated-service")))
  assert_true(service_config.contains(("version", "1.0.0")))
  assert_true(service_config.contains(("port", "8080")))
  
  let database_config = ConfigurationManager::get_group(config_manager, "database")
  assert_eq(database_config.length(), 3)
  assert_true(database_config.contains(("host", "localhost")))
  assert_true(database_config.contains(("port", "5432")))
  assert_true(database_config.contains(("name", "azimuth_db")))
  
  // Test configuration export/import
  let exported_config = ConfigurationManager::export(config_manager)
  assert_true(exported_config.contains("service.name"))
  assert_true(exported_config.contains("database.host"))
  
  let new_config_manager = ConfigurationManager::new()
  ConfigurationManager::import(new_config_manager, exported_config)
  
  assert_eq(ConfigurationManager::get(new_config_manager, "service.name"), Some("updated-service"))
  assert_eq(ConfigurationManager::get(new_config_manager, "database.host"), Some("localhost"))
  
  // Test configuration environment variable override
  ConfigurationManager::set_env_override(config_manager, "service.port", "9090")
  
  let overridden_port = ConfigurationManager::get(config_manager, "service.port")
  assert_eq(overridden_port, Some("9090"))
  
  // Test configuration file reload
  let temp_config_file = "/tmp/azimuth_config.json"
  ConfigurationManager::save_to_file(config_manager, temp_config_file)
  
  // Modify config in memory
  ConfigurationManager::set(config_manager, "service.name", "memory-modified")
  
  // Reload from file
  ConfigurationManager::load_from_file(config_manager, temp_config_file)
  
  let reloaded_name = ConfigurationManager::get(config_manager, "service.name")
  assert_eq(reloaded_name, Some("updated-service"))  // Should be restored from file
}

// Test 6: Caching Strategies
test "caching strategies and mechanisms" {
  let cache_manager = CacheManager::new()
  
  // Test LRU cache creation
  let lru_cache = CacheManager::create_lru_cache(cache_manager, "user_data", 100)  // 100 items capacity
  
  // Test cache operations
  CacheManager::put(cache_manager, lru_cache, "user:123", { name: "John Doe", email: "john@example.com" })
  CacheManager::put(cache_manager, lru_cache, "user:456", { name: "Jane Smith", email: "jane@example.com" })
  CacheManager::put(cache_manager, lru_cache, "user:789", { name: "Bob Johnson", email: "bob@example.com" })
  
  // Test cache retrieval
  let user123 = CacheManager::get(cache_manager, lru_cache, "user:123")
  match user123 {
    Some(user) => {
      assert_eq(user.name, "John Doe")
      assert_eq(user.email, "john@example.com")
    }
    None => assert_true(false)
  }
  
  let missing_user = CacheManager::get(cache_manager, lru_cache, "user:999")
  assert_eq(missing_user, None)
  
  // Test cache statistics
  let stats = CacheManager::get_stats(cache_manager, lru_cache)
  assert_eq(stats.size, 3)
  assert_eq(stats.hits, 1)
  assert_eq(stats.misses, 1)
  
  // Test TTL cache creation
  let ttl_cache = CacheManager::create_ttl_cache(cache_manager, "session_data", 60)  // 60 seconds TTL
  
  CacheManager::put_with_ttl(cache_manager, ttl_cache, "session:abc123", { user_id: "123", expires: 1640995800 }, 30)
  
  let session = CacheManager::get(cache_manager, ttl_cache, "session:abc123")
  match session {
    Some(s) => assert_eq(s.user_id, "123")
    None => assert_true(false)
  }
  
  // Test cache expiration (simulate time passing)
  CacheManager::advance_time(cache_manager, 35)  // Advance 35 seconds
  
  let expired_session = CacheManager::get(cache_manager, ttl_cache, "session:abc123")
  assert_eq(expired_session, None)  // Should be expired
  
  // Test cache eviction
  let small_cache = CacheManager::create_lru_cache(cache_manager, "small_cache", 2)
  
  CacheManager::put(cache_manager, small_cache, "key1", "value1")
  CacheManager::put(cache_manager, small_cache, "key2", "value2")
  CacheManager::put(cache_manager, small_cache, "key3", "value3")  // Should evict key1
  
  let evicted_key = CacheManager::get(cache_manager, small_cache, "key1")
  assert_eq(evicted_key, None)  // Should be evicted
  
  let retained_key = CacheManager::get(cache_manager, small_cache, "key2")
  assert_eq(retained_key, Some("value2"))  // Should still be there
  
  // Test cache warming
  let warm_cache = CacheManager::create_lru_cache(cache_manager, "product_data", 50)
  
  let warm_function = fn(key: String) {
    match key {
      "product:1" => Some({ name: "Product 1", price: 10.99 })
      "product:2" => Some({ name: "Product 2", price: 20.99 })
      "product:3" => Some({ name: "Product 3", price: 30.99 })
      _ => None
    }
  }
  
  CacheManager::warm(cache_manager, warm_cache, ["product:1", "product:2", "product:3"], warm_function)
  
  let warmed_product = CacheManager::get(cache_manager, warm_cache, "product:2")
  match warmed_product {
    Some(product) => assert_eq(product.name, "Product 2")
    None => assert_true(false)
  }
  
  // Test cache invalidation
  let invalidation_cache = CacheManager::create_lru_cache(cache_manager, "config_data", 20)
  
  CacheManager::put(cache_manager, invalidation_cache, "config:app", { version: "1.0.0", debug: false })
  CacheManager::put(cache_manager, invalidation_cache, "config:db", { host: "localhost", port: 5432 })
  
  // Invalidate by pattern
  CacheManager::invalidate_by_pattern(cache_manager, invalidation_cache, "config:*")
  
  let invalidated_app = CacheManager::get(cache_manager, invalidation_cache, "config:app")
  let invalidated_db = CacheManager::get(cache_manager, invalidation_cache, "config:db")
  
  assert_eq(invalidated_app, None)
  assert_eq(invalidated_db, None)
  
  // Test distributed cache (simulation)
  let distributed_cache = CacheManager::create_distributed_cache(cache_manager, "shared_data", "redis://localhost:6379")
  
  CacheManager::put(cache_manager, distributed_cache, "shared:counter", 42)
  
  let local_counter = CacheManager::get(cache_manager, distributed_cache, "shared:counter")
  assert_eq(local_counter, Some(42))
  
  // Test cache synchronization
  CacheManager::put(cache_manager, distributed_cache, "shared:counter", 100)  // Update value
  
  let synchronized_counter = CacheManager::sync_get(cache_manager, distributed_cache, "shared:counter")
  assert_eq(synchronized_counter, Some(100))
}

// Test 7: Data Serialization
test "data serialization and deserialization" {
  let serialization_manager = SerializationManager::new()
  
  // Test JSON serialization
  let test_user = {
    id: 123,
    name: "John Doe",
    email: "john@example.com",
    addresses: [
      { street: "123 Main St", city: "New York", zip: "10001" },
      { street: "456 Oak Ave", city: "Boston", zip: "02108" }
    ],
    active: true,
    created_at: 1640995200
  }
  
  let json_data = SerializationManager::to_json(serialization_manager, test_user)
  assert_true(json_data.contains("\"id\":123"))
  assert_true(json_data.contains("\"name\":\"John Doe\""))
  assert_true(json_data.contains("\"email\":\"john@example.com\""))
  
  // Test JSON deserialization
  let deserialized_user = SerializationManager::from_json(serialization_manager, json_data)
  assert_eq(deserialized_user.id, test_user.id)
  assert_eq(deserialized_user.name, test_user.name)
  assert_eq(deserialized_user.email, test_user.email)
  assert_eq(deserialized_user.addresses.length(), test_user.addresses.length())
  
  // Test binary serialization
  let binary_data = SerializationManager::to_binary(serialization_manager, test_user)
  assert_true(binary_data.length() > 0)
  
  let binary_deserialized = SerializationManager::from_binary(serialization_manager, binary_data)
  assert_eq(binary_deserialized.id, test_user.id)
  assert_eq(binary_deserialized.name, test_user.name)
  
  // Test protocol buffer serialization
  SerializationManager::register_protobuf_schema(serialization_manager, "User", user_schema)
  
  let protobuf_data = SerializationManager::to_protobuf(serialization_manager, test_user, "User")
  assert_true(protobuf_data.length() > 0)
  
  let protobuf_deserialized = SerializationManager::from_protobuf(serialization_manager, protobuf_data, "User")
  assert_eq(protobuf_deserialized.id, test_user.id)
  assert_eq(protobuf_deserialized.name, test_user.name)
  
  // Test custom serialization
  let custom_serializer = CustomSerializer::new()
  CustomSerializer::register_serializer(custom_serializer, "TelemetrySpan", serialize_span, deserialize_span)
  
  let telemetry_span = {
    trace_id: "trace-123",
    span_id: "span-456",
    parent_span_id: Some("span-789"),
    operation_name: "database.query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users"),
      ("service.name", "payment-service")
    ]
  }
  
  let custom_serialized = CustomSerializer::serialize(custom_serializer, telemetry_span, "TelemetrySpan")
  let custom_deserialized = CustomSerializer::deserialize(custom_serializer, custom_serialized, "TelemetrySpan")
  
  assert_eq(custom_deserialized.trace_id, telemetry_span.trace_id)
  assert_eq(custom_deserialized.span_id, telemetry_span.span_id)
  assert_eq(custom_deserialized.operation_name, telemetry_span.operation_name)
  
  // Test serialization with compression
  let large_data = "x".repeat(10000)  // 10KB of data
  let compressed_json = SerializationManager::to_json_compressed(serialization_manager, large_data)
  
  assert_true(compressed_json.length() < large_data.length())  // Should be smaller
  
  let decompressed_data = SerializationManager::from_json_compressed(serialization_manager, compressed_json)
  assert_eq(decompressed_data, large_data)
  
  // Test streaming serialization
  let stream = SerializationManager::create_json_stream(serialization_manager)
  
  for i in 1..=100 {
    let item = { id: i, name: "Item " + i.to_string(), value: i * 2.5 }
    SerializationManager::stream_write(serialization_manager, stream, item)
  }
  
  let stream_data = SerializationManager::stream_finish(serialization_manager, stream)
  assert_true(stream_data.length() > 0)
  
  // Test schema validation during deserialization
  let valid_json = "{\"id\":123,\"name\":\"Test\",\"email\":\"test@example.com\"}"
  let invalid_json = "{\"id\":\"not_a_number\",\"name\":\"Test\",\"email\":\"test@example.com\"}"
  
  let valid_result = SerializationManager::from_json_with_validation(serialization_manager, valid_json, user_schema)
  match valid_result {
    Ok(user) => assert_eq(user.id, 123)
    Err(_) => assert_true(false)
  }
  
  let invalid_result = SerializationManager::from_json_with_validation(serialization_manager, invalid_json, user_schema)
  match invalid_result {
    Ok(_) => assert_true(false)  // Should fail validation
    Err(validation_error) => assert_true(validation_error.contains("id"))
  }
  
  // Test serialization format conversion
  let json_data = SerializationManager::to_json(serialization_manager, test_user)
  let xml_data = SerializationManager::convert_format(serialization_manager, json_data, "json", "xml")
  
  assert_true(xml_data.contains("<id>123</id>"))
  assert_true(xml_data.contains("<name>John Doe</name>"))
  
  let csv_data = SerializationManager::convert_format(serialization_manager, json_data, "json", "csv")
  assert_true(csv_data.contains("id,name,email"))
  assert_true(csv_data.contains("123,John Doe,john@example.com"))
}

// Test 8: Security Authentication
test "security authentication and authorization" {
  let security_manager = SecurityManager::new()
  
  // Test user authentication
  let user_credentials = {
    username: "john.doe",
    password: "secure_password_123"
  }
  
  // Register user
  let user_id = SecurityManager::register_user(security_manager, user_credentials.username, user_credentials.password)
  assert_true(user_id.length() > 0)
  
  // Test successful authentication
  let auth_result = SecurityManager::authenticate(security_manager, user_credentials.username, user_credentials.password)
  match auth_result {
    Ok(token) => {
      assert_true(token.length() > 0)
      assert_true(SecurityManager::is_token_valid(security_manager, token))
    }
    Err(_) => assert_true(false)
  }
  
  // Test failed authentication
  let failed_auth = SecurityManager::authenticate(security_manager, user_credentials.username, "wrong_password")
  match failed_auth {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, AuthenticationError::InvalidCredentials)
  }
  
  // Test token validation
  let valid_token = match auth_result {
    Ok(token) => token
    Err(_) => ""
  }
  
  assert_true(SecurityManager::is_token_valid(security_manager, valid_token))
  assert_false(SecurityManager::is_token_valid(security_manager, "invalid_token"))
  
  // Test token refresh
  let refreshed_token = SecurityManager::refresh_token(security_manager, valid_token)
  assert_true(refreshed_token.length() > 0)
  assert_not_eq(refreshed_token, valid_token)  // Should be different
  
  // Test token revocation
  SecurityManager::revoke_token(security_manager, valid_token)
  assert_false(SecurityManager::is_token_valid(security_manager, valid_token))
  
  // Test role-based authorization
  SecurityManager::create_role(security_manager, "admin", ["user.read", "user.write", "system.admin"])
  SecurityManager::create_role(security_manager, "user", ["user.read"])
  
  let admin_user_id = SecurityManager::register_user(security_manager, "admin.user", "admin_password")
  let regular_user_id = SecurityManager::register_user(security_manager, "regular.user", "user_password")
  
  SecurityManager::assign_role(security_manager, admin_user_id, "admin")
  SecurityManager::assign_role(security_manager, regular_user_id, "user")
  
  // Test permission checks
  assert_true(SecurityManager::has_permission(security_manager, admin_user_id, "user.write"))
  assert_true(SecurityManager::has_permission(security_manager, admin_user_id, "system.admin"))
  assert_false(SecurityManager::has_permission(security_manager, regular_user_id, "user.write"))
  assert_true(SecurityManager::has_permission(security_manager, regular_user_id, "user.read"))
  
  // Test API key authentication
  let api_key = SecurityManager::generate_api_key(security_manager, "service-account", ["metrics.read", "metrics.write"])
  assert_true(api_key.length() > 0)
  
  let api_key_validation = SecurityManager::validate_api_key(security_manager, api_key)
  match api_key_validation {
    Ok(key_info) => {
      assert_eq(key_info.account_id, "service-account")
      assert_true(key_info.permissions.contains("metrics.read"))
      assert_true(key_info.permissions.contains("metrics.write"))
    }
    Err(_) => assert_true(false)
  }
  
  // Test session management
  let session_id = SecurityManager::create_session(security_manager, regular_user_id)
  assert_true(session_id.length() > 0)
  
  assert_true(SecurityManager::is_session_valid(security_manager, session_id))
  
  SecurityManager::extend_session(security_manager, session_id, 3600)  // Extend by 1 hour
  assert_true(SecurityManager::is_session_valid(security_manager, session_id))
  
  SecurityManager::destroy_session(security_manager, session_id)
  assert_false(SecurityManager::is_session_valid(security_manager, session_id))
  
  // Test multi-factor authentication
  SecurityManager::enable_mfa(security_manager, admin_user_id)
  
  let mfa_secret = SecurityManager::generate_mfa_secret(security_manager, admin_user_id)
  assert_true(mfa_secret.length() > 0)
  
  let mfa_token = SecurityManager::generate_mfa_token(security_manager, mfa_secret)
  assert_true(mfa_token.length() == 6)  // 6-digit code
  
  assert_true(SecurityManager::verify_mfa_token(security_manager, admin_user_id, mfa_token))
  assert_false(SecurityManager::verify_mfa_token(security_manager, admin_user_id, "000000"))
  
  // Test password policies
  let password_policy = PasswordPolicy::new(
    min_length = 8,
    require_uppercase = true,
    require_lowercase = true,
    require_digits = true,
    require_special_chars = true,
    max_age_days = 90
  )
  
  SecurityManager::set_password_policy(security_manager, password_policy)
  
  let weak_password_validation = SecurityManager::validate_password(security_manager, "weak")
  match weak_password_validation {
    Ok(_) => assert_true(false)
    Err(errors) => assert_true(errors.length() > 0)
  }
  
  let strong_password_validation = SecurityManager::validate_password(security_manager, "StrongP@ssw0rd")
  match strong_password_validation {
    Ok(_) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  // Test audit logging
  let audit_log = SecurityManager::get_audit_log(security_manager)
  
  SecurityManager::log_security_event(security_manager, SecurityEvent::LoginSuccess, regular_user_id)
  SecurityManager::log_security_event(security_manager, SecurityEvent::PermissionDenied, regular_user_id)
  SecurityManager::log_security_event(security_manager, SecurityEvent::PasswordChange, admin_user_id)
  
  let updated_log = SecurityManager::get_audit_log(security_manager)
  assert_eq(updated_log.length(), audit_log.length() + 3)
  
  let last_event = updated_log[updated_log.length() - 1]
  assert_eq(last_event.event_type, SecurityEvent::PasswordChange)
  assert_eq(last_event.user_id, admin_user_id)
}

// Test 9: Resource Limiting
test "resource limiting and throttling" {
  let resource_limiter = ResourceLimiter::new()
  
  // Test rate limiting
  let rate_limiter = ResourceLimiter::create_rate_limiter(resource_limiter, "api.requests", 100, 60)  // 100 requests per minute
  
  // Test rate limit consumption
  for i in 1..=50 {
    let allowed = ResourceLimiter::check_rate_limit(resource_limiter, rate_limiter)
    assert_true(allowed)  // First 50 should be allowed
  }
  
  // Test rate limit exhaustion
  for i in 51..=100 {
    let allowed = ResourceLimiter::check_rate_limit(resource_limiter, rate_limiter)
    assert_true(allowed)  // Up to 100 should be allowed
  }
  
  // Next request should be denied
  let denied = ResourceLimiter::check_rate_limit(resource_limiter, rate_limiter)
  assert_false(denied)
  
  // Test rate limit reset
  ResourceLimiter::advance_time(resource_limiter, 61)  // Advance 61 seconds
  
  let allowed_after_reset = ResourceLimiter::check_rate_limit(resource_limiter, rate_limiter)
  assert_true(allowed_after_reset)
  
  // Test concurrent rate limiting
  let concurrent_limiter = ResourceLimiter::create_concurrent_limiter(resource_limiter, "file.operations", 5)  // 5 concurrent operations
  
  let operation1 = ResourceLimiter::acquire_concurrent(resource_limiter, concurrent_limiter)
  let operation2 = ResourceLimiter::acquire_concurrent(resource_limiter, concurrent_limiter)
  let operation3 = ResourceLimiter::acquire_concurrent(resource_limiter, concurrent_limiter)
  let operation4 = ResourceLimiter::acquire_concurrent(resource_limiter, concurrent_limiter)
  let operation5 = ResourceLimiter::acquire_concurrent(resource_limiter, concurrent_limiter)
  
  // All 5 should succeed
  assert_true(operation1.success)
  assert_true(operation2.success)
  assert_true(operation3.success)
  assert_true(operation4.success)
  assert_true(operation5.success)
  
  // 6th should fail
  let operation6 = ResourceLimiter::acquire_concurrent(resource_limiter, concurrent_limiter)
  assert_false(operation6.success)
  
  // Release one operation
  ResourceLimiter::release_concurrent(resource_limiter, concurrent_limiter, operation1.permit)
  
  // Now 6th should succeed
  let operation7 = ResourceLimiter::acquire_concurrent(resource_limiter, concurrent_limiter)
  assert_true(operation7.success)
  
  // Test resource quotas
  ResourceLimiter::set_quota(resource_limiter, "memory.usage", 1024 * 1024 * 1024)  // 1GB
  ResourceLimiter::set_quota(resource_limiter, "disk.usage", 100 * 1024 * 1024 * 1024)  // 100GB
  ResourceLimiter::set_quota(resource_limiter, "cpu.time", 3600)  // 1 hour in seconds
  
  // Test quota consumption
  let memory_allocation1 = ResourceLimiter::consume_quota(resource_limiter, "memory.usage", 512 * 1024 * 1024)  // 512MB
  assert_true(memory_allocation1.success)
  
  let memory_allocation2 = ResourceLimiter::consume_quota(resource_limiter, "memory.usage", 512 * 1024 * 1024)  // 512MB
  assert_true(memory_allocation2.success)
  
  // Should exceed quota
  let memory_allocation3 = ResourceLimiter::consume_quota(resource_limiter, "memory.usage", 512 * 1024 * 1024)  // 512MB
  assert_false(memory_allocation3.success)
  
  // Test quota release
  ResourceLimiter::release_quota(resource_limiter, "memory.usage", 256 * 1024 * 1024)  // Release 256MB
  
  // Now should have enough for 256MB
  let memory_allocation4 = ResourceLimiter::consume_quota(resource_limiter, "memory.usage", 256 * 1024 * 1024)  // 256MB
  assert_true(memory_allocation4.success)
  
  // Test priority-based limiting
  let priority_limiter = ResourceLimiter::create_priority_limiter(resource_limiter, "critical.operations", 10)
  
  let critical_op1 = ResourceLimiter::acquire_with_priority(resource_limiter, priority_limiter, Priority::High)
  let critical_op2 = ResourceLimiter::acquire_with_priority(resource_limiter, priority_limiter, Priority::High)
  let critical_op3 = ResourceLimiter::acquire_with_priority(resource_limiter, priority_limiter, Priority::Low)
  
  // High priority operations should be preferred
  assert_true(critical_op1.success)
  assert_true(critical_op2.success)
  
  // Test adaptive rate limiting
  let adaptive_limiter = ResourceLimiter::create_adaptive_rate_limiter(resource_limiter, "adaptive.requests", 100, 60)
  
  // Simulate high success rate
  for i in 1..=50 {
    ResourceLimiter::record_outcome(resource_limiter, adaptive_limiter, true)
  }
  
  let adaptive_limit1 = ResourceLimiter::get_current_limit(resource_limiter, adaptive_limiter)
  assert_true(adaptive_limit1 >= 100)  // Should increase due to high success rate
  
  // Simulate high failure rate
  for i in 1..=50 {
    ResourceLimiter::record_outcome(resource_limiter, adaptive_limiter, false)
  }
  
  let adaptive_limit2 = ResourceLimiter::get_current_limit(resource_limiter, adaptive_limiter)
  assert_true(adaptive_limit2 < adaptive_limit1)  // Should decrease due to high failure rate
  
  // Test resource limiting statistics
  let stats = ResourceLimiter::get_statistics(resource_limiter)
  assert_true(stats.rate_limiters > 0)
  assert_true(stats.concurrent_limiters > 0)
  assert_true(stats.quotas > 0)
  
  // Test resource limiting configuration
  let config = ResourceLimiter::export_configuration(resource_limiter)
  assert_true(config.contains("rate_limiters"))
  assert_true(config.contains("concurrent_limiters"))
  assert_true(config.contains("quotas"))
  
  // Test resource limiting reset
  ResourceLimiter::reset_all_limits(resource_limiter)
  
  let reset_stats = ResourceLimiter::get_statistics(resource_limiter)
  assert_eq(reset_stats.rate_limiters, 0)
  assert_eq(reset_stats.concurrent_limiters, 0)
  assert_eq(reset_stats.quotas, 0)
}

// Test 10: Cross-Service Communication
test "cross-service communication patterns" {
  let communication_manager = CommunicationManager::new()
  
  // Test service discovery
  let service_registry = CommunicationManager::get_service_registry(communication_manager)
  
  ServiceRegistry::register_service(service_registry, "user-service", "http://user-service:8080", ["v1", "v2"])
  ServiceRegistry::register_service(service_registry, "order-service", "http://order-service:8080", ["v1"])
  ServiceRegistry::register_service(service_registry, "payment-service", "http://payment-service:8080", ["v1", "v2"])
  
  let user_service_instances = ServiceRegistry::get_service_instances(service_registry, "user-service")
  assert_eq(user_service_instances.length(), 1)
  assert_eq(user_service_instances[0], "http://user-service:8080")
  
  let missing_service = ServiceRegistry::get_service_instances(service_registry, "missing-service")
  assert_eq(missing_service.length(), 0)
  
  // Test load balancing
  let load_balancer = CommunicationManager::get_load_balancer(communication_manager)
  
  // Register multiple instances for load balancing
  ServiceRegistry::register_service(service_registry, "api-service", "http://api-service-1:8080", ["v1"])
  ServiceRegistry::register_service(service_registry, "api-service", "http://api-service-2:8080", ["v1"])
  ServiceRegistry::register_service(service_registry, "api-service", "http://api-service-3:8080", ["v1"])
  
  // Test round-robin load balancing
  let instance1 = LoadBalancer::select_instance(load_balancer, "api-service", LoadBalancingStrategy::RoundRobin)
  let instance2 = LoadBalancer::select_instance(load_balancer, "api-service", LoadBalancingStrategy::RoundRobin)
  let instance3 = LoadBalancer::select_instance(load_balancer, "api-service", LoadBalancingStrategy::RoundRobin)
  let instance4 = LoadBalancer::select_instance(load_balancer, "api-service", LoadBalancingStrategy::RoundRobin)
  
  assert_not_eq(instance1, instance2)
  assert_not_eq(instance2, instance3)
  assert_not_eq(instance3, instance1)
  assert_eq(instance4, instance1)  // Should cycle back to first
  
  // Test request/response communication
  let request_builder = CommunicationManager::create_request(communication_manager)
  
  let request = RequestBuilder::method(request_builder, "GET")
    .url("http://user-service:8080/users/123")
    .header("Authorization", "Bearer token123")
    .header("Content-Type", "application/json")
    .timeout(5000)  // 5 seconds
    .build()
  
  // Test request serialization
  let serialized_request = CommunicationManager::serialize_request(communication_manager, request)
  assert_true(serialized_request.contains("GET"))
  assert_true(serialized_request.contains("/users/123"))
  assert_true(serialized_request.contains("Authorization"))
  
  // Test response handling
  let mock_response = {
    status_code: 200,
    headers: [
      ("Content-Type", "application/json"),
      ("Cache-Control", "no-cache")
    ],
    body: "{\"id\":123,\"name\":\"John Doe\",\"email\":\"john@example.com\"}"
  }
  
  let response_parser = CommunicationManager::create_response_parser(communication_manager)
  let parsed_response = ResponseParser::parse(response_parser, mock_response)
  
  assert_eq(parsed_response.status_code, 200)
  assert_eq(parsed_response.headers.get("Content-Type"), Some("application/json"))
  assert_true(parsed_response.body.contains("John Doe"))
  
  // Test async communication
  let async_client = CommunicationManager::create_async_client(communication_manager)
  
  let future_request = AsyncClient::send_request(async_client, request)
  
  // Simulate async response
  let async_response = AsyncClient::wait_for_response(future_request)
  assert_eq(async_response.status_code, 200)
  
  // Test circuit breaker for service communication
  let circuit_breaker = CommunicationManager::create_circuit_breaker(communication_manager, "user-service", 5, 60000)
  
  // Test successful request
  let successful_result = CircuitBreaker::execute(circuit_breaker, fn() {
    // Simulate successful service call
    return Ok("{\"id\":123,\"name\":\"John Doe\"}")
  })
  
  match successful_result {
    Ok(response) => assert_true(response.contains("John Doe"))
    Err(_) => assert_true(false)
  }
  
  // Test failed requests to trigger circuit breaker
  for i in 1..=5 {
    CircuitBreaker::execute(circuit_breaker, fn() {
      return Err("Service unavailable")
    })
  }
  
  // Circuit should now be open
  let circuit_breaker_result = CircuitBreaker::execute(circuit_breaker, fn() {
    return Ok("This should not be called")
  })
  
  match circuit_breaker_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(error, "Circuit breaker is open")
  }
  
  // Test message queue communication
  let message_queue = CommunicationManager::create_message_queue(communication_manager, "telemetry.events")
  
  let telemetry_event = {
    event_type: "span.created",
    timestamp: 1640995200,
    data: {
      trace_id: "trace-123",
      span_id: "span-456",
      operation_name: "user.get"
    }
  }
  
  // Test message publishing
  let publish_result = MessageQueue::publish(message_queue, telemetry_event)
  assert_true(publish_result.success)
  
  // Test message subscription
  let subscription = MessageQueue::subscribe(message_queue, "telemetry.processor")
  
  let received_message = MessageQueue::receive(subscription, 1000)  // 1 second timeout
  match received_message {
    Some(message) => {
      assert_eq(message.event_type, "span.created")
      assert_eq(message.data.operation_name, "user.get")
    }
    None => assert_true(false)
  }
  
  // Test service mesh communication
  let service_mesh = CommunicationManager::create_service_mesh(communication_manager)
  
  ServiceMesh::register_service_instance(service_mesh, "user-service", "instance-1", "192.168.1.10:8080")
  ServiceMesh::register_service_instance(service_mesh, "user-service", "instance-2", "192.168.1.11:8080")
  ServiceMesh::register_service_instance(service_mesh, "order-service", "instance-1", "192.168.1.20:8080")
  
  // Test service mesh routing
  let route_result = ServiceMesh::route_request(service_mesh, "user-service", "/users/123")
  assert_true(route_result.success)
  assert_true(route_result.destination.contains("192.168.1."))
  
  // Test service mesh observability
  let mesh_metrics = ServiceMesh::get_metrics(service_mesh)
  assert_true(mesh_metrics.request_count >= 0)
  assert_true(mesh_metrics.response_time_avg >= 0)
  assert_true(mesh_metrics.error_rate >= 0.0)
  
  // Test cross-service tracing
  let trace_context = TraceContext::new("trace-789", "span-123", "sampled")
  
  let traced_request = CommunicationManager::propagate_trace_context(communication_manager, request, trace_context)
  let propagated_context = CommunicationManager::extract_trace_context(communication_manager, traced_request)
  
  assert_eq(propagated_context.trace_id, "trace-789")
  assert_eq(propagated_context.span_id, "span-123")
  assert_eq(propagated_context.flags, "sampled")
  
  // Test service communication security
  let tls_config = TLSConfig::new()
    .cert_path("/etc/ssl/certs/service.crt")
    .key_path("/etc/ssl/private/service.key")
    .ca_path("/etc/ssl/certs/ca.crt")
    .verify_peer(true)
    .build()
  
  let secure_client = CommunicationManager::create_secure_client(communication_manager, tls_config)
  
  let secure_request = CommunicationManager::create_request(communication_manager)
    .method("POST")
    .url("https://payment-service:8443/process")
    .body("{\"amount\":100.0,\"currency\":\"USD\"}")
    .build()
  
  let secure_response = SecureClient::send_request(secure_client, secure_request)
  assert_eq(secure_response.status_code, 200)
}