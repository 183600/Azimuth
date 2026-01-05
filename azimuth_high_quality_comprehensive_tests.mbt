// Azimuth Telemetry System - High Quality Comprehensive Tests
// This file contains 10 high-quality test cases covering various aspects of the telemetry system

// Test 1: Advanced Telemetry Data Processing with Aggregation
test "advanced telemetry data processing with aggregation" {
  // Create telemetry data points
  let data_points = [
    ("metric1", 1.5, 1000L),
    ("metric2", 2.3, 1001L),
    ("metric1", 1.8, 1002L),
    ("metric3", 4.2, 1003L),
    ("metric2", 2.7, 1004L)
  ]
  
  // Test aggregation by metric name
  let mut aggregated_data = Map::new()
  for (name, value, timestamp) in data_points {
    match Map::get(aggregated_data, name) {
      Some((sum, count, min_ts, max_ts)) => {
        let new_sum = sum + value
        let new_count = count + 1
        let new_min_ts = if timestamp < min_ts { timestamp } else { min_ts }
        let new_max_ts = if timestamp > max_ts { timestamp } else { max_ts }
        Map::set(aggregated_data, name, (new_sum, new_count, new_min_ts, new_max_ts))
      }
      None => {
        Map::set(aggregated_data, name, (value, 1, timestamp, timestamp))
      }
    }
  }
  
  // Verify aggregated results
  match Map::get(aggregated_data, "metric1") {
    Some((sum, count, min_ts, max_ts)) => {
      assert_eq(sum, 3.3)
      assert_eq(count, 2)
      assert_eq(min_ts, 1000L)
      assert_eq(max_ts, 1002L)
    }
    None => assert_true(false)
  }
  
  match Map::get(aggregated_data, "metric2") {
    Some((sum, count, min_ts, max_ts)) => {
      assert_eq(sum, 5.0)
      assert_eq(count, 2)
      assert_eq(min_ts, 1001L)
      assert_eq(max_ts, 1004L)
    }
    None => assert_true(false)
  }
  
  match Map::get(aggregated_data, "metric3") {
    Some((sum, count, min_ts, max_ts)) => {
      assert_eq(sum, 4.2)
      assert_eq(count, 1)
      assert_eq(min_ts, 1003L)
      assert_eq(max_ts, 1003L)
    }
    None => assert_true(false)
  }
}

// Test 2: Concurrent Telemetry Processing with Thread Safety
test "concurrent telemetry processing with thread safety" {
  // Simulate concurrent telemetry data collection
  let shared_counter = AtomicInt::new(0)
  let shared_data = AtomicRef::new(Map::new())
  
  // Simulate multiple threads collecting telemetry data
  let thread_ids = [1, 2, 3, 4, 5]
  for thread_id in thread_ids {
    // Simulate atomic counter increment
    AtomicInt::add(shared_counter, 1)
    
    // Simulate thread-safe data insertion
    let key = "thread_" + thread_id.to_string()
    let value = "data_from_thread_" + thread_id.to_string()
    let current_map = AtomicRef::load(shared_data)
    let new_map = Map::set(current_map, key, value)
    AtomicRef::store(shared_data, new_map)
  }
  
  // Verify thread safety results
  assert_eq(AtomicInt::load(shared_counter), 5)
  
  let final_map = AtomicRef::load(shared_data)
  assert_eq(Map::size(final_map), 5)
  
  // Verify each thread's data is present
  for thread_id in thread_ids {
    let key = "thread_" + thread_id.to_string()
    let expected_value = "data_from_thread_" + thread_id.to_string()
    match Map::get(final_map, key) {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false)
    }
  }
}

// Test 3: Performance Optimization with Caching Strategy
test "performance optimization with caching strategy" {
  // Create a cache with TTL (Time To Live)
  let cache = Cache::new(100) // 100 entries max
  let current_time = 1000L
  
  // Test cache insertion and retrieval
  Cache::put(cache, "key1", "value1", current_time, 100L) // TTL: 100
  Cache::put(cache, "key2", "value2", current_time, 200L) // TTL: 200
  Cache::put(cache, "key3", "value3", current_time, 50L)  // TTL: 50
  
  // Test cache hits
  match Cache::get(cache, "key1", current_time + 50L) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match Cache::get(cache, "key2", current_time + 50L) {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  // Test cache expiration
  match Cache::get(cache, "key3", current_time + 60L) { // Beyond TTL
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test cache eviction when full
  for i in 4..=105 {
    let key = "key" + i.to_string()
    let value = "value" + i.to_string()
    Cache::put(cache, key, value, current_time, 1000L)
  }
  
  // Verify oldest entries are evicted
  match Cache::get(cache, "key1", current_time + 50L) {
    Some(_) => assert_true(false) // Should be evicted
    None => assert_true(true)
  }
  
  // Verify newest entries are present
  match Cache::get(cache, "key105", current_time + 50L) {
    Some(value) => assert_eq(value, "value105")
    None => assert_true(false)
  }
}

// Test 4: Advanced Error Handling and Recovery
test "advanced error handling and recovery" {
  // Create a telemetry processor with error handling
  let processor = TelemetryProcessor::new()
  
  // Test processing valid data
  let valid_data = TelemetryData::new("valid.metric", 42.0, 1234567890L)
  match TelemetryProcessor::process(processor, valid_data) {
    Ok(result) => assert_eq(result.value, 42.0)
    Err(_) => assert_true(false)
  }
  
  // Test processing invalid data with recovery
  let invalid_data = TelemetryData::new("", -1.0, -1L) // Invalid data
  match TelemetryProcessor::process(processor, invalid_data) {
    Ok(_) => assert_true(false) // Should not succeed
    Err(error) => {
      match error {
        ValidationError(msg) => assert_true(msg.contains("Invalid"))
        _ => assert_true(false)
      }
    }
  }
  
  // Test retry mechanism
  let flaky_data = TelemetryData::new("flaky.metric", 24.0, 1234567891L)
  let mut retry_count = 0
  let mut success = false
  
  while retry_count < 3 && !success {
    match TelemetryProcessor::process(processor, flaky_data) {
      Ok(result) => {
        assert_eq(result.value, 24.0)
        success = true
      }
      Err(RetryableError(_)) => {
        retry_count = retry_count + 1
        // Simulate delay
        retry_count = retry_count
      }
      Err(_) => {
        assert_true(false) // Non-retryable error
      }
    }
  }
  
  assert_true(success || retry_count >= 3)
}

// Test 5: Boundary Conditions and Edge Cases
test "boundary conditions and edge cases" {
  // Test with empty collections
  let empty_map = Map::new()
  assert_eq(Map::size(empty_map), 0)
  match Map::get(empty_map, "any_key") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test with maximum values
  let max_int = 2147483647
  let max_float = 3.402823466e+38
  let max_long = 9223372036854775807L
  
  let max_data = TelemetryData::new("max.metric", max_float, max_long)
  assert_eq(max_data.value, max_float)
  assert_eq(max_data.timestamp, max_long)
  
  // Test with minimum values
  let min_int = -2147483648
  let min_float = -3.402823466e+38
  let min_long = -9223372036854775808L
  
  let min_data = TelemetryData::new("min.metric", min_float, min_long)
  assert_eq(min_data.value, min_float)
  assert_eq(min_data.timestamp, min_long)
  
  // Test with special float values
  let infinity_data = TelemetryData::new("infinity.metric", Float::infinity(), 123L)
  assert_true(Float::is_infinity(infinity_data.value))
  
  let neg_infinity_data = TelemetryData::new("neg.infinity.metric", Float::neg_infinity(), 124L)
  assert_true(Float::is_neg_infinity(neg_infinity_data.value))
  
  // Test with NaN (Not a Number)
  let nan_data = TelemetryData::new("nan.metric", Float::nan(), 125L)
  assert_true(Float::is_nan(nan_data.value))
  
  // Test with very long strings
  let long_string = "a".repeat(10000)
  let long_data = TelemetryData::new_with_metadata("long.metric", 1.0, 126L, long_string)
  assert_eq(long_data.metadata.length(), 10000)
}

// Test 6: Type Conversion and Validation
test "type conversion and validation" {
  // Test string to number conversion
  let valid_int_str = "42"
  match String::to_int(valid_int_str) {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  let invalid_int_str = "not_a_number"
  match String::to_int(invalid_int_str) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let valid_float_str = "3.14159"
  match String::to_float(valid_float_str) {
    Some(value) => assert_true(Float::abs(value - 3.14159) < 0.00001)
    None => assert_true(false)
  }
  
  // Test attribute value type conversion
  let string_attr = StringValue("123")
  match AttributeValue::to_int(string_attr) {
    Some(value) => assert_eq(value, 123)
    None => assert_true(false)
  }
  
  let int_attr = IntValue(456)
  match AttributeValue::to_string(int_attr) {
    Some(value) => assert_eq(value, "456")
    None => assert_true(false)
  }
  
  let float_attr = FloatValue(7.89)
  match AttributeValue::to_int(float_attr) {
    Some(value) => assert_eq(value, 7) // Truncated
    None => assert_true(false)
  }
  
  // Test validation rules
  let validator = Validator::new()
  Validator::add_rule(validator, "positive", |x| x > 0)
  Validator::add_rule(validator, "less_than_100", |x| x < 100)
  
  assert_true(Validator::validate(validator, 50))
  assert_false(Validator::validate(validator, -5))
  assert_false(Validator::validate(validator, 150))
}

// Test 7: Resource Management and Cleanup
test "resource management and cleanup" {
  // Create a resource manager
  let resource_manager = ResourceManager::new()
  
  // Allocate resources
  let resource1 = ResourceManager::allocate(resource_manager, "resource1", 1024)
  let resource2 = ResourceManager::allocate(resource_manager, "resource2", 2048)
  let resource3 = ResourceManager::allocate(resource_manager, "resource3", 512)
  
  // Verify resource allocation
  assert_eq(ResourceManager::get_allocated_size(resource_manager, resource1), 1024)
  assert_eq(ResourceManager::get_allocated_size(resource_manager, resource2), 2048)
  assert_eq(ResourceManager::get_allocated_size(resource_manager, resource3), 512)
  assert_eq(ResourceManager::get_total_allocated(resource_manager), 3584)
  
  // Test resource deallocation
  ResourceManager::deallocate(resource_manager, resource1)
  assert_eq(ResourceManager::get_total_allocated(resource_manager), 2560)
  
  // Test resource cleanup on scope exit
  {
    let scoped_resource = ResourceManager::allocate(resource_manager, "scoped", 4096)
    assert_eq(ResourceManager::get_total_allocated(resource_manager), 6656)
  } // scoped_resource should be automatically cleaned up here
  
  assert_eq(ResourceManager::get_total_allocated(resource_manager), 2560)
  
  // Test resource limit enforcement
  ResourceManager::set_limit(resource_manager, 3000)
  let oversized_resource = ResourceManager::allocate(resource_manager, "oversized", 1024)
  match oversized_resource {
    Some(_) => assert_true(false) // Should fail due to limit
    None => assert_true(true)
  }
  
  // Test resource reuse
  let reusable_resource = ResourceManager::allocate(resource_manager, "reusable", 256)
  ResourceManager::deallocate(resource_manager, reusable_resource)
  let reused_resource = ResourceManager::allocate(resource_manager, "reused", 256)
  assert_true(ResourceManager::is_reused(reused_resource))
}

// Test 8: Serialization and Deserialization
test "serialization and deserialization" {
  // Create a complex telemetry object
  let telemetry_span = TelemetrySpan::new(
    "operation_name",
    "trace_id_123",
    "span_id_456",
    "parent_span_id_789",
    1234567890L,
    1234567990L
  )
  
  // Add attributes
  TelemetrySpan::add_attribute(telemetry_span, "user.id", IntValue(42))
  TelemetrySpan::add_attribute(telemetry_span, "user.name", StringValue("test_user"))
  TelemetrySpan::add_attribute(telemetry_span, "operation.duration", FloatValue(100.5))
  
  // Add events
  TelemetrySpan::add_event(telemetry_span, "event1", 1234567900L, [
    ("event.attr1", StringValue("value1")),
    ("event.attr2", IntValue(123))
  ])
  
  TelemetrySpan::add_event(telemetry_span, "event2", 1234567950L, [
    ("event.attr3", FloatValue(456.7))
  ])
  
  // Serialize to JSON
  let json_data = TelemetrySpan::to_json(telemetry_span)
  assert_true(json_data.contains("operation_name"))
  assert_true(json_data.contains("trace_id_123"))
  assert_true(json_data.contains("user.id"))
  assert_true(json_data.contains("event1"))
  
  // Deserialize from JSON
  match TelemetrySpan::from_json(json_data) {
    Ok(deserialized_span) => {
      assert_eq(TelemetrySpan::name(deserialized_span), "operation_name")
      assert_eq(TelemetrySpan::trace_id(deserialized_span), "trace_id_123")
      assert_eq(TelemetrySpan::span_id(deserialized_span), "span_id_456")
      
      // Verify attributes
      match TelemetrySpan::get_attribute(deserialized_span, "user.id") {
        Some(IntValue(42)) => assert_true(true)
        _ => assert_true(false)
      }
      
      match TelemetrySpan::get_attribute(deserialized_span, "user.name") {
        Some(StringValue("test_user")) => assert_true(true)
        _ => assert_true(false)
      }
      
      // Verify events
      let events = TelemetrySpan::events(deserialized_span)
      assert_eq(events.length(), 2)
    }
    Err(_) => assert_true(false)
  }
  
  // Test serialization of invalid data
  let invalid_span = TelemetrySpan::new("", "", "", "", -1L, -1L)
  let invalid_json = TelemetrySpan::to_json(invalid_span)
  assert_true(invalid_json.contains("error"))
  
  // Test deserialization of invalid JSON
  match TelemetrySpan::from_json("{invalid json}") {
    Ok(_) => assert_true(false)
    Err(_) => assert_true(true)
  }
}

// Test 9: Configuration Management
test "configuration management" {
  // Create a configuration manager
  let config_manager = ConfigurationManager::new()
  
  // Set configuration values
  ConfigurationManager::set(config_manager, "telemetry.enabled", BoolValue(true))
  ConfigurationManager::set(config_manager, "telemetry.sampling_rate", FloatValue(0.5))
  ConfigurationManager::set(config_manager, "telemetry.max_batch_size", IntValue(100))
  ConfigurationManager::set(config_manager, "telemetry.export_interval", IntValue(60))
  ConfigurationManager::set(config_manager, "service.name", StringValue("test_service"))
  
  // Get configuration values
  match ConfigurationManager::get(config_manager, "telemetry.enabled") {
    Some(BoolValue(enabled)) => assert_true(enabled)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "telemetry.sampling_rate") {
    Some(FloatValue(rate)) => assert_eq(rate, 0.5)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "telemetry.max_batch_size") {
    Some(IntValue(size)) => assert_eq(size, 100)
    _ => assert_true(false)
  }
  
  match ConfigurationManager::get(config_manager, "service.name") {
    Some(StringValue(name)) => assert_eq(name, "test_service")
    _ => assert_true(false)
  }
  
  // Test configuration validation
  let validation_rules = [
    ("telemetry.sampling_rate", |value| match value {
      FloatValue(rate) => rate >= 0.0 && rate <= 1.0
      _ => false
    }),
    ("telemetry.max_batch_size", |value| match value {
      IntValue(size) => size > 0
      _ => false
    })
  ]
  
  assert_true(ConfigurationManager::validate(config_manager, validation_rules))
  
  // Test invalid configuration
  ConfigurationManager::set(config_manager, "telemetry.sampling_rate", FloatValue(1.5)) // Invalid
  assert_false(ConfigurationManager::validate(config_manager, validation_rules))
  
  // Test configuration defaults
  ConfigurationManager::set_default(config_manager, "telemetry.timeout", IntValue(30))
  match ConfigurationManager::get(config_manager, "telemetry.timeout") {
    Some(IntValue(timeout)) => assert_eq(timeout, 30)
    None => assert_true(false)
  }
  
  // Test configuration inheritance
  let child_config = ConfigurationManager::inherit(config_manager)
  ConfigurationManager::set(child_config, "service.version", StringValue("1.0.0"))
  
  // Child should have parent's values
  match ConfigurationManager::get(child_config, "telemetry.enabled") {
    Some(BoolValue(enabled)) => assert_true(enabled)
    _ => assert_true(false)
  }
  
  // Child should have its own values
  match ConfigurationManager::get(child_config, "service.version") {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  // Parent should not have child's values
  match ConfigurationManager::get(config_manager, "service.version") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 10: Internationalization Support
test "internationalization support" {
  // Create an internationalization manager
  let i18n_manager = I18nManager::new()
  
  // Add translations for different languages
  I18nManager::add_translation(i18n_manager, "en", "telemetry.error.connection_failed", "Connection to telemetry server failed")
  I18nManager::add_translation(i18n_manager, "en", "telemetry.error.invalid_data", "Invalid telemetry data received")
  I18nManager::add_translation(i18n_manager, "en", "telemetry.status.processing", "Processing telemetry data")
  
  I18nManager::add_translation(i18n_manager, "zh", "telemetry.error.connection_failed", "连接遥测服务器失败")
  I18nManager::add_translation(i18n_manager, "zh", "telemetry.error.invalid_data", "接收到无效的遥测数据")
  I18nManager::add_translation(i18n_manager, "zh", "telemetry.status.processing", "正在处理遥测数据")
  
  I18nManager::add_translation(i18n_manager, "es", "telemetry.error.connection_failed", "Error de conexión al servidor de telemetría")
  I18nManager::add_translation(i18n_manager, "es", "telemetry.error.invalid_data", "Datos de telemetría inválidos recibidos")
  I18nManager::add_translation(i18n_manager, "es", "telemetry.status.processing", "Procesando datos de telemetría")
  
  // Test English translations
  I18nManager::set_locale(i18n_manager, "en")
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.error.connection_failed"),
    "Connection to telemetry server failed"
  )
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.error.invalid_data"),
    "Invalid telemetry data received"
  )
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.status.processing"),
    "Processing telemetry data"
  )
  
  // Test Chinese translations
  I18nManager::set_locale(i18n_manager, "zh")
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.error.connection_failed"),
    "连接遥测服务器失败"
  )
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.error.invalid_data"),
    "接收到无效的遥测数据"
  )
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.status.processing"),
    "正在处理遥测数据"
  )
  
  // Test Spanish translations
  I18nManager::set_locale(i18n_manager, "es")
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.error.connection_failed"),
    "Error de conexión al servidor de telemetría"
  )
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.error.invalid_data"),
    "Datos de telemetría inválidos recibidos"
  )
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.status.processing"),
    "Procesando datos de telemetría"
  )
  
  // Test missing translation fallback
  assert_eq(
    I18nManager::translate(i18n_manager, "telemetry.error.unknown"),
    "telemetry.error.unknown" // Fallback to key
  )
  
  // Test parameterized translations
  I18nManager::add_translation(i18n_manager, "en", "telemetry.status.items_processed", "Processed {count} items in {duration}ms")
  I18nManager::add_translation(i18n_manager, "zh", "telemetry.status.items_processed", "在{duration}毫秒内处理了{count}个项目")
  
  let params = Map::new()
  Map::set(params, "count", "42")
  Map::set(params, "duration", "150")
  
  I18nManager::set_locale(i18n_manager, "en")
  assert_eq(
    I18nManager::translate_with_params(i18n_manager, "telemetry.status.items_processed", params),
    "Processed 42 items in 150ms"
  )
  
  I18nManager::set_locale(i18n_manager, "zh")
  assert_eq(
    I18nManager::translate_with_params(i18n_manager, "telemetry.status.items_processed", params),
    "在150毫秒内处理了42个项目"
  )
}