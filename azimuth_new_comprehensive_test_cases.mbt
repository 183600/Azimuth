// Azimuth Telemetry System - New Comprehensive Test Cases
// This file contains new test cases covering various aspects of the telemetry system

// Test 1: Advanced String Manipulation
test "advanced string manipulation" {
  let text = "Azimuth Telemetry System"
  
  // Test string splitting
  let words = text.split(" ")
  assert_eq(words.length(), 3)
  assert_eq(words[0], "Azimuth")
  assert_eq(words[1], "Telemetry")
  assert_eq(words[2], "System")
  
  // Test string transformation
  let upper_text = text.to_uppercase()
  assert_eq(upper_text, "AZIMUTH TELEMETRY SYSTEM")
  
  let lower_text = text.to_lowercase()
  assert_eq(lower_text, "azimuth telemetry system")
  
  // Test substring operations
  let substring = text.substring(0, 7)
  assert_eq(substring, "Azimuth")
  
  // Test string contains
  assert_true(text.contains("Telemetry"))
  assert_false(text.contains("Testing"))
}

// Test 2: Advanced Array Operations
test "advanced array operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Test array filtering
  let even_numbers = numbers.filter(fn(x) { x % 2 == 0 })
  assert_eq(even_numbers.length(), 5)
  assert_eq(even_numbers[0], 2)
  assert_eq(even_numbers[4], 10)
  
  // Test array mapping
  let doubled = numbers.map(fn(x) { x * 2 })
  assert_eq(doubled.length(), 10)
  assert_eq(doubled[0], 2)
  assert_eq(doubled[9], 20)
  
  // Test array reduction
  let sum = numbers.reduce(fn(acc, x) { acc + x }, 0)
  assert_eq(sum, 55)
  
  // Test array concatenation
  let first_half = numbers.slice(0, 5)
  let second_half = numbers.slice(5, 10)
  let concatenated = first_half.concat(second_half)
  assert_eq(concatenated.length(), 10)
  assert_eq(concatenated[0], 1)
  assert_eq(concatenated[9], 10)
}

// Test 3: Telemetry Data Validation
test "telemetry data validation" {
  // Test valid trace ID format
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  assert_true(valid_trace_id.length() == 32)
  assert_true(is_hexadecimal(valid_trace_id))
  
  // Test valid span ID format
  let valid_span_id = "b7ad6b7169203331"
  assert_true(valid_span_id.length() == 16)
  assert_true(is_hexadecimal(valid_span_id))
  
  // Test invalid formats
  let invalid_trace_id = "invalid_trace_id"
  assert_false(invalid_trace_id.length() == 32)
  assert_false(is_hexadecimal(invalid_trace_id))
  
  let invalid_span_id = "too_short"
  assert_false(invalid_span_id.length() == 16)
  assert_false(is_hexadecimal(invalid_span_id))
}

// Test 4: Time Series Data Operations
test "time series data operations" {
  // Create time series data points
  let data_points = [
    TimeSeriesPoint::new(1000L, 10.5),
    TimeSeriesPoint::new(2000L, 15.3),
    TimeSeriesPoint::new(3000L, 12.7),
    TimeSeriesPoint::new(4000L, 18.9),
    TimeSeriesPoint::new(5000L, 14.2)
  ]
  
  // Test time series aggregation
  let time_series = TimeSeries::new(data_points)
  let average = TimeSeries::average(time_series)
  assert_true(average > 14.0 && average < 15.0)
  
  // Test time series filtering by time range
  let filtered = TimeSeries::filter_by_time_range(time_series, 2000L, 4000L)
  assert_eq(filtered.length(), 3)
  
  // Test time series min/max
  let min_value = TimeSeries::min(time_series)
  let max_value = TimeSeries::max(time_series)
  assert_eq(min_value, 10.5)
  assert_eq(max_value, 18.9)
}

// Test 5: Concurrent Operations Safety
test "concurrent operations safety" {
  // Test thread-safe counter operations
  let counter = AtomicCounter::new(0)
  
  // Simulate concurrent increments
  AtomicCounter::increment(counter)
  AtomicCounter::increment(counter)
  AtomicCounter::increment(counter)
  
  let value = AtomicCounter::get(counter)
  assert_eq(value, 3)
  
  // Test thread-safe flag operations
  let flag = AtomicFlag::new(false)
  assert_false(AtomicFlag::get(flag))
  
  AtomicFlag::set(flag, true)
  assert_true(AtomicFlag::get(flag))
  
  // Test compare-and-swap
  let success = AtomicFlag::compare_and_swap(flag, true, false)
  assert_true(success)
  assert_false(AtomicFlag::get(flag))
}

// Test 6: Error Handling and Recovery
test "error handling and recovery" {
  // Test Result type operations
  let success_result = Ok(42)
  let error_result = Error("Something went wrong")
  
  match success_result {
    Ok(value) => assert_eq(value, 42)
    Error(_) => assert_true(false)
  }
  
  match error_result {
    Ok(_) => assert_true(false)
    Error(message) => assert_eq(message, "Something went wrong")
  }
  
  // Test error recovery mechanisms
  let operation_result = try_recover_operation(fn() { Error("Initial failure") }, fn() { Ok(100) })
  match operation_result {
    Ok(value) => assert_eq(value, 100)
    Error(_) => assert_true(false)
  }
  
  // Test error chaining
  let chained_error = chain_errors(Error("First error"), Error("Second error"))
  match chained_error {
    Error(message) => assert_true(message.contains("First error") && message.contains("Second error"))
    Ok(_) => assert_true(false)
  }
}

// Test 7: Configuration Management
test "configuration management" {
  // Test configuration loading and validation
  let config = Configuration::new()
  
  // Test setting and getting configuration values
  Configuration::set(config, "telemetry.enabled", BoolValue(true))
  Configuration::set(config, "telemetry.sampling_rate", FloatValue(0.1))
  Configuration::set(config, "telemetry.max_batch_size", IntValue(100))
  
  let enabled = Configuration::get(config, "telemetry.enabled")
  match enabled {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  let sampling_rate = Configuration::get(config, "telemetry.sampling_rate")
  match sampling_rate {
    Some(FloatValue(value)) => assert_eq(value, 0.1)
    _ => assert_true(false)
  }
  
  // Test configuration validation
  let validation_result = Configuration::validate(config)
  assert_true(validation_result.is_valid)
  
  // Test default values
  let default_value = Configuration::get_with_default(config, "nonexistent.key", StringValue("default"))
  match default_value {
    StringValue(value) => assert_eq(value, "default")
    _ => assert_true(false)
  }
}

// Test 8: Metric Aggregation and Analysis
test "metric aggregation and analysis" {
  // Create metric data
  let metric_data = [
    MetricValue::new(10.5, "ms"),
    MetricValue::new(15.3, "ms"),
    MetricValue::new(12.7, "ms"),
    MetricValue::new(18.9, "ms"),
    MetricValue::new(14.2, "ms")
  ]
  
  // Test percentile calculations
  let aggregator = MetricAggregator::new(metric_data)
  let p50 = MetricAggregator::percentile(aggregator, 50.0)
  let p95 = MetricAggregator::percentile(aggregator, 95.0)
  let p99 = MetricAggregator::percentile(aggregator, 99.0)
  
  assert_true(p50 > 12.0 && p50 < 16.0)
  assert_true(p95 > 15.0 && p95 < 19.0)
  assert_true(p99 > 15.0 && p99 < 19.0)
  
  // Test rate calculations
  let rate_calculator = RateCalculator::new()
  RateCalculator::add_sample(rate_calculator, 100, 1000L)
  RateCalculator::add_sample(rate_calculator, 200, 2000L)
  RateCalculator::add_sample(rate_calculator, 150, 3000L)
  
  let rate = RateCalculator::calculate_rate(rate_calculator)
  assert_true(rate > 0.0)
}

// Test 9: Data Serialization and Deserialization
test "data serialization and deserialization" {
  // Test JSON serialization
  let telemetry_data = TelemetryData::new(
    "trace_id_123",
    "span_id_456",
    "operation_name",
    1234567890L,
    1234567895L
  )
  
  let json_string = TelemetryData::to_json(telemetry_data)
  assert_true(json_string.contains("trace_id_123"))
  assert_true(json_string.contains("span_id_456"))
  
  // Test JSON deserialization
  let deserialized_data = TelemetryData::from_json(json_string)
  assert_eq(TelemetryData::trace_id(deserialized_data), "trace_id_123")
  assert_eq(TelemetryData::span_id(deserialized_data), "span_id_456")
  assert_eq(TelemetryData::operation_name(deserialized_data), "operation_name")
  
  // Test binary serialization
  let binary_data = TelemetryData::to_binary(telemetry_data)
  let deserialized_binary = TelemetryData::from_binary(binary_data)
  assert_eq(TelemetryData::trace_id(deserialized_binary), "trace_id_123")
  assert_eq(TelemetryData::span_id(deserialized_binary), "span_id_456")
}

// Test 10: Resource Management and Cleanup
test "resource management and cleanup" {
  // Test resource pool management
  let resource_pool = ResourcePool::new(5)
  
  // Test resource acquisition
  let resource1 = ResourcePool::acquire(resource_pool)
  let resource2 = ResourcePool::acquire(resource_pool)
  let resource3 = ResourcePool::acquire(resource_pool)
  
  assert_true(ResourcePool::available_count(resource_pool) == 2)
  
  // Test resource release
  ResourcePool::release(resource_pool, resource1)
  assert_true(ResourcePool::available_count(resource_pool) == 3)
  
  // Test resource exhaustion
  let resources = []
  for i in 0..=5 {
    let resource = ResourcePool::acquire(resource_pool)
    resources.push(resource)
  }
  
  let exhausted_resource = ResourcePool::acquire(resource_pool)
  match exhausted_resource {
    Some(_) => assert_true(false) // Should not succeed
    None => assert_true(true) // Expected to fail
  }
  
  // Cleanup resources
  for resource in resources {
    ResourcePool::release(resource_pool, resource)
  }
  
  assert_true(ResourcePool::available_count(resource_pool) == 5)
}

// Helper functions
fn is_hexadecimal(str : String) -> Bool {
  let hex_chars = "0123456789abcdefABCDEF"
  for char in str.to_char_array() {
    if !hex_chars.contains(char.to_string()) {
      return false
    }
  }
  return true
}

fn try_recover_operation(operation : () -> Result<Int, String>, recovery : () -> Result<Int, String>) -> Result<Int, String] {
  match operation() {
    Ok(value) => Ok(value)
    Error(_) => recovery()
  }
}

fn chain_errors(first_error : Result<Unit, String], second_error : Result<Unit, String>) -> Result<Unit, String> {
  match first_error {
    Error(first_msg) => {
      match second_error {
        Error(second_msg) => Error(first_msg + " -> " + second_msg)
        Ok(_) => Error(first_msg)
      }
    }
    Ok(_) => first_error
  }
}