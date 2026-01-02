// Azimuth Boundary Conditions and Error Handling Tests
// This file contains comprehensive test cases for boundary conditions and error handling

// Test 1: Extreme Value Boundary Testing
test "extreme value boundary conditions in telemetry data" {
  // Test maximum and minimum numeric values
  let max_float = 3.402823466e+38 // Maximum 32-bit float
  let min_float = -3.402823466e+38 // Minimum 32-bit float
  let max_int = 2147483647 // Maximum 32-bit integer
  let min_int = -2147483648 // Minimum 32-bit integer
  
  // Test with extreme metric values
  let extreme_data_points = [
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "extreme-test",
      metric_name = "max.float",
      metric_value = max_float,
      attributes = []
    ),
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "extreme-test",
      metric_name = "min.float",
      metric_value = min_float,
      attributes = []
    ),
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "extreme-test",
      metric_name = "max.int",
      metric_value = max_int.to_double(),
      attributes = []
    ),
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "extreme-test",
      metric_name = "min.int",
      metric_value = min_int.to_double(),
      attributes = []
    ),
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "extreme-test",
      metric_name = "zero",
      metric_value = 0.0,
      attributes = []
    ),
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "extreme-test",
      metric_name = "tiny.positive",
      metric_value = 1.0e-10,
      attributes = []
    ),
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "extreme-test",
      metric_name = "tiny.negative",
      metric_value = -1.0e-10,
      attributes = []
    )
  ]
  
  // Process extreme values
  let processor = azimuth::TelemetryProcessor::new()
  let processed_results = []
  
  for data_point in extreme_data_points {
    let result = azimuth::TelemetryProcessor::process(processor, data_point)
    processed_results.push(result)
  }
  
  // Verify all extreme values are handled correctly
  assert_eq(processed_results.length(), 7)
  
  for result in processed_results {
    assert_true(result.success)
    assert_false(result.has_overflow)
    assert_false(result.has_underflow)
  }
  
  // Test overflow detection
  let overflow_test = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "overflow-test",
    metric_name = "overflow.test",
    metric_value = max_float * 2.0, // Should cause overflow
    attributes = []
  )
  
  let overflow_result = azimuth::TelemetryProcessor::process(processor, overflow_test)
  assert_true(overflow_result.has_overflow)
  assert_true(overflow_result.success) // Should still succeed but with overflow flag
  
  // Test underflow detection
  let underflow_test = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "underflow-test",
    metric_name = "underflow.test",
    metric_value = min_float * 2.0, // Should cause underflow
    attributes = []
  )
  
  let underflow_result = azimuth::TelemetryProcessor::process(processor, underflow_test)
  assert_true(underflow_result.has_underflow)
  assert_true(underflow_result.success)
  
  // Test infinity and NaN handling
  let infinity_test = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "infinity-test",
    metric_name = "infinity.test",
    metric_value = 1.0 / 0.0, // Infinity
    attributes = []
  )
  
  let infinity_result = azimuth::TelemetryProcessor::process(processor, infinity_test)
  assert_true(infinity_result.has_infinity)
  assert_true(infinity_result.success)
  
  let nan_test = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "nan-test",
    metric_name = "nan.test",
    metric_value = 0.0 / 0.0, // NaN
    attributes = []
  )
  
  let nan_result = azimuth::TelemetryProcessor::process(processor, nan_test)
  assert_true(nan_result.has_nan)
  assert_true(nan_result.success)
}

// Test 2: Memory and Resource Boundary Testing
test "memory and resource boundary conditions" {
  // Test with very large datasets
  let large_dataset_size = 1000000 // 1 million data points
  let large_dataset = []
  
  // Generate large dataset (in practice, this would be done more efficiently)
  for i = 0; i < 1000; i = i + 1 { // Using smaller size for test feasibility
    let data_point = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i.to_long() * 1000,
      service_name = "large-dataset-test",
      metric_name = "large.metric",
      metric_value = i.to_double(),
      attributes = [
        ("index", i.to_string()),
        ("batch", "large"),
        ("category", "memory-test")
      ]
    )
    large_dataset.push(data_point)
  }
  
  // Test memory-efficient processing
  let memory_efficient_processor = azimuth::MemoryEfficientProcessor::new(
    batch_size = 100,
    max_memory_mb = 100
  )
  
  let memory_results = azimuth::MemoryEfficientProcessor::process_batch(
    memory_efficient_processor, 
    large_dataset
  )
  
  assert_true(memory_results.success)
  assert_eq(memory_results.processed_count, 1000)
  assert_true(memory_results.memory_usage_mb <= 100)
  
  // Test memory pressure handling
  let memory_pressure_processor = azimuth::MemoryEfficientProcessor::new(
    batch_size = 1000,
    max_memory_mb = 1 // Very low limit
  )
  
  let pressure_results = azimuth::MemoryEfficientProcessor::process_batch(
    memory_pressure_processor, 
    large_dataset
  )
  
  // Should still succeed but with memory management strategies
  assert_true(pressure_results.success)
  assert_true(pressure_results.used_disk_storage || pressure_results.used_compression)
  
  // Test with very deep attribute nesting
  let deeply_nested_attributes = []
  for i = 0; i < 100; i = i + 1 {
    deeply_nested_attributes.push(("level." + i.to_string(), "value-" + i.to_string()))
  }
  
  let nested_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "nested-test",
    metric_name = "nested.metric",
    metric_value = 100.0,
    attributes = deeply_nested_attributes
  )
  
  let nested_result = azimuth::TelemetryProcessor::process(processor, nested_data)
  assert_true(nested_result.success)
  assert_true(nested_result.attribute_count > 100)
  
  // Test attribute count limits
  let too_many_attributes = []
  for i = 0; i < 10000; i = i + 1 { // Exceed reasonable limits
    too_many_attributes.push(("attr." + i.to_string(), "value-" + i.to_string()))
  }
  
  let excessive_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "excessive-test",
    metric_name = "excessive.metric",
    metric_value = 100.0,
    attributes = too_many_attributes
  )
  
  let excessive_result = azimuth::TelemetryProcessor::process(processor, excessive_data)
  assert_true(excessive_result.success)
  assert_true(excessive_result.attributes_truncated)
  assert_true(excessive_result.final_attribute_count < 10000)
}

// Test 3: Network and I/O Error Handling
test "network and I/O error handling and recovery" {
  // Create network-aware processor
  let network_processor = azimuth::NetworkTelemetryProcessor::new(
    retry_count = 3,
    timeout_ms = 5000,
    circuit_breaker_threshold = 5
  )
  
  // Simulate network failure scenarios
  let failure_scenarios = [
    azimuth::NetworkFailure::ConnectionTimeout,
    azimuth::NetworkFailure::ConnectionRefused,
    azimuth::NetworkFailure::DnsResolutionFailure,
    azimuth::NetworkFailure::SocketError,
    azimuth::NetworkFailure::SslHandshakeFailure,
    azimuth::NetworkFailure::PartialDataTransfer,
    azimuth::NetworkFailure::ConnectionReset
  ]
  
  for failure in failure_scenarios {
    let test_data = azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "network-test",
      metric_name = "network.metric",
      metric_value = 100.0,
      attributes = [("test.scenario", failure.to_string())]
    )
    
    // Simulate network failure
    azimuth::NetworkTelemetryProcessor::simulate_failure(network_processor, failure)
    
    let result = azimuth::NetworkTelemetryProcessor::send_to_remote(network_processor, test_data)
    
    // Should handle failure gracefully
    assert_true(result.attempted)
    assert_true(result.retry_count > 0)
    
    if failure == azimuth::NetworkFailure::PartialDataTransfer {
      // Partial transfer might succeed
      assert_true(result.success || result.retried)
    } else {
      // Other failures should be retried
      assert_true(result.retried || result.failed)
    }
  }
  
  // Test circuit breaker behavior
  let circuit_breaker_processor = azimuth::NetworkTelemetryProcessor::new(
    retry_count = 1,
    timeout_ms = 1000,
    circuit_breaker_threshold = 3
  )
  
  // Trigger multiple failures to activate circuit breaker
  for i = 0; i < 5; i = i + 1 {
    let test_data = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i * 1000,
      service_name = "circuit-test",
      metric_name = "circuit.metric",
      metric_value = i.to_double(),
      attributes = []
    )
    
    azimuth::NetworkTelemetryProcessor::simulate_failure(
      circuit_breaker_processor, 
      azimuth::NetworkFailure::ConnectionTimeout
    )
    
    let result = azimuth::NetworkTelemetryProcessor::send_to_remote(
      circuit_breaker_processor, 
      test_data
    )
    
    // First few attempts should be tried, later ones should be blocked by circuit breaker
    if i < 3 {
      assert_true(result.attempted)
    } else {
      assert_false(result.attempted) // Blocked by circuit breaker
      assert_true(result.circuit_breaker_open)
    }
  }
  
  // Test circuit breaker recovery
  azimuth::NetworkTelemetryProcessor::reset_circuit_breaker(circuit_breaker_processor)
  
  let recovery_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "recovery-test",
    metric_name = "recovery.metric",
    metric_value = 100.0,
    attributes = []
  )
  
  let recovery_result = azimuth::NetworkTelemetryProcessor::send_to_remote(
    circuit_breaker_processor, 
    recovery_data
  )
  
  assert_true(recovery_result.attempted)
  assert_false(recovery_result.circuit_breaker_open)
  
  // Test I/O error handling
  let io_processor = azimuth::IOTelemetryProcessor::new(
    max_file_size_mb = 100,
    backup_count = 5,
    compression_enabled = true
  )
  
  // Simulate disk full scenario
  azimuth::IOTelemetryProcessor::simulate_disk_full(io_processor)
  
  let io_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "io-test",
    metric_name = "io.metric",
    metric_value = 100.0,
    attributes = []
  )
  
  let io_result = azimuth::IOTelemetryProcessor::write_to_disk(io_processor, io_data)
  assert_false(io_result.success)
  assert_true(io_result.error.contains("disk_full"))
  
  // Test fallback to alternative storage
  let fallback_result = azimuth::IOTelemetryProcessor::write_with_fallback(io_processor, io_data)
  assert_true(fallback_result.success)
  assert_true(fallback_result.used_fallback_storage)
}

// Test 4: Data Corruption and Validation Error Handling
test "data corruption detection and validation error handling" {
  // Create data validator with strict rules
  let validator = azimuth::StrictDataValidator::new()
  
  // Configure validation rules
  azimuth::StrictDataValidator::add_rule(validator, azimuth::ValidationRule::required_fields([
    "timestamp", "service_name", "metric_name", "metric_value"
  ]))
  
  azimuth::StrictDataValidator::add_rule(validator, azimuth::ValidationRule::timestamp_format())
  azimuth::StrictDataValidator::add_rule(validator, azimuth::ValidationRule::metric_value_type())
  azimuth::StrictDataValidator::add_rule(validator, azimuth::ValidationRule::attribute_format())
  
  // Test corrupted data scenarios
  let corrupted_data_cases = [
    // Missing timestamp
    azimuth::TelemetryData::new(
      timestamp = 0, // Invalid timestamp
      service_name = "corruption-test",
      metric_name = "missing.timestamp",
      metric_value = 100.0,
      attributes = []
    ),
    // Negative timestamp
    azimuth::TelemetryData::new(
      timestamp = -1000,
      service_name = "corruption-test",
      metric_name = "negative.timestamp",
      metric_value = 100.0,
      attributes = []
    ),
    // Future timestamp (beyond reasonable bounds)
    azimuth::TelemetryData::new(
      timestamp = 4102444800000, // Year 2100
      service_name = "corruption-test",
      metric_name = "future.timestamp",
      metric_value = 100.0,
      attributes = []
    ),
    // Empty service name
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "",
      metric_name = "empty.service",
      metric_value = 100.0,
      attributes = []
    ),
    // Invalid characters in service name
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "invalid/service/name",
      metric_name = "invalid.chars",
      metric_value = 100.0,
      attributes = []
    ),
    // NaN metric value
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "corruption-test",
      metric_name = "nan.value",
      metric_value = 0.0 / 0.0,
      attributes = []
    ),
    // Infinity metric value
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "corruption-test",
      metric_name = "infinity.value",
      metric_value = 1.0 / 0.0,
      attributes = []
    ),
    // Extremely large metric value
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "corruption-test",
      metric_name = "huge.value",
      metric_value = 1.0e100,
      attributes = []
    )
  ]
  
  let validation_results = []
  
  for corrupted_data in corrupted_data_cases {
    let result = azimuth::StrictDataValidator::validate(validator, corrupted_data)
    validation_results.push(result)
  }
  
  // All corrupted data cases should fail validation
  assert_eq(validation_results.length(), 8)
  
  for result in validation_results {
    assert_false(result.is_valid)
    assert_true(result.errors.length() > 0)
  }
  
  // Test data corruption detection in serialized data
  let valid_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "valid-data",
    metric_name = "valid.metric",
    metric_value = 100.0,
    attributes = [("test", "value")]
  )
  
  let serialized_data = azimuth::TelemetrySerializer::serialize(valid_data)
  
  // Corrupt serialized data
  let corrupted_serialized = serialized_data.slice(0, serialized_data.length() / 2)
  
  let deserialization_result = azimuth::TelemetrySerializer::deserialize(corrupted_serialized)
  assert_false(deserialization_result.success)
  assert_true(deserialization_result.error.contains("truncated") || 
              deserialization_result.error.contains("corrupted"))
  
  // Test checksum validation
  let checksum_validator = azimuth::ChecksumValidator::new()
  
  // Add checksum to valid data
  let data_with_checksum = azimuth::ChecksumValidator::add_checksum(checksum_validator, valid_data)
  
  // Verify checksum
  let checksum_result = azimuth::ChecksumValidator::verify_checksum(checksum_validator, data_with_checksum)
  assert_true(checksum_result.valid)
  
  // Corrupt data and verify checksum failure
  let corrupted_with_checksum = azimuth::ChecksumValidator::corrupt_data(checksum_validator, data_with_checksum)
  let corrupted_checksum_result = azimuth::ChecksumValidator::verify_checksum(checksum_validator, corrupted_with_checksum)
  assert_false(corrupted_checksum_result.valid)
  
  // Test data repair functionality
  let repairable_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "repairable-data",
    metric_name = "repairable.metric",
    metric_value = 100.0,
    attributes = [
      ("valid.attr", "value"),
      ("invalid.timestamp", "not-a-number"),
      ("empty.attr", "")
    ]
  )
  
  let repair_result = azimuth::StrictDataValidator::validate_and_repair(validator, repairable_data)
  assert_true(repair_result.repaired)
  assert_true(repair_result.final_data.is_some)
  
  let repaired_data = repair_result.final_data.unwrap()
  assert_eq(repaired_data.service_name, "repairable-data")
  assert_eq(repaired_data.metric_name, "repairable.metric")
  assert_eq(repaired_data.metric_value, 100.0)
  
  // Invalid attributes should be removed or fixed
  assert_false(repaired_data.attributes.contains(("invalid.timestamp", "not-a-number")))
}

// Test 5: Concurrency and Race Condition Error Handling
test "concurrency and race condition error handling" {
  // Create concurrent processor
  let concurrent_processor = azimuth::ConcurrentTelemetryProcessor::new(
    max_workers = 4,
    queue_size = 1000
  )
  
  // Test concurrent data processing
  let concurrent_data = []
  for i = 0; i < 1000; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i * 1000,
      service_name = "concurrent-test",
      metric_name = "concurrent.metric",
      metric_value = i.to_double(),
      attributes = [("worker.id", (i % 4).to_string())]
    )
    concurrent_data.push(data_point)
  }
  
  // Process data concurrently
  let concurrent_results = azimuth::ConcurrentTelemetryProcessor::process_concurrent(
    concurrent_processor, 
    concurrent_data
  )
  
  assert_true(concurrent_results.success)
  assert_eq(concurrent_results.processed_count, 1000)
  assert_true(concurrent_results.worker_count <= 4)
  
  // Test race condition detection
  let race_detector = azimuth::RaceConditionDetector::new()
  
  // Simulate potential race condition
  let shared_resource = azimuth::SharedCounter::new()
  
  // Start multiple threads incrementing the same counter
  let thread_count = 10
  let increments_per_thread = 100
  
  azimuth::RaceConditionDetector::start_monitoring(race_detector, shared_resource)
  
  for i = 0; i < thread_count; i = i + 1 {
    azimuth::ConcurrentTelemetryProcessor::spawn_worker(concurrent_processor, || {
      for j = 0; j < increments_per_thread; j = j + 1 {
        azimuth::SharedCounter::increment(shared_resource)
      }
    })
  }
  
  azimuth::ConcurrentTelemetryProcessor::wait_for_all_workers(concurrent_processor)
  
  let race_results = azimuth::RaceConditionDetector::stop_monitoring(race_detector)
  
  // Verify final count
  let final_count = azimuth::SharedCounter::get_value(shared_resource)
  let expected_count = thread_count * increments_per_thread
  
  // If properly synchronized, should match expected count
  if race_results.has_race_conditions {
    assert_true(final_count != expected_count)
  } else {
    assert_eq(final_count, expected_count)
  }
  
  // Test deadlock detection
  let deadlock_detector = azimuth::DeadlockDetector::new()
  
  let resource_a = azimuth::LockableResource::new("resource-a")
  let resource_b = azimuth::LockableResource::new("resource-b")
  
  azimuth::DeadlockDetector::start_monitoring(deadlock_detector)
  
  // Simulate potential deadlock scenario
  azimuth::ConcurrentTelemetryProcessor::spawn_worker(concurrent_processor, || {
    azimuth::LockableResource::lock(resource_a)
    // Simulate some work
    azimuth::LockableResource::lock(resource_b) // Potential deadlock
    azimuth::LockableResource::unlock(resource_b)
    azimuth::LockableResource::unlock(resource_a)
  })
  
  azimuth::ConcurrentTelemetryProcessor::spawn_worker(concurrent_processor, || {
    azimuth::LockableResource::lock(resource_b)
    // Simulate some work
    azimuth::LockableResource::lock(resource_a) // Potential deadlock
    azimuth::LockableResource::unlock(resource_a)
    azimuth::LockableResource::unlock(resource_b)
  })
  
  // Set timeout to prevent actual deadlock in test
  let deadlock_results = azimuth::DeadlockDetector::monitor_with_timeout(deadlock_detector, 5000)
  
  if deadlock_results.deadlock_detected {
    assert_true(deadlock_results.involved_resources.contains("resource-a"))
    assert_true(deadlock_results.involved_resources.contains("resource-b"))
  }
  
  // Test thread safety violations
  let thread_safety_checker = azimuth::ThreadSafetyChecker::new()
  
  let unsafe_collection = azimuth::UnsafeCollection::new()
  
  azimuth::ThreadSafetyChecker::start_monitoring(thread_safety_checker, unsafe_collection)
  
  // Multiple threads modifying collection without proper synchronization
  for i = 0; i < 5; i = i + 1 {
    azimuth::ConcurrentTelemetryProcessor::spawn_worker(concurrent_processor, || {
      for j = 0; j < 100; j = j + 1 {
        azimuth::UnsafeCollection::add(unsafe_collection, j.to_string())
        azimuth::UnsafeCollection::remove(unsafe_collection, 0) // Potential race condition
      }
    })
  }
  
  azimuth::ConcurrentTelemetryProcessor::wait_for_all_workers(concurrent_processor)
  
  let safety_results = azimuth::ThreadSafetyChecker::stop_monitoring(thread_safety_checker)
  
  if safety_results.violations_detected {
    assert_true(safety_results.violation_types.contains("concurrent_modification"))
    assert_true(safety_results.violation_types.contains("unsafe_access"))
  }
}

// Test 6: Configuration Error Handling
test "configuration error handling and validation" {
  // Test invalid configuration scenarios
  let invalid_configs = [
    // Invalid buffer size (negative)
    azimuth::TelemetryConfig::new().with_buffer_size(-100),
    // Invalid timeout (zero)
    azimuth::TelemetryConfig::new().with_timeout(0),
    // Invalid retry count (negative)
    azimuth::TelemetryConfig::new().with_retry_count(-5),
    // Invalid worker count (zero)
    azimuth::TelemetryConfig::new().with_worker_count(0),
    // Invalid memory limit (zero)
    azimuth::TelemetryConfig::new().with_memory_limit_mb(0),
    // Invalid sampling rate (out of range)
    azimuth::TelemetryConfig::new().with_sampling_rate(1.5),
    // Invalid sampling rate (negative)
    azimuth::TelemetryConfig::new().with_sampling_rate(-0.1),
    // Invalid port number (out of range)
    azimuth::TelemetryConfig::new().with_port(70000),
    // Invalid port number (negative)
    azimuth::TelemetryConfig::new().with_port(-1),
    // Invalid URL format
    azimuth::TelemetryConfig::new().with_endpoint_url("not-a-url"),
    // Empty configuration
    azimuth::TelemetryConfig::new()
  ]
  
  let config_validator = azimuth::ConfigValidator::new()
  let validation_results = []
  
  for config in invalid_configs {
    let result = azimuth::ConfigValidator::validate(config_validator, config)
    validation_results.push(result)
  }
  
  // All invalid configs should fail validation
  assert_eq(validation_results.length(), 11)
  
  for result in validation_results {
    assert_false(result.is_valid)
    assert_true(result.errors.length() > 0)
  }
  
  // Test configuration error recovery
  let recovery_configs = [
    // Auto-correct negative buffer size
    azimuth::TelemetryConfig::new().with_buffer_size(-100),
    // Auto-correct zero timeout
    azimuth::TelemetryConfig::new().with_timeout(0),
    // Auto-correct negative retry count
    azimuth::TelemetryConfig::new().with_retry_count(-5),
    // Auto-correct out-of-range sampling rate
    azimuth::TelemetryConfig::new().with_sampling_rate(1.5)
  ]
  
  let recovery_results = []
  
  for config in recovery_configs {
    let result = azimuth::ConfigValidator::validate_and_recover(config_validator, config)
    recovery_results.push(result)
  }
  
  // All configs should be recoverable
  assert_eq(recovery_results.length(), 4)
  
  for result in recovery_results {
    assert_true(result.recovered)
    assert_true(result.final_config.is_some)
    
    let final_config = result.final_config.unwrap()
    assert_true(final_config.buffer_size > 0)
    assert_true(final_config.timeout > 0)
    assert_true(final_config.retry_count >= 0)
    assert_true(final_config.sampling_rate >= 0.0 && final_config.sampling_rate <= 1.0)
  }
  
  // Test configuration conflict detection
  let conflicting_config = azimuth::TelemetryConfig::new()
    .with_buffer_size(1000)
    .with_memory_limit_mb(1) // Small memory with large buffer - conflict
    .with_worker_count(10)
    .with_max_concurrent_requests(5) // More workers than max requests - conflict
  
  let conflict_result = azimuth::ConfigValidator::validate(config_validator, conflicting_config)
  assert_false(conflict_result.is_valid)
  assert_true(conflict_result.conflicts.length() > 0)
  
  // Verify specific conflicts
  assert_true(conflict_result.conflicts.contains("buffer_size_vs_memory_limit"))
  assert_true(conflict_result.conflicts.contains("worker_count_vs_max_requests"))
  
  // Test configuration priority and inheritance
  let base_config = azimuth::TelemetryConfig::new()
    .with_buffer_size(500)
    .with_timeout(5000)
    .with_retry_count(3)
  
  let override_config = azimuth::TelemetryConfig::new()
    .with_buffer_size(1000) // Override buffer size
    .with_sampling_rate(0.5) // New setting
  
  let merged_config = azimuth::ConfigMerger::merge(base_config, override_config)
  
  // Verify merged configuration
  assert_eq(merged_config.buffer_size, 1000) // From override
  assert_eq(merged_config.timeout, 5000)     // From base
  assert_eq(merged_config.retry_count, 3)    // From base
  assert_eq(merged_config.sampling_rate, 0.5) // From override
  
  // Test configuration hot-reload error handling
  let hot_reload_manager = azimuth::HotReloadConfigManager::new()
  
  // Load initial valid configuration
  let initial_config = azimuth::TelemetryConfig::new()
    .with_buffer_size(1000)
    .with_timeout(5000)
  
  let load_result = azimuth::HotReloadConfigManager::load_config(hot_reload_manager, initial_config)
  assert_true(load_result.success)
  
  // Try to reload with invalid configuration
  let invalid_reload_config = azimuth::TelemetryConfig::new()
    .with_buffer_size(-100) // Invalid
  
  let reload_result = azimuth::HotReloadConfigManager::reload_config(hot_reload_manager, invalid_reload_config)
  assert_false(reload_result.success)
  assert_true(reload_result.error.contains("invalid_configuration"))
  
  // Verify original configuration is still active
  let current_config = azimuth::HotReloadConfigManager::get_current_config(hot_reload_manager)
  assert_eq(current_config.buffer_size, 1000) // Should still be original valid config
}

// Test 7: Serialization and Deserialization Error Handling
test "serialization and deserialization error handling" {
  // Test various serialization error scenarios
  let test_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "serialization-test",
    metric_name = "test.metric",
    metric_value = 100.0,
    attributes = [
      ("string.attr", "test-value"),
      ("number.attr", "42"),
      ("boolean.attr", "true"),
      ("array.attr", "[1,2,3]"),
      ("object.attr", "{\"key\":\"value\"}")
    ]
  )
  
  // Test JSON serialization errors
  let json_serializer = azimuth::JSONTelemetrySerializer::new()
  
  // Valid serialization
  let json_result = azimuth::JSONTelemetrySerializer::serialize(json_serializer, test_data)
  assert_true(json_result.success)
  assert_true(json_result.data.length() > 0)
  
  // Test deserialization with corrupted JSON
  let corrupted_json = json_result.data.slice(0, json_result.data.length() / 2) + "corrupted"
  
  let json_deserialize_result = azimuth::JSONTelemetrySerializer::deserialize(
    json_serializer, 
    corrupted_json
  )
  assert_false(json_deserialize_result.success)
  assert_true(json_deserialize_result.error.contains("parse_error"))
  
  // Test deserialization with wrong data type
  let wrong_type_json = "{\"timestamp\":\"not-a-number\",\"service_name\":\"test\"}"
  
  let wrong_type_result = azimuth::JSONTelemetrySerializer::deserialize(
    json_serializer, 
    wrong_type_json
  )
  assert_false(wrong_type_result.success)
  assert_true(wrong_type_result.error.contains("type_error"))
  
  // Test binary serialization errors
  let binary_serializer = azimuth::BinaryTelemetrySerializer::new()
  
  // Valid binary serialization
  let binary_result = azimuth::BinaryTelemetrySerializer::serialize(binary_serializer, test_data)
  assert_true(binary_result.success)
  assert_true(binary_result.data.length() > 0)
  
  // Test deserialization with truncated binary data
  let truncated_binary = binary_result.data.slice(0, binary_result.data.length() / 2)
  
  let binary_deserialize_result = azimuth::BinaryTelemetrySerializer::deserialize(
    binary_serializer, 
    truncated_binary
  )
  assert_false(binary_deserialize_result.success)
  assert_true(binary_deserialize_result.error.contains("truncated"))
  
  // Test deserialization with corrupted binary data
  let mut corrupted_binary = binary_result.data.to_array()
  // Flip some bits to corrupt the data
  if corrupted_binary.length() > 10 {
    corrupted_binary[5] = corrupted_binary[5] ^ 0xFF // Flip all bits
    corrupted_binary[10] = corrupted_binary[10] ^ 0xFF // Flip all bits
  }
  
  let corrupted_binary_result = azimuth::BinaryTelemetrySerializer::deserialize(
    binary_serializer, 
    corrupted_binary
  )
  assert_false(corrupted_binary_result.success)
  assert_true(corrupted_binary_result.error.contains("checksum") || 
              corrupted_binary_result.error.contains("corrupted"))
  
  // Test version compatibility errors
  let versioned_serializer = azimuth::VersionedTelemetrySerializer::new()
  
  // Serialize with current version
  let versioned_result = azimuth::VersionedTelemetrySerializer::serialize(
    versioned_serializer, 
    test_data
  )
  assert_true(versioned_result.success)
  
  // Try to deserialize with incompatible version
  azimuth::VersionedTelemetrySerializer::set_target_version(
    versioned_serializer, 
    "99.99.99" // Non-existent future version
  )
  
  let incompatible_result = azimuth::VersionedTelemetrySerializer::deserialize(
    versioned_serializer, 
    versioned_result.data
  )
  assert_false(incompatible_result.success)
  assert_true(incompatible_result.error.contains("version_mismatch"))
  
  // Test schema validation during deserialization
  let schema_validator = azimuth::SchemaValidator::new()
  
  // Define expected schema
  let expected_schema = azimuth::TelemetrySchema::new()
    .with_required_field("timestamp", "timestamp")
    .with_required_field("service_name", "string")
    .with_required_field("metric_name", "string")
    .with_required_field("metric_value", "number")
    .with_optional_field("attributes", "object")
  
  azimuth::SchemaValidator::set_schema(schema_validator, expected_schema)
  
  // Test valid data against schema
  let valid_schema_result = azimuth::SchemaValidator::validate_data(
    schema_validator, 
    json_result.data
  )
  assert_true(valid_schema_result.valid)
  
  // Test invalid data against schema (missing required field)
  let invalid_schema_data = "{\"service_name\":\"test\",\"metric_name\":\"test\",\"metric_value\":100}"
  
  let invalid_schema_result = azimuth::SchemaValidator::validate_data(
    schema_validator, 
    invalid_schema_data
  )
  assert_false(invalid_schema_result.valid)
  assert_true(invalid_schema_result.errors.contains("missing_required_field"))
  
  // Test schema evolution handling
  let evolved_schema = azimuth::TelemetrySchema::new()
    .with_required_field("timestamp", "timestamp")
    .with_required_field("service_name", "string")
    .with_required_field("metric_name", "string")
    .with_required_field("metric_value", "number")
    .with_required_field("metric_unit", "string") // New required field
    .with_optional_field("attributes", "object")
    .with_optional_field("deprecated_field", "string") // New optional field
  
  let evolution_result = azimuth::SchemaValidator::validate_with_evolution(
    schema_validator, 
    json_result.data, 
    evolved_schema
  )
  
  // Should handle missing new field gracefully
  assert_true(evolution_result.success)
  assert_true(evolution_result.warnings.contains("missing_new_field"))
}

// Test 8: Security Error Handling
test "security error handling and threat protection" {
  // Test injection attack protection
  let security_validator = azimuth::SecurityValidator::new()
  
  // Configure security rules
  azimuth::SecurityValidator::enable_injection_protection(security_validator, true)
  azimuth::SecurityValidator::enable_xss_protection(security_validator, true)
  azimuth::SecurityValidator::enable_path_traversal_protection(security_validator, true)
  
  // Test various injection attacks
  let injection_attempts = [
    // SQL injection
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "test'; DROP TABLE users; --",
      metric_name = "test.metric",
      metric_value = 100.0,
      attributes = [("user_input", "'; DROP TABLE users; --")]
    ),
    // XSS attempt
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "test-service",
      metric_name = "<script>alert('xss')</script>",
      metric_value = 100.0,
      attributes = [("html_input", "<img src=x onerror=alert('xss')>")]
    ),
    // Path traversal
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "test-service",
      metric_name = "test.metric",
      metric_value = 100.0,
      attributes = [("file_path", "../../../etc/passwd")]
    ),
    // Command injection
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "test-service",
      metric_name = "test.metric",
      metric_value = 100.0,
      attributes = [("command", "ls; rm -rf /")]
    ),
    // LDAP injection
    azimuth::TelemetryData::new(
      timestamp = 1640995200000,
      service_name = "test-service",
      metric_name = "test.metric",
      metric_value = 100.0,
      attributes = [("ldap_filter", "*)(|(objectClass=*)(uid=*")]
    )
  ]
  
  let security_results = []
  
  for malicious_data in injection_attempts {
    let result = azimuth::SecurityValidator::validate(security_validator, malicious_data)
    security_results.push(result)
  }
  
  // All injection attempts should be blocked
  assert_eq(security_results.length(), 5)
  
  for result in security_results {
    assert_false(result.is_safe)
    assert_true(result.threats_detected.length() > 0)
    assert_true(result.sanitized_data.is_some)
  }
  
  // Test rate limiting for DDoS protection
  let rate_limiter = azimuth::SecurityRateLimiter::new(
    max_requests_per_second = 100,
    burst_size = 200
  )
  
  let ddos_results = []
  
  // Simulate burst of requests
  for i = 0; i < 300; i = i + 1 {
    let test_data = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i,
      service_name = "ddos-test",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = [("request_id", i.to_string())]
    )
    
    let result = azimuth::SecurityRateLimiter::check_rate(rate_limiter, test_data)
    ddos_results.push(result)
  }
  
  // First 200 requests should be allowed (burst size)
  // Remaining requests should be rate limited
  let allowed_count = ddos_results.filter(|r| r.allowed).length()
  let blocked_count = ddos_results.filter(|r| !r.allowed).length()
  
  assert_eq(allowed_count, 200)
  assert_eq(blocked_count, 100)
  
  // Test data encryption/decryption error handling
  let encryption_manager = azimuth::EncryptionManager::new()
  
  // Generate encryption key
  let key_result = azimuth::EncryptionManager::generate_key(encryption_manager)
  assert_true(key_result.success)
  
  let encryption_key = key_result.key
  
  // Test valid encryption/decryption
  let test_data = "sensitive telemetry data"
  
  let encrypt_result = azimuth::EncryptionManager::encrypt(encryption_manager, test_data, encryption_key)
  assert_true(encrypt_result.success)
  assert_true(encrypt_result.encrypted_data.length() > 0)
  
  let decrypt_result = azimuth::EncryptionManager::decrypt(
    encryption_manager, 
    encrypt_result.encrypted_data, 
    encryption_key
  )
  assert_true(decrypt_result.success)
  assert_eq(decrypt_result.decrypted_data, test_data)
  
  // Test decryption with wrong key
  let wrong_key = "wrong-encryption-key-12345"
  
  let wrong_key_result = azimuth::EncryptionManager::decrypt(
    encryption_manager, 
    encrypt_result.encrypted_data, 
    wrong_key
  )
  assert_false(wrong_key_result.success)
  assert_true(wrong_key_result.error.contains("invalid_key") || 
              wrong_key_result.error.contains("decryption_failed"))
  
  // Test decryption with corrupted data
  let mut corrupted_data = encrypt_result.encrypted_data.to_array()
  if corrupted_data.length() > 5 {
    corrupted_data[0] = corrupted_data[0] ^ 0xFF // Corrupt first byte
  }
  
  let corrupted_result = azimuth::EncryptionManager::decrypt(
    encryption_manager, 
    corrupted_data, 
    encryption_key
  )
  assert_false(corrupted_result.success)
  assert_true(corrupted_result.error.contains("corrupted") || 
              corrupted_result.error.contains("invalid_data"))
  
  // Test authentication and authorization errors
  let auth_manager = azimuth::AuthenticationManager::new()
  
  // Create test users
  let admin_user = azimuth::User::new("admin", "admin123", ["admin", "read", "write"])
  let readonly_user = azimuth::User::new("readonly", "read123", ["read"])
  
  azimuth::AuthenticationManager::add_user(auth_manager, admin_user)
  azimuth::AuthenticationManager::add_user(auth_manager, readonly_user)
  
  // Test authentication
  let valid_auth = azimuth::AuthenticationManager::authenticate(auth_manager, "admin", "admin123")
  assert_true(valid_auth.success)
  assert_true(valid_auth.user.is_some)
  
  let invalid_auth = azimuth::AuthenticationManager::authenticate(auth_manager, "admin", "wrongpassword")
  assert_false(invalid_auth.success)
  assert_true(invalid_auth.error.contains("invalid_credentials"))
  
  // Test authorization
  let admin_session = valid_auth.user.unwrap()
  
  let admin_write_result = azimuth::AuthenticationManager::check_permission(
    auth_manager, 
    admin_session, 
    "write"
  )
  assert_true(admin_write_result.allowed)
  
  let readonly_auth = azimuth::AuthenticationManager::authenticate(auth_manager, "readonly", "read123")
  let readonly_session = readonly_auth.user.unwrap()
  
  let readonly_write_result = azimuth::AuthenticationManager::check_permission(
    auth_manager, 
    readonly_session, 
    "write"
  )
  assert_false(readonly_write_result.allowed)
  assert_true(readonly_write_result.error.contains("insufficient_permissions"))
  
  // Test session expiration
  let expired_session = azimuth::UserSession::new(admin_session.user_id, 0) // Expired timestamp
  
  let expired_result = azimuth::AuthenticationManager::check_permission(
    auth_manager, 
    expired_session, 
    "read"
  )
  assert_false(expired_result.allowed)
  assert_true(expired_result.error.contains("session_expired"))
}

// Test 9: Resource Exhaustion and Recovery Testing
test "resource exhaustion and recovery mechanisms" {
  // Test memory exhaustion handling
  let memory_monitor = azimuth::MemoryMonitor::new()
  
  // Set memory limits
  azimuth::MemoryMonitor::set_limit(memory_monitor, 100) // 100MB limit
  
  let memory_results = []
  
  // Gradually consume memory
  let large_data_sets = []
  for i = 0; i < 10; i = i + 1 {
    let large_dataset = []
    for j = 0; j < 10000; j = j + 1 {
      large_dataset.push("memory-test-data-" + i.to_string() + "-" + j.to_string())
    }
    large_data_sets.push(large_dataset)
    
    let memory_usage = azimuth::MemoryMonitor::get_usage(memory_monitor)
    memory_results.push(memory_usage)
  }
  
  // Should detect memory pressure
  assert_true(memory_results.length() > 0)
  
  let max_memory_usage = memory_results.reduce(|max, usage| if usage > max { usage } else { max }, 0)
  assert_true(max_memory_usage > 0)
  
  // Test memory recovery
  let recovery_triggered = azimuth::MemoryMonitor::trigger_recovery(memory_monitor)
  assert_true(recovery_triggered)
  
  // Clear some data to free memory
  large_data_sets.clear()
  
  let recovered_memory = azimuth::MemoryMonitor::get_usage(memory_monitor)
  assert_true(recovered_memory < max_memory_usage)
  
  // Test disk space exhaustion handling
  let disk_monitor = azimuth::DiskMonitor::new()
  
  // Set disk limits
  azimuth::DiskMonitor::set_limit(disk_monitor, 1000) // 1GB limit
  
  // Simulate disk usage
  let disk_usage_before = azimuth::DiskMonitor::get_usage(disk_monitor)
  
  // Create large files to consume disk space
  let large_files = []
  for i = 0; i < 5; i = i + 1 {
    let file_data = "x".repeat(1000000) // 1MB of data
    let file_path = "/tmp/test-file-" + i.to_string() + ".txt"
    
    // Write file (simulated)
    large_files.push((file_path, file_data))
  }
  
  let disk_usage_after = azimuth::DiskMonitor::get_usage(disk_monitor)
  assert_true(disk_usage_after > disk_usage_before)
  
  // Test disk cleanup
  azimuth::DiskMonitor::trigger_cleanup(disk_monitor)
  
  // Remove files (simulated)
  large_files.clear()
  
  let disk_usage_cleanup = azimuth::DiskMonitor::get_usage(disk_monitor)
  assert_true(disk_usage_cleanup <= disk_usage_after)
  
  // Test connection pool exhaustion
  let connection_pool = azimuth::ConnectionPool::new(
    max_connections = 5,
    connection_timeout_ms = 5000
  )
  
  let connection_results = []
  
  // Exhaust connection pool
  for i = 0; i < 10; i = i + 1 {
    let result = azimuth::ConnectionPool::acquire_connection(connection_pool)
    connection_results.push(result)
    
    if result.success {
      // Don't release connection yet to exhaust pool
    } else {
      // Should fail when pool is exhausted
      assert_true(result.error.contains("pool_exhausted"))
    }
  }
  
  // Verify pool exhaustion
  let successful_connections = connection_results.filter(|r| r.success).length()
  assert_eq(successful_connections, 5) // Max connections
  
  // Test connection recovery
  // Release one connection
  if successful_connections > 0 {
    let connection_to_release = connection_results.find(|r| r.success).unwrap()
    azimuth::ConnectionPool::release_connection(connection_pool, connection_to_release.connection)
    
    // Should be able to acquire new connection
    let recovery_result = azimuth::ConnectionPool::acquire_connection(connection_pool)
    assert_true(recovery_result.success)
  }
  
  // Test thread pool exhaustion
  let thread_pool = azimuth::ThreadPool::new(
    max_threads = 3,
    queue_size = 10
  )
  
  let thread_results = []
  
  // Submit tasks that will block threads
  for i = 0; i < 10; i = i + 1 {
    let task_result = azimuth::ThreadPool::submit(thread_pool, || {
      // Simulate long-running task
      azimuth::Thread::sleep(1000) // 1 second
      return i
    })
    thread_results.push(task_result)
  }
  
  // Some tasks should be queued when threads are exhausted
  let queued_tasks = thread_results.filter(|r| r.queued).length()
  assert_true(queued_tasks > 0)
  
  // Wait for tasks to complete
  for result in thread_results {
    if result.queued || result.running {
      let task_result = azimuth::ThreadPool::get_result(thread_pool, result.task_id)
      assert_true(task_result.completed)
    }
  }
  
  // Test CPU exhaustion handling
  let cpu_monitor = azimuth::CPUMonitor::new()
  
  // Set CPU limits
  azimuth::CPUMonitor::set_limit(cpu_monitor, 80.0) // 80% CPU limit
  
  // Simulate CPU-intensive tasks
  let cpu_tasks = []
  for i = 0; i < 8; i = i + 1 {
    let task_id = azimuth::CPUMonitor::start_cpu_intensive_task(cpu_monitor, || {
      // CPU-intensive work
      let mut result = 0
      for j = 0; j < 1000000; j = j + 1 {
        result = result + j
      }
      result
    })
    cpu_tasks.push(task_id)
  }
  
  // Monitor CPU usage
  let cpu_usage = azimuth::CPUMonitor::get_usage(cpu_monitor)
  
  if cpu_usage > 80.0 {
    // Should trigger CPU throttling
    let throttling_active = azimuth::CPUMonitor::is_throttling_active(cpu_monitor)
    assert_true(throttling_active)
  }
  
  // Wait for CPU tasks to complete
  for task_id in cpu_tasks {
    azimuth::CPUMonitor::wait_for_task(cpu_monitor, task_id)
  }
  
  // Verify CPU usage returns to normal
  let cpu_usage_after = azimuth::CPUMonitor::get_usage(cpu_monitor)
  assert_true(cpu_usage_after < cpu_usage)
  
  // Test graceful degradation under resource pressure
  let degradation_manager = azimuth::GracefulDegradationManager::new()
  
  // Configure degradation strategies
  azimuth::GracefulDegradationManager::add_strategy(
    degradation_manager, 
    azimuth::DegradationStrategy::reduce_sampling_rate(0.5)
  )
  
  azimuth::GracefulDegradationManager::add_strategy(
    degradation_manager, 
    azimuth::DegradationStrategy::disable_optional_features(["detailed_logging", "debug_metrics"])
  )
  
  azimuth::GracefulDegradationManager::add_strategy(
    degradation_manager, 
    azimuth::DegradationStrategy::increase_batch_sizes(2.0)
  )
  
  // Simulate resource pressure
  azimuth::GracefulDegradationManager::set_resource_pressure(degradation_manager, 0.9) // 90% pressure
  
  let degradation_status = azimuth::GracefulDegradationManager::get_status(degradation_manager)
  assert_true(degradation_status.active)
  assert_true(degradation_status.applied_strategies.length() > 0)
  
  // Verify specific strategies were applied
  assert_true(degradation_status.applied_strategies.contains("reduced_sampling_rate"))
  assert_true(degradation_status.applied_strategies.contains("disabled_optional_features"))
  
  // Test recovery when pressure is reduced
  azimuth::GracefulDegradationManager::set_resource_pressure(degradation_manager, 0.3) // 30% pressure
  
  let recovery_status = azimuth::GracefulDegradationManager::get_status(degradation_manager)
  assert_false(recovery_status.active) // Should be inactive now
  assert_true(recovery_status.applied_strategies.length() == 0)
}

// Test 10: Catastrophic Failure and Recovery Testing
test "catastrophic failure and disaster recovery mechanisms" {
  // Test system crash simulation and recovery
  let crash_recovery = azimuth::CrashRecoveryManager::new()
  
  // Configure recovery settings
  azimuth::CrashRecoveryManager::set_backup_interval(crash_recovery, 60000) // 1 minute
  azimuth::CrashRecoveryManager::set_max_backups(crash_recovery, 10)
  azimuth::CrashRecoveryManager::enable_auto_recovery(crash_recovery, true)
  
  // Create some test data
  let test_data = []
  for i = 0; i < 100; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i * 1000,
      service_name = "crash-test",
      metric_name = "test.metric",
      metric_value = i.to_double(),
      attributes = [("index", i.to_string())]
    )
    test_data.push(data_point)
  }
  
  // Process data and create backup
  let processor = azimuth::TelemetryProcessor::new()
  azimuth::CrashRecoveryManager::create_backup(crash_recovery, test_data)
  
  // Simulate system crash
  azimuth::CrashRecoveryManager::simulate_crash(crash_recovery)
  
  // Verify crash detection
  let crash_detected = azimuth::CrashRecoveryManager::detect_crash(crash_recovery)
  assert_true(crash_detected)
  
  // Test recovery from backup
  let recovery_result = azimuth::CrashRecoveryManager::recover_from_backup(crash_recovery)
  assert_true(recovery_result.success)
  assert_true(recovery_result.recovered_data.is_some)
  
  let recovered_data = recovery_result.recovered_data.unwrap()
  assert_eq(recovered_data.length(), 100)
  
  // Verify data integrity after recovery
  for i = 0; i < recovered_data.length(); i = i + 1 {
    assert_eq(recovered_data[i].service_name, "crash-test")
    assert_eq(recovered_data[i].metric_name, "test.metric")
    assert_eq(recovered_data[i].metric_value, i.to_double())
  }
  
  // Test data corruption detection and repair
  let corruption_detector = azimuth::DataCorruptionDetector::new()
  
  // Configure corruption detection
  azimuth::DataCorruptionDetector::enable_checksum_validation(corruption_detector, true)
  azimuth::DataCorruptionDetector::enable_data_integrity_checks(corruption_detector, true)
  
  // Add data to corruption detector
  for data_point in test_data {
    azimuth::DataCorruptionDetector::add_data(corruption_detector, data_point)
  }
  
  // Simulate data corruption
  azimuth::DataCorruptionDetector::simulate_corruption(corruption_detector, 0.1) // 10% corruption
  
  // Detect corruption
  let corruption_result = azimuth::DataCorruptionDetector::detect_corruption(corruption_detector)
  assert_true(corruption_result.corruption_detected)
  assert_true(corruption_result.corrupted_items.length() > 0)
  
  // Test data repair
  let repair_result = azimuth::DataCorruptionDetector::repair_corruption(corruption_detector)
  assert_true(repair_result.success)
  assert_true(repair_result.repaired_items.length() > 0)
  
  // Verify repair effectiveness
  let post_repair_check = azimuth::DataCorruptionDetector::detect_corruption(corruption_detector)
  assert_true(post_repair_check.corrupted_items.length() < corruption_result.corrupted_items.length())
  
  // Test network partition handling
  let partition_handler = azimuth::NetworkPartitionHandler::new()
  
  // Configure partition handling
  azimuth::NetworkPartitionHandler::set_partition_timeout(partition_handler, 30000) // 30 seconds
  azimuth::NetworkPartitionHandler::enable_buffering_during_partition(partition_handler, true)
  
  // Simulate network partition
  azimuth::NetworkPartitionHandler::simulate_partition(partition_handler)
  
  // Verify partition detection
  let partition_detected = azimuth::NetworkPartitionHandler::detect_partition(partition_handler)
  assert_true(partition_detected)
  
  // Test data buffering during partition
  let partition_data = []
  for i = 0; i < 50; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i * 1000,
      service_name = "partition-test",
      metric_name = "buffered.metric",
      metric_value = i.to_double(),
      attributes = []
    )
    partition_data.push(data_point)
    
    let result = azimuth::NetworkPartitionHandler::process_during_partition(
      partition_handler, 
      data_point
    )
    assert_true(result.buffered)
  }
  
  // Verify buffered data
  let buffered_data = azimuth::NetworkPartitionHandler::get_buffered_data(partition_handler)
  assert_eq(buffered_data.length(), 50)
  
  // Simulate partition recovery
  azimuth::NetworkPartitionHandler::simulate_partition_recovery(partition_handler)
  
  // Test buffered data replay
  let replay_result = azimuth::NetworkPartitionHandler::replay_buffered_data(partition_handler)
  assert_true(replay_result.success)
  assert_eq(replay_result.replayed_count, 50)
  
  // Test failover mechanisms
  let failover_manager = azimuth::FailoverManager::new()
  
  // Configure primary and backup systems
  let primary_system = azimuth::TelemetrySystem::new("primary")
  let backup_system = azimuth::TelemetrySystem::new("backup")
  
  azimuth::FailoverManager::add_primary(failover_manager, primary_system)
  azimuth::FailoverManager::add_backup(failover_manager, backup_system)
  
  azimuth::FailoverManager::enable_health_checks(failover_manager, true)
  azimuth::FailoverManager::set_health_check_interval(failover_manager, 5000) // 5 seconds
  
  // Test normal operation with primary
  let normal_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "failover-test",
    metric_name = "normal.metric",
    metric_value = 100.0,
    attributes = []
  )
  
  let normal_result = azimuth::FailoverManager::process(failover_manager, normal_data)
  assert_true(normal_result.success)
  assert_eq(normal_result.processed_by, "primary")
  
  // Simulate primary system failure
  azimuth::FailoverManager::simulate_primary_failure(failover_manager)
  
  // Test failover to backup
  let failover_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "failover-test",
    metric_name = "failover.metric",
    metric_value = 200.0,
    attributes = []
  )
  
  let failover_result = azimuth::FailoverManager::process(failover_manager, failover_data)
  assert_true(failover_result.success)
  assert_eq(failover_result.processed_by, "backup")
  assert_true(failover_result.failover_triggered)
  
  // Test primary recovery and failback
  azimuth::FailoverManager::simulate_primary_recovery(failover_manager)
  
  let failback_data = azimuth::TelemetryData::new(
    timestamp = 1640995200000,
    service_name = "failover-test",
    metric_name = "failback.metric",
    metric_value = 300.0,
    attributes = []
  )
  
  let failback_result = azimuth::FailoverManager::process(failover_manager, failback_data)
  assert_true(failback_result.success)
  assert_eq(failback_result.processed_by, "primary")
  assert_true(failback_result.failback_triggered)
  
  // Test disaster recovery scenario
  let disaster_recovery = azimuth::DisasterRecoveryManager::new()
  
  // Configure disaster recovery
  azimuth::DisasterRecoveryManager::set_recovery_point_objective(disaster_recovery, 3600000) // 1 hour RPO
  azimuth::DisasterRecoveryManager::set_recovery_time_objective(disaster_recovery, 1800000) // 30 minutes RTO
  
  // Create disaster recovery backup
  let critical_data = []
  for i = 0; i < 1000; i = i + 1 {
    let data_point = azimuth::TelemetryData::new(
      timestamp = 1640995200000 + i * 1000,
      service_name = "critical-service",
      metric_name = "critical.metric",
      metric_value = i.to_double(),
      attributes = [("critical", "true")]
    )
    critical_data.push(data_point)
  }
  
  let backup_result = azimuth::DisasterRecoveryManager::create_disaster_backup(
    disaster_recovery, 
    critical_data
  )
  assert_true(backup_result.success)
  assert_true(backup_result.backup_id.length() > 0)
  
  // Simulate disaster scenario
  azimuth::DisasterRecoveryManager::simulate_disaster(disaster_recovery)
  
  // Test disaster recovery
  let recovery_start_time = azimuth::Time::now()
  
  let disaster_recovery_result = azimuth::DisasterRecoveryManager::execute_disaster_recovery(
    disaster_recovery, 
    backup_result.backup_id
  )
  
  let recovery_end_time = azimuth::Time::now()
  let recovery_time = recovery_end_time - recovery_start_time
  
  assert_true(disaster_recovery_result.success)
  assert_true(recovery_time <= 1800000) // Should meet RTO
  
  let recovered_critical_data = disaster_recovery_result.recovered_data
  assert_eq(recovered_critical_data.length(), 1000)
  
  // Verify recovery point objective
  let backup_timestamp = backup_result.backup_timestamp
  let disaster_timestamp = azimuth::DisasterRecoveryManager::get_disaster_timestamp(disaster_recovery)
  let data_loss_window = disaster_timestamp - backup_timestamp
  
  assert_true(data_loss_window <= 3600000) // Should meet RPO
}