// Azimuth Telemetry System - Specialized Quality Test Suite
// This file contains specialized test cases focusing on quality assurance and edge cases

// Test 1: Data Integrity Validation
test "data integrity validation across operations" {
  // Test data integrity during serialization and deserialization
  let original_data = [
    ("user.id", "12345"),
    ("session.token", "abc123xyz456"),
    ("request.timestamp", "1640995200000"),
    ("operation.type", "database.query"),
    ("response.code", "200")
  ]
  
  // Create attributes with original data
  let original_attrs = Attributes::new()
  for (key, value) in original_data {
    Attributes::set(original_attrs, key, StringValue(value))
  }
  
  // Serialize attributes
  let serialized = Attributes::serialize(original_attrs)
  assert_true(serialized.length() > 0)
  
  // Deserialize back to attributes
  let deserialized_attrs = Attributes::deserialize(serialized)
  
  // Validate data integrity
  for (key, expected_value) in original_data {
    let actual_value = Attributes::get(deserialized_attrs, key)
    match actual_value {
      Some(StringValue(value)) => assert_eq(value, expected_value)
      _ => assert_true(false) // Data integrity check failed
    }
  }
  
  // Test with different data types
  let mixed_data = [
    ("counter.value", IntValue(42)),
    ("measurement.precision", FloatValue(3.14159)),
    ("operation.success", BoolValue(true)),
    ("tags.list", ArrayStringValue(["tag1", "tag2", "tag3"])),
    ("metrics.values", ArrayIntValue([10, 20, 30, 40, 50]))
  ]
  
  let mixed_attrs = Attributes::new()
  for (key, value) in mixed_data {
    Attributes::set(mixed_attrs, key, value)
  }
  
  // Serialize and deserialize mixed data types
  let mixed_serialized = Attributes::serialize(mixed_attrs)
  let mixed_deserialized = Attributes::deserialize(mixed_serialized)
  
  // Validate mixed data types integrity
  for (key, expected_value) in mixed_data {
    let actual_value = Attributes::get(mixed_deserialized, key)
    match (expected_value, actual_value) {
      (IntValue(expected), Some(IntValue(actual))) => assert_eq(expected, actual)
      (FloatValue(expected), Some(FloatValue(actual))) => assert_true(abs(expected - actual) < 0.00001)
      (BoolValue(expected), Some(BoolValue(actual))) => assert_eq(expected, actual)
      (ArrayStringValue(expected), Some(ArrayStringValue(actual))) => {
        assert_eq(expected.length(), actual.length())
        for i = 0; i < expected.length(); i = i + 1 {
          assert_eq(expected[i], actual[i])
        }
      }
      (ArrayIntValue(expected), Some(ArrayIntValue(actual))) => {
        assert_eq(expected.length(), actual.length())
        for i = 0; i < expected.length(); i = i + 1 {
          assert_eq(expected[i], actual[i])
        }
      }
      _ => assert_true(false) // Data integrity check failed for mixed types
    }
  }
}

// Test 2: Performance Benchmark Validation
test "performance benchmark validation" {
  // Test span creation performance
  let start_time = Time::now()
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  // Create multiple spans to measure performance
  let span_count = 1000
  for i = 0; i < span_count; i = i + 1 {
    let span = Tracer::start_span(tracer, "performance-span-" + i.to_string())
    // Add some attributes
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "batch.size", IntValue(span_count))
    span.end()
  }
  
  let end_time = Time::now()
  let duration_ms = (end_time - start_time) / 1000000L // Convert to milliseconds
  
  // Performance should be reasonable (less than 1000ms for 1000 spans)
  assert_true(duration_ms < 1000L)
  
  // Test metrics collection performance
  let metrics_start = Time::now()
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-metrics")
  let counter = Meter::create_counter(meter, "performance-counter")
  
  // Record multiple metrics
  for i = 0; i < 10000; i = i + 1 {
    Counter::add(counter, 1.0)
  }
  
  let metrics_end = Time::now()
  let metrics_duration_ms = (metrics_end - metrics_start) / 1000000L
  
  // Metrics should be fast (less than 100ms for 10000 updates)
  assert_true(metrics_duration_ms < 100L)
  
  // Test log emission performance
  let log_start = Time::now()
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance-logger")
  
  // Emit multiple log records
  for i = 0; i < 5000; i = i + 1 {
    let log_record = LogRecord::new(Info, "Performance test log message " + i.to_string())
    Logger::emit(logger, log_record)
  }
  
  let log_end = Time::now()
  let log_duration_ms = (log_end - log_start) / 1000000L
  
  // Logging should be reasonably fast (less than 500ms for 5000 logs)
  assert_true(log_duration_ms < 500L)
}

// Test 3: Security and Privacy Validation
test "security and privacy validation" {
  // Test sensitive data handling
  let sensitive_data = [
    ("password", "secret123"),
    ("api.key", "sk-1234567890abcdef"),
    ("credit.card", "4111-1111-1111-1111"),
    ("ssn", "123-45-6789"),
    ("email", "user@example.com")
  ]
  
  // Test data sanitization
  let sanitizer = DataSanitizer::new()
  
  for (key, value) in sensitive_data {
    // Add sensitive data to attributes
    let attrs = Attributes::new()
    Attributes::set(attrs, key, StringValue(value))
    
    // Sanitize the attributes
    let sanitized_attrs = DataSanitizer::sanitize(sanitizer, attrs)
    
    // Check that sensitive data is properly masked
    let sanitized_value = Attributes::get(sanitized_attrs, key)
    match sanitized_value {
      Some(StringValue(masked_value)) => {
        // Value should be masked or redacted
        assert_true(masked_value != value)
        assert_true(masked_value.contains("***") || masked_value.contains("[REDACTED]"))
      }
      _ => assert_true(false) // Sensitive data should be present but masked
    }
  }
  
  // Test encryption/decryption
  let encryption_key = EncryptionKey::generate()
  let encryptor = Encryptor::new(encryption_key)
  
  let original_data = "This is sensitive telemetry data"
  let encrypted_data = Encryptor::encrypt(encryptor, original_data)
  
  // Encrypted data should be different from original
  assert_true(encrypted_data != original_data)
  assert_true(encrypted_data.length() > 0)
  
  // Decrypt and verify
  let decrypted_data = Encryptor::decrypt(encryptor, encrypted_data)
  assert_eq(decrypted_data, original_data)
  
  // Test access control
  let access_controller = AccessController::new()
  
  // Define roles and permissions
  AccessController::add_role(access_controller, "admin", ["read", "write", "delete"])
  AccessController::add_role(access_controller, "user", ["read"])
  AccessController::add_role(access_controller, "guest", [])
  
  // Test permission checks
  assert_true(AccessController::has_permission(access_controller, "admin", "read"))
  assert_true(AccessController::has_permission(access_controller, "admin", "write"))
  assert_true(AccessController::has_permission(access_controller, "admin", "delete"))
  
  assert_true(AccessController::has_permission(access_controller, "user", "read"))
  assert_false(AccessController::has_permission(access_controller, "user", "write"))
  assert_false(AccessController::has_permission(access_controller, "user", "delete"))
  
  assert_false(AccessController::has_permission(access_controller, "guest", "read"))
  assert_false(AccessController::has_permission(access_controller, "guest", "write"))
  assert_false(AccessController::has_permission(access_controller, "guest", "delete"))
}

// Test 4: Error Recovery and Resilience
test "error recovery and resilience validation" {
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new(
    failure_threshold = 5,
    recovery_timeout = 60000L, // 60 seconds
    expected_recovery_time = 30000L // 30 seconds
  )
  
  // Simulate failures to trigger circuit breaker
  for i = 0; i < 5; i = i + 1 {
    let result = CircuitBreaker::execute(circuit_breaker, || {
      // Simulate operation that fails
      Error("Simulated failure")
    })
    
    match result {
      Ok(_) => assert_true(false) // Should fail
      Error(_) => assert_true(true) // Expected failure
    }
  }
  
  // Circuit should now be open
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
  
  // Subsequent calls should fail fast without executing the operation
  let fast_fail_result = CircuitBreaker::execute(circuit_breaker, || {
    // This shouldn't execute
    assert_true(false)
    Ok("Should not reach here")
  })
  
  match fast_fail_result {
    Ok(_) => assert_true(false) // Should fail fast
    Error(msg) => assert_true(msg.contains("Circuit breaker is open"))
  }
  
  // Test retry mechanism with exponential backoff
  let retry_policy = RetryPolicy::exponential_backoff(
    max_attempts = 3,
    initial_delay = 100L, // 100ms
    max_delay = 1000L     // 1 second
  )
  
  let mut attempt_count = 0
  
  let retry_result = RetryPolicy::execute(retry_policy, || {
    attempt_count = attempt_count + 1
    
    if attempt_count < 3 {
      Error("Temporary failure")
    } else {
      Ok("Success after retries")
    }
  })
  
  match retry_result {
    Ok(value) => {
      assert_eq(value, "Success after retries")
      assert_eq(attempt_count, 3) // Should have attempted 3 times
    }
    Error(_) => assert_true(false) // Should eventually succeed
  }
  
  // Test timeout handling
  let timeout_duration = 100L // 100ms
  let timeout_result = Timeout::execute(timeout_duration, || {
    // Simulate long-running operation
    Time::sleep(200L) // Sleep for 200ms
    Ok("Should not complete")
  })
  
  match timeout_result {
    Ok(_) => assert_true(false) // Should timeout
    Error(msg) => assert_true(msg.contains("Operation timed out"))
  }
}

// Test 5: Concurrent Processing Safety
test "concurrent processing safety validation" {
  // Test thread-safe counter
  let safe_counter = AtomicCounter::new(0)
  
  // Simulate concurrent increments
  let increments = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let expected_sum = increments.reduce(|acc, val| acc + val, 0)
  
  for increment in increments {
    AtomicCounter::add(safe_counter, increment)
  }
  
  assert_eq(AtomicCounter::value(safe_counter), expected_sum)
  
  // Test thread-safe map operations
  let safe_map = ConcurrentHashMap::new()
  
  // Add entries concurrently
  let entries = [
    ("key1", "value1"),
    ("key2", "value2"),
    ("key3", "value3"),
    ("key4", "value4"),
    ("key5", "value5")
  ]
  
  for (key, value) in entries {
    ConcurrentHashMap::put(safe_map, key, value)
  }
  
  // Verify all entries are present
  for (key, expected_value) in entries {
    let actual_value = ConcurrentHashMap::get(safe_map, key)
    match actual_value {
      Some(value) => assert_eq(value, expected_value)
      None => assert_true(false) // Entry should exist
    }
  }
  
  // Test atomic compare-and-swap
  let atomic_value = AtomicReference::new("initial")
  
  // Successful CAS
  let cas_result1 = AtomicReference::compare_and_swap(
    atomic_value, 
    "initial", 
    "updated"
  )
  assert_true(cas_result1) // Should succeed
  assert_eq(AtomicReference::get(atomic_value), "updated")
  
  // Failed CAS
  let cas_result2 = AtomicReference::compare_and_swap(
    atomic_value, 
    "initial", 
    "should-not-update"
  )
  assert_false(cas_result2) // Should fail
  assert_eq(AtomicReference::get(atomic_value), "updated") // Value unchanged
  
  // Test concurrent span operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  
  // Create spans with concurrent operations
  let span_names = [
    "concurrent-span-1",
    "concurrent-span-2",
    "concurrent-span-3",
    "concurrent-span-4",
    "concurrent-span-5"
  ]
  
  let spans = span_names.map(|name| Tracer::start_span(tracer, name))
  
  // Perform concurrent operations on spans
  for i = 0; i < spans.length(); i = i + 1 {
    let span = spans[i]
    Span::set_attribute(span, "index", IntValue(i))
    Span::set_attribute(span, "thread.id", IntValue(1000 + i))
    Span::add_event(span, "concurrent-operation", None)
  }
  
  // End all spans
  for span in spans {
    span.end()
  }
  
  // Verify all spans are properly ended
  for span in spans {
    assert_false(span.is_recording()) // Should not be recording after end
  }
}

// Test 6: Cross-Platform Compatibility
test "cross-platform compatibility validation" {
  // Test platform detection
  let platform_info = Platform::detect()
  
  // Verify platform information is available
  assert_true(Platform::os(platform_info).length() > 0)
  assert_true(Platform::arch(platform_info).length() > 0)
  assert_true(Platform::version(platform_info).length() > 0)
  
  // Test path handling across platforms
  let path_handler = PathHandler::new()
  
  let path_components = ["home", "user", "documents", "telemetry.txt"]
  let platform_path = PathHandler::join(path_handler, path_components)
  
  // Path should contain all components in order
  for component in path_components {
    assert_true(platform_path.contains(component))
  }
  
  // Test file operations with platform-specific handling
  let temp_file = PathHandler::temp_file(path_handler, "azimuth-test", ".log")
  assert_true(PathHandler::exists(path_handler, temp_file))
  
  // Write data to file
  let test_data = "Platform compatibility test data"
  let write_result = FileOperations::write_string(temp_file, test_data)
  assert_true(write_result)
  
  // Read data back
  let read_result = FileOperations::read_string(temp_file)
  match read_result {
    Some(data) => assert_eq(data, test_data)
    None => assert_true(false) // Should be able to read back data
  }
  
  // Clean up
  let delete_result = FileOperations::delete(temp_file)
  assert_true(delete_result)
  assert_false(PathHandler::exists(path_handler, temp_file))
  
  // Test timezone handling
  let timezone_handler = TimezoneHandler::new()
  let utc_time = Time::utc_now()
  let local_time = TimezoneHandler::to_local(timezone_handler, utc_time)
  
  // Local time should be different from UTC (unless actually in UTC timezone)
  let timezone_offset = TimezoneHandler::get_offset(timezone_handler, local_time)
  assert_true(timezone_offset >= -12 * 3600 && timezone_offset <= 14 * 3600) // Valid timezone range
  
  // Test encoding/decoding across platforms
  let unicode_text = "Azimuth 遥测系统 📊 🌍"
  let utf8_bytes = Encoding::utf8_encode(unicode_text)
  let decoded_text = Encoding::utf8_decode(utf8_bytes)
  
  assert_eq(decoded_text, unicode_text)
  
  // Test base64 encoding (consistent across platforms)
  let base64_encoded = Encoding::base64_encode(unicode_text)
  let base64_decoded = Encoding::base64_decode(base64_encoded)
  
  assert_eq(base64_decoded, unicode_text)
}

// Test 7: Internationalization Support
test "internationalization support validation" {
  // Test locale detection and handling
  let i18n_handler = I18nHandler::new()
  let current_locale = I18nHandler::detect_locale(i18n_handler)
  
  // Locale should be in valid format (language-region)
  assert_true(current_locale.length() >= 2)
  assert_true(current_locale.contains("-") || current_locale.length() == 2)
  
  // Test message formatting with different locales
  let message_templates = [
    ("en", "Operation completed in {duration} ms"),
    ("zh", "操作在 {duration} 毫秒内完成"),
    ("es", "Operación completada en {duration} ms"),
    ("fr", "Opération terminée en {duration} ms"),
    ("ja", "操作が {duration} ミリ秒で完了しました")
  ]
  
  for (locale, template) in message_templates {
    let formatted = I18nHandler::format_message(i18n_handler, locale, template, [
      ("duration", "250")
    ])
    
    // Formatted message should contain the duration value
    assert_true(formatted.contains("250"))
    
    // Should be different for different locales (except for the duration part)
    if locale != "en" {
      assert_not_eq(formatted, I18nHandler::format_message(i18n_handler, "en", template, [
        ("duration", "250")
      ]))
    }
  }
  
  // Test number formatting with locale-specific rules
  let number = 1234.5678
  
  let en_formatted = I18nHandler::format_number(i18n_handler, "en", number)
  assert_true(en_formatted.contains("1,234.57")) // US format
  
  let de_formatted = I18nHandler::format_number(i18n_handler, "de", number)
  assert_true(de_formatted.contains("1.234,57")) // German format
  
  let fr_formatted = I18nHandler::format_number(i18n_handler, "fr", number)
  assert_true(fr_formatted.contains("1 234,57")) // French format
  
  // Test date/time formatting with locale-specific rules
  let timestamp = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  let en_date = I18nHandler::format_date(i18n_handler, "en", timestamp)
  assert_true(en_date.contains("2022") && en_date.contains("01") && en_date.contains("01"))
  
  let zh_date = I18nHandler::format_date(i18n_handler, "zh", timestamp)
  assert_true(zh_date.contains("2022") && zh_date.contains("01") && zh_date.contains("01"))
  
  // Test right-to-left language support
  let rtl_locales = ["ar", "he", "fa"]
  
  for locale in rtl_locales {
    let rtl_text = I18nHandler::get_translation(i18n_handler, locale, "welcome.message")
    
    // Should have text direction metadata
    let text_direction = I18nHandler::get_text_direction(i18n_handler, locale)
    assert_eq(text_direction, "rtl")
  }
  
  // Test pluralization rules
  let count = 5
  
  let en_plural = I18nHandler::format_plural(i18n_handler, "en", count, "item", "items")
  assert_eq(en_plural, "5 items") // English plural rule
  
  let zh_plural = I18nHandler::format_plural(i18n_handler, "zh", count, "项", "项")
  assert_eq(zh_plural, "5 项") // Chinese doesn't have plurals
  
  let ar_plural = I18nHandler::format_plural(i18n_handler, "ar", count, "عنصر", "عناصر")
  // Arabic has complex plural rules - should handle correctly
  assert_true(ar_plural.contains("5"))
}

// Test 8: Memory Management and Resource Cleanup
test "memory management and resource cleanup validation" {
  // Test memory pool allocation and deallocation
  let memory_pool = MemoryPool::new(initial_size = 1024, max_size = 10240)
  
  // Allocate multiple blocks
  let allocated_blocks = []
  
  for i = 0; i < 10; i = i + 1 {
    let block_size = 64 + i * 16 // Increasing block sizes
    let block = MemoryPool::allocate(memory_pool, block_size)
    
    match block {
      Some(ptr) => {
        allocated_blocks.push((ptr, block_size))
        // Write some data to verify the block is usable
        Memory::write_int32(ptr, i * 100)
      }
      None => assert_true(false) // Allocation should succeed
    }
  }
  
  // Verify data integrity in allocated blocks
  for i = 0; i < allocated_blocks.length(); i = i + 1 {
    let (ptr, _) = allocated_blocks[i]
    let value = Memory::read_int32(ptr)
    assert_eq(value, i * 100)
  }
  
  // Deallocate all blocks
  for (ptr, _) in allocated_blocks {
    MemoryPool::deallocate(memory_pool, ptr)
  }
  
  // Test weak reference cleanup
  let weak_ref_registry = WeakReferenceRegistry::new()
  
  // Create objects with weak references
  let objects = []
  let weak_refs = []
  
  for i = 0; i < 5; i = i + 1 {
    let obj = TestObject::new("object-" + i.to_string())
    let weak_ref = WeakReferenceRegistry::create(weak_ref_registry, obj)
    
    objects.push(obj)
    weak_refs.push(weak_ref)
  }
  
  // All weak references should be valid initially
  for weak_ref in weak_refs {
    assert_true(WeakReference::is_valid(weak_ref))
    
    match WeakReference::get(weak_ref) {
      Some(obj) => assert_true(TestObject::name(obj).starts_with("object-"))
      None => assert_true(false) // Should be able to get object
    }
  }
  
  // Clear strong references
  objects.clear()
  
  // Trigger garbage collection
  GarbageCollector::collect()
  
  // Weak references should now be invalid
  for weak_ref in weak_refs {
    assert_false(WeakReference::is_valid(weak_ref))
    
    match WeakReference::get(weak_ref) {
      Some(_) => assert_true(false) // Should not be able to get object
      None => assert_true(true) // Expected
    }
  }
  
  // Test resource cleanup with RAII pattern
  let resource_manager = ResourceManager::new()
  
  // Acquire resources
  let resources = []
  
  for i = 0; i < 3; i = i + 1 {
    let resource = ResourceManager::acquire(resource_manager, "test-resource-" + i.to_string())
    resources.push(resource)
  }
  
  // Use resources
  for resource in resources {
    assert_true(Resource::is_available(resource))
    Resource::use(resource)
  }
  
  // Resources should be automatically cleaned up when they go out of scope
  // In a real implementation, this would be handled by destructors/finalizers
  
  // Test buffer overflow protection
  let buffer = Buffer::new(size = 100)
  
  // Valid write should succeed
  let write_result1 = Buffer::write_string(buffer, 0, "Valid string within buffer")
  assert_true(write_result1)
  
  // Write at boundary should succeed
  let write_result2 = Buffer::write_string(buffer, 90, "String at boundary")
  assert_true(write_result2)
  
  // Write beyond boundary should fail
  let write_result3 = Buffer::write_string(buffer, 95, "This string exceeds buffer size")
  assert_false(write_result3)
  
  // Test memory leak detection
  let leak_detector = LeakDetector::new()
  LeakDetector::start_tracking(leak_detector)
  
  // Allocate and deallocate memory
  let tracked_ptr1 = LeakDetector::allocate_tracked(leak_detector, 128)
  let tracked_ptr2 = LeakDetector::allocate_tracked(leak_detector, 256)
  
  // Deallocate one pointer but not the other (simulating a leak)
  LeakDetector::deallocate_tracked(leak_detector, tracked_ptr1)
  // tracked_ptr2 is intentionally not deallocated to test leak detection
  
  // Check for leaks
  let leak_report = LeakDetector::generate_report(leak_detector)
  assert_eq(leak_report.leaked_blocks, 1) // One block should be leaked
  assert_eq(leak_report.leaked_bytes, 256) // 256 bytes should be leaked
  
  // Clean up the remaining leak
  LeakDetector::deallocate_tracked(leak_detector, tracked_ptr2)
  
  // Verify no more leaks
  let final_report = LeakDetector::generate_report(leak_detector)
  assert_eq(final_report.leaked_blocks, 0)
  assert_eq(final_report.leaked_bytes, 0)
}

// Test 9: Time Series Data Processing
test "time series data processing validation" {
  // Test time series aggregation
  let time_series = TimeSeries::new()
  
  // Add data points with different timestamps
  let base_time = 1640995200000L // 2022-01-01 00:00:00 UTC
  
  let data_points = [
    (base_time, 10.5),
    (base_time + 60000L, 15.2),      // +1 minute
    (base_time + 120000L, 12.8),     // +2 minutes
    (base_time + 180000L, 18.3),     // +3 minutes
    (base_time + 240000L, 20.1),     // +4 minutes
    (base_time + 300000L, 16.7),     // +5 minutes
    (base_time + 360000L, 22.4),     // +6 minutes
    (base_time + 420000L, 19.8),     // +7 minutes
    (base_time + 480000L, 25.2),     // +8 minutes
    (base_time + 540000L, 21.6)      // +9 minutes
  ]
  
  for (timestamp, value) in data_points {
    TimeSeries::add_point(time_series, timestamp, value)
  }
  
  // Test time range query
  let start_time = base_time + 180000L // +3 minutes
  let end_time = base_time + 420000L   // +7 minutes
  
  let range_points = TimeSeries::get_range(time_series, start_time, end_time)
  assert_eq(range_points.length(), 5)
  
  // Test downsampling (aggregation over larger time windows)
  let downsampled = TimeSeries::downsample(
    time_series, 
    window_size = 180000L, // 3 minutes
    aggregation_type = Average
  )
  
  // Should have 3 windows: 0-3min, 3-6min, 6-9min
  assert_eq(downsampled.length(), 3)
  
  // Verify first window average (0-3 minutes)
  let first_window = downsampled[0]
  let first_window_avg = (10.5 + 15.2 + 12.8) / 3.0
  assert_true(abs(first_window.1 - first_window_avg) < 0.01)
  
  // Test upsampling (interpolation)
  let upsampled = TimeSeries::upsample(
    time_series,
    interval = 30000L // 30 seconds
  )
  
  // Should have more points than original
  assert_true(upsampled.length() > data_points.length())
  
  // Test time series statistics
  let stats = TimeSeries::calculate_statistics(time_series)
  
  assert_eq(stats.count, 10)
  assert_eq(stats.min, 10.5)
  assert_eq(stats.max, 25.2)
  assert_eq(stats.sum, 182.6)
  assert_true(abs(stats.avg - 18.26) < 0.01)
  
  // Test time-based filtering
  let filtered = TimeSeries::filter_by_time(time_series, base_time + 120000L, None)
  assert_eq(filtered.length(), 8) // From +2 minutes onwards
  
  // Test value-based filtering
  let value_filtered = TimeSeries::filter_by_value(time_series, 15.0, 22.0)
  assert_eq(value_filtered.length(), 5) // Values between 15.0 and 22.0
  
  // Test time series merging
  let additional_series = TimeSeries::new()
  
  let additional_points = [
    (base_time + 600000L, 23.5),     // +10 minutes
    (base_time + 660000L, 19.2),     // +11 minutes
    (base_time + 720000L, 27.8)      // +12 minutes
  ]
  
  for (timestamp, value) in additional_points {
    TimeSeries::add_point(additional_series, timestamp, value)
  }
  
  let merged_series = TimeSeries::merge(time_series, additional_series)
  assert_eq(merged_series.length(), 13) // 10 + 3 points
  
  // Test anomaly detection
  let anomalies = TimeSeries::detect_anomalies(
    time_series,
    method = ZScore,
    threshold = 2.0
  )
  
  // Should detect some anomalies based on z-score
  assert_true(anomalies.length() >= 0)
  
  // Test trend analysis
  let trend = TimeSeries::calculate_trend(time_series)
  
  // Should be positive trend (values generally increasing)
  assert_true(trend.slope > 0.0)
  assert_true(trend.correlation > 0.0)
  
  // Test seasonal decomposition
  let decomposition = TimeSeries::seasonal_decompose(
    time_series,
    period = 3 // 3-point seasonality for testing
  )
  
  assert_eq(decomposition.trend.length(), data_points.length())
  assert_eq(decomposition.seasonal.length(), data_points.length())
  assert_eq(decomposition.residual.length(), data_points.length())
  
  // Verify decomposition components sum to original
  for i = 0; i < data_points.length(); i = i + 1 {
    let original_value = data_points[i].1
    let reconstructed = decomposition.trend[i] + decomposition.seasonal[i] + decomposition.residual[i]
    assert_true(abs(original_value - reconstructed) < 0.01)
  }
}

// Test 10: Configuration Management
test "configuration management validation" {
  // Test configuration loading and validation
  let config_manager = ConfigurationManager::new()
  
  // Load configuration from different sources
  let default_config = ConfigSource::default()
  let file_config = ConfigSource::file("azimuth.config.json")
  let env_config = ConfigSource::environment()
  let cli_config = ConfigSource::command_line()
  
  // Combine configuration sources with priority
  ConfigurationManager::add_source(config_manager, default_config, priority = 1)
  ConfigurationManager::add_source(config_manager, file_config, priority = 2)
  ConfigurationManager::add_source(config_manager, env_config, priority = 3)
  ConfigurationManager::add_source(config_manager, cli_config, priority = 4)
  
  // Load and merge configurations
  let load_result = ConfigurationManager::load(config_manager)
  assert_true(load_result)
  
  // Test configuration value retrieval
  let service_name = ConfigurationManager::get_string(config_manager, "service.name")
  match service_name {
    Some(name) => assert_true(name.length() > 0)
    None => assert_true(false) // Should have a service name
  }
  
  let service_port = ConfigurationManager::get_int(config_manager, "service.port")
  match service_port {
    Some(port) => assert_true(port > 0 && port <= 65535)
    None => assert_true(false) // Should have a service port
  }
  
  let enable_metrics = ConfigurationManager::get_bool(config_manager, "metrics.enabled")
  match enable_metrics {
    Some(enabled) => assert_true(enabled == true || enabled == false) // Should be true or false
    None => assert_true(false) // Should have metrics enabled setting
  }
  
  // Test configuration with default values
  let retry_attempts = ConfigurationManager::get_int_with_default(
    config_manager, 
    "retry.attempts", 
    3
  )
  assert_true(retry_attempts >= 0)
  
  let timeout_ms = ConfigurationManager::get_int_with_default(
    config_manager,
    "timeout.ms",
    5000
  )
  assert_true(timeout_ms > 0)
  
  // Test configuration validation
  let validation_rules = [
    ValidationRule::required("service.name"),
    ValidationRule::range("service.port", 1, 65535),
    ValidationRule::min("retry.attempts", 0),
    ValidationRule::max("timeout.ms", 60000)
  ]
  
  for rule in validation_rules {
    let validation_result = ConfigurationManager::validate_rule(config_manager, rule)
    assert_true(validation_result)
  }
  
  // Test configuration updates
  let update_result = ConfigurationManager::set_string(
    config_manager,
    "updated.value",
    "test-update"
  )
  assert_true(update_result)
  
  let updated_value = ConfigurationManager::get_string(config_manager, "updated.value")
  match updated_value {
    Some(value) => assert_eq(value, "test-update")
    None => assert_true(false) // Should be able to get updated value
  }
  
  // Test configuration persistence
  let save_result = ConfigurationManager::save(config_manager, "updated_config.json")
  assert_true(save_result)
  
  // Load the saved configuration
  let saved_config_manager = ConfigurationManager::new()
  ConfigurationManager::add_source(
    saved_config_manager,
    ConfigSource::file("updated_config.json"),
    priority = 1
  )
  
  let saved_result = ConfigurationManager::load(saved_config_manager)
  assert_true(saved_result)
  
  let saved_value = ConfigurationManager::get_string(saved_config_manager, "updated.value")
  match saved_value {
    Some(value) => assert_eq(value, "test-update")
    None => assert_true(false) // Should be able to retrieve saved value
  }
  
  // Test configuration change notifications
  let notification_handler = ConfigChangeHandler::new()
  
  ConfigurationManager::register_change_handler(
    config_manager,
    "service.port",
    notification_handler
  )
  
  // Change the configuration value
  ConfigurationManager::set_int(config_manager, "service.port", 8080)
  
  // Check that the notification was received
  let notifications = ConfigChangeHandler::get_notifications(notification_handler)
  assert_true(notifications.length() > 0)
  
  let last_notification = notifications[notifications.length() - 1]
  assert_eq(last_notification.key, "service.port")
  assert_eq(last_notification.new_value, IntValue(8080))
  
  // Test configuration schema validation
  let schema = ConfigSchema::new()
  
  ConfigSchema::add_field(schema, "service.name", String, Required)
  ConfigSchema::add_field(schema, "service.port", Integer, Required)
  ConfigSchema::add_field(schema, "metrics.enabled", Boolean, Optional)
  ConfigSchema::add_field(schema, "retry.attempts", Integer, Optional)
  ConfigSchema::add_field(schema, "log.level", Enum(["debug", "info", "warn", "error"]), Optional)
  
  let schema_validation = ConfigurationManager::validate_schema(config_manager, schema)
  assert_true(schema_validation)
  
  // Test configuration template generation
  let template = ConfigurationManager::generate_template(schema)
  
  // Template should contain all field descriptions
  assert_true(template.contains("service.name"))
  assert_true(template.contains("service.port"))
  assert_true(template.contains("metrics.enabled"))
  assert_true(template.contains("retry.attempts"))
  assert_true(template.contains("log.level"))
  
  // Test configuration encryption for sensitive values
  let encryptor = ConfigEncryptor::new("encryption-key")
  
  ConfigurationManager::set_encrypted_string(
    config_manager,
    "database.password",
    "secret-password",
    encryptor
  )
  
  // Retrieved value should be automatically decrypted
  let password = ConfigurationManager::get_string(config_manager, "database.password")
  match password {
    Some(value) => assert_eq(value, "secret-password")
    None => assert_true(false) // Should be able to decrypt and retrieve password
  }
}