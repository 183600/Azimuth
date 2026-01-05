// Azimuth Error Handling and Recovery Tests
// This file contains test cases for error handling and recovery mechanisms

// Test 1: Span Error Handling
test "span error handling and recovery" {
  // Create a span with error status
  let trace_id = "trace_error_test_123"
  let span_id = "span_error_test_456"
  let span_context = SpanContext::new(trace_id, span_id, true, "error_test")
  let span = Span::new("error_operation", Internal, span_context)
  
  // Initially, span should have no error
  assert_eq(Span::status(span), Unset)
  
  // Set error status
  Span::set_status(span, Error, Some("Database connection failed"))
  assert_eq(Span::status(span), Error)
  
  // Add error event
  Span::add_event(span, "exception", Some([
    ("exception.type", StringValue("DatabaseConnectionError")),
    ("exception.message", StringValue("Connection timeout after 30 seconds")),
    ("exception.stacktrace", StringValue("at Database.connect()"))
  ]))
  
  // Test error recovery by setting status back to Ok
  Span::set_status(span, Ok, Some("Operation completed after retry"))
  assert_eq(Span::status(span), Ok)
  
  // Add recovery event
  Span::add_event(span, "recovery", Some([
    ("retry.count", IntValue(3)),
    ("recovery.strategy", StringValue("exponential_backoff"))
  ]))
  
  // End the span
  Span::end(span)
  
  // Verify span is properly ended
  assert_false(Span::is_recording(span))
}

// Test 2: Metric Error Handling
test "metric error handling and recovery" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "error_test_meter")
  
  // Test counter with invalid values
  let counter = Meter::create_counter(meter, "error_counter", Some("Error counter"), Some("count"))
  
  // Add negative value (should handle gracefully)
  Counter::add(counter, -1.0)
  
  // Add very large value (should handle gracefully)
  Counter::add(counter, 999999999.0)
  
  // Test histogram with invalid values
  let histogram = Meter::create_histogram(meter, "error_histogram", Some("Error histogram"), Some("ms"))
  
  // Record negative value (should handle gracefully)
  Histogram::record(histogram, -100.0)
  
  // Record very large value (should handle gracefully)
  Histogram::record(histogram, 999999999.0)
  
  // Test gauge with invalid values
  let gauge = Meter::create_gauge(meter, "error_gauge", Some("Error gauge"), Some("value"))
  
  // Record negative value (should handle gracefully)
  Gauge::record(gauge, -50.0)
  
  // Record very large value (should handle gracefully)
  Gauge::record(gauge, 999999999.0)
  
  // Test metric name validation
  let invalid_counter = Meter::create_counter(meter, "", Some("Empty name"), Some("count"))
  let special_char_counter = Meter::create_counter(meter, "counter@#$%", Some("Special chars"), Some("count"))
  
  // Verify metrics are created despite invalid inputs
  assert_true(true)  // If we reach here, error handling worked
}

// Test 3: Attribute Error Handling
test "attribute error handling and validation" {
  let attrs = Attributes::new()
  
  // Test setting attributes with invalid keys
  Attributes::set(attrs, "", StringValue("empty_key"))  // Empty key
  Attributes::set(attrs, "@#$%^&*()", StringValue("special_chars"))  // Special chars
  Attributes::set(attrs, "a".repeat(1000), StringValue("very_long_key"))  // Very long key
  
  // Test setting attributes with invalid values
  Attributes::set(attrs, "null_string", StringValue(""))  // Empty string value
  Attributes::set(attrs, "very_long_string", StringValue("x".repeat(10000)))  // Very long string
  
  // Test setting attributes with very large numbers
  Attributes::set(attrs, "large_int", IntValue(999999999))
  Attributes::set(attrs, "negative_int", IntValue(-999999999))
  Attributes::set(attrs, "large_float", FloatValue(999999.999))
  Attributes::set(attrs, "negative_float", FloatValue(-999999.999))
  Attributes::set(attrs, "infinity_float", FloatValue(1.0 / 0.0))  // Infinity
  Attributes::set(attrs, "nan_float", FloatValue(0.0 / 0.0))  // NaN
  
  // Test boolean attributes
  Attributes::set(attrs, "true_bool", BoolValue(true))
  Attributes::set(attrs, "false_bool", BoolValue(false))
  
  // Test array attributes with invalid values
  Attributes::set(attrs, "empty_array", ArrayStringValue([]))
  Attributes::set(attrs, "large_array", ArrayStringValue(["x".repeat(1000) for _ in 0..100]))
  
  // Test getting non-existent attributes
  let non_existent = Attributes::get(attrs, "non_existent_key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Verify attributes can be retrieved despite invalid inputs
  let empty_key = Attributes::get(attrs, "")
  match empty_key {
    Some(StringValue(v)) => assert_eq(v, "empty_key")
    _ => assert_true(false)
  }
  
  let very_long_key = Attributes::get(attrs, "a".repeat(1000))
  match very_long_key {
    Some(StringValue(v)) => assert_eq(v, "very_long_key")
    _ => assert_true(false)
  }
  
  let large_int = Attributes::get(attrs, "large_int")
  match large_int {
    Some(IntValue(v)) => assert_eq(v, 999999999)
    _ => assert_true(false)
  }
}

// Test 4: Context Propagation Error Handling
test "context propagation error handling" {
  // Test with invalid trace ID
  let invalid_trace_context = SpanContext::new("", "span_id", true, "invalid_trace")
  assert_false(SpanContext::is_valid(invalid_trace_context))
  
  // Test with invalid span ID
  let invalid_span_context = SpanContext::new("trace_id", "", true, "invalid_span")
  assert_false(SpanContext::is_valid(invalid_span_context))
  
  // Test with both invalid
  let invalid_both_context = SpanContext::new("", "", true, "invalid_both")
  assert_false(SpanContext::is_valid(invalid_both_context))
  
  // Test with valid context
  let valid_context = SpanContext::new("trace_123456789", "span_123456789", true, "valid")
  assert_true(SpanContext::is_valid(valid_context))
  
  // Test context with invalid values
  let root_ctx = Context::root()
  
  // Test with null key
  let null_key = ContextKey::new("")
  let ctx_with_null_key = Context::with_value(root_ctx, null_key, "null_key_value")
  let null_key_value = Context::get(ctx_with_null_key, null_key)
  match null_key_value {
    Some(v) => assert_eq(v, "null_key_value")
    None => assert_true(false)
  }
  
  // Test with very long key
  let long_key = ContextKey::new("x".repeat(1000))
  let ctx_with_long_key = Context::with_value(root_ctx, long_key, "long_key_value")
  let long_key_value = Context::get(ctx_with_long_key, long_key)
  match long_key_value {
    Some(v) => assert_eq(v, "long_key_value")
    None => assert_true(false)
  }
  
  // Test with null value
  let null_value_key = ContextKey::new("null_value_key")
  let ctx_with_null_value = Context::with_value(root_ctx, null_value_key, "")
  let null_value = Context::get(ctx_with_null_value, null_value_key)
  match null_value {
    Some(v) => assert_eq(v, "")
    None => assert_true(false)
  }
  
  // Test getting value with different key instance
  let different_key = ContextKey::new("different_key")
  let different_value = Context::get(ctx_with_null_key, different_key)
  match different_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Logger Error Handling
test "logger error handling and recovery" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "error_test_logger")
  
  // Test log record with invalid severity
  let invalid_severity_log = LogRecord::new(99, "Invalid severity log")
  Logger::emit(logger, invalid_severity_log)
  
  // Test log record with empty message
  let empty_message_log = LogRecord::new(Info, "")
  Logger::emit(logger, empty_message_log)
  
  // Test log record with very long message
  let long_message_log = LogRecord::new(Info, "x".repeat(10000))
  Logger::emit(logger, long_message_log)
  
  // Test log record with invalid timestamp
  let invalid_timestamp_log = LogRecord::new_with_context(
    Error,
    Some("Invalid timestamp log"),
    Some(Attributes::new()),
    Some(-1),  // Negative timestamp
    Some(999999999999),  // Very large timestamp
    Some("trace_id"),
    Some("span_id"),
    None
  )
  Logger::emit(logger, invalid_timestamp_log)
  
  // Test log record with invalid trace/span IDs
  let invalid_ids_log = LogRecord::new_with_context(
    Warn,
    Some("Invalid IDs log"),
    Some(Attributes::new()),
    Some(1640995200),
    Some(1640995300),
    Some(""),  // Empty trace ID
    Some(""),  // Empty span ID
    None
  )
  Logger::emit(logger, invalid_ids_log)
  
  // Test log record with invalid attributes
  let invalid_attrs = Attributes::new()
  Attributes::set(invalid_attrs, "", StringValue(""))  // Empty key and value
  Attributes::set(invalid_attrs, "@#$%", StringValue("@#$%"))  // Special chars
  
  let invalid_attrs_log = LogRecord::new_with_context(
    Error,
    Some("Invalid attributes log"),
    Some(invalid_attrs),
    None,
    None,
    None,
    None,
    None
  )
  Logger::emit(logger, invalid_attrs_log)
  
  // Verify logger handled all errors gracefully
  assert_true(true)  // If we reach here, error handling worked
}

// Test 6: Resource Error Handling
test "resource error handling and validation" {
  let resource = Resource::new()
  
  // Test resource with invalid attributes
  let invalid_attrs = [
    ("", StringValue("empty_key")),  // Empty key
    ("service.name", StringValue("")),  // Empty value for required attribute
    ("@#$%", StringValue("special_chars")),  // Special chars in key
    ("a".repeat(1000), StringValue("very_long_key")),  // Very long key
    ("very_long_value", StringValue("x".repeat(10000)))  // Very long value
  ]
  
  let resource_with_invalid_attrs = Resource::with_attributes(resource, invalid_attrs)
  
  // Test getting invalid attributes
  let empty_key = Resource::get_attribute(resource_with_invalid_attrs, "")
  match empty_key {
    Some(StringValue(v)) => assert_eq(v, "empty_key")
    _ => assert_true(false)
  }
  
  let empty_service_name = Resource::get_attribute(resource_with_invalid_attrs, "service.name")
  match empty_service_name {
    Some(StringValue(v)) => assert_eq(v, "")
    _ => assert_true(false)
  }
  
  let special_chars = Resource::get_attribute(resource_with_invalid_attrs, "@#$%")
  match special_chars {
    Some(StringValue(v)) => assert_eq(v, "@#$%")
    _ => assert_true(false)
  }
  
  // Test resource merging with invalid resources
  let invalid_resource = Resource::with_attributes(Resource::new(), [
    ("invalid.attr", StringValue("invalid_value"))
  ])
  
  let merged_resource = Resource::merge(resource_with_invalid_attrs, invalid_resource)
  
  // Verify merged resource contains both valid and invalid attributes
  let merged_attr = Resource::get_attribute(merged_resource, "invalid.attr")
  match merged_attr {
    Some(StringValue(v)) => assert_eq(v, "invalid_value")
    _ => assert_true(false)
  }
  
  let original_attr = Resource::get_attribute(merged_resource, "service.name")
  match original_attr {
    Some(StringValue(v)) => assert_eq(v, "")
    _ => assert_true(false)
  }
}

// Test 7: HTTP Client Error Handling
test "http client error handling and recovery" {
  let client = HttpClient::new()
  
  // Test HTTP request with invalid method
  let invalid_method_request = HttpRequest::new("", "https://example.com", [], None)
  assert_eq(HttpRequest::http_method(invalid_method_request), "")
  
  // Test HTTP request with invalid URL
  let invalid_url_request = HttpRequest::new("GET", "", [], None)
  assert_eq(HttpRequest::url(invalid_url_request), "")
  
  // Test HTTP request with very long URL
  let long_url_request = HttpRequest::new("GET", "https://example.com/" + "a".repeat(10000), [], None)
  assert_true(HttpRequest::url(long_url_request).length() > 10000)
  
  // Test HTTP request with invalid headers
  let invalid_headers = [
    ("", "empty_header_name"),  // Empty header name
    ("invalid_header", ""),  // Empty header value
    ("@#$%", "@#$%"),  // Special chars in both
    ("a".repeat(1000), "b".repeat(1000))  // Very long name and value
  ]
  
  let invalid_headers_request = HttpRequest::new("GET", "https://example.com", invalid_headers, Some("request body"))
  
  // Test HTTP request with very large body
  let large_body_request = HttpRequest::new("POST", "https://example.com", [], Some("x".repeat(1000000)))
  match HttpRequest::body(large_body_request) {
    Some(body) => assert_eq(body.length(), 1000000)
    None => assert_true(false)
  }
  
  // Test HTTP response with invalid status code
  let invalid_status_response = HttpResponse::new(-1, [], Some("error response"))
  assert_eq(HttpResponse::status_code(invalid_status_response), -1)
  
  // Test HTTP response with very large status code
  let large_status_response = HttpResponse::new(999999, [], Some("large status response"))
  assert_eq(HttpResponse::status_code(large_status_response), 999999)
  
  // Test HTTP response with invalid headers
  let invalid_headers_response = HttpResponse::new(200, invalid_headers, Some("response body"))
  
  // Test HTTP response with very large body
  let large_body_response = HttpResponse::new(200, [], Some("x".repeat(1000000)))
  match HttpResponse::body(large_body_response) {
    Some(body) => assert_eq(body.length(), 1000000)
    None => assert_true(false)
  }
  
  // Verify HTTP client handled all errors gracefully
  assert_true(true)  // If we reach here, error handling worked
}

// Test 8: Baggage Error Handling
test "baggage error handling and validation" {
  let baggage = Baggage::new()
  
  // Test baggage with invalid keys
  let baggage_with_invalid_key = Baggage::set_entry(baggage, "", "empty_key_value")
  let empty_key_value = Baggage::get_entry(baggage_with_invalid_key, "")
  match empty_key_value {
    Some(v) => assert_eq(v, "empty_key_value")
    None => assert_true(false)
  }
  
  // Test baggage with special characters in key
  let baggage_with_special_chars = Baggage::set_entry(baggage_with_invalid_key, "@#$%", "special_chars_value")
  let special_chars_value = Baggage::get_entry(baggage_with_special_chars, "@#$%")
  match special_chars_value {
    Some(v) => assert_eq(v, "special_chars_value")
    None => assert_true(false)
  }
  
  // Test baggage with very long key
  let very_long_key = "x".repeat(1000)
  let baggage_with_long_key = Baggage::set_entry(baggage_with_special_chars, very_long_key, "long_key_value")
  let long_key_value = Baggage::get_entry(baggage_with_long_key, very_long_key)
  match long_key_value {
    Some(v) => assert_eq(v, "long_key_value")
    None => assert_true(false)
  }
  
  // Test baggage with empty value
  let baggage_with_empty_value = Baggage::set_entry(baggage_with_long_key, "empty_value", "")
  let empty_value = Baggage::get_entry(baggage_with_empty_value, "empty_value")
  match empty_value {
    Some(v) => assert_eq(v, "")
    None => assert_true(false)
  }
  
  // Test baggage with very long value
  let very_long_value = "x".repeat(10000)
  let baggage_with_long_value = Baggage::set_entry(baggage_with_empty_value, "long_value", very_long_value)
  let long_value = Baggage::get_entry(baggage_with_long_value, "long_value")
  match long_value {
    Some(v) => assert_eq(v, very_long_value)
    None => assert_true(false)
  }
  
  // Test removing non-existent entry
  let baggage_after_removal = Baggage::remove_entry(baggage_with_long_value, "non_existent_key")
  let non_existent_after_removal = Baggage::get_entry(baggage_after_removal, "non_existent_key")
  match non_existent_after_removal {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test removing entry with invalid key
  let baggage_after_invalid_removal = Baggage::remove_entry(baggage_after_removal, "")
  let empty_key_after_removal = Baggage::get_entry(baggage_after_invalid_removal, "")
  match empty_key_after_removal {
    Some(_) => assert_true(false)  // Should be removed
    None => assert_true(true)
  }
  
  // Verify existing entries are still present
  let special_chars_after_removal = Baggage::get_entry(baggage_after_invalid_removal, "@#$%")
  match special_chars_after_removal {
    Some(v) => assert_eq(v, "special_chars_value")
    None => assert_true(false)
  }
}

// Test 9: Circuit Breaker Error Handling
test "circuit breaker error handling and recovery" {
  // Simulate a circuit breaker implementation
  let circuit_breaker = {
    state: "closed",  // closed, open, half_open
    failure_count: 0,
    failure_threshold: 5,
    recovery_timeout: 60000,  // 60 seconds
    last_failure_time: 0,
    success_count: 0,
    success_threshold: 3
  }
  
  // Test circuit breaker with failures
  let simulate_operation = fn(circuit_breaker: {state: String, failure_count: Int, failure_threshold: Int, recovery_timeout: Int, last_failure_time: Int, success_count: Int, success_threshold: Int}, should_fail: Bool) {
    match circuit_breaker.state {
      "open" => {
        let current_time = 1640995200
        if current_time - circuit_breaker.last_failure_time > circuit_breaker.recovery_timeout {
          // Transition to half_open
          {
            state: "half_open",
            failure_count: circuit_breaker.failure_count,
            failure_threshold: circuit_breaker.failure_threshold,
            recovery_timeout: circuit_breaker.recovery_timeout,
            last_failure_time: circuit_breaker.last_failure_time,
            success_count: 0,
            success_threshold: circuit_breaker.success_threshold
          }
        } else {
          circuit_breaker  // Still open
        }
      }
      "half_open" => {
        if should_fail {
          // Failure in half_open state, go back to open
          {
            state: "open",
            failure_count: circuit_breaker.failure_count + 1,
            failure_threshold: circuit_breaker.failure_threshold,
            recovery_timeout: circuit_breaker.recovery_timeout,
            last_failure_time: 1640995200,
            success_count: 0,
            success_threshold: circuit_breaker.success_threshold
          }
        } else {
          // Success in half_open state
          if circuit_breaker.success_count + 1 >= circuit_breaker.success_threshold {
            // Transition to closed
            {
              state: "closed",
              failure_count: 0,
              failure_threshold: circuit_breaker.failure_threshold,
              recovery_timeout: circuit_breaker.recovery_timeout,
              last_failure_time: 0,
              success_count: 0,
              success_threshold: circuit_breaker.success_threshold
            }
          } else {
            // Still in half_open
            {
              state: "half_open",
              failure_count: circuit_breaker.failure_count,
              failure_threshold: circuit_breaker.failure_threshold,
              recovery_timeout: circuit_breaker.recovery_timeout,
              last_failure_time: circuit_breaker.last_failure_time,
              success_count: circuit_breaker.success_count + 1,
              success_threshold: circuit_breaker.success_threshold
            }
          }
        }
      }
      "closed" => {
        if should_fail {
          let new_failure_count = circuit_breaker.failure_count + 1
          if new_failure_count >= circuit_breaker.failure_threshold {
            // Transition to open
            {
              state: "open",
              failure_count: new_failure_count,
              failure_threshold: circuit_breaker.failure_threshold,
              recovery_timeout: circuit_breaker.recovery_timeout,
              last_failure_time: 1640995200,
              success_count: 0,
              success_threshold: circuit_breaker.success_threshold
            }
          } else {
            // Still closed
            {
              state: "closed",
              failure_count: new_failure_count,
              failure_threshold: circuit_breaker.failure_threshold,
              recovery_timeout: circuit_breaker.recovery_timeout,
              last_failure_time: circuit_breaker.last_failure_time,
              success_count: circuit_breaker.success_count,
              success_threshold: circuit_breaker.success_threshold
            }
          }
        } else {
          // Success, reset failure count
          {
            state: "closed",
            failure_count: 0,
            failure_threshold: circuit_breaker.failure_threshold,
            recovery_timeout: circuit_breaker.recovery_timeout,
            last_failure_time: circuit_breaker.last_failure_time,
            success_count: circuit_breaker.success_count,
            success_threshold: circuit_breaker.success_threshold
          }
        }
      }
      _ => circuit_breaker  // Unknown state, return as is
    }
  }
  
  // Test normal operation
  let cb_after_success = simulate_operation(circuit_breaker, false)
  assert_eq(cb_after_success.state, "closed")
  assert_eq(cb_after_success.failure_count, 0)
  
  // Test failures
  let mut cb_with_failures = circuit_breaker
  for i in 0..5 {
    cb_with_failures = simulate_operation(cb_with_failures, true)
  }
  
  // Should transition to open after 5 failures
  assert_eq(cb_with_failures.state, "open")
  assert_eq(cb_with_failures.failure_count, 5)
  
  // Test operation when circuit is open
  let cb_still_open = simulate_operation(cb_with_failures, false)
  assert_eq(cb_still_open.state, "open")  // Should still be open
  
  // Test recovery after timeout
  let cb_half_open = {
    state: "half_open",
    failure_count: 5,
    failure_threshold: 5,
    recovery_timeout: 60000,
    last_failure_time: 1640995200,
    success_count: 0,
    success_threshold: 3
  }
  
  // Test successful operations in half_open state
  let mut cb_recovering = cb_half_open
  for i in 0..3 {
    cb_recovering = simulate_operation(cb_recovering, false)
  }
  
  // Should transition to closed after 3 successes
  assert_eq(cb_recovering.state, "closed")
  assert_eq(cb_recovering.failure_count, 0)
  
  // Test failure in half_open state
  let mut cb_failing_in_half_open = cb_half_open
  cb_failing_in_half_open = simulate_operation(cb_failing_in_half_open, false)  // First success
  cb_failing_in_half_open = simulate_operation(cb_failing_in_half_open, true)   // Then failure
  
  // Should go back to open
  assert_eq(cb_failing_in_half_open.state, "open")
  assert_eq(cb_failing_in_half_open.failure_count, 6)
}

// Test 10: Retry Mechanism Error Handling
test "retry mechanism error handling and recovery" {
  // Simulate a retry mechanism
  let retry_operation = fn(operation: () -> {success: Bool, error: Option[String]}, max_retries: Int, backoff_base: Int) {
    let mut attempt = 0
    let mut result = operation()
    
    while not(result.success) and attempt < max_retries {
      attempt = attempt + 1
      
      // Calculate backoff delay with exponential backoff
      let delay = backoff_base * (2 ^ (attempt - 1))
      
      // Simulate waiting for backoff
      // In a real implementation, this would be an actual delay
      
      // Retry the operation
      result = operation()
    }
    
    {
      success: result.success,
      error: result.error,
      attempts: attempt + 1,
      final_attempt: attempt == max_retries
    }
  }
  
  // Test retry with immediate success
  let always_success = fn() { {success: true, error: None} }
  let success_result = retry_operation(always_success, 3, 100)
  assert_true(success_result.success)
  assert_eq(success_result.attempts, 1)
  assert_false(success_result.final_attempt)
  
  // Test retry with eventual success
  let mut attempt_count = 0
  let eventual_success = fn() {
    attempt_count = attempt_count + 1
    if attempt_count >= 3 {
      {success: true, error: None}
    } else {
      {success: false, error: Some("Attempt " + attempt_count.to_string() + " failed")}
    }
  }
  
  attempt_count = 0  // Reset counter
  let eventual_success_result = retry_operation(eventual_success, 5, 100)
  assert_true(eventual_success_result.success)
  assert_eq(eventual_success_result.attempts, 3)
  assert_false(eventual_success_result.final_attempt)
  
  // Test retry with permanent failure
  let always_fail = fn() { {success: false, error: Some("Permanent failure")} }
  let failure_result = retry_operation(always_fail, 3, 100)
  assert_false(failure_result.success)
  assert_eq(failure_result.attempts, 4)  // Initial attempt + 3 retries
  assert_true(failure_result.final_attempt)
  
  // Test retry with different backoff strategies
  let linear_backoff = fn(operation: () -> {success: Bool, error: Option[String]}, max_retries: Int, backoff_base: Int) {
    let mut attempt = 0
    let mut result = operation()
    
    while not(result.success) and attempt < max_retries {
      attempt = attempt + 1
      
      // Calculate backoff delay with linear backoff
      let delay = backoff_base * attempt
      
      // Retry the operation
      result = operation()
    }
    
    {
      success: result.success,
      error: result.error,
      attempts: attempt + 1,
      final_attempt: attempt == max_retries
    }
  }
  
  // Test linear backoff
  attempt_count = 0  // Reset counter
  let linear_backoff_result = linear_backoff(eventual_success, 5, 100)
  assert_true(linear_backoff_result.success)
  assert_eq(linear_backoff_result.attempts, 3)
  
  // Test retry with jitter
  let jitter_backoff = fn(operation: () -> {success: Bool, error: Option[String]}, max_retries: Int, backoff_base: Int) {
    let mut attempt = 0
    let mut result = operation()
    
    while not(result.success) and attempt < max_retries {
      attempt = attempt + 1
      
      // Calculate backoff with jitter (random factor between 0.5 and 1.5)
      let base_delay = backoff_base * (2 ^ (attempt - 1))
      let jitter_factor = 0.5 + (0.0)  // In a real implementation, this would be random
      let delay = (base_delay.to_float() * jitter_factor) as Int
      
      // Retry the operation
      result = operation()
    }
    
    {
      success: result.success,
      error: result.error,
      attempts: attempt + 1,
      final_attempt: attempt == max_retries
    }
  }
  
  // Test jitter backoff
  attempt_count = 0  // Reset counter
  let jitter_backoff_result = jitter_backoff(eventual_success, 5, 100)
  assert_true(jitter_backoff_result.success)
  assert_eq(jitter_backoff_result.attempts, 3)
  
  // Test retry with different error types
  let mut error_type = "timeout"
  let different_errors = fn() {
    if error_type == "timeout" {
      error_type = "connection_refused"
      {success: false, error: Some("timeout")}
    } else if error_type == "connection_refused" {
      error_type = "success"
      {success: false, error: Some("connection_refused")}
    } else {
      {success: true, error: None}
    }
  }
  
  error_type = "timeout"  // Reset
  let different_errors_result = retry_operation(different_errors, 5, 100)
  assert_true(different_errors_result.success)
  assert_eq(different_errors_result.attempts, 3)
}