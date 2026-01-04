// Data Validation Tests for Azimuth Telemetry System
// This file contains test cases for data validation and integrity checks

// Test 1: Attribute Value Validation
test "attribute value validation" {
  let validator = AttributeValidator::new()
  
  // Test string attribute validation
  let valid_string = StringValue("valid_string")
  assert_true(AttributeValidator::is_valid(validator, valid_string))
  
  let empty_string = StringValue("")
  assert_true(AttributeValidator::is_valid(validator, empty_string))
  
  let long_string = StringValue("a" * 1000) // 1000 character string
  assert_true(AttributeValidator::is_valid(validator, long_string))
  
  // Test int attribute validation
  let valid_int = IntValue(42)
  assert_true(AttributeValidator::is_valid(validator, valid_int))
  
  let zero_int = IntValue(0)
  assert_true(AttributeValidator::is_valid(validator, zero_int))
  
  let negative_int = IntValue(-100)
  assert_true(AttributeValidator::is_valid(validator, negative_int))
  
  let max_int = IntValue(2147483647)
  assert_true(AttributeValidator::is_valid(validator, max_int))
  
  // Test float attribute validation
  let valid_float = FloatValue(3.14)
  assert_true(AttributeValidator::is_valid(validator, valid_float))
  
  let zero_float = FloatValue(0.0)
  assert_true(AttributeValidator::is_valid(validator, zero_float))
  
  let negative_float = FloatValue(-2.71)
  assert_true(AttributeValidator::is_valid(validator, negative_float))
  
  let infinity_float = FloatValue(Float::infinity())
  assert_true(AttributeValidator::is_valid(validator, infinity_float))
  
  let nan_float = FloatValue(Float::nan())
  assert_false(AttributeValidator::is_valid(validator, nan_float)) // NaN should be invalid
  
  // Test bool attribute validation
  let true_bool = BoolValue(true)
  assert_true(AttributeValidator::is_valid(validator, true_bool))
  
  let false_bool = BoolValue(false)
  assert_true(AttributeValidator::is_valid(validator, false_bool))
  
  // Test array attribute validation
  let valid_string_array = ArrayStringValue(["a", "b", "c"])
  assert_true(AttributeValidator::is_valid(validator, valid_string_array))
  
  let empty_string_array = ArrayStringValue([])
  assert_true(AttributeValidator::is_valid(validator, empty_string_array))
  
  let large_string_array = ArrayStringValue([])
  for i in 1..=1000 {
    large_string_array.push("item_" + i.to_string())
  }
  assert_true(AttributeValidator::is_valid(validator, large_string_array))
  
  let valid_int_array = ArrayIntValue([1, 2, 3, 4, 5])
  assert_true(AttributeValidator::is_valid(validator, valid_int_array))
  
  let mixed_int_array = ArrayIntValue([0, -1, 100, -100, 2147483647])
  assert_true(AttributeValidator::is_valid(validator, mixed_int_array))
}

// Test 2: Span Context Validation
test "span context validation" {
  let validator = SpanContextValidator::new()
  
  // Test valid span contexts
  let valid_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let valid_span_id = "b7ad6b7169203331"
  let valid_ctx = SpanContext::new(valid_trace_id, valid_span_id, true, "valid_state")
  assert_true(SpanContextValidator::is_valid(validator, valid_ctx))
  
  // Test invalid trace IDs
  let empty_trace_id = ""
  let invalid_ctx1 = SpanContext::new(empty_trace_id, valid_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx1))
  
  let short_trace_id = "0af7651916cd43dd8448eb211c80319" // 31 characters instead of 32
  let invalid_ctx2 = SpanContext::new(short_trace_id, valid_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx2))
  
  let long_trace_id = "0af7651916cd43dd8448eb211c80319cc" // 33 characters instead of 32
  let invalid_ctx3 = SpanContext::new(long_trace_id, valid_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx3))
  
  let invalid_hex_trace_id = "zg7651916cd43dd8448eb211c80319c" // Contains 'g' which is not valid hex
  let invalid_ctx4 = SpanContext::new(invalid_hex_trace_id, valid_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx4))
  
  // Test invalid span IDs
  let empty_span_id = ""
  let invalid_ctx5 = SpanContext::new(valid_trace_id, empty_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx5))
  
  let short_span_id = "b7ad6b716920333" // 15 characters instead of 16
  let invalid_ctx6 = SpanContext::new(valid_trace_id, short_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx6))
  
  let long_span_id = "b7ad6b71692033311" // 17 characters instead of 16
  let invalid_ctx7 = SpanContext::new(valid_trace_id, long_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx7))
  
  let invalid_hex_span_id = "b7ad6b71692033g1" // Contains 'g' which is not valid hex
  let invalid_ctx8 = SpanContext::new(valid_trace_id, invalid_hex_span_id, true, "invalid_state")
  assert_false(SpanContextValidator::is_valid(validator, invalid_ctx8))
  
  // Test edge cases
  let all_zero_trace_id = "00000000000000000000000000000000"
  let all_zero_span_id = "0000000000000000"
  let zero_ctx = SpanContext::new(all_zero_trace_id, all_zero_span_id, true, "zero_state")
  assert_false(SpanContextValidator::is_valid(validator, zero_ctx)) // All zeros should be invalid
  
  let all_f_trace_id = "ffffffffffffffffffffffffffffffff"
  let all_f_span_id = "ffffffffffffffff"
  let f_ctx = SpanContext::new(all_f_trace_id, all_f_span_id, true, "f_state")
  assert_true(SpanContextValidator::is_valid(validator, f_ctx)) // All 'f's should be valid
}

// Test 3: Telemetry Data Validation
test "telemetry data validation" {
  let validator = TelemetryDataValidator::new()
  
  // Test valid telemetry data
  let valid_attrs = Attributes::new()
  Attributes::set(valid_attrs, "service.name", StringValue("test_service"))
  Attributes::set(valid_attrs, "operation.name", StringValue("test_operation"))
  
  let valid_data = TelemetryData::new(42.0, valid_attrs, 1234567890L)
  assert_true(TelemetryDataValidator::is_valid(validator, valid_data))
  
  // Test invalid values
  let nan_data = TelemetryData::new(Float::nan(), valid_attrs, 1234567890L)
  assert_false(TelemetryDataValidator::is_valid(validator, nan_data))
  
  let infinity_data = TelemetryData::new(Float::infinity(), valid_attrs, 1234567890L)
  assert_false(TelemetryDataValidator::is_valid(validator, infinity_data))
  
  // Test invalid timestamps
  let negative_timestamp_data = TelemetryData::new(42.0, valid_attrs, -1L)
  assert_false(TelemetryDataValidator::is_valid(validator, negative_timestamp_data))
  
  let future_timestamp_data = TelemetryData::new(42.0, valid_attrs, 9999999999999L) // Far future
  assert_true(TelemetryDataValidator::is_valid(validator, future_timestamp_data)) // Future timestamps should be valid
  
  // Test data with invalid attributes
  let invalid_attrs = Attributes::new()
  Attributes::set(invalid_attrs, "invalid.attr", StringValue("")) // Empty string value
  
  let data_with_invalid_attrs = TelemetryData::new(42.0, invalid_attrs, 1234567890L)
  assert_true(TelemetryDataValidator::is_valid(validator, data_with_invalid_attrs)) // Empty strings should be valid
  
  // Test data with too many attributes
  let many_attrs = Attributes::new()
  for i in 1..=1000 {
    Attributes::set(many_attrs, "attr_" + i.to_string(), StringValue("value_" + i.to_string()))
  }
  
  let data_with_many_attrs = TelemetryData::new(42.0, many_attrs, 1234567890L)
  assert_true(TelemetryDataValidator::is_valid(validator, data_with_many_attrs)) // Many attributes should be valid
}

// Test 4: Metric Validation
test "metric validation" {
  let validator = MetricValidator::new()
  
  // Test valid metric names
  let valid_names = [
    "counter",
    "request_count",
    "response_time_ms",
    "cpu_usage_percentage",
    "memory.bytes",
    "http_requests_total",
    "database_connections_active"
  ]
  
  for name in valid_names {
    assert_true(MetricValidator::is_valid_name(validator, name))
  }
  
  // Test invalid metric names
  let invalid_names = [
    "", // Empty
    "1invalid_starting_with_digit", // Starts with digit
    "invalid-name-with-dashes", // Contains dashes
    "invalid name with spaces", // Contains spaces
    "invalid.name.with.dots", // Contains dots
    "invalid$name", // Contains special character
    "a" * 300 // Too long (over 255 characters)
  ]
  
  for name in invalid_names {
    assert_false(MetricValidator::is_valid_name(validator, name))
  }
  
  // Test valid metric values
  let valid_values = [0.0, 1.0, -1.0, 3.14, -2.71, 1000000.0, -1000000.0]
  for value in valid_values {
    assert_true(MetricValidator::is_valid_value(validator, value))
  }
  
  // Test invalid metric values
  let invalid_values = [Float::nan(), Float::infinity(), Float::neg_infinity()]
  for value in invalid_values {
    assert_false(MetricValidator::is_valid_value(validator, value))
  }
  
  // Test valid metric units
  let valid_units = [
    "", // Empty unit
    "bytes",
    "ms",
    "seconds",
    "percent",
    "count",
    "items",
    "By", // SI unit
    "ms", // Time unit
    "%", // Percent
    "1" // Dimensionless
  ]
  
  for unit in valid_units {
    assert_true(MetricValidator::is_valid_unit(validator, unit))
  }
  
  // Test invalid metric units
  let invalid_units = [
    "invalid unit with spaces",
    "unit-with-dashes",
    "unit@with#special$chars",
    "a" * 100 // Too long
  ]
  
  for unit in invalid_units {
    assert_false(MetricValidator::is_valid_unit(validator, unit))
  }
}

// Test 5: Log Record Validation
test "log record validation" {
  let validator = LogRecordValidator::new()
  
  // Test valid log records
  let valid_log = LogRecord::new(Info, "Test log message")
  assert_true(LogRecordValidator::is_valid(validator, valid_log))
  
  let log_with_attrs = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    Some(Attributes::new()),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  assert_true(LogRecordValidator::is_valid(validator, log_with_attrs))
  
  // Test invalid severity levels
  let invalid_severity_log = LogRecord::new_with_context(
    999, // Invalid severity level
    Some("Invalid severity message"),
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_false(LogRecordValidator::is_valid(validator, invalid_severity_log))
  
  // Test log with extremely long message
  let long_message = "a" * 100000 // 100k character message
  let long_message_log = LogRecord::new(Info, long_message)
  assert_true(LogRecordValidator::is_valid(validator, long_message_log)) // Long messages should be valid
  
  // Test log with invalid timestamps
  let negative_timestamp_log = LogRecord::new_with_context(
    Info,
    Some("Test message"),
    None,
    Some(-1L),
    None,
    None,
    None,
    None
  )
  assert_false(LogRecordValidator::is_valid(validator, negative_timestamp_log))
  
  // Test log with invalid trace/span IDs
  let log_with_invalid_ids = LogRecord::new_with_context(
    Info,
    Some("Test message"),
    None,
    None,
    None,
    Some("invalid_trace_id"), // Invalid format
    Some("invalid_span_id"), // Invalid format
    None
  )
  assert_false(LogRecordValidator::is_valid(validator, log_with_invalid_ids))
  
  // Test log with valid trace/span IDs
  let log_with_valid_ids = LogRecord::new_with_context(
    Info,
    Some("Test message"),
    None,
    None,
    None,
    Some("0af7651916cd43dd8448eb211c80319c"), // Valid trace ID
    Some("b7ad6b7169203331"), // Valid span ID
    None
  )
  assert_true(LogRecordValidator::is_valid(validator, log_with_valid_ids))
}

// Test 6: Resource Validation
test "resource validation" {
  let validator = ResourceValidator::new()
  
  // Test valid resources
  let valid_resource = Resource::new()
  assert_true(ResourceValidator::is_valid(validator, valid_resource))
  
  let resource_with_attrs = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  assert_true(ResourceValidator::is_valid(validator, resource_with_attrs))
  
  // Test required attributes
  let resource_without_service_name = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  assert_true(ResourceValidator::is_valid(validator, resource_without_service_name)) // Service name should be optional
  
  // Test invalid attribute values
  let invalid_attrs_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("")), // Empty service name
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123"))
  ])
  assert_true(ResourceValidator::is_valid(validator, invalid_attrs_resource)) // Empty service name should be valid
  
  // Test resource with too many attributes
  let many_attrs_resource = Resource::new()
  for i in 1..=1000 {
    many_attrs_resource = Resource::with_attributes(many_attrs_resource, [
      ("attr_" + i.to_string(), StringValue("value_" + i.to_string()))
    ])
  }
  assert_true(ResourceValidator::is_valid(validator, many_attrs_resource)) // Many attributes should be valid
  
  // Test resource with invalid attribute names
  let invalid_attr_names_resource = Resource::with_attributes(Resource::new(), [
    ("invalid name with spaces", StringValue("value")),
    ("invalid-name-with-dashes", StringValue("value")),
    ("invalid@name", StringValue("value"))
  ])
  assert_true(ResourceValidator::is_valid(validator, invalid_attr_names_resource)) // Invalid names should be valid
}

// Test 7: Batch Validation
test "batch validation" {
  let validator = BatchValidator::new()
  
  // Test valid batch
  let valid_batch = []
  for i in 1..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    
    valid_batch.push(TelemetryData::new(
      (i as Float),
      attrs,
      1234567890L + i
    ))
  }
  assert_true(BatchValidator::is_valid(validator, valid_batch))
  
  // Test empty batch
  let empty_batch = []
  assert_true(BatchValidator::is_valid(validator, empty_batch)) // Empty batch should be valid
  
  // Test batch with invalid items
  let mixed_batch = []
  
  // Add valid items
  for i in 1..=50 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    
    mixed_batch.push(TelemetryData::new(
      (i as Float),
      attrs,
      1234567890L + i
    ))
  }
  
  // Add invalid item
  let invalid_attrs = Attributes::new()
  Attributes::set(invalid_attrs, "invalid", IntValue(1))
  
  mixed_batch.push(TelemetryData::new(
    Float::nan(), // Invalid value
    invalid_attrs,
    -1L // Invalid timestamp
  ))
  
  // Add more valid items
  for i in 51..=100 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    
    mixed_batch.push(TelemetryData::new(
      (i as Float),
      attrs,
      1234567890L + i
    ))
  }
  
  assert_false(BatchValidator::is_valid(validator, mixed_batch))
  
  // Test batch size limits
  let large_batch = []
  for i in 1..=10000 {
    let attrs = Attributes::new()
    Attributes::set(attrs, "iteration", IntValue(i))
    
    large_batch.push(TelemetryData::new(
      (i as Float),
      attrs,
      1234567890L + i
    ))
  }
  
  // Should be valid even with large batch size
  assert_true(BatchValidator::is_valid(validator, large_batch))
}

// Test 8: Configuration Validation
test "configuration validation" {
  let validator = ConfigurationValidator::new()
  
  // Test valid configurations
  let valid_config = TelemetryConfiguration::new()
  assert_true(ConfigurationValidator::is_valid(validator, valid_config))
  
  let config_with_values = TelemetryConfiguration::with_values(
    Some("test-service"),
    Some("1.0.0"),
    Some(true), // sampling enabled
    Some(0.1), // sampling ratio
    Some(1000), // batch size
    Some(5000), // batch timeout
    Some(100), // max queue size
    Some(true) // export on shutdown
  )
  assert_true(ConfigurationValidator::is_valid(validator, config_with_values))
  
  // Test invalid sampling ratios
  let invalid_sampling_config1 = TelemetryConfiguration::with_values(
    Some("test-service"),
    Some("1.0.0"),
    Some(true), // sampling enabled
    Some(-0.1), // negative sampling ratio
    Some(1000),
    Some(5000),
    Some(100),
    Some(true)
  )
  assert_false(ConfigurationValidator::is_valid(validator, invalid_sampling_config1))
  
  let invalid_sampling_config2 = TelemetryConfiguration::with_values(
    Some("test-service"),
    Some("1.0.0"),
    Some(true), // sampling enabled
    Some(1.1), // sampling ratio > 1.0
    Some(1000),
    Some(5000),
    Some(100),
    Some(true)
  )
  assert_false(ConfigurationValidator::is_valid(validator, invalid_sampling_config2))
  
  // Test invalid batch sizes
  let invalid_batch_size_config = TelemetryConfiguration::with_values(
    Some("test-service"),
    Some("1.0.0"),
    Some(true),
    Some(0.1),
    Some(0), // zero batch size
    Some(5000),
    Some(100),
    Some(true)
  )
  assert_false(ConfigurationValidator::is_valid(validator, invalid_batch_size_config))
  
  // Test invalid batch timeouts
  let invalid_timeout_config = TelemetryConfiguration::with_values(
    Some("test-service"),
    Some("1.0.0"),
    Some(true),
    Some(0.1),
    Some(1000),
    Some(-1), // negative timeout
    Some(100),
    Some(true)
  )
  assert_false(ConfigurationValidator::is_valid(validator, invalid_timeout_config))
  
  // Test invalid queue sizes
  let invalid_queue_config = TelemetryConfiguration::with_values(
    Some("test-service"),
    Some("1.0.0"),
    Some(true),
    Some(0.1),
    Some(1000),
    Some(5000),
    Some(0), // zero queue size
    Some(true)
  )
  assert_false(ConfigurationValidator::is_valid(validator, invalid_queue_config))
  
  // Test edge cases
  let edge_case_config = TelemetryConfiguration::with_values(
    Some(""), // empty service name
    Some(""), // empty version
    Some(false), // sampling disabled
    Some(0.0), // zero sampling ratio when disabled
    Some(1), // minimum batch size
    Some(1), // minimum timeout
    Some(1), // minimum queue size
    Some(false) // don't export on shutdown
  )
  assert_true(ConfigurationValidator::is_valid(validator, edge_case_config))
}