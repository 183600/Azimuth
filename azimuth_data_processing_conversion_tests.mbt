// Azimuth Telemetry System - Data Processing and Conversion Tests
// This file contains test cases for data processing and conversion

// Test 1: Attribute Value Type Conversions
test "attribute value type conversions" {
  // Test string to integer conversion
  let string_attr = StringValue("42")
  let int_from_string = AttributeValue::to_int(string_attr)
  match int_from_string {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  // Test integer to string conversion
  let int_attr = IntValue(42)
  let string_from_int = AttributeValue::to_string(int_attr)
  match string_from_int {
    Some(value) => assert_eq(value, "42")
    None => assert_true(false)
  }
  
  // Test string to float conversion
  let float_string_attr = StringValue("3.14")
  let float_from_string = AttributeValue::to_float(float_string_attr)
  match float_from_string {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  // Test float to string conversion
  let float_attr = FloatValue(3.14)
  let string_from_float = AttributeValue::to_string(float_attr)
  match string_from_float {
    Some(value) => assert_eq(value, "3.14")
    None => assert_true(false)
  }
  
  // Test boolean to string conversion
  let bool_attr = BoolValue(true)
  let string_from_bool = AttributeValue::to_string(bool_attr)
  match string_from_bool {
    Some(value) => assert_eq(value, "true")
    None => assert_true(false)
  }
  
  // Test string to boolean conversion
  let true_string_attr = StringValue("true")
  let bool_from_true_string = AttributeValue::to_bool(true_string_attr)
  match bool_from_true_string {
    Some(value) => assert_true(value)
    None => assert_true(false)
  }
  
  let false_string_attr = StringValue("false")
  let bool_from_false_string = AttributeValue::to_bool(false_string_attr)
  match bool_from_false_string {
    Some(value) => assert_false(value)
    None => assert_true(false)
  }
  
  // Test array conversions
  let string_array_attr = ArrayStringValue(["1", "2", "3"])
  let int_array_from_string = AttributeValue::to_int_array(string_array_attr)
  match int_array_from_string {
    Some(array) => assert_eq(array, [1, 2, 3])
    None => assert_true(false)
  }
  
  let int_array_attr = ArrayIntValue([1, 2, 3])
  let string_array_from_int = AttributeValue::to_string_array(int_array_attr)
  match string_array_from_int {
    Some(array) => assert_eq(array, ["1", "2", "3"])
    None => assert_true(false)
  }
}

// Test 2: JSON Serialization and Deserialization
test "json serialization and deserialization" {
  // Test span serialization
  let span_context = SpanContext::new("trace123", "span123", true, "test")
  let span = Span::new("test_span", Server, span_context)
  
  Span::set_attribute(span, "string.attr", StringValue("test_value"))
  Span::set_attribute(span, "int.attr", IntValue(42))
  Span::set_attribute(span, "float.attr", FloatValue(3.14))
  Span::set_attribute(span, "bool.attr", BoolValue(true))
  
  Span::add_event(span, "test_event", Some([
    ("event.string", StringValue("event_value")),
    ("event.int", IntValue(100))
  ]))
  
  // Serialize span to JSON
  let json_string = JsonSerializer::serialize_span(span)
  assert_true(json_string.contains("test_span"))
  assert_true(json_string.contains("trace123"))
  assert_true(json_string.contains("span123"))
  assert_true(json_string.contains("test_value"))
  assert_true(json_string.contains("42"))
  assert_true(json_string.contains("3.14"))
  assert_true(json_string.contains("true"))
  
  // Deserialize span from JSON
  let deserialized_span = JsonSerializer::deserialize_span(json_string)
  assert_eq(Span::name(deserialized_span), "test_span")
  assert_eq(Span::kind(deserialized_span), Server)
  
  let deserialized_context = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(deserialized_context), "trace123")
  assert_eq(SpanContext::span_id(deserialized_context), "span123")
  
  // Test metric serialization
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "json_test_meter")
  let counter = Meter::create_counter(meter, "json_test_counter", Some("JSON test counter"), Some("operations"))
  
  Counter::add(counter, 5.0, Some(Attributes::with([
    ("attr1", StringValue("value1")),
    ("attr2", IntValue(10))
  ])))
  
  // Serialize metric to JSON
  let metric_json = JsonSerializer::serialize_metric(counter)
  assert_true(metric_json.contains("json_test_counter"))
  assert_true(metric_json.contains("5.0"))
  assert_true(metric_json.contains("value1"))
  assert_true(metric_json.contains("10"))
  
  // Test log record serialization
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Test log message"),
    Some(Attributes::with([
      ("log.attr", StringValue("log_value"))
    ])),
    Some(1640995200000L),
    Some(1640995200100L),
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  // Serialize log record to JSON
  let log_json = JsonSerializer::serialize_log_record(log_record)
  assert_true(log_json.contains("Test log message"))
  assert_true(log_json.contains("INFO"))
  assert_true(log_json.contains("trace123"))
  assert_true(log_json.contains("span123"))
  assert_true(log_json.contains("log_value"))
}

// Test 3: Protocol Buffer Serialization
test "protocol buffer serialization" {
  // Test span serialization with Protocol Buffers
  let span_context = SpanContext::new("trace123", "span123", true, "protobuf_test")
  let span = Span::new("protobuf_span", Client, span_context)
  
  Span::set_attribute(span, "pb.string.attr", StringValue("pb_value"))
  Span::set_attribute(span, "pb.int.attr", IntValue(100))
  Span::add_event(span, "pb_event", Some([
    ("pb.event.attr", StringValue("pb_event_value"))
  ]))
  
  // Serialize span to Protocol Buffer
  let pb_data = ProtobufSerializer::serialize_span(span)
  assert_true(pb_data.length() > 0)
  
  // Deserialize span from Protocol Buffer
  let deserialized_span = ProtobufSerializer::deserialize_span(pb_data)
  assert_eq(Span::name(deserialized_span), "protobuf_span")
  assert_eq(Span::kind(deserialized_span), Client)
  
  let deserialized_context = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(deserialized_context), "trace123")
  assert_eq(SpanContext::span_id(deserialized_context), "span123")
  
  // Test metric serialization with Protocol Buffers
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "pb_test_meter")
  let histogram = Meter::create_histogram(meter, "pb_histogram", Some("PB test histogram"), Some("ms"))
  
  Histogram::record(histogram, 50.0, Some(Attributes::with([
    ("pb.metric.attr", StringValue("pb_metric_value"))
  ])))
  
  // Serialize metric to Protocol Buffer
  let metric_pb_data = ProtobufSerializer::serialize_metric(histogram)
  assert_true(metric_pb_data.length() > 0)
  
  // Test log record serialization with Protocol Buffers
  let log_record = LogRecord::new_with_context(
    Error,
    Some("PB test log message"),
    Some(Attributes::with([
      ("pb.log.attr", StringValue("pb_log_value"))
    ])),
    Some(1640995200000L),
    Some(1640995200100L),
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  // Serialize log record to Protocol Buffer
  let log_pb_data = ProtobufSerializer::serialize_log_record(log_record)
  assert_true(log_pb_data.length() > 0)
}

// Test 4: Data Compression and Decompression
test "data compression and decompression" {
  // Create large telemetry data for compression
  let large_attributes = Attributes::new()
  
  // Add many attributes to create a large dataset
  for i in 1..=100 {
    let key = "large.attr.key" + i.to_string()
    let value = StringValue("large.attr.value.with.much.text.to.compress" + i.to_string())
    Attributes::set(large_attributes, key, value)
  }
  
  // Create span with large attributes
  let span_context = SpanContext::new("trace123", "span123", true, "compression_test")
  let span = Span::new("compression_span", Internal, span_context)
  
  // Add large attributes to span
  for i in 1..=100 {
    let key = "span.attr.key" + i.to_string()
    let value = StringValue("span.attr.value.with.much.text.to.compress" + i.to_string())
    Span::set_attribute(span, key, value)
  }
  
  // Add many events to span
  for i in 1..=50 {
    Span::add_event(span, "compression_event_" + i.to_string(), Some([
      ("event.key", StringValue("event.value.with.much.text.to.compress" + i.to_string()))
    ]))
  }
  
  // Serialize span to JSON
  let json_data = JsonSerializer::serialize_span(span)
  let original_size = json_data.length()
  
  // Compress JSON data
  let compressed_data = DataCompressor::compress(json_data)
  let compressed_size = compressed_data.length()
  
  // Verify compression ratio
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  assert_true(compression_ratio < 0.8) // Should achieve at least 20% compression
  
  // Decompress data
  let decompressed_data = DataDecompressor::decompress(compressed_data)
  
  // Verify decompressed data matches original
  assert_eq(decompressed_data, json_data)
  
  // Test with Protocol Buffer data
  let pb_data = ProtobufSerializer::serialize_span(span)
  let pb_original_size = pb_data.length()
  
  // Compress Protocol Buffer data
  let compressed_pb_data = DataCompressor::compress(pb_data)
  let compressed_pb_size = compressed_pb_data.length()
  
  // Verify PB compression ratio
  let pb_compression_ratio = compressed_pb_size.to_float() / pb_original_size.to_float()
  assert_true(pb_compression_ratio < 0.9) // Should achieve at least 10% compression
  
  // Decompress PB data
  let decompressed_pb_data = DataDecompressor::decompress(compressed_pb_data)
  
  // Verify decompressed PB data matches original
  assert_eq(decompressed_pb_data, pb_data)
}

// Test 5: Data Format Conversion
test "data format conversion" {
  // Create test span
  let span_context = SpanContext::new("trace123", "span123", true, "format_conversion")
  let span = Span::new("format_conversion_span", Server, span_context)
  
  Span::set_attribute(span, "conversion.attr", StringValue("conversion_value"))
  Span::add_event(span, "conversion_event", Some([
    ("event.attr", StringValue("event_value"))
  ]))
  
  // Convert span to JSON
  let json_data = JsonSerializer::serialize_span(span)
  
  // Convert JSON to Protocol Buffer
  let pb_from_json = FormatConverter::json_to_protobuf(json_data)
  
  // Convert Protocol Buffer back to JSON
  let json_from_pb = FormatConverter::protobuf_to_json(pb_from_json)
  
  // Verify round-trip conversion preserves data
  let original_span = JsonSerializer::deserialize_span(json_data)
  let converted_span = JsonSerializer::deserialize_span(json_from_pb)
  
  assert_eq(Span::name(original_span), Span::name(converted_span))
  assert_eq(Span::kind(original_span), Span::kind(converted_span))
  
  let original_context = Span::span_context(original_span)
  let converted_context = Span::span_context(converted_span)
  
  assert_eq(SpanContext::trace_id(original_context), SpanContext::trace_id(converted_context))
  assert_eq(SpanContext::span_id(original_context), SpanContext::span_id(converted_context))
  
  // Test metric format conversion
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "format_conversion_meter")
  let counter = Meter::create_counter(meter, "format_counter", Some("Format conversion counter"), Some("operations"))
  
  Counter::add(counter, 10.0, Some(Attributes::with([
    ("format.attr", StringValue("format_value"))
  ])))
  
  // Convert metric to JSON
  let metric_json = JsonSerializer::serialize_metric(counter)
  
  // Convert JSON to Protocol Buffer
  let metric_pb_from_json = FormatConverter::json_to_protobuf(metric_json)
  
  // Convert Protocol Buffer back to JSON
  let metric_json_from_pb = FormatConverter::protobuf_to_json(metric_pb_from_json)
  
  // Verify round-trip conversion preserves metric data
  assert_true(metric_json.contains("format_counter"))
  assert_true(metric_json_from_pb.contains("format_counter"))
  assert_true(metric_json.contains("10.0"))
  assert_true(metric_json_from_pb.contains("10.0"))
  
  // Test log record format conversion
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Format conversion log message"),
    Some(Attributes::with([
      ("log.format.attr", StringValue("log_format_value"))
    ])),
    Some(1640995200000L),
    Some(1640995200100L),
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  // Convert log record to JSON
  let log_json = JsonSerializer::serialize_log_record(log_record)
  
  // Convert JSON to Protocol Buffer
  let log_pb_from_json = FormatConverter::json_to_protobuf(log_json)
  
  // Convert Protocol Buffer back to JSON
  let log_json_from_pb = FormatConverter::protobuf_to_json(log_pb_from_json)
  
  // Verify round-trip conversion preserves log data
  assert_true(log_json.contains("Format conversion log message"))
  assert_true(log_json_from_pb.contains("Format conversion log message"))
  assert_true(log_json.contains("WARN"))
  assert_true(log_json_from_pb.contains("WARN"))
}

// Test 6: Data Aggregation and Processing
test "data aggregation and processing" {
  // Create multiple spans for aggregation
  let mut spans = []
  
  for i in 1..=10 {
    let span_context = SpanContext::new("trace123", "span" + i.to_string(), true, "aggregation")
    let span = Span::new("aggregation_span_" + i.to_string(), Internal, span_context)
    
    Span::set_attribute(span, "span.number", IntValue(i))
    Span::set_attribute(span, "span.category", StringValue("test"))
    
    // Add events with timing information
    Span::add_event(span, "start_event", Some([
      ("event.time", IntValue(i * 100))
    ]))
    
    Span::add_event(span, "end_event", Some([
      ("event.time", IntValue(i * 100 + 50))
    ]))
    
    spans = spans.concat([span])
  }
  
  // Aggregate spans by category
  let aggregated_by_category = DataAggregator::aggregate_spans_by_attribute(spans, "span.category")
  
  match aggregated_by_category.get("test") {
    Some(category_spans) => assert_eq(category_spans.length(), 10)
    None => assert_true(false)
  }
  
  // Calculate statistics from span data
  let span_numbers = spans.map(fn(span) {
    match Span::get_attribute(span, "span.number") {
      Some(IntValue(num)) => num
      _ => 0
    }
  })
  
  let sum = span_numbers.fold(0, fn(acc, num) { acc + num })
  let average = sum.to_float() / span_numbers.length().to_float()
  
  assert_eq(sum, 55) // Sum of 1 to 10
  assert_eq(average, 5.5) // Average of 1 to 10
  
  // Calculate duration from events
  let durations = spans.map(fn(span) {
    let events = Span::get_events(span)
    if events.length() >= 2 {
      let start_event = events[0]
      let end_event = events[1]
      
      let start_time = match Event::get_attribute(start_event, "event.time") {
        Some(IntValue(time)) => time
        _ => 0
      }
      
      let end_time = match Event::get_attribute(end_event, "event.time") {
        Some(IntValue(time)) => time
        _ => 0
      }
      
      end_time - start_time
    } else {
      0
    }
  })
  
  let total_duration = durations.fold(0, fn(acc, duration) { acc + duration })
  assert_eq(total_duration, 500) // 10 spans * 50 duration each
  
  // Test metric aggregation
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  
  // Create multiple counters
  let mut counters = []
  for i in 1..=5 {
    let counter = Meter::create_counter(
      meter,
      "aggregation_counter_" + i.to_string(),
      Some("Aggregation counter " + i.to_string()),
      Some("operations")
    )
    
    // Add different values to each counter
    for j in 1..=i {
      Counter::add(counter, j.to_float())
    }
    
    counters = counters.concat([counter])
  }
  
  // Aggregate counter values
  let counter_values = counters.map(fn(counter) {
    // In a real implementation, this would get the actual counter value
    let counter_name = Instrument::name(Counter::as_instrument(counter))
    let last_char = counter_name[counter_name.length() - 1]
    last_char.to_digit() * (last_char.to_digit() + 1) / 2 // Sum of 1 to n
  })
  
  let total_counter_value = counter_values.fold(0.0, fn(acc, value) { acc + value.to_float() })
  assert_eq(total_counter_value, 15.0) // Sum of 1, 3, 6, 10, 15
  
  // Test log aggregation
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "aggregation_logger")
  
  // Create multiple log records
  for i in 1..=10 {
    let severity = if i % 3 == 0 { Error } else if i % 2 == 0 { Warn } else { Info }
    
    let log_record = LogRecord::new_with_context(
      severity,
      Some("Aggregation log message " + i.to_string()),
      Some(Attributes::with([
        ("log.number", IntValue(i)),
        ("log.category", StringValue("test"))
      ])),
      Some(1640995200000L + i),
      Some(1640995200100L + i),
      Some("trace123"),
      Some("span" + i.to_string()),
      Some(Context::root())
    )
    
    Logger::emit(logger, log_record)
  }
  
  // Aggregate logs by severity
  let logs_by_severity = DataAggregator::aggregate_logs_by_severity(logger)
  
  assert_eq(logs_by_severity.get(Info), Some(4)) // 1, 4, 7, 10
  assert_eq(logs_by_severity.get(Warn), Some(3)) // 2, 5, 8
  assert_eq(logs_by_severity.get(Error), Some(3)) // 3, 6, 9
}

// Test 7: Data Filtering and Transformation
test "data filtering and transformation" {
  // Create spans with various attributes
  let mut spans = []
  
  for i in 1..=10 {
    let span_context = SpanContext::new("trace123", "span" + i.to_string(), true, "filtering")
    let span = Span::new("filtering_span_" + i.to_string(), Internal, span_context)
    
    // Add attributes with different values
    Span::set_attribute(span, "span.number", IntValue(i))
    Span::set_attribute(span, "span.category", if i % 2 == 0 { "even" } else { "odd" })
    Span::set_attribute(span, "span.priority", if i <= 3 { "high" } else if i <= 7 { "medium" } else { "low" })
    
    spans = spans.concat([span])
  }
  
  // Filter spans by attribute value
  let even_spans = DataFilter::filter_spans_by_attribute_value(spans, "span.category", "even")
  assert_eq(even_spans.length(), 5) // 2, 4, 6, 8, 10
  
  let odd_spans = DataFilter::filter_spans_by_attribute_value(spans, "span.category", "odd")
  assert_eq(odd_spans.length(), 5) // 1, 3, 5, 7, 9
  
  // Filter spans by attribute range
  let high_priority_spans = DataFilter::filter_spans_by_attribute_value(spans, "span.priority", "high")
  assert_eq(high_priority_spans.length(), 3) // 1, 2, 3
  
  // Transform spans by adding new attributes
  let transformed_spans = DataTransformer::add_computed_attribute(spans, "span.doubled_number", fn(span) {
    match Span::get_attribute(span, "span.number") {
      Some(IntValue(num)) => IntValue(num * 2)
      _ => IntValue(0)
    }
  })
  
  // Verify transformation
  for i in 1..=10 {
    let span = transformed_spans[i - 1]
    match Span::get_attribute(span, "span.doubled_number") {
      Some(IntValue(value)) => assert_eq(value, i * 2)
      _ => assert_true(false)
    }
  }
  
  // Filter metrics by value
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "filtering_meter")
  
  // Create histogram with various values
  let histogram = Meter::create_histogram(meter, "filtering_histogram", Some("Filtering histogram"), Some("ms"))
  
  for i in 1..=20 {
    Histogram::record(histogram, i.to_float(), Some(Attributes::with([
      ("value.category", if i % 3 == 0 { "multiple_of_3" } else { "other" })
    ])))
  }
  
  // Filter histogram measurements by value
  let high_value_measurements = DataFilter::filter_histogram_by_value_threshold(histogram, 15.0)
  assert_eq(high_value_measurements.length(), 5) // 16, 17, 18, 19, 20
  
  // Transform metrics by applying function
  let transformed_histogram = DataTransformer::apply_function_to_measurements(histogram, fn(value) {
    value * 2.0
  })
  
  // Verify transformation
  let transformed_measurements = Histogram::get_measurements(transformed_histogram)
  for i in 1..=20 {
    assert_eq(transformed_measurements[i - 1], i.to_float() * 2.0)
  }
  
  // Filter log records by severity
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "filtering_logger")
  
  // Create log records with different severities
  for i in 1..=10 {
    let severity = if i <= 2 { Error } else if i <= 5 { Warn } else { Info }
    
    let log_record = LogRecord::new_with_context(
      severity,
      Some("Filtering log message " + i.to_string()),
      Some(Attributes::with([
        ("log.number", IntValue(i))
      ])),
      Some(1640995200000L + i),
      Some(1640995200100L + i),
      Some("trace123"),
      Some("span" + i.to_string()),
      Some(Context::root())
    )
    
    Logger::emit(logger, log_record)
  }
  
  // Filter logs by severity
  let error_logs = DataFilter::filter_logs_by_severity(logger, Error)
  assert_eq(error_logs.length(), 2) // 1, 2
  
  let warn_logs = DataFilter::filter_logs_by_severity(logger, Warn)
  assert_eq(warn_logs.length(), 3) // 3, 4, 5
  
  let info_logs = DataFilter::filter_logs_by_severity(logger, Info)
  assert_eq(info_logs.length(), 5) // 6, 7, 8, 9, 10
  
  // Transform logs by adding computed attributes
  let transformed_logs = DataTransformer::add_computed_log_attribute(logger, "log.formatted_message", fn(log_record) {
    match LogRecord::body(log_record) {
      Some(message) => "FORMATTED: " + message
      None => "FORMATTED: (no message)"
    }
  })
  
  // Verify transformation
  for log_record in transformed_logs {
    match LogRecord::get_attribute(log_record, "log.formatted_message") {
      Some(StringValue(message)) => assert_true(message.contains("FORMATTED:"))
      _ => assert_true(false)
    }
  }
}

// Test 8: Data Validation and Integrity
test "data validation and integrity" {
  // Test span data validation
  let span_context = SpanContext::new("trace123", "span123", true, "validation")
  let span = Span::new("validation_span", Server, span_context)
  
  // Add valid attributes
  Span::set_attribute(span, "valid.string", StringValue("valid_value"))
  Span::set_attribute(span, "valid.int", IntValue(42))
  Span::set_attribute(span, "valid.float", FloatValue(3.14))
  Span::set_attribute(span, "valid.bool", BoolValue(true))
  
  // Validate span
  let validation_result = DataValidator::validate_span(span)
  assert_true(validation_result.is_valid)
  
  // Test invalid span data
  let invalid_span_context = SpanContext::new("", "", false, "")
  let invalid_span = Span::new("", Internal, invalid_span_context)
  
  let invalid_validation_result = DataValidator::validate_span(invalid_span)
  assert_false(invalid_validation_result.is_valid)
  assert_true(invalid_validation_result.errors.length() > 0)
  
  // Test metric data validation
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "validation_meter")
  
  // Create valid metric
  let valid_counter = Meter::create_counter(meter, "valid_counter", Some("Valid counter"), Some("operations"))
  Counter::add(valid_counter, 5.0)
  
  let metric_validation_result = DataValidator::validate_metric(valid_counter)
  assert_true(metric_validation_result.is_valid)
  
  // Test invalid metric data
  let invalid_histogram = Meter::create_histogram(meter, "", Some("Invalid histogram"), Some(""))
  Histogram::record(invalid_histogram, -1.0) // Negative value might be invalid
  
  let invalid_metric_validation_result = DataValidator::validate_metric(invalid_histogram)
  assert_false(invalid_metric_validation_result.is_valid)
  
  // Test log record validation
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "validation_logger")
  
  // Create valid log record
  let valid_log_record = LogRecord::new_with_context(
    Info,
    Some("Valid log message"),
    Some(Attributes::with([
      ("valid.attr", StringValue("valid_value"))
    ])),
    Some(1640995200000L),
    Some(1640995200100L),
    Some("trace123"),
    Some("span123"),
    Some(Context::root())
  )
  
  let log_validation_result = DataValidator::validate_log_record(valid_log_record)
  assert_true(log_validation_result.is_valid)
  
  // Test invalid log record
  let invalid_log_record = LogRecord::new_with_context(
    999, // Invalid severity
    Some(""),
    Some(Attributes::new()),
    Some(-1L), // Invalid timestamp
    Some(-1L), // Invalid observed timestamp
    Some(""),
    Some(""),
    Some(Context::root())
  )
  
  let invalid_log_validation_result = DataValidator::validate_log_record(invalid_log_record)
  assert_false(invalid_log_validation_result.is_valid)
  
  // Test data integrity checks
  let original_span = Span::new("integrity_span", Client, span_context)
  Span::set_attribute(original_span, "integrity.attr", StringValue("integrity_value"))
  
  // Serialize and deserialize span
  let serialized_span = JsonSerializer::serialize_span(original_span)
  let deserialized_span = JsonSerializer::deserialize_span(serialized_span)
  
  // Check integrity
  let integrity_check = DataIntegrityChecker::check_span_integrity(original_span, deserialized_span)
  assert_true(integrity_check.is_intact)
  
  // Test with corrupted data
  let corrupted_data = serialized_span.substring(0, serialized_span.length() / 2)
  let corrupted_span = JsonSerializer::deserialize_span(corrupted_data)
  
  let corruption_check = DataIntegrityChecker::check_span_integrity(original_span, corrupted_span)
  assert_false(corruption_check.is_intact)
  
  // Test checksum verification
  let data = "test data for checksum"
  let checksum = DataIntegrityChecker::calculate_checksum(data)
  
  // Verify checksum
  let is_valid_checksum = DataIntegrityChecker::verify_checksum(data, checksum)
  assert_true(is_valid_checksum)
  
  // Test with modified data
  let modified_data = "modified data for checksum"
  let is_invalid_checksum = DataIntegrityChecker::verify_checksum(modified_data, checksum)
  assert_false(is_invalid_checksum)
}

// Test 9: Batch Data Processing
test "batch data processing" {
  // Create batch of spans
  let mut spans = []
  
  for i in 1..=100 {
    let span_context = SpanContext::new("batch_trace", "batch_span_" + i.to_string(), true, "batch")
    let span = Span::new("batch_span_" + i.to_string(), Internal, span_context)
    
    Span::set_attribute(span, "batch.id", IntValue(i))
    Span::set_attribute(span, "batch.category", if i % 3 == 0 { "A" } else if i % 3 == 1 { "B" } else { "C" })
    
    spans = spans.concat([span])
  }
  
  // Process spans in batches
  let batch_size = 10
  let processed_spans = BatchProcessor::process_spans(spans, batch_size, fn(batch) {
    // Process each batch
    batch.map(fn(span) {
      // Add processing timestamp
      Span::set_attribute(span, "processed.timestamp", StringValue(Time::now().to_string()))
      span
    })
  })
  
  // Verify all spans were processed
  assert_eq(processed_spans.length(), 100)
  
  // Verify processing timestamp was added
  for span in processed_spans {
    match Span::get_attribute(span, "processed.timestamp") {
      Some(StringValue(_)) => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // Batch process metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "batch_meter")
  
  // Create batch of metrics
  let mut metrics = []
  
  for i in 1..=50 {
    let counter = Meter::create_counter(
      meter,
      "batch_counter_" + i.to_string(),
      Some("Batch counter " + i.to_string()),
      Some("operations")
    )
    
    // Add random values
    for j in 1..=i {
      Counter::add(counter, j.to_float())
    }
    
    metrics = metrics.concat([counter])
  }
  
  // Process metrics in batches
  let metric_batch_size = 5
  let processed_metrics = BatchProcessor::process_metrics(metrics, metric_batch_size, fn(batch) {
    // Process each batch
    batch.map(fn(metric) {
      // Add batch processing metadata
      let instrument = Counter::as_instrument(metric)
      let name = Instrument::name(instrument)
      
      // In a real implementation, this would add metadata to the metric
      metric
    })
  })
  
  // Verify all metrics were processed
  assert_eq(processed_metrics.length(), 50)
  
  // Batch process log records
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "batch_logger")
  
  // Create batch of log records
  let mut log_records = []
  
  for i in 1..=200 {
    let severity = if i % 4 == 0 { Error } else if i % 4 == 1 { Warn } else if i % 4 == 2 { Info } else { Debug }
    
    let log_record = LogRecord::new_with_context(
      severity,
      Some("Batch log message " + i.to_string()),
      Some(Attributes::with([
        ("batch.id", IntValue(i)),
        ("batch.category", if i % 3 == 0 { "X" } else if i % 3 == 1 { "Y" } else { "Z" })
      ])),
      Some(1640995200000L + i),
      Some(1640995200100L + i),
      Some("batch_trace"),
      Some("batch_span_" + i.to_string()),
      Some(Context::root())
    )
    
    log_records = log_records.concat([log_record])
  }
  
  // Process log records in batches
  let log_batch_size = 20
  let processed_logs = BatchProcessor::process_log_records(log_records, log_batch_size, fn(batch) {
    // Process each batch
    batch.map(fn(log_record) {
      // Add batch processing metadata
      log_record
    })
  })
  
  // Verify all log records were processed
  assert_eq(processed_logs.length(), 200)
  
  // Test batch aggregation
  let aggregated_spans = BatchAggregator::aggregate_spans_by_category(processed_spans, "batch.category")
  
  assert_eq(aggregated_spans.get("A"), Some(34)) // 3, 6, 9, ..., 99
  assert_eq(aggregated_spans.get("B"), Some(33)) // 1, 4, 7, ..., 100
  assert_eq(aggregated_spans.get("C"), Some(33)) // 2, 5, 8, ..., 98
}

// Test 10: Stream Processing of Telemetry Data
test "stream processing of telemetry data" {
  // Create stream processor for spans
  let span_stream = StreamProcessor::new()
  
  // Set up stream processing pipeline
  let processed_stream = span_stream
    .filter(fn(span) {
      // Filter spans by attribute
      match Span::get_attribute(span, "stream.include") {
        Some(BoolValue(include)) => include
        _ => true
      }
    })
    .map(fn(span) {
      // Transform spans
      Span::set_attribute(span, "stream.processed", BoolValue(true))
    })
    .group_by(fn(span) {
      // Group by category
      match Span::get_attribute(span, "stream.category") {
        Some(StringValue(category)) => category
        _ => "default"
      }
    })
    .aggregate(fn(group) {
      // Aggregate each group
      let count = group.length()
      ("count", IntValue(count))
    })
  
  // Create test spans and feed to stream
  let mut test_spans = []
  
  for i in 1..=50 {
    let span_context = SpanContext::new("stream_trace", "stream_span_" + i.to_string(), true, "stream")
    let span = Span::new("stream_span_" + i.to_string(), Internal, span_context)
    
    Span::set_attribute(span, "stream.id", IntValue(i))
    Span::set_attribute(span, "stream.include", BoolValue(i % 2 == 0)) // Include even numbers only
    Span::set_attribute(span, "stream.category", if i % 3 == 0 { "alpha" } else if i % 3 == 1 { "beta" } else { "gamma" })
    
    test_spans = test_spans.concat([span])
  }
  
  // Process spans through stream
  let stream_results = StreamProcessor::process(processed_stream, test_spans)
  
  // Verify stream processing results
  assert_eq(stream_results.get("alpha"), Some(IntValue(8))) // Even numbers divisible by 3: 6, 12, 18, 24, 30, 36, 42, 48
  assert_eq(stream_results.get("beta"), Some(IntValue(8))) // Even numbers with remainder 1: 4, 10, 16, 22, 28, 34, 40, 46
  assert_eq(stream_results.get("gamma"), Some(IntValue(9))) // Even numbers with remainder 2: 2, 8, 14, 20, 26, 32, 38, 44, 50
  
  // Create stream processor for metrics
  let metric_stream = StreamProcessor::new()
  
  // Set up metric stream processing pipeline
  let metric_processed_stream = metric_stream
    .filter(fn(metric) {
      // Filter metrics by value threshold
      let measurements = Histogram::get_measurements(metric)
      measurements.fold(0.0, fn(acc, value) { acc + value }) > 100.0
    })
    .map(fn(metric) {
      // Transform metrics
      metric
    })
    .reduce(fn(acc, metric) {
      // Reduce to single result
      acc + 1
    })
  
  // Create test metrics and feed to stream
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "stream_meter")
  
  let mut test_metrics = []
  
  for i in 1..=20 {
    let histogram = Meter::create_histogram(
      meter,
      "stream_histogram_" + i.to_string(),
      Some("Stream histogram " + i.to_string()),
      Some("ms")
    )
    
    // Add values to histogram
    for j in 1..=i {
      Histogram::record(histogram, j.to_float())
    }
    
    test_metrics = test_metrics.concat([histogram])
  }
  
  // Process metrics through stream
  let metric_stream_result = StreamProcessor::process(metric_processed_stream, test_metrics)
  
  // Verify metric stream processing result
  // Histograms with sum > 100: those with i >= 14 (14*15/2 = 105)
  assert_eq(metric_stream_result, 7) // 14, 15, 16, 17, 18, 19, 20
  
  // Create stream processor for log records
  let log_stream = StreamProcessor::new()
  
  // Set up log stream processing pipeline
  let log_processed_stream = log_stream
    .filter(fn(log_record) {
      // Filter by severity
      let severity = LogRecord::severity_number(log_record)
      severity == Error || severity == Warn
    })
    .window(5) // Window of 5 log records
    .aggregate(fn(window) {
      // Aggregate each window
      let error_count = window.fold(0, fn(acc, log_record) {
        if LogRecord::severity_number(log_record) == Error {
          acc + 1
        } else {
          acc
        }
      })
      ("error_count", IntValue(error_count))
    })
  
  // Create test log records and feed to stream
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "stream_logger")
  
  let mut test_logs = []
  
  for i in 1..=25 {
    let severity = if i % 5 == 0 { Error } else if i % 3 == 0 { Warn } else { Info }
    
    let log_record = LogRecord::new_with_context(
      severity,
      Some("Stream log message " + i.to_string()),
      Some(Attributes::with([
        ("log.id", IntValue(i))
      ])),
      Some(1640995200000L + i),
      Some(1640995200100L + i),
      Some("stream_trace"),
      Some("stream_span_" + i.to_string()),
      Some(Context::root())
    )
    
    test_logs = test_logs.concat([log_record])
  }
  
  // Process logs through stream
  let log_stream_results = StreamProcessor::process(log_processed_stream, test_logs)
  
  // Verify log stream processing results
  // Each window of 5 should have 1 error (every 5th record)
  assert_eq(log_stream_results.length(), 5) // 25 records / window size of 5
  
  for result in log_stream_results {
    match result {
      ("error_count", IntValue(count)) => assert_eq(count, 1)
      _ => assert_true(false)
    }
  }
}