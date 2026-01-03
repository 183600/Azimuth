// Azimuth Core Telemetry Functionality Tests
// This file contains comprehensive test cases for the core telemetry functionality

// Test 1: AttributeValue Type Operations
test "attribute value type operations and conversions" {
  let string_val = StringValue("test_value")
  let int_val = IntValue(42)
  let float_val = FloatValue(3.14)
  let bool_val = BoolValue(true)
  let array_str_val = ArrayStringValue(["a", "b", "c"])
  let array_int_val = ArrayIntValue([1, 2, 3])
  
  // Test type matching and extraction
  let extracted_string = match string_val {
    StringValue(s) => Some(s)
    _ => None
  }
  assert_eq(extracted_string, Some("test_value"))
  
  let extracted_int = match int_val {
    IntValue(i) => Some(i)
    _ => None
  }
  assert_eq(extracted_int, Some(42))
  
  let extracted_float = match float_val {
    FloatValue(f) => Some(f)
    _ => None
  }
  assert_eq(extracted_float, Some(3.14))
  
  let extracted_bool = match bool_val {
    BoolValue(b) => Some(b)
    _ => None
  }
  assert_eq(extracted_bool, Some(true))
  
  let extracted_array_str = match array_str_val {
    ArrayStringValue(arr) => Some(arr.length())
    _ => None
  }
  assert_eq(extracted_array_str, Some(3))
  
  let extracted_array_int = match array_int_val {
    ArrayIntValue(arr) => Some(arr.reduce(fn(acc, x) { acc + x }, 0))
    _ => None
  }
  assert_eq(extracted_array_int, Some(6))
}

// Test 2: Attributes Collection Operations
test "attributes collection operations" {
  let attrs = Attributes::new()
  
  // Test setting and getting attributes
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  Attributes::set(attrs, "int.key", IntValue(42))
  
  let retrieved_string = Attributes::get(attrs, "string.key")
  match retrieved_string {
    Some(StringValue(s)) => assert_eq(s, "test_value")
    _ => assert_true(false)
  }
  
  let retrieved_int = Attributes::get(attrs, "int.key")
  match retrieved_int {
    Some(IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  let non_existent = Attributes::get(attrs, "non.existent")
  assert_eq(non_existent, None)
}

// Test 3: Context Management
test "context management and value propagation" {
  let root_ctx = Context::root()
  let key = ContextKey::new("test_key")
  
  // Test context with value
  let ctx_with_value = Context::with_value(root_ctx, key, "test_value")
  let retrieved_value = Context::get(ctx_with_value, key)
  assert_eq(retrieved_value, Some("test_value"))
  
  // Test context without value
  let empty_ctx = Context::root()
  let empty_value = Context::get(empty_ctx, key)
  assert_eq(empty_value, None)
  
  // Test different keys
  let different_key = ContextKey::new("different_key")
  let different_value = Context::get(ctx_with_value, different_key)
  assert_eq(different_value, None)
}

// Test 4: SpanContext Operations
test "span context operations and validation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // Test basic properties
  assert_eq(SpanContext::trace_id(span_ctx), trace_id)
  assert_eq(SpanContext::span_id(span_ctx), span_id)
  assert_true(SpanContext::is_valid(span_ctx))
  assert_true(SpanContext::is_sampled(span_ctx))
  
  // Test invalid context
  let invalid_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_ctx))
  assert_false(SpanContext::is_sampled(invalid_ctx))
  
  // Test unsampled context
  let unsampled_ctx = SpanContext::new(trace_id, span_id, false, "")
  assert_true(SpanContext::is_valid(unsampled_ctx))
  assert_false(SpanContext::is_sampled(unsampled_ctx))
}

// Test 5: Span Lifecycle
test "span lifecycle and operations" {
  let span_ctx = SpanContext::new("test_trace_id", "test_span_id", true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test_span")
  match Span::kind(span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), "test_trace_id")
  
  // Test status operations
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // Test event operations
  Span::add_event(span, "test_event", Some([("event_key", StringValue("event_value"))]))
  
  // Test span ending
  Span::end(span)
}

// Test 6: Tracer Operations
test "tracer operations and span creation" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "test_tracer", Some("1.0.0"))
  
  // Test tracer properties
  let scope = Tracer::instrumentation_scope(tracer)
  assert_eq(scope.name, "test_tracer")
  assert_eq(scope.version, Some("1.0.0"))
  assert_eq(scope.schema_url, None)
  
  // Test span creation
  let span = Tracer::start_span(tracer, "test_operation", Some([("operation.type", StringValue("test"))]))
  assert_eq(Span::name(span), "test_operation")
  assert_true(Span::is_recording(span))
}

// Test 7: Meter and Counter Operations
test "meter and counter operations" {
  let provider = MeterProvider::noop()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  
  // Test counter properties
  assert_eq(counter.name, "test_counter")
  assert_eq(counter.description, Some("Test counter"))
  assert_eq(counter.unit, Some("count"))
  
  // Test counter operations
  Counter::add(counter, 1.0, None)
  Counter::add(counter, 2.5, Some(Attributes::new()))
  
  // Test instrument conversion
  let instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(instrument), "test_counter")
  assert_eq(Instrument::description(instrument), Some("Test counter"))
  assert_eq(Instrument::unit(instrument), Some("count"))
}

// Test 8: Histogram Operations
test "histogram operations and measurements" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test_meter")
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  
  // Test histogram properties
  assert_eq(histogram.name, "test_histogram")
  assert_eq(histogram.description, Some("Test histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  // Test histogram recording
  Histogram::record(histogram, 100.0, None)
  Histogram::record(histogram, 200.5, Some(Attributes::new()))
  
  // Test instrument conversion
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "test_histogram")
  assert_eq(Instrument::description(instrument), Some("Test histogram"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

// Test 9: Logger and LogRecord Operations
test "logger and log record operations" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "test_logger")
  
  // Test logger properties
  let scope = Logger::instrumentation_scope(logger)
  assert_eq(scope.name, "test_logger")
  
  // Test log record creation
  let record = LogRecord::new(Info, "Test log message")
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Test log message"))
  
  // Test log record with context
  let record_with_context = LogRecord::new_with_context(
    Error,
    Some("Error message"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace_id"),
    Some("span_id"),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(record_with_context), Error)
  assert_eq(LogRecord::body(record_with_context), Some("Error message"))
  assert_eq(LogRecord::trace_id(record_with_context), Some("trace_id"))
  assert_eq(LogRecord::span_id(record_with_context), Some("span_id"))
  
  // Test log emission
  Logger::emit(logger, record)
  Logger::emit(logger, record_with_context)
}

// Test 10: Clock and Random Operations
test "clock and random operations" {
  let clock = Clock::system()
  let timestamp = Clock::now_unix_nanos(clock)
  
  // Test timestamp is reasonable (should be around 2025)
  assert_true(timestamp > 1700000000000000000L)
  assert_true(timestamp < 1800000000000000000L)
  
  let random = Random::system()
  
  // Test random bytes
  let bytes = Random::next_bytes(random, 8)
  assert_eq(bytes.length(), 8)
  
  // Test random u64
  let random_value = Random::next_u64(random)
  assert_true(random_value > 0UL)
}