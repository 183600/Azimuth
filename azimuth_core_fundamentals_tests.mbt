// Azimuth Core Fundamentals Test Suite
// This file contains fundamental test cases for core Azimuth functionality

// Test 1: Basic Telemetry Context Operations
test "telemetry context basic operations" {
  // Test context creation and validation
  let trace_id = "12345678901234567890123456789012"
  let span_id = "1234567890123456"
  let context = TelemetryContext::new(trace_id, span_id)
  
  // Verify context properties
  assert_eq(TelemetryContext::trace_id(context), trace_id)
  assert_eq(TelemetryContext::span_id(context), span_id)
  assert_true(TelemetryContext::is_valid(context))
  
  // Test context with invalid trace ID
  let invalid_trace_id = "invalid"
  let invalid_context = TelemetryContext::new(invalid_trace_id, span_id)
  assert_false(TelemetryContext::is_valid(invalid_context))
  
  // Test context with empty span ID
  let empty_span_id = ""
  let empty_context = TelemetryContext::new(trace_id, empty_span_id)
  assert_false(TelemetryContext::is_valid(empty_context))
}

// Test 2: Attribute Operations and Type Safety
test "attribute operations and type safety" {
  // Test attribute creation with different types
  let string_attr = Attribute::create("string.key", "string.value")
  let int_attr = Attribute::create("int.key", 42)
  let float_attr = Attribute::create("float.key", 3.14159)
  let bool_attr = Attribute::create("bool.key", true)
  
  // Verify attribute types and values
  assert_eq(Attribute::key(string_attr), "string.key")
  assert_eq(Attribute::value_as_string(string_attr), "string.value")
  
  assert_eq(Attribute::key(int_attr), "int.key")
  assert_eq(Attribute::value_as_int(int_attr), 42)
  
  assert_eq(Attribute::key(float_attr), "float.key")
  assert_true(Attribute::value_as_float(float_attr) > 3.14 && Attribute::value_as_float(float_attr) < 3.15)
  
  assert_eq(Attribute::key(bool_attr), "bool.key")
  assert_true(Attribute::value_as_bool(bool_attr))
  
  // Test attribute array operations
  let string_array_attr = Attribute::create_array("array.key", ["value1", "value2", "value3"])
  assert_eq(Attribute::array_length(string_array_attr), 3)
  assert_eq(Attribute::array_value_as_string(string_array_attr, 0), "value1")
  assert_eq(Attribute::array_value_as_string(string_array_attr, 2), "value3")
  
  // Test attribute collection
  let attributes = AttributeCollection::new()
  let updated_attrs = AttributeCollection::add(attributes, string_attr)
  let final_attrs = AttributeCollection::add(updated_attrs, int_attr)
  
  // Verify attribute retrieval
  let retrieved_string = AttributeCollection::get(final_attrs, "string.key")
  match retrieved_string {
    Some(attr) => assert_eq(Attribute::value_as_string(attr), "string.value")
    None => assert_true(false)
  }
  
  let retrieved_int = AttributeCollection::get(final_attrs, "int.key")
  match retrieved_int {
    Some(attr) => assert_eq(Attribute::value_as_int(attr), 42)
    None => assert_true(false)
  }
  
  // Test non-existent attribute
  let non_existent = AttributeCollection::get(final_attrs, "non.existent.key")
  assert_eq(non_existent, None)
}

// Test 3: Metric Collection and Aggregation
test "metric collection and aggregation" {
  // Test counter metric
  let metric_provider = MetricProvider::default()
  let counter = MetricProvider::create_counter(metric_provider, "test.counter", "Test counter metric", "operations")
  
  // Add values to counter
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  Counter::add(counter, 10.0)
  
  // Test gauge metric
  let gauge = MetricProvider::create_gauge(metric_provider, "test.gauge", "Test gauge metric", "percentage")
  
  // Set gauge values
  Gauge::set(gauge, 25.5)
  Gauge::set(gauge, 50.0)
  Gauge::set(gauge, 75.3)
  
  // Test histogram metric
  let histogram = MetricProvider::create_histogram(metric_provider, "test.histogram", "Test histogram metric", "milliseconds")
  
  // Record histogram values
  Histogram::record(histogram, 10.0)
  Histogram::record(histogram, 25.0)
  Histogram::record(histogram, 50.0)
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  
  // Test metric collection export
  let metrics = MetricProvider::collect_all(metric_provider)
  assert_true(metrics.length() >= 3) // At least counter, gauge, and histogram
  
  // Verify metric names
  let counter_metric = metrics.filter(|m| Metric::name(m) == "test.counter")
  assert_eq(counter_metric.length(), 1)
  
  let gauge_metric = metrics.filter(|m| Metric::name(m) == "test.gauge")
  assert_eq(gauge_metric.length(), 1)
  
  let histogram_metric = metrics.filter(|m| Metric::name(m) == "test.histogram")
  assert_eq(histogram_metric.length(), 1)
}

// Test 4: Span Lifecycle Management
test "span lifecycle management" {
  // Test tracer creation
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test.tracer", "1.0.0")
  
  // Test span creation
  let span = Tracer::start_span(tracer, "test.span")
  
  // Verify initial span state
  assert_eq(Span::name(span), "test.span")
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  // Test span attributes
  Span::set_attribute(span, "user.id", "user123")
  Span::set_attribute(span, "operation.type", "database.query")
  
  // Test span events
  Span::add_event(span, "operation.start", [("operation.name", "SELECT"), ("table.name", "users")])
  Span::add_event(span, "operation.complete", [("rows.affected", "5"), ("duration.ms", "125")])
  
  // Test span status changes
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Test span ending
  Span::end(span)
  assert_false(Span::is_recording(span))
  
  // Test child span creation
  let parent_span = Tracer::start_span(tracer, "parent.span")
  let child_span = Tracer::start_span_with_parent(tracer, "child.span", parent_span)
  
  // Verify parent-child relationship
  assert_eq(Span::parent_span_id(child_span), Span::span_id(parent_span))
  
  // End both spans
  Span::end(child_span)
  Span::end(parent_span)
}

// Test 5: Logging Operations
test "logging operations" {
  // Test logger creation
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test.logger", "1.0.0")
  
  // Test log record creation
  let info_log = LogRecord::new(Info, "Test info message")
  let warn_log = LogRecord::new(Warn, "Test warning message")
  let error_log = LogRecord::new(Error, "Test error message")
  
  // Verify log severity levels
  assert_eq(LogRecord::severity(info_log), Info)
  assert_eq(LogRecord::severity(warn_log), Warn)
  assert_eq(LogRecord::severity(error_log), Error)
  
  // Test log messages
  assert_eq(LogRecord::message(info_log), "Test info message")
  assert_eq(LogRecord::message(warn_log), "Test warning message")
  assert_eq(LogRecord::message(error_log), "Test error message")
  
  // Test log with attributes
  let log_with_attrs = LogRecord::with_attributes(
    Error,
    "Database operation failed",
    [("error.code", "500"), ("error.message", "Connection timeout"), ("retry.count", "3")]
  )
  
  // Verify log attributes
  let error_code = LogRecord::get_attribute(log_with_attrs, "error.code")
  match error_code {
    Some(StringValue(value)) => assert_eq(value, "500")
    _ => assert_true(false)
  }
  
  // Test log emission
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, log_with_attrs)
  
  // Test log with context correlation
  let trace_id = "12345678901234567890123456789012"
  let span_id = "1234567890123456"
  let correlated_log = LogRecord::with_context(
    Info,
    "Operation with trace context",
    [("operation.type", "http.request")],
    trace_id,
    span_id
  )
  
  assert_eq(LogRecord::trace_id(correlated_log), trace_id)
  assert_eq(LogRecord::span_id(correlated_log), span_id)
  
  Logger::emit(logger, correlated_log)
}

// Test 6: Resource Management
test "resource management" {
  // Test resource creation
  let resource = Resource::new()
  
  // Test resource with attributes
  let resource_attrs = [
    ("service.name", "azimuth-service"),
    ("service.version", "1.0.0"),
    ("service.namespace", "production"),
    ("host.name", "web-server-01"),
    ("host.ip", "192.168.1.100")
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  // Verify resource attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  match service_name {
    Some(StringValue(value)) => assert_eq(value, "azimuth-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  match service_version {
    Some(StringValue(value)) => assert_eq(value, "1.0.0")
    _ => assert_true(false)
  }
  
  // Test resource merging
  let additional_attrs = [
    ("deployment.environment", "staging"),
    ("process.id", "12345"),
    ("process.name", "azimuth")
  ]
  
  let additional_resource = Resource::with_attributes(Resource::new(), additional_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, additional_resource)
  
  // Verify merged resource contains attributes from both
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  match host_name {
    Some(StringValue(value)) => assert_eq(value, "web-server-01")
    _ => assert_true(false)
  }
  
  let deployment_env = Resource::get_attribute(merged_resource, "deployment.environment")
  match deployment_env {
    Some(StringValue(value)) => assert_eq(value, "staging")
    _ => assert_true(false)
  }
  
  // Test resource with different attribute types
  let typed_attrs = [
    ("int.attr", IntValue(42)),
    ("float.attr", FloatValue(3.14159)),
    ("bool.attr", BoolValue(true)),
    ("string.array.attr", ArrayStringValue(["a", "b", "c"])),
    ("int.array.attr", ArrayIntValue([1, 2, 3]))
  ]
  
  let typed_resource = Resource::with_attributes(Resource::new(), typed_attrs)
  
  // Verify typed attributes
  let int_attr = Resource::get_attribute(typed_resource, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let bool_attr = Resource::get_attribute(typed_resource, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
}

// Test 7: Context Propagation
test "context propagation" {
  // Test context creation
  let context = Context::root()
  
  // Test context with values
  let key1 = ContextKey::new("key1")
  let key2 = ContextKey::new("key2")
  let key3 = ContextKey::new("key3")
  
  let context_with_value1 = Context::with_value(context, key1, "value1")
  let context_with_value2 = Context::with_value(context_with_value1, key2, 42)
  let context_with_value3 = Context::with_value(context_with_value2, key3, true)
  
  // Verify context values
  let value1 = Context::get(context_with_value3, key1)
  match value1 {
    Some(StringValue(v)) => assert_eq(v, "value1")
    _ => assert_true(false)
  }
  
  let value2 = Context::get(context_with_value3, key2)
  match value2 {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  let value3 = Context::get(context_with_value3, key3)
  match value3 {
    Some(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
  
  // Test propagator creation
  let propagator = TextMapPropagator::new()
  
  // Test context injection
  let carrier = TextMapCarrier::new()
  Propagator::inject(propagator, context_with_value3, carrier)
  
  // Test context extraction
  let extracted_context = Propagator::extract(propagator, carrier)
  
  // Verify extracted context values
  let extracted_value1 = Context::get(extracted_context, key1)
  match extracted_value1 {
    Some(StringValue(v)) => assert_eq(v, "value1")
    _ => assert_true(false)
  }
  
  // Test composite propagator
  let composite_propagator = CompositePropagator::new([propagator])
  
  let composite_carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, context_with_value3, composite_carrier)
  
  let composite_extracted = CompositePropagator::extract(composite_propagator, composite_carrier)
  let composite_value2 = Context::get(composite_extracted, key2)
  
  match composite_value2 {
    Some(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
}

// Test 8: Baggage Operations
test "baggage operations" {
  // Test baggage creation
  let baggage = Baggage::new()
  
  // Test baggage entry creation
  let entry1 = BaggageEntry::new("user.id", "12345")
  let entry2 = BaggageEntry::new("session.id", "abcdef123456")
  let entry3 = BaggageEntry::new("request.id", "req-789")
  
  // Verify baggage entries
  assert_eq(BaggageEntry::key(entry1), "user.id")
  assert_eq(BaggageEntry::value(entry1), "12345")
  
  // Test baggage with entries
  let baggage_with_entry1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_entry2 = Baggage::set_entry(baggage_with_entry1, "session.id", "abcdef123456")
  let baggage_with_entry3 = Baggage::set_entry(baggage_with_entry2, "request.id", "req-789")
  
  // Verify baggage retrieval
  let user_id = Baggage::get_entry(baggage_with_entry3, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(baggage_with_entry3, "session.id")
  match session_id {
    Some(value) => assert_eq(value, "abcdef123456")
    None => assert_true(false)
  }
  
  // Test baggage entry removal
  let baggage_without_user = Baggage::remove_entry(baggage_with_entry3, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_without_user, "user.id")
  assert_eq(removed_user_id, None)
  
  // Verify other entries still exist
  let remaining_session_id = Baggage::get_entry(baggage_without_user, "session.id")
  match remaining_session_id {
    Some(value) => assert_eq(value, "abcdef123456")
    None => assert_true(false)
  }
  
  // Test baggage with metadata
  let baggage_with_metadata = Baggage::set_entry_with_metadata(
    baggage_without_user,
    "metadata.key",
    "metadata.value",
    [("property1", "value1"), ("property2", "value2")]
  )
  
  // Test baggage serialization
  let serialized = Baggage::serialize(baggage_with_metadata)
  assert_true(serialized.length() > 0)
  
  // Test baggage deserialization
  let deserialized = Baggage::deserialize(serialized)
  let metadata_value = Baggage::get_entry(deserialized, "metadata.key")
  match metadata_value {
    Some(value) => assert_eq(value, "metadata.value")
    None => assert_true(false)
  }
}

// Test 9: Sampling Operations
test "sampling operations" {
  // Test sampler creation
  let always_on_sampler = Sampler::always_on()
  let always_off_sampler = Sampler::always_off()
  let trace_id_ratio_sampler = Sampler::trace_id_ratio(0.5)
  
  // Test sampling decision
  let trace_id = "12345678901234567890123456789012"
  let span_name = "test.span"
  let parent_context = None
  
  // Always on sampler should always sample
  let always_on_decision = Sampler::should_sample(always_on_sampler, parent_context, trace_id, span_name, [])
  assert_eq(SamplingDecision::decision(always_on_decision), RecordAndSample)
  
  // Always off sampler should never sample
  let always_off_decision = Sampler::should_sample(always_off_sampler, parent_context, trace_id, span_name, [])
  assert_eq(SamplingDecision::decision(always_off_decision), Drop)
  
  // Trace ID ratio sampler should make consistent decisions
  let ratio_decision1 = Sampler::should_sample(trace_id_ratio_sampler, parent_context, trace_id, span_name, [])
  let ratio_decision2 = Sampler::should_sample(trace_id_ratio_sampler, parent_context, trace_id, span_name, [])
  assert_eq(SamplingDecision::decision(ratio_decision1), SamplingDecision::decision(ratio_decision2))
  
  // Test sampler with attributes
  let attributes = [("service.name", "test.service"), ("operation.type", "http.request")]
  let decision_with_attrs = Sampler::should_sample(always_on_sampler, parent_context, trace_id, span_name, attributes)
  assert_eq(SamplingDecision::decision(decision_with_attrs), RecordAndSample)
  
  // Test parent-based sampler
  let parent_based_sampler = Sampler::parent_based(Box::new(always_on_sampler))
  
  // Test with sampled parent
  let sampled_parent_context = Some(Context::with_value(
    Context::root(),
    ContextKey::new("sampling.decision"),
    "record_and_sample"
  ))
  
  let parent_based_decision = Sampler::should_sample(
    parent_based_sampler,
    sampled_parent_context,
    trace_id,
    span_name,
    []
  )
  
  assert_eq(SamplingDecision::decision(parent_based_decision), RecordAndSample)
  
  // Test with non-sampled parent
  let non_sampled_parent_context = Some(Context::with_value(
    Context::root(),
    ContextKey::new("sampling.decision"),
    "drop"
  ))
  
  let parent_based_decision2 = Sampler::should_sample(
    parent_based_sampler,
    non_sampled_parent_context,
    trace_id,
    span_name,
    []
  )
  
  assert_eq(SamplingDecision::decision(parent_based_decision2), Drop)
}

// Test 10: Instrumentation Library Operations
test "instrumentation library operations" {
  // Test instrumentation library creation
  let library = InstrumentationLibrary::new("test.library", "1.0.0")
  
  // Verify library properties
  assert_eq(InstrumentationLibrary::name(library), "test.library")
  assert_eq(InstrumentationLibrary::version(library), Some("1.0.0"))
  
  // Test library without version
  let library_without_version = InstrumentationLibrary::new("test.library.no.version")
  assert_eq(InstrumentationLibrary::name(library_without_version), "test.library.no.version")
  assert_eq(InstrumentationLibrary::version(library_without_version), None)
  
  // Test library with schema URL
  let library_with_schema = InstrumentationLibrary::with_schema(
    "test.library.schema",
    Some("1.0.0"),
    Some("https://example.com/schema/v1")
  )
  
  assert_eq(InstrumentationLibrary::name(library_with_schema), "test.library.schema")
  assert_eq(InstrumentationLibrary::version(library_with_schema), Some("1.0.0"))
  assert_eq(InstrumentationLibrary::schema_url(library_with_schema), Some("https://example.com/schema/v1"))
  
  // Test library equality
  let library1 = InstrumentationLibrary::new("test.library", "1.0.0")
  let library2 = InstrumentationLibrary::new("test.library", "1.0.0")
  let library3 = InstrumentationLibrary::new("test.library", "2.0.0")
  
  assert_true(InstrumentationLibrary::equals(library1, library2))
  assert_false(InstrumentationLibrary::equals(library1, library3))
  
  // Test library serialization
  let serialized = InstrumentationLibrary::serialize(library)
  assert_true(serialized.contains("test.library"))
  assert_true(serialized.contains("1.0.0"))
  
  // Test library deserialization
  let deserialized = InstrumentationLibrary::deserialize(serialized)
  assert_eq(InstrumentationLibrary::name(deserialized), "test.library")
  assert_eq(InstrumentationLibrary::version(deserialized), Some("1.0.0"))
}