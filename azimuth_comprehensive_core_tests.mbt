// Comprehensive Core Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for core functionality

test "attributes operations" {
  let attrs = Attributes::new()
  
  // Test setting and getting string attributes
  Attributes::set(attrs, "string.key", StringValue("test_value"))
  let string_value = Attributes::get(attrs, "string.key")
  assert_eq(string_value, Some(StringValue("test_value")))
  
  // Test setting and getting int attributes
  Attributes::set(attrs, "int.key", IntValue(42))
  let int_value = Attributes::get(attrs, "int.key")
  assert_eq(int_value, Some(IntValue(42)))
  
  // Test getting non-existent attribute
  let missing_value = Attributes::get(attrs, "missing.key")
  assert_eq(missing_value, None)
}

test "span lifecycle with different kinds" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  // Test creating different span kinds
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-operation", Server, span_ctx)
  let client_span = Span::new("client-operation", Client, span_ctx)
  
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  
  // Test span status
  Span::set_status(internal_span, Ok, Some("Operation completed"))
  assert_eq(Span::status(internal_span), Ok)
  
  // Test adding events
  Span::add_event(internal_span, "event1", Some([("key1", StringValue("value1"))]))
  Span::end(internal_span)
}

test "metrics instruments comprehensive" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-metrics")
  
  // Test Counter
  let counter = Meter::create_counter(meter, "http.requests")
  assert_eq(counter.name, "http.requests")
  Counter::add(counter, 1.0)
  Counter::add(counter, 5.0)
  
  // Test Histogram
  let histogram = Meter::create_histogram(meter, "response.time", Some("Response time in milliseconds"), Some("ms"))
  assert_eq(histogram.name, "response.time")
  assert_eq(histogram.description, Some("Response time in milliseconds"))
  assert_eq(histogram.unit, Some("ms"))
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.0)
  
  // Test UpDownCounter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  assert_eq(updown_counter.name, "active.connections")
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -5.0)
  
  // Test Gauge
  let gauge = Meter::create_gauge(meter, "memory.usage", Some("Memory usage in bytes"), Some("bytes"))
  assert_eq(gauge.name, "memory.usage")
  assert_eq(gauge.description, Some("Memory usage in bytes"))
  assert_eq(gauge.unit, Some("bytes"))
}

test "log record with full context" {
  let severity = Error
  let body = Some("Database connection failed")
  let attrs = Attributes::new()
  Attributes::set(attrs, "error.code", IntValue(500))
  let timestamp = Some(1735689600000000000L)
  let observed_timestamp = Some(1735689600000001000L)
  let trace_id = Some("trace123")
  let span_id = Some("span456")
  let ctx = Context::with_value(Context::root(), ContextKey::new("user.id"), "user123")
  
  let record = LogRecord::new_with_context(
    severity, body, Some(attrs), timestamp, observed_timestamp, trace_id, span_id, Some(ctx)
  )
  
  assert_eq(LogRecord::severity_number(record), Error)
  assert_eq(LogRecord::body(record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
}

test "resource operations and merging" {
  let base_resource = Resource::new()
  let base_attrs = [("service.name", StringValue("payment-service")), ("service.version", StringValue("1.0.0"))]
  let resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // Test getting attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(StringValue("payment-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(missing_attr, None)
  
  // Test resource merging
  let override_resource = Resource::new()
  let override_attrs = [("environment", StringValue("production"))]
  let resource_with_override = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(resource_with_attrs, resource_with_override)
  let env_attr = Resource::get_attribute(merged_resource, "environment")
  assert_eq(env_attr, Some(StringValue("production")))
}

test "baggage operations" {
  let baggage = Baggage::new()
  
  // Test setting and getting entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "user123")
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "request.id", "req456")
  
  let user_id = Baggage::get_entry(updated_baggage2, "user.id")
  let request_id = Baggage::get_entry(updated_baggage2, "request.id")
  let missing_entry = Baggage::get_entry(updated_baggage2, "missing.key")
  
  assert_eq(user_id, Some("user123"))
  assert_eq(request_id, Some("req456"))
  assert_eq(missing_entry, None)
  
  // Test removing entries
  let final_baggage = Baggage::remove_entry(updated_baggage2, "user.id")
  let removed_user_id = Baggage::get_entry(final_baggage, "user.id")
  let still_present_request_id = Baggage::get_entry(final_baggage, "request.id")
  
  assert_eq(removed_user_id, None)
  assert_eq(still_present_request_id, Some("req456"))
}

test "composite propagator operations" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Test injection
  let ctx = Context::with_value(Context::root(), ContextKey::new("test.key"), "test.value")
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx, carrier)
  
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extraction
  let carrier_with_trace = TextMapCarrier::new()
  TextMapCarrier::set(carrier_with_trace, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier_with_trace)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "instrumentation scope operations" {
  let scope1 = InstrumentationScope::{ 
    name: "tracer1", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  let scope2 = InstrumentationScope::{ 
    name: "tracer2", 
    version: None, 
    schema_url: None 
  }
  
  assert_eq(scope1.name, "tracer1")
  assert_eq(scope1.version, Some("1.0.0"))
  assert_eq(scope1.schema_url, Some("https://example.com/schema/v1"))
  
  assert_eq(scope2.name, "tracer2")
  assert_eq(scope2.version, None)
  assert_eq(scope2.schema_url, None)
  
  // Test with Tracer and Meter
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "test-tracer", Some("2.0.0"))
  let tracer_scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(tracer_scope.name, "test-tracer")
  assert_eq(tracer_scope.version, Some("2.0.0"))
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  let meter_scope = meter.scope
  
  assert_eq(meter_scope.name, "test-meter")
  assert_eq(meter_scope.version, None)
}

test "attribute value types and conversions" {
  let string_attr = StringValue("test string")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  let string_array_attr = ArrayStringValue(["value1", "value2", "value3"])
  let int_array_attr = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Test attribute values in Attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", string_attr)
  Attributes::set(attrs, "int.key", int_attr)
  Attributes::set(attrs, "float.key", float_attr)
  Attributes::set(attrs, "bool.key", bool_attr)
  Attributes::set(attrs, "string.array.key", string_array_attr)
  Attributes::set(attrs, "int.array.key", int_array_attr)
  
  // Test retrieving different attribute types
  let retrieved_string = Attributes::get(attrs, "string.key")
  let retrieved_int = Attributes::get(attrs, "int.key")
  let retrieved_float = Attributes::get(attrs, "float.key")
  let retrieved_bool = Attributes::get(attrs, "bool.key")
  let retrieved_string_array = Attributes::get(attrs, "string.array.key")
  let retrieved_int_array = Attributes::get(attrs, "int.array.key")
  
  assert_eq(retrieved_string, Some(StringValue("test string")))
  assert_eq(retrieved_int, Some(IntValue(42)))
  assert_eq(retrieved_float, Some(FloatValue(3.14)))
  assert_eq(retrieved_bool, Some(BoolValue(true)))
  assert_eq(retrieved_string_array, Some(ArrayStringValue(["value1", "value2", "value3"])))
  assert_eq(retrieved_int_array, Some(ArrayIntValue([1, 2, 3, 4, 5])))
}

test "logger operations with different severity levels" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "test-logger")
  
  // Test creating log records with different severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let warn_record = LogRecord::new(Warn, "Warning message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  
  // Test severity levels
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Test log bodies
  assert_eq(LogRecord::body(trace_record), Some("Trace message"))
  assert_eq(LogRecord::body(debug_record), Some("Debug message"))
  assert_eq(LogRecord::body(info_record), Some("Info message"))
  assert_eq(LogRecord::body(warn_record), Some("Warning message"))
  assert_eq(LogRecord::body(error_record), Some("Error message"))
  assert_eq(LogRecord::body(fatal_record), Some("Fatal message"))
  
  // Test emitting log records
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  
  assert_true(true)  // If we reach here, emit operations didn't crash
}