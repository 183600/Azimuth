// Azimuth Telemetry System - Enhanced Test Suite
// This file contains comprehensive test cases for various telemetry features

// Test Case 1: Attributes Operations
test "attributes operations with different value types" {
  let attrs = Attributes::new()
  
  // Test setting string attribute
  Attributes::set(attrs, "service.name", StringValue("azimuth-service"))
  let string_value = Attributes::get(attrs, "service.name")
  assert_eq(string_value, Some(StringValue("azimuth-service")))
  
  // Test setting integer attribute
  Attributes::set(attrs, "service.instance.id", IntValue(12345))
  let int_value = Attributes::get(attrs, "service.instance.id")
  assert_eq(int_value, Some(IntValue(12345)))
  
  // Test setting float attribute
  Attributes::set(attrs, "system.cpu.utilization", FloatValue(0.75))
  let float_value = Attributes::get(attrs, "system.cpu.utilization")
  assert_eq(float_value, Some(FloatValue(0.75)))
  
  // Test setting boolean attribute
  Attributes::set(attrs, "service.enabled", BoolValue(true))
  let bool_value = Attributes::get(attrs, "service.enabled")
  assert_eq(bool_value, Some(BoolValue(true)))
  
  // Test getting non-existent attribute
  let missing_value = Attributes::get(attrs, "non.existent.key")
  assert_eq(missing_value, None)
}

// Test Case 2: Span Lifecycle
test "span lifecycle management" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  let span = Span::new("test-operation", Server, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(span), "test-operation")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(Span::span_context(span), span_ctx)
  
  // Test span status
  assert_eq(Span::status(span), Unset)
  
  // Test adding events
  Span::add_event(span, "event1", Some([("event.type", StringValue("operation.start"))]))
  Span::add_event(span, "event2", Some([("event.type", StringValue("operation.complete"))]))
  
  // Test setting status
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Test ending span
  Span::end(span)
}

// Test Case 3: Baggage Propagation
test "baggage propagation across contexts" {
  let baggage = Baggage::new()
  
  // Test setting baggage entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage2 = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // Test getting baggage entries
  let user_id = Baggage::get_entry(updated_baggage2, "user.id")
  let request_id = Baggage::get_entry(updated_baggage2, "request.id")
  let missing_entry = Baggage::get_entry(updated_baggage2, "missing.key")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing_entry, None)
  
  // Test removing baggage entries
  let final_baggage = Baggage::remove_entry(updated_baggage2, "user.id")
  let removed_user_id = Baggage::get_entry(final_baggage, "user.id")
  let still_present_request_id = Baggage::get_entry(final_baggage, "request.id")
  
  assert_eq(removed_user_id, None)
  assert_eq(still_present_request_id, Some("req-67890"))
}

// Test Case 4: Composite Propagator
test "composite propagator injection and extraction" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  
  // Test injection
  CompositePropagator::inject(composite, ctx, carrier)
  let injected_traceparent = TextMapCarrier::get(carrier, "traceparent")
  assert_eq(injected_traceparent, Some("00-test-trace-id-test-span-id-01"))
  
  // Test extraction
  let carrier_with_data = TextMapCarrier::new()
  TextMapCarrier::set(carrier_with_data, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let extracted_ctx = CompositePropagator::extract(composite, carrier_with_data)
  let extracted_key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, extracted_key)
  
  assert_eq(extracted_value, Some("true"))
}

// Test Case 5: Resource Operations
test "resource operations and merging" {
  let resource1 = Resource::new()
  let attrs1 = [("service.name", StringValue("service-a")), ("service.version", StringValue("1.0.0"))]
  let resource1_with_attrs = Resource::with_attributes(resource1, attrs1)
  
  let resource2 = Resource::new()
  let attrs2 = ["host.name", StringValue("host-1"), "host.ip", StringValue("192.168.1.100")]
  let resource2_with_attrs = Resource::with_attributes(resource2, attrs2)
  
  // Test getting attributes
  let service_name = Resource::get_attribute(resource1_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource1_with_attrs, "service.version")
  let host_name = Resource::get_attribute(resource2_with_attrs, "host.name")
  let missing_attr = Resource::get_attribute(resource1_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(StringValue("service-a")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(host_name, Some(StringValue("host-1")))
  assert_eq(missing_attr, None)
  
  // Test resource merging
  let merged_resource = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  let merged_host_name = Resource::get_attribute(merged_resource, "host.name")
  let merged_service_name = Resource::get_attribute(merged_resource, "service.name")
  
  assert_eq(merged_host_name, Some(StringValue("host-1")))
  assert_eq(merged_service_name, Some(StringValue("service-a")))
}

// Test Case 6: Instrument Types
test "different instrument types and operations" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "test-meter")
  
  // Test Counter
  let counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  assert_eq(counter.name, "http.requests.total")
  assert_eq(counter.description, Some("Total HTTP requests"))
  assert_eq(counter.unit, Some("requests"))
  
  Counter::add(counter, 10.0)
  Counter::add(counter, 5.0)
  
  // Test Histogram
  let histogram = Meter::create_histogram(meter, "http.request.duration", Some("HTTP request duration"), Some("ms"))
  assert_eq(histogram.name, "http.request.duration")
  assert_eq(histogram.description, Some("HTTP request duration"))
  assert_eq(histogram.unit, Some("ms"))
  
  Histogram::record(histogram, 150.5)
  Histogram::record(histogram, 200.0)
  
  // Test UpDownCounter
  let updown_counter = Meter::create_updown_counter(meter, "active.connections", Some("Active connections"), Some("connections"))
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(updown_counter.description, Some("Active connections"))
  assert_eq(updown_counter.unit, Some("connections"))
  
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -2.0)
  
  // Test Gauge
  let gauge = Meter::create_gauge(meter, "system.memory.usage", Some("System memory usage"), Some("bytes"))
  assert_eq(gauge.name, "system.memory.usage")
  assert_eq(gauge.description, Some("System memory usage"))
  assert_eq(gauge.unit, Some("bytes"))
  
  // Test instrument conversion
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
}

// Test Case 7: Log Record with Context
test "log record with context and trace information" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let timestamp = 1735689600000000000L
  let observed_timestamp = 1735689600000001000L
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "log.level", StringValue("INFO"))
  Attributes::set(attrs, "component", StringValue("database"))
  
  let ctx = Context::root()
  let key = ContextKey::new("correlation.id")
  let ctx_with_value = Context::with_value(ctx, key, "corr-12345")
  
  let record = LogRecord::new_with_context(
    Info,
    Some("Database connection established"),
    Some(attrs),
    Some(timestamp),
    Some(observed_timestamp),
    Some(trace_id),
    Some(span_id),
    Some(ctx_with_value)
  )
  
  // Test log record properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Database connection established"))
  assert_eq(LogRecord::trace_id(record), Some(trace_id))
  assert_eq(LogRecord::span_id(record), Some(span_id))
  
  // Test with logger
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "database-logger")
  
  Logger::emit(logger, record)
  
  // Test creating simple log record
  let simple_record = LogRecord::new(Error, "Database connection failed")
  assert_eq(LogRecord::severity_number(simple_record), Error)
  assert_eq(LogRecord::body(simple_record), Some("Database connection failed"))
  assert_eq(LogRecord::trace_id(simple_record), None)
  assert_eq(LogRecord::span_id(simple_record), None)
}

// Test Case 8: Error Boundary Handling
test "error boundary handling and edge cases" {
  // Test empty span context
  let empty_span_ctx = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(empty_span_ctx))
  assert_false(SpanContext::is_sampled(empty_span_ctx))
  
  // Test valid span context
  let valid_span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", false, "")
  assert_true(SpanContext::is_valid(valid_span_ctx))
  assert_false(SpanContext::is_sampled(valid_span_ctx))
  
  // Test empty baggage operations
  let empty_baggage = Baggage::new()
  let empty_result = Baggage::get_entry(empty_baggage, "any.key")
  assert_eq(empty_result, None)
  
  // Test empty resource operations
  let empty_resource = Resource::new()
  let empty_attr_result = Resource::get_attribute(empty_resource, "any.attr")
  assert_eq(empty_attr_result, None)
  
  // Test HTTP operations with empty data
  let empty_headers = [] : Array[(String, String)]
  let http_request = HttpRequest::new("GET", "https://example.com", empty_headers, Some(""))
  assert_eq(HttpRequest::http_method(http_request), "GET")
  assert_eq(HttpRequest::url(http_request), "https://example.com")
  assert_eq(HttpRequest::body(http_request), Some(""))
  
  let http_response = HttpResponse::new(404, empty_headers, Some("Not Found"))
  assert_eq(HttpResponse::status_code(http_response), 404)
  assert_eq(HttpResponse::body(http_response), Some("Not Found"))
  
  // Test text map carrier with empty data
  let empty_carrier = TextMapCarrier::new()
  let empty_carrier_result = TextMapCarrier::get(empty_carrier, "any.header")
  assert_eq(empty_carrier_result, None)
}