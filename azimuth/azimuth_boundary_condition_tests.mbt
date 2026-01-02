// Boundary Condition Tests for Azimuth Telemetry System
// This file contains test cases covering edge cases and boundary conditions

test "empty and null string handling" {
  // Test empty string in attributes
  let attributes = Attributes::new()
  Attributes::set(attributes, "empty.string", StringValue(""))
  assert_eq(Attributes::get(attributes, "empty.string"), Some(StringValue("")))
  
  // Test empty string in baggage
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "", "value")
  assert_eq(Baggage::get_entry(updated_baggage, ""), Some("value"))
  
  let empty_value_baggage = Baggage::set_entry(baggage, "key", "")
  assert_eq(Baggage::get_entry(empty_value_baggage, "key"), Some(""))
  
  // Test empty string in resource attributes
  let resource = Resource::new()
  let resource_with_empty = Resource::with_attributes(resource, [("empty.key", StringValue(""))])
  assert_eq(Resource::get_attribute(resource_with_empty, "empty.key"), Some(StringValue("")))
  
  // Test empty string in log body
  let log_record = LogRecord::new(Info, "")
  assert_eq(LogRecord::body(log_record), Some(""))
  
  // Test empty string in span name
  let span_context = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("", Internal, span_context)
  assert_eq(Span::name(span), "")
}

test "zero and negative numeric values" {
  // Test zero values in metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test_meter")
  
  let counter = Meter::create_counter(meter, "zero_counter")
  Counter::add(counter, 0.0)
  
  let histogram = Meter::create_histogram(meter, "zero_histogram")
  Histogram::record(histogram, 0.0)
  
  let updown_counter = Meter::create_updown_counter(meter, "negative_counter")
  UpDownCounter::add(updown_counter, -10.0)
  
  // Test negative values in attributes
  let attributes = Attributes::new()
  Attributes::set(attributes, "negative.int", IntValue(-42))
  assert_eq(Attributes::get(attributes, "negative.int"), Some(IntValue(-42)))
  
  Attributes::set(attributes, "negative.float", FloatValue(-3.14))
  assert_eq(Attributes::get(attributes, "negative.float"), Some(FloatValue(-3.14)))
  
  // Test zero timestamp
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Zero timestamp test"),
    None,
    Some(0L),
    Some(0L),
    None,
    None,
    None
  )
  assert_eq(log_record.timestamp, Some(0L))
  assert_eq(log_record.observed_timestamp, Some(0L))
}

test "maximum and minimum values" {
  // Test maximum values in attributes
  let attributes = Attributes::new()
  Attributes::set(attributes, "max.int", IntValue(2147483647)) // Max 32-bit int
  Attributes::set(attributes, "min.int", IntValue(-2147483648)) // Min 32-bit int
  
  // Test maximum values in metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test_meter")
  
  let histogram = Meter::create_histogram(meter, "max_histogram")
  Histogram::record(histogram, 999999.999)
  
  let counter = Meter::create_counter(meter, "max_counter")
  Counter::add(counter, 999999.999)
  
  // Test maximum timestamp
  let max_timestamp = 9223372036854775807L // Max 64-bit int
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Max timestamp test"),
    None,
    Some(max_timestamp),
    Some(max_timestamp),
    None,
    None,
    None
  )
  assert_eq(log_record.timestamp, Some(max_timestamp))
  assert_eq(log_record.observed_timestamp, Some(max_timestamp))
}

test "array boundary conditions" {
  // Test empty arrays
  let empty_string_array = ArrayStringValue([])
  let empty_int_array = ArrayIntValue([])
  
  let attributes = Attributes::new()
  Attributes::set(attributes, "empty.string.array", empty_string_array)
  Attributes::set(attributes, "empty.int.array", empty_int_array)
  
  // Test single element arrays
  let single_string_array = ArrayStringValue(["single"])
  let single_int_array = ArrayIntValue([1])
  
  Attributes::set(attributes, "single.string.array", single_string_array)
  Attributes::set(attributes, "single.int.array", single_int_array)
  
  // Test large arrays (simulated)
  let large_string_array = ArrayStringValue(["item1", "item2", "item3", "item4", "item5"])
  let large_int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  Attributes::set(attributes, "large.string.array", large_string_array)
  Attributes::set(attributes, "large.int.array", large_int_array)
}

test "context and baggage boundary conditions" {
  // Test empty context
  let empty_context = Context::root()
  let key = ContextKey::new("test_key")
  assert_eq(Context::get(empty_context, key), None)
  
  // Test context with empty key
  let empty_key = ContextKey::new("")
  let context_with_empty_key = Context::with_value(empty_context, empty_key, "value")
  assert_eq(Context::get(context_with_empty_key, empty_key), Some("value"))
  
  // Test empty baggage
  let empty_baggage = Baggage::new()
  assert_eq(Baggage::get_entry(empty_baggage, "any_key"), None)
  
  // Test baggage with special characters
  let special_baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(special_baggage, "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./", "special_value")
  assert_eq(Baggage::get_entry(updated_baggage, "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"), Some("special_value"))
}

test "span context boundary conditions" {
  // Test invalid span context (empty trace_id and span_id)
  let invalid_context = SpanContext::new("", "", false, "")
  assert_false(SpanContext::is_valid(invalid_context))
  assert_false(SpanContext::is_sampled(invalid_context))
  
  // Test valid span context with empty trace_state
  let valid_context = SpanContext::new("trace_id", "span_id", true, "")
  assert_true(SpanContext::is_valid(valid_context))
  assert_true(SpanContext::is_sampled(valid_context))
  
  // Test span context with very long trace_id and span_id
  let long_trace_id = "0123456789abcdef0123456789abcdef01234567"
  let long_span_id = "0123456789abcdef"
  let long_context = SpanContext::new(long_trace_id, long_span_id, false, "trace_state")
  assert_eq(SpanContext::trace_id(long_context), long_trace_id)
  assert_eq(SpanContext::span_id(long_context), long_span_id)
  assert_false(SpanContext::is_sampled(long_context))
}

test "http client boundary conditions" {
  // Test empty HTTP method and URL
  let empty_request = HttpRequest::new("", "", [], None)
  assert_eq(HttpRequest::http_method(empty_request), "")
  assert_eq(HttpRequest::url(empty_request), "")
  assert_eq(HttpRequest::body(empty_request), None)
  
  // Test HTTP response with extreme status codes
  let min_status_response = HttpResponse::new(0, [], None)
  assert_eq(HttpResponse::status_code(min_status_response), 0)
  
  let max_status_response = HttpResponse::new(999, [], None)
  assert_eq(HttpResponse::status_code(max_status_response), 999)
  
  // Test HTTP with empty headers
  let no_headers_request = HttpRequest::new("GET", "https://example.com", [], Some("body"))
  assert_eq(HttpRequest::http_method(no_headers_request), "GET")
  assert_eq(HttpRequest::url(no_headers_request), "https://example.com")
  assert_eq(HttpRequest::body(no_headers_request), Some("body"))
  
  let no_headers_response = HttpResponse::new(200, [], Some("response"))
  assert_eq(HttpResponse::status_code(no_headers_response), 200)
  assert_eq(HttpResponse::body(no_headers_response), Some("response"))
}

test "text map carrier boundary conditions" {
  // Test empty carrier
  let empty_carrier = TextMapCarrier::new()
  assert_eq(TextMapCarrier::get(empty_carrier, "any_key"), None)
  
  // Test carrier with empty key and value
  TextMapCarrier::set(empty_carrier, "", "")
  assert_eq(TextMapCarrier::get(empty_carrier, ""), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Test carrier with special characters in key
  TextMapCarrier::set(empty_carrier, "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./", "special_value")
  assert_eq(TextMapCarrier::get(empty_carrier, "special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"), Some("special_value"))
}

test "instrumentation scope boundary conditions" {
  // Test instrumentation scope with empty name
  let empty_scope = InstrumentationScope::{ name: "", version: None, schema_url: None }
  assert_eq(empty_scope.name, "")
  assert_eq(empty_scope.version, None)
  assert_eq(empty_scope.schema_url, None)
  
  // Test instrumentation scope with empty version and schema_url
  let empty_version_scope = InstrumentationScope::{ name: "test", version: Some(""), schema_url: Some("") }
  assert_eq(empty_version_scope.name, "test")
  assert_eq(empty_version_scope.version, Some(""))
  assert_eq(empty_version_scope.schema_url, Some(""))
  
  // Test instrumentation scope with very long name
  let long_name = "0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz0123456789abcdefghijklmnopqrstuvwxyz"
  let long_name_scope = InstrumentationScope::{ name: long_name, version: Some("1.0.0"), schema_url: Some("https://example.com/schema") }
  assert_eq(long_name_scope.name, long_name)
  assert_eq(long_name_scope.version, Some("1.0.0"))
  assert_eq(long_name_scope.schema_url, Some("https://example.com/schema"))
}

test "severity number boundary conditions" {
  // Test all severity levels
  let trace_record = LogRecord::new(Trace, "Trace message")
  assert_eq(LogRecord::severity_number(trace_record), Trace)
  
  let debug_record = LogRecord::new(Debug, "Debug message")
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  
  let info_record = LogRecord::new(Info, "Info message")
  assert_eq(LogRecord::severity_number(info_record), Info)
  
  let warn_record = LogRecord::new(Warn, "Warning message")
  assert_eq(LogRecord::severity_number(warn_record), Warn)
  
  let error_record = LogRecord::new(Error, "Error message")
  assert_eq(LogRecord::severity_number(error_record), Error)
  
  let fatal_record = LogRecord::new(Fatal, "Fatal message")
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
}

test "span kind and status code boundary conditions" {
  // Test all span kinds
  let span_context = SpanContext::new("trace_id", "span_id", true, "")
  
  let internal_span = Span::new("internal_span", Internal, span_context)
  assert_eq(Span::kind(internal_span), Internal)
  
  let server_span = Span::new("server_span", Server, span_context)
  assert_eq(Span::kind(server_span), Server)
  
  let client_span = Span::new("client_span", Client, span_context)
  assert_eq(Span::kind(client_span), Client)
  
  let producer_span = Span::new("producer_span", Producer, span_context)
  assert_eq(Span::kind(producer_span), Producer)
  
  let consumer_span = Span::new("consumer_span", Consumer, span_context)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // Test all status codes
  Span::set_status(internal_span, Unset, None)
  assert_eq(Span::status(internal_span), Unset)
  
  Span::set_status(server_span, Ok, None)
  assert_eq(Span::status(server_span), Ok)
  
  Span::set_status(client_span, Error, None)
  assert_eq(Span::status(client_span), Error)
}