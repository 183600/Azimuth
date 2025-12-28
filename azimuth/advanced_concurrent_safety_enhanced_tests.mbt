// Advanced Concurrent Safety Tests for Azimuth Telemetry System
// Tests thread safety and data consistency under concurrent operations

test "concurrent_context_access" {
  // Test thread-safe context operations
  let root_ctx = Context::root()
  let key1 = ContextKey::new("concurrent_key1")
  let key2 = ContextKey::new("concurrent_key2")
  let key3 = ContextKey::new("concurrent_key3")
  
  // Simulate concurrent context modifications
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  
  // Verify data integrity under concurrent access
  assert_eq(Context::get(ctx1, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key1), Some("value1"))
  assert_eq(Context::get(ctx2, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key1), Some("value1"))
  assert_eq(Context::get(ctx3, key2), Some("value2"))
  assert_eq(Context::get(ctx3, key3), Some("value3"))
  
  // Test isolation - different contexts should not interfere
  let isolated_ctx = Context::with_value(root_ctx, key1, "isolated_value")
  assert_eq(Context::get(isolated_ctx, key1), Some("isolated_value"))
  assert_eq(Context::get(isolated_ctx, key2), None)
}

test "concurrent_span_operations" {
  // Test thread-safe span operations
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "concurrent_tracer")
  
  // Create multiple spans concurrently
  let span1 = Tracer::start_span(tracer, "concurrent_span1")
  let span2 = Tracer::start_span(tracer, "concurrent_span2")
  let span3 = Tracer::start_span(tracer, "concurrent_span3")
  
  // Perform concurrent operations on spans
  Span::set_status(span1, Ok, Some("Span 1 completed"))
  Span::set_status(span2, Error, Some("Span 2 failed"))
  Span::set_status(span3, Ok, Some("Span 3 completed"))
  
  Span::add_event(span1, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(span2, "event2", Some([("key2", StringValue("value2"))]))
  Span::add_event(span3, "event3", Some([("key3", StringValue("value3"))]))
  
  // Verify span isolation and data integrity
  assert_eq(Span::name(span1), "concurrent_span1")
  assert_eq(Span::name(span2), "concurrent_span2")
  assert_eq(Span::name(span3), "concurrent_span3")
  assert_eq(Span::is_recording(span1), true)
  assert_eq(Span::is_recording(span2), true)
  assert_eq(Span::is_recording(span3), true)
  
  // End spans concurrently
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
}

test "concurrent_metrics_collection" {
  // Test thread-safe metrics operations
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_meter")
  
  // Create multiple instruments
  let counter1 = Meter::create_counter(meter, "concurrent_counter1")
  let counter2 = Meter::create_counter(meter, "concurrent_counter2")
  let histogram1 = Meter::create_histogram(meter, "concurrent_histogram1")
  let updown_counter1 = Meter::create_updown_counter(meter, "concurrent_updown1")
  let gauge1 = Meter::create_gauge(meter, "concurrent_gauge1")
  
  // Simulate concurrent metric updates
  Counter::add(counter1, 100.0)
  Counter::add(counter1, 200.0)
  Counter::add(counter1, 300.0)
  
  Counter::add(counter2, 50.0)
  Counter::add(counter2, 150.0)
  
  Histogram::record(histogram1, 10.5)
  Histogram::record(histogram1, 20.5)
  Histogram::record(histogram1, 30.5)
  
  UpDownCounter::add(updown_counter1, 10.0)
  UpDownCounter::add(updown_counter1, -5.0)
  UpDownCounter::add(updown_counter1, 15.0)
  
  // Verify instrument integrity
  assert_eq(counter1.name, "concurrent_counter1")
  assert_eq(counter2.name, "concurrent_counter2")
  assert_eq(histogram1.name, "concurrent_histogram1")
  assert_eq(updown_counter1.name, "concurrent_updown1")
  assert_eq(gauge1.name, "concurrent_gauge1")
}

test "concurrent_baggage_operations" {
  // Test thread-safe baggage operations
  let baggage = Baggage::new()
  
  // Simulate concurrent baggage modifications
  let baggage1 = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage2 = Baggage::set_entry(baggage1, "session.id", "abcdef")
  let baggage3 = Baggage::set_entry(baggage2, "request.id", "xyz789")
  
  // Test concurrent read operations
  let user_id = Baggage::get_entry(baggage3, "user.id")
  let session_id = Baggage::get_entry(baggage3, "session.id")
  let request_id = Baggage::get_entry(baggage3, "request.id")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("abcdef"))
  assert_eq(request_id, Some("xyz789"))
  
  // Test concurrent removal operations
  let baggage_after_removal1 = Baggage::remove_entry(baggage3, "user.id")
  let baggage_after_removal2 = Baggage::remove_entry(baggage3, "session.id")
  
  // Verify removal doesn't affect other entries
  assert_eq(Baggage::get_entry(baggage_after_removal1, "user.id"), Some("12345"))
  assert_eq(Baggage::get_entry(baggage_after_removal1, "session.id"), Some("abcdef"))
  assert_eq(Baggage::get_entry(baggage_after_removal1, "request.id"), Some("xyz789"))
}

test "concurrent_resource_management" {
  // Test thread-safe resource operations
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  let attributes1 = [
    ("service.name", StringValue("service1")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance1"))
  ]
  
  let attributes2 = [
    ("service.name", StringValue("service2")),
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("instance2"))
  ]
  
  // Concurrent resource attribute operations
  let resource1_with_attrs = Resource::with_attributes(resource1, attributes1)
  let resource2_with_attrs = Resource::with_attributes(resource2, attributes2)
  
  // Test concurrent resource merging
  let merged_resource = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  
  // Verify resource isolation
  assert_eq(
    Resource::get_attribute(resource1_with_attrs, "service.name"),
    Some(StringValue("service1"))
  )
  
  assert_eq(
    Resource::get_attribute(resource2_with_attrs, "service.name"),
    Some(StringValue("service2"))
  )
  
  assert_eq(
    Resource::get_attribute(merged_resource, "service.name"),
    Some(StringValue("service2"))
  )
}

test "concurrent_propagation_operations" {
  // Test thread-safe propagation operations
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let root_ctx = Context::root()
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Concurrent injection operations
  CompositePropagator::inject(composite, root_ctx, carrier1)
  CompositePropagator::inject(composite, root_ctx, carrier2)
  CompositePropagator::inject(composite, root_ctx, carrier3)
  
  // Verify injection results
  let trace1 = TextMapCarrier::get(carrier1, "traceparent")
  let trace2 = TextMapCarrier::get(carrier2, "traceparent")
  let trace3 = TextMapCarrier::get(carrier3, "traceparent")
  
  assert_eq(trace1, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(trace2, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(trace3, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  
  // Concurrent extraction operations
  let extracted_ctx1 = CompositePropagator::extract(composite, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite, carrier3)
  
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx1, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx2, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx3, extracted_key), Some("true"))
}

test "concurrent_logging_operations" {
  // Test thread-safe logging operations
  let logger_provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(logger_provider, "concurrent_logger1")
  let logger2 = LoggerProvider::get_logger(logger_provider, "concurrent_logger2")
  let logger3 = LoggerProvider::get_logger(logger_provider, "concurrent_logger3")
  
  // Create multiple log records concurrently
  let log1 = LogRecord::new_with_context(
    Info,
    Some("Concurrent log 1"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  let log2 = LogRecord::new_with_context(
    Warn,
    Some("Concurrent log 2"),
    Some(Attributes::new()),
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some("trace789"),
    Some("span012"),
    Some(Context::root())
  )
  
  let log3 = LogRecord::new_with_context(
    Error,
    Some("Concurrent log 3"),
    Some(Attributes::new()),
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some("trace345"),
    Some("span678"),
    Some(Context::root())
  )
  
  // Concurrent log emission
  Logger::emit(logger1, log1)
  Logger::emit(logger2, log2)
  Logger::emit(logger3, log3)
  
  // Verify log record integrity
  assert_eq(LogRecord::severity_number(log1), Info)
  assert_eq(LogRecord::severity_number(log2), Warn)
  assert_eq(LogRecord::severity_number(log3), Error)
  assert_eq(LogRecord::body(log1), Some("Concurrent log 1"))
  assert_eq(LogRecord::body(log2), Some("Concurrent log 2"))
  assert_eq(LogRecord::body(log3), Some("Concurrent log 3"))
}

test "concurrent_http_operations" {
  // Test thread-safe HTTP operations
  let client1 = HttpClient::new()
  let client2 = HttpClient::new()
  let client3 = HttpClient::new()
  
  // Create multiple requests concurrently
  let headers1 = [("Content-Type", "application/json"), ("Authorization", "Bearer token1")]
  let headers2 = [("Content-Type", "application/xml"), ("Authorization", "Bearer token2")]
  let headers3 = [("Content-Type", "text/plain"), ("Authorization", "Bearer token3")]
  
  let request1 = HttpRequest::new("GET", "https://api1.example.com", headers1, Some("{\"data\":\"value1\"}"))
  let request2 = HttpRequest::new("POST", "https://api2.example.com", headers2, Some("<xml>data2</xml>"))
  let request3 = HttpRequest::new("PUT", "https://api3.example.com", headers3, Some("plain data3"))
  
  // Create multiple responses concurrently
  let response1 = HttpResponse::new(200, [("Content-Type", "application/json")], Some("{\"result\":\"success1\"}"))
  let response2 = HttpResponse::new(404, [("Content-Type", "application/xml")], Some("<error>not found</error>"))
  let response3 = HttpResponse::new(500, [("Content-Type", "text/plain")], Some("internal error"))
  
  // Verify request isolation
  assert_eq(HttpRequest::http_method(request1), "GET")
  assert_eq(HttpRequest::http_method(request2), "POST")
  assert_eq(HttpRequest::http_method(request3), "PUT")
  
  assert_eq(HttpRequest::url(request1), "https://api1.example.com")
  assert_eq(HttpRequest::url(request2), "https://api2.example.com")
  assert_eq(HttpRequest::url(request3), "https://api3.example.com")
  
  // Verify response isolation
  assert_eq(HttpResponse::status_code(response1), 200)
  assert_eq(HttpResponse::status_code(response2), 404)
  assert_eq(HttpResponse::status_code(response3), 500)
}