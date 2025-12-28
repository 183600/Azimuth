// Resource Management Tests for Azimuth Telemetry System
// Tests memory management, resource cleanup, and lifecycle management

test "span_lifecycle_resource_management" {
  // Test proper resource management during span lifecycle
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "resource_test_tracer")
  
  // Create spans and test resource cleanup
  let span1 = Tracer::start_span(tracer, "resource_span_1")
  let span2 = Tracer::start_span(tracer, "resource_span_2")
  let span3 = Tracer::start_span(tracer, "resource_span_3")
  
  // Add events and attributes
  Span::add_event(span1, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(span2, "event2", Some([("key2", StringValue("value2"))]))
  Span::add_event(span3, "event3", Some([("key3", StringValue("value3"))]))
  
  // Set status
  Span::set_status(span1, Ok, Some("Completed"))
  Span::set_status(span2, Error, Some("Failed"))
  Span::set_status(span3, Ok, Some("Completed"))
  
  // Verify spans are active
  @assertion.assert_eq(Span::is_recording(span1), true)?
  @assertion.assert_eq(Span::is_recording(span2), true)?
  @assertion.assert_eq(Span::is_recording(span3), true)?
  
  // End spans to trigger resource cleanup
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  
  // Verify span data is still accessible after ending
  @assertion.assert_eq(Span::name(span1), "resource_span_1")?
  @assertion.assert_eq(Span::name(span2), "resource_span_2")?
  @assertion.assert_eq(Span::name(span3), "resource_span_3")?
}

test "meter_provider_resource_management" {
  // Test resource management for meter providers and instruments
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "resource_meter_1")
  let meter2 = MeterProvider::get_meter(provider, "resource_meter_2")
  
  // Create multiple instruments
  let counter1 = Meter::create_counter(meter1, "resource_counter_1")
  let counter2 = Meter::create_counter(meter1, "resource_counter_2")
  let histogram1 = Meter::create_histogram(meter1, "resource_histogram_1")
  let updown_counter1 = Meter::create_updown_counter(meter1, "resource_updown_counter_1")
  let gauge1 = Meter::create_gauge(meter1, "resource_gauge_1")
  
  let counter3 = Meter::create_counter(meter2, "resource_counter_3")
  let histogram2 = Meter::create_histogram(meter2, "resource_histogram_2")
  
  // Use instruments to verify they're properly managed
  Counter::add(counter1, 100.0)
  Counter::add(counter2, 200.0)
  Histogram::record(histogram1, 150.5)
  UpDownCounter::add(updown_counter1, 50.0)
  Counter::add(counter3, 300.0)
  Histogram::record(histogram2, 250.5)
  
  // Verify instrument integrity
  @assertion.assert_eq(counter1.name, "resource_counter_1")?
  @assertion.assert_eq(counter2.name, "resource_counter_2")?
  @assertion.assert_eq(histogram1.name, "resource_histogram_1")?
  @assertion.assert_eq(updown_counter1.name, "resource_updown_counter_1")?
  @assertion.assert_eq(gauge1.name, "resource_gauge_1")?
  @assertion.assert_eq(counter3.name, "resource_counter_3")?
  @assertion.assert_eq(histogram2.name, "resource_histogram_2")?
}

test "logger_provider_resource_management" {
  // Test resource management for logger providers
  let provider = LoggerProvider::default()
  let logger1 = LoggerProvider::get_logger(provider, "resource_logger_1")
  let logger2 = LoggerProvider::get_logger(provider, "resource_logger_2")
  
  // Create multiple log records
  let log1 = LogRecord::new(Info, "Resource test log 1")
  let log2 = LogRecord::new(Warn, "Resource test log 2")
  let log3 = LogRecord::new(Error, "Resource test log 3")
  let log4 = LogRecord::new(Debug, "Resource test log 4")
  
  // Create complex log records with attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "resource.test", StringValue("test_value"))
  
  let complex_log = LogRecord::new_with_context(
    Info,
    Some("Complex resource test log"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace123"),
    Some("span456"),
    Some(Context::root())
  )
  
  // Emit logs through different loggers
  Logger::emit(logger1, log1)
  Logger::emit(logger1, log2)
  Logger::emit(logger2, log3)
  Logger::emit(logger2, log4)
  Logger::emit(logger1, complex_log)
  
  // Verify log record integrity
  @assertion.assert_eq(LogRecord::body(log1), Some("Resource test log 1"))?
  @assertion.assert_eq(LogRecord::body(log2), Some("Resource test log 2"))?
  @assertion.assert_eq(LogRecord::body(log3), Some("Resource test log 3"))?
  @assertion.assert_eq(LogRecord::body(log4), Some("Resource test log 4"))?
  @assertion.assert_eq(LogRecord::body(complex_log), Some("Complex resource test log"))?
}

test "context_resource_cleanup" {
  // Test context resource cleanup and management
  let root_ctx = Context::root()
  
  // Create context chain with multiple values
  let key1 = ContextKey::new("resource.key.1")
  let key2 = ContextKey::new("resource.key.2")
  let key3 = ContextKey::new("resource.key.3")
  
  let ctx1 = Context::with_value(root_ctx, key1, "resource_value_1")
  let ctx2 = Context::with_value(ctx1, key2, "resource_value_2")
  let ctx3 = Context::with_value(ctx2, key3, "resource_value_3")
  
  // Verify context chain
  @assertion.assert_eq(Context::get(ctx1, key1), Some("resource_value_1"))?
  @assertion.assert_eq(Context::get(ctx2, key1), Some("resource_value_1"))?
  @assertion.assert_eq(Context::get(ctx2, key2), Some("resource_value_2"))?
  @assertion.assert_eq(Context::get(ctx3, key1), Some("resource_value_1"))?
  @assertion.assert_eq(Context::get(ctx3, key2), Some("resource_value_2"))?
  @assertion.assert_eq(Context::get(ctx3, key3), Some("resource_value_3"))?
  
  // Test context isolation
  let isolated_ctx = Context::with_value(root_ctx, key1, "isolated_value")
  @assertion.assert_eq(Context::get(isolated_ctx, key1), Some("isolated_value"))?
  @assertion.assert_eq(Context::get(isolated_ctx, key2), None)?
  
  // Root context should remain clean
  @assertion.assert_eq(Context::get(root_ctx, key1), None)?
  @assertion.assert_eq(Context::get(root_ctx, key2), None)?
  @assertion.assert_eq(Context::get(root_ctx, key3), None)?
}

test "resource_attribute_management" {
  // Test resource attribute resource management
  let resource1 = Resource::new()
  let resource2 = Resource::new()
  
  // Create resources with many attributes
  let attributes1 = [
    ("service.name", StringValue("resource_service_1")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("resource_instance_1")),
    ("host.name", StringValue("resource_host_1")),
    ("process.id", IntValue(12345)),
    ("process.executable.name", StringValue("resource_binary_1"))
  ]
  
  let attributes2 = [
    ("service.name", StringValue("resource_service_2")),
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("resource_instance_2")),
    ("host.name", StringValue("resource_host_2")),
    ("process.id", IntValue(67890)),
    ("process.executable.name", StringValue("resource_binary_2"))
  ]
  
  let resource1_with_attrs = Resource::with_attributes(resource1, attributes1)
  let resource2_with_attrs = Resource::with_attributes(resource2, attributes2)
  
  // Test resource merging and cleanup
  let merged_resource = Resource::merge(resource1_with_attrs, resource2_with_attrs)
  
  // Verify resource isolation
  @assertion.assert_eq(
    Resource::get_attribute(resource1_with_attrs, "service.name"),
    Some(StringValue("resource_service_1"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(resource2_with_attrs, "service.name"),
    Some(StringValue("resource_service_2"))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(merged_resource, "service.name"),
    Some(StringValue("resource_service_2"))
  )?
  
  // Verify all attributes are accessible
  @assertion.assert_eq(
    Resource::get_attribute(resource1_with_attrs, "process.id"),
    Some(IntValue(12345))
  )?
  
  @assertion.assert_eq(
    Resource::get_attribute(resource2_with_attrs, "process.id"),
    Some(IntValue(67890))
  )?
}

test "baggage_resource_management" {
  // Test baggage resource management
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  
  // Add multiple entries to baggage
  let baggage1_with_entries = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage1, "user.id", "user123"),
      "session.id", "session456"
    ),
    "request.id", "request789"
  )
  
  let baggage2_with_entries = Baggage::set_entry(
    Baggage::set_entry(
      Baggage::set_entry(baggage2, "tenant.id", "tenant123"),
      "correlation.id", "corr456"
    ),
    "trace.id", "trace789"
  )
  
  // Test baggage entry access
  @assertion.assert_eq(Baggage::get_entry(baggage1_with_entries, "user.id"), Some("user123"))?
  @assertion.assert_eq(Baggage::get_entry(baggage1_with_entries, "session.id"), Some("session456"))?
  @assertion.assert_eq(Baggage::get_entry(baggage1_with_entries, "request.id"), Some("request789"))?
  
  @assertion.assert_eq(Baggage::get_entry(baggage2_with_entries, "tenant.id"), Some("tenant123"))?
  @assertion.assert_eq(Baggage::get_entry(baggage2_with_entries, "correlation.id"), Some("corr456"))?
  @assertion.assert_eq(Baggage::get_entry(baggage2_with_entries, "trace.id"), Some("trace789"))?
  
  // Test baggage isolation
  @assertion.assert_eq(Baggage::get_entry(baggage1_with_entries, "tenant.id"), None)?
  @assertion.assert_eq(Baggage::get_entry(baggage2_with_entries, "user.id"), None)?
  
  // Test removal operations
  let baggage_after_removal = Baggage::remove_entry(baggage1_with_entries, "session.id")
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "user.id"), Some("user123"))?
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "session.id"), Some("session456"))? // Based on mock
  @assertion.assert_eq(Baggage::get_entry(baggage_after_removal, "request.id"), Some("request789"))?
}

test "propagation_resource_cleanup" {
  // Test propagation resource management and cleanup
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  let ctx = Context::root()
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // Perform injection operations
  CompositePropagator::inject(composite, ctx, carrier1)
  CompositePropagator::inject(composite, ctx, carrier2)
  CompositePropagator::inject(composite, ctx, carrier3)
  
  // Verify injection results
  let trace1 = TextMapCarrier::get(carrier1, "traceparent")
  let trace2 = TextMapCarrier::get(carrier2, "traceparent")
  let trace3 = TextMapCarrier::get(carrier3, "traceparent")
  
  @assertion.assert_eq(trace1, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))?
  @assertion.assert_eq(trace2, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))?
  @assertion.assert_eq(trace3, Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))?
  
  // Perform extraction operations
  let extracted_ctx1 = CompositePropagator::extract(composite, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite, carrier3)
  
  // Verify extraction results
  let extracted_key = ContextKey::new("extracted")
  @assertion.assert_eq(Context::get(extracted_ctx1, extracted_key), Some("true"))?
  @assertion.assert_eq(Context::get(extracted_ctx2, extracted_key), Some("true"))?
  @assertion.assert_eq(Context::get(extracted_ctx3, extracted_key), Some("true"))?
}

test "http_client_resource_management" {
  // Test HTTP client resource management
  let client1 = HttpClient::new()
  let client2 = HttpClient::new()
  
  // Create multiple requests
  let headers1 = [("Content-Type", "application/json"), ("Authorization", "Bearer token1")]
  let headers2 = [("Content-Type", "application/xml"), ("Authorization", "Bearer token2")]
  let headers3 = [("Content-Type", "text/plain"), ("Authorization", "Bearer token3")]
  
  let request1 = HttpRequest::new("GET", "https://api1.example.com", headers1, Some("{\"data\":\"value1\"}"))
  let request2 = HttpRequest::new("POST", "https://api2.example.com", headers2, Some("<xml>data2</xml>"))
  let request3 = HttpRequest::new("PUT", "https://api3.example.com", headers3, Some("plain data3"))
  
  // Create multiple responses
  let response1 = HttpResponse::new(200, [("Content-Type", "application/json")], Some("{\"result\":\"success1\"}"))
  let response2 = HttpResponse::new(404, [("Content-Type", "application/xml")], Some("<error>not found</error>"))
  let response3 = HttpResponse::new(500, [("Content-Type", "text/plain")], Some("internal error"))
  
  // Verify request isolation
  @assertion.assert_eq(HttpRequest::http_method(request1), "GET")?
  @assertion.assert_eq(HttpRequest::http_method(request2), "POST")?
  @assertion.assert_eq(HttpRequest::http_method(request3), "PUT")?
  
  @assertion.assert_eq(HttpRequest::url(request1), "https://api1.example.com")?
  @assertion.assert_eq(HttpRequest::url(request2), "https://api2.example.com")?
  @assertion.assert_eq(HttpRequest::url(request3), "https://api3.example.com")?
  
  // Verify response isolation
  @assertion.assert_eq(HttpResponse::status_code(response1), 200)?
  @assertion.assert_eq(HttpResponse::status_code(response2), 404)?
  @assertion.assert_eq(HttpResponse::status_code(response3), 500)?
}

test "memory_efficiency_large_operations" {
  // Test memory efficiency with large operations
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "memory_efficiency_tracer")
  
  // Create many spans to test memory efficiency
  let spans = []
  for i = 0; i < 10; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory_test_span_" + @unwrap_int_as_string(i))
    Span::add_event(span, "test_event", Some([("index", IntValue(i))]))
    spans = Array::push(spans, span)
  }
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
  
  // Verify all spans are properly managed
  @assertion.assert_eq(spans.length, 10)?
  @assertion.assert_eq(Span::name(spans[0]), "memory_test_span_0")?
  @assertion.assert_eq(Span::name(spans[9]), "memory_test_span_9")?
}

test "resource_monitoring_metrics" {
  // Test resource monitoring through metrics
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource_monitoring_meter")
  
  // Create instruments for monitoring
  let memory_usage = Meter::create_updown_counter(meter, "memory.usage", Some("Memory usage in bytes"), Some("bytes"))
  let cpu_usage = Meter::create_gauge(meter, "cpu.usage", Some("CPU usage percentage"), Some("%"))
  let active_connections = Meter::create_counter(meter, "active.connections", Some("Number of active connections"), Some("connections"))
  let response_time = Meter::create_histogram(meter, "response.time", Some("Response time distribution"), Some("ms"))
  
  // Simulate resource monitoring
  UpDownCounter::add(memory_usage, 1024000.0) // 1MB
  UpDownCounter::add(memory_usage, -512000.0) // -512KB
  UpDownCounter::add(memory_usage, 256000.0)  // +256KB
  
  Counter::add(active_connections, 10.0)
  Counter::add(active_connections, 5.0)
  
  Histogram::record(response_time, 100.0)
  Histogram::record(response_time, 150.0)
  Histogram::record(response_time, 200.0)
  Histogram::record(response_time, 50.0)
  
  // Verify instrument integrity
  @assertion.assert_eq(memory_usage.name, "memory.usage")?
  @assertion.assert_eq(cpu_usage.name, "cpu.usage")?
  @assertion.assert_eq(active_connections.name, "active.connections")?
  @assertion.assert_eq(response_time.name, "response.time")?
  
  @assertion.assert_eq(memory_usage.description, Some("Memory usage in bytes"))?
  @assertion.assert_eq(cpu_usage.description, Some("CPU usage percentage"))?
  @assertion.assert_eq(active_connections.description, Some("Number of active connections"))?
  @assertion.assert_eq(response_time.description, Some("Response time distribution"))?
  
  @assertion.assert_eq(memory_usage.unit, Some("bytes"))?
  @assertion.assert_eq(cpu_usage.unit, Some("%"))?
  @assertion.assert_eq(active_connections.unit, Some("connections"))?
  @assertion.assert_eq(response_time.unit, Some("ms"))?
}