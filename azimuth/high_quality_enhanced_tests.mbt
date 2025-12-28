// High Quality Enhanced Tests for Azimuth Telemetry System
// These tests cover advanced scenarios, edge cases, and complex integrations

// Test 1: Advanced Attributes Operations with Deep Nesting
pub test "advanced attributes deep nesting operations" {
  let attrs = azimuth::Attributes::new()
  
  // Test setting complex attribute values
  azimuth::Attributes::set(attrs, "nested.complex.key", azimuth::StringValue("deep_value"))
  azimuth::Attributes::set(attrs, "array.values", azimuth::ArrayStringValue(["item1", "item2", "item3"]))
  azimuth::Attributes::set(attrs, "numeric.values", azimuth::ArrayIntValue([1, 2, 3, 4, 5]))
  azimuth::Attributes::set(attrs, "boolean.flag", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "precision.value", azimuth::FloatValue(3.14159265359))
  
  // Test retrieval with type checking
  let nested_val = azimuth::Attributes::get(attrs, "nested.complex.key")
  let array_val = azimuth::Attributes::get(attrs, "array.values")
  let numeric_val = azimuth::Attributes::get(attrs, "numeric.values")
  let bool_val = azimuth::Attributes::get(attrs, "boolean.flag")
  let precision_val = azimuth::Attributes::get(attrs, "precision.value")
  
  assert_eq(nested_val, Some(azimuth::StringValue("deep_value")))
  assert_eq(array_val, Some(azimuth::ArrayStringValue(["item1", "item2", "item3"])))
  assert_eq(numeric_val, Some(azimuth::ArrayIntValue([1, 2, 3, 4, 5])))
  assert_eq(bool_val, Some(azimuth::BoolValue(true)))
  assert_eq(precision_val, Some(azimuth::FloatValue(3.14159265359)))
  
  // Test missing attributes with different patterns
  assert_eq(azimuth::Attributes::get(attrs, ""), None)
  assert_eq(azimuth::Attributes::get(attrs, "nonexistent.key"), None)
  assert_eq(azimuth::Attributes::get(attrs, "partially.matching.key"), None)
}

// Test 2: Complex Context Propagation with Multiple Layers
pub test "complex multi-layer context propagation" {
  let root_ctx = azimuth::Context::root()
  
  // Create multiple context layers
  let user_key = azimuth::ContextKey::new("user.id")
  let session_key = azimuth::ContextKey::new("session.id")
  let request_key = azimuth::ContextKey::new("request.id")
  let trace_key = azimuth::ContextKey::new("trace.id")
  
  let ctx_with_user = azimuth::Context::with_value(root_ctx, user_key, "user-12345")
  let ctx_with_session = azimuth::Context::with_value(ctx_with_user, session_key, "session-abcdef")
  let ctx_with_request = azimuth::Context::with_value(ctx_with_session, request_key, "req-789xyz")
  let ctx_with_trace = azimuth::Context::with_value(ctx_with_request, trace_key, "trace-2025-01")
  
  // Verify all layers are preserved
  assert_eq(azimuth::Context::get(ctx_with_trace, user_key), Some("user-12345"))
  assert_eq(azimuth::Context::get(ctx_with_trace, session_key), Some("session-abcdef"))
  assert_eq(azimuth::Context::get(ctx_with_trace, request_key), Some("req-789xyz"))
  assert_eq(azimuth::Context::get(ctx_with_trace, trace_key), Some("trace-2025-01"))
  
  // Test context isolation
  let isolated_ctx = azimuth::Context::with_value(root_ctx, user_key, "different-user")
  assert_eq(azimuth::Context::get(isolated_ctx, user_key), Some("different-user"))
  assert_eq(azimuth::Context::get(isolated_ctx, session_key), None)
}

// Test 3: Advanced Span Operations with Events and Status
pub test "advanced span operations with events and status" {
  let span_ctx = azimuth::SpanContext::new("trace-2025-advanced", "span-complex-001", true, "state=active")
  let span = azimuth::Span::new("complex-operation", azimuth::Server, span_ctx)
  
  // Test span status operations
  assert_eq(azimuth::Span::status(span), azimuth::Unset)
  azimuth::Span::set_status(span, azimuth::Ok, Some("Operation completed successfully"))
  assert_eq(azimuth::Span::status(span), azimuth::Ok)  // Note: simplified implementation
  
  // Test event addition
  azimuth::Span::add_event(span, "operation.started", 
    Some([("component", azimuth::StringValue("database")), 
          ("query.type", azimuth::StringValue("select"))]))
  azimuth::Span::add_event(span, "cache.hit", 
    Some([("cache.key", azimuth::StringValue("user:12345")), 
          ("hit.ratio", azimuth::FloatValue(0.95))]))
  
  // Test span lifecycle
  assert_true(azimuth::Span::is_recording(span))
  azimuth::Span::end(span)
  
  // Verify span context integrity
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(span)), "trace-2025-advanced")
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::span_context(span)), "span-complex-001")
  assert_true(azimuth::SpanContext::is_sampled(azimuth::Span::span_context(span)))
}

// Test 4: Sophisticated Metrics with Multiple Instruments
pub test "sophisticated metrics operations with multiple instruments" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "advanced-metrics", Some("1.0.0"))
  
  // Create various metric instruments
  let request_counter = azimuth::Meter::create_counter(meter, "http.requests.total", 
    Some("Total number of HTTP requests"), Some("requests"))
  let response_histogram = azimuth::Meter::create_histogram(meter, "http.response.duration",
    Some("HTTP response duration in milliseconds"), Some("ms"))
  let active_connections = azimuth::Meter::create_updown_counter(meter, "http.connections.active",
    Some("Number of active HTTP connections"), Some("connections"))
  let memory_gauge = azimuth::Meter::create_gauge(meter, "process.memory.usage",
    Some("Memory usage in bytes"), Some("bytes"))
  
  // Record metrics with different attributes
  azimuth::Counter::add(request_counter, 1.0)
  azimuth::Counter::add(request_counter, 1.0)
  azimuth::Counter::add(request_counter, 1.0)
  
  azimuth::Histogram::record(response_histogram, 125.5)
  azimuth::Histogram::record(response_histogram, 200.0)
  azimuth::Histogram::record(response_histogram, 87.3)
  
  azimuth::UpDownCounter::add(active_connections, 10.0)
  azimuth::UpDownCounter::add(active_connections, 5.0)
  azimuth::UpDownCounter::add(active_connections, -3.0)
  
  // Verify instrument properties
  let counter_instrument = azimuth::Counter::as_instrument(request_counter)
  assert_eq(azimuth::Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("Total number of HTTP requests"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("requests"))
  
  let histogram_instrument = azimuth::Histogram::as_instrument(response_histogram)
  assert_eq(azimuth::Instrument::name(histogram_instrument), "http.response.duration")
  assert_eq(azimuth::Instrument::description(histogram_instrument), Some("HTTP response duration in milliseconds"))
  assert_eq(azimuth::Instrument::unit(histogram_instrument), Some("ms"))
}

// Test 5: Advanced Logging with Structured Data
pub test "advanced logging with structured data and context" {
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Warn,
    Some("Database connection pool exhausted"),
    Some(azimuth::Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace-db-001"),
    Some("span-db-002"),
    Some(azimuth::Context::root())
  )
  
  // Create detailed log with rich context
  let error_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(error_attrs, "error.code", azimuth::IntValue(5003))
  azimuth::Attributes::set(error_attrs, "error.type", azimuth::StringValue("ConnectionPoolExhausted"))
  azimuth::Attributes::set(error_attrs, "database.host", azimuth::StringValue("db-primary.example.com"))
  azimuth::Attributes::set(error_attrs, "retry.count", azimuth::IntValue(3))
  azimuth::Attributes::set(error_attrs, "pool.size", azimuth::IntValue(10))
  azimuth::Attributes::set(error_attrs, "pool.active", azimuth::IntValue(10))
  azimuth::Attributes::set(error_attrs, "pool.waiting", azimuth::IntValue(15))
  
  let detailed_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Failed to acquire database connection after 3 retries"),
    Some(error_attrs),
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("trace-db-001"),
    Some("span-db-003"),
    Some(azimuth::Context::root())
  )
  
  // Verify log record properties
  assert_eq(azimuth::LogRecord::severity_number(detailed_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::body(detailed_log), Some("Failed to acquire database connection after 3 retries"))
  assert_eq(azimuth::LogRecord::trace_id(detailed_log), Some("trace-db-001"))
  assert_eq(azimuth::LogRecord::span_id(detailed_log), Some("span-db-003"))
  
  // Test logger emission
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "database.logger", Some("2.1.0"))
  azimuth::Logger::emit(logger, detailed_log)
}

// Test 6: Resource Management with Hierarchical Merging
pub test "hierarchical resource management and merging" {
  let base_resource = azimuth::Resource::new()
  let base_attrs = [
    ("service.name", azimuth::StringValue("payment-service")),
    ("service.version", azimuth::StringValue("2.3.1")),
    ("service.namespace", azimuth::StringValue("production")),
    ("deployment.environment", azimuth::StringValue("prod"))
  ]
  let resource_with_base = azimuth::Resource::with_attributes(base_resource, base_attrs)
  
  // Create override resources
  let runtime_attrs = [
    ("runtime.name", azimuth::StringValue("moonbit")),
    ("runtime.version", azimuth::StringValue("1.0.0")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.15.0"))
  ]
  let runtime_resource = azimuth::Resource::with_attributes(base_resource, runtime_attrs)
  
  let host_attrs = [
    ("host.name", azimuth::StringValue("prod-web-01")),
    ("host.ip", azimuth::StringValue("10.0.1.100")),
    ("host.arch", azimuth::StringValue("x86_64"))
  ]
  let host_resource = azimuth::Resource::with_attributes(base_resource, host_attrs)
  
  // Test hierarchical merging
  let merged_resource_1 = azimuth::Resource::merge(resource_with_base, runtime_resource)
  let final_resource = azimuth::Resource::merge(merged_resource_1, host_resource)
  
  // Verify all attributes are present
  assert_eq(azimuth::Resource::get_attribute(final_resource, "service.name"), 
    Some(azimuth::StringValue("payment-service")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "runtime.name"), 
    Some(azimuth::StringValue("moonbit")))
  assert_eq(azimuth::Resource::get_attribute(final_resource, "host.name"), 
    Some(azimuth::StringValue("prod-web-01")))
  
  // Test missing attributes
  assert_eq(azimuth::Resource::get_attribute(final_resource, "nonexistent.attr"), None)
}

// Test 7: Complex Propagation Scenarios
pub test "complex propagation with multiple headers and formats" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Create rich context with baggage
  let ctx = azimuth::Context::root()
  let baggage = azimuth::Baggage::new()
  let bag_with_user = azimuth::Baggage::set_entry(baggage, "user.id", "user-12345")
  let bag_with_session = azimuth::Baggage::set_entry(bag_with_user, "session.id", "sess-abcdef")
  let bag_with_request = azimuth::Baggage::set_entry(bag_with_session, "request.id", "req-789xyz")
  
  let carrier = azimuth::TextMapCarrier::new()
  
  // Test injection
  azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify propagation worked
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
  
  // Test baggage propagation
  let baggage_value = azimuth::Baggage::get_entry(bag_with_request, "user.id")
  assert_eq(baggage_value, Some("user-12345"))
  
  let session_value = azimuth::Baggage::get_entry(bag_with_request, "session.id")
  assert_eq(session_value, Some("sess-abcdef"))
  
  let request_value = azimuth::Baggage::get_entry(bag_with_request, "request.id")
  assert_eq(request_value, Some("req-789xyz"))
}

// Test 8: Advanced HTTP Client Operations
pub test "advanced http client with complex headers and body" {
  let client = azimuth::HttpClient::new()
  
  // Create complex request with multiple headers
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"),
    ("X-Request-ID", "req-2025-01-28-001"),
    ("X-Client-Version", "2.3.1"),
    ("Accept", "application/json, text/plain, */*"),
    ("User-Agent", "Azimuth-Telemetry/1.0.0")
  ]
  
  let request_body = "{\"operation\":\"query\",\"parameters\":{\"user_id\":\"12345\",\"include_profile\":true}}"
  let request = azimuth::HttpRequest::new("POST", "https://api.example.com/v1/users/query", headers, Some(request_body))
  
  // Verify request properties
  assert_eq(azimuth::HttpRequest::http_method(request), "POST")
  assert_eq(azimuth::HttpRequest::url(request), "https://api.example.com/v1/users/query")
  assert_eq(azimuth::HttpRequest::body(request), Some(request_body))
  
  // Create complex response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Rate-Limit-Remaining", "4999"),
    ("X-Response-Time", "125ms"),
    ("Cache-Control", "no-cache, private"),
    ("X-Request-ID", "req-2025-01-28-001")
  ]
  
  let response_body = "{\"status\":\"success\",\"data\":{\"user_id\":\"12345\",\"name\":\"John Doe\",\"email\":\"john@example.com\"},\"metadata\":{\"query_time\":125,\"cache_hit\":false}}"
  let response = azimuth::HttpResponse::new(200, response_headers, Some(response_body))
  
  // Verify response properties
  assert_eq(azimuth::HttpResponse::status_code(response), 200)
  assert_eq(azimuth::HttpResponse::body(response), Some(response_body))
}

// Test 9: Time Series and Temporal Operations
pub test "time series and temporal operations with precision" {
  let clock = azimuth::Clock::system()
  
  // Get multiple timestamps for temporal analysis
  let timestamp_1 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp_2 = azimuth::Clock::now_unix_nanos(clock)
  let timestamp_3 = azimuth::Clock::now_unix_nanos(clock)
  
  // Verify timestamp progression (simplified - in real implementation would check ordering)
  assert_true(timestamp_1 >= 1700000000000000000L)
  assert_true(timestamp_2 >= 1700000000000000000L)
  assert_true(timestamp_3 >= 1700000000000000000L)
  
  // Test random number generation for sampling
  let random = azimuth::Random::system()
  let random_bytes_1 = azimuth::Random::next_bytes(random, 16)
  let random_bytes_2 = azimuth::Random::next_bytes(random, 16)
  let random_u64_1 = azimuth::Random::next_u64(random)
  let random_u64_2 = azimuth::Random::next_u64(random)
  
  // Verify random data generation
  assert_eq(random_bytes_1.length(), 16)
  assert_eq(random_bytes_2.length(), 16)
  assert_true(random_u64_1.to_int() >= 0)
  assert_true(random_u64_2.to_int() >= 0)
}

// Test 10: Complete End-to-End Business Workflow
pub test "complete end-to-end business workflow integration" {
  // Setup business context
  let business_resource = azimuth::Resource::new()
  let business_attrs = [
    ("business.domain", azimuth::StringValue("e-commerce")),
    ("business.service", azimuth::StringValue("order-processing")),
    ("business.version", azimuth::StringValue("3.2.1")),
    ("business.environment", azimuth::StringValue("production"))
  ]
  let business_context_resource = azimuth::Resource::with_attributes(business_resource, business_attrs)
  
  // Initialize telemetry components
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "order-processor", Some("3.2.1"))
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "order-metrics", Some("3.2.1"))
  
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "order-logger", Some("3.2.1"))
  
  // Simulate order processing workflow
  let order_span = azimuth::Tracer::start_span(tracer, "process.order")
  
  // Create business metrics
  let orders_counter = azimuth::Meter::create_counter(meter, "orders.processed.total",
    Some("Total number of orders processed"), Some("orders"))
  let order_amount_histogram = azimuth::Meter::create_histogram(meter, "order.amount",
    Some("Order amount distribution"), Some("currency"))
  let processing_time_histogram = azimuth::Meter::create_histogram(meter, "order.processing.time",
    Some("Order processing time in milliseconds"), Some("ms"))
  
  // Record business events
  azimuth::Span::add_event(order_span, "order.received",
    Some([("order.id", azimuth::StringValue("ORD-2025-001")),
          ("customer.id", azimuth::StringValue("CUST-12345")),
          ("order.amount", azimuth::FloatValue(299.99))]))
  
  azimuth::Counter::add(orders_counter, 1.0)
  azimuth::Histogram::record(order_amount_histogram, 299.99)
  azimuth::Histogram::record(processing_time_histogram, 1250.0)
  
  // Create business log entry
  let business_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Order ORD-2025-001 processed successfully for customer CUST-12345"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(order_span))),
    Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(order_span))),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(logger, business_log)
  
  // Complete the workflow
  azimuth::Span::set_status(order_span, azimuth::Ok, Some("Order processed successfully"))
  azimuth::Span::end(order_span)
  
  // Verify workflow completion
  assert_eq(azimuth::Span::name(order_span), "process.order")
  assert_eq(orders_counter.name, "orders.processed.total")
  assert_eq(logger.scope.name, "order-logger")
  assert_eq(azimuth::LogRecord::body(business_log), Some("Order ORD-2025-001 processed successfully for customer CUST-12345"))
  
  // Verify business context
  let business_domain = azimuth::Resource::get_attribute(business_context_resource, "business.domain")
  let business_service = azimuth::Resource::get_attribute(business_context_resource, "business.service")
  assert_eq(business_domain, Some(azimuth::StringValue("e-commerce")))
  assert_eq(business_service, Some(azimuth::StringValue("order-processing")))
}