// Azimuth Telemetry System - Advanced Telemetry Features Tests
// This file contains test cases for advanced telemetry features

// Test 1: Distributed Tracing with Context Propagation
test "distributed tracing with context propagation" {
  // Create root span context
  let trace_id = "1234567890abcdef1234567890abcdef"
  let root_span_id = "1111111111111111"
  let root_context = SpanContext::new(trace_id, root_span_id, true, "root")
  
  // Create root span
  let root_span = Span::new("root_operation", Server, root_context)
  Span::add_event(root_span, "operation_started", Some([
    ("operation.type", StringValue("distributed")),
    ("service.name", StringValue("auth_service"))
  ]))
  
  // Create child span with context propagation
  let child_span_id = "2222222222222222"
  let child_context = SpanContext::new(trace_id, child_span_id, true, "child")
  let child_span = Span::new("user_validation", Internal, child_context)
  Span::set_parent(child_span, root_span)
  
  // Add attributes and events to child span
  Span::add_event(child_span, "validation_started", Some([
    ("user.id", StringValue("user123")),
    ("validation.type", StringValue("authentication"))
  ]))
  
  // Create grandchild span
  let grandchild_span_id = "3333333333333333"
  let grandchild_context = SpanContext::new(trace_id, grandchild_span_id, true, "grandchild")
  let grandchild_span = Span::new("database_query", Client, grandchild_context)
  Span::set_parent(grandchild_span, child_span)
  
  Span::add_event(grandchild_span, "query_executed", Some([
    ("db.statement", StringValue("SELECT * FROM users WHERE id = ?")),
    ("db.type", StringValue("postgresql"))
  ]))
  
  // End spans in reverse order
  Span::end(grandchild_span)
  Span::end(child_span)
  Span::end(root_span)
  
  // Verify trace consistency
  assert_eq(SpanContext::trace_id(root_context), trace_id)
  assert_eq(SpanContext::trace_id(child_context), trace_id)
  assert_eq(SpanContext::trace_id(grandchild_context), trace_id)
}

// Test 2: Metrics Aggregation and Percentiles
test "metrics aggregation and percentiles" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance_meter")
  
  // Create histogram for response times
  let response_time_histogram = Meter::create_histogram(
    meter, 
    "http.server.response_time", 
    Some("HTTP server response time"), 
    Some("ms")
  )
  
  // Record various response times
  let response_times = [10, 25, 50, 75, 100, 150, 200, 300, 500, 1000]
  for time in response_times {
    Histogram::record(response_time_histogram, time.to_float(), Some(Attributes::new()))
  }
  
  // Create counter for requests
  let request_counter = Meter::create_counter(
    meter,
    "http.server.requests",
    Some("Total HTTP requests"),
    Some("count")
  )
  
  // Record requests with different status codes
  Counter::add(request_counter, 80.0, Some(Attributes::with([("status.code", StringValue("200"))])))
  Counter::add(request_counter, 15.0, Some(Attributes::with([("status.code", StringValue("404"))])))
  Counter::add(request_counter, 5.0, Some(Attributes::with([("status.code", StringValue("500"))])))
  
  // Create gauge for active connections
  let active_connections_gauge = Meter::create_gauge(
    meter,
    "http.server.active_connections",
    Some("Active HTTP connections"),
    Some("connections")
  )
  
  // Set gauge values
  Gauge::set(active_connections_gauge, 25.0)
  Gauge::set(active_connections_gauge, 30.0)
  Gauge::set(active_connections_gauge, 15.0)
  
  // Verify metric instruments
  let histogram_instrument = Histogram::as_instrument(response_time_histogram)
  assert_eq(Instrument::name(histogram_instrument), "http.server.response_time")
  
  let counter_instrument = Counter::as_instrument(request_counter)
  assert_eq(Instrument::name(counter_instrument), "http.server.requests")
  
  let gauge_instrument = Gauge::as_instrument(active_connections_gauge)
  assert_eq(Instrument::name(gauge_instrument), "http.server.active_connections")
}

// Test 3: Advanced Logging with Correlation
test "advanced logging with correlation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "correlation_logger")
  
  // Create context with correlation ID
  let correlation_id = "req-123456789"
  let context = Context::with_value(
    Context::root(),
    ContextKey::new("correlation_id"),
    correlation_id
  )
  
  // Create log record with correlation
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Processing user request"),
    Some(Attributes::with([
      ("user.id", StringValue("user123")),
      ("request.path", StringValue("/api/users")),
      ("request.method", StringValue("GET"))
    ])),
    Some(1640995200000L), // timestamp
    Some(1640995200100L), // observed timestamp
    Some("1234567890abcdef1234567890abcdef"), // trace ID
    Some("1111111111111111"), // span ID
    Some(context)
  )
  
  // Emit log record
  Logger::emit(logger, log_record)
  
  // Create error log record
  let error_log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::with([
      ("error.type", StringValue("ConnectionError")),
      ("error.message", StringValue("Connection timeout")),
      ("db.host", StringValue("db.example.com")),
      ("db.port", IntValue(5432))
    ])),
    Some(1640995200200L),
    Some(1640995200250L),
    Some("1234567890abcdef1234567890abcdef"),
    Some("2222222222222222"),
    Some(context)
  )
  
  Logger::emit(logger, error_log_record)
  
  // Verify log records
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::severity_number(error_log_record), Error)
  assert_eq(LogRecord::trace_id(log_record), Some("1234567890abcdef1234567890abcdef"))
  assert_eq(LogRecord::span_id(log_record), Some("1111111111111111"))
}

// Test 4: Baggage Propagation Across Services
test "baggage propagation across services" {
  // Create initial baggage
  let baggage = Baggage::new()
  
  // Add baggage entries
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "sess456")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant789")
  
  // Simulate service boundary crossing
  let service1_baggage = baggage_with_tenant
  
  // Service 1 adds its own baggage
  let service1_enhanced = Baggage::set_entry(service1_baggage, "service1.version", "1.2.3")
  
  // Service 2 receives and adds baggage
  let service2_baggage = service1_enhanced
  let service2_enhanced = Baggage::set_entry(service2_baggage, "service2.region", "us-west-2")
  
  // Service 3 receives and adds baggage
  let service3_baggage = service2_enhanced
  let service3_enhanced = Baggage::set_entry(service3_baggage, "service3.instance", "i-1234567890abcdef")
  
  // Verify baggage propagation
  let user_id = Baggage::get_entry(service3_enhanced, "user.id")
  match user_id {
    Some(id) => assert_eq(id, "user123")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(service3_enhanced, "session.id")
  match session_id {
    Some(id) => assert_eq(id, "sess456")
    None => assert_true(false)
  }
  
  let service1_version = Baggage::get_entry(service3_enhanced, "service1.version")
  match service1_version {
    Some(version) => assert_eq(version, "1.2.3")
    None => assert_true(false)
  }
  
  // Test baggage removal
  let final_baggage = Baggage::remove_entry(service3_enhanced, "session.id")
  let removed_session = Baggage::get_entry(final_baggage, "session.id")
  match removed_session {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Resource Detection and Merging
test "resource detection and merging" {
  // Create base resource with service information
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("payment_service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("payment-123"))
  ])
  
  // Create resource with host information
  let host_resource = Resource::with_attributes(Resource::new(), [
    ("host.name", StringValue("prod-web-01")),
    ("host.type", StringValue("c5.large")),
    ("host.image.id", StringValue("ami-12345678"))
  ])
  
  // Create resource with telemetry information
  let telemetry_resource = Resource::with_attributes(Resource::new(), [
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("1.0.0")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ])
  
  // Merge resources
  let merged_resource = Resource::merge(
    Resource::merge(base_resource, host_resource),
    telemetry_resource
  )
  
  // Verify merged attributes
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment_service")
    _ => assert_true(false)
  }
  
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "prod-web-01")
    _ => assert_true(false)
  }
  
  let sdk_name = Resource::get_attribute(merged_resource, "telemetry.sdk.name")
  match sdk_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth")
    _ => assert_true(false)
  }
  
  // Test resource override
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.1.1")) // Newer version
  ])
  
  let final_resource = Resource::merge(merged_resource, override_resource)
  let updated_version = Resource::get_attribute(final_resource, "service.version")
  match updated_version {
    Some(StringValue(version)) => assert_eq(version, "2.1.1")
    _ => assert_true(false)
  }
}

// Test 6: Span Links for Causal Relationships
test "span links for causal relationships" {
  // Create parent span
  let parent_context = SpanContext::new("trace123", "span111", true, "parent")
  let parent_span = Span::new("parent_operation", Server, parent_context)
  
  // Create concurrent child spans
  let child1_context = SpanContext::new("trace123", "span222", true, "child1")
  let child1_span = Span::new("concurrent_operation_1", Internal, child1_context)
  
  let child2_context = SpanContext::new("trace123", "span333", true, "child2")
  let child2_span = Span::new("concurrent_operation_2", Internal, child2_context)
  
  // Create a span that links to both concurrent operations
  let linking_context = SpanContext::new("trace123", "span444", true, "linking")
  let linking_span = Span::new("aggregation_operation", Internal, linking_context)
  
  // Add links from the aggregation span to the concurrent operations
  Span::add_link(linking_span, child1_context, Some(Attributes::with([
    ("link.type", StringValue("follows_from")),
    ("operation", StringValue("concurrent_operation_1"))
  ])))
  
  Span::add_link(linking_span, child2_context, Some(Attributes::with([
    ("link.type", StringValue("follows_from")),
    ("operation", StringValue("concurrent_operation_2"))
  ])))
  
  // Add events to indicate processing
  Span::add_event(linking_span, "aggregation_started", Some([
    ("input.count", IntValue(2))
  ]))
  
  // End all spans
  Span::end(child1_span)
  Span::end(child2_span)
  Span::add_event(linking_span, "aggregation_completed", Some([
    ("result.status", StringValue("success"))
  ]))
  Span::end(linking_span)
  Span::end(parent_span)
  
  // Verify span relationships
  assert_eq(Span::name(parent_span), "parent_operation")
  assert_eq(Span::name(child1_span), "concurrent_operation_1")
  assert_eq(Span::name(child2_span), "concurrent_operation_2")
  assert_eq(Span::name(linking_span), "aggregation_operation")
}

// Test 7: Sampling Strategies
test "sampling strategies" {
  // Test parent-based sampling
  let parent_context = SpanContext::new("trace123", "span111", true, "parent")
  let parent_span = Span::new("parent_operation", Server, parent_context)
  
  let child_context = SpanContext::new("trace123", "span222", true, "child")
  let child_span = Span::new("child_operation", Internal, child_context)
  
  // Child should be sampled if parent is sampled
  assert_true(SpanContext::is_sampled(parent_context))
  assert_true(SpanContext::is_sampled(child_context))
  
  // Test trace ID ratio sampling
  let trace_id_ratio_sampler = TraceIdRatioBasedSampler::new(0.5) // 50% sampling
  
  let sampled_trace = "1234567890abcdef1234567890abcdef" // Should be sampled
  let not_sampled_trace = "fedcba0987654321fedcba0987654321" // Should not be sampled
  
  let sampled_context = SpanContext::new(sampled_trace, "span333", true, "sampled")
  let not_sampled_context = SpanContext::new(not_sampled_trace, "span444", false, "not_sampled")
  
  assert_true(SpanContext::is_sampled(sampled_context))
  assert_false(SpanContext::is_sampled(not_sampled_context))
  
  // Test always-on sampler
  let always_on_sampler = AlwaysOnSampler::new()
  let always_on_context = SpanContext::new("trace456", "span555", true, "always_on")
  assert_true(SpanContext::is_sampled(always_on_context))
  
  // Test always-off sampler
  let always_off_sampler = AlwaysOffSampler::new()
  let always_off_context = SpanContext::new("trace789", "span666", false, "always_off")
  assert_false(SpanContext::is_sampled(always_off_context))
}

// Test 8: Custom Instrumentation
test "custom instrumentation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "custom_instrumentation")
  
  // Create custom counter with custom attributes
  let custom_counter = Meter::create_counter(
    meter,
    "custom.business.metric",
    Some("Custom business metric"),
    Some("operations")
  )
  
  // Record custom metrics with business context
  Counter::add(custom_counter, 1.0, Some(Attributes::with([
    ("business.unit", StringValue("payments")),
    ("operation.type", StringValue("refund")),
    ("currency", StringValue("USD")),
    ("amount.category", StringValue("small"))
  ])))
  
  Counter::add(custom_counter, 1.0, Some(Attributes::with([
    ("business.unit", StringValue("payments")),
    ("operation.type", StringValue("charge")),
    ("currency", StringValue("EUR")),
    ("amount.category", StringValue("large"))
  ])))
  
  // Create custom histogram for business timing
  let business_histogram = Meter::create_histogram(
    meter,
    "business.operation.duration",
    Some("Business operation duration"),
    Some("ms")
  )
  
  // Record business operation durations
  Histogram::record(business_histogram, 150.0, Some(Attributes::with([
    ("operation.name", StringValue("payment_processing")),
    ("payment.method", StringValue("credit_card"))
  ])))
  
  Histogram::record(business_histogram, 85.0, Some(Attributes::with([
    ("operation.name", StringValue("payment_processing")),
    ("payment.method", StringValue("paypal"))
  ])))
  
  // Create custom gauge for resource utilization
  let resource_gauge = Meter::create_gauge(
    meter,
    "resource.utilization.percentage",
    Some("Resource utilization percentage"),
    Some("%")
  )
  
  // Set resource utilization values
  Gauge::set(resource_gauge, 75.5, Some(Attributes::with([
    ("resource.type", StringValue("cpu")),
    ("instance.id", StringValue("i-1234567890abcdef"))
  ])))
  
  Gauge::set(resource_gauge, 60.2, Some(Attributes::with([
    ("resource.type", StringValue("memory")),
    ("instance.id", StringValue("i-1234567890abcdef"))
  ])))
}

// Test 9: Span Status and Error Handling
test "span status and error handling" {
  // Create span for successful operation
  let success_context = SpanContext::new("trace123", "span111", true, "success")
  let success_span = Span::new("successful_operation", Server, success_context)
  
  // Set success status
  Span::set_status(success_span, Ok, Some("Operation completed successfully"))
  Span::add_event(success_span, "operation_completed", Some([
    ("result.status", StringValue("success"))
  ]))
  Span::end(success_span)
  
  // Create span for failed operation
  let error_context = SpanContext::new("trace123", "span222", true, "error")
  let error_span = Span::new("failed_operation", Server, error_context)
  
  // Record error
  Span::record_error(error_span, "Database connection failed", Some(Attributes::with([
    ("error.type", StringValue("ConnectionError")),
    ("error.code", StringValue("DB_CONN_FAILED"))
  ])))
  
  // Set error status
  Span::set_status(error_span, Error, Some("Operation failed due to database error"))
  Span::add_event(error_span, "operation_failed", Some([
    ("error.message", StringValue("Database connection failed"))
  ]))
  Span::end(error_span)
  
  // Verify span status
  assert_eq(Span::status(success_span), Ok)
  assert_eq(Span::status(error_span), Error)
  
  // Create span with exception
  let exception_context = SpanContext::new("trace123", "span333", true, "exception")
  let exception_span = Span::new("exception_operation", Server, exception_context)
  
  // Record exception
  Span::record_exception(
    exception_span,
    "NullPointerException",
    Some("Attempted to access null reference"),
    Some([
      ("stack_trace", StringValue("at com.example.Service.process(Service.java:123)")),
      ("exception.type", StringValue("RuntimeException"))
    ])
  )
  
  Span::set_status(exception_span, Error, Some("Unhandled exception"))
  Span::end(exception_span)
  
  assert_eq(Span::status(exception_span), Error)
}

// Test 10: Context Propagation with Custom Propagators
test "context propagation with custom propagators" {
  // Create custom propagator
  let custom_propagator = CustomPropagator::new()
  
  // Create context with trace information
  let trace_context = SpanContext::new("trace123", "span111", true, "custom")
  let context = Context::with_value(
    Context::root(),
    ContextKey::new("trace_context"),
    trace_context
  )
  
  // Inject context into carrier
  let carrier = {}
  let injected_carrier = Propagator::inject(custom_propagator, context, carrier)
  
  // Verify injected headers
  let trace_header = Carrier::get(injected_carrier, "x-trace-id")
  match trace_header {
    Some(header) => assert_eq(header, "trace123")
    None => assert_true(false)
  }
  
  let span_header = Carrier::get(injected_carrier, "x-span-id")
  match span_header {
    Some(header) => assert_eq(header, "span111")
    None => assert_true(false)
  }
  
  let sampled_header = Carrier::get(injected_carrier, "x-sampled")
  match sampled_header {
    Some(header) => assert_eq(header, "true")
    None => assert_true(false)
  }
  
  // Extract context from carrier
  let extracted_context = Propagator::extract(custom_propagator, injected_carrier)
  let extracted_trace_context = Context::get(extracted_context, ContextKey::new("trace_context"))
  
  match extracted_trace_context {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), "trace123")
      assert_eq(SpanContext::span_id(ctx), "span111")
      assert_true(SpanContext::is_sampled(ctx))
    }
    None => assert_true(false)
  }
  
  // Test baggage propagation
  let baggage_context = Baggage::set_entry(Baggage::new(), "user.id", "user123")
  let context_with_baggage = Context::with_value(
    extracted_context,
    ContextKey::new("baggage"),
    baggage_context
  )
  
  let carrier_with_baggage = Propagator::inject(custom_propagator, context_with_baggage, {})
  let baggage_header = Carrier::get(carrier_with_baggage, "x-baggage")
  
  match baggage_header {
    Some(header) => assert_true(header.contains("user.id=user123"))
    None => assert_true(false)
  }
}