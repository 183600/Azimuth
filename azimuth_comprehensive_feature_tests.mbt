// Azimuth Telemetry System - Comprehensive Feature Tests
// This file contains comprehensive test cases for various features of the telemetry system

// Test 1: Advanced Attribute Type Conversions
test "advanced attribute type conversions" {
  // Test string to int conversion
  let string_num = StringValue("42")
  match string_num {
    StringValue(v) => {
      let int_val = IntValue(v.to_int())
      match int_val {
        IntValue(i) => assert_eq(i, 42)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test string to float conversion
  let string_float = StringValue("3.14")
  match string_float {
    StringValue(v) => {
      let float_val = FloatValue(v.to_float())
      match float_val {
        FloatValue(f) => assert_eq(f, 3.14)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test bool to string conversion
  let bool_val = BoolValue(true)
  match bool_val {
    BoolValue(v) => {
      let bool_str = StringValue(v.to_string())
      match bool_str {
        StringValue(s) => assert_eq(s, "true")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 2: Span Event and Timeline Management
test "span event and timeline management" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "active")
  let span = Span::new("operation_span", Server, span_ctx)
  
  // Test adding events with timestamps
  Span::add_event(span, "operation_started", Some([("step", StringValue("1"))]))
  Span::add_event(span, "processing_data", Some([("records", IntValue(100))]))
  Span::add_event(span, "operation_completed", Some([("status", StringValue("success"))]))
  
  // Test span status transitions
  assert_eq(Span::status(span), Unset)
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_eq(Span::status(span), Ok)
  
  // Test span duration tracking
  Span::end(span)
  assert_false(Span::is_recording(span))
}

// Test 3: Metric Aggregation and Statistics
test "metric aggregation and statistics" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_meter")
  
  // Create histogram for aggregation
  let response_histogram = Meter::create_histogram(
    meter, 
    "http_response_time", 
    Some("HTTP response time histogram"), 
    Some("ms")
  )
  
  // Record multiple measurements
  let measurements = [120.5, 85.2, 200.1, 150.8, 95.3, 180.7, 110.4]
  for measurement in measurements {
    Histogram::record(response_histogram, measurement)
  }
  
  // Test counter with attributes
  let request_counter = Meter::create_counter(
    meter,
    "http_requests_total",
    Some("Total HTTP requests"),
    Some("requests")
  )
  
  // Record requests with different attributes
  let success_attrs = Attributes::new()
  Attributes::set(success_attrs, "status", StringValue("200"))
  Attributes::set(success_attrs, "method", StringValue("GET"))
  Counter::add(request_counter, 1.0, Some(success_attrs))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "status", StringValue("500"))
  Attributes::set(error_attrs, "method", StringValue("POST"))
  Counter::add(request_counter, 1.0, Some(error_attrs))
}

// Test 4: Context Propagation Across Threads
test "context propagation across threads" {
  // Create root context with baggage
  let root_ctx = Context::root()
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // Create context with baggage
  let ctx_with_baggage = Context::with_value(
    root_ctx,
    ContextKey::new("baggage"),
    updated_baggage
  )
  
  // Create span context
  let span_ctx = SpanContext::new("trace987", "span654", true, "propagated")
  let ctx_with_span = Context::with_value(
    ctx_with_baggage,
    ContextKey::new("span_context"),
    span_ctx
  )
  
  // Test context extraction
  match Context::get(ctx_with_span, ContextKey::new("baggage")) {
    Some(b) => {
      match Baggage::get_entry(b, "user.id") {
        Some(user_id) => assert_eq(user_id, "12345")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  match Context::get(ctx_with_span, ContextKey::new("span_context")) {
    Some(sc) => {
      match sc {
        SpanContext(trace_id, span_id, sampled, state) => {
          assert_eq(trace_id, "trace987")
          assert_eq(span_id, "span654")
          assert_true(sampled)
          assert_eq(state, "propagated")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 5: Log Correlation and Structured Logging
test "log correlation and structured logging" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "correlation_logger")
  
  // Create log record with correlation
  let trace_id = "trace111"
  let span_id = "span222"
  let correlation_attrs = [
    ("correlation.id", StringValue("corr-333")),
    ("user.id", StringValue("user-444")),
    ("session.id", StringValue("session-555"))
  ]
  
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("User operation completed with warnings"),
    Some(Attributes::from_array(correlation_attrs)),
    Some(1640995200000L), // timestamp
    Some(1640995201000L), // observed_timestamp
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  assert_eq(LogRecord::severity_number(log_record), Warn)
  assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(log_record), Some(span_id))
  
  // Test log emission
  Logger::emit(logger, log_record)
  
  // Create error log record
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::new()),
    Some(1640995202000L),
    Some(1640995203000L),
    Some(trace_id),
    Some("span333"),
    Some(Context::root())
  )
  
  Logger::emit(logger, error_log)
}

// Test 6: Resource Merge and Override Behavior
test "resource merge and override behavior" {
  // Create base resource
  let base_attrs = [
    ("service.name", StringValue("payment-service")),
    ("service.version", StringValue("1.2.3")),
    ("deployment.environment", StringValue("production"))
  ]
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Create override resource
  let override_attrs = [
    ("service.version", StringValue("2.0.0")), // This should override
    ("host.name", StringValue("prod-server-01")),
    ("region", StringValue("us-west-2"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  
  // Merge resources
  let merged_resource = Resource::merge(base_resource, override_resource)
  
  // Test merged attributes
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment-service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(merged_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "2.0.0") // Should be overridden
    _ => assert_true(false)
  }
  
  let environment = Resource::get_attribute(merged_resource, "deployment.environment")
  match environment {
    Some(StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false)
  }
  
  let host_name = Resource::get_attribute(merged_resource, "host.name")
  match host_name {
    Some(StringValue(host)) => assert_eq(host, "prod-server-01")
    _ => assert_true(false)
  }
  
  let region = Resource::get_attribute(merged_resource, "region")
  match region {
    Some(StringValue(r)) => assert_eq(r, "us-west-2")
    _ => assert_true(false)
  }
}

// Test 7: HTTP Client Request/Response Chain
test "http client request/response chain" {
  let client = HttpClient::new()
  
  // Create request with telemetry context
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Accept", "application/json"),
    ("X-Request-ID", "req-12345"),
    ("X-Trace-ID", "trace-67890")
  ]
  
  let request_body = "{\"operation\":\"create\",\"data\":{\"id\":123,\"name\":\"test\"}}"
  let request = HttpRequest::new(
    "POST",
    "https://api.example.com/v1/resources",
    request_headers,
    Some(request_body)
  )
  
  // Test request properties
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/v1/resources")
  
  // Simulate response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-ID", "resp-54321"),
    ("X-Processing-Time", "150")
  ]
  
  let response_body = "{\"id\":123,\"name\":\"test\",\"status\":\"created\",\"timestamp\":1640995200}"
  let response = HttpResponse::new(201, response_headers, Some(response_body))
  
  // Test response properties
  assert_eq(HttpResponse::status_code(response), 201)
  match HttpResponse::body(response) {
    Some(body) => {
      assert_true(body.contains("created"))
      assert_true(body.contains("123"))
    }
    None => assert_true(false)
  }
}

// Test 8: Composite Propagator Injection and Extraction
test "composite propagator injection and extraction" {
  // Create composite propagator
  let trace_propagator = TraceContextPropagator::new()
  let baggage_propagator = BaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Create context with span context and baggage
  let span_ctx = SpanContext::new("trace-abc123", "span-def456", true, "active")
  let baggage = Baggage::new()
  let baggage = Baggage::set_entry(baggage, "user.id", "user-789")
  let baggage = Baggage::set_entry(baggage, "session.id", "session-012")
  
  let ctx = Context::root()
  let ctx = Context::with_value(ctx, ContextKey::new("span_context"), span_ctx)
  let ctx = Context::with_value(ctx, ContextKey::new("baggage"), baggage)
  
  // Test injection
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extracted span context
  match Context::get(extracted_ctx, ContextKey::new("span_context")) {
    Some(extracted_span_ctx) => {
      match extracted_span_ctx {
        SpanContext(trace_id, span_id, sampled, state) => {
          assert_eq(trace_id, "trace-abc123")
          assert_eq(span_id, "span-def456")
          assert_true(sampled)
          assert_eq(state, "active")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Verify extracted baggage
  match Context::get(extracted_ctx, ContextKey::new("baggage")) {
    Some(extracted_baggage) => {
      match Baggage::get_entry(extracted_baggage, "user.id") {
        Some(user_id) => assert_eq(user_id, "user-789")
        None => assert_true(false)
      }
      
      match Baggage::get_entry(extracted_baggage, "session.id") {
        Some(session_id) => assert_eq(session_id, "session-012")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}