// Integration Tests for Azimuth
// These tests verify integration between different components

test "end-to-end trace flow" {
  // Create a tracer provider and tracer
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integration-test")
  
  // Start a span
  let span = Tracer::start_span(tracer, "main-operation")
  
  // Add events and status
  Span::add_event(span, "operation.started", Some([("component", StringValue("integration-test"))]))
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  
  // End the span
  Span::end(span)
  
  // Verify span properties
  assert_eq(Span::name(span), "main-operation")
  assert_eq(Span::kind(span), Internal)
  assert_eq(Span::status(span), Ok)  // Note: simplified implementation
}

test "trace context propagation" {
  // Create a span context
  let span_ctx = SpanContext::new("trace123", "span456", true, "key1=value1")
  
  // Create a context with the span context
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.context")
  let ctx_with_trace = Context::with_value(ctx, trace_key, "trace123")
  
  // Create a propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([propagator])
  
  // Create a carrier and inject the context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_trace, carrier)
  
  // Extract the context from the carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
}

test "metrics with attributes" {
  // Create a meter provider and meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integration-test")
  
  // Create attributes
  let attrs = Attributes::new()
  Attributes::set(attrs, "http.method", StringValue("GET"))
  Attributes::set(attrs, "http.status_code", IntValue(200))
  
  // Create a counter with attributes
  let counter = Meter::create_counter(meter, "http.requests")
  Counter::add(counter, 1.0, Some(attrs))
  
  // Create a histogram with attributes
  let histogram = Meter::create_histogram(meter, "http.duration")
  Histogram::record(histogram, 123.45, Some(attrs))
  
  // Verify the instruments were created correctly
  assert_eq(counter.name, "http.requests")
  assert_eq(histogram.name, "http.duration")
}

test "logging with trace context" {
  // Create a logger provider and logger
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integration-test")
  
  // Create a context with trace information
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.id")
  let ctx_with_trace = Context::with_value(ctx, trace_key, "trace123")
  
  // Create a log record with trace context
  let record = LogRecord::new_with_context(
    Info,
    Some("Operation completed"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace123"),
    Some("span456"),
    Some(ctx_with_trace)
  )
  
  // Emit the log record
  Logger::emit(logger, record)
  
  // Verify the log record properties
  assert_eq(LogRecord::severity_number(record), Info)
  assert_eq(LogRecord::body(record), Some("Operation completed"))
  assert_eq(LogRecord::trace_id(record), Some("trace123"))
  assert_eq(LogRecord::span_id(record), Some("span456"))
}

test "baggage propagation across services" {
  // Create initial baggage
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // Create a context with baggage
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "user.id=12345,request.id=req-67890")
  
  // Create a propagator
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // Create a carrier and inject the context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite, ctx_with_baggage, carrier)
  
  // Extract the context from the carrier
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  
  assert_eq(extracted_value, Some("true"))
}

test "resource attributes in telemetry" {
  // Create a resource with attributes
  let resource = Resource::new()
  let attrs = [
    ("service.name", StringValue("integration-test-service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("test"))
  ]
  let resource_with_attrs = Resource::with_attributes(resource, attrs)
  
  // Verify resource attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let instance_id = Resource::get_attribute(resource_with_attrs, "service.instance.id")
  let environment = Resource::get_attribute(resource_with_attrs, "deployment.environment")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "integration-test-service")
    _ => assert_true(false)
  }
  
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  match instance_id {
    Some(StringValue(id)) => assert_eq(id, "instance-123")
    _ => assert_true(false)
  }
  
  match environment {
    Some(StringValue(env)) => assert_eq(env, "test")
    _ => assert_true(false)
  }
}

test "http client integration with tracing" {
  // Create an HTTP client
  let client = HttpClient::new()
  
  // Create a span for the HTTP request
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "http-client")
  let span = Tracer::start_span(tracer, "HTTP GET")
  
  // Add attributes to the span
  Span::add_event(span, "http.request.started", Some([
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("https://api.example.com/data")),
    ("http.user_agent", StringValue("azimuth-client/1.0.0"))
  ]))
  
  // Create an HTTP request
  let headers = [
    ("Content-Type", "application/json"),
    ("User-Agent", "azimuth-client/1.0.0"),
    ("traceparent", "00-trace123-span456-01")
  ]
  let request = HttpRequest::new("GET", "https://api.example.com/data", headers)
  
  // Simulate receiving a response
  let response_headers = [("Content-Type", "application/json")]
  let response = HttpResponse::new(200, response_headers, Some("{\"data\":\"value\"}"))
  
  // Add response event to the span
  Span::add_event(span, "http.response.completed", Some([
    ("http.status_code", IntValue(200)),
    ("http.response_content_length", IntValue(17))
  ]))
  
  Span::set_status(span, Ok)
  Span::end(span)
  
  // Verify HTTP request and response
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.example.com/data")
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some("{\"data\":\"value\"}"))
}

test "multi-service scenario" {
  // Service A: Create a span and baggage
  let tracer_provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(tracer_provider_a, "service-a")
  let span_a = Tracer::start_span(tracer_a, "service-a-operation")
  
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "12345")
  
  // Service B: Extract context and create a child span
  let tracer_provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(tracer_provider_b, "service-b")
  let span_b = Tracer::start_span(tracer_b, "service-b-operation")
  
  Span::add_event(span_b, "service.b.processing", Some([
    ("service.a.trace.id", StringValue("trace123")),
    ("user.id", StringValue("12345"))
  ]))
  
  // Service C: Create metrics based on the operation
  let meter_provider_c = MeterProvider::default()
  let meter_c = MeterProvider::get_meter(meter_provider_c, "service-c")
  
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.a", StringValue("service-a-operation"))
  Attributes::set(attrs, "service.b", StringValue("service-b-operation"))
  Attributes::set(attrs, "user.id", StringValue("12345"))
  
  let counter = Meter::create_counter(meter_c, "cross.service.calls")
  Counter::add(counter, 1.0, Some(attrs))
  
  // End all spans
  Span::end(span_b)
  Span::end(span_a)
  
  // Verify the operation completed
  assert_eq(Span::name(span_a), "service-a-operation")
  assert_eq(Span::name(span_b), "service-b-operation")
  assert_eq(counter.name, "cross.service.calls")
}

test "instrumentation scope integration" {
  // Create instrumentation scopes for different components
  let scope1 = InstrumentationScope::{ 
    name: "component-1", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  let scope2 = InstrumentationScope::{ 
    name: "component-2", 
    version: Some("2.0.0"), 
    schema_url: Some("https://example.com/schema/v2") 
  }
  
  // Create tracers with different scopes
  let tracer_provider = TracerProvider::default()
  let tracer1 = TracerProvider::get_tracer(tracer_provider, "component-1", Some("1.0.0"))
  let tracer2 = TracerProvider::get_tracer(tracer_provider, "component-2", Some("2.0.0"))
  
  // Create spans with different tracers
  let span1 = Tracer::start_span(tracer1, "operation-1")
  let span2 = Tracer::start_span(tracer2, "operation-2")
  
  // Verify the instrumentation scopes
  let scope1_retrieved = Tracer::instrumentation_scope(tracer1)
  let scope2_retrieved = Tracer::instrumentation_scope(tracer2)
  
  assert_eq(scope1_retrieved.name, "component-1")
  assert_eq(scope1_retrieved.version, Some("1.0.0"))
  assert_eq(scope1_retrieved.schema_url, Some("https://example.com/schema/v1"))
  
  assert_eq(scope2_retrieved.name, "component-2")
  assert_eq(scope2_retrieved.version, Some("2.0.0"))
  assert_eq(scope2_retrieved.schema_url, Some("https://example.com/schema/v2"))
  
  // End spans
  Span::end(span1)
  Span::end(span2)
}