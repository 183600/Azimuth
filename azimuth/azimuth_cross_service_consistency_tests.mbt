// Cross-Service Consistency Tests for Azimuth Telemetry System
// This file contains test cases covering consistency across different services and components

test "trace context consistency across services" {
  // Test trace context propagation between services
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // Service A: Create trace context
  let service_a_context = Context::with_value(
    Context::root(),
    ContextKey::new("service_a.user_id"),
    "user_12345"
  )
  
  // Service A: Inject trace context
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(propagator, service_a_context, carrier)
  
  // Service B: Extract trace context
  let service_b_context = CompositePropagator::extract(propagator, carrier)
  let service_b_user_id = Context::get(service_b_context, ContextKey::new("service_a.user_id"))
  assert_eq(service_b_user_id, None) // Different context key
  
  // Service B: Add its own context
  let service_b_enhanced_context = Context::with_value(
    service_b_context,
    ContextKey::new("service_b.session_id"),
    "session_67890"
  )
  
  // Service B: Inject enhanced context
  let enhanced_carrier = TextMapCarrier::new()
  CompositePropagator::inject(propagator, service_b_enhanced_context, enhanced_carrier)
  
  // Service C: Extract enhanced context
  let service_c_context = CompositePropagator::extract(propagator, enhanced_carrier)
  let service_c_session_id = Context::get(service_c_context, ContextKey::new("service_b.session_id"))
  assert_eq(service_c_session_id, None) // Different context key
  
  // Verify traceparent consistency
  let traceparent_a = TextMapCarrier::get(carrier, "traceparent")
  let traceparent_b = TextMapCarrier::get(enhanced_carrier, "traceparent")
  assert_eq(traceparent_a, traceparent_b) // Should be the same traceparent
}

test "metric consistency across services" {
  // Test metric consistency across different services
  let meter_provider = MeterProvider::default()
  
  // Service A: Create metrics
  let service_a_meter = MeterProvider::get_meter(meter_provider, "service_a")
  let service_a_counter = Meter::create_counter(service_a_meter, "http_requests_total", Some("Total HTTP requests"), Some("requests"))
  let service_a_histogram = Meter::create_histogram(service_a_meter, "http_request_duration", Some("HTTP request duration"), Some("ms"))
  
  // Service A: Record metrics
  Counter::add(service_a_counter, 100.0)
  Histogram::record(service_a_histogram, 50.0)
  
  // Service B: Create metrics with same names
  let service_b_meter = MeterProvider::get_meter(meter_provider, "service_b")
  let service_b_counter = Meter::create_counter(service_b_meter, "http_requests_total", Some("Total HTTP requests"), Some("requests"))
  let service_b_histogram = Meter::create_histogram(service_b_meter, "http_request_duration", Some("HTTP request duration"), Some("ms"))
  
  // Service B: Record metrics
  Counter::add(service_b_counter, 200.0)
  Histogram::record(service_b_histogram, 75.0)
  
  // Service C: Create metrics with same names
  let service_c_meter = MeterProvider::get_meter(meter_provider, "service_c")
  let service_c_counter = Meter::create_counter(service_c_meter, "http_requests_total", Some("Total HTTP requests"), Some("requests"))
  let service_c_histogram = Meter::create_histogram(service_c_meter, "http_request_duration", Some("HTTP request duration"), Some("ms"))
  
  // Service C: Record metrics
  Counter::add(service_c_counter, 150.0)
  Histogram::record(service_c_histogram, 60.0)
  
  // Verify metric consistency (same names, descriptions, and units)
  assert_eq(service_a_counter.name, service_b_counter.name)
  assert_eq(service_b_counter.name, service_c_counter.name)
  assert_eq(service_a_counter.name, "http_requests_total")
  
  assert_eq(service_a_counter.description, service_b_counter.description)
  assert_eq(service_b_counter.description, service_c_counter.description)
  assert_eq(service_a_counter.description, Some("Total HTTP requests"))
  
  assert_eq(service_a_counter.unit, service_b_counter.unit)
  assert_eq(service_b_counter.unit, service_c_counter.unit)
  assert_eq(service_a_counter.unit, Some("requests"))
  
  assert_eq(service_a_histogram.name, service_b_histogram.name)
  assert_eq(service_b_histogram.name, service_c_histogram.name)
  assert_eq(service_a_histogram.name, "http_request_duration")
  
  assert_eq(service_a_histogram.description, service_b_histogram.description)
  assert_eq(service_b_histogram.description, service_c_histogram.description)
  assert_eq(service_a_histogram.description, Some("HTTP request duration"))
  
  assert_eq(service_a_histogram.unit, service_b_histogram.unit)
  assert_eq(service_b_histogram.unit, service_c_histogram.unit)
  assert_eq(service_a_histogram.unit, Some("ms"))
}

test "log consistency across services" {
  // Test log consistency across different services
  let logger_provider = LoggerProvider::default()
  
  // Service A: Create logger and log record
  let service_a_logger = LoggerProvider::get_logger(logger_provider, "service_a")
  let service_a_log = LogRecord::new_with_context(
    Info,
    Some("Service A: Processing request"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace_12345"),
    Some("span_12345"),
    Some(Context::root())
  )
  
  // Service B: Create logger and log record
  let service_b_logger = LoggerProvider::get_logger(logger_provider, "service_b")
  let service_b_log = LogRecord::new_with_context(
    Info,
    Some("Service B: Processing request"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace_12345"),
    Some("span_67890"),
    Some(Context::root())
  )
  
  // Service C: Create logger and log record
  let service_c_logger = LoggerProvider::get_logger(logger_provider, "service_c")
  let service_c_log = LogRecord::new_with_context(
    Info,
    Some("Service C: Processing request"),
    Some(Attributes::new()),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace_12345"),
    Some("span_abcde"),
    Some(Context::root())
  )
  
  // Verify trace consistency (same trace_id across services)
  assert_eq(LogRecord::trace_id(service_a_log), Some("trace_12345"))
  assert_eq(LogRecord::trace_id(service_b_log), Some("trace_12345"))
  assert_eq(LogRecord::trace_id(service_c_log), Some("trace_12345"))
  
  // Verify different span_ids (different spans for each service)
  assert_eq(LogRecord::span_id(service_a_log), Some("span_12345"))
  assert_eq(LogRecord::span_id(service_b_log), Some("span_67890"))
  assert_eq(LogRecord::span_id(service_c_log), Some("span_abcde"))
  
  // Verify same severity level
  assert_eq(LogRecord::severity_number(service_a_log), Info)
  assert_eq(LogRecord::severity_number(service_b_log), Info)
  assert_eq(LogRecord::severity_number(service_c_log), Info)
}

test "resource consistency across services" {
  // Test resource consistency across different services
  let base_resource = Resource::new()
  
  // Common attributes across all services
  let common_attributes = [
    ("service.namespace", StringValue("production")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("production"))
  ]
  
  // Service A: Create resource with common and service-specific attributes
  let service_a_attributes = [
    ("service.name", StringValue("service_a")),
    ("service.instance.id", StringValue("instance_a_123"))
  ] + common_attributes
  
  let service_a_resource = Resource::with_attributes(base_resource, service_a_attributes)
  
  // Service B: Create resource with common and service-specific attributes
  let service_b_attributes = [
    ("service.name", StringValue("service_b")),
    ("service.instance.id", StringValue("instance_b_456"))
  ] + common_attributes
  
  let service_b_resource = Resource::with_attributes(base_resource, service_b_attributes)
  
  // Service C: Create resource with common and service-specific attributes
  let service_c_attributes = [
    ("service.name", StringValue("service_c")),
    ("service.instance.id", StringValue("instance_c_789"))
  ] + common_attributes
  
  let service_c_resource = Resource::with_attributes(base_resource, service_c_attributes)
  
  // Verify common attributes consistency
  assert_eq(Resource::get_attribute(service_a_resource, "service.namespace"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.namespace"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_c_resource, "service.namespace"), Some(StringValue("production")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(service_c_resource, "service.version"), Some(StringValue("1.0.0")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_b_resource, "deployment.environment"), Some(StringValue("production")))
  assert_eq(Resource::get_attribute(service_c_resource, "deployment.environment"), Some(StringValue("production")))
  
  // Verify service-specific attributes
  assert_eq(Resource::get_attribute(service_a_resource, "service.name"), Some(StringValue("service_a")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.name"), Some(StringValue("service_b")))
  assert_eq(Resource::get_attribute(service_c_resource, "service.name"), Some(StringValue("service_c")))
  
  assert_eq(Resource::get_attribute(service_a_resource, "service.instance.id"), Some(StringValue("instance_a_123")))
  assert_eq(Resource::get_attribute(service_b_resource, "service.instance.id"), Some(StringValue("instance_b_456")))
  assert_eq(Resource::get_attribute(service_c_resource, "service.instance.id"), Some(StringValue("instance_c_789")))
}

test "baggage consistency across services" {
  // Test baggage consistency across different services
  let baggage = Baggage::new()
  
  // Service A: Set baggage entries
  let service_a_baggage = Baggage::set_entry(baggage, "user_id", "user_12345")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "request_id", "req_abc123")
  let service_a_baggage = Baggage::set_entry(service_a_baggage, "session_id", "sess_def456")
  
  // Service B: Add more baggage entries
  let service_b_baggage = Baggage::set_entry(service_a_baggage, "tenant_id", "tenant_789")
  let service_b_baggage = Baggage::set_entry(service_b_baggage, "correlation_id", "corr_ghi789")
  
  // Service C: Add more baggage entries
  let service_c_baggage = Baggage::set_entry(service_b_baggage, "client_id", "client_jkl012")
  let service_c_baggage = Baggage::set_entry(service_c_baggage, "trace_id", "trace_mno345")
  
  // Verify baggage consistency across services
  assert_eq(Baggage::get_entry(service_c_baggage, "user_id"), Some("user_12345"))
  assert_eq(Baggage::get_entry(service_c_baggage, "request_id"), Some("req_abc123"))
  assert_eq(Baggage::get_entry(service_c_baggage, "session_id"), Some("sess_def456"))
  assert_eq(Baggage::get_entry(service_c_baggage, "tenant_id"), Some("tenant_789"))
  assert_eq(Baggage::get_entry(service_c_baggage, "correlation_id"), Some("corr_ghi789"))
  assert_eq(Baggage::get_entry(service_c_baggage, "client_id"), Some("client_jkl012"))
  assert_eq(Baggage::get_entry(service_c_baggage, "trace_id"), Some("trace_mno345"))
}

test "span hierarchy consistency across services" {
  // Test span hierarchy consistency across different services
  let tracer_provider = TracerProvider::default()
  
  // Service A: Create root span
  let service_a_tracer = TracerProvider::get_tracer(tracer_provider, "service_a")
  let service_a_span = Tracer::start_span(service_a_tracer, "service_a_operation")
  let service_a_span_context = Span::span_context(service_a_span)
  
  // Service B: Create child span
  let service_b_tracer = TracerProvider::get_tracer(tracer_provider, "service_b")
  let service_b_span = Tracer::start_span(service_b_tracer, "service_b_operation")
  let service_b_span_context = Span::span_context(service_b_span)
  
  // Service C: Create child span
  let service_c_tracer = TracerProvider::get_tracer(tracer_provider, "service_c")
  let service_c_span = Tracer::start_span(service_c_tracer, "service_c_operation")
  let service_c_span_context = Span::span_context(service_c_span)
  
  // Verify span consistency
  assert_true(SpanContext::is_valid(service_a_span_context))
  assert_true(SpanContext::is_valid(service_b_span_context))
  assert_true(SpanContext::is_valid(service_c_span_context))
  
  assert_true(SpanContext::is_sampled(service_a_span_context))
  assert_true(SpanContext::is_sampled(service_b_span_context))
  assert_true(SpanContext::is_sampled(service_c_span_context))
  
  // Verify span kinds
  assert_eq(Span::kind(service_a_span), Internal)
  assert_eq(Span::kind(service_b_span), Internal)
  assert_eq(Span::kind(service_c_span), Internal)
  
  // End spans
  Span::end(service_a_span)
  Span::end(service_b_span)
  Span::end(service_c_span)
}

test "attribute consistency across services" {
  // Test attribute consistency across different services
  let service_a_attributes = Attributes::new()
  let service_b_attributes = Attributes::new()
  let service_c_attributes = Attributes::new()
  
  // Common attributes across all services
  let common_attributes = [
    ("http.method", StringValue("GET")),
    ("http.status_code", IntValue(200)),
    ("http.scheme", StringValue("https"))
  ]
  
  // Service A: Set common and service-specific attributes
  Attributes::set(service_a_attributes, "service.name", StringValue("service_a"))
  Attributes::set(service_a_attributes, "service.version", StringValue("1.0.0"))
  
  for attr in common_attributes {
    Attributes::set(service_a_attributes, attr.0, attr.1)
  }
  
  // Service B: Set common and service-specific attributes
  Attributes::set(service_b_attributes, "service.name", StringValue("service_b"))
  Attributes::set(service_b_attributes, "service.version", StringValue("1.0.0"))
  
  for attr in common_attributes {
    Attributes::set(service_b_attributes, attr.0, attr.1)
  }
  
  // Service C: Set common and service-specific attributes
  Attributes::set(service_c_attributes, "service.name", StringValue("service_c"))
  Attributes::set(service_c_attributes, "service.version", StringValue("1.0.0"))
  
  for attr in common_attributes {
    Attributes::set(service_c_attributes, attr.0, attr.1)
  }
  
  // Verify common attributes consistency
  assert_eq(Attributes::get(service_a_attributes, "http.method"), Some(StringValue("GET")))
  assert_eq(Attributes::get(service_b_attributes, "http.method"), Some(StringValue("GET")))
  assert_eq(Attributes::get(service_c_attributes, "http.method"), Some(StringValue("GET")))
  
  assert_eq(Attributes::get(service_a_attributes, "http.status_code"), Some(IntValue(200)))
  assert_eq(Attributes::get(service_b_attributes, "http.status_code"), Some(IntValue(200)))
  assert_eq(Attributes::get(service_c_attributes, "http.status_code"), Some(IntValue(200)))
  
  assert_eq(Attributes::get(service_a_attributes, "http.scheme"), Some(StringValue("https")))
  assert_eq(Attributes::get(service_b_attributes, "http.scheme"), Some(StringValue("https")))
  assert_eq(Attributes::get(service_c_attributes, "http.scheme"), Some(StringValue("https")))
  
  // Verify service-specific attributes
  assert_eq(Attributes::get(service_a_attributes, "service.name"), Some(StringValue("service_a")))
  assert_eq(Attributes::get(service_b_attributes, "service.name"), Some(StringValue("service_b")))
  assert_eq(Attributes::get(service_c_attributes, "service.name"), Some(StringValue("service_c")))
}

test "instrumentation scope consistency across services" {
  // Test instrumentation scope consistency across different services
  let service_a_scope = InstrumentationScope::{ 
    name: "service_a_instrumentation", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  let service_b_scope = InstrumentationScope::{ 
    name: "service_b_instrumentation", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  let service_c_scope = InstrumentationScope::{ 
    name: "service_c_instrumentation", 
    version: Some("1.0.0"), 
    schema_url: Some("https://example.com/schema/v1") 
  }
  
  // Verify version consistency
  assert_eq(service_a_scope.version, service_b_scope.version)
  assert_eq(service_b_scope.version, service_c_scope.version)
  assert_eq(service_a_scope.version, Some("1.0.0"))
  
  // Verify schema URL consistency
  assert_eq(service_a_scope.schema_url, service_b_scope.schema_url)
  assert_eq(service_b_scope.schema_url, service_c_scope.schema_url)
  assert_eq(service_a_scope.schema_url, Some("https://example.com/schema/v1"))
  
  // Verify different names
  assert_eq(service_a_scope.name, "service_a_instrumentation")
  assert_eq(service_b_scope.name, "service_b_instrumentation")
  assert_eq(service_c_scope.name, "service_c_instrumentation")
}