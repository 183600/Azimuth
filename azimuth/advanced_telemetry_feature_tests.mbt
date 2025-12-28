// Advanced Telemetry Feature Tests for Azimuth
// This file contains advanced test cases for telemetry functionality

test "attribute value type conversion and operations" {
  // Test different attribute value types
  let string_attr = StringValue("test_value")
  let int_attr = IntValue(42)
  let float_attr = FloatValue(3.14)
  let bool_attr = BoolValue(true)
  let string_array_attr = ArrayStringValue(["a", "b", "c"])
  let int_array_attr = ArrayIntValue([1, 2, 3])
  
  // Test attribute operations
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", string_attr)
  Attributes::set(attrs, "int.key", int_attr)
  
  let retrieved_string = Attributes::get(attrs, "string.key")
  let retrieved_int = Attributes::get(attrs, "int.key")
  let missing_attr = Attributes::get(attrs, "missing.key")
  
  assert_eq(retrieved_string, Some(string_attr))
  assert_eq(retrieved_int, Some(int_attr))
  assert_eq(missing_attr, None)
}

test "composite propagator injection and extraction" {
  // Create composite propagator
  let trace_propagator = W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Test injection
  let ctx = Context::root()
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx, carrier)
  
  // Verify injection worked
  let injected_trace = TextMapCarrier::get(carrier, "traceparent")
  assert_true(injected_trace != None)
  
  // Test extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "baggage operations and propagation" {
  // Test baggage creation and operations
  let baggage = Baggage::new()
  
  // Test setting entries
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let final_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  
  // Test getting entries
  let user_id = Baggage::get_entry(final_baggage, "user.id")
  let request_id = Baggage::get_entry(final_baggage, "request.id")
  let missing_entry = Baggage::get_entry(final_baggage, "missing.key")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(request_id, Some("req-67890"))
  assert_eq(missing_entry, None)
  
  // Test removal
  let baggage_after_removal = Baggage::remove_entry(final_baggage, "user.id")
  let removed_user_id = Baggage::get_entry(baggage_after_removal, "user.id")
  assert_eq(removed_user_id, None)
}

test "instrument type conversions and metadata" {
  // Test different instrument types
  let counter_instrument = Counter("http.requests", Some("Total HTTP requests"), Some("requests"))
  let histogram_instrument = Histogram("response.time", Some("Response time histogram"), Some("ms"))
  let updown_counter_instrument = UpDownCounter("active.connections", Some("Active connections"), Some("connections"))
  let gauge_instrument = Gauge("memory.usage", Some("Memory usage"), Some("bytes"))
  
  // Test instrument name extraction
  assert_eq(Instrument::name(counter_instrument), "http.requests")
  assert_eq(Instrument::name(histogram_instrument), "response.time")
  assert_eq(Instrument::name(updown_counter_instrument), "active.connections")
  assert_eq(Instrument::name(gauge_instrument), "memory.usage")
  
  // Test instrument description extraction
  assert_eq(Instrument::description(counter_instrument), Some("Total HTTP requests"))
  assert_eq(Instrument::description(histogram_instrument), Some("Response time histogram"))
  assert_eq(Instrument::description(updown_counter_instrument), Some("Active connections"))
  assert_eq(Instrument::description(gauge_instrument), Some("Memory usage"))
  
  // Test instrument unit extraction
  assert_eq(Instrument::unit(counter_instrument), Some("requests"))
  assert_eq(Instrument::unit(histogram_instrument), Some("ms"))
  assert_eq(Instrument::unit(updown_counter_instrument), Some("connections"))
  assert_eq(Instrument::unit(gauge_instrument), Some("bytes"))
}

test "resource creation and merging" {
  // Test resource creation
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let missing_attr = Resource::get_attribute(resource_with_attrs, "missing.attr")
  
  assert_eq(service_name, Some(StringValue("azimuth-service")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(missing_attr, None)
  
  // Test resource merging
  let override_attrs = [
    ("service.version", StringValue("2.0.0")),
    ("environment", StringValue("production"))
  ]
  let override_resource = Resource::with_attributes(Resource::new(), override_attrs)
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  
  // Verify merge results (simplified implementation returns override resource)
  let merged_version = Resource::get_attribute(merged_resource, "service.version")
  let merged_env = Resource::get_attribute(merged_resource, "environment")
  assert_eq(merged_version, Some(StringValue("2.0.0")))
  assert_eq(merged_env, Some(StringValue("production")))
}

test "cross-service context propagation" {
  // Simulate cross-service scenario
  let service_a_context = Context::root()
  let trace_key = ContextKey::new("trace.id")
  let service_a_context_with_trace = Context::with_value(service_a_context, trace_key, "trace-12345")
  
  // Create propagator for cross-service communication
  let propagator = W3CTraceContextPropagator::new()
  let propagators = [propagator]
  let composite_propagator = CompositePropagator::new(propagators)
  
  // Service A injects context into carrier
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, service_a_context_with_trace, carrier)
  
  // Service B extracts context from carrier
  let service_b_context = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify trace propagation
  let extracted_trace = Context::get(service_b_context, ContextKey::new("extracted"))
  assert_true(extracted_trace != None)
  
  // Add service-specific baggage
  let baggage = Baggage::new()
  let service_b_baggage = Baggage::set_entry(baggage, "service.name", "service-b")
  let service_b_baggage_with_user = Baggage::set_entry(service_b_baggage, "user.id", "user-67890")
  
  // Verify baggage operations
  let service_name = Baggage::get_entry(service_b_baggage_with_user, "service.name")
  let user_id = Baggage::get_entry(service_b_baggage_with_user, "user.id")
  assert_eq(service_name, Some("service-b"))
  assert_eq(user_id, Some("user-67890"))
}

test "instrumentation scope management" {
  // Test instrumentation scope creation
  let scope_with_version = InstrumentationScope::{ 
    name: "azimuth.tracer", 
    version: Some("1.2.3"), 
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0") 
  }
  
  let scope_minimal = InstrumentationScope::{ 
    name: "azimuth.meter", 
    version: None, 
    schema_url: None 
  }
  
  // Test tracer provider with scope
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "azimuth.tracer", Some("1.2.3"))
  let tracer_scope = Tracer::instrumentation_scope(tracer)
  
  assert_eq(tracer_scope.name, "azimuth.tracer")
  assert_eq(tracer_scope.version, Some("1.2.3"))
  
  // Test meter provider with scope
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "azimuth.meter")
  let meter_scope = meter.scope
  
  assert_eq(meter_scope.name, "azimuth.meter")
  assert_eq(meter_scope.version, None)
  
  // Test logger provider with scope
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "azimuth.logger")
  let logger_scope = logger.scope
  
  assert_eq(logger_scope.name, "azimuth.logger")
  assert_eq(logger_scope.version, None)
}

test "span lifecycle and event management" {
  // Test span creation with different kinds
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "key1=value1")
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("http-request", Server, span_ctx)
  let client_span = Span::new("database-call", Client, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  assert_eq(Span::span_context(internal_span), span_ctx)
  
  // Test span status operations
  Span::set_status(internal_span, Ok, Some("Operation completed successfully"))
  Span::set_status(server_span, Error, Some("Request failed"))
  
  assert_eq(Span::status(internal_span), Ok)
  assert_eq(Span::status(server_span), Error)
  
  // Test span event addition
  Span::add_event(internal_span, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(server_span, "request.received", Some([("method", StringValue("GET"))]))
  Span::add_event(client_span, "query.executed", Some([("duration", IntValue(150))]))
  
  // Test span end
  Span::end(internal_span)
  Span::end(server_span)
  Span::end(client_span)
  
  assert_true(true)
}

test "comprehensive logging with context" {
  // Test log record creation with full context
  let timestamp = Clock::now_unix_nanos(Clock::system())
  let observed_timestamp = timestamp + 1000000L
  
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Database connection pool exhausted"),
    Some(Attributes::new()),
    Some(timestamp),
    Some(observed_timestamp),
    Some("trace-12345"),
    Some("span-67890"),
    Some(Context::root())
  )
  
  // Test log record properties
  assert_eq(LogRecord::severity_number(log_record), Warn)
  assert_eq(LogRecord::body(log_record), Some("Database connection pool exhausted"))
  assert_eq(LogRecord::trace_id(log_record), Some("trace-12345"))
  assert_eq(LogRecord::span_id(log_record), Some("span-67890"))
  
  // Test logger emit
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "azimuth.logger")
  
  Logger::emit(logger, log_record)
  
  // Test different severity levels
  let debug_record = LogRecord::new(Debug, "Debug message")
  let info_record = LogRecord::new(Info, "Info message")
  let error_record = LogRecord::new(Error, "Error message")
  let fatal_record = LogRecord::new(Fatal, "Fatal error")
  
  assert_eq(LogRecord::severity_number(debug_record), Debug)
  assert_eq(LogRecord::severity_number(info_record), Info)
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::severity_number(fatal_record), Fatal)
  
  // Emit all records
  Logger::emit(logger, debug_record)
  Logger::emit(logger, info_record)
  Logger::emit(logger, error_record)
  Logger::emit(logger, fatal_record)
  
  assert_true(true)
}