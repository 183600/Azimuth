// High Quality Telemetry Core Tests for Azimuth Telemetry System
// This file contains comprehensive tests for core telemetry functionality

// Test 1: Span Lifecycle Management and Operations
test "span lifecycle comprehensive management" {
  // Test span creation with different kinds
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "key1=value1,key2=value2")
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("server-operation", Server, span_ctx)
  let client_span = Span::new("client-operation", Client, span_ctx)
  let producer_span = Span::new("producer-operation", Producer, span_ctx)
  let consumer_span = Span::new("consumer-operation", Consumer, span_ctx)
  
  // Test span properties
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::name(server_span), "server-operation")
  assert_eq(Span::name(client_span), "client-operation")
  assert_eq(Span::name(producer_span), "producer-operation")
  assert_eq(Span::name(consumer_span), "consumer-operation")
  
  // Test span context access
  let internal_ctx = Span::context(internal_span)
  let server_ctx = Span::context(server_span)
  assert_eq(SpanContext::trace_id(internal_ctx), "trace-12345")
  assert_eq(SpanContext::trace_id(server_ctx), "trace-12345")
  assert_eq(SpanContext::span_id(internal_ctx), "span-67890")
  assert_eq(SpanContext::span_id(server_ctx), "span-67890")
  
  // Test span recording state
  assert_true(Span::is_recording(internal_span))
  assert_true(Span::is_recording(server_span))
  
  // Test span status operations
  let status_span = Span::new("status-test", Internal, span_ctx)
  Span::set_status(status_span, Ok, "operation completed successfully")
  // Note: Status testing would require status access methods
  
  // Test span attributes
  let attr_span = Span::new("attributes-test", Internal, span_ctx)
  Span::set_attribute(attr_span, "user.id", StringValue("user123"))
  Span::set_attribute(attr_span, "operation.duration", IntValue(1500))
  Span::set_attribute(attr_span, "operation.success", BoolValue(true))
  // Note: Attribute testing would require attribute access methods
}

// Test 2: Tracer Provider and Tracer Operations
test "tracer provider and tracer operations" {
  // Test tracer provider creation
  let tracer_provider = TracerProvider::new()
  assert_true(tracer_provider != None)
  
  // Test tracer creation with different names
  let telemetry_tracer = TracerProvider::get_tracer(tracer_provider, "azimuth.telemetry")
  let metrics_tracer = TracerProvider::get_tracer(tracer_provider, "azimuth.metrics")
  let logging_tracer = TracerProvider::get_tracer(tracer_provider, "azimuth.logging")
  
  // Test tracer properties
  assert_eq(Tracer::scope(telemetry_tracer).name, "azimuth.telemetry")
  assert_eq(Tracer::scope(metrics_tracer).name, "azimuth.metrics")
  assert_eq(Tracer::scope(logging_tracer).name, "azimuth.logging")
  
  // Test span creation through tracer
  let telemetry_span = Tracer::start_span(telemetry_tracer, "telemetry.operation")
  let metrics_span = Tracer::start_span(metrics_tracer, "metrics.operation")
  let logging_span = Tracer::start_span(logging_tracer, "logging.operation")
  
  assert_eq(Span::name(telemetry_span), "telemetry.operation")
  assert_eq(Span::name(metrics_span), "metrics.operation")
  assert_eq(Span::name(logging_span), "logging.operation")
}

// Test 3: Metrics Instrument Operations
test "metrics instrument comprehensive operations" {
  // Test counter instrument
  let counter_instrument = Counter("http.requests.total", Some("Total HTTP requests"), Some("requests"))
  assert_eq(counter_instrument.name, "http.requests.total")
  assert_eq(counter_instrument.description, Some("Total HTTP requests"))
  assert_eq(counter_instrument.unit, Some("requests"))
  
  // Test histogram instrument
  let histogram_instrument = Histogram("http.request.duration", Some("HTTP request duration"), Some("ms"))
  assert_eq(histogram_instrument.name, "http.request.duration")
  assert_eq(histogram_instrument.description, Some("HTTP request duration"))
  assert_eq(histogram_instrument.unit, Some("ms"))
  
  // Test up-down counter instrument
  let updown_counter_instrument = UpDownCounter("active.connections", Some("Active connections"), Some("connections"))
  assert_eq(updown_counter_instrument.name, "active.connections")
  assert_eq(updown_counter_instrument.description, Some("Active connections"))
  assert_eq(updown_counter_instrument.unit, Some("connections"))
  
  // Test gauge instrument
  let gauge_instrument = Gauge("memory.usage", Some("Memory usage"), Some("bytes"))
  assert_eq(gauge_instrument.name, "memory.usage")
  assert_eq(gauge_instrument.description, Some("Memory usage"))
  assert_eq(gauge_instrument.unit, Some("bytes"))
  
  // Test counter creation and operations
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "test-meter")
  let counter = Meter::create_counter(meter, "test.counter")
  let histogram = Meter::create_histogram(meter, "test.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "test.updown_counter")
  let gauge = Meter::create_gauge(meter, "test.gauge")
  
  // Test instrument properties
  assert_eq(counter.name, "test.counter")
  assert_eq(histogram.name, "test.histogram")
  assert_eq(updown_counter.name, "test.updown_counter")
  assert_eq(gauge.name, "test.gauge")
}

// Test 4: Context Propagation Operations
test "context propagation comprehensive operations" {
  // Test context creation and propagation
  let root_ctx = Context::root()
  let trace_id_key = ContextKey::new("trace.id")
  let span_id_key = ContextKey::new("span.id")
  let baggage_key = ContextKey::new("baggage")
  
  // Create context with trace information
  let ctx_with_trace = Context::with_value(root_ctx, trace_id_key, "trace-12345")
  let ctx_with_span = Context::with_value(ctx_with_trace, span_id_key, "span-67890")
  let ctx_with_baggage = Context::with_value(ctx_with_span, baggage_key, "user=user123,session=session456")
  
  // Test context value retrieval
  assert_eq(Context::get(ctx_with_baggage, trace_id_key), Some("trace-12345"))
  assert_eq(Context::get(ctx_with_baggage, span_id_key), Some("span-67890"))
  assert_eq(Context::get(ctx_with_baggage, baggage_key), Some("user=user123,session=session456"))
  
  // Test context propagation through span
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "user=user123")
  let span = Span::new("propagation-test", Internal, span_ctx)
  let propagated_ctx = Span::context(span)
  
  assert_eq(SpanContext::trace_id(propagated_ctx), "trace-12345")
  assert_eq(SpanContext::span_id(propagated_ctx), "span-67890")
  assert_true(SpanContext::is_sampled(propagated_ctx))
}

// Test 5: Text Map Propagation Operations
test "text map propagation operations" {
  // Test text map carrier creation
  let carrier = TextMapCarrier::new()
  assert_eq(carrier.headers.length(), 0)
  
  // Test header setting and getting
  TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  TextMapCarrier::set(carrier, "tracestate", "key1=value1,key2=value2")
  TextMapCarrier::set(carrier, "baggage", "user=user123,session=session456")
  
  assert_eq(TextMapCarrier::get(carrier, "traceparent"), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(TextMapCarrier::get(carrier, "tracestate"), Some("key1=value1,key2=value2"))
  assert_eq(TextMapCarrier::get(carrier, "baggage"), Some("user=user123,session=session456"))
  assert_eq(TextMapCarrier::get(carrier, "nonexistent"), None)
  
  // Test W3C trace context propagation
  let trace_propagator = W3CTraceContextPropagator::new()
  let ctx = Context::root()
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  let ctx_with_span = Context::with_value(ctx, ContextKey::new("span.context"), span_ctx)
  
  // Test injection
  let injected_carrier = TextMapCarrier::new()
  W3CTraceContextPropagator::inject(trace_propagator, ctx_with_span, injected_carrier)
  
  // Test extraction
  let extracted_ctx = W3CTraceContextPropagator::extract(trace_propagator, injected_carrier)
  let extracted_span_ctx = Context::get(extracted_ctx, ContextKey::new("span.context"))
  
  // Verify extracted context matches original
  match (extracted_span_ctx) {
    Some(extracted_ctx) => {
      assert_eq(SpanContext::trace_id(extracted_ctx), "0af7651916cd43dd8448eb211c80319c")
      assert_eq(SpanContext::span_id(extracted_ctx), "b7ad6b7169203331")
      assert_true(SpanContext::is_sampled(extracted_ctx))
    }
    None => assert_true(false) // Should not happen
  }
}

// Test 6: Baggage Propagation Operations
test "baggage propagation operations" {
  // Test baggage creation and operations
  let baggage = Baggage::new()
  let with_user = Baggage::set_entry(baggage, "user.id", "user123")
  let with_session = Baggage::set_entry(with_user, "session.id", "session456")
  let with_tenant = Baggage::set_entry(with_session, "tenant.id", "tenant789")
  
  // Test baggage entry retrieval
  assert_eq(Baggage::get_entry(with_tenant, "user.id"), Some("user123"))
  assert_eq(Baggage::get_entry(with_tenant, "session.id"), Some("session456"))
  assert_eq(Baggage::get_entry(with_tenant, "tenant.id"), Some("tenant789"))
  
  // Test W3C baggage propagation
  let baggage_propagator = W3CBaggagePropagator::new()
  let ctx = Context::root()
  let ctx_with_baggage = Context::with_value(ctx, ContextKey::new("baggage"), with_tenant)
  
  // Test injection
  let carrier = TextMapCarrier::new()
  W3CBaggagePropagator::inject(baggage_propagator, ctx_with_baggage, carrier)
  
  // Test extraction
  let extracted_ctx = W3CBaggagePropagator::extract(baggage_propagator, carrier)
  let extracted_baggage = Context::get(extracted_ctx, ContextKey::new("baggage"))
  
  // Verify extracted baggage matches original
  match (extracted_baggage) {
    Some(extracted) => {
      assert_eq(Baggage::get_entry(extracted, "user.id"), Some("user123"))
      assert_eq(Baggage::get_entry(extracted, "session.id"), Some("session456"))
      assert_eq(Baggage::get_entry(extracted, "tenant.id"), Some("tenant789"))
    }
    None => assert_true(false) // Should not happen
  }
}

// Test 7: Composite Propagator Operations
test "composite propagator operations" {
  // Test composite propagator creation
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Test context with both trace and baggage
  let ctx = Context::root()
  let span_ctx = SpanContext::new("0af7651916cd43dd8448eb211c80319c", "b7ad6b7169203331", true, "")
  let baggage = Baggage::new()
  let with_user = Baggage::set_entry(baggage, "user.id", "user123")
  
  let ctx_with_span = Context::with_value(ctx, ContextKey::new("span.context"), span_ctx)
  let ctx_with_both = Context::with_value(ctx_with_span, ContextKey::new("baggage"), with_user)
  
  // Test composite injection
  let carrier = TextMapCarrier::new()
  CompositePropagator::inject(composite_propagator, ctx_with_both, carrier)
  
  // Verify both traceparent and baggage are injected
  assert_true(TextMapCarrier::get(carrier, "traceparent") != None)
  assert_true(TextMapCarrier::get(carrier, "baggage") != None)
  
  // Test composite extraction
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  let extracted_span_ctx = Context::get(extracted_ctx, ContextKey::new("span.context"))
  let extracted_baggage = Context::get(extracted_ctx, ContextKey::new("baggage"))
  
  // Verify extracted context matches original
  match (extracted_span_ctx) {
    Some(span_ctx) => {
      assert_eq(SpanContext::trace_id(span_ctx), "0af7651916cd43dd8448eb211c80319c")
      assert_eq(SpanContext::span_id(span_ctx), "b7ad6b7169203331")
      assert_true(SpanContext::is_sampled(span_ctx))
    }
    None => assert_true(false) // Should not happen
  }
  
  match (extracted_baggage) {
    Some(baggage) => {
      assert_eq(Baggage::get_entry(baggage, "user.id"), Some("user123"))
    }
    None => assert_true(false) // Should not happen
  }
}

// Test 8: Logger and Log Record Operations
test "logger and log record operations" {
  // Test logger provider creation
  let logger_provider = LoggerProvider::new()
  assert_true(logger_provider != None)
  
  // Test logger creation
  let logger = LoggerProvider::get_logger(logger_provider, "azimuth.logger", Some("1.0.0"), None)
  assert_eq(Logger::name(logger), "azimuth.logger")
  assert_eq(Logger::version(logger), Some("1.0.0"))
  
  // Test log record creation
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Test log message")
  LogRecord::set_severity(log_record, Info)
  LogRecord::set_timestamp(log_record, 1640995200000) // Unix timestamp in milliseconds
  
  // Test log record properties
  assert_eq(LogRecord::body(log_record), "Test log message")
  assert_eq(LogRecord::severity(log_record), Info)
  assert_eq(LogRecord::timestamp(log_record), 1640995200000)
  
  // Test log record with attributes
  let attr_log_record = LogRecord::new()
  LogRecord::set_body(attr_log_record, "Log with attributes")
  LogRecord::set_attribute(attr_log_record, "user.id", StringValue("user123"))
  LogRecord::set_attribute(attr_log_record, "request.id", StringValue("req456"))
  LogRecord::set_attribute(attr_log_record, "duration", IntValue(1500))
  
  assert_eq(LogRecord::body(attr_log_record), "Log with attributes")
}

// Test 9: Meter and Metrics Operations
test "meter and metrics operations" {
  // Test meter provider creation
  let meter_provider = MeterProvider::new()
  assert_true(meter_provider != None)
  
  // Test meter creation
  let meter = MeterProvider::get_meter(meter_provider, "azimuth.meter", Some("1.0.0"), None)
  assert_eq(Meter::name(meter), "azimuth.meter")
  assert_eq(Meter::version(meter), Some("1.0.0"))
  
  // Test counter operations
  let counter = Meter::create_counter(meter, "http.requests.total")
  Counter::add(counter, 1, [ ("method", StringValue("GET")), ("status", StringValue("200")) ])
  Counter::add(counter, 1, [ ("method", StringValue("POST")), ("status", StringValue("201")) ])
  Counter::add(counter, 1, [ ("method", StringValue("GET")), ("status", StringValue("404")) ])
  
  // Test histogram operations
  let histogram = Meter::create_histogram(meter, "http.request.duration")
  Histogram::record(histogram, 150.5, [ ("method", StringValue("GET")) ])
  Histogram::record(histogram, 200.0, [ ("method", StringValue("POST")) ])
  Histogram::record(histogram, 50.0, [ ("method", StringValue("GET")) ])
  
  // Test up-down counter operations
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  UpDownCounter::add(updown_counter, 1, [ ("connection.type", StringValue("websocket")) ])
  UpDownCounter::add(updown_counter, -1, [ ("connection.type", StringValue("websocket")) ])
  UpDownCounter::add(updown_counter, 1, [ ("connection.type", StringValue("http")) ])
  
  // Test gauge operations
  let gauge = Meter::create_gauge(meter, "memory.usage")
  Gauge::record(gauge, 1024.0, [ ("memory.type", StringValue("heap")) ])
  Gauge::record(gauge, 512.0, [ ("memory.type", StringValue("stack")) ])
}

// Test 10: Resource and Instrumentation Scope Integration
test "resource and instrumentation scope integration" {
  // Test resource creation with comprehensive attributes
  let resource = Resource::new()
  Resource::set_attribute(resource, "service.name", StringValue("azimuth-service"))
  Resource::set_attribute(resource, "service.version", StringValue("1.0.0"))
  Resource::set_attribute(resource, "service.instance.id", StringValue("instance-123"))
  Resource::set_attribute(resource, "deployment.environment", StringValue("production"))
  
  // Test instrumentation scope creation
  let scope = InstrumentationScope::new(
    "azimuth.telemetry",
    Some("1.0.0"),
    Some("https://example.com/schema/v1")
  )
  
  // Test tracer with resource and scope
  let tracer_provider = TracerProvider::with_resource(resource)
  let tracer = TracerProvider::get_tracer(tracer_provider, "azimuth.tracer")
  
  // Test span with resource attributes
  let span = Tracer::start_span(tracer, "resource.test")
  let span_resource = Span::resource(span)
  
  // Verify resource attributes are accessible
  assert_eq(Resource::get_attribute(span_resource, "service.name"), Some(StringValue("azimuth-service")))
  assert_eq(Resource::get_attribute(span_resource, "service.version"), Some(StringValue("1.0.0")))
  assert_eq(Resource::get_attribute(span_resource, "deployment.environment"), Some(StringValue("production")))
  
  // Test meter with resource and scope
  let meter_provider = MeterProvider::with_resource(resource)
  let meter = MeterProvider::get_meter(meter_provider, "azimuth.meter")
  
  // Test metrics with resource attributes
  let counter = Meter::create_counter(meter, "test.counter")
  Counter::add(counter, 1, [])
  
  // Test logger with resource and scope
  let logger_provider = LoggerProvider::with_resource(resource)
  let logger = LoggerProvider::get_logger(logger_provider, "azimuth.logger")
  
  // Test log record with resource attributes
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Resource test log")
  Logger::emit_log(logger, log_record)
}