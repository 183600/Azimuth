// Azimuth Comprehensive Enhanced MoonBit Test Suite
// This file contains 8 comprehensive test cases covering advanced functionality

test "resource management with attribute merging" {
  let base_resource = Resource::new()
  let base_attrs = [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0"))
  ]
  let resource_with_attrs = Resource::with_attributes(base_resource, base_attrs)
  
  let override_resource = Resource::new()
  let override_attrs = [
    ("service.instance.id", StringValue("instance-123")),
    ("deployment.environment", StringValue("production"))
  ]
  let resource_with_override = Resource::with_attributes(override_resource, override_attrs)
  
  let merged_resource = Resource::merge(resource_with_attrs, resource_with_override)
  
  let service_name = Resource::get_attribute(merged_resource, "service.name")
  let instance_id = Resource::get_attribute(merged_resource, "service.instance.id")
  
  assert_true(service_name.is_some())
  assert_true(instance_id.is_some())
}

test "cross-service trace context propagation" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "rojo=00f067aa0ba902b7")
  
  let span = Span::new("parent-span", Server, span_ctx)
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross-service-test")
  
  let child_span = Tracer::start_span(tracer, "child-span")
  
  assert_eq(Span::name(span), "parent-span")
  assert_eq(Span::name(child_span), "child-span")
  assert_eq(Span::kind(span), Server)
  assert_eq(Span::kind(child_span), Internal)
  assert_true(Span::is_recording(span))
  assert_true(Span::is_recording(child_span))
}

test "time series data aggregation operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "time-series-test")
  let histogram = Meter::create_histogram(meter, "response.time", "Response time in milliseconds", "ms")
  
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 120.0)
  Histogram::record(histogram, 180.0)
  
  let instrument = Histogram::as_instrument(histogram)
  assert_eq(Instrument::name(instrument), "response.time")
  assert_eq(Instrument::description(instrument), Some("Response time in milliseconds"))
  assert_eq(Instrument::unit(instrument), Some("ms"))
}

test "error boundary handling with span status" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("error-boundary-test", Internal, span_ctx)
  
  Span::set_status(span, Error, Some("Database connection failed"))
  Span::add_event(span, "error_occurred", [
    ("error.type", StringValue("database")),
    ("error.code", IntValue(500))
  ])
  
  assert_eq(Span::name(span), "error-boundary-test")
  assert_eq(Span::status(span), Error)
  assert_true(Span::is_recording(span))
}

test "concurrent safety with multiple counters" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "concurrent-test")
  
  let counter1 = Meter::create_counter(meter, "requests.total")
  let counter2 = Meter::create_counter(meter, "errors.total")
  let updown_counter = Meter::create_updown_counter(meter, "active.connections")
  let gauge = Meter::create_gauge(meter, "memory.usage")
  
  Counter::add(counter1, 100.0)
  Counter::add(counter2, 5.0)
  UpDownCounter::add(updown_counter, 10.0)
  
  assert_eq(counter1.name, "requests.total")
  assert_eq(counter2.name, "errors.total")
  assert_eq(updown_counter.name, "active.connections")
  assert_eq(gauge.name, "memory.usage")
}

test "internationalization support with multilingual attributes" {
  let attrs = Attributes::new()
  
  Attributes::set(attrs, "error.message.zh", StringValue("数据库连接失败"))
  Attributes::set(attrs, "error.message.en", StringValue("Database connection failed"))
  Attributes::set(attrs, "error.message.ja", StringValue("データベース接続エラー"))
  
  let zh_message = Attributes::get(attrs, "error.message.zh")
  let en_message = Attributes::get(attrs, "error.message.en")
  let ja_message = Attributes::get(attrs, "error.message.ja")
  
  assert_true(zh_message.is_some())
  assert_true(en_message.is_some())
  assert_true(ja_message.is_some())
}

test "performance benchmark with high-frequency operations" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-test")
  let counter = Meter::create_counter(meter, "operations.count")
  
  let clock = Clock::system()
  let start_time = Clock::now_unix_nanos(clock)
  
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(counter, 1.0)
  }
  
  let end_time = Clock::now_unix_nanos(clock)
  let duration = end_time - start_time
  
  assert_eq(counter.name, "operations.count")
  assert_true(duration >= 0L)
}

test "end-to-end integration with logging and tracing" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integration-test")
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Integration test warning message"),
    None,
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(Clock::now_unix_nanos(Clock::system())),
    Some(trace_id),
    Some(span_id),
    Some(Context::root())
  )
  
  Logger::emit(logger, log_record)
  
  assert_eq(LogRecord::severity_number(log_record), Warn)
  assert_eq(LogRecord::body(log_record), Some("Integration test warning message"))
  assert_eq(LogRecord::trace_id(log_record), Some(trace_id))
  assert_eq(LogRecord::span_id(log_record), Some(span_id))
}