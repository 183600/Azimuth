// Multi-Instrument Type Integration Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for integration of different instrument types

test "counter and histogram integration" {
  // Create meter and instruments
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "integration-test-meter")
  
  // Create counter for request counting
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  
  // Create histogram for request duration
  let duration_histogram = Meter::create_histogram(meter, "http.request.duration")
  
  // Simulate HTTP request processing
  for i = 0; i < 10; i = i + 1 {
    // Increment request counter
    Counter::add(request_counter, 1.0)
    
    // Record request duration (simulated varying durations)
    let duration = 50.0 + (i.to_int() * 10.0) + (i.to_int() * 5.0)
    Histogram::record(duration_histogram, duration)
  }
  
  // Verify instrument properties
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(duration_histogram.name, "http.request.duration")
  
  // Test instrument conversion to generic Instrument type
  let counter_instrument = Counter(request_counter.name, request_counter.description, request_counter.unit)
  let histogram_instrument = Histogram::as_instrument(duration_histogram)
  
  assert_eq(Instrument::name(counter_instrument), "http.requests.total")
  assert_eq(Instrument::name(histogram_instrument), "http.request.duration")
  
  // Test instrument metadata
  assert_eq(Instrument::description(counter_instrument), request_counter.description)
  assert_eq(Instrument::unit(counter_instrument), request_counter.unit)
}

test "up-down counter and gauge integration" {
  // Create meter and instruments
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource-monitoring-meter")
  
  // Create up-down counter for active connections
  let active_connections = Meter::create_updown_counter(meter, "server.active_connections")
  
  // Create gauge for memory usage
  let memory_usage = Meter::create_gauge(meter, "server.memory_usage")
  
  // Simulate server lifecycle
  // Start with some initial connections
  UpDownCounter::add(active_connections, 5.0)
  
  // Simulate connection changes
  UpDownCounter::add(active_connections, 3.0)  // +3 connections
  UpDownCounter::add(active_connections, -1.0) // -1 connection
  UpDownCounter::add(active_connections, 2.0)  // +2 connections
  
  // Simulate memory usage changes
  // Note: Gauge operations would depend on implementation
  // In a real implementation, we might have a Gauge::set() method
  
  // Verify instrument properties
  assert_eq(active_connections.name, "server.active_connections")
  assert_eq(memory_usage.name, "server.memory_usage")
  
  // Test instrument metadata
  assert_eq(Instrument::name(UpDownCounter(active_connections.name, active_connections.description, active_connections.unit)), "server.active_connections")
  assert_eq(Instrument::name(Gauge(memory_usage.name, memory_usage.description, memory_usage.unit)), "server.memory_usage")
}

test "multi-instrument scenario with attributes" {
  // Create meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi-instrument-meter")
  
  // Create various instruments
  let api_counter = Meter::create_counter(meter, "api.requests")
  let db_histogram = Meter::create_histogram(meter, "db.query.duration")
  let cache_updown = Meter::create_updown_counter(meter, "cache.items")
  let cpu_gauge = Meter::create_gauge(meter, "system.cpu.usage")
  
  // Create attributes for different operations
  let api_attrs = Attributes::new()
  Attributes::set(api_attrs, "endpoint", StringValue("/api/users"))
  Attributes::set(api_attrs, "method", StringValue("GET"))
  Attributes::set(api_attrs, "status", StringValue("200"))
  
  let db_attrs = Attributes::new()
  Attributes::set(db_attrs, "operation", StringValue("SELECT"))
  Attributes::set(db_attrs, "table", StringValue("users"))
  
  let cache_attrs = Attributes::new()
  Attributes::set(cache_attrs, "cache.type", StringValue("redis"))
  
  // Record metrics with attributes
  Counter::add(api_counter, 1.0, Some(api_attrs))
  Histogram::record(db_histogram, 25.5, Some(db_attrs))
  UpDownCounter::add(cache_updown, 10.0, Some(cache_attrs))
  
  // Update attributes and record more metrics
  Attributes::set(api_attrs, "status", StringValue("404"))
  Counter::add(api_counter, 1.0, Some(api_attrs))
  
  Attributes::set(db_attrs, "operation", StringValue("INSERT"))
  Histogram::record(db_histogram, 45.2, Some(db_attrs))
  
  UpDownCounter::add(cache_updown, -3.0, Some(cache_attrs))
  
  // Verify all instruments have correct names
  assert_eq(api_counter.name, "api.requests")
  assert_eq(db_histogram.name, "db.query.duration")
  assert_eq(cache_updown.name, "cache.items")
  assert_eq(cpu_gauge.name, "system.cpu.usage")
}

test "instrument factory integration" {
  // Create multiple meters for different services
  let provider = MeterProvider::default()
  
  let auth_meter = MeterProvider::get_meter(provider, "auth-service")
  let user_meter = MeterProvider::get_meter(provider, "user-service")
  let payment_meter = MeterProvider::get_meter(provider, "payment-service")
  
  // Create instruments for each service
  let auth_counter = Meter::create_counter(auth_meter, "auth.attempts")
  let auth_histogram = Meter::create_histogram(auth_meter, "auth.duration")
  
  let user_counter = Meter::create_counter(user_meter, "user.operations")
  let user_updown = Meter::create_updown_counter(user_meter, "user.sessions")
  
  let payment_counter = Meter::create_counter(payment_meter, "payment.transactions")
  let payment_histogram = Meter::create_histogram(payment_meter, "payment.amount")
  let payment_gauge = Meter::create_gauge(payment_meter, "payment.queue.size")
  
  // Record metrics for each service
  Counter::add(auth_counter, 1.0)
  Histogram::record(auth_histogram, 100.0)
  
  Counter::add(user_counter, 1.0)
  UpDownCounter::add(user_updown, 1.0)
  
  Counter::add(payment_counter, 1.0)
  Histogram::record(payment_histogram, 99.99)
  
  // Verify instrument isolation by service
  assert_eq(auth_counter.name, "auth.attempts")
  assert_eq(user_counter.name, "user.operations")
  assert_eq(payment_counter.name, "payment.transactions")
  
  // Verify instrument types are correct
  assert_eq(Instrument::name(Counter(auth_counter.name, auth_counter.description, auth_counter.unit)), "auth.attempts")
  assert_eq(Instrument::name(Histogram(auth_histogram.name, auth_histogram.description, auth_histogram.unit)), "auth.duration")
  assert_eq(Instrument::name(UpDownCounter(user_updown.name, user_updown.description, user_updown.unit)), "user.sessions")
  assert_eq(Instrument::name(Gauge(payment_gauge.name, payment_gauge.description, payment_gauge.unit)), "payment.queue.size")
}

test "instrument performance and scalability" {
  // Create meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance-test-meter")
  
  // Create multiple instruments
  let counters = Array::new[Counter]()
  let histograms = Array::new[Histogram]()
  let updown_counters = Array::new[UpDownCounter]()
  let gauges = Array::new[Gauge]()
  
  // Create many instruments
  for i = 0; i < 100; i = i + 1 {
    counters.push(Meter::create_counter(meter, "counter." + i.to_string()))
    histograms.push(Meter::create_histogram(meter, "histogram." + i.to_string()))
    updown_counters.push(Meter::create_updown_counter(meter, "updown." + i.to_string()))
    gauges.push(Meter::create_gauge(meter, "gauge." + i.to_string()))
  }
  
  // Record metrics on all instruments
  for i = 0; i < 100; i = i + 1 {
    Counter::add(counters[i], i.to_int().to_double())
    Histogram::record(histograms[i], i.to_int().to_double() * 2.0)
    UpDownCounter::add(updown_counters[i], i.to_int().to_double() - 50.0)
  }
  
  // Verify instrument names
  assert_eq(counters[0].name, "counter.0")
  assert_eq(histograms[50].name, "histogram.50")
  assert_eq(updown_counters[99].name, "updown.99")
  assert_eq(gauges[25].name, "gauge.25")
  
  // Test instrument access patterns
  let first_counter = counters[0]
  let last_histogram = histograms[99]
  let middle_updown = updown_counters[50]
  let random_gauge = gauges[75]
  
  assert_eq(first_counter.name, "counter.0")
  assert_eq(last_histogram.name, "histogram.99")
  assert_eq(middle_updown.name, "updown.50")
  assert_eq(random_gauge.name, "gauge.75")
}

test "instrument error handling and edge cases" {
  // Create meter
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "edge-case-meter")
  
  // Create instruments with edge case names
  let empty_counter = Meter::create_counter(meter, "")
  let long_histogram = Meter::create_histogram(meter, "very.long.histogram.name." + "x".repeat(100))
  let special_updown = Meter::create_updown_counter(meter, "special-chars-!@#$%^&*()")
  let unicode_gauge = Meter::create_gauge(meter, "unicode-gauge-测试")
  
  // Test operations with edge case values
  Counter::add(empty_counter, 0.0)
  Counter::add(empty_counter, -1.0)
  Counter::add(empty_counter, 1.7976931348623157e+308)  // Max double
  
  Histogram::record(long_histogram, 0.0)
  Histogram::record(long_histogram, -1.0)
  Histogram::record(long_histogram, 1.0 / 0.0)  // Infinity
  Histogram::record(long_histogram, 0.0 / 0.0)  // NaN
  
  UpDownCounter::add(special_updown, 1.7976931348623157e+308)  // Max double
  UpDownCounter::add(special_updown, -1.7976931348623157e+308)  // Min double
  
  // Verify instrument names are preserved
  assert_eq(empty_counter.name, "")
  assert_eq(long_histogram.name, "very.long.histogram.name." + "x".repeat(100))
  assert_eq(special_updown.name, "special-chars-!@#$%^&*()")
  assert_eq(unicode_gauge.name, "unicode-gauge-测试")
  
  // Test instrument with attributes containing edge cases
  let edge_attrs = Attributes::new()
  Attributes::set(edge_attrs, "", StringValue("empty_key"))
  Attributes::set(edge_attrs, "empty.value", StringValue(""))
  Attributes::set(edge_attrs, "unicode.value", StringValue("测试"))
  Attributes::set(edge_attrs, "emoji.value", StringValue("🚀"))
  
  Counter::add(empty_counter, 1.0, Some(edge_attrs))
  Histogram::record(long_histogram, 100.0, Some(edge_attrs))
  UpDownCounter::add(special_updown, 5.0, Some(edge_attrs))
}

test "instrument integration with telemetry context" {
  // Create comprehensive telemetry scenario
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "telemetry-integration-meter")
  
  // Create instruments
  let request_counter = Meter::create_counter(meter, "http.requests")
  let duration_histogram = Meter::create_histogram(meter, "http.duration")
  let active_gauge = Meter::create_gauge(meter, "http.active")
  
  // Create span context for correlation
  let trace_id = "telemetry_trace_1234567890abcdef"
  let span_id = "telemetry_span_1111222233334444"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("telemetry-operation", Server, span_ctx)
  
  // Create context with telemetry data
  let ctx = Context::root()
  let telemetry_ctx = Context::with_value(ctx, ContextKey::new("telemetry.context"), trace_id + ":" + span_id)
  
  // Create attributes with telemetry context
  let telemetry_attrs = Attributes::new()
  Attributes::set(telemetry_attrs, "trace.id", StringValue(trace_id))
  Attributes::set(telemetry_attrs, "span.id", StringValue(span_id))
  Attributes::set(telemetry_attrs, "operation.name", StringValue("telemetry-operation"))
  
  // Add span events for instrument operations
  Span::add_event(span, "metrics.recorded", Some([
    ("metric.type", StringValue("counter")),
    ("metric.name", StringValue("http.requests"))
  ]))
  
  // Record metrics with telemetry context
  Counter::add(request_counter, 1.0, Some(telemetry_attrs))
  Histogram::record(duration_histogram, 150.0, Some(telemetry_attrs))
  
  Span::add_event(span, "metrics.recorded", Some([
    ("metric.type", StringValue("histogram")),
    ("metric.name", StringValue("http.duration"))
  ]))
  
  // Create log record correlated with metrics
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "telemetry-logger")
  
  let metrics_log = LogRecord::new_with_context(
    Info,
    Some("Metrics recorded with telemetry context"),
    Some(telemetry_attrs),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some(trace_id),
    Some(span_id),
    Some(telemetry_ctx)
  )
  
  Logger::emit(logger, metrics_log)
  
  // Verify correlation
  assert_eq(SpanContext::trace_id(span.span_context), trace_id)
  assert_eq(LogRecord::trace_id(metrics_log), Some(trace_id))
  assert_eq(request_counter.name, "http.requests")
  assert_eq(duration_histogram.name, "http.duration")
  
  // End span
  Span::end(span)
}