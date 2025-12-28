// Comprehensive test suite for Azimuth telemetry system
// This file contains additional test cases covering edge cases, error handling, and integration scenarios

test "error handling and boundary conditions" {
  // Test handling of invalid inputs and boundary conditions
  
  // Test empty string trace ID and span ID
  let empty_trace_ctx = @azimuth.telemetry.api.trace.SpanContext.new("", "", false, "")
  assert_false(empty_trace_ctx.is_valid())
  
  // Test very long attribute names and values
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  let long_key = "a".repeat(1000)
  let long_value = "b".repeat(10000)
  @azimuth.telemetry.api.common.Attributes.set(attrs, long_key, @azimuth.telemetry.api.common.StringValue(long_value))
  
  // Test boundary values for metrics
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "boundary.test")
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "boundary.counter")
  
  // Test with very large and small values
  @azimuth.telemetry.api.metrics.Counter.add(counter, 999999999.0)
  @azimuth.telemetry.api.metrics.Counter.add(counter, 0.000001)
  @azimuth.telemetry.api.metrics.Counter.add(counter, -999999999.0)
  
  // Test with special floating point values
  @azimuth.telemetry.api.metrics.Counter.add(counter, 0.0)
  @azimuth.telemetry.api.metrics.Counter.add(counter, -0.0)
}

test "http client advanced functionality" {
  // Test advanced HTTP client functionality including error scenarios
  
  let client = @azimuth.telemetry.sdk.platform.http.HttpClient.new()
  
  // Test request with various HTTP methods
  let methods = ["GET", "POST", "PUT", "DELETE", "PATCH", "HEAD", "OPTIONS"]
  for method in methods {
    let request = @azimuth.telemetry.sdk.platform.http.HttpRequest.new(
      method,
      "https://httpbin.org/${method.lower()}",
      [("User-Agent", "Azimuth-Telemetry/1.0")],
      if method == "POST" || method == "PUT" { Some("{\"test\": \"data\"}") } else { None }
    )
    assert_eq(request.method(), method)
  }
  
  // Test response with various status codes
  let status_codes = [200, 201, 400, 401, 404, 500, 502, 503]
  for code in status_codes {
    let response = @azimuth.telemetry.sdk.platform.http.HttpResponse.new(
      code,
      [("Content-Type", "application/json")],
      Some("{\"status\": \"${code}\"}")
    )
    assert_eq(response.status_code(), code)
  }
  
  // Test request with multiple headers
  let multi_header_request = @azimuth.telemetry.sdk.platform.http.HttpRequest.new(
    "POST",
    "https://api.example.com/data",
    [
      ("Content-Type", "application/json"),
      ("Authorization", "Bearer token123"),
      ("X-Custom-Header", "custom-value"),
      ("X-Request-ID", "req-12345")
    ],
    Some("{\"data\": \"test\"}")
  )
  
  let headers = multi_header_request.headers()
  assert_true(headers.length() >= 4)
}

test "comprehensive telemetry integration" {
  // Test integration between tracing, metrics, and logging
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "integration.test")
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "integration.test")
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "integration.test")
  
  // Create instruments
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "operations.total")
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "operation.duration")
  
  // Start a span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (ctx_with_span, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "integration.operation")
  
  // Add attributes to span
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "operation.type", @azimuth.telemetry.api.common.StringValue("integration"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "service.name", @azimuth.telemetry.api.common.StringValue("test-service"))
  
  // Record metrics
  @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0)
  @azimuth.telemetry.api.metrics.Histogram.record(histogram, 150.5)
  
  // Log within the span context
  let log_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(log_attrs, "operation.id", @azimuth.telemetry.api.common.StringValue("op-123"))
  @azimuth.telemetry.api.logs.Logger.info_with_attributes(logger, "Operation started", log_attrs)
  
  // Add event to span
  let event_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(event_attrs, "event.type", @azimuth.telemetry.api.common.StringValue("milestone"))
  @azimuth.telemetry.api.trace.Span.add_event(span, "operation.milestone", Some(event_attrs))
  
  // Complete the operation
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Operation completed"))
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // Final log
  @azimuth.telemetry.api.logs.Logger.info(logger, "Operation completed successfully")
}

test "time and timestamp functionality" {
  // Test advanced time and timestamp functionality
  
  let clock = @azimuth.telemetry.sdk.platform.time.Clock.system()
  
  // Test multiple time readings
  let times = []
  for i in 0..10 {
    let time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
    times.push(time)
  }
  
  // Verify times are monotonically increasing
  for i in 1..times.length() {
    assert_true(times[i] >= times[i-1])
  }
  
  // Test time difference calculations
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  // Simulate some work
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(clock)
  let duration = end_time - start_time
  assert_true(duration >= 0)
  
  // Test log records with different timestamp scenarios
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "time.test")
  
  // Log with explicit timestamp
  let explicit_time = 1735689600000000000L  // Fixed timestamp
  let log_record1 = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Message with explicit timestamp",
    None,
    Some(explicit_time),
    None,
    None,
    None,
    None
  )
  assert_eq(log_record1.timestamp(), Some(explicit_time))
  
  // Log with automatic timestamp
  let log_record2 = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Message with automatic timestamp",
    None,
    None,
    None,
    None,
    None,
    None
  )
  assert_true(log_record2.observed_timestamp() != None)
  assert_true(log_record2.observed_timestamp().unwrap() > 0)
}

test "random number generation and id uniqueness" {
  // Test random number generation and ID uniqueness
  
  let random = @azimuth.telemetry.sdk.platform.random.Random.system()
  
  // Test trace ID uniqueness
  let trace_ids = []
  for i in 0..100 {
    let bytes = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 16)
    let hex = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(bytes)
    trace_ids.push(hex)
  }
  
  // Verify all trace IDs are unique
  let unique_trace_ids = trace_ids.to_set()
  assert_eq(unique_trace_ids.length(), trace_ids.length())
  
  // Test span ID uniqueness
  let span_ids = []
  for i in 0..100 {
    let bytes = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, 8)
    let hex = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(bytes)
    span_ids.push(hex)
  }
  
  // Verify all span IDs are unique
  let unique_span_ids = span_ids.to_set()
  assert_eq(unique_span_ids.length(), span_ids.length())
  
  // Test random u64 distribution (basic statistical test)
  let random_values = []
  for i in 0..1000 {
    let value = @azimuth.telemetry.sdk.platform.random.Random.next_u64(random)
    random_values.push(value)
  }
  
  // Basic distribution check - values should be spread across the range
  let max_value = random_values.max()
  let min_value = random_values.min()
  let range = max_value - min_value
  
  // Check that we have a reasonable spread (not all values are the same)
  assert_true(range > 0)
  
  // Test hex encoding/decoding with various lengths
  let test_lengths = [1, 2, 4, 8, 16, 32, 64]
  for length in test_lengths {
    let bytes = @azimuth.telemetry.sdk.platform.random.Random.next_bytes(random, length)
    let hex = @azimuth.telemetry.sdk.platform.random.bytes_to_hex(bytes)
    let decoded = @azimuth.telemetry.sdk.platform.random.hex_to_bytes(hex)
    assert_eq(bytes, decoded)
    assert_eq(hex.length(), length * 2)
  }
}

test "data serialization and format handling" {
  // Test data serialization and format handling
  
  // Test attribute value serialization
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  // Set various attribute types
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.val", @azimuth.telemetry.api.common.StringValue("test string"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.val", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.val", @azimuth.telemetry.api.common.FloatValue(3.14159))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.val", @azimuth.telemetry.api.common.BoolValue(true))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.string", @azimuth.telemetry.api.common.ArrayStringValue(["a", "b", "c"]))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "array.int", @azimuth.telemetry.api.common.ArrayIntValue([1, 2, 3]))
  
  // Test complex log record serialization
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "serialization.test")
  
  let complex_record = @azimuth.telemetry.api.logs.LogRecord.new_with_context(
    @azimuth.telemetry.api.logs.SeverityNumber.Error,
    Some("Complex serialization test"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("trace12345678901234567890123456789012"),
    Some("span12345678901234"),
    Some(@azimuth.telemetry.api.context.Context.root())
  )
  
  // Verify all fields are set correctly
  assert_eq(complex_record.severity_number(), @azimuth.telemetry.api.logs.SeverityNumber.Error)
  assert_eq(complex_record.body(), Some("Complex serialization test"))
  assert_eq(complex_record.timestamp(), Some(1735689600000000000L))
  assert_eq(complex_record.observed_timestamp(), Some(1735689600000000001L))
  assert_eq(complex_record.trace_id(), Some("trace12345678901234567890123456789012"))
  assert_eq(complex_record.span_id(), Some("span12345678901234"))
  
  // Test resource attribute serialization
  let resource_attrs = [
    ("service.name", @azimuth.telemetry.api.common.StringValue("test-service")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0")),
    ("service.instance.id", @azimuth.telemetry.api.common.StringValue("instance-123")),
    ("deployment.environment", @azimuth.telemetry.api.common.StringValue("test"))
  ]
  
  let resource = @azimuth.telemetry.api.common.Resource.with_attributes(
    @azimuth.telemetry.api.common.Resource.new(),
    resource_attrs
  )
  
  // Verify resource attributes
  for (key, value) in resource_attrs {
    let retrieved = @azimuth.telemetry.api.common.Resource.get_attribute(resource, key)
    assert_eq(retrieved, Some(value))
  }
}

test "performance and resource management" {
  // Test performance characteristics and resource management
  
  // Test creating many spans
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "performance.test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  let spans = []
  for i in 0..1000 {
    let (_, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "performance.span.${i}")
    spans.push(span)
  }
  
  // End all spans
  for span in spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // Test creating many metric measurements
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.noop()
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "performance.test")
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "performance.counter")
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "performance.histogram")
  
  // Record many measurements
  for i in 0..10000 {
    @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0)
    @azimuth.telemetry.api.metrics.Histogram.record(histogram, (i % 1000) as Double)
  }
  
  // Test large attribute sets
  let large_attrs = @azimuth.telemetry.api.common.Attributes.new()
  for i in 0..100 {
    let key = "attr.${i}"
    let value = @azimuth.telemetry.api.common.StringValue("value.${i}")
    @azimuth.telemetry.api.common.Attributes.set(large_attrs, key, value)
  }
  
  // Test large baggage entries
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  for i in 0..50 {
    baggage = @azimuth.telemetry.api.context.Baggage.set_value(baggage, "key.${i}", "value.${i}")
  }
  
  // Test many log records
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "performance.test")
  
  for i in 0..1000 {
    @azimuth.telemetry.api.logs.Logger.info(logger, "Performance test message ${i}")
  }
}

test "data consistency and validation" {
  // Test data consistency and validation across different components
  
  // Test span context consistency
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = @azimuth.telemetry.api.trace.SpanContext.new(trace_id, span_id, true, "congo=t61rcWkgMzE")
  
  // Verify span context properties
  assert_eq(span_ctx.trace_id(), trace_id)
  assert_eq(span_ctx.span_id(), span_id)
  assert_true(span_ctx.is_sampled())
  assert_eq(span_ctx.trace_state(), "congo=t61rcWkgMzE")
  assert_true(span_ctx.is_valid())
  
  // Test context propagation consistency
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let ctx_with_span = @azimuth.telemetry.api.context.Context.with_span_context(ctx, span_ctx)
  let extracted_span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(ctx_with_span)
  
  // Verify propagated context matches original
  assert_eq(extracted_span_ctx.trace_id(), trace_id)
  assert_eq(extracted_span_ctx.span_id(), span_id)
  assert_true(extracted_span_ctx.is_sampled())
  
  // Test baggage consistency
  let baggage = @azimuth.telemetry.api.context.Baggage.new()
  let baggage_with_entries = @azimuth.telemetry.api.context.Baggage.set_value(baggage, "test.key", "test.value")
  let ctx_with_baggage = @azimuth.telemetry.api.context.Context.with_baggage(ctx, baggage_with_entries)
  let extracted_baggage = @azimuth.telemetry.api.context.Context.get_baggage(ctx_with_baggage)
  
  // Verify baggage propagation
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(extracted_baggage, "test.key"), Some("test.value"))
  
  // Test text map propagation consistency
  let carrier = @azimuth.telemetry.api.propagation.HttpHeadersCarrier.new()
  let trace_propagator = @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  let baggage_propagator = @azimuth.telemetry.api.propagation.W3CBaggagePropagator.new()
  
  // Inject context
  let composite_ctx = @azimuth.telemetry.api.context.Context.with_baggage(ctx_with_span, baggage_with_entries)
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator.new([trace_propagator, baggage_propagator])
  @azimuth.telemetry.api.propagation.TextMapPropagator.inject(composite, composite_ctx, carrier)
  
  // Extract context
  let empty_ctx = @azimuth.telemetry.api.context.Context.root()
  let extracted_ctx = @azimuth.telemetry.api.propagation.TextMapPropagator.extract(composite, empty_ctx, carrier)
  
  // Verify round-trip consistency
  let final_span_ctx = @azimuth.telemetry.api.context.Context.get_span_context(extracted_ctx)
  let final_baggage = @azimuth.telemetry.api.context.Context.get_baggage(extracted_ctx)
  
  assert_eq(final_span_ctx.trace_id(), trace_id)
  assert_eq(final_span_ctx.span_id(), span_id)
  assert_eq(@azimuth.telemetry.api.context.Baggage.get_value(final_baggage, "test.key"), Some("test.value"))
  
  // Test attribute type consistency
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.key", @azimuth.telemetry.api.common.StringValue("test"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.key", @azimuth.telemetry.api.common.IntValue(42))
  
  // Verify attribute retrieval consistency
  let string_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "string.key")
  let int_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "int.key")
  
  assert_eq(string_val, Some(@azimuth.telemetry.api.common.StringValue("test")))
  assert_eq(int_val, Some(@azimuth.telemetry.api.common.IntValue(42)))
  
  // Test non-existent keys return None consistently
  let non_existent = @azimuth.telemetry.api.common.Attributes.get(attrs, "non.existent.key")
  assert_eq(non_existent, None)
}