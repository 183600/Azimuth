test "console exporter basic functionality" {
  // Test console exporter for basic telemetry data output
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  
  // Test exporter creation
  assert_true(exporter != @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter::{})
  
  // Create a test span for export
  let span_context = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  
  let span = @azimuth.telemetry.api.trace.Span.new(
    "test.console.span",
    @azimuth.telemetry.api.trace.SpanKind.Internal,
    span_context
  )
  
  // Test span export
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(exporter, span)
  
  // Add attributes and events to span
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "string.attr", @azimuth.telemetry.api.common.StringValue("test_value"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "int.attr", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.trace.Span.add_event(span, "test.event", None, None)
  
  // Export enhanced span
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(exporter, span)
  
  // Test span with status
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Operation completed"))
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(exporter, span)
}

test "console exporter with different span kinds" {
  // Test console exporter with different span kinds
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  
  let span_kinds = [
    (@azimuth.telemetry.api.trace.SpanKind.Internal, "internal.operation"),
    (@azimuth.telemetry.api.trace.SpanKind.Server, "server.request"),
    (@azimuth.telemetry.api.trace.SpanKind.Client, "client.request"),
    (@azimuth.telemetry.api.trace.SpanKind.Producer, "producer.event"),
    (@azimuth.telemetry.api.trace.SpanKind.Consumer, "consumer.event")
  ]
  
  for (kind, name) in span_kinds {
    let span_context = @azimuth.telemetry.api.trace.SpanContext.new(
      "trace123456789abcdef",
      "span" + (name.length().to_string()),
      true,
      ""
    )
    
    let span = @azimuth.telemetry.api.trace.Span.new(name, kind, span_context)
    @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(exporter, span)
  }
}

test "console log exporter functionality" {
  // Test console log exporter for log records
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter.new()
  
  // Test exporter creation
  assert_true(exporter != @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter::{})
  
  // Create test log records with different severity levels
  let severity_levels = [
    (@azimuth.telemetry.api.logs.SeverityNumber.Trace, "Trace message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Debug, "Debug message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Info, "Info message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Warn, "Warning message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Error, "Error message"),
    (@azimuth.telemetry.api.logs.SeverityNumber.Fatal, "Fatal message")
  ]
  
  for (severity, message) in severity_levels {
    let log_record = @azimuth.telemetry.api.logs.LogRecord.new(severity, message, None, None, None, None, None, None)
    @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter.export_log(exporter, log_record)
  }
  
  // Test log record with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "user.id", @azimuth.telemetry.api.common.StringValue("12345"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "request.id", @azimuth.telemetry.api.common.StringValue("req-abcdef"))
  
  let log_with_attrs = @azimuth.telemetry.api.logs.LogRecord.new(
    @azimuth.telemetry.api.logs.SeverityNumber.Info,
    "Request completed",
    Some(attrs),
    Some(1735689600000000000L),
    None,
    Some("trace123"),
    Some("span456"),
    None
  )
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleLogExporter.export_log(exporter, log_with_attrs)
}

test "console metrics exporter functionality" {
  // Test console metrics exporter for different metric types
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.new()
  
  // Test exporter creation
  assert_true(exporter != @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter::{})
  
  // Test counter metric export
  let counter_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(counter_attrs, "method", @azimuth.telemetry.api.common.StringValue("GET"))
  @azimuth.telemetry.api.common.Attributes.set(counter_attrs, "status", @azimuth.telemetry.api.common.StringValue("200"))
  
  let counter_data = @azimuth.telemetry.sdk.exporter.console.MetricData.Counter(
    "http.requests.total",
    Some("Total HTTP requests"),
    Some("requests"),
    100.0,
    counter_attrs
  )
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(exporter, counter_data)
  
  // Test histogram metric export
  let histogram_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(histogram_attrs, "endpoint", @azimuth.telemetry.api.common.StringValue("/api/users"))
  
  let histogram_data = @azimuth.telemetry.sdk.exporter.console.MetricData.Histogram(
    "http.request.duration",
    Some("HTTP request duration"),
    Some("ms"),
    [100.0, 150.0, 200.0, 75.0, 300.0],
    histogram_attrs
  )
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(exporter, histogram_data)
  
  // Test gauge metric export
  let gauge_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(gauge_attrs, "instance", @azimuth.telemetry.api.common.StringValue("server-1"))
  
  let gauge_data = @azimuth.telemetry.sdk.exporter.console.MetricData.Gauge(
    "system.memory.usage",
    Some("Memory usage percentage"),
    Some("percent"),
    75.5,
    gauge_attrs
  )
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleMetricExporter.export_metric(exporter, gauge_data)
}

test "otlp http exporter configuration" {
  // Test OTLP HTTP exporter configuration and setup
  
  // Test exporter creation with default endpoint
  let default_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  assert_true(default_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // Test exporter creation with custom endpoint
  let custom_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_endpoint(
    "https://custom.otlp.example.com:4318/v1/traces"
  )
  assert_true(custom_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // Test exporter creation with headers
  let headers = [("Authorization", "Bearer token123"), ("x-custom-header", "custom-value")]
  let auth_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_headers(headers)
  assert_true(auth_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
  
  // Test exporter creation with timeout
  let timeout_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.with_timeout(30000)
  assert_true(timeout_exporter != @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter::{})
}

test "otlp http exporter span serialization" {
  // Test OTLP HTTP exporter span serialization
  
  let exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  
  // Create a comprehensive span for testing
  let span_context = @azimuth.telemetry.api.trace.SpanContext.new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "congo=t61rcWkgMzE"
  )
  
  let span = @azimuth.telemetry.api.trace.Span.new(
    "otlp.test.span",
    @azimuth.telemetry.api.trace.SpanKind.Server,
    span_context
  )
  
  // Add comprehensive data to span
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "string.attr", @azimuth.telemetry.api.common.StringValue("test_value"))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "int.attr", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "float.attr", @azimuth.telemetry.api.common.FloatValue(3.14))
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "bool.attr", @azimuth.telemetry.api.common.BoolValue(true))
  
  @azimuth.telemetry.api.trace.Span.add_event(
    span,
    "test.event",
    Some([("event.attr", @azimuth.telemetry.api.common.StringValue("event_value"))]),
    Some(1735689600000000000L)
  )
  
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Success"))
  
  // Test span export (should serialize to OTLP format)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(exporter, span)
}

test "exporter batch operations" {
  // Test batch export operations for console and OTLP exporters
  
  let console_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  let otlp_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  
  // Create multiple spans for batch export
  let spans = []
  
  // Add spans to the batch
  for i in 0..5 {
    let span_context = @azimuth.telemetry.api.trace.SpanContext.new(
      "batch_trace_id_" + i.to_string(),
      "batch_span_id_" + i.to_string(),
      true,
      ""
    )
    
    let span = @azimuth.telemetry.api.trace.Span.new(
      "batch.span." + i.to_string(),
      @azimuth.telemetry.api.trace.SpanKind.Internal,
      span_context
    )
    
    spans = spans @ [span]
  }
  
  // Test batch export to console
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export_batch(console_exporter, spans)
  
  // Test batch export to OTLP
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export_batch(otlp_exporter, spans)
  
  // Test empty batch
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export_batch(console_exporter, [])
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export_batch(otlp_exporter, [])
}

test "exporter error handling" {
  // Test exporter error handling and edge cases
  
  let console_exporter = @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.new()
  let otlp_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.new()
  
  // Test with invalid span (empty trace ID)
  let invalid_span_context = @azimuth.telemetry.api.trace.SpanContext.new("", "", false, "")
  let invalid_span = @azimuth.telemetry.api.trace.Span.new("invalid.span", @azimuth.telemetry.api.trace.SpanKind.Internal, invalid_span_context)
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(console_exporter, invalid_span)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(otlp_exporter, invalid_span)
  
  // Test with span that has very long name
  let long_name = "a".repeat(1000)
  let long_span_context = @azimuth.telemetry.api.trace.SpanContext.new("trace123", "span456", true, "")
  let long_span = @azimuth.telemetry.api.trace.Span.new(long_name, @azimuth.telemetry.api.trace.SpanKind.Internal, long_span_context)
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(console_exporter, long_span)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(otlp_exporter, long_span)
  
  // Test with span that has many attributes
  let many_attrs_span = @azimuth.telemetry.api.trace.Span.new("many.attrs.span", @azimuth.telemetry.api.trace.SpanKind.Internal, long_span_context)
  
  for i in 0..100 {
    @azimuth.telemetry.api.trace.Span.set_attribute(
      many_attrs_span,
      "attr." + i.to_string(),
      @azimuth.telemetry.api.common.StringValue("value." + i.to_string())
    )
  }
  
  @azimuth.telemetry.sdk.exporter.console.ConsoleSpanExporter.export(console_exporter, many_attrs_span)
  @azimuth.telemetry.sdk.exporter.otlp_http.OtlpHttpSpanExporter.export(otlp_exporter, many_attrs_span)
}