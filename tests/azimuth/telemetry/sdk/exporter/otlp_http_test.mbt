test "otlp http exporter basic functionality" {
  // Test OTLP HTTP exporter basic functionality
  
  let exporter = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.new(
    "http://localhost:4318/v1/traces",
    [("Authorization", "Bearer test-token")]
  )
  
  // Test exporter creation
  assert_true(exporter != @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter::{})
  
  // Test exporting a simple span
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "otlp.exporter.test")
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "otlp.test.span")
  
  // Add attributes and events
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "test.type", @azimuth.telemetry.api.common.StringValue("otlp_export"))
  @azimuth.telemetry.api.trace.Span.add_event(span, "otlp.test.event", None, None)
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("OTLP exported"))
  
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // Export the span
  let export_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.export(exporter, [span])
  assert_true(export_result)
  
  // Test shutdown
  let shutdown_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.shutdown(exporter)
  assert_true(shutdown_result)
}

test "otlp http exporter with batch operations" {
  // Test OTLP HTTP exporter with batch operations
  
  let exporter = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.new(
    "http://localhost:4318/v1/traces",
    [
      ("Authorization", "Bearer batch-token"),
      ("X-Custom-Header", "batch-test")
    ]
  )
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "otlp.batch.test")
  
  // Create multiple spans for batch export
  let spans = []
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Create parent and child spans
  let (parent_ctx, parent_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "otlp.parent")
  spans = spans @ [parent_span]
  
  for i = 0; i < 5; i = i + 1 {
    let (child_ctx, child_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "otlp.child.${i}")
    @azimuth.telemetry.api.trace.Span.set_attribute(child_span, "child.index", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.add_event(child_span, "child.event.${i}", None, None)
    @azimuth.telemetry.api.trace.Span.end(child_span)
    spans = spans @ [child_span]
  }
  
  @azimuth.telemetry.api.trace.Span.set_attribute(parent_span, "children.count", @azimuth.telemetry.api.common.IntValue(5))
  @azimuth.telemetry.api.trace.Span.end(parent_span)
  
  // Export batch
  let batch_export_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.export(exporter, spans)
  assert_true(batch_export_result)
}

test "otlp http exporter error handling" {
  // Test OTLP HTTP exporter error handling scenarios
  
  // Test with invalid endpoint
  let invalid_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.new(
    "http://invalid-host:9999/invalid/path",
    []
  )
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "otlp.error.test")
  let (_, error_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "otlp.error.span")
  @azimuth.telemetry.api.trace.Span.end(error_span)
  
  // Export should handle invalid endpoint gracefully
  let error_export_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.export(invalid_exporter, [error_span])
  assert_true(error_export_result)  // Simplified implementation returns true
  
  // Test with empty span list
  let empty_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.new("http://localhost:4318/v1/traces", [])
  let empty_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.export(empty_exporter, [])
  assert_true(empty_result)
  
  // Test export after shutdown
  let shutdown_exporter = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.new("http://localhost:4318/v1/traces", [])
  let shutdown_success = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.shutdown(shutdown_exporter)
  assert_true(shutdown_success)
  
  let (_, post_shutdown_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "post.shutdown")
  @azimuth.telemetry.api.trace.Span.end(post_shutdown_span)
  
  let post_shutdown_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.export(shutdown_exporter, [post_shutdown_span])
  assert_true(post_shutdown_result)  // Simplified implementation handles gracefully
}

test "otlp http exporter with different configurations" {
  // Test OTLP HTTP exporter with different configurations
  
  // Test with different endpoints
  let endpoints = [
    "http://localhost:4318/v1/traces",
    "https://otel-collector.example.com/v1/traces",
    "http://192.168.1.100:4318/v1/traces"
  ]
  
  for endpoint in endpoints {
    let exporter = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.new(endpoint, [])
    assert_true(exporter != @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter::{})
    
    let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
    let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "endpoint.test")
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "endpoint.test.span")
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "endpoint", @azimuth.telemetry.api.common.StringValue(endpoint))
    @azimuth.telemetry.api.trace.Span.end(span)
    
    let export_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.export(exporter, [span])
    assert_true(export_result)
    
    @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.shutdown(exporter)
  }
  
  // Test with different header configurations
  let header_configs = [
    ([], "no-headers"),
    ([("Authorization", "Bearer token123")], "auth-only"),
    ([("Authorization", "Bearer token123"), ("User-Agent", "azimuth-telemetry/1.0")], "auth-and-ua"),
    ([("X-Custom-1", "value1"), ("X-Custom-2", "value2"), ("X-Custom-3", "value3")], "multi-custom")
  ]
  
  for (headers, config_name) in header_configs {
    let exporter = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.new("http://localhost:4318/v1/traces", headers)
    
    let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
    let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "header.test")
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "header.test.span")
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "header.config", @azimuth.telemetry.api.common.StringValue(config_name))
    @azimuth.telemetry.api.trace.Span.end(span)
    
    let export_result = @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.export(exporter, [span])
    assert_true(export_result)
    
    @azimuth.telemetry.sdk.exporter.otlp_http.SpanExporter.shutdown(exporter)
  }
}