test "sdk console exporter basic functionality" {
  // Test SDK console exporter basic functionality for traces
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.SpanExporter.new()
  
  // Test exporter creation
  assert_true(exporter != @azimuth.telemetry.sdk.exporter.console.SpanExporter::{})
  
  // Test exporting a simple span
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "console.exporter.test")
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "console.test.span")
  
  // Add some attributes and events
  @azimuth.telemetry.api.trace.Span.set_attribute(span, "test.type", @azimuth.telemetry.api.common.StringValue("console_export"))
  @azimuth.telemetry.api.trace.Span.add_event(span, "test.event", None, None)
  @azimuth.telemetry.api.trace.Span.set_status(span, @azimuth.telemetry.api.trace.StatusCode.Ok, Some("Exported successfully"))
  
  // End the span
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // Export the span
  let export_result = @azimuth.telemetry.sdk.exporter.console.SpanExporter.export(exporter, [span])
  assert_true(export_result)
  
  // Test shutdown
  let shutdown_result = @azimuth.telemetry.sdk.exporter.console.SpanExporter.shutdown(exporter)
  assert_true(shutdown_result)
}

test "sdk console exporter with multiple spans" {
  // Test SDK console exporter with multiple spans
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.SpanExporter.new()
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "multi.span.test")
  
  // Create multiple spans
  let spans = []
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Parent span
  let (parent_ctx, parent_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "parent.span")
  spans = spans @ [parent_span]
  
  // Child spans
  let (child1_ctx, child1_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "child.span.1")
  let (child2_ctx, child2_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "child.span.2")
  
  spans = spans @ [child1_span]
  spans = spans @ [child2_span]
  
  // Grandchild span
  let (_, grandchild_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, child1_ctx, "grandchild.span")
  spans = spans @ [grandchild_span]
  
  // Add different attributes to each span
  @azimuth.telemetry.api.trace.Span.set_attribute(parent_span, "span.level", @azimuth.telemetry.api.common.StringValue("parent"))
  @azimuth.telemetry.api.trace.Span.set_attribute(child1_span, "span.level", @azimuth.telemetry.api.common.StringValue("child"))
  @azimuth.telemetry.api.trace.Span.set_attribute(child2_span, "span.level", @azimuth.telemetry.api.common.StringValue("child"))
  @azimuth.telemetry.api.trace.Span.set_attribute(grandchild_span, "span.level", @azimuth.telemetry.api.common.StringValue("grandchild"))
  
  // End all spans
  @azimuth.telemetry.api.trace.Span.end(grandchild_span)
  @azimuth.telemetry.api.trace.Span.end(child2_span)
  @azimuth.telemetry.api.trace.Span.end(child1_span)
  @azimuth.telemetry.api.trace.Span.end(parent_span)
  
  // Export all spans
  let export_result = @azimuth.telemetry.sdk.exporter.console.SpanExporter.export(exporter, spans)
  assert_true(export_result)
}

test "sdk console exporter error handling" {
  // Test SDK console exporter error handling scenarios
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.SpanExporter.new()
  
  // Test exporting empty span list
  let empty_export_result = @azimuth.telemetry.sdk.exporter.console.SpanExporter.export(exporter, [])
  assert_true(empty_export_result)
  
  // Test exporting after shutdown
  let shutdown_result = @azimuth.telemetry.sdk.exporter.console.SpanExporter.shutdown(exporter)
  assert_true(shutdown_result)
  
  // Try to export after shutdown (should still work in simplified implementation)
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "shutdown.test")
  let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "shutdown.test.span")
  @azimuth.telemetry.api.trace.Span.end(span)
  
  let post_shutdown_export = @azimuth.telemetry.sdk.exporter.console.SpanExporter.export(exporter, [span])
  assert_true(post_shutdown_export)
  
  // Test multiple shutdowns
  let second_shutdown = @azimuth.telemetry.sdk.exporter.console.SpanExporter.shutdown(exporter)
  assert_true(second_shutdown)
}

test "sdk console exporter with different span types" {
  // Test SDK console exporter with different span kinds and configurations
  
  let exporter = @azimuth.telemetry.sdk.exporter.console.SpanExporter.new()
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "span.types.test")
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Test different span kinds
  let span_kinds = [
    (@azimuth.telemetry.api.trace.SpanKind.Internal, "internal.span"),
    (@azimuth.telemetry.api.trace.SpanKind.Server, "server.span"),
    (@azimuth.telemetry.api.trace.SpanKind.Client, "client.span"),
    (@azimuth.telemetry.api.trace.SpanKind.Producer, "producer.span"),
    (@azimuth.telemetry.api.trace.SpanKind.Consumer, "consumer.span")
  ]
  
  let spans = []
  for (kind, name) in span_kinds {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span_with_kind(tracer, ctx, name, Some(kind))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "span.kind.test", @azimuth.telemetry.api.common.StringValue("true"))
    @azimuth.telemetry.api.trace.Span.end(span)
    spans = spans @ [span]
  }
  
  // Test span with exception
  let (_, exception_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "exception.span")
  @azimuth.telemetry.api.trace.Span.record_exception(
    exception_span,
    "Test exception for console exporter",
    Some("ConsoleExporterTestException"),
    Some("at console_exporter_test\n  at test_function()"),
    None
  )
  @azimuth.telemetry.api.trace.Span.set_status(exception_span, @azimuth.telemetry.api.trace.StatusCode.Error, Some("Exception occurred"))
  @azimuth.telemetry.api.trace.Span.end(exception_span)
  spans = spans @ [exception_span]
  
  // Test span with many events
  let (_, event_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "events.span")
  for i = 0; i < 5; i = i + 1 {
    @azimuth.telemetry.api.trace.Span.add_event(
      event_span,
      "event.${i}",
      Some([("event.index", @azimuth.telemetry.api.common.IntValue(i))]),
      None
    )
  }
  @azimuth.telemetry.api.trace.Span.end(event_span)
  spans = spans @ [event_span]
  
  // Export all spans
  let export_result = @azimuth.telemetry.sdk.exporter.console.SpanExporter.export(exporter, spans)
  assert_true(export_result)
}