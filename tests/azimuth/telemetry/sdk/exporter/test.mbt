test "sdk console exporter basic functionality" {
  // Test SDK console exporter basic functionality for traces
  
  let exporter = telemetry.sdk.exporter.console.SpanExporter.new()
  
  // Test exporter creation
  assert_true(exporter != telemetry.sdk.exporter.console.SpanExporter::{})
  
  // Test exporting a simple span
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "console.exporter.test")
  let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "console.test.span")
  
  // Add some attributes and events
  telemetry.api.trace.Span.set_attribute(span, "test.type", telemetry.api.common.StringValue("console_export"))
  telemetry.api.trace.Span.add_event(span, "test.event", None, None)
  telemetry.api.trace.Span.set_status(span, telemetry.api.trace.StatusCode.Ok, Some("Exported successfully"))
  
  // End the span
  telemetry.api.trace.Span.end(span)
  
  // Export the span
  let export_result = telemetry.sdk.exporter.console.SpanExporter.export(exporter, [span])
  assert_true(export_result)
  
  // Test shutdown
  let shutdown_result = telemetry.sdk.exporter.console.SpanExporter.shutdown(exporter)
  assert_true(shutdown_result)
}

test "sdk console exporter with multiple spans" {
  // Test SDK console exporter with multiple spans
  
  let exporter = telemetry.sdk.exporter.console.SpanExporter.new()
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "multi.span.test")
  
  // Create multiple spans
  let spans = []
  let ctx = telemetry.api.context.Context.root()
  
  // Parent span
  let (parent_ctx, parent_span) = telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "parent.span")
  spans = spans @ [parent_span]
  
  // Child spans
  let (child1_ctx, child1_span) = telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "child.span.1")
  let (child2_ctx, child2_span) = telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "child.span.2")
  
  spans = spans @ [child1_span]
  spans = spans @ [child2_span]
  
  // Grandchild span
  let (_, grandchild_span) = telemetry.sdk.trace.Tracer.start_span(tracer, child1_ctx, "grandchild.span")
  spans = spans @ [grandchild_span]
  
  // Add different attributes to each span
  telemetry.api.trace.Span.set_attribute(parent_span, "span.level", telemetry.api.common.StringValue("parent"))
  telemetry.api.trace.Span.set_attribute(child1_span, "span.level", telemetry.api.common.StringValue("child"))
  telemetry.api.trace.Span.set_attribute(child2_span, "span.level", telemetry.api.common.StringValue("child"))
  telemetry.api.trace.Span.set_attribute(grandchild_span, "span.level", telemetry.api.common.StringValue("grandchild"))
  
  // End all spans
  telemetry.api.trace.Span.end(grandchild_span)
  telemetry.api.trace.Span.end(child2_span)
  telemetry.api.trace.Span.end(child1_span)
  telemetry.api.trace.Span.end(parent_span)
  
  // Export all spans
  let export_result = telemetry.sdk.exporter.console.SpanExporter.export(exporter, spans)
  assert_true(export_result)
}

test "sdk console exporter error handling" {
  // Test SDK console exporter error handling scenarios
  
  let exporter = telemetry.sdk.exporter.console.SpanExporter.new()
  
  // Test exporting empty span list
  let empty_export_result = telemetry.sdk.exporter.console.SpanExporter.export(exporter, [])
  assert_true(empty_export_result)
  
  // Test exporting after shutdown
  let shutdown_result = telemetry.sdk.exporter.console.SpanExporter.shutdown(exporter)
  assert_true(shutdown_result)
  
  // Try to export after shutdown (should still work in simplified implementation)
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "shutdown.test")
  let (_, span) = telemetry.sdk.trace.Tracer.start_span(tracer, "shutdown.test.span")
  telemetry.api.trace.Span.end(span)
  
  let post_shutdown_export = telemetry.sdk.exporter.console.SpanExporter.export(exporter, [span])
  assert_true(post_shutdown_export)
  
  // Test multiple shutdowns
  let second_shutdown = telemetry.sdk.exporter.console.SpanExporter.shutdown(exporter)
  assert_true(second_shutdown)
}

test "sdk console exporter with different span types" {
  // Test SDK console exporter with different span kinds and configurations
  
  let exporter = telemetry.sdk.exporter.console.SpanExporter.new()
  let trace_provider = telemetry.sdk.trace.TracerProvider.default()
  let tracer = telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "span.types.test")
  let ctx = telemetry.api.context.Context.root()
  
  // Test different span kinds
  let span_kinds = [
    (telemetry.api.trace.SpanKind.Internal, "internal.span"),
    (telemetry.api.trace.SpanKind.Server, "server.span"),
    (telemetry.api.trace.SpanKind.Client, "client.span"),
    (telemetry.api.trace.SpanKind.Producer, "producer.span"),
    (telemetry.api.trace.SpanKind.Consumer, "consumer.span")
  ]
  
  let spans = []
  for (kind, name) in span_kinds {
    let (_, span) = telemetry.sdk.trace.Tracer.start_span_with_kind(tracer, ctx, name, Some(kind))
    telemetry.api.trace.Span.set_attribute(span, "span.kind.test", telemetry.api.common.StringValue("true"))
    telemetry.api.trace.Span.end(span)
    spans = spans @ [span]
  }
  
  // Test span with exception
  let (_, exception_span) = telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "exception.span")
  telemetry.api.trace.Span.record_exception(
    exception_span,
    "Test exception for console exporter",
    Some("ConsoleExporterTestException"),
    Some("at console_exporter_test\n  at test_function()"),
    None
  )
  telemetry.api.trace.Span.set_status(exception_span, telemetry.api.trace.StatusCode.Error, Some("Exception occurred"))
  telemetry.api.trace.Span.end(exception_span)
  spans = spans @ [exception_span]
  
  // Test span with many events
  let (_, event_span) = telemetry.sdk.trace.Tracer.start_span(tracer, "events.span")
  for i = 0; i < 5; i = i + 1 {
    telemetry.api.trace.Span.add_event(
      event_span,
      "event.${i}",
      Some([("event.index", telemetry.api.common.IntValue(i))]),
      None
    )
  }
  telemetry.api.trace.Span.end(event_span)
  spans = spans @ [event_span]
  
  // Export all spans
  let export_result = telemetry.sdk.exporter.console.SpanExporter.export(exporter, spans)
  assert_true(export_result)
}