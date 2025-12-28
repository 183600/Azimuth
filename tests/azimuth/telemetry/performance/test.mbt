test "telemetry performance and efficiency" {
  // Test telemetry performance and efficiency scenarios
  
  // Test high-frequency span creation
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "performance.test")
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Create many spans quickly
  let spans = []
  for i = 0; i < 100; i = i + 1 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "perf.span.${i}")
    // Add minimal attributes
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "index", @azimuth.telemetry.api.common.IntValue(i))
    spans = spans @ [span]
  }
  
  // End all spans
  for span in spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let duration_ms = (end_time - start_time) / 1_000_000L
  
  // Performance should be reasonable (less than 1 second for 100 spans in test environment)
  assert_true(duration_ms < 1000L)
  
  // Test high-frequency metrics recording
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "perf.metrics.test")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "perf.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "perf.histogram")
  
  let metrics_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Record many metrics quickly
  for i = 0; i < 1000; i = i + 1 {
    @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
    @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, @azimuth.telemetry.api.common.IntValue(i).to_double())
  }
  
  let metrics_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let metrics_duration_ms = (metrics_end - metrics_start) / 1_000_000L
  
  // Metrics recording should be fast
  assert_true(metrics_duration_ms < 1000L)
  
  // Test high-frequency logging
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "perf.logs.test")
  
  let logs_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Log many messages quickly
  for i = 0; i < 500; i = i + 1 {
    @azimuth.telemetry.sdk.logs.Logger.info(logger, "Performance test log message ${i}")
  }
  
  let logs_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let logs_duration_ms = (logs_end - logs_start) / 1_000_000L
  
  // Logging should be reasonably fast
  assert_true(logs_duration_ms < 1000L)
  
  // Test memory efficiency with large attribute values
  let large_attrs = @azimuth.telemetry.api.common.Attributes.new()
  let large_string = "x".repeat(10000)  // 10KB string
  let large_array = []
  for i = 0; i < 1000; i = i + 1 {
    large_array = large_array @ ["item.${i}"]
  }
  
  @azimuth.telemetry.api.common.Attributes.set(large_attrs, "large.string", @azimuth.telemetry.api.common.StringValue(large_string))
  @azimuth.telemetry.api.common.Attributes.set(large_attrs, "large.array", @azimuth.telemetry.api.common.ArrayStringValue(large_array))
  
  let memory_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Use large attributes in span
  let (_, large_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "large.attrs.span")
  @azimuth.telemetry.api.trace.Span.set_attribute(large_span, "payload", @azimuth.telemetry.api.common.StringValue(large_string))
  
  for i = 0; i < 10; i = i + 1 {
    @azimuth.telemetry.api.trace.Span.set_attribute(
      large_span, 
      "array.attr.${i}", 
      @azimuth.telemetry.api.common.ArrayStringValue(large_array)
    )
  }
  
  @azimuth.telemetry.api.trace.Span.end(large_span)
  
  let memory_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let memory_duration_ms = (memory_end - memory_start) / 1_000_000L
  
  // Large attribute operations should complete in reasonable time
  assert_true(memory_duration_ms < 5000L)
  
  // Test concurrent operations simulation
  let concurrent_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Simulate concurrent span operations
  let concurrent_spans = []
  for batch = 0; batch < 10; batch = batch + 1 {
    let batch_spans = []
    for i = 0; i < 10; i = i + 1 {
      let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "concurrent.span.${batch}.${i}")
      @azimuth.telemetry.api.trace.Span.set_attribute(span, "batch", @azimuth.telemetry.api.common.IntValue(batch))
      @azimuth.telemetry.api.trace.Span.set_attribute(span, "index", @azimuth.telemetry.api.common.IntValue(i))
      batch_spans = batch_spans @ [span]
    }
    
    // End spans in batch
    for span in batch_spans {
      @azimuth.telemetry.api.trace.Span.end(span)
    }
  }
  
  let concurrent_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let concurrent_duration_ms = (concurrent_end - concurrent_start) / 1_000_000L
  
  // Concurrent operations should be efficient
  assert_true(concurrent_duration_ms < 2000L)
  
  // Test resource efficiency with many resources
  let resource_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let resources = []
  for i = 0; i < 50; i = i + 1 {
    let resource_attrs = [
      ("resource.id", @azimuth.telemetry.api.common.StringValue("resource.${i}")),
      ("resource.type", @azimuth.telemetry.api.common.StringValue("test.resource")),
      ("resource.index", @azimuth.telemetry.api.common.IntValue(i))
    ]
    let resource = @azimuth.telemetry.api.common.Resource.with_attributes(
      @azimuth.telemetry.api.common.Resource.new(),
      resource_attrs
    )
    resources = resources @ [resource]
  }
  
  let resource_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let resource_duration_ms = (resource_end - resource_start) / 1_000_000L
  
  // Resource creation should be efficient
  assert_true(resource_duration_ms < 1000L)
  
  // Test context propagation efficiency
  let propagation_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let key1 = @azimuth.telemetry.api.context.ContextKey::new("perf.key1")
  let key2 = @azimuth.telemetry.api.context.ContextKey::new("perf.key2")
  let key3 = @azimuth.telemetry.api.context.ContextKey::new("perf.key3")
  
  // Create deep context chain
  let ctx1 = @azimuth.telemetry.api.context.Context.with_value(ctx, key1, "value1")
  let ctx2 = @azimuth.telemetry.api.context.Context.with_value(ctx1, key2, "value2")
  let ctx3 = @azimuth.telemetry.api.context.Context.with_value(ctx2, key3, "value3")
  
  // Retrieve values through deep context chain
  for i = 0; i < 1000; i = i + 1 {
    let value1 = @azimuth.telemetry.api.context.Context.get(ctx3, key1)
    let value2 = @azimuth.telemetry.api.context.Context.get(ctx3, key2)
    let value3 = @azimuth.telemetry.api.context.Context.get(ctx3, key3)
    
    assert_eq(value1, Some("value1"))
    assert_eq(value2, Some("value2"))
    assert_eq(value3, Some("value3"))
  }
  
  let propagation_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  let propagation_duration_ms = (propagation_end - propagation_start) / 1_000_000L
  
  // Context operations should be fast
  assert_true(propagation_duration_ms < 1000L)
}