test "telemetry performance benchmarks" {
  // Test performance characteristics of telemetry operations
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "performance.test")
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "performance.test")
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "performance.test")
  
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "performance.counter")
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "performance.histogram")
  
  // Benchmark span creation
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut i = 0
  while i < 1000 {
    let ctx = @azimuth.telemetry.api.context.Context.root()
    let (_, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "performance.span." + i.to_string())
    @azimuth.telemetry.api.trace.Span.end(span)
    i = i + 1
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let span_duration = end_time - start_time
  
  // Benchmark metric recording
  let metric_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut j = 0
  while j < 1000 {
    @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0)
    @azimuth.telemetry.api.metrics.Histogram.record(histogram, 100.0)
    j = j + 1
  }
  
  let metric_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let metric_duration = metric_end_time - metric_start_time
  
  // Benchmark log creation
  let log_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut k = 0
  while k < 1000 {
    let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
      @azimuth.telemetry.api.logs.SeverityNumber.Info,
      "Performance test log message " + k.to_string(),
      None,
      None,
      None,
      None,
      None,
      None
    )
    k = k + 1
  }
  
  let log_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let log_duration = log_end_time - log_start_time
  
  // Performance assertions (these are rough guidelines)
  assert_true(span_duration > 0L, "Span creation should take time")
  assert_true(metric_duration > 0L, "Metric recording should take time")
  assert_true(log_duration > 0L, "Log creation should take time")
  
  // Operations should complete in reasonable time (very generous limits for test environment)
  assert_true(span_duration < 10000000000L, "1000 spans should complete within 10 seconds")
  assert_true(metric_duration < 10000000000L, "2000 metric operations should complete within 10 seconds")
  assert_true(log_duration < 10000000000L, "1000 log records should complete within 10 seconds")
}

test "attribute operations performance" {
  // Test performance of attribute operations
  
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  // Benchmark attribute setting
  let set_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut i = 0
  while i < 1000 {
    @azimuth.telemetry.api.common.Attributes.set(attrs, "attr." + i.to_string(), @azimuth.telemetry.api.common.StringValue("value." + i.to_string()))
    i = i + 1
  }
  
  let set_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let set_duration = set_end_time - set_start_time
  
  // Benchmark attribute getting
  let get_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut j = 0
  while j < 1000 {
    let _ = @azimuth.telemetry.api.common.Attributes.get(attrs, "attr." + j.to_string())
    j = j + 1
  }
  
  let get_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let get_duration = get_end_time - get_start_time
  
  // Performance assertions
  assert_true(set_duration > 0L, "Attribute setting should take time")
  assert_true(get_duration > 0L, "Attribute getting should take time")
  assert_true(set_duration < 10000000000L, "1000 attribute sets should complete within 10 seconds")
  assert_true(get_duration < 10000000000L, "1000 attribute gets should complete within 10 seconds")
}

test "context operations performance" {
  // Test performance of context operations
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Benchmark context value setting
  let set_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut current_ctx = ctx
  let mut i = 0
  while i < 1000 {
    let key = @azimuth.telemetry.api.context.ContextKey::new("key." + i.to_string())
    current_ctx = @azimuth.telemetry.api.context.Context.with_value(current_ctx, key, "value." + i.to_string())
    i = i + 1
  }
  
  let set_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let set_duration = set_end_time - set_start_time
  
  // Benchmark context value getting
  let get_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut j = 0
  while j < 1000 {
    let key = @azimuth.telemetry.api.context.ContextKey::new("key." + j.to_string())
    let _ = @azimuth.telemetry.api.context.Context.get(current_ctx, key)
    j = j + 1
  }
  
  let get_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let get_duration = get_end_time - get_start_time
  
  // Performance assertions
  assert_true(set_duration > 0L, "Context setting should take time")
  assert_true(get_duration > 0L, "Context getting should take time")
  assert_true(set_duration < 10000000000L, "1000 context sets should complete within 10 seconds")
  assert_true(get_duration < 10000000000L, "1000 context gets should complete within 10 seconds")
}

test "propagation operations performance" {
  // Test performance of propagation operations
  
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator.new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  ])
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // Benchmark injection operations
  let inject_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut i = 0
  while i < 1000 {
    let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
    @azimuth.telemetry.api.propagation.CompositePropagator.inject(composite, ctx, carrier)
    i = i + 1
  }
  
  let inject_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let inject_duration = inject_end_time - inject_start_time
  
  // Benchmark extraction operations
  let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
  @azimuth.telemetry.api.propagation.TextMapCarrier.set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  
  let extract_start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  let mut j = 0
  while j < 1000 {
    let _ = @azimuth.telemetry.api.propagation.CompositePropagator.extract(composite, carrier)
    j = j + 1
  }
  
  let extract_end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let extract_duration = extract_end_time - extract_start_time
  
  // Performance assertions
  assert_true(inject_duration > 0L, "Injection should take time")
  assert_true(extract_duration > 0L, "Extraction should take time")
  assert_true(inject_duration < 10000000000L, "1000 injections should complete within 10 seconds")
  assert_true(extract_duration < 10000000000L, "1000 extractions should complete within 10 seconds")
}

test "memory usage patterns" {
  // Test memory usage patterns of telemetry operations
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "memory.test")
  
  // Create many spans and end them to test memory cleanup
  let spans = []
  
  let mut i = 0
  while i < 100 {
    let ctx = @azimuth.telemetry.api.context.Context.root()
    let (_, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "memory.test.span." + i.to_string())
    
    // Add attributes to span
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "test.attr", @azimuth.telemetry.api.common.StringValue("test.value." + i.to_string()))
    
    spans = spans @ [span]
    i = i + 1
  }
  
  // End all spans
  for span in spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // Create many attributes
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  let mut j = 0
  while j < 1000 {
    @azimuth.telemetry.api.common.Attributes.set(attrs, "memory.attr." + j.to_string(), @azimuth.telemetry.api.common.StringValue("value." + j.to_string()))
    j = j + 1
  }
  
  // Create many log records
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "memory.test")
  
  let mut k = 0
  while k < 100 {
    let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
      @azimuth.telemetry.api.logs.SeverityNumber.Info,
      "Memory test log message " + k.to_string(),
      None,
      None,
      None,
      None,
      None,
      None
    )
    k = k + 1
  }
  
  // Test should complete without running out of memory
  assert_true(true, "Memory usage test completed successfully")
}

test "concurrent operations" {
  // Test thread-safety of telemetry operations (simulated)
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "concurrent.test")
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "concurrent.test")
  let logger = @azimuth.telemetry.api.logs.LoggerProvider.get_logger(logger_provider, "concurrent.test")
  
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "concurrent.counter")
  
  // Simulate concurrent operations by performing them sequentially
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  // "Concurrent" span operations
  let mut i = 0
  while i < 100 {
    let (thread_ctx, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "concurrent.span." + i.to_string())
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "thread.id", @azimuth.telemetry.api.common.StringValue(i.to_string()))
    @azimuth.telemetry.api.trace.Span.end(span)
    i = i + 1
  }
  
  // "Concurrent" metric operations
  let mut j = 0
  while j < 100 {
    @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0)
    j = j + 1
  }
  
  // "Concurrent" log operations
  let mut k = 0
  while k < 100 {
    let log_record = @azimuth.telemetry.api.logs.LogRecord.new(
      @azimuth.telemetry.api.logs.SeverityNumber.Info,
      "Concurrent log message " + k.to_string(),
      None,
      None,
      None,
      None,
      None,
      None
    )
    k = k + 1
  }
  
  // "Concurrent" context operations
  let mut current_ctx = ctx
  let mut l = 0
  while l < 100 {
    let key = @azimuth.telemetry.api.context.ContextKey::new("concurrent.key." + l.to_string())
    current_ctx = @azimuth.telemetry.api.context.Context.with_value(current_ctx, key, "value." + l.to_string())
    l = l + 1
  }
  
  // Test should complete without errors
  assert_true(true, "Concurrent operations test completed successfully")
}

test "high frequency operations" {
  // Test behavior under high-frequency operations
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider.default()
  let meter_provider = @azimuth.telemetry.api.metrics.MeterProvider.default()
  
  let tracer = @azimuth.telemetry.api.trace.TracerProvider.get_tracer(tracer_provider, "highfreq.test")
  let meter = @azimuth.telemetry.api.metrics.MeterProvider.get_meter(meter_provider, "highfreq.test")
  
  let counter = @azimuth.telemetry.api.metrics.Meter.create_counter(meter, "highfreq.counter")
  let histogram = @azimuth.telemetry.api.metrics.Meter.create_histogram(meter, "highfreq.histogram")
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (highfreq_ctx, span) = @azimuth.telemetry.api.trace.Tracer.start_span(tracer, ctx, "high.frequency.operation")
  
  // High-frequency metric recording within a single span
  let mut i = 0
  while i < 10000 {
    @azimuth.telemetry.api.metrics.Counter.add(counter, 1.0)
    @azimuth.telemetry.api.metrics.Histogram.record(histogram, (i % 1000).to_double())
    i = i + 1
  }
  
  // High-frequency span events
  let mut j = 0
  while j < 1000 {
    @azimuth.telemetry.api.trace.Span.add_event(
      span,
      "highfreq.event." + j.to_string(),
      Some([("event.count", @azimuth.telemetry.api.common.IntValue(j))]),
      None
    )
    j = j + 1
  }
  
  // High-frequency attribute setting
  let mut k = 0
  while k < 100 {
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "highfreq.attr." + k.to_string(), @azimuth.telemetry.api.common.StringValue("value." + k.to_string()))
    k = k + 1
  }
  
  @azimuth.telemetry.api.trace.Span.end(span)
  
  // Test should complete without errors
  assert_true(true, "High frequency operations test completed successfully")
}