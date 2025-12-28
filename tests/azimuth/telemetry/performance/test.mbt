test "high frequency span creation and termination" {
  // Test performance of high-frequency span creation and termination
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "perf.test.tracer")
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Create and end many spans rapidly
  for i in 0..10000 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "perf.span." + i.to_string())
    
    // Set some attributes
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "iteration", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "operation", @azimuth.telemetry.api.common.StringValue("perf_test"))
    
    // Add an event
    @azimuth.telemetry.api.trace.Span.add_event(span, "test.event", None, None)
    
    // End the span
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let total_duration = end_time - start_time
  let avg_duration_per_span = total_duration.to_double() / 10000.0
  
  // Log performance metrics
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "perf.test.logger")
  
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation", @azimuth.telemetry.api.common.StringValue("span_creation"))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_spans", @azimuth.telemetry.api.common.IntValue(10000))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_duration_ns", @azimuth.telemetry.api.common.IntValue(total_duration.to_int()))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "avg_duration_per_span_ns", @azimuth.telemetry.api.common.FloatValue(avg_duration_per_span))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Span creation performance test completed", perf_attrs)
  
  // Performance assertions (adjust thresholds based on expected performance)
  assert_true(avg_duration_per_span < 100000.0)  // Less than 100 microseconds per span
}

test "high frequency metric recording" {
  // Test performance of high-frequency metric recording
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "perf.test.meter")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "perf.test.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "perf.test.histogram")
  let gauge = @azimuth.telemetry.sdk.metrics.Meter.create_gauge(meter, "perf.test.gauge")
  let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter.create_up_down_counter(meter, "perf.test.up_down_counter")
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Record many metrics rapidly
  for i in 0..10000 {
    // Counter measurements
    @azimuth.telemetry.sdk.metrics.Counter.add(counter, i.to_double())
    
    // Histogram measurements
    @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, (i % 1000).to_double())
    
    // Gauge measurements
    @azimuth.telemetry.sdk.metrics.Gauge.record(gauge, (i % 100).to_double())
    
    // Up-down counter measurements
    @azimuth.telemetry.sdk.metrics.UpDownCounter.add(up_down_counter, (i % 21 - 10).to_double())
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let total_duration = end_time - start_time
  let avg_duration_per_measurement = total_duration.to_double() / 40000.0  // 4 measurements per iteration
  
  // Log performance metrics
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "perf.test.logger")
  
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation", @azimuth.telemetry.api.common.StringValue("metric_recording"))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_measurements", @azimuth.telemetry.api.common.IntValue(40000))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_duration_ns", @azimuth.telemetry.api.common.IntValue(total_duration.to_int()))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "avg_duration_per_measurement_ns", @azimuth.telemetry.api.common.FloatValue(avg_duration_per_measurement))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Metric recording performance test completed", perf_attrs)
  
  // Performance assertions
  assert_true(avg_duration_per_measurement < 50000.0)  // Less than 50 microseconds per measurement
}

test "high frequency logging" {
  // Test performance of high-frequency logging
  
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "perf.test.logger")
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Log many messages rapidly
  for i in 0..10000 {
    let message = "Performance test log message " + i.to_string()
    
    if i % 6 == 0 {
      @azimuth.telemetry.sdk.logs.Logger.trace(logger, message)
    } else if i % 6 == 1 {
      @azimuth.telemetry.sdk.logs.Logger.debug(logger, message)
    } else if i % 6 == 2 {
      @azimuth.telemetry.sdk.logs.Logger.info(logger, message)
    } else if i % 6 == 3 {
      @azimuth.telemetry.sdk.logs.Logger.warn(logger, message)
    } else if i % 6 == 4 {
      @azimuth.telemetry.sdk.logs.Logger.error(logger, message)
    } else {
      @azimuth.telemetry.sdk.logs.Logger.fatal(logger, message)
    }
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let total_duration = end_time - start_time
  let avg_duration_per_log = total_duration.to_double() / 10000.0
  
  // Log performance metrics
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation", @azimuth.telemetry.api.common.StringValue("logging"))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_logs", @azimuth.telemetry.api.common.IntValue(10000))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_duration_ns", @azimuth.telemetry.api.common.IntValue(total_duration.to_int()))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "avg_duration_per_log_ns", @azimuth.telemetry.api.common.FloatValue(avg_duration_per_log))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Logging performance test completed", perf_attrs)
  
  // Performance assertions
  assert_true(avg_duration_per_log < 75000.0)  // Less than 75 microseconds per log
}

test "context propagation performance" {
  // Test performance of context propagation operations
  
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let propagator = @azimuth.telemetry.api.propagation.CompositePropagator.new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator.new()
  ])
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Perform many inject/extract cycles
  for i in 0..10000 {
    let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier.new()
    
    // Add some context values
    let key1 = @azimuth.telemetry.api.context.ContextKey::new("test.key1")
    let key2 = @azimuth.telemetry.api.context.ContextKey::new("test.key2")
    let ctx_with_values = @azimuth.telemetry.api.context.Context.with_value(
      @azimuth.telemetry.api.context.Context.with_value(ctx, key1, "value1." + i.to_string()),
      key2,
      "value2." + i.to_string()
    )
    
    // Inject context
    @azimuth.telemetry.api.propagation.CompositePropagator.inject(propagator, ctx_with_values, carrier)
    
    // Extract context
    let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator.extract(propagator, carrier)
    
    // Get values from extracted context
    let _ = @azimuth.telemetry.api.context.Context.get(extracted_ctx, key1)
    let _ = @azimuth.telemetry.api.context.Context.get(extracted_ctx, key2)
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let total_duration = end_time - start_time
  let avg_duration_per_cycle = total_duration.to_double() / 10000.0
  
  // Log performance metrics
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "perf.test.logger")
  
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation", @azimuth.telemetry.api.common.StringValue("context_propagation"))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_cycles", @azimuth.telemetry.api.common.IntValue(10000))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_duration_ns", @azimuth.telemetry.api.common.IntValue(total_duration.to_int()))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "avg_duration_per_cycle_ns", @azimuth.telemetry.api.common.FloatValue(avg_duration_per_cycle))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Context propagation performance test completed", perf_attrs)
  
  // Performance assertions
  assert_true(avg_duration_per_cycle < 100000.0)  // Less than 100 microseconds per cycle
}

test "attributes handling performance" {
  // Test performance of attributes handling with large datasets
  
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  
  // Create and manipulate many attributes
  for i in 0..1000 {
    let attrs = @azimuth.telemetry.api.common.Attributes.new()
    
    // Set many attributes
    for j in 0..100 {
      let key = "attr." + j.to_string()
      let value = @azimuth.telemetry.api.common.StringValue("value." + i.to_string() + "." + j.to_string())
      @azimuth.telemetry.api.common.Attributes.set(attrs, key, value)
    }
    
    // Get some attributes
    for j in 0..10 {
      let key = "attr." + j.to_string()
      let _ = @azimuth.telemetry.api.common.Attributes.get(attrs, key)
    }
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock.now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock.system())
  let total_duration = end_time - start_time
  let avg_duration_per_attribute_set = total_duration.to_double() / 100000.0  // 100 attributes * 1000 iterations
  
  // Log performance metrics
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "perf.test.logger")
  
  let perf_attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "operation", @azimuth.telemetry.api.common.StringValue("attributes_handling"))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_attribute_sets", @azimuth.telemetry.api.common.IntValue(100000))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "total_duration_ns", @azimuth.telemetry.api.common.IntValue(total_duration.to_int()))
  @azimuth.telemetry.api.common.Attributes.set(perf_attrs, "avg_duration_per_set_ns", @azimuth.telemetry.api.common.FloatValue(avg_duration_per_attribute_set))
  
  @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Attributes handling performance test completed", perf_attrs)
  
  // Performance assertions
  assert_true(avg_duration_per_attribute_set < 25000.0)  // Less than 25 microseconds per attribute set
}

test "memory usage under load" {
  // Test memory usage patterns under high load
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "memory.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "memory.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "memory.test.logger")
  
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "memory.test.counter")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "memory.test.histogram")
  
  // Create many telemetry objects without ending them immediately
  let active_spans = []
  let large_attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  // Create large attribute set
  for i in 0..100 {
    let key = "memory.attr." + i.to_string()
    let value = @azimuth.telemetry.api.common.StringValue("memory.value." + i.to_string() + " with additional data to increase memory usage")
    @azimuth.telemetry.api.common.Attributes.set(large_attrs, key, value)
  }
  
  // Create many spans
  for i in 0..1000 {
    let (_, span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, "memory.test.span." + i.to_string())
    
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "iteration", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span.set_attribute(span, "large.data", @azimuth.telemetry.api.common.StringValue("x".repeat(1000)))
    
    // Add events with large data
    for j in 0..5 {
      let event_data = "Event data " + j.to_string() + " with additional content: " + "y".repeat(500)
      @azimuth.telemetry.api.trace.Span.add_event(span, "memory.event." + j.to_string(), Some([("data", @azimuth.telemetry.api.common.StringValue(event_data))]), None)
    }
    
    active_spans = active_spans @ [span]
    
    // Record metrics
    @azimuth.telemetry.sdk.metrics.Counter.add(counter, i.to_double(), Some(large_attrs))
    @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, (i % 1000).to_double(), Some(large_attrs))
    
    // Log messages
    @azimuth.telemetry.sdk.logs.Logger.info_with_attributes(logger, "Memory test log " + i.to_string(), large_attrs)
  }
  
  // Now end all spans
  for span in active_spans {
    @azimuth.telemetry.api.trace.Span.end(span)
  }
  
  // Log completion
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "Memory usage test completed")
}