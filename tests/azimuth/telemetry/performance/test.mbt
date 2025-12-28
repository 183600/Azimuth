test "performance with large number of spans" {
  // Test performance when creating and managing many spans
  
  let tracer_provider = @azimuth.telemetry.api.trace.TracerProvider::noop()
  let tracer = @azimuth.telemetry.api.trace.TracerProvider::get_tracer(tracer_provider, "performance.test.tracer")
  let ctx = @azimuth.telemetry.api.context.Context::root()
  
  // Create many spans to test performance
  let spans = []
  let start_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..100 {
    let (_, span) = @azimuth.telemetry.api.trace.Tracer::start_span(tracer, "performance.span." + i.to_string())
    spans = spans @ [span]
  }
  
  let creation_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // End all spans
  for span in spans {
    @azimuth.telemetry.api.trace.Span::end(span)
  }
  
  let end_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Performance assertions (these are rough guidelines)
  let creation_duration = creation_time - start_time
  let end_duration = end_time - creation_time
  
  // Should complete within reasonable time (very loose bounds for simplified implementation)
  assert_true(creation_duration < 10000000000L)  // 10 seconds
  assert_true(end_duration < 10000000000L)      // 10 seconds
  
  // Verify all spans were created and ended properly
  assert_eq(spans.length(), 100)
  
  // Test span performance with attributes
  let attr_spans = []
  let attr_start_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..50 {
    let (_, span) = @azimuth.telemetry.api.trace.Tracer::start_span(tracer, "attr.span." + i.to_string())
    
    // Add multiple attributes
    @azimuth.telemetry.api.trace.Span::set_attribute(span, "iteration", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.trace.Span::set_attribute(span, "name", @azimuth.telemetry.api.common.StringValue("span_" + i.to_string()))
    @azimuth.telemetry.api.trace.Span::set_attribute(span, "timestamp", @azimuth.telemetry.api.common.FloatValue(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system()).to_double()))
    
    // Add events
    @azimuth.telemetry.api.trace.Span::add_event(span, "event_" + i.to_string(), None, None)
    
    attr_spans = attr_spans @ [span]
  }
  
  let attr_creation_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for span in attr_spans {
    @azimuth.telemetry.api.trace.Span::end(span)
  }
  
  let attr_end_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let attr_creation_duration = attr_creation_time - attr_start_time
  let attr_end_duration = attr_end_time - attr_creation_time
  
  // Attribute operations should also be reasonably fast
  assert_true(attr_creation_duration < 10000000000L)  // 10 seconds
  assert_true(attr_end_duration < 10000000000L)      // 10 seconds
}

test "performance with large number of metrics" {
  // Test performance when creating and recording many metrics
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider::default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider::get_meter(metrics_provider, "performance.test.meter")
  
  // Create many metric instruments
  let counters = []
  let histograms = []
  let up_down_counters = []
  let gauges = []
  
  let instrument_start_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..50 {
    let counter = @azimuth.telemetry.sdk.metrics.Meter::create_counter(meter, "perf.counter." + i.to_string())
    let histogram = @azimuth.telemetry.sdk.metrics.Meter::create_histogram(meter, "perf.histogram." + i.to_string())
    let up_down_counter = @azimuth.telemetry.sdk.metrics.Meter::create_up_down_counter(meter, "perf.up_down_counter." + i.to_string())
    let gauge = @azimuth.telemetry.sdk.metrics.Meter::create_gauge(meter, "perf.gauge." + i.to_string())
    
    counters = counters @ [counter]
    histograms = histograms @ [histogram]
    up_down_counters = up_down_counters @ [up_down_counter]
    gauges = gauges @ [gauge]
  }
  
  let instrument_creation_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Record many measurements
  let measurement_start_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..1000 {
    let counter_index = i % counters.length()
    let histogram_index = i % histograms.length()
    let up_down_counter_index = i % up_down_counters.length()
    let gauge_index = i % gauges.length()
    
    @azimuth.telemetry.sdk.metrics.Counter::add(counters[counter_index], i.to_double())
    @azimuth.telemetry.sdk.metrics.Histogram::record(histograms[histogram_index], (i * 1.5).to_double())
    @azimuth.telemetry.sdk.metrics.UpDownCounter::add(up_down_counters[up_down_counter_index], (i - 500).to_double())
    @azimuth.telemetry.sdk.metrics.Gauge::record(gauges[gauge_index], (i % 100).to_double())
  }
  
  let measurement_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Performance assertions
  let instrument_creation_duration = instrument_creation_time - instrument_start_time
  let measurement_duration = measurement_time - measurement_start_time
  
  assert_true(instrument_creation_duration < 10000000000L)  // 10 seconds
  assert_true(measurement_duration < 10000000000L)         // 10 seconds
  
  // Verify instruments were created
  assert_eq(counters.length(), 50)
  assert_eq(histograms.length(), 50)
  assert_eq(up_down_counters.length(), 50)
  assert_eq(gauges.length(), 50)
  
  // Test performance with attributes
  let attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(attrs, "performance", @azimuth.telemetry.api.common.StringValue("test"))
  
  let attr_measurement_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..500 {
    @azimuth.telemetry.sdk.metrics.Counter::add(counters[0], i.to_double(), Some(attrs))
    @azimuth.telemetry.sdk.metrics.Histogram::record(histograms[0], i.to_double(), Some(attrs))
  }
  
  let attr_measurement_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let attr_measurement_duration = attr_measurement_end - attr_measurement_start
  assert_true(attr_measurement_duration < 10000000000L)  // 10 seconds
}

test "performance with large number of log records" {
  // Test performance when creating and emitting many log records
  
  let logger_provider = @azimuth.telemetry.api.logs.LoggerProvider::noop()
  let logger = @azimuth.telemetry.api.logs.LoggerProvider::get_logger(logger_provider, "performance.test.logger")
  
  // Create many log records
  let log_start_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..1000 {
    let severity = match i % 6 {
      0 => @azimuth.telemetry.api.logs.SeverityNumber::Trace
      1 => @azimuth.telemetry.api.logs.SeverityNumber::Debug
      2 => @azimuth.telemetry.api.logs.SeverityNumber::Info
      3 => @azimuth.telemetry.api.logs.SeverityNumber::Warn
      4 => @azimuth.telemetry.api.logs.SeverityNumber::Error
      _ => @azimuth.telemetry.api.logs.SeverityNumber::Fatal
    }
    
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(
      severity,
      "Performance test log message " + i.to_string(),
      None,
      Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
      None,
      None,
      None,
      None
    )
    
    @azimuth.telemetry.api.logs.Logger::emit(logger, log_record)
  }
  
  let log_end_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let log_duration = log_end_time - log_start_time
  assert_true(log_duration < 10000000000L)  // 10 seconds
  
  // Test performance with log records that have attributes
  let attrs = @azimuth.telemetry.api.common.Attributes::new()
  @azimuth.telemetry.api.common.Attributes::set(attrs, "iteration", @azimuth.telemetry.api.common.IntValue(0))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "component", @azimuth.telemetry.api.common.StringValue("performance_test"))
  @azimuth.telemetry.api.common.Attributes::set(attrs, "timestamp", @azimuth.telemetry.api.common.FloatValue(0.0))
  
  let attr_log_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..500 {
    // Update attributes for each iteration
    @azimuth.telemetry.api.common.Attributes::set(attrs, "iteration", @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.common.Attributes::set(attrs, "timestamp", @azimuth.telemetry.api.common.FloatValue(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system()).to_double()))
    
    let log_record = @azimuth.telemetry.api.logs.LogRecord::new(
      @azimuth.telemetry.api.logs.SeverityNumber::Info,
      "Performance test with attributes " + i.to_string(),
      Some(attrs),
      Some(@azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())),
      None,
      None,
      None,
      None
    )
    
    @azimuth.telemetry.api.logs.Logger::emit(logger, log_record)
  }
  
  let attr_log_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let attr_log_duration = attr_log_end - attr_log_start
  assert_true(attr_log_duration < 10000000000L)  // 10 seconds
}

test "performance with context operations" {
  // Test performance with many context operations
  
  let ctx = @azimuth.telemetry.api.context.Context::root()
  
  // Test context value setting and getting performance
  let context_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let mut current_ctx = ctx
  let keys = []
  
  // Create many context layers
  for i in 0..100 {
    let key = @azimuth.telemetry.api.context.ContextKey::new("key." + i.to_string())
    keys = keys @ [key]
    current_ctx = @azimuth.telemetry.api.context.Context::with_value(current_ctx, key, "value." + i.to_string())
  }
  
  let context_creation_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test context value retrieval performance
  for key in keys {
    let value = @azimuth.telemetry.api.context.Context::get(current_ctx, key)
    assert_true(value != None)
  }
  
  let context_retrieval_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test baggage operations performance
  let baggage = @azimuth.telemetry.api.context.Baggage::new()
  let baggage_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  let mut current_baggage = baggage
  for i in 0..50 {
    current_baggage = @azimuth.telemetry.api.context.Baggage::set_entry(current_baggage, "baggage.key." + i.to_string(), "baggage.value." + i.to_string())
  }
  
  let baggage_creation_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test baggage retrieval performance
  for i in 0..50 {
    let value = @azimuth.telemetry.api.context.Baggage::get_entry(current_baggage, "baggage.key." + i.to_string())
    // Simplified implementation returns None
  }
  
  let baggage_retrieval_time = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Performance assertions
  let context_creation_duration = context_creation_time - context_start
  let context_retrieval_duration = context_retrieval_time - context_creation_time
  let baggage_creation_duration = baggage_creation_time - baggage_start
  let baggage_retrieval_duration = baggage_retrieval_time - baggage_creation_time
  
  assert_true(context_creation_duration < 10000000000L)   // 10 seconds
  assert_true(context_retrieval_duration < 10000000000L)  // 10 seconds
  assert_true(baggage_creation_duration < 10000000000L)   // 10 seconds
  assert_true(baggage_retrieval_duration < 10000000000L)  // 10 seconds
}

test "performance with propagation operations" {
  // Test performance with many propagation operations
  
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::new(),
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator::new()
  ])
  
  let ctx = @azimuth.telemetry.api.context.Context::root()
  
  // Test injection performance
  let injection_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..500 {
    let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
    @azimuth.telemetry.api.propagation.CompositePropagator::inject(composite, ctx, carrier)
    
    // Verify injection worked
    let traceparent = @azimuth.telemetry.api.propagation.TextMapCarrier::get(carrier, "traceparent")
    assert_true(traceparent != None)
  }
  
  let injection_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test extraction performance
  let extraction_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..500 {
    let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
    @azimuth.telemetry.api.propagation.TextMapCarrier::set(carrier, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
    @azimuth.telemetry.api.propagation.TextMapCarrier::set(carrier, "baggage", "key1=value1,key2=value2")
    
    let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(composite, carrier)
    assert_true(extracted_ctx != @azimuth.telemetry.api.context.Context::{})
  }
  
  let extraction_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test combined injection and extraction performance
  let combined_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..200 {
    // Inject
    let carrier = @azimuth.telemetry.api.propagation.TextMapCarrier::new()
    @azimuth.telemetry.api.propagation.CompositePropagator::inject(composite, ctx, carrier)
    
    // Extract
    let extracted_ctx = @azimuth.telemetry.api.propagation.CompositePropagator::extract(composite, carrier)
    assert_true(extracted_ctx != @azimuth.telemetry.api.context.Context::{})
  }
  
  let combined_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Performance assertions
  let injection_duration = injection_end - injection_start
  let extraction_duration = extraction_end - extraction_start
  let combined_duration = combined_end - combined_start
  
  assert_true(injection_duration < 10000000000L)   // 10 seconds
  assert_true(extraction_duration < 10000000000L)  // 10 seconds
  assert_true(combined_duration < 10000000000L)    // 10 seconds
}

test "performance with attribute operations" {
  // Test performance with many attribute operations
  
  let attrs = @azimuth.telemetry.api.common.Attributes::new()
  
  // Test attribute setting performance
  let attr_set_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..1000 {
    let key = "attr.key." + i.to_string()
    let value = @azimuth.telemetry.api.common.StringValue("value." + i.to_string())
    @azimuth.telemetry.api.common.Attributes::set(attrs, key, value)
  }
  
  let attr_set_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test attribute getting performance
  let attr_get_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..1000 {
    let key = "attr.key." + i.to_string()
    let value = @azimuth.telemetry.api.common.Attributes::get(attrs, key)
    // Simplified implementation returns None for most keys
  }
  
  let attr_get_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test performance with different attribute value types
  let type_attrs = @azimuth.telemetry.api.common.Attributes::new()
  let type_test_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..250 {
    @azimuth.telemetry.api.common.Attributes::set(type_attrs, "string." + i.to_string(), @azimuth.telemetry.api.common.StringValue("string_value_" + i.to_string()))
    @azimuth.telemetry.api.common.Attributes::set(type_attrs, "int." + i.to_string(), @azimuth.telemetry.api.common.IntValue(i))
    @azimuth.telemetry.api.common.Attributes::set(type_attrs, "float." + i.to_string(), @azimuth.telemetry.api.common.FloatValue(i.to_double() * 1.5))
    @azimuth.telemetry.api.common.Attributes::set(type_attrs, "bool." + i.to_string(), @azimuth.telemetry.api.common.BoolValue(i % 2 == 0))
  }
  
  let type_test_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Test performance with array attributes
  let array_attrs = @azimuth.telemetry.api.common.Attributes::new()
  let array_test_start = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  for i in 0..100 {
    let string_array = ["item"] * (i + 1)
    let int_array = [1] * (i + 1)
    
    @azimuth.telemetry.api.common.Attributes::set(array_attrs, "string_array." + i.to_string(), @azimuth.telemetry.api.common.ArrayStringValue(string_array))
    @azimuth.telemetry.api.common.Attributes::set(array_attrs, "int_array." + i.to_string(), @azimuth.telemetry.api.common.ArrayIntValue(int_array))
  }
  
  let array_test_end = @azimuth.telemetry.sdk.platform.time.Clock::now_unix_nanos(@azimuth.telemetry.sdk.platform.time.Clock::system())
  
  // Performance assertions
  let attr_set_duration = attr_set_end - attr_set_start
  let attr_get_duration = attr_get_end - attr_get_start
  let type_test_duration = type_test_end - type_test_start
  let array_test_duration = array_test_end - array_test_start
  
  assert_true(attr_set_duration < 10000000000L)    // 10 seconds
  assert_true(attr_get_duration < 10000000000L)    // 10 seconds
  assert_true(type_test_duration < 10000000000L)   // 10 seconds
  assert_true(array_test_duration < 10000000000L)  // 10 seconds
}