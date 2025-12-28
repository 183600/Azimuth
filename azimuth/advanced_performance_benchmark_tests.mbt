// Advanced Performance Benchmark Tests for Azimuth
// Tests performance characteristics and benchmarks under various conditions

test "span_creation_performance_benchmark" {
  // Benchmark span creation performance
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "performance-benchmark")
  
  // Measure span creation time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create multiple spans for benchmarking
  let span1 = Tracer::start_span(tracer, "benchmark-span-1")
  let span2 = Tracer::start_span(tracer, "benchmark-span-2")
  let span3 = Tracer::start_span(tracer, "benchmark-span-3")
  let span4 = Tracer::start_span(tracer, "benchmark-span-4")
  let span5 = Tracer::start_span(tracer, "benchmark-span-5")
  let span6 = Tracer::start_span(tracer, "benchmark-span-6")
  let span7 = Tracer::start_span(tracer, "benchmark-span-7")
  let span8 = Tracer::start_span(tracer, "benchmark-span-8")
  let span9 = Tracer::start_span(tracer, "benchmark-span-9")
  let span10 = Tracer::start_span(tracer, "benchmark-span-10")
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let creation_duration = end_time - start_time
  
  // Verify all spans are created correctly
  assert_eq(Span::name(span1), "benchmark-span-1")
  assert_eq(Span::name(span2), "benchmark-span-2")
  assert_eq(Span::name(span3), "benchmark-span-3")
  assert_eq(Span::name(span4), "benchmark-span-4")
  assert_eq(Span::name(span5), "benchmark-span-5")
  assert_eq(Span::name(span6), "benchmark-span-6")
  assert_eq(Span::name(span7), "benchmark-span-7")
  assert_eq(Span::name(span8), "benchmark-span-8")
  assert_eq(Span::name(span9), "benchmark-span-9")
  assert_eq(Span::name(span10), "benchmark-span-10")
  
  // Verify all spans are recording
  assert_eq(Span::is_recording(span1), true)
  assert_eq(Span::is_recording(span2), true)
  assert_eq(Span::is_recording(span3), true)
  assert_eq(Span::is_recording(span4), true)
  assert_eq(Span::is_recording(span5), true)
  assert_eq(Span::is_recording(span6), true)
  assert_eq(Span::is_recording(span7), true)
  assert_eq(Span::is_recording(span8), true)
  assert_eq(Span::is_recording(span9), true)
  assert_eq(Span::is_recording(span10), true)
  
  // End all spans
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  Span::end(span4)
  Span::end(span5)
  Span::end(span6)
  Span::end(span7)
  Span::end(span8)
  Span::end(span9)
  Span::end(span10)
  
  // Performance assertion - creation should complete in reasonable time
  @assertion.assert_true(creation_duration < 1000000000L)? // Less than 1 second
}

test "metrics_recording_performance_benchmark" {
  // Benchmark metrics recording performance
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "metrics-benchmark")
  
  // Create multiple metric instruments
  let counter = Meter::create_counter(meter, "benchmark_counter", Some("Benchmark counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "benchmark_histogram", Some("Benchmark histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "benchmark_gauge", Some("Benchmark gauge"), Some("items"))
  let updown_counter = Meter::create_updown_counter(meter, "benchmark_updown", Some("Benchmark updown counter"), Some("items"))
  
  // Measure metrics recording time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Record multiple metrics
  Counter::add(counter, 100.0)
  Counter::add(counter, 200.0)
  Counter::add(counter, 300.0)
  Counter::add(counter, 400.0)
  Counter::add(counter, 500.0)
  
  Histogram::record(histogram, 100.5)
  Histogram::record(histogram, 200.75)
  Histogram::record(histogram, 300.25)
  Histogram::record(histogram, 400.5)
  Histogram::record(histogram, 500.75)
  
  UpDownCounter::add(updown_counter, 1000.0)
  UpDownCounter::add(updown_counter, -500.0)
  UpDownCounter::add(updown_counter, 250.0)
  UpDownCounter::add(updown_counter, -100.0)
  UpDownCounter::add(updown_counter, 350.0)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let recording_duration = end_time - start_time
  
  // Verify metric instrument properties
  assert_eq(counter.name, "benchmark_counter")
  assert_eq(counter.description, Some("Benchmark counter"))
  assert_eq(counter.unit, Some("count"))
  
  assert_eq(histogram.name, "benchmark_histogram")
  assert_eq(histogram.description, Some("Benchmark histogram"))
  assert_eq(histogram.unit, Some("ms"))
  
  assert_eq(gauge.name, "benchmark_gauge")
  assert_eq(gauge.description, Some("Benchmark gauge"))
  assert_eq(gauge.unit, Some("items"))
  
  assert_eq(updown_counter.name, "benchmark_updown")
  assert_eq(updown_counter.description, Some("Benchmark updown counter"))
  assert_eq(updown_counter.unit, Some("items"))
  
  // Performance assertion - recording should complete in reasonable time
  @assertion.assert_true(recording_duration < 1000000000L)? // Less than 1 second
}

test "logging_performance_benchmark" {
  // Benchmark logging performance
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "logging-benchmark")
  
  // Measure log creation time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create multiple log records
  let log1 = LogRecord::new(Trace, "Trace message 1")
  let log2 = LogRecord::new(Debug, "Debug message 2")
  let log3 = LogRecord::new(Info, "Info message 3")
  let log4 = LogRecord::new(Warn, "Warning message 4")
  let log5 = LogRecord::new(Error, "Error message 5")
  let log6 = LogRecord::new(Fatal, "Fatal message 6")
  
  // Create log records with full context
  let log7 = LogRecord::new_with_context(
    Info,
    Some("Contextual log 7"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace_perf_7"),
    Some("span_perf_7"),
    None
  )
  
  let log8 = LogRecord::new_with_context(
    Error,
    Some("Contextual error 8"),
    None,
    Some(1735689600000002000L),
    Some(1735689600000003000L),
    Some("trace_perf_8"),
    Some("span_perf_8"),
    None
  )
  
  let log9 = LogRecord::new_with_context(
    Warn,
    Some("Contextual warning 9"),
    None,
    Some(1735689600000004000L),
    Some(1735689600000005000L),
    Some("trace_perf_9"),
    Some("span_perf_9"),
    None
  )
  
  let log10 = LogRecord::new_with_context(
    Info,
    Some("Contextual info 10"),
    None,
    Some(1735689600000006000L),
    Some(1735689600000007000L),
    Some("trace_perf_10"),
    Some("span_perf_10"),
    None
  )
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let logging_duration = end_time - start_time
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log1), Trace)
  assert_eq(LogRecord::severity_number(log2), Debug)
  assert_eq(LogRecord::severity_number(log3), Info)
  assert_eq(LogRecord::severity_number(log4), Warn)
  assert_eq(LogRecord::severity_number(log5), Error)
  assert_eq(LogRecord::severity_number(log6), Fatal)
  
  assert_eq(LogRecord::body(log1), Some("Trace message 1"))
  assert_eq(LogRecord::body(log2), Some("Debug message 2"))
  assert_eq(LogRecord::body(log3), Some("Info message 3"))
  assert_eq(LogRecord::body(log4), Some("Warning message 4"))
  assert_eq(LogRecord::body(log5), Some("Error message 5"))
  assert_eq(LogRecord::body(log6), Some("Fatal message 6"))
  
  assert_eq(LogRecord::body(log7), Some("Contextual log 7"))
  assert_eq(LogRecord::trace_id(log7), Some("trace_perf_7"))
  assert_eq(LogRecord::span_id(log7), Some("span_perf_7"))
  
  assert_eq(LogRecord::body(log8), Some("Contextual error 8"))
  assert_eq(LogRecord::trace_id(log8), Some("trace_perf_8"))
  assert_eq(LogRecord::span_id(log8), Some("span_perf_8"))
  
  // Performance assertion - logging should complete in reasonable time
  @assertion.assert_true(logging_duration < 1000000000L)? // Less than 1 second
}

test "context_operations_performance_benchmark" {
  // Benchmark context operations performance
  let root_ctx = Context::root()
  
  // Measure context operations time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create multiple context keys and values
  let key1 = ContextKey::new("perf_key_1")
  let key2 = ContextKey::new("perf_key_2")
  let key3 = ContextKey::new("perf_key_3")
  let key4 = ContextKey::new("perf_key_4")
  let key5 = ContextKey::new("perf_key_5")
  
  // Set values in context
  let ctx1 = Context::with_value(root_ctx, key1, "value1")
  let ctx2 = Context::with_value(ctx1, key2, "value2")
  let ctx3 = Context::with_value(ctx2, key3, "value3")
  let ctx4 = Context::with_value(ctx3, key4, "value4")
  let ctx5 = Context::with_value(ctx4, key5, "value5")
  
  // Get values from context
  let val1 = Context::get(ctx5, key1)
  let val2 = Context::get(ctx5, key2)
  let val3 = Context::get(ctx5, key3)
  let val4 = Context::get(ctx5, key4)
  let val5 = Context::get(ctx5, key5)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let context_duration = end_time - start_time
  
  // Verify context values
  assert_eq(val1, Some("value1"))
  assert_eq(val2, Some("value2"))
  assert_eq(val3, Some("value3"))
  assert_eq(val4, Some("value4"))
  assert_eq(val5, Some("value5"))
  
  // Performance assertion - context operations should complete in reasonable time
  @assertion.assert_true(context_duration < 1000000000L)? // Less than 1 second
}

test "propagation_performance_benchmark" {
  // Benchmark propagation operations performance
  let trace_propagator = W3CTraceContextPropagator::new()
  let composite = CompositePropagator::new([trace_propagator])
  
  // Measure propagation operations time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create multiple carriers and contexts
  let ctx1 = Context::root()
  let ctx2 = Context::with_value(Context::root(), ContextKey::new("test2"), "value2")
  let ctx3 = Context::with_value(Context::root(), ContextKey::new("test3"), "value3")
  let ctx4 = Context::with_value(Context::root(), ContextKey::new("test4"), "value4")
  let ctx5 = Context::with_value(Context::root(), ContextKey::new("test5"), "value5")
  
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  let carrier4 = TextMapCarrier::new()
  let carrier5 = TextMapCarrier::new()
  
  // Inject contexts into carriers
  CompositePropagator::inject(composite, ctx1, carrier1)
  CompositePropagator::inject(composite, ctx2, carrier2)
  CompositePropagator::inject(composite, ctx3, carrier3)
  CompositePropagator::inject(composite, ctx4, carrier4)
  CompositePropagator::inject(composite, ctx5, carrier5)
  
  // Extract contexts from carriers
  let extracted_ctx1 = CompositePropagator::extract(composite, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite, carrier3)
  let extracted_ctx4 = CompositePropagator::extract(composite, carrier4)
  let extracted_ctx5 = CompositePropagator::extract(composite, carrier5)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let propagation_duration = end_time - start_time
  
  // Verify propagation worked
  let extracted_key = ContextKey::new("extracted")
  assert_eq(Context::get(extracted_ctx1, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx2, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx3, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx4, extracted_key), Some("true"))
  assert_eq(Context::get(extracted_ctx5, extracted_key), Some("true"))
  
  // Performance assertion - propagation operations should complete in reasonable time
  @assertion.assert_true(propagation_duration < 1000000000L)? // Less than 1 second
}

test "memory_allocation_benchmark" {
  // Benchmark memory allocation patterns
  
  // Measure memory allocation time for creating many telemetry objects
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create many spans
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "memory-benchmark")
  
  let spans = [
    Tracer::start_span(tracer, "mem-span-1"),
    Tracer::start_span(tracer, "mem-span-2"),
    Tracer::start_span(tracer, "mem-span-3"),
    Tracer::start_span(tracer, "mem-span-4"),
    Tracer::start_span(tracer, "mem-span-5")
  ]
  
  // Create many metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "memory-metrics")
  
  let counters = [
    Meter::create_counter(meter, "mem-counter-1", None, None),
    Meter::create_counter(meter, "mem-counter-2", None, None),
    Meter::create_counter(meter, "mem-counter-3", None, None),
    Meter::create_counter(meter, "mem-counter-4", None, None),
    Meter::create_counter(meter, "mem-counter-5", None, None)
  ]
  
  // Create many log records
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "memory-logger")
  
  let logs = [
    LogRecord::new(Info, "Memory test log 1"),
    LogRecord::new(Info, "Memory test log 2"),
    LogRecord::new(Info, "Memory test log 3"),
    LogRecord::new(Info, "Memory test log 4"),
    LogRecord::new(Info, "Memory test log 5")
  ]
  
  // Create many contexts
  let contexts = [
    Context::with_value(Context::root(), ContextKey::new("mem_ctx_1"), "value1"),
    Context::with_value(Context::root(), ContextKey::new("mem_ctx_2"), "value2"),
    Context::with_value(Context::root(), ContextKey::new("mem_ctx_3"), "value3"),
    Context::with_value(Context::root(), ContextKey::new("mem_ctx_4"), "value4"),
    Context::with_value(Context::root(), ContextKey::new("mem_ctx_5"), "value5")
  ]
  
  // Create many baggage entries
  let baggages = [
    Baggage::set_entry(Baggage::new(), "mem.baggage.1", "baggage_value_1"),
    Baggage::set_entry(Baggage::new(), "mem.baggage.2", "baggage_value_2"),
    Baggage::set_entry(Baggage::new(), "mem.baggage.3", "baggage_value_3"),
    Baggage::set_entry(Baggage::new(), "mem.baggage.4", "baggage_value_4"),
    Baggage::set_entry(Baggage::new(), "mem.baggage.5", "baggage_value_5")
  ]
  
  // Create many resources
  let resources = [
    Resource::with_attributes(Resource::new(), [("mem.test", StringValue("resource-1"))]),
    Resource::with_attributes(Resource::new(), [("mem.test", StringValue("resource-2"))]),
    Resource::with_attributes(Resource::new(), [("mem.test", StringValue("resource-3"))]),
    Resource::with_attributes(Resource::new(), [("mem.test", StringValue("resource-4"))]),
    Resource::with_attributes(Resource::new(), [("mem.test", StringValue("resource-5"))])
  ]
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let allocation_duration = end_time - start_time
  
  // Verify all objects are properly created
  assert_eq(spans.length, 5)
  assert_eq(counters.length, 5)
  assert_eq(logs.length, 5)
  assert_eq(contexts.length, 5)
  assert_eq(baggages.length, 5)
  assert_eq(resources.length, 5)
  
  // Clean up all spans
  Span::end(spans[0])
  Span::end(spans[1])
  Span::end(spans[2])
  Span::end(spans[3])
  Span::end(spans[4])
  
  // Performance assertion - memory allocation should complete in reasonable time
  @assertion.assert_true(allocation_duration < 2000000000L)? // Less than 2 seconds
}

test "resource_cleanup_performance_benchmark" {
  // Benchmark resource cleanup performance
  
  // Create many telemetry objects for cleanup testing
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "cleanup-benchmark")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "cleanup-metrics")
  
  // Create many spans
  let span1 = Tracer::start_span(tracer, "cleanup-span-1")
  let span2 = Tracer::start_span(tracer, "cleanup-span-2")
  let span3 = Tracer::start_span(tracer, "cleanup-span-3")
  let span4 = Tracer::start_span(tracer, "cleanup-span-4")
  let span5 = Tracer::start_span(tracer, "cleanup-span-5")
  let span6 = Tracer::start_span(tracer, "cleanup-span-6")
  let span7 = Tracer::start_span(tracer, "cleanup-span-7")
  let span8 = Tracer::start_span(tracer, "cleanup-span-8")
  let span9 = Tracer::start_span(tracer, "cleanup-span-9")
  let span10 = Tracer::start_span(tracer, "cleanup-span-10")
  
  // Measure cleanup time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // End all spans (cleanup)
  Span::end(span1)
  Span::end(span2)
  Span::end(span3)
  Span::end(span4)
  Span::end(span5)
  Span::end(span6)
  Span::end(span7)
  Span::end(span8)
  Span::end(span9)
  Span::end(span10)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let cleanup_duration = end_time - start_time
  
  // Verify spans are properly ended
  assert_eq(Span::name(span1), "cleanup-span-1")
  assert_eq(Span::name(span2), "cleanup-span-2")
  assert_eq(Span::name(span3), "cleanup-span-3")
  assert_eq(Span::name(span4), "cleanup-span-4")
  assert_eq(Span::name(span5), "cleanup-span-5")
  assert_eq(Span::name(span6), "cleanup-span-6")
  assert_eq(Span::name(span7), "cleanup-span-7")
  assert_eq(Span::name(span8), "cleanup-span-8")
  assert_eq(Span::name(span9), "cleanup-span-9")
  assert_eq(Span::name(span10), "cleanup-span-10")
  
  // Performance assertion - cleanup should complete in reasonable time
  @assertion.assert_true(cleanup_duration < 1000000000L)? // Less than 1 second
}

test "concurrent_operations_performance_benchmark" {
  // Benchmark concurrent operations performance
  
  // Create providers
  let trace_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(trace_provider, "concurrent-benchmark")
  
  let metrics_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(metrics_provider, "concurrent-metrics")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent-logger")
  
  // Measure concurrent operations time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Concurrent span operations
  let span_a = Tracer::start_span(tracer, "concurrent-span-a")
  let span_b = Tracer::start_span(tracer, "concurrent-span-b")
  let span_c = Tracer::start_span(tracer, "concurrent-span-c")
  
  // Concurrent metric operations
  let counter = Meter::create_counter(meter, "concurrent_counter", None, None)
  let histogram = Meter::create_histogram(meter, "concurrent_histogram", None, None)
  let gauge = Meter::create_gauge(meter, "concurrent_gauge", None, None)
  
  Counter::add(counter, 100.0)
  Histogram::record(histogram, 200.0)
  
  // Concurrent logging operations
  let log_a = LogRecord::new(Info, "Concurrent log A")
  let log_b = LogRecord::new(Warn, "Concurrent log B")
  let log_c = LogRecord::new(Error, "Concurrent log C")
  
  // Concurrent context operations
  let ctx_a = Context::with_value(Context::root(), ContextKey::new("concurrent_a"), "value_a")
  let ctx_b = Context::with_value(Context::root(), ContextKey::new("concurrent_b"), "value_b")
  let ctx_c = Context::with_value(Context::root(), ContextKey::new("concurrent_c"), "value_c")
  
  // Concurrent baggage operations
  let baggage_a = Baggage::set_entry(Baggage::new(), "concurrent.baggage.a", "value_a")
  let baggage_b = Baggage::set_entry(Baggage::new(), "concurrent.baggage.b", "value_b")
  let baggage_c = Baggage::set_entry(Baggage::new(), "concurrent.baggage.c", "value_c")
  
  // Concurrent resource operations
  let resource_a = Resource::with_attributes(Resource::new(), [("concurrent.test", StringValue("resource-a"))])
  let resource_b = Resource::with_attributes(Resource::new(), [("concurrent.test", StringValue("resource-b"))])
  let resource_c = Resource::with_attributes(Resource::new(), [("concurrent.test", StringValue("resource-c"))])
  
  // End spans
  Span::end(span_a)
  Span::end(span_b)
  Span::end(span_c)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let concurrent_duration = end_time - start_time
  
  // Verify all operations completed successfully
  assert_eq(Span::name(span_a), "concurrent-span-a")
  assert_eq(Span::name(span_b), "concurrent-span-b")
  assert_eq(Span::name(span_c), "concurrent-span-c")
  
  assert_eq(counter.name, "concurrent_counter")
  assert_eq(histogram.name, "concurrent_histogram")
  assert_eq(gauge.name, "concurrent_gauge")
  
  assert_eq(LogRecord::body(log_a), Some("Concurrent log A"))
  assert_eq(LogRecord::body(log_b), Some("Concurrent log B"))
  assert_eq(LogRecord::body(log_c), Some("Concurrent log C"))
  
  assert_eq(Context::get(ctx_a, ContextKey::new("concurrent_a")), Some("value_a"))
  assert_eq(Context::get(ctx_b, ContextKey::new("concurrent_b")), Some("value_b"))
  assert_eq(Context::get(ctx_c, ContextKey::new("concurrent_c")), Some("value_c"))
  
  assert_eq(Baggage::get_entry(baggage_a, "concurrent.baggage.a"), Some("value_a"))
  assert_eq(Baggage::get_entry(baggage_b, "concurrent.baggage.b"), Some("value_b"))
  assert_eq(Baggage::get_entry(baggage_c, "concurrent.baggage.c"), Some("value_c"))
  
  assert_eq(Resource::get_attribute(resource_a, "concurrent.test"), Some(StringValue("resource-a")))
  assert_eq(Resource::get_attribute(resource_b, "concurrent.test"), Some(StringValue("resource-b")))
  assert_eq(Resource::get_attribute(resource_c, "concurrent.test"), Some(StringValue("resource-c")))
  
  // Performance assertion - concurrent operations should complete in reasonable time
  @assertion.assert_true(concurrent_duration < 1500000000L)? // Less than 1.5 seconds
}

test "large_data_handling_performance_benchmark" {
  // Benchmark performance with large data sets
  
  // Create large attribute values
  let large_text = "This is a very large text value that is used to test performance when handling large amounts of data in telemetry attributes. ".repeat(10)
  let large_string_attr = StringValue(large_text)
  
  // Create large array values
  let large_string_array = ArrayStringValue([
    "item1", "item2", "item3", "item4", "item5",
    "item6", "item7", "item8", "item9", "item10"
  ])
  
  let large_int_array = ArrayIntValue([
    1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  ])
  
  // Measure large data operations time
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // Create resource with large attributes
  let large_attrs = [
    ("large.text.attribute", large_string_attr),
    ("large.array.string", large_string_array),
    ("large.array.int", large_int_array)
  ]
  
  let large_resource = Resource::with_attributes(Resource::new(), large_attrs)
  
  // Create log with large body
  let large_log = LogRecord::new(Info, large_text)
  
  // Create span with events containing large data
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "large-data-benchmark")
  let large_span = Tracer::start_span(tracer, "large-data-span")
  
  let large_event_attrs = [
    ("large.event.data", large_string_attr)
  ]
  
  Span::add_event(large_span, "large event", Some(large_event_attrs))
  Span::end(large_span)
  
  // Create baggage with large entries
  let large_baggage = Baggage::set_entry(Baggage::new(), "large.baggage.key", large_text)
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  let large_data_duration = end_time - start_time
  
  // Verify large data handling
  match large_string_attr {
    StringValue(s) => assert_eq(s, large_text)
    _ => @test.fail("Expected StringValue")
  }
  
  match large_string_array {
    ArrayStringValue(arr) => assert_eq(arr.length, 10)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match large_int_array {
    ArrayIntValue(arr) => assert_eq(arr.length, 10)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  assert_eq(Resource::get_attribute(large_resource, "large.text.attribute"), Some(large_string_attr))
  assert_eq(LogRecord::body(large_log), Some(large_text))
  assert_eq(Baggage::get_entry(large_baggage, "large.baggage.key"), Some(large_text))
  
  // Performance assertion - large data operations should complete in reasonable time
  @assertion.assert_true(large_data_duration < 2000000000L)? // Less than 2 seconds
}