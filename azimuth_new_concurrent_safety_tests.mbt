// Azimuth Telemetry System - New Concurrent Safety Tests
// This file contains new test cases for concurrent safety functionality

// Test 1: Concurrent Metric Updates
test "concurrent metric updates" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "concurrent_test_meter")
  
  // Create shared metrics
  let counter = Meter::create_counter(meter, "concurrent_counter", Some("Concurrent counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "concurrent_histogram", Some("Concurrent histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "concurrent_gauge", Some("Concurrent gauge"), Some("value"))
  
  // Simulate concurrent updates
  let num_threads = 10
  let updates_per_thread = 100
  
  // Create concurrent task for counter updates
  let counter_task = ConcurrentTask::spawn(|| {
    for i in 0..=updates_per_thread {
      Counter::add(counter, 1.0)
    }
  })
  
  // Create concurrent task for histogram updates
  let histogram_task = ConcurrentTask::spawn(|| {
    for i in 0..=updates_per_thread {
      let value = Int::to_float(i) * 1.5
      Histogram::record(histogram, value)
    }
  })
  
  // Create concurrent task for gauge updates
  let gauge_task = ConcurrentTask::spawn(|| {
    for i in 0..=updates_per_thread {
      let value = Int::to_float(i) * 0.1
      Gauge::set(gauge, value)
    }
  })
  
  // Wait for all tasks to complete
  ConcurrentTask::join(counter_task)
  ConcurrentTask::join(histogram_task)
  ConcurrentTask::join(gauge_task)
  
  // Verify final state consistency
  let counter_instrument = Counter::as_instrument(counter)
  let counter_data = Instrument::get_data(counter_instrument)
  
  match counter_data {
    Some(data) => {
      // Should have num_threads * (updates_per_thread + 1) updates
      let expected_count = Int::to_float(num_threads * (updates_per_thread + 1))
      assert_eq(data.value, expected_count)
    }
    None => assert_true(false)
  }
  
  // Verify histogram data consistency
  let histogram_instrument = Histogram::as_instrument(histogram)
  let histogram_data = Instrument::get_data(histogram_instrument)
  
  match histogram_data {
    Some(data) => {
      // Should have num_threads * (updates_per_thread + 1) measurements
      let expected_count = num_threads * (updates_per_thread + 1)
      assert_eq(data.count, expected_count)
      assert_true(data.mean > 0.0)
    }
    None => assert_true(false)
  }
  
  // Verify gauge final state
  let gauge_instrument = Gauge::as_instrument(gauge)
  let gauge_data = Instrument::get_data(gauge_instrument)
  
  match gauge_data {
    Some(data) => {
      // Final value should be from the last update
      let expected_final_value = Int::to_float(updates_per_thread) * 0.1
      assert_eq(data.value, expected_final_value)
    }
    None => assert_true(false)
  }
}

// Test 2: Concurrent Span Operations
test "concurrent span operations" {
  let trace_id = "concurrent_trace_1234567890abcdef"
  
  // Create multiple spans concurrently
  let num_spans = 20
  let mut span_tasks = []
  
  for i in 0..=num_spans {
    let span_id = "span_" + Int::to_string(i)
    let span_ctx = SpanContext::new(trace_id, span_id, true, "concurrent_state")
    let span = Span::new("concurrent_operation_" + Int::to_string(i), Internal, span_ctx)
    
    let task = ConcurrentTask::spawn(move || {
      // Perform span operations
      Span::add_event(span, "start_event", Some([("thread_id", IntValue(i))]))
      
      // Simulate some work
      for j in 0..=10 {
        let attrs = Attributes::new()
        Attributes::set(attrs, "iteration", IntValue(j))
        Attributes::set(attrs, "thread_id", IntValue(i))
        Span::add_event(span, "iteration_event", Some(attrs))
      }
      
      Span::set_status(span, Ok, Some("Concurrent operation completed"))
      Span::end(span)
    })
    
    span_tasks.push(task)
  }
  
  // Wait for all span tasks to complete
  for task in span_tasks {
    ConcurrentTask::join(task)
  }
  
  // Verify span data integrity
  let span_processor = SpanProcessor::default()
  let completed_spans = SpanProcessor::get_completed_spans(span_processor)
  
  // Should have all spans completed
  assert_eq(completed_spans.length(), num_spans + 1)
  
  // Verify trace ID consistency across all spans
  for span_data in completed_spans {
    assert_eq(span_data.trace_id, trace_id)
    assert_eq(span_data.status, Ok)
    assert_true(span_data.events.length() >= 11) // start_event + 10 iteration_events
  }
}