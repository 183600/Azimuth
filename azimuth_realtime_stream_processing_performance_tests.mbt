// Azimuth Telemetry System - Real-time Stream Processing Performance Tests
// This file contains test cases for real-time stream processing performance functionality

// Test 1: Basic Stream Processing Throughput
test "basic stream processing throughput" {
  // Create a stream processor
  let processor = StreamProcessor::new()
  
  // Configure processor for performance testing
  StreamProcessor::set_batch_size(processor, 1000)
  StreamProcessor::set_buffer_size(processor, 10000)
  
  // Create a simple processing pipeline
  processor.add_stage("parse", |event| {
    // Simulate parsing telemetry data
    let parsed = TelemetryEvent::parse(event)
    StreamResult::success(parsed)
  })
  
  processor.add_stage("filter", |event| {
    // Filter only error events
    if TelemetryEvent::get_level(event) == "ERROR" {
      StreamResult::success(event)
    } else {
      StreamResult::skip()
    }
  })
  
  processor.add_stage("aggregate", |event| {
    // Aggregate error counts by service
    let service = TelemetryEvent::get_service_name(event)
    let error_count = 1
    StreamResult::success(AggregatedEvent::new(service, error_count))
  })
  
  // Generate test data
  let test_events = []
  for i in 0..10000 {
    let level = if i % 10 == 0 { "ERROR" } else { "INFO" }
    let service = "service-" + ((i % 5) + 1).to_string()
    let event = TelemetryEvent::new(i.to_string(), level, service, "test message")
    test_events.push(event)
  }
  
  // Measure processing time
  let start_time = Time::now()
  
  // Process events
  let results = processor.process_batch(test_events)
  
  let end_time = Time::now()
  let processing_time = end_time - start_time
  
  // Verify results
  assert_eq(results.length(), 1000)  // 10% are errors (10000 / 10)
  
  // Verify performance metrics
  let throughput = test_events.length() as Float / processing_time.as_seconds()
  assert_true(throughput > 1000.0)  // Should process at least 1000 events per second
  
  let metrics = processor.get_metrics()
  assert_eq(StreamMetrics::get_events_processed(metrics), 10000)
  assert_eq(StreamMetrics::get_events_output(metrics), 1000)
  assert_true(StreamMetrics::get_avg_processing_time(metrics) < 0.001)  // Less than 1ms per event
}

// Test 2: Stream Processing Latency
test "stream processing latency" {
  // Create a stream processor with latency monitoring
  let processor = StreamProcessor::new()
  processor.enable_latency_monitoring(true)
  
  // Add processing stages with different complexities
  processor.add_stage("simple_transform", |event| {
    // Simple transformation
    let transformed = TelemetryEvent::add_tag(event, "processed", "true")
    StreamResult::success(transformed)
  })
  
  processor.add_stage("complex_calculation", |event| {
    // More complex calculation
    let metrics = TelemetryEvent::get_metrics(event)
    let calculated = []
    
    for metric in metrics {
      let value = TelemetryMetric::get_value(metric)
      let calculated_value = value * 1.5 + Math::sin(value)  // Complex calculation
      calculated.push(TelemetryMetric::new(
        TelemetryMetric::get_name(metric) + "_calculated",
        calculated_value
      ))
    }
    
    let result = TelemetryEvent::add_metrics(event, calculated)
    StreamResult::success(result)
  })
  
  processor.add_stage("external_lookup", |event| {
    // Simulate external lookup with delay
    let service_name = TelemetryEvent::get_service_name(event)
    let service_info = ServiceRegistry::lookup(service_name)  // Simulated delay
    let enriched = TelemetryEvent::add_service_info(event, service_info)
    StreamResult::success(enriched)
  })
  
  // Generate test events with varying complexity
  let test_events = []
  for i in 0..1000 {
    let metrics = []
    for j in 0..(i % 10 + 1) {  // Varying number of metrics
      metrics.push(TelemetryMetric::new("metric_" + j.to_string(), j as Float))
    }
    
    let event = TelemetryEvent::with_metrics(
      i.to_string(),
      "INFO",
      "service-" + ((i % 5) + 1).to_string(),
      "test message",
      metrics
    )
    test_events.push(event)
  }
  
  // Process events and measure latency
  let latency_results = processor.process_with_latency_measurement(test_events)
  
  // Verify latency metrics
  let avg_latency = StreamProcessor::get_average_latency(processor)
  let p95_latency = StreamProcessor::get_percentile_latency(processor, 95)
  let p99_latency = StreamProcessor::get_percentile_latency(processor, 99)
  let max_latency = StreamProcessor::get_max_latency(processor)
  
  assert_true(avg_latency < 0.01)    // Average less than 10ms
  assert_true(p95_latency < 0.05)    // 95th percentile less than 50ms
  assert_true(p99_latency < 0.1)     // 99th percentile less than 100ms
  assert_true(max_latency < 0.2)     // Maximum less than 200ms
  
  // Verify latency distribution
  let latency_distribution = StreamProcessor::get_latency_distribution(processor)
  assert_true(latency_distribution.size() > 0)
  
  // Verify all events were processed
  assert_eq(latency_results.length(), test_events.length())
  
  // Verify latency for each event is recorded
  for result in latency_results {
    assert_true(LatencyResult::get_latency(result) > 0)
    assert_true(LatencyResult::get_latency(result) < 1.0)  // Less than 1 second
  }
}

// Test 3: Backpressure Handling
test "backpressure handling" {
  // Create a stream processor with backpressure control
  let processor = StreamProcessor::new()
  processor.enable_backpressure(true)
  processor.set_backpressure_threshold(1000)  // Start applying backpressure at 1000 events
  processor.set_max_buffer_size(5000)        // Maximum buffer size
  
  // Add a slow processing stage
  processor.add_stage("slow_processing", |event| {
    // Simulate slow processing
    Thread::sleep(1)  // 1ms delay
    StreamResult::success(event)
  })
  
  // Generate a large number of events to trigger backpressure
  let test_events = []
  for i in 0..10000 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
    test_events.push(event)
  }
  
  // Process events with backpressure
  let start_time = Time::now()
  let results = processor.process_with_backpressure(test_events)
  let end_time = Time::now()
  
  // Verify all events were eventually processed
  assert_eq(results.length(), test_events.length())
  
  // Verify backpressure was applied
  let metrics = processor.get_metrics()
  assert_true(StreamMetrics::get_backpressure_events(metrics) > 0)
  
  // Verify buffer management
  assert_true(StreamMetrics::get_max_buffer_usage(metrics) <= processor.get_max_buffer_size())
  
  // Test adaptive backpressure
  processor.enable_adaptive_backpressure(true)
  
  // Process another batch with adaptive backpressure
  let adaptive_results = processor.process_with_backpressure(test_events)
  assert_eq(adaptive_results.length(), test_events.length())
  
  // Verify adaptive metrics
  let adaptive_metrics = processor.get_metrics()
  assert_true(StreamMetrics::get_adaptive_backpressure_adjustments(adaptive_metrics) > 0)
}

// Test 4: Stream Processing Scalability
test "stream processing scalability" {
  // Test scalability with different numbers of processing threads
  let thread_counts = [1, 2, 4, 8]
  let throughput_results = []
  
  for thread_count in thread_counts {
    // Create a processor with specified thread count
    let processor = StreamProcessor::new()
    processor.set_parallelism(thread_count)
    
    // Add CPU-intensive processing stages
    processor.add_stage("cpu_intensive", |event| {
      // CPU-intensive calculation
      let value = TelemetryEvent::get_timestamp(event)
      let mut result = value
      
      for i in 0..1000 {
        result = result + (value * i) % 1000000
      }
      
      let updated = TelemetryEvent::set_timestamp(event, result)
      StreamResult::success(updated)
    })
    
    processor.add_stage("memory_intensive", |event| {
      // Memory-intensive operation
      let large_data = Array::new(1000, |_| "large string data")
      let processed = TelemetryEvent::add_large_data(event, large_data)
      StreamResult::success(processed)
    })
    
    // Generate test data
    let test_events = []
    for i in 0..5000 {
      let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
      test_events.push(event)
    }
    
    // Measure throughput
    let start_time = Time::now()
    let results = processor.process_batch(test_events)
    let end_time = Time::now()
    
    let processing_time = end_time - start_time
    let throughput = test_events.length() as Float / processing_time.as_seconds()
    
    throughput_results.push((thread_count, throughput))
    
    // Verify all events were processed
    assert_eq(results.length(), test_events.length())
  }
  
  // Verify scalability - throughput should increase with more threads
  for i in 1..throughput_results.length() {
    let (prev_threads, prev_throughput) = throughput_results[i-1]
    let (curr_threads, curr_throughput) = throughput_results[i]
    
    assert_true(curr_threads > prev_threads)
    
    // Throughput should increase (though not necessarily linearly due to overhead)
    let improvement = (curr_throughput - prev_throughput) / prev_throughput
    assert_true(improvement > -0.1)  // Allow for some overhead with more threads
  }
  
  // Test with very high parallelism
  let high_parallel_processor = StreamProcessor::new()
  high_parallel_processor.set_parallelism(16)
  
  // Add simple processing stage
  high_parallel_processor.add_stage("simple", |event| {
    StreamResult::success(event)
  })
  
  // Generate large test dataset
  let large_test_events = []
  for i in 0..50000 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
    large_test_events.push(event)
  }
  
  // Measure high parallelism throughput
  let start_time = Time::now()
  let large_results = high_parallel_processor.process_batch(large_test_events)
  let end_time = Time::now()
  
  let processing_time = end_time - start_time
  let high_throughput = large_test_events.length() as Float / processing_time.as_seconds()
  
  // Verify high parallelism performance
  assert_eq(large_results.length(), large_test_events.length())
  assert_true(high_throughput > 5000.0)  // Should handle high throughput
  
  // Verify thread utilization
  let thread_metrics = high_parallel_processor.get_thread_metrics()
  assert_true(ThreadMetrics::get_avg_utilization(thread_metrics) > 0.7)  // 70% utilization
}

// Test 5: Stream Processing Memory Efficiency
test "stream processing memory efficiency" {
  // Create a stream processor with memory monitoring
  let processor = StreamProcessor::new()
  processor.enable_memory_monitoring(true)
  processor.set_memory_limit(100 * 1024 * 1024)  // 100MB limit
  
  // Add memory-intensive processing stages
  processor.add_stage("memory_allocation", |event| {
    // Allocate memory for processing
    let data_size = 1024 * 10  // 10KB per event
    let data = Array::new(data_size, |_| 0)
    let processed = TelemetryEvent::add_binary_data(event, data)
    StreamResult::success(processed)
  })
  
  processor.add_stage("memory_processing", |event| {
    // Process the allocated data
    let binary_data = TelemetryEvent::get_binary_data(event)
    let processed_data = []
    
    for byte in binary_data {
      let processed_byte = byte * 2 % 256
      processed_data.push(processed_byte)
    }
    
    let result = TelemetryEvent::replace_binary_data(event, processed_data)
    StreamResult::success(result)
  })
  
  // Generate test events
  let test_events = []
  for i in 0..10000 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
    test_events.push(event)
  }
  
  // Process events with memory monitoring
  let initial_memory = processor.get_memory_usage()
  let results = processor.process_batch(test_events)
  let peak_memory = processor.get_peak_memory_usage()
  let final_memory = processor.get_memory_usage()
  
  // Verify all events were processed
  assert_eq(results.length(), test_events.length())
  
  // Verify memory usage stayed within limits
  assert_true(peak_memory < processor.get_memory_limit())
  
  // Verify memory was properly released
  let memory_growth = final_memory - initial_memory
  assert_true(memory_growth < test_events.length() * 100)  // Less than 100 bytes per event
  
  // Test memory cleanup
  processor.force_memory_cleanup()
  let cleanup_memory = processor.get_memory_usage()
  assert_true(cleanup_memory <= final_memory)
  
  // Test with memory pressure simulation
  processor.simulate_memory_pressure(0.8)  // 80% memory pressure
  
  let pressure_events = []
  for i in 0..5000 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
    pressure_events.push(event)
  }
  
  let pressure_results = processor.process_with_memory_management(pressure_events)
  assert_eq(pressure_results.length(), pressure_events.length())
  
  // Verify memory pressure was handled
  let pressure_metrics = processor.get_memory_metrics()
  assert_true(MemoryMetrics::get_gc_count(pressure_metrics) > 0)
  assert_true(MemoryMetrics::get_memory_pressure_events(pressure_metrics) > 0)
}

// Test 6: Stream Processing Fault Tolerance
test "stream processing fault tolerance" {
  // Create a stream processor with fault tolerance
  let processor = StreamProcessor::new()
  processor.enable_fault_tolerance(true)
  processor.set_max_retries(3)
  processor.set_retry_delay(10)  // 10ms
  
  // Add processing stages with potential failures
  let failure_rate = 0.1  // 10% failure rate
  let mut failure_count = 0
  
  processor.add_stage("unreliable_processing", |event| {
    // Simulate random failures
    failure_count = failure_count + 1
    if failure_count % 10 == 0 {  // 10% failure rate
      StreamResult::error("Simulated processing error")
    } else {
      let processed = TelemetryEvent::add_tag(event, "processed", "true")
      StreamResult::success(processed)
    }
  })
  
  processor.add_stage("error_handling", |event| {
    // Handle errors from previous stage
    if StreamResult::is_error(event) {
      let error_event = TelemetryEvent::new(
        TelemetryEvent::get_id(event),
        "ERROR",
        "error-handler",
        "Processing failed: " + StreamResult::get_error_message(event)
      )
      StreamResult::success(error_event)
    } else {
      StreamResult::success(event)
    }
  })
  
  // Generate test events
  let test_events = []
  for i in 0..1000 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
    test_events.push(event)
  }
  
  // Process events with fault tolerance
  let results = processor.process_with_fault_tolerance(test_events)
  
  // Verify all events were processed (either successfully or with error handling)
  assert_eq(results.length(), test_events.length())
  
  // Verify error handling
  let error_count = results.count(|result| TelemetryEvent::get_level(result) == "ERROR")
  assert_true(error_count > 0)  // Should have some errors
  assert_true(error_count < test_events.length() / 2)  // But not too many
  
  // Verify retry metrics
  let metrics = processor.get_metrics()
  assert_true(StreamMetrics::get_retry_count(metrics) > 0)
  assert_true(StreamMetrics::get_retry_success_rate(metrics) > 0.8)  // 80% retry success rate
  
  // Test circuit breaker functionality
  processor.enable_circuit_breaker(true)
  processor.set_circuit_breaker_threshold(5)  // Open after 5 failures
  processor.set_circuit_breaker_timeout(100)  // 100ms timeout
  
  // Create events that will always fail
  let failing_events = []
  for i in 0..10 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "failing-service", "test message")
    failing_events.push(event)
  }
  
  // Add a stage that always fails
  processor.add_stage("always_fails", |event| {
    StreamResult::error("Always fails")
  })
  
  let failing_results = processor.process_with_fault_tolerance(failing_events)
  
  // Verify circuit breaker was triggered
  let circuit_breaker_metrics = processor.get_circuit_breaker_metrics()
  assert_true(CircuitBreakerMetrics::get_open_count(circuit_breaker_metrics) > 0)
  assert_true(CircuitBreakerMetrics::get_blocked_requests(circuit_breaker_metrics) > 0)
}

// Test 7: Stream Processing Windowing
test "stream processing windowing" {
  // Create a stream processor with windowing support
  let processor = StreamProcessor::new()
  processor.enable_windowing(true)
  
  // Add time-based windowing
  processor.add_time_window("5_second_window", 5000)  // 5 second window
  
  // Add tumbling window aggregation
  processor.add_tumbling_window("count_window", 100, |events| {
    let count = events.length()
    let window_result = WindowResult::new("count", count)
    StreamResult::success(window_result)
  })
  
  // Add sliding window aggregation
  processor.add_sliding_window("sum_window", 50, 10, |events| {
    let mut sum = 0
    for event in events {
      sum = sum + TelemetryEvent::get_value(event)
    }
    let window_result = WindowResult::new("sum", sum)
    StreamResult::success(window_result)
  })
  
  // Generate time-series test data
  let test_events = []
  let base_time = Time::now()
  
  for i in 0..1000 {
    let timestamp = base_time + (i * 100)  // 100ms apart
    let value = i % 100
    let event = TelemetryEvent::with_timestamp(
      i.to_string(),
      "INFO",
      "test-service",
      "test message",
      timestamp,
      value
    )
    test_events.push(event)
  }
  
  // Process events with windowing
  let window_results = processor.process_with_windowing(test_events)
  
  // Verify window results
  assert_true(window_results.length() > 0)
  
  // Verify tumbling window results
  let count_results = window_results.filter(|result| WindowResult::get_type(result) == "count")
  assert_true(count_results.length() > 0)
  
  for result in count_results {
    let count = WindowResult::get_value(result) as Int
    assert_eq(count, 100)  // Each window should have exactly 100 events
  }
  
  // Verify sliding window results
  let sum_results = window_results.filter(|result| WindowResult::get_type(result) == "sum")
  assert_true(sum_results.length() > 0)
  
  // Test session windowing
  processor.add_session_window("session_window", 1000, |events| {
    let session_duration = TelemetryEvent::get_timestamp(events[events.length() - 1]) - TelemetryEvent::get_timestamp(events[0])
    let window_result = WindowResult::new("session_duration", session_duration.as_milliseconds())
    StreamResult::success(window_result)
  })
  
  // Generate session-based test data (with gaps)
  let session_events = []
  let session_base_time = Time::now()
  
  for session in 0..5 {
    for event_in_session in 0..10 {
      let timestamp = session_base_time + (session * 5000) + (event_in_session * 100)  // 5s between sessions
      let event = TelemetryEvent::with_timestamp(
        "session-" + session.to_string() + "-" + event_in_session.to_string(),
        "INFO",
        "test-service",
        "test message",
        timestamp,
        0
      )
      session_events.push(event)
    }
  }
  
  let session_results = processor.process_with_windowing(session_events)
  
  // Verify session window results
  let session_window_results = session_results.filter(|result| WindowResult::get_type(result) == "session_duration")
  assert_eq(session_window_results.length(), 5)  // 5 sessions
  
  for result in session_window_results {
    let duration = WindowResult::get_value(result) as Int
    assert_true(duration >= 900 && duration <= 1100)  // Approximately 1000ms (10 events * 100ms apart)
  }
}

// Test 8: Stream Processing State Management
test "stream processing state management" {
  // Create a stream processor with state management
  let processor = StreamProcessor::new()
  processor.enable_state_management(true)
  
  // Add stateful processing stages
  processor.add_stateful_stage("counter", |event, state| {
    let key = TelemetryEvent::get_service_name(event)
    let count = StateManager::get_or_default(state, key, 0)
    StateManager::set(state, key, count + 1)
    StreamResult::success(event)
  })
  
  processor.add_stateful_stage("aggregator", |event, state| {
    let key = TelemetryEvent::get_service_name(event)
    let value = TelemetryEvent::get_value(event)
    let sum = StateManager::get_or_default(state, key + "_sum", 0.0)
    StateManager::set(state, key + "_sum", sum + value)
    StreamResult::success(event)
  })
  
  // Generate test events
  let test_events = []
  for i in 0..1000 {
    let service_name = "service-" + ((i % 5) + 1).to_string()
    let value = i as Float
    let event = TelemetryEvent::with_value(
      i.to_string(),
      "INFO",
      service_name,
      "test message",
      value
    )
    test_events.push(event)
  }
  
  // Process events with state management
  let results = processor.process_with_state(test_events)
  
  // Verify all events were processed
  assert_eq(results.length(), test_events.length())
  
  // Verify state was maintained correctly
  let state = processor.get_state()
  
  // Check counters
  for i in 1..6 {
    let key = "service-" + i.to_string()
    let count = StateManager::get(state, key)
    match count {
      Some(c) => assert_eq(c as Int, 200)  // 1000 events / 5 services = 200 per service
      None => assert_true(false)
    }
  }
  
  // Check aggregators
  for i in 1..6 {
    let key = "service-" + i.to_string() + "_sum"
    let sum = StateManager::get(state, key)
    match sum {
      Some(s) => {
        // Calculate expected sum for this service
        let mut expected_sum = 0.0
        for j in i..1000 step 5 {
          expected_sum = expected_sum + j as Float
        }
        assert_true((s - expected_sum).abs() < 0.001)
      }
      None => assert_true(false)
    }
  }
  
  // Test state persistence
  let serialized_state = processor.serialize_state()
  assert_true(serialized_state.length() > 0)
  
  // Create new processor and restore state
  let new_processor = StreamProcessor::new()
  new_processor.enable_state_management(true)
  new_processor.deserialize_state(serialized_state)
  
  // Verify state was restored
  let restored_state = new_processor.get_state()
  
  for i in 1..6 {
    let key = "service-" + i.to_string()
    let count = StateManager::get(restored_state, key)
    match count {
      Some(c) => assert_eq(c as Int, 200)
      None => assert_true(false)
    }
  }
  
  // Test state checkpointing
  processor.enable_checkpointing(true)
  processor.set_checkpoint_interval(100)  // Checkpoint every 100 events
  
  // Process more events
  let additional_events = []
  for i in 1000..1200 {
    let service_name = "service-" + ((i % 5) + 1).to_string()
    let value = i as Float
    let event = TelemetryEvent::with_value(
      i.to_string(),
      "INFO",
      service_name,
      "test message",
      value
    )
    additional_events.push(event)
  }
  
  processor.process_with_state(additional_events)
  
  // Verify checkpoints were created
  let checkpoints = processor.get_checkpoints()
  assert_true(checkpoints.length() > 0)
  
  // Test state recovery from checkpoint
  let latest_checkpoint = checkpoints[checkpoints.length() - 1]
  processor.restore_from_checkpoint(latest_checkpoint)
  
  let recovered_state = processor.get_state()
  
  // Verify state was recovered to checkpoint state
  for i in 1..6 {
    let key = "service-" + i.to_string()
    let count = StateManager::get(recovered_state, key)
    match count {
      Some(c) => assert_true(c >= 200)  // Should be at least the original count
      None => assert_true(false)
    }
  }
}

// Test 9: Stream Processing Dynamic Configuration
test "stream processing dynamic configuration" {
  // Create a stream processor with dynamic configuration
  let processor = StreamProcessor::new()
  processor.enable_dynamic_configuration(true)
  
  // Add processing stages
  processor.add_stage("filter", |event| {
    let level = TelemetryEvent::get_level(event)
    if level == "ERROR" {
      StreamResult::success(event)
    } else {
      StreamResult::skip()
    }
  })
  
  processor.add_stage("transform", |event| {
    let transformed = TelemetryEvent::add_tag(event, "processed", "true")
    StreamResult::success(transformed)
  })
  
  // Generate test events
  let test_events = []
  for i in 0..1000 {
    let level = if i % 3 == 0 { "ERROR" } else { "INFO" }
    let event = TelemetryEvent::new(i.to_string(), level, "test-service", "test message")
    test_events.push(event)
  }
  
  // Process with initial configuration
  let initial_results = processor.process_batch(test_events)
  assert_eq(initial_results.length(), 334)  // Approximately 1/3 are errors
  
  // Dynamically change filter configuration
  processor.update_stage_config("filter", "filter_level", "WARN")
  
  // Update filter implementation
  processor.update_stage("filter", |event| {
    let level = TelemetryEvent::get_level(event)
    if level == "WARN" || level == "ERROR" {
      StreamResult::success(event)
    } else {
      StreamResult::skip()
    }
  })
  
  // Generate new test events with WARN level
  let new_test_events = []
  for i in 0..1000 {
    let level = match i % 4 {
      0 => "ERROR"
      1 => "WARN"
      _ => "INFO"
    }
    let event = TelemetryEvent::new(i.to_string(), level, "test-service", "test message")
    new_test_events.push(event)
  }
  
  // Process with updated configuration
  let updated_results = processor.process_batch(new_test_events)
  assert_eq(updated_results.length(), 500)  // 1/2 are ERROR or WARN
  
  // Test adding new stage dynamically
  processor.add_stage("enrichment", |event| {
    let enriched = TelemetryEvent::add_tag(event, "enriched", "true")
    StreamResult::success(enriched)
  })
  
  let enriched_results = processor.process_batch(new_test_events)
  
  // Verify enrichment was applied
  for result in enriched_results {
    assert_true(TelemetryEvent::has_tag(result, "enriched"))
    assert_eq(TelemetryEvent::get_tag(result, "enriched"), "true")
  }
  
  // Test removing stage dynamically
  processor.remove_stage("transform")
  
  let simplified_results = processor.process_batch(new_test_events)
  
  // Verify transform stage was removed
  for result in simplified_results {
    assert_false(TelemetryEvent::has_tag(result, "processed"))
  }
  
  // Test runtime parameter tuning
  processor.set_parameter("batch_size", 500)
  processor.set_parameter("parallelism", 4)
  processor.set_parameter("buffer_size", 2000)
  
  // Verify parameters were updated
  assert_eq(processor.get_parameter("batch_size"), 500)
  assert_eq(processor.get_parameter("parallelism"), 4)
  assert_eq(processor.get_parameter("buffer_size"), 2000)
  
  // Test configuration persistence
  let config = processor.export_configuration()
  assert_true(config.length() > 0)
  
  // Create new processor and import configuration
  let new_processor = StreamProcessor::new()
  new_processor.enable_dynamic_configuration(true)
  new_processor.import_configuration(config)
  
  // Verify configuration was imported
  assert_eq(new_processor.get_parameter("batch_size"), 500)
  assert_eq(new_processor.get_parameter("parallelism"), 4)
  assert_eq(new_processor.get_parameter("buffer_size"), 2000)
  
  // Verify stages were imported
  assert_true(new_processor.has_stage("filter"))
  assert_true(new_processor.has_stage("enrichment"))
  assert_false(new_processor.has_stage("transform"))  // Was removed
}

// Test 10: Stream Processing Performance Monitoring
test "stream processing performance monitoring" {
  // Create a stream processor with comprehensive monitoring
  let processor = StreamProcessor::new()
  processor.enable_comprehensive_monitoring(true)
  
  // Add processing stages with different performance characteristics
  processor.add_stage("fast_stage", |event| {
    // Fast processing stage
    let processed = TelemetryEvent::add_tag(event, "fast_processed", "true")
    StreamResult::success(processed)
  })
  
  processor.add_stage("medium_stage", |event| {
    // Medium processing stage
    Thread::sleep(1)  // 1ms delay
    let processed = TelemetryEvent::add_tag(event, "medium_processed", "true")
    StreamResult::success(processed)
  })
  
  processor.add_stage("slow_stage", |event| {
    // Slow processing stage
    Thread::sleep(5)  // 5ms delay
    let processed = TelemetryEvent::add_tag(event, "slow_processed", "true")
    StreamResult::success(processed)
  })
  
  // Generate test events
  let test_events = []
  for i in 0..5000 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
    test_events.push(event)
  }
  
  // Process events with monitoring
  let start_time = Time::now()
  let results = processor.process_with_monitoring(test_events)
  let end_time = Time::now()
  
  // Verify all events were processed
  assert_eq(results.length(), test_events.length())
  
  // Get comprehensive performance metrics
  let performance_metrics = processor.get_performance_metrics()
  
  // Verify throughput metrics
  let throughput = PerformanceMetrics::get_throughput(performance_metrics)
  assert_true(throughput > 100)  // Should process at least 100 events per second
  
  // Verify latency metrics
  let avg_latency = PerformanceMetrics::get_average_latency(performance_metrics)
  let p95_latency = PerformanceMetrics::get_percentile_latency(performance_metrics, 95)
  let p99_latency = PerformanceMetrics::get_percentile_latency(performance_metrics, 99)
  
  assert_true(avg_latency > 5.0)  // Should be around 6ms (1ms + 5ms)
  assert_true(avg_latency < 10.0)  // But less than 10ms
  
  // Verify stage-specific metrics
  let stage_metrics = PerformanceMetrics::get_stage_metrics(performance_metrics)
  
  let fast_stage_metrics = stage_metrics["fast_stage"]
  let medium_stage_metrics = stage_metrics["medium_stage"]
  let slow_stage_metrics = stage_metrics["slow_stage"]
  
  assert_true(StageMetrics::get_avg_latency(fast_stage_metrics) < 1.0)
  assert_true(StageMetrics::get_avg_latency(medium_stage_metrics) >= 1.0)
  assert_true(StageMetrics::get_avg_latency(slow_stage_metrics) >= 5.0)
  
  // Verify resource utilization metrics
  let resource_metrics = PerformanceMetrics::get_resource_metrics(performance_metrics)
  
  assert_true(ResourceMetrics::get_cpu_utilization(resource_metrics) >= 0.0)
  assert_true(ResourceMetrics::get_cpu_utilization(resource_metrics) <= 1.0)
  
  assert_true(ResourceMetrics::get_memory_utilization(resource_metrics) >= 0.0)
  assert_true(ResourceMetrics::get_memory_utilization(resource_metrics) <= 1.0)
  
  // Verify error metrics
  let error_metrics = PerformanceMetrics::get_error_metrics(performance_metrics)
  
  assert_eq(ErrorMetrics::get_total_errors(error_metrics), 0)  // No errors in this test
  assert_eq(ErrorMetrics::get_error_rate(error_metrics), 0.0)
  
  // Test real-time monitoring dashboard data generation
  let dashboard_data = processor.generate_dashboard_data()
  
  assert_true(DashboardData::get_throughput_chart(dashboard_data).length() > 0)
  assert_true(DashboardData::get_latency_chart(dashboard_data).length() > 0)
  assert_true(DashboardData::get_error_rate_chart(dashboard_data).length() > 0)
  assert_true(DashboardData::get_resource_utilization_chart(dashboard_data).length() > 0)
  
  // Test performance alerts
  processor.enable_performance_alerts(true)
  processor.set_latency_threshold(10.0)  // Alert if latency > 10ms
  processor.set_throughput_threshold(50.0)  // Alert if throughput < 50 events/sec
  processor.set_error_rate_threshold(0.01)  // Alert if error rate > 1%
  
  // Process events that might trigger alerts
  let alert_test_events = []
  for i in 0..100 {
    let event = TelemetryEvent::new(i.to_string(), "INFO", "test-service", "test message")
    alert_test_events.push(event)
  }
  
  processor.process_with_monitoring(alert_test_events)
  
  // Check for alerts
  let alerts = processor.get_performance_alerts()
  
  // In our test, we shouldn't trigger any alerts since the performance is good
  assert_eq(alerts.length(), 0)
  
  // Test performance report generation
  let report = processor.generate_performance_report()
  
  assert_true(PerformanceReport::get_summary(report).contains("Total events processed"))
  assert_true(PerformanceReport::get_summary(report).contains("Average throughput"))
  assert_true(PerformanceReport::get_summary(report).contains("Average latency"))
  
  let detailed_metrics = PerformanceReport::get_detailed_metrics(report)
  assert_true(detailed_metrics.length() > 0)
  
  // Test metrics export
  let json_metrics = PerformanceMetrics::export_to_json(performance_metrics)
  assert_true(json_metrics.length() > 0)
  
  let csv_metrics = PerformanceMetrics::export_to_csv(performance_metrics)
  assert_true(csv_metrics.length() > 0)
  
  // Test metrics comparison
  processor.process_batch(test_events)  // Process another batch
  
  let new_performance_metrics = processor.get_performance_metrics()
  let comparison = PerformanceMetrics::compare(performance_metrics, new_performance_metrics)
  
  assert_true(MetricsComparison::get_throughput_change(comparison) != 0.0)
  assert_true(MetricsComparison::get_latency_change(comparison) != 0.0)
}