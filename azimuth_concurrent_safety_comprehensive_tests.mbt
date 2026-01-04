// Azimuth Telemetry System - Concurrent Safety Comprehensive Tests
// This file contains comprehensive tests for concurrent safety and thread safety

// Test 1: Concurrent Attribute Access Safety
test "concurrent attribute access safety" {
  let attributes = Attributes::new()
  let thread_count = 10
  let operations_per_thread = 100
  
  // Create multiple threads that access attributes concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=operations_per_thread {
        let key = "thread_" + thread_id.to_string() + "_key_" + i.to_string()
        let value = "thread_" + thread_id.to_string() + "_value_" + i.to_string()
        
        // Concurrent write operations
        Attributes::set(attributes, key, StringValue(value))
        
        // Concurrent read operations
        let retrieved = Attributes::get(attributes, key)
        match retrieved {
          Some(StringValue(retrieved_value)) => {
            // Verify value consistency
            assert_eq(retrieved_value, value)
          }
          None => {
            // Attribute might not be set yet due to race condition
            // This is acceptable in concurrent scenarios
          }
          _ => assert_true(false)
        }
        
        // Concurrent delete operations
        if i % 10 == 0 {
          Attributes::remove(attributes, key)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify data integrity after concurrent operations
  let all_keys = Attributes::get_all_keys(attributes)
  
  // Verify no corrupted data exists
  for key in all_keys {
    let value = Attributes::get(attributes, key)
    match value {
      Some(StringValue(_)) => assert_true(true) // Valid string value
      Some(IntValue(_)) => assert_true(true) // Valid int value
      Some(FloatValue(_)) => assert_true(true) // Valid float value
      Some(BoolValue(_)) => assert_true(true) // Valid bool value
      Some(ArrayStringValue(_)) => assert_true(true) // Valid array string value
      Some(ArrayIntValue(_)) => assert_true(true) // Valid array int value
      None => assert_true(false) // Should not happen
    }
  }
}

// Test 2: Concurrent Span Creation and Management
test "concurrent span creation and management" {
  let span_manager = SpanManager::new()
  let thread_count = 8
  let spans_per_thread = 50
  
  // Create multiple threads that create spans concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=spans_per_thread {
        let span_name = "concurrent_operation_" + thread_id.to_string() + "_" + i.to_string()
        let trace_id = "trace_" + thread_id.to_string()
        let span_id = "span_" + thread_id.to_string() + "_" + i.to_string()
        
        let span_ctx = SpanContext::new(trace_id, span_id, true, "")
        let span = Span::new(span_name, Internal, span_ctx)
        
        // Concurrent span registration
        SpanManager::register_span(span_manager, span)
        
        // Concurrent attribute setting
        Span::set_attribute(span, "thread.id", thread_id.to_string())
        Span::set_attribute(span, "operation.id", i.to_string())
        
        // Concurrent event addition
        Span::add_event(span, "concurrent_event", Some([
          ("thread_id", thread_id.to_string()),
          ("event_id", i.to_string())
        ]))
        
        // Concurrent span status updates
        if i % 5 == 0 {
          Span::set_status(span, Ok, Some("Periodic status update"))
        }
        
        // Concurrent span ending
        if i % 3 == 0 {
          Span::end(span)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify span integrity after concurrent operations
  let all_spans = SpanManager::get_all_spans(span_manager)
  assert_eq(all_spans.length(), (thread_count + 1) * (spans_per_thread + 1))
  
  // Verify no corrupted span data
  for span in all_spans {
    let span_name = Span::name(span)
    assert_true(span_name.length() > 0)
    
    let span_ctx = Span::span_context(span)
    assert_true(SpanContext::is_valid(span_ctx))
    
    // Verify attributes are consistent
    let thread_id_attr = Span::get_attribute(span, "thread.id")
    match thread_id_attr {
      Some(StringValue(_)) => assert_true(true)
      None => assert_true(false)
      _ => assert_true(false)
    }
  }
}

// Test 3: Concurrent Metric Collection Safety
test "concurrent metric collection safety" {
  let metric_collector = MetricCollector::new()
  let thread_count = 12
  let metrics_per_thread = 200
  
  // Create multiple threads that collect metrics concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      // Create counter metric
      let counter = MetricCollector::create_counter(
        metric_collector, 
        "concurrent_counter_" + thread_id.to_string(),
        "Concurrent counter for thread " + thread_id.to_string()
      )
      
      // Create histogram metric
      let histogram = MetricCollector::create_histogram(
        metric_collector,
        "concurrent_histogram_" + thread_id.to_string(),
        "Concurrent histogram for thread " + thread_id.to_string(),
        "ms"
      )
      
      for i in 0..=metrics_per_thread {
        // Concurrent counter updates
        Counter::add(counter, 1.0)
        
        // Concurrent histogram recordings
        let value = (i % 100) as Float + thread_id as Float
        Histogram::record(histogram, value)
        
        // Concurrent gauge updates
        if i % 10 == 0 {
          let gauge = MetricCollector::create_gauge(
            metric_collector,
            "concurrent_gauge_" + thread_id.to_string(),
            "Concurrent gauge for thread " + thread_id.to_string()
          )
          Gauge::set(gauge, (i % 50) as Float)
        }
        
        // Concurrent metric snapshots
        if i % 20 == 0 {
          let snapshot = MetricCollector::create_snapshot(metric_collector)
          assert_true(snapshot.metrics.length() > 0)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify metric integrity after concurrent operations
  let final_snapshot = MetricCollector::create_snapshot(metric_collector)
  
  // Verify all expected metrics exist
  assert_true(final_snapshot.metrics.length() >= thread_count * 2) // At least counters and histograms
  
  // Verify metric values are consistent
  for metric in final_snapshot.metrics {
    match metric {
      CounterMetric(name, value) => {
        assert_true(name.starts_with("concurrent_counter_"))
        assert_true(value >= 0.0)
        assert_eq(value, (metrics_per_thread + 1) as Float) // Each thread added metrics_per_thread + 1 times
      }
      HistogramMetric(name, buckets) => {
        assert_true(name.starts_with("concurrent_histogram_"))
        assert_true(buckets.length() > 0)
        
        // Verify bucket counts sum to total
        let mut total_count = 0
        for bucket in buckets {
          total_count = total_count + bucket.count
        }
        assert_eq(total_count, metrics_per_thread + 1)
      }
      GaugeMetric(name, value) => {
        assert_true(name.starts_with("concurrent_gauge_"))
        assert_true(value >= 0.0)
      }
    }
  }
}

// Test 4: Concurrent Log Record Processing
test "concurrent log record processing" {
  let log_processor = LogProcessor::new()
  let thread_count = 6
  let logs_per_thread = 150
  
  // Create multiple threads that process logs concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=logs_per_thread {
        let severity = match i % 4 {
          0 => Debug,
          1 => Info,
          2 => Warning,
          _ => Error
        }
        
        let message = "Concurrent log message from thread " + thread_id.to_string() + 
                     " iteration " + i.to_string()
        
        let attributes = [
          ("thread.id", thread_id.to_string()),
          ("iteration", i.to_string()),
          ("severity", severity.to_string())
        ]
        
        let log_record = LogRecord::new_with_attributes(
          severity,
          message,
          attributes,
          Some(1234567890L + i as Long),
          None,
          Some("trace_" + thread_id.to_string()),
          Some("span_" + i.to_string())
        )
        
        // Concurrent log processing
        LogProcessor::process(log_processor, log_record)
        
        // Concurrent log filtering
        if i % 5 == 0 {
          let filter = LogFilter::new()
          LogFilter::add_criteria(filter, "thread.id", thread_id.to_string())
          let filtered_logs = LogProcessor::filter_logs(log_processor, filter)
          assert_true(filtered_logs.length() >= 0)
        }
        
        // Concurrent log aggregation
        if i % 10 == 0 {
          let aggregator = LogAggregator::new()
          LogAggregator::add_logs(aggregator, LogProcessor::get_recent_logs(log_processor, 100))
          let summary = LogAggregator::summarize(aggregator)
          assert_true(summary.total_logs >= 0)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify log processing integrity after concurrent operations
  let all_logs = LogProcessor::get_all_logs(log_processor)
  assert_eq(all_logs.length(), (thread_count + 1) * (logs_per_thread + 1))
  
  // Verify no corrupted log data
  for log in all_logs {
    let severity = LogRecord::severity_number(log)
    assert_true(severity >= Debug && severity <= Fatal)
    
    match LogRecord::body(log) {
      Some(message) => assert_true(message.length() > 0)
      None => assert_true(false)
    }
    
    let thread_id = LogRecord::get_attribute(log, "thread.id")
    match thread_id {
      Some(StringValue(_)) => assert_true(true)
      _ => assert_true(false)
    }
  }
  
  // Verify log counts by severity
  let severity_counts = LogProcessor::count_by_severity(log_processor)
  assert_eq(severity_counts[Debug], (thread_count + 1) * ((logs_per_thread + 1) / 4 + 1))
  assert_eq(severity_counts[Info], (thread_count + 1) * ((logs_per_thread + 1) / 4 + 1))
  assert_eq(severity_counts[Warning], (thread_count + 1) * ((logs_per_thread + 1) / 4 + 1))
  assert_eq(severity_counts[Error], (thread_count + 1) * ((logs_per_thread + 1) / 4))
}

// Test 5: Concurrent Context Propagation Safety
test "concurrent context propagation safety" {
  let context_propagator = ContextPropagator::new()
  let thread_count = 8
  let contexts_per_thread = 30
  
  // Create multiple threads that propagate contexts concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=contexts_per_thread {
        let trace_id = "trace_" + thread_id.to_string() + "_" + i.to_string()
        let span_id = "span_" + thread_id.to_string() + "_" + i.to_string()
        
        let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
        let context = Context::with_span_context(Context::root(), span_ctx)
        
        // Concurrent context injection
        let headers = []
        let injected_headers = ContextPropagator::inject(context_propagator, context, headers)
        
        // Concurrent context extraction
        let extracted_context = ContextPropagator::extract(context_propagator, injected_headers)
        
        // Verify context consistency
        let extracted_span_ctx = Context::span_context(extracted_context)
        assert_eq(SpanContext::trace_id(extracted_span_ctx), trace_id)
        assert_eq(SpanContext::span_id(extracted_span_ctx), span_id)
        
        // Concurrent baggage operations
        let baggage = Baggage::new()
        let baggage_with_entries = Baggage::set_entry(baggage, "thread.id", thread_id.to_string())
        let baggage_with_entries = Baggage::set_entry(baggage_with_entries, "iteration", i.to_string())
        
        let context_with_baggage = Context::with_baggage(context, baggage_with_entries)
        
        // Concurrent baggage propagation
        let propagated_context = ContextPropagator::propagate(context_propagator, context_with_baggage)
        
        // Verify baggage consistency
        let propagated_baggage = Context::baggage(propagated_context)
        let thread_id_value = Baggage::get_entry(propagated_baggage, "thread.id")
        match thread_id_value {
          Some(value) => assert_eq(value, thread_id.to_string())
          None => assert_true(false)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify no context corruption occurred
  let propagation_stats = ContextPropagator::get_statistics(context_propagator)
  assert_eq(propagation_stats.total_injections, (thread_count + 1) * (contexts_per_thread + 1))
  assert_eq(propagation_stats.total_extractions, (thread_count + 1) * (contexts_per_thread + 1))
  assert_eq(propagation_stats.failed_injections, 0)
  assert_eq(propagation_stats.failed_extractions, 0)
}

// Test 6: Concurrent Resource Management Safety
test "concurrent resource management safety" {
  let resource_manager = ResourceManager::new()
  let thread_count = 10
  let operations_per_thread = 100
  
  // Create multiple threads that manage resources concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=operations_per_thread {
        let resource_id = "resource_" + thread_id.to_string() + "_" + i.to_string()
        
        // Concurrent resource creation
        let resource = Resource::new_with_id(resource_id)
        ResourceManager::register_resource(resource_manager, resource)
        
        // Concurrent resource acquisition
        let acquired_resource = ResourceManager::acquire_resource(resource_manager, resource_id)
        match acquired_resource {
          Some(res) => {
            // Concurrent resource usage
            Resource::mark_in_use(res)
            
            // Concurrent resource updates
            Resource::update_attribute(res, "last_used_by", thread_id.to_string())
            Resource::update_attribute(res, "usage_count", (i + 1).to_string())
            
            // Concurrent resource release
            Resource::release(res)
            ResourceManager::release_resource(resource_manager, resource_id)
          }
          None => {
            // Resource might not be available due to concurrent access
            // This is acceptable in concurrent scenarios
          }
        }
        
        // Concurrent resource cleanup
        if i % 20 == 0 {
          ResourceManager::cleanup_unused_resources(resource_manager)
        }
        
        // Concurrent resource statistics
        if i % 10 == 0 {
          let stats = ResourceManager::get_statistics(resource_manager)
          assert_true(stats.total_resources >= 0)
          assert_true(stats.active_resources >= 0)
          assert_true(stats.idle_resources >= 0)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify resource management integrity after concurrent operations
  let final_stats = ResourceManager::get_statistics(resource_manager)
  
  // Verify resource counts are consistent
  assert_eq(final_stats.total_resources, final_stats.active_resources + final_stats.idle_resources)
  
  // Verify no resource leaks
  assert_eq(final_stats.leaked_resources, 0)
  
  // Verify no corrupted resources
  let all_resources = ResourceManager::get_all_resources(resource_manager)
  for resource in all_resources {
    let resource_id = Resource::id(resource)
    assert_true(resource_id.length() > 0)
    assert_false(Resource::is_corrupted(resource))
  }
}

// Test 7: Concurrent Telemetry Pipeline Safety
test "concurrent telemetry pipeline safety" {
  let telemetry_pipeline = TelemetryPipeline::new()
  let thread_count = 6
  let data_points_per_thread = 80
  
  // Configure pipeline components
  let collector = TelemetryPipeline::get_collector(telemetry_pipeline)
  let processor = TelemetryPipeline::get_processor(telemetry_pipeline)
  let exporter = TelemetryPipeline::get_exporter(telemetry_pipeline)
  
  // Create multiple threads that process telemetry data concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=data_points_per_thread {
        // Create telemetry data
        let span = Span::new(
          "operation_" + thread_id.to_string() + "_" + i.to_string(),
          Internal,
          SpanContext::new("trace_" + thread_id.to_string(), "span_" + i.to_string(), true, "")
        )
        
        // Concurrent span collection
        TelemetryCollector::add_span(collector, span)
        
        // Concurrent metric collection
        let counter = TelemetryCollector::create_counter(
          collector,
          "counter_" + thread_id.to_string(),
          "Counter for thread " + thread_id.to_string()
        )
        Counter::add(counter, 1.0)
        
        // Concurrent log collection
        let log_record = LogRecord::new(
          Info,
          "Log message from thread " + thread_id.to_string() + " iteration " + i.to_string(),
          Some([
            ("thread.id", thread_id.to_string()),
            ("iteration", i.to_string())
          ])
        )
        TelemetryCollector::add_log(collector, log_record)
        
        // Concurrent processing
        if i % 5 == 0 {
          TelemetryProcessor::process_span(processor, span)
          TelemetryProcessor::process_metrics(processor, TelemetryCollector::get_metrics(collector))
          TelemetryProcessor::process_logs(processor, TelemetryCollector::get_logs(collector))
        }
        
        // Concurrent exporting
        if i % 10 == 0 {
          let telemetry_data = TelemetryCollector::get_all_data(collector)
          TelemetryExporter::export_async(exporter, telemetry_data)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Wait for all async operations to complete
  TelemetryPipeline::wait_for_completion(telemetry_pipeline)
  
  // Verify pipeline integrity after concurrent operations
  let pipeline_stats = TelemetryPipeline::get_statistics(telemetry_pipeline)
  
  // Verify all data was processed
  assert_eq(pipeline_stats.spans_collected, (thread_count + 1) * (data_points_per_thread + 1))
  assert_eq(pipeline_stats.metrics_collected, (thread_count + 1) * (data_points_per_thread + 1))
  assert_eq(pipeline_stats.logs_collected, (thread_count + 1) * (data_points_per_thread + 1))
  
  // Verify processing consistency
  assert_eq(pipeline_stats.spans_processed, pipeline_stats.spans_collected)
  assert_eq(pipeline_stats.metrics_processed, pipeline_stats.metrics_collected)
  assert_eq(pipeline_stats.logs_processed, pipeline_stats.logs_collected)
  
  // Verify no data corruption
  assert_eq(pipeline_stats.corrupted_spans, 0)
  assert_eq(pipeline_stats.corrupted_metrics, 0)
  assert_eq(pipeline_stats.corrupted_logs, 0)
  
  // Verify export consistency
  assert_true(pipeline_stats.exports_completed > 0)
  assert_eq(pipeline_stats.export_failures, 0)
}

// Test 8: Concurrent Configuration Management Safety
test "concurrent configuration management safety" {
  let config_manager = ConfigManager::new()
  let thread_count = 8
  let operations_per_thread = 50
  
  // Create multiple threads that manage configuration concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=operations_per_thread {
        let key = "config.thread_" + thread_id.to_string() + "." + i.to_string()
        let value = "value.thread_" + thread_id.to_string() + "." + i.to_string()
        
        // Concurrent configuration updates
        ConfigManager::set(config_manager, key, value)
        
        // Concurrent configuration reads
        let retrieved_value = ConfigManager::get(config_manager, key)
        match retrieved_value {
          Some(retrieved) => {
            // Verify value consistency
            assert_eq(retrieved, value)
          }
          None => {
            // Configuration might not be set yet due to race condition
            // This is acceptable in concurrent scenarios
          }
        }
        
        // Concurrent configuration validation
        if i % 5 == 0 {
          let validation_result = ConfigManager::validate(config_manager)
          assert_true(validation_result.is_valid)
        }
        
        // Concurrent configuration snapshots
        if i % 10 == 0 {
          let snapshot = ConfigManager::create_snapshot(config_manager)
          assert_true(snapshot.configurations.length() > 0)
        }
        
        // Concurrent configuration reload
        if i % 20 == 0 {
          ConfigManager::reload(config_manager)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Verify configuration integrity after concurrent operations
  let all_configs = ConfigManager::get_all(config_manager)
  
  // Verify all expected configurations exist
  assert_eq(all_configs.length(), (thread_count + 1) * (operations_per_thread + 1))
  
  // Verify no corrupted configuration data
  for (key, value) in all_configs {
    assert_true(key.length() > 0)
    assert_true(value.length() > 0)
    
    // Verify key format consistency
    assert_true(key.starts_with("config.thread_"))
    assert_true(key.contains("."))
    
    // Verify value format consistency
    assert_true(value.starts_with("value.thread_"))
    assert_true(value.contains("."))
  }
  
  // Verify configuration statistics
  let config_stats = ConfigManager::get_statistics(config_manager)
  assert_eq(config_stats.total_configurations, (thread_count + 1) * (operations_per_thread + 1))
  assert_eq(config_stats.read_operations, (thread_count + 1) * (operations_per_thread + 1))
  assert_eq(config_stats.write_operations, (thread_count + 1) * (operations_per_thread + 1))
  assert_eq(config_stats.validation_operations, (thread_count + 1) * ((operations_per_thread + 1) / 5 + 1))
}

// Test 9: Concurrent Memory Management Safety
test "concurrent memory management safety" {
  let memory_manager = MemoryManager::new()
  let thread_count = 12
  let allocations_per_thread = 200
  
  // Create multiple threads that manage memory concurrently
  let threads = []
  for thread_id in 0..=thread_count {
    let thread = Thread::spawn(fn() {
      for i in 0..=allocations_per_thread {
        let allocation_id = "alloc_" + thread_id.to_string() + "_" + i.to_string()
        let allocation_size = 1024 * (i % 10 + 1) // 1KB to 10KB
        
        // Concurrent memory allocation
        let allocation = MemoryManager::allocate(memory_manager, allocation_id, allocation_size)
        match allocation {
          Some(alloc) => {
            // Concurrent memory usage
            MemoryManager::mark_in_use(memory_manager, alloc)
            
            // Concurrent memory operations
            let data = "test data from thread " + thread_id.to_string() + " iteration " + i.to_string()
            MemoryManager::write_data(memory_manager, alloc, data)
            
            let read_data = MemoryManager::read_data(memory_manager, alloc)
            assert_eq(read_data, data)
            
            // Concurrent memory deallocation
            MemoryManager::deallocate(memory_manager, alloc)
          }
          None => {
            // Allocation might fail due to memory pressure
            // This is acceptable in concurrent scenarios
          }
        }
        
        // Concurrent memory cleanup
        if i % 25 == 0 {
          MemoryManager::cleanup_leaks(memory_manager)
        }
        
        // Concurrent memory statistics
        if i % 15 == 0 {
          let stats = MemoryManager::get_statistics(memory_manager)
          assert_true(stats.total_allocations >= 0)
          assert_true(stats.active_allocations >= 0)
          assert_true(stats.memory_usage_bytes >= 0)
        }
        
        // Concurrent garbage collection
        if i % 50 == 0 {
          MemoryManager::force_gc(memory_manager)
        }
      }
    })
    threads = threads.push(thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    Thread::join(thread)
  }
  
  // Final cleanup
  MemoryManager::cleanup_leaks(memory_manager)
  MemoryManager::force_gc(memory_manager)
  
  // Verify memory management integrity after concurrent operations
  let final_stats = MemoryManager::get_statistics(memory_manager)
  
  // Verify no memory leaks
  assert_eq(final_stats.leaked_allocations, 0)
  
  // Verify allocation consistency
  assert_eq(final_stats.total_allocations, final_stats.deallocated_allocations + final_stats.active_allocations)
  
  // Verify no corrupted memory
  assert_eq(final_stats.corrupted_allocations, 0)
  
  // Verify memory usage is reasonable
  assert_true(final_stats.memory_usage_bytes < 100 * 1024 * 1024) // Less than 100MB
}

// Test 10: Concurrent Thread Pool Safety
test "concurrent thread pool safety" {
  let thread_pool = ThreadPool::new(4) // 4 worker threads
  let task_count = 100
  
  // Create shared data structures for concurrent access
  let shared_counter = AtomicCounter::new(0)
  let shared_results = ConcurrentList::new()
  let shared_map = ConcurrentHashMap::new()
  
  // Submit multiple tasks to thread pool
  let task_futures = []
  for i in 0..=task_count {
    let task_future = ThreadPool::submit(thread_pool, fn() {
      // Concurrent counter increment
      AtomicCounter::increment(shared_counter)
      
      // Concurrent result addition
      let result = "task_" + i.to_string() + "_result"
      ConcurrentList::add(shared_results, result)
      
      // Concurrent map operations
      let key = "key_" + i.to_string()
      let value = "value_" + i.to_string()
      ConcurrentHashMap::put(shared_map, key, value)
      
      // Simulate some work
      Thread::sleep(10) // 10ms
      
      // Concurrent map lookup
      let retrieved_value = ConcurrentHashMap::get(shared_map, key)
      match retrieved_value {
        Some(retrieved) => assert_eq(retrieved, value)
        None => assert_true(false)
      }
      
      return i * 2
    })
    task_futures = task_futures.push(task_future)
  }
  
  // Wait for all tasks to complete
  let mut task_results = []
  for future in task_futures {
    let result = ThreadPool::get_result(future)
    task_results = task_results.push(result)
  }
  
  // Verify thread pool integrity after concurrent operations
  let pool_stats = ThreadPool::get_statistics(thread_pool)
  
  // Verify all tasks were executed
  assert_eq(pool_stats.tasks_submitted, task_count + 1)
  assert_eq(pool_stats.tasks_completed, task_count + 1)
  assert_eq(pool_stats.tasks_failed, 0)
  
  // Verify shared data consistency
  assert_eq(AtomicCounter::get_value(shared_counter), task_count + 1)
  assert_eq(shared_results.length(), task_count + 1)
  assert_eq(shared_map.size(), task_count + 1)
  
  // Verify task results consistency
  assert_eq(task_results.length(), task_count + 1)
  for i in 0..=task_count {
    assert_eq(task_results[i], i * 2)
  }
  
  // Verify no thread corruption
  assert_eq(pool_stats.corrupted_threads, 0)
  
  // Verify thread pool is still functional
  let final_task = ThreadPool::submit(thread_pool, fn() {
    return "final_task_result"
  })
  let final_result = ThreadPool::get_result(final_task)
  assert_eq(final_result, "final_task_result")
  
  // Shutdown thread pool
  ThreadPool::shutdown(thread_pool)
  assert_true(ThreadPool::is_shutdown(thread_pool))
}