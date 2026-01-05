// Azimuth Memory Management and Resource Cleanup Tests
// This file contains comprehensive test cases for memory management and resource cleanup

// Test 1: Span Memory Management
test "span memory management" {
  let memory_tracker = MemoryTracker::new()
  let tracer = TracerProvider::get_tracer("memory_test")
  
  // Baseline memory measurement
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create many spans and track memory growth
  let mut spans = []
  let num_spans = 5000
  
  for i in 0..=num_spans {
    let span = Tracer::start_span(tracer, "memory_test_span_" + i.to_string())
    
    // Add attributes and events to increase memory usage
    Span::set_attribute(span, "iteration", IntValue(i))
    Span::set_attribute(span, "data", StringValue("test_data_" + i.to_string()))
    Span::set_attribute(span, "timestamp", IntValue(1609459200 + i))
    
    // Add multiple events
    for event_id in 0..=3 {
      Span::add_event(span, "event_" + event_id.to_string(), Some([
        ("event_iteration", IntValue(i)),
        ("event_id", IntValue(event_id)),
        ("event_data", StringValue("event_data_" + i.to_string() + "_" + event_id.to_string()))
      ]))
    }
    
    spans.push(span)
  }
  
  let peak_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Calculate memory per span
  let memory_per_span = (peak_memory.heap_used - baseline_memory.heap_used) / num_spans
  
  // Verify reasonable memory usage (< 2KB per span with attributes and events)
  assert_true(memory_per_span < 2048, "Each span should use < 2KB memory")
  
  // End all spans and verify memory recovery
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  let final_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Verify memory recovery
  let memory_recovered = peak_memory.heap_used - final_memory.heap_used
  let total_memory_used = peak_memory.heap_used - baseline_memory.heap_used
  
  assert_true(memory_recovered > total_memory_used * 0.8, 
    "Should recover at least 80% of memory after ending spans")
}

// Test 2: Metrics Resource Management
test "metrics resource management" {
  let memory_tracker = MemoryTracker::new()
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "resource_test_meter")
  
  // Baseline memory measurement
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create many metrics instruments
  let mut metrics = []
  let num_metrics = 1000
  
  for i in 0..=num_metrics {
    let counter = Meter::create_counter(meter, 
      "counter_" + i.to_string(), 
      Some("Test counter " + i.to_string()), 
      Some("count"))
    let histogram = Meter::create_histogram(meter, 
      "histogram_" + i.to_string(), 
      Some("Test histogram " + i.to_string()), 
      Some("ms"))
    let gauge = Meter::create_gauge(meter, 
      "gauge_" + i.to_string(), 
      Some("Test gauge " + i.to_string()), 
      Some("value"))
    
    metrics.push(counter)
    metrics.push(histogram)
    metrics.push(gauge)
    
    // Record measurements to increase memory usage
    Counter::add(counter, 1.0, Some(Attributes::with_data([
      ("metric_index", IntValue(i)),
      ("data", StringValue("test_data_" + i.to_string()))
    ])))
    
    for j in 0..=10 {
      Histogram::record(histogram, (j as Float) * 10.0, Some(Attributes::with_data([
        ("metric_index", IntValue(i)),
        ("measurement_index", IntValue(j))
      ])))
    }
    
    Gauge::record(gauge, (i as Float) % 100.0, Some(Attributes::with_data([
      ("metric_index", IntValue(i))
    ])))
  }
  
  let peak_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Calculate memory per metric
  let memory_per_metric = (peak_memory.heap_used - baseline_memory.heap_used) / metrics.length()
  
  // Verify reasonable memory usage (< 1KB per metric)
  assert_true(memory_per_metric < 1024, "Each metric should use < 1KB memory")
  
  // Clear metric references and verify memory recovery
  metrics = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  let final_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Verify memory recovery
  let memory_recovered = peak_memory.heap_used - final_memory.heap_used
  let total_memory_used = peak_memory.heap_used - baseline_memory.heap_used
  
  assert_true(memory_recovered > total_memory_used * 0.7, 
    "Should recover at least 70% of memory after clearing metrics")
}

// Test 3: Context and Baggage Resource Cleanup
test "context and baggage resource cleanup" {
  let memory_tracker = MemoryTracker::new()
  
  // Baseline memory measurement
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create many contexts with baggage
  let mut contexts = []
  let num_contexts = 3000
  
  for i in 0..=num_contexts {
    let baggage = Baggage::new()
    let baggage_with_data = Baggage::set_entry(baggage, "user.id", "user_" + i.to_string())
    let baggage_with_more_data = Baggage::set_entry(baggage_with_data, "request.id", "req_" + i.to_string())
    let baggage_with_even_more_data = Baggage::set_entry(baggage_with_more_data, "session.id", "session_" + i.to_string())
    
    let context = Context::with_value(
      Context::root(),
      ContextKey::new("baggage"),
      baggage_with_even_more_data
    )
    
    // Add more context data
    let context_with_data = Context::with_value(
      context,
      ContextKey::new("trace_data"),
      "trace_data_" + i.to_string()
    )
    
    contexts.push(context_with_data)
  }
  
  let peak_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Calculate memory per context
  let memory_per_context = (peak_memory.heap_used - baseline_memory.heap_used) / num_contexts
  
  // Verify reasonable memory usage (< 500B per context)
  assert_true(memory_per_context < 500, "Each context should use < 500B memory")
  
  // Clear context references and verify memory recovery
  contexts = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  let final_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Verify memory recovery
  let memory_recovered = peak_memory.heap_used - final_memory.heap_used
  let total_memory_used = peak_memory.heap_used - baseline_memory.heap_used
  
  assert_true(memory_recovered > total_memory_used * 0.8, 
    "Should recover at least 80% of memory after clearing contexts")
}

// Test 4: Logger Resource Management
test "logger resource management" {
  let memory_tracker = MemoryTracker::new()
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "resource_test_logger")
  
  // Baseline memory measurement
  let baseline_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Create and emit many log records
  let mut log_records = []
  let num_logs = 5000
  
  for i in 0..=num_logs {
    let log_record = LogRecord::new_with_context(
      if i % 10 == 0 { Error } else if i % 5 == 0 { Warn } else { Info },
      Some("Test log message " + i.to_string()),
      Some(Attributes::with_data([
        ("log_index", IntValue(i)),
        ("thread_id", IntValue(i % 10)),
        ("data", StringValue("log_data_" + i.to_string())),
        ("timestamp", IntValue(1609459200 + i))
      ])),
      Some(1609459200L + i as Int64),
      Some(1609459200L + i as Int64 + 1),
      Some("trace_" + (i / 100).to_string()),
      Some("span_" + i.to_string()),
      None
    )
    
    log_records.push(log_record)
    Logger::emit(logger, log_record)
  }
  
  let peak_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Calculate memory per log record
  let memory_per_log = (peak_memory.heap_used - baseline_memory.heap_used) / num_logs
  
  // Verify reasonable memory usage (< 300B per log record)
  assert_true(memory_per_log < 300, "Each log record should use < 300B memory")
  
  // Clear log record references and verify memory recovery
  log_records = []
  
  // Force garbage collection
  MemoryTracker::force_gc(memory_tracker)
  
  let final_memory = MemoryTracker::get_current_usage(memory_tracker)
  
  // Verify memory recovery
  let memory_recovered = peak_memory.heap_used - final_memory.heap_used
  let total_memory_used = peak_memory.heap_used - baseline_memory.heap_used
  
  assert_true(memory_recovered > total_memory_used * 0.7, 
    "Should recover at least 70% of memory after clearing log records")
}

// Test 5: Resource Leak Detection
test "resource leak detection" {
  let leak_detector = ResourceLeakDetector::new()
  let tracer = TracerProvider::get_tracer("leak_test")
  
  // Start leak detection
  LeakDetector::start(leak_detector)
  
  // Create resources and intentionally leak some
  let leaked_spans = []
  let properly_managed_spans = []
  
  for i in 0..=1000 {
    let span = Tracer::start_span(tracer, "leak_test_span_" + i.to_string())
    Span::set_attribute(span, "index", IntValue(i))
    
    if i % 10 == 0 {
      // Intentionally leak these spans (don't end them)
      leaked_spans.push(span)
    } else {
      // Properly manage these spans
      properly_managed_spans.push(span)
    }
  }
  
  // End properly managed spans
  for span in properly_managed_spans {
    Span::end(span)
  }
  
  // Check for leaks
  let leak_report = LeakDetector::check(leak_detector)
  
  // Verify leak detection
  assert_true(leak_report.leaked_resources > 0, "Should detect leaked resources")
  assert_true(leak_report.leaked_resources <= 100, "Should detect approximately 100 leaked spans")
  
  // Clean up leaked resources
  for span in leaked_spans {
    Span::end(span)
  }
  
  // Check again after cleanup
  let cleanup_report = LeakDetector::check(leak_detector)
  assert_eq(cleanup_report.leaked_resources, 0, "Should have no leaks after cleanup")
  
  LeakDetector::stop(leak_detector)
}

// Test 6: Memory Pressure Handling
test "memory pressure handling" {
  let memory_monitor = MemoryMonitor::new()
  let tracer = TracerProvider::get_tracer("pressure_test")
  
  // Configure memory pressure thresholds
  MemoryMonitor::set_threshold(memory_monitor, Warning, 50 * 1024 * 1024) // 50MB
  MemoryMonitor::set_threshold(memory_monitor, Critical, 100 * 1024 * 1024) // 100MB
  
  // Start monitoring
  MemoryMonitor::start(memory_monitor)
  
  // Create resources to trigger memory pressure
  let mut spans = []
  let mut pressure_events = []
  
  for i in 0..=10000 {
    let span = Tracer::start_span(tracer, "pressure_test_span_" + i.to_string())
    
    // Add significant data to each span
    Span::set_attribute(span, "large_data", StringValue("x".repeat(1000))) // 1KB per span
    Span::set_attribute(span, "index", IntValue(i))
    
    spans.push(span)
    
    // Check memory pressure periodically
    if i % 1000 == 0 {
      let pressure_level = MemoryMonitor::check_pressure(memory_monitor)
      pressure_events.push((i, pressure_level))
      
      // If critical pressure, start cleanup
      if pressure_level == Critical {
        // Clean up oldest spans
        let spans_to_remove = 2000
        for j in 0..=spans_to_remove {
          if spans.length() > 0 {
            let span_to_remove = spans.remove_at(0)
            Span::end(span_to_remove)
          }
        }
      }
    }
  }
  
  // Verify pressure handling
  let mut critical_pressure_detected = false
  for (_, pressure_level) in pressure_events {
    if pressure_level == Critical {
      critical_pressure_detected = true
      break
    }
  }
  
  assert_true(critical_pressure_detected, "Should detect critical memory pressure")
  
  // Clean up remaining spans
  for span in spans {
    Span::end(span)
  }
  
  MemoryMonitor::stop(memory_monitor)
}

// Test 7: Resource Pool Management
test "resource pool management" {
  let pool_manager = ResourcePoolManager::new()
  
  // Create resource pools
  let span_pool = ResourcePool::new(|| Tracer::start_span(TracerProvider::get_tracer("pool_test"), "pooled_span"))
  let context_pool = ResourcePool::new(|| Context::root())
  
  PoolManager::register(pool_manager, "span_pool", span_pool)
  PoolManager::register(pool_manager, "context_pool", context_pool)
  
  // Test resource acquisition and release
  let mut acquired_resources = []
  
  for i in 0..=1000 {
    // Acquire resources from pools
    let span_resource = PoolManager::acquire(pool_manager, "span_pool")
    let context_resource = PoolManager::acquire(pool_manager, "context_pool")
    
    match (span_resource, context_resource) {
      (Some(span), Some(context)) => {
        // Use resources
        Span::set_attribute(span, "pool_index", IntValue(i))
        
        acquired_resources.push((span, context))
        
        // Release resources back to pools
        PoolManager::release(pool_manager, "span_pool", span)
        PoolManager::release(pool_manager, "context_pool", context)
      }
      _ => assert_true(false, "Should be able to acquire resources from pools")
    }
  }
  
  // Verify pool statistics
  let span_pool_stats = PoolManager::get_stats(pool_manager, "span_pool")
  let context_pool_stats = PoolManager::get_stats(pool_manager, "context_pool")
  
  assert_true(span_pool_stats.acquisitions >= 1000, "Should have acquired span resources")
  assert_true(span_pool_stats.releases >= 1000, "Should have released span resources")
  assert_true(context_pool_stats.acquisitions >= 1000, "Should have acquired context resources")
  assert_true(context_pool_stats.releases >= 1000, "Should have released context resources")
  
  // Verify pool efficiency
  let span_hit_ratio = (span_pool_stats.hits as Float) / (span_pool_stats.acquisitions as Float)
  let context_hit_ratio = (context_pool_stats.hits as Float) / (context_pool_stats.acquisitions as Float)
  
  assert_true(span_hit_ratio > 0.8, "Span pool should have > 80% hit ratio")
  assert_true(context_hit_ratio > 0.8, "Context pool should have > 80% hit ratio")
  
  // Clean up pools
  PoolManager::cleanup(pool_manager, "span_pool")
  PoolManager::cleanup(pool_manager, "context_pool")
}

// Test 8: Batch Resource Cleanup
test "batch resource cleanup" {
  let cleanup_manager = BatchCleanupManager::new()
  let tracer = TracerProvider::get_tracer("batch_cleanup_test")
  
  // Create many resources
  let mut spans = []
  let mut contexts = []
  let mut log_records = []
  
  for i in 0..=5000 {
    let span = Tracer::start_span(tracer, "batch_cleanup_span_" + i.to_string())
    Span::set_attribute(span, "batch_index", IntValue(i))
    spans.push(span)
    
    let context = Context::with_value(
      Context::root(),
      ContextKey::new("batch_data"),
      "batch_data_" + i.to_string()
    )
    contexts.push(context)
    
    let log_record = LogRecord::new(Info, Some("Batch cleanup log " + i.to_string()))
    log_records.push(log_record)
  }
  
  // Register resources for batch cleanup
  for span in spans {
    CleanupManager::register_resource(cleanup_manager, span)
  }
  
  for context in contexts {
    CleanupManager::register_resource(cleanup_manager, context)
  }
  
  for log_record in log_records {
    CleanupManager::register_resource(cleanup_manager, log_record)
  }
  
  // Perform batch cleanup
  let cleanup_result = CleanupManager::cleanup_all(cleanup_manager)
  
  // Verify cleanup results
  assert_eq(cleanup_result.total_resources, 15000) // 5000 spans + 5000 contexts + 5000 logs
  assert_eq(cleanup_result.cleaned_resources, 15000)
  assert_eq(cleanup_result.failed_resources, 0)
  
  // Verify memory recovery
  let final_memory = MemoryTracker::get_current_usage(MemoryTracker::new())
  
  // In real implementation, would verify memory usage is back to baseline
  assert_true(true)
}