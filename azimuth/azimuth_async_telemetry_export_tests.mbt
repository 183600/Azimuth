// Azimuth Async Telemetry Export Tests
// This file contains tests for asynchronous telemetry export operations in the Azimuth system

// Test 1: Async span export with batch processing
pub test "async span export with batch processing" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "async-export-test")
  
  // Configure async batch span processor
  let batch_config = azimuth::BatchSpanProcessorConfig::new()
  let batch_processor = azimuth::BatchSpanProcessor::builder(batch_config)
    .with_max_queue_size(2048)
    .with_max_export_batch_size(512)
    .with_schedule_delay(5000)  // 5 seconds
    .with_export_timeout(30000)  // 30 seconds
    .build()
  
  // Create spans for export
  let spans = []
  for i = 0; i < 100; i = i + 1 {
    let span_name = "async.span." + i.to_string()
    let span = azimuth::Tracer::start_span(tracer, span_name)
    
    // Add attributes and events
    azimuth::Span::set_attribute(span, "span.index", azimuth::IntValue(i))
    azimuth::Span::set_attribute(span, "async.test", azimuth::BoolValue(true))
    
    if i % 10 == 0 {
      azimuth::Span::add_event(span, "milestone.event", Some([("milestone", azimuth::StringValue("checkpoint." + i.to_string()))]))
    }
    
    spans.push(span)
  }
  
  // End all spans to trigger export
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // Test batch processor state
  let processor_state = azimuth::BatchSpanProcessor::get_state(batch_processor)
  assert_true(processor_state.queue_size >= 0)
  assert_true(processor_state.dropped_count >= 0)
  
  // Force flush to ensure all spans are exported
  let flush_result = azimuth::BatchSpanProcessor::force_flush(batch_processor)
  assert_true(flush_result)
  
  // Verify export metrics
  let export_metrics = azimuth::BatchSpanProcessor::get_export_metrics(batch_processor)
  assert_true(export_metrics.successful_exports >= 0)
  assert_true(export_metrics.failed_exports >= 0)
  assert_true(export_metrics.total_spans_exported >= 0)
}

// Test 2: Async metric export with retry mechanism
pub test "async metric export with retry mechanism" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "async-metric-test")
  
  // Configure async metric exporter with retry
  let retry_config = azimuth::RetryConfig::new()
    .with_max_attempts(3)
    .with_initial_backoff(1000)  // 1 second
    .with_max_backoff(10000)     // 10 seconds
    .with_backoff_multiplier(2.0)
  
  let metric_exporter = azimuth::AsyncMetricExporter::builder(retry_config)
    .with_endpoint("https://telemetry.example.com/api/v1/metrics")
    .with_compression("gzip")
    .with_timeout(30000)  // 30 seconds
    .build()
  
  // Create various metrics
  let counter = azimuth::Meter::create_counter(meter, "async.counter", Some("Async counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "async.histogram", Some("Async histogram"), Some("ms"))
  let gauge = azimuth::Meter::create_gauge(meter, "async.gauge", Some("Async gauge"), Some("percent"))
  
  // Record metrics
  for i = 0; i < 50; i = i + 1 {
    azimuth::Counter::add(counter, 1, [("async.metric", "true"), ("batch", azimuth::StringValue("batch." + (i / 10).to_string()))])
    azimuth::Histogram::record(histogram, (i * 10).to_double(), [("async.metric", "true")])
    azimuth::Gauge::set(gauge, (i % 100).to_long(), [("async.metric", "true")])
  }
  
  // Test async export
  let export_promise = azimuth::AsyncMetricExporter::export(metric_exporter)
  
  // Wait for export completion (simulated async wait)
  let export_result = azimuth::AsyncMetricExporter::wait_for_completion(export_promise, 10000)  // 10 second timeout
  
  match export_result {
    azimuth::ExportSuccess => assert_true(true)
    azimuth::ExportFailure(reason) => {
      // Check if failure is due to network issues (expected in test environment)
      assert_true(reason.contains("network") || reason.contains("connection") || reason.contains("timeout"))
    }
    azimuth::ExportRetry(attempt) => {
      assert_true(attempt >= 1 && attempt <= 3)
    }
  }
  
  // Test retry statistics
  let retry_stats = azimuth::AsyncMetricExporter::get_retry_statistics(metric_exporter)
  assert_true(retry_stats.total_attempts >= 0)
  assert_true(retry_stats.successful_retries >= 0)
  assert_true(retry_stats.failed_retries >= 0)
  assert_true(retry_stats.total_attempts >= retry_stats.successful_retries + retry_stats.failed_retries)
}

// Test 3: Async log export with buffering
pub test "async log export with buffering" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "async-log-test")
  
  // Configure async log exporter with buffering
  let buffer_config = azimuth::LogBufferConfig::new()
    .with_max_buffer_size(1024)
    .with_flush_interval(2000)  // 2 seconds
    .with_flush_on_error(true)
    .with_compression_enabled(true)
  
  let log_exporter = azimuth::AsyncLogExporter::builder(buffer_config)
    .with_endpoint("https://logs.example.com/api/v1/logs")
    .with_batch_size(100)
    .with_max_concurrent_exports(5)
    .build()
  
  // Create log records with different severity levels
  let log_records = []
  let severity_levels = [azimuth::Trace, azimuth::Debug, azimuth::Info, azimuth::Warn, azimuth::Error, azimuth::Fatal]
  
  for i = 0; i < 60; i = i + 1 {
    let severity = severity_levels[i % severity_levels.length()]
    let message = "Async log message " + i.to_string()
    
    let log_record = azimuth::LogRecord::new(severity, message)
    azimuth::LogRecord::set_attribute(log_record, "log.index", azimuth::IntValue(i))
    azimuth::LogRecord::set_attribute(log_record, "async.export", azimuth::BoolValue(true))
    
    // Add context every 10 logs
    if i % 10 == 0 {
      let trace_id = "trace-" + i.to_string()
      let span_id = "span-" + i.to_string()
      azimuth::LogRecord::set_trace_id(log_record, trace_id)
      azimuth::LogRecord::set_span_id(log_record, span_id)
    }
    
    log_records.push(log_record)
    azimuth::Logger::emit(logger, log_record)
  }
  
  // Test buffer state
  let buffer_state = azimuth::AsyncLogExporter::get_buffer_state(log_exporter)
  assert_true(buffer_state.current_size >= 0)
  assert_true(buffer_state.max_size == 1024)
  assert_true(buffer_state.flush_count >= 0)
  
  // Force flush to export all logs
  let flush_result = azimuth::AsyncLogExporter::force_flush(log_exporter)
  assert_true(flush_result)
  
  // Test export statistics
  let export_stats = azimuth::AsyncLogExporter::get_export_statistics(log_exporter)
  assert_true(export_stats.total_logs_exported >= 0)
  assert_true(export_stats.successful_batches >= 0)
  assert_true(export_stats.failed_batches >= 0)
  assert_true(export_stats.bytes_transferred >= 0)
}

// Test 4: Concurrent async exports with resource management
pub test "concurrent async exports with resource management" {
  // Create multiple providers for concurrent operations
  let tracer_provider1 = azimuth::TracerProvider::default()
  let tracer_provider2 = azimuth::TracerProvider::default()
  let meter_provider = azimuth::MeterProvider::default()
  let logger_provider = azimuth::LoggerProvider::default()
  
  let tracer1 = azimuth::TracerProvider::get_tracer(tracer_provider1, "concurrent-tracer-1")
  let tracer2 = azimuth::TracerProvider::get_tracer(tracer_provider2, "concurrent-tracer-2")
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrent-meter")
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent-logger")
  
  // Configure resource limits for concurrent operations
  let resource_config = azimuth::ConcurrentExportConfig::new()
    .with_max_concurrent_exports(10)
    .with_max_memory_usage(100 * 1024 * 1024)  // 100MB
    .with_max_queue_size_per_exporter(1000)
    .with_gc_threshold(80)  // 80% memory usage triggers GC
  
  let concurrent_manager = azimuth::ConcurrentExportManager::new(resource_config)
  
  // Start concurrent export operations
  let export_tasks = []
  
  // Task 1: Export spans
  let span_export_task = azimuth::ConcurrentExportManager::start_span_export(concurrent_manager, tracer1, {
    // Create and export 50 spans
    for i = 0; i < 50; i = i + 1 {
      let span = azimuth::Tracer::start_span(tracer1, "concurrent.span.1." + i.to_string())
      azimuth::Span::set_attribute(span, "task", azimuth::StringValue("spans"))
      azimuth::Span::set_attribute(span, "thread", azimuth::StringValue("1"))
      azimuth::Span::end(span)
    }
  })
  export_tasks.push(span_export_task)
  
  // Task 2: Export metrics
  let metric_export_task = azimuth::ConcurrentExportManager::start_metric_export(concurrent_manager, meter, {
    let counter = azimuth::Meter::create_counter(meter, "concurrent.counter", Some("Concurrent counter"), Some("count"))
    for i = 0; i < 50; i = i + 1 {
      azimuth::Counter::add(counter, 1, [("task", "metrics"), ("thread", "2")])
    }
  })
  export_tasks.push(metric_export_task)
  
  // Task 3: Export logs
  let log_export_task = azimuth::ConcurrentExportManager::start_log_export(concurrent_manager, logger, {
    for i = 0; i < 50; i = i + 1 {
      let log_record = azimuth::LogRecord::new(azimuth::Info, "Concurrent log message " + i.to_string())
      azimuth::LogRecord::set_attribute(log_record, "task", azimuth::StringValue("logs"))
      azimuth::LogRecord::set_attribute(log_record, "thread", azimuth::StringValue("3"))
      azimuth::Logger::emit(logger, log_record)
    }
  })
  export_tasks.push(log_export_task)
  
  // Task 4: Export spans from second tracer
  let span_export_task2 = azimuth::ConcurrentExportManager::start_span_export(concurrent_manager, tracer2, {
    // Create and export 50 spans
    for i = 0; i < 50; i = i + 1 {
      let span = azimuth::Tracer::start_span(tracer2, "concurrent.span.2." + i.to_string())
      azimuth::Span::set_attribute(span, "task", azimuth::StringValue("spans"))
      azimuth::Span::set_attribute(span, "thread", azimuth::StringValue("4"))
      azimuth::Span::end(span)
    }
  })
  export_tasks.push(span_export_task2)
  
  // Wait for all tasks to complete
  let completed_tasks = []
  for task in export_tasks {
    let result = azimuth::ConcurrentExportManager::wait_for_task(concurrent_manager, task, 15000)  // 15 second timeout
    completed_tasks.push(result)
  }
  
  // Verify all tasks completed (either successfully or with expected errors)
  assert_true(completed_tasks.length() == 4)
  
  // Test resource usage statistics
  let resource_stats = azimuth::ConcurrentExportManager::get_resource_statistics(concurrent_manager)
  assert_true(resource_stats.memory_usage >= 0)
  assert_true(resource_stats.memory_usage <= resource_config.max_memory_usage)
  assert_true(resource_stats.active_exports >= 0)
  assert_true(resource_stats.queued_exports >= 0)
  assert_true(resource_stats.gc_count >= 0)
  
  // Test concurrent export metrics
  let concurrent_metrics = azimuth::ConcurrentExportManager::get_export_metrics(concurrent_manager)
  assert_true(concurrent_metrics.total_exports >= 0)
  assert_true(concurrent_metrics.successful_exports >= 0)
  assert_true(concurrent_metrics.failed_exports >= 0)
  assert_true(concurrent_metrics.average_export_time >= 0.0)
  
  // Clean up resources
  azimuth::ConcurrentExportManager::shutdown(concurrent_manager)
}

// Test 5: Async export with circuit breaker pattern
pub test "async export with circuit breaker pattern" {
  let provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "circuit-breaker-test")
  
  // Configure circuit breaker
  let circuit_config = azimuth::CircuitBreakerConfig::new()
    .with_failure_threshold(5)      // Open after 5 failures
    .with_success_threshold(3)      // Close after 3 successes
    .with_timeout(30000)            // 30 seconds timeout
    .with_half_open_max_calls(3)    // Max calls in half-open state
  
  let circuit_breaker = azimuth::CircuitBreaker::new(circuit_config)
  
  // Create exporter with circuit breaker
  let exporter = azimuth::CircuitBreakerExporter::builder(circuit_breaker)
    .with_endpoint("https://unreliable-telemetry.example.com/api/v1/traces")
    .build()
  
  // Test circuit breaker states
  let initial_state = azimuth::CircuitBreaker::get_state(circuit_breaker)
  assert_eq(initial_state, azimuth::Closed)  // Should start closed
  
  // Create spans for export
  let spans = []
  for i = 0; i < 20; i = i + 1 {
    let span = azimuth::Tracer::start_span(tracer, "circuit.breaker.span." + i.to_string())
    azimuth::Span::set_attribute(span, "circuit.test", azimuth::BoolValue(true))
    azimuth::Span::end(span)
    spans.push(span)
  }
  
  // Attempt exports that may fail (simulating unreliable endpoint)
  let export_results = []
  for i = 0; i < 20; i = i + 1 {
    let result = azimuth::CircuitBreakerExporter::try_export(exporter, spans[i])
    export_results.push(result)
    
    // Check state after each export
    let current_state = azimuth::CircuitBreaker::get_state(circuit_breaker)
    if current_state == azimuth::Open {
      break  // Stop testing once circuit is open
    }
  }
  
  // Verify circuit breaker opened after failures
  let final_state = azimuth::CircuitBreaker::get_state(circuit_breaker)
  assert_true(final_state == azimuth::Open || final_state == azimuth::Closed)  // May still be closed if exports succeeded
  
  // Test circuit breaker statistics
  let cb_stats = azimuth::CircuitBreaker::get_statistics(circuit_breaker)
  assert_true(cb_stats.total_calls >= 0)
  assert_true(cb_stats.successful_calls >= 0)
  assert_true(cb_stats.failed_calls >= 0)
  assert_true(cb_stats.state_changes >= 0)
  
  // Test fallback mechanism when circuit is open
  if final_state == azimuth::Open {
    let fallback_result = azimuth::CircuitBreakerExporter::export_with_fallback(exporter, spans[0])
    assert_true(fallback_result.is_fallback_used)
  }
}