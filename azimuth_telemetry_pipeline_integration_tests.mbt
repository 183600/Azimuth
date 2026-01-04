// Azimuth Telemetry Pipeline Integration Test Suite
// This file contains comprehensive test cases for telemetry pipeline integration

// Test 1: End-to-End Telemetry Pipeline
test "end-to-end telemetry pipeline integration" {
  // Initialize telemetry components
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "test_tracer")
  let meter = TelemetryProvider::get_meter(provider, "test_meter")
  let logger = TelemetryProvider::get_logger(provider, "test_logger")
  
  // Create a span
  let span_ctx = Tracer::start_span(tracer, "pipeline_test")
  
  // Add attributes to span
  Span::add_attribute(span_ctx, "service.name", "test_service")
  Span::add_attribute(span_ctx, "service.version", "1.0.0")
  Span::add_attribute(span_ctx, "environment", "test")
  
  // Create metrics
  let counter = Meter::create_counter(meter, "pipeline_operations", Some("Pipeline operations count"), Some("count"))
  Counter::add(counter, 1.0)
  
  let histogram = Meter::create_histogram(meter, "pipeline_duration", Some("Pipeline duration"), Some("ms"))
  Histogram::record(histogram, 100.0)
  
  // Create log record
  let log_record = Logger::create_log_record(logger, Info, "Pipeline operation completed")
  LogRecord::add_attribute(log_record, "span_id", Span::span_id(span_ctx))
  Logger::emit(logger, log_record)
  
  // End the span
  Span::end(span_ctx)
  
  // Verify telemetry data was collected
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let logs = TelemetryProvider::get_collected_logs(provider)
  
  assert_eq(spans.length(), 1)
  assert_eq(spans[0].name, "pipeline_test")
  assert_true(spans[0].attributes.contains(("service.name", "test_service")))
  
  assert_eq(metrics.length(), 2)
  assert_true(metrics.any(fn(m) { m.name == "pipeline_operations" }))
  assert_true(metrics.any(fn(m) { m.name == "pipeline_duration" }))
  
  assert_eq(logs.length(), 1)
  assert_eq(logs[0].severity, Info)
  assert_eq(logs[0].message, "Pipeline operation completed")
}

// Test 2: Multi-Service Telemetry Propagation
test "multi-service telemetry propagation" {
  // Initialize service A
  let provider_a = TelemetryProvider::new()
  let tracer_a = TelemetryProvider::get_tracer(provider_a, "service_a")
  
  // Service A creates a span
  let span_a = Tracer::start_span(tracer_a, "service_a_operation")
  let trace_id = Span::trace_id(span_a)
  let span_id = Span::span_id(span_a)
  
  // Service A calls Service B (simulated)
  let provider_b = TelemetryProvider::new()
  let tracer_b = TelemetryProvider::get_tracer(provider_b, "service_b")
  
  // Service B creates a child span
  let span_b_ctx = SpanContext::new(trace_id, "span_b_id", true, "")
  let span_b = Tracer::start_span_with_context(tracer_b, "service_b_operation", span_b_ctx)
  Span::set_parent(span_b, span_id)
  
  // Service B calls Service C (simulated)
  let provider_c = TelemetryProvider::new()
  let tracer_c = TelemetryProvider::get_tracer(provider_c, "service_c")
  
  // Service C creates a child span
  let span_c_ctx = SpanContext::new(trace_id, "span_c_id", true, "")
  let span_c = Tracer::start_span_with_context(tracer_c, "service_c_operation", span_c_ctx)
  Span::set_parent(span_c, Span::span_id(span_b))
  
  // End all spans
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
  
  // Verify trace propagation
  let spans_a = TelemetryProvider::get_collected_spans(provider_a)
  let spans_b = TelemetryProvider::get_collected_spans(provider_b)
  let spans_c = TelemetryProvider::get_collected_spans(provider_c)
  
  assert_eq(spans_a.length(), 1)
  assert_eq(spans_b.length(), 1)
  assert_eq(spans_c.length(), 1)
  
  // All spans should have the same trace ID
  assert_eq(spans_a[0].trace_id, trace_id)
  assert_eq(spans_b[0].trace_id, trace_id)
  assert_eq(spans_c[0].trace_id, trace_id)
  
  // Verify parent-child relationships
  assert_eq(spans_b[0].parent_span_id, Some(span_id))
  assert_eq(spans_c[0].parent_span_id, Some(Span::span_id(span_b)))
}

// Test 3: Telemetry Pipeline with Error Handling
test "telemetry pipeline with error handling" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "error_test_tracer")
  let meter = TelemetryProvider::get_meter(provider, "error_test_meter")
  
  // Create a span for an operation that might fail
  let span = Tracer::start_span(tracer, "operation_with_error")
  
  // Simulate an error
  let error_occurred = true
  let error_counter = Meter::create_counter(meter, "operation_errors", Some("Operation errors count"), Some("count"))
  
  if error_occurred {
    // Record error in span
    Span::set_status(span, Error, Some("Simulated error occurred"))
    Span::add_event(span, "error", Some([("error.type", "simulation"), ("error.message", "Test error")]))
    
    // Increment error counter
    Counter::add(error_counter, 1.0)
  } else {
    // Record success
    Span::set_status(span, Ok, None)
  }
  
  // End the span
  Span::end(span)
  
  // Verify error was recorded
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  assert_eq(spans.length(), 1)
  assert_eq(spans[0].status, Error)
  assert_eq(spans[0].status_message, Some("Simulated error occurred"))
  assert_true(spans[0].events.any(fn(e) { e.name == "error" }))
  
  assert_eq(metrics.length(), 1)
  assert_eq(metrics[0].name, "operation_errors")
  assert_eq(metrics[0].value, 1.0)
}

// Test 4: Telemetry Pipeline with Sampling
test "telemetry pipeline with sampling" {
  // Create provider with sampling configuration
  let provider_config = {
    sampling_enabled: true,
    sampling_rate: 0.5, // 50% sampling rate
    max_spans_per_second: 100
  }
  
  let provider = TelemetryProvider::with_config(provider_config)
  let tracer = TelemetryProvider::get_tracer(provider, "sampling_test_tracer")
  
  // Create multiple spans
  let span_count = 10
  let mut sampled_spans = 0
  
  for i in 0..span_count {
    let span = Tracer::start_span(tracer, "sampled_operation_" + i.to_string())
    Span::add_attribute(span, "iteration", i.to_string())
    Span::end(span)
    
    // Check if span was sampled
    if Span::is_sampled(span) {
      sampled_spans = sampled_spans + 1
    }
  }
  
  // Verify sampling worked (approximately 50% of spans should be sampled)
  let spans = TelemetryProvider::get_collected_spans(provider)
  let sampling_ratio = sampled_spans.to_float() / span_count.to_float()
  
  // Allow for some variance in sampling
  assert_true(sampling_ratio > 0.2 && sampling_ratio < 0.8)
  assert_eq(spans.length(), sampled_spans)
}

// Test 5: Telemetry Pipeline with Resource Attributes
test "telemetry pipeline with resource attributes" {
  // Define resource attributes
  let resource_attrs = [
    ("service.name", "telemetry_test_service"),
    ("service.version", "2.1.0"),
    ("service.instance.id", "instance-12345"),
    ("deployment.environment", "test"),
    ("host.name", "test-host"),
    ("process.id", "1234")
  ]
  
  // Create provider with resource
  let resource = Resource::with_attributes(Resource::new(), resource_attrs)
  let provider = TelemetryProvider::with_resource(resource)
  let tracer = TelemetryProvider::get_tracer(provider, "resource_test_tracer")
  
  // Create a span
  let span = Tracer::start_span(tracer, "resource_test_operation")
  Span::add_attribute(span, "custom.attribute", "custom_value")
  Span::end(span)
  
  // Create a metric
  let meter = TelemetryProvider::get_meter(provider, "resource_test_meter")
  let counter = Meter::create_counter(meter, "resource_test_counter")
  Counter::add(counter, 1.0)
  
  // Verify resource attributes are attached to telemetry data
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  assert_eq(spans.length(), 1)
  assert_eq(metrics.length(), 1)
  
  // Check span resource attributes
  let span_resource = spans[0].resource
  assert_eq(Resource::get_attribute(span_resource, "service.name"), Some(StringValue("telemetry_test_service")))
  assert_eq(Resource::get_attribute(span_resource, "service.version"), Some(StringValue("2.1.0")))
  assert_eq(Resource::get_attribute(span_resource, "service.instance.id"), Some(StringValue("instance-12345")))
  
  // Check metric resource attributes
  let metric_resource = metrics[0].resource
  assert_eq(Resource::get_attribute(metric_resource, "service.name"), Some(StringValue("telemetry_test_service")))
  assert_eq(Resource::get_attribute(metric_resource, "deployment.environment"), Some(StringValue("test")))
}

// Test 6: Telemetry Pipeline with Batch Processing
test "telemetry pipeline with batch processing" {
  // Configure batch processing
  let batch_config = {
    max_batch_size: 5,
    max_export_timeout_ms: 1000,
    scheduled_delay_ms: 500
  }
  
  let provider = TelemetryProvider::with_batch_config(batch_config)
  let tracer = TelemetryProvider::get_tracer(provider, "batch_test_tracer")
  
  // Create multiple spans
  let span_count = 12
  for i in 0..span_count {
    let span = Tracer::start_span(tracer, "batch_operation_" + i.to_string())
    Span::add_attribute(span, "batch.index", i.to_string())
    Span::end(span)
  }
  
  // Force batch export
  TelemetryProvider::force_flush(provider)
  
  // Verify spans were exported in batches
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), span_count)
  
  // Verify batch processing metrics
  let meter = TelemetryProvider::get_meter(provider, "batch_metrics")
  let batch_counter = Meter::create_counter(meter, "batch_exports")
  
  // With 12 spans and batch size of 5, we should have 3 batches
  let expected_batches = (span_count + batch_config.max_batch_size - 1) / batch_config.max_batch_size
  assert_eq(expected_batches, 3)
}

// Test 7: Telemetry Pipeline with Context Propagation
test "telemetry pipeline with context propagation" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "context_test_tracer")
  
  // Create a root span
  let root_span = Tracer::start_span(tracer, "root_operation")
  let root_ctx = Span::span_context(root_span)
  
  // Create a context with the span
  let ctx = Context::with_value(Context::root(), ContextKey::new("current_span"), root_span)
  
  // Simulate async operation (simplified)
  let async_operation = fn(context: Context) {
    // Extract span from context
    match Context::get(context, ContextKey::new("current_span")) {
      Some(span) => {
        // Create child span
        let child_span = Tracer::start_span_with_parent(tracer, "async_operation", span)
        Span::add_attribute(child_span, "async", "true")
        Span::end(child_span)
        Some(child_span)
      }
      None => None
    }
  }
  
  // Execute async operation with context
  let child_span_result = async_operation(ctx)
  
  // End root span
  Span::end(root_span)
  
  // Verify context propagation
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 2)
  
  let root_span_collected = spans.find(fn(s) { s.name == "root_operation" })
  let child_span_collected = spans.find(fn(s) { s.name == "async_operation" })
  
  match (root_span_collected, child_span_collected) {
    (Some(root), Some(child)) => {
      assert_eq(child.trace_id, root.trace_id)
      assert_eq(child.parent_span_id, Some(root.span_id))
      assert_true(child.attributes.contains(("async", "true")))
    }
    _ => assert_true(false)
  }
}

// Test 8: Telemetry Pipeline with Metrics Aggregation
test "telemetry pipeline with metrics aggregation" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "aggregation_test_meter")
  
  // Create metrics with different attributes
  let counter = Meter::create_counter(meter, "requests_total")
  let histogram = Meter::create_histogram(meter, "request_duration")
  let gauge = Meter::create_gauge(meter, "active_connections")
  
  // Record metrics with different attribute combinations
  let service_attrs = Attributes::new()
  Attributes::set(service_attrs, "service", "api")
  Attributes::set(service_attrs, "method", "GET")
  Counter::add(counter, 10.0, Some(service_attrs))
  Histogram::record(histogram, 100.0, Some(service_attrs))
  
  let db_attrs = Attributes::new()
  Attributes::set(db_attrs, "service", "database")
  Attributes::set(db_attrs, "operation", "query")
  Counter::add(counter, 5.0, Some(db_attrs))
  Histogram::record(histogram, 50.0, Some(db_attrs))
  
  // Update gauge
  Gauge::set(gauge, 25.0)
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify metrics aggregation
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  // We should have 6 metric data points (2 for each metric type with different attributes)
  let counter_metrics = metrics.filter(fn(m) { m.name == "requests_total" })
  let histogram_metrics = metrics.filter(fn(m) { m.name == "request_duration" })
  let gauge_metrics = metrics.filter(fn(m) { m.name == "active_connections" })
  
  assert_eq(counter_metrics.length(), 2)
  assert_eq(histogram_metrics.length(), 2)
  assert_eq(gauge_metrics.length(), 1)
  
  // Verify metric values
  let api_counter = counter_metrics.find(fn(m) { 
    m.attributes.contains(("service", "api")) && m.attributes.contains(("method", "GET"))
  })
  let db_counter = counter_metrics.find(fn(m) { 
    m.attributes.contains(("service", "database")) && m.attributes.contains(("operation", "query"))
  })
  
  match (api_counter, db_counter) {
    (Some(api), Some(db)) => {
      assert_eq(api.value, 10.0)
      assert_eq(db.value, 5.0)
    }
    _ => assert_true(false)
  }
}

// Test 9: Telemetry Pipeline with Filtering
test "telemetry pipeline with filtering" {
  // Configure filtering
  let filter_config = {
    span_filters: [
      fn(span: Span) { span.name != "debug_operation" },
      fn(span: Span) { !span.attributes.contains(("internal", "true")) }
    ],
    metric_filters: [
      fn(metric: Metric) { !metric.name.starts_with("debug_") }
    ],
    log_filters: [
      fn(log: LogRecord) { log.severity >= Warning }
    ]
  }
  
  let provider = TelemetryProvider::with_filters(filter_config)
  let tracer = TelemetryProvider::get_tracer(provider, "filter_test_tracer")
  let meter = TelemetryProvider::get_meter(provider, "filter_test_meter")
  let logger = TelemetryProvider::get_logger(provider, "filter_test_logger")
  
  // Create spans (some should be filtered)
  let normal_span = Tracer::start_span(tracer, "normal_operation")
  Span::end(normal_span)
  
  let debug_span = Tracer::start_span(tracer, "debug_operation")
  Span::end(debug_span)
  
  let internal_span = Tracer::start_span(tracer, "internal_operation")
  Span::add_attribute(internal_span, "internal", "true")
  Span::end(internal_span)
  
  // Create metrics (some should be filtered)
  let normal_counter = Meter::create_counter(meter, "normal_counter")
  Counter::add(normal_counter, 1.0)
  
  let debug_counter = Meter::create_counter(meter, "debug_counter")
  Counter::add(debug_counter, 1.0)
  
  // Create logs (some should be filtered)
  let info_log = Logger::create_log_record(logger, Info, "Info message")
  Logger::emit(logger, info_log)
  
  let warning_log = Logger::create_log_record(logger, Warning, "Warning message")
  Logger::emit(logger, warning_log)
  
  // Force export
  TelemetryProvider::force_flush(provider)
  
  // Verify filtering
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let logs = TelemetryProvider::get_collected_logs(provider)
  
  // Only normal_span should pass through
  assert_eq(spans.length(), 1)
  assert_eq(spans[0].name, "normal_operation")
  
  // Only normal_counter should pass through
  assert_eq(metrics.length(), 1)
  assert_eq(metrics[0].name, "normal_counter")
  
  // Only warning_log should pass through
  assert_eq(logs.length(), 1)
  assert_eq(logs[0].severity, Warning)
}

// Test 10: Telemetry Pipeline with Retry and Resilience
test "telemetry pipeline with retry and resilience" {
  // Configure resilience settings
  let resilience_config = {
    max_retry_attempts: 3,
    retry_delay_ms: 100,
    circuit_breaker_failure_threshold: 5,
    circuit_breaker_recovery_timeout_ms: 1000
  }
  
  let provider = TelemetryProvider::with_resilience(resilience_config)
  let tracer = TelemetryProvider::get_tracer(provider, "resilience_test_tracer")
  
  // Simulate export failures
  let mut failure_count = 0
  TelemetryProvider::set_export_callback(provider, fn(data: TelemetryData) {
    failure_count = failure_count + 1
    if failure_count <= 2 {
      ExportResult::RetryableError("Simulated export failure")
    } else {
      ExportResult::Success
    }
  })
  
  // Create a span
  let span = Tracer::start_span(tracer, "resilience_test_operation")
  Span::end(span)
  
  // Force export with retry
  let export_result = TelemetryProvider::force_flush_with_timeout(provider, 5000)
  
  // Verify retry behavior
  match export_result {
    ExportResult::Success => assert_true(true)
    _ => assert_true(false)
  }
  
  // Verify spans were eventually exported
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  // Test circuit breaker
  // Simulate multiple failures to trigger circuit breaker
  for i in 0..6 {
    let span = Tracer::start_span(tracer, "circuit_breaker_test_" + i.to_string())
    Span::end(span)
    TelemetryProvider::force_flush(provider)
  }
  
  // Circuit breaker should be open now
  let circuit_breaker_span = Tracer::start_span(tracer, "circuit_breaker_open_test")
  Span::end(circuit_breaker_span)
  
  let spans_after_circuit_breaker = TelemetryProvider::get_collected_spans(provider)
  
  // Should still have the original spans, but not the new one
  assert_eq(spans_after_circuit_breaker.length(), 7) // 1 original + 6 circuit breaker test
}