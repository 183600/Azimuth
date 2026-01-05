// Azimuth Enhanced Telemetry System Tests
// This file contains enhanced test cases for telemetry system functionality

// Test 1: Telemetry Span Creation and Lifecycle
test "telemetry span creation and lifecycle management" {
  let tracer = Tracer::new("azimuth-test")
  let span = Tracer::start_span(tracer, "test-operation")
  
  // Verify span creation
  assert_true(Span::is_recording(span))
  assert_eq(Span::name(span), "test-operation")
  assert_true(Span::start_time(span) > 0)
  
  // Add attributes to span
  Span::set_attribute(span, "service.name", "payment-service")
  Span::set_attribute(span, "service.version", "1.2.3")
  Span::set_attribute(span, "environment", "production")
  
  // Verify attributes
  let service_name = Span::get_attribute(span, "service.name")
  let service_version = Span::get_attribute(span, "service.version")
  let environment = Span::get_attribute(span, "environment")
  
  assert_eq(service_name, Some("payment-service"))
  assert_eq(service_version, Some("1.2.3"))
  assert_eq(environment, Some("production"))
  
  // Add events to span
  Span::add_event(span, "operation.started")
  Span::add_event(span, "database.query.executed")
  Span::add_event(span, "operation.completed")
  
  // Get events
  let events = Span::get_events(span)
  assert_eq(events.length(), 3)
  assert_eq(events[0].name, "operation.started")
  assert_eq(events[1].name, "database.query.executed")
  assert_eq(events[2].name, "operation.completed")
  
  // Set span status
  Span::set_status(span, Ok)
  assert_eq(Span::status(span), Ok)
  
  // End span
  Span::end(span)
  assert_false(Span::is_recording(span))
  assert_true(Span::end_time(span) > 0)
}

// Test 2: Telemetry Metrics Collection and Aggregation
test "telemetry metrics collection and aggregation" {
  let meter = Meter::new("azimuth-test")
  let counter = Meter::create_counter(meter, "requests.total")
  let histogram = Meter::create_histogram(meter, "request.duration")
  let gauge = Meter::create_gauge(meter, "memory.usage")
  
  // Record counter metrics
  Counter::add(counter, 1.0, [("method", "GET"), ("status", "200")])
  Counter::add(counter, 1.0, [("method", "GET"), ("status", "200")])
  Counter::add(counter, 1.0, [("method", "POST"), ("status", "201")])
  Counter::add(counter, 1.0, [("method", "GET"), ("status", "404")])
  
  // Record histogram metrics
  Histogram::record(histogram, 120.5, [("endpoint", "/api/users")])
  Histogram::record(histogram, 85.3, [("endpoint", "/api/users")])
  Histogram::record(histogram, 250.7, [("endpoint", "/api/orders")])
  Histogram::record(histogram, 95.2, [("endpoint", "/api/users")])
  
  // Record gauge metrics
  Gauge::set(gauge, 512.0, [("instance", "server-1")])
  Gauge::set(gauge, 768.0, [("instance", "server-1")])
  Gauge::set(gauge, 256.0, [("instance", "server-2")])
  
  // Get counter metrics
  let total_requests = Counter::get_value(counter)
  assert_eq(total_requests, 4.0)
  
  let get_requests = Counter::get_value_with_attributes(counter, [("method", "GET")])
  assert_eq(get_requests, 3.0)
  
  // Get histogram metrics
  let users_endpoint_stats = Histogram::get_stats(histogram, [("endpoint", "/api/users")])
  assert_eq(users_endpoint_stats.count, 3)
  assert_true(users_endpoint_stats.sum > 300.0 && users_endpoint_stats.sum < 301.0)
  assert_true(users_endpoint_stats.min > 85.0 && users_endpoint_stats.min < 86.0)
  assert_true(users_endpoint_stats.max > 120.0 && users_endpoint_stats.max < 121.0)
  
  // Get gauge metrics
  let server1_memory = Gauge::get_value(gauge, [("instance", "server-1")])
  assert_eq(server1_memory, 768.0)
  
  let server2_memory = Gauge::get_value(gauge, [("instance", "server-2")])
  assert_eq(server2_memory, 256.0)
}

// Test 3: Telemetry Context Propagation
test "telemetry context propagation across service boundaries" {
  let context = Context::root()
  let trace_id_key = ContextKey::new("trace_id")
  let span_id_key = ContextKey::new("span_id")
  let baggage_key = ContextKey::new("baggage")
  
  // Create initial context with trace information
  let trace_id = "trace-1234567890abcdef"
  let span_id = "span-1234567890abcdef"
  
  let context_with_trace = Context::with_value(context, trace_id_key, trace_id)
  let context_with_span = Context::with_value(context_with_trace, span_id_key, span_id)
  
  // Add baggage to context
  let baggage = [("user.id", "user-123"), ("session.id", "session-456")]
  let context_with_baggage = Context::with_value(context_with_span, baggage_key, baggage)
  
  // Verify context values
  let retrieved_trace_id = Context::get(context_with_baggage, trace_id_key)
  let retrieved_span_id = Context::get(context_with_baggage, span_id_key)
  let retrieved_baggage = Context::get(context_with_baggage, baggage_key)
  
  assert_eq(retrieved_trace_id, Some(trace_id))
  assert_eq(retrieved_span_id, Some(span_id))
  assert_eq(retrieved_baggage, Some(baggage))
  
  // Simulate context propagation to downstream service
  let carrier = TextMapCarrier::new()
  let propagator = TraceContextPropagator::new()
  
  // Inject context into carrier
  TraceContextPropagator::inject(propagator, context_with_baggage, carrier)
  
  // Extract context from carrier in downstream service
  let extracted_context = TraceContextPropagator::extract(propagator, carrier)
  
  // Verify extracted context
  let extracted_trace_id = Context::get(extracted_context, trace_id_key)
  let extracted_span_id = Context::get(extracted_context, span_id_key)
  let extracted_baggage = Context::get(extracted_context, baggage_key)
  
  assert_eq(extracted_trace_id, Some(trace_id))
  assert_eq(extracted_span_id, Some(span_id))
  assert_eq(extracted_baggage, Some(baggage))
}

// Test 4: Telemetry Data Sampling Strategies
test "telemetry data sampling strategies" {
  let sampler_config = SamplerConfig::new()
  
  // Test always-on sampler
  let always_on_sampler = Sampler::always_on()
  let decision1 = Sampler::should_sample(always_on_sampler, "trace-123", "span-123", "test-span")
  assert_eq(decision1, Sample)
  
  // Test always-off sampler
  let always_off_sampler = Sampler::always_off()
  let decision2 = Sampler::should_sample(always_off_sampler, "trace-456", "span-456", "test-span")
  assert_eq(decision2, Drop)
  
  // Test ratio-based sampler (50%)
  let ratio_sampler = Sampler::ratio(0.5)
  let sampled_count = { mut count: 0 }
  let dropped_count = { mut count: 0 }
  
  // Simulate 100 sampling decisions
  for i in 0..100 {
    let trace_id = "trace-" + i.to_string()
    let span_id = "span-" + i.to_string()
    let decision = Sampler::should_sample(ratio_sampler, trace_id, span_id, "test-span")
    
    match decision {
      Sample => sampled_count.count = sampled_count.count + 1
      Drop => dropped_count.count = dropped_count.count + 1
    }
  }
  
  // Should be close to 50% (allowing for some variance)
  assert_true(sampled_count.count > 40 && sampled_count.count < 60)
  assert_eq(sampled_count.count + dropped_count.count, 100)
  
  // Test parent-based sampler
  let parent_sampler = Sampler::parent_based(always_on_sampler, always_off_sampler)
  
  // With parent context (should follow parent)
  let parent_context = Context::with_sampling_decision(Context::root(), Drop)
  let decision3 = Sampler::should_sample_with_parent(parent_sampler, "trace-789", "span-789", "test-span", parent_context)
  assert_eq(decision3, Drop)
  
  // Without parent context (should use root sampler)
  let no_parent_context = Context::root()
  let decision4 = Sampler::should_sample_with_parent(parent_sampler, "trace-999", "span-999", "test-span", no_parent_context)
  assert_eq(decision4, Sample)
}

// Test 5: Telemetry Resource and Attributes Management
test "telemetry resource and attributes management" {
  // Create resource with default attributes
  let resource = Resource::default()
  
  // Verify default resource attributes
  let service_name = Resource::get_attribute(resource, "service.name")
  let service_version = Resource::get_attribute(resource, "service.version")
  let telemetry_sdk_name = Resource::get_attribute(resource, "telemetry.sdk.name")
  let telemetry_sdk_version = Resource::get_attribute(resource, "telemetry.sdk.version")
  
  assert_eq(service_name, Some("azimuth"))
  assert_eq(service_version, Some("1.0.0"))
  assert_eq(telemetry_sdk_name, Some("azimuth-sdk"))
  assert_eq(telemetry_sdk_version, Some("1.0.0"))
  
  // Create custom resource with additional attributes
  let custom_attributes = [
    ("deployment.environment", "production"),
    ("host.name", "server-prod-01"),
    ("host.ip", "10.0.1.100"),
    ("kubernetes.pod.name", "azimuth-service-7f8d9c2e"),
    ("kubernetes.namespace", "default")
  ]
  
  let custom_resource = Resource::with_attributes(resource, custom_attributes)
  
  // Verify custom attributes
  let deployment_env = Resource::get_attribute(custom_resource, "deployment.environment")
  let host_name = Resource::get_attribute(custom_resource, "host.name")
  let host_ip = Resource::get_attribute(custom_resource, "host.ip")
  let pod_name = Resource::get_attribute(custom_resource, "kubernetes.pod.name")
  let namespace = Resource::get_attribute(custom_resource, "kubernetes.namespace")
  
  assert_eq(deployment_env, Some("production"))
  assert_eq(host_name, Some("server-prod-01"))
  assert_eq(host_ip, Some("10.0.1.100"))
  assert_eq(pod_name, Some("azimuth-service-7f8d9c2e"))
  assert_eq(namespace, Some("default"))
  
  // Verify default attributes are still present
  let custom_service_name = Resource::get_attribute(custom_resource, "service.name")
  assert_eq(custom_service_name, Some("azimuth"))
  
  // Test resource merging
  let additional_attributes = [
    ("region", "us-west-2"),
    ("availability.zone", "us-west-2a"),
    ("deployment.environment", "staging")  // This should override the previous value
  ]
  
  let additional_resource = Resource::with_attributes(Resource::empty(), additional_attributes)
  let merged_resource = Resource::merge(custom_resource, additional_resource)
  
  // Verify merged attributes
  let merged_region = Resource::get_attribute(merged_resource, "region")
  let merged_zone = Resource::get_attribute(merged_resource, "availability.zone")
  let merged_env = Resource::get_attribute(merged_resource, "deployment.environment")
  let merged_host_name = Resource::get_attribute(merged_resource, "host.name")
  
  assert_eq(merged_region, Some("us-west-2"))
  assert_eq(merged_zone, Some("us-west-2a"))
  assert_eq(merged_env, Some("staging"))  // Should be overridden
  assert_eq(merged_host_name, Some("server-prod-01"))  // Should be preserved
}

// Test 6: Telemetry Log Correlation with Traces
test "telemetry log correlation with traces" {
  let logger = Logger::new("azimuth-test")
  let tracer = Tracer::new("azimuth-test")
  
  // Start a span
  let span = Tracer::start_span(tracer, "user-registration")
  let span_context = Span::get_context(span)
  
  // Create log record with span context
  let log_record = LogRecord::new_with_span_context(
    Info,
    "User registration started",
    span_context,
    Some([("user.id", "user-123"), ("email", "user@example.com")])
  )
  
  // Emit log record
  Logger::emit(logger, log_record)
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("User registration started"))
  assert_eq(LogRecord::trace_id(log_record), Some(SpanContext::trace_id(span_context)))
  assert_eq(LogRecord::span_id(log_record), Some(SpanContext::span_id(span_context)))
  
  // Add event to span
  Span::add_event_with_attributes(span, "validation.started", [("step", "email-validation")])
  
  // Create another log record
  let validation_log = LogRecord::new_with_span_context(
    Debug,
    "Validating user email",
    span_context,
    Some([("validation.type", "email"), ("validation.result", "pending")])
  )
  
  Logger::emit(logger, validation_log)
  
  // Add another event to span
  Span::add_event_with_attributes(span, "validation.completed", [("step", "email-validation"), ("result", "success")])
  
  // Create final log record
  let completion_log = LogRecord::new_with_span_context(
    Info,
    "User registration completed successfully",
    span_context,
    Some([("user.id", "user-123"), ("registration.status", "success")])
  )
  
  Logger::emit(logger, completion_log)
  
  // End the span
  Span::set_status(span, Ok)
  Span::end(span)
  
  // Verify all log records are correlated with the same trace
  assert_eq(LogRecord::trace_id(log_record), LogRecord::trace_id(validation_log))
  assert_eq(LogRecord::trace_id(validation_log), LogRecord::trace_id(completion_log))
}

// Test 7: Telemetry Data Export and Serialization
test "telemetry data export and serialization" {
  let exporter = Exporter::new("json")
  let processor = BatchSpanProcessor::new(exporter, 1000, 5000)  // max_queue_size=1000, schedule_delay=5000ms
  
  // Create spans
  let tracer = Tracer::new("azimuth-test")
  let span1 = Tracer::start_span(tracer, "operation-1")
  let span2 = Tracer::start_span(tracer, "operation-2")
  
  // Add attributes and events to spans
  Span::set_attribute(span1, "service.name", "auth-service")
  Span::set_attribute(span1, "operation.type", "login")
  Span::add_event(span1, "authentication.started")
  
  Span::set_attribute(span2, "service.name", "user-service")
  Span::set_attribute(span2, "operation.type", "profile-update")
  Span::add_event(span2, "validation.started")
  
  // End spans
  Span::end(span1)
  Span::end(span2)
  
  // Force export
  BatchSpanProcessor::force_flush(processor)
  
  // Verify exported data
  let exported_spans = Exporter::get_exported_spans(exporter)
  assert_eq(exported_spans.length(), 2)
  
  // Verify first span
  let exported_span1 = exported_spans[0]
  assert_eq(exported_span1.name, "operation-1")
  assert_eq(exported_span1.attributes["service.name"], "auth-service")
  assert_eq(exported_span1.attributes["operation.type"], "login")
  assert_eq(exported_span1.events.length(), 1)
  assert_eq(exported_span1.events[0].name, "authentication.started")
  
  // Verify second span
  let exported_span2 = exported_spans[1]
  assert_eq(exported_span2.name, "operation-2")
  assert_eq(exported_span2.attributes["service.name"], "user-service")
  assert_eq(exported_span2.attributes["operation.type"], "profile-update")
  assert_eq(exported_span2.events.length(), 1)
  assert_eq(exported_span2.events[0].name, "validation.started")
  
  // Test serialization to JSON
  let json_data = Exporter::serialize_to_json(exporter, exported_spans)
  assert_true(json_data.contains("\"name\":\"operation-1\""))
  assert_true(json_data.contains("\"name\":\"operation-2\""))
  assert_true(json_data.contains("\"service.name\":\"auth-service\""))
  assert_true(json_data.contains("\"service.name\":\"user-service\""))
  assert_true(json_data.contains("\"authentication.started\""))
  assert_true(json_data.contains("\"validation.started\""))
}

// Test 8: Telemetry Performance and Memory Optimization
test "telemetry performance and memory optimization" {
  let performance_monitor = PerformanceMonitor::new()
  
  // Test span creation performance
  let span_creation_start = PerformanceMonitor::current_time(performance_monitor)
  
  let tracer = Tracer::new("performance-test")
  let spans = []
  
  // Create 1000 spans
  for i in 0..1000 {
    let span_name = "span-" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    spans = spans.push(span)
  }
  
  let span_creation_end = PerformanceMonitor::current_time(performance_monitor)
  let span_creation_duration = span_creation_end - span_creation_start
  
  // Verify span creation performance (should be reasonably fast)
  assert_true(span_creation_duration < 100000000)  // Less than 100ms
  
  // Test attribute setting performance
  let attribute_setting_start = PerformanceMonitor::current_time(performance_monitor)
  
  // Set attributes on all spans
  for i in 0..1000 {
    let span = spans[i]
    Span::set_attribute(span, "index", i.to_string())
    Span::set_attribute(span, "batch", "performance-test")
  }
  
  let attribute_setting_end = PerformanceMonitor::current_time(performance_monitor)
  let attribute_setting_duration = attribute_setting_end - attribute_setting_start
  
  // Verify attribute setting performance
  assert_true(attribute_setting_duration < 50000000)  // Less than 50ms
  
  // Test memory usage
  let memory_before = PerformanceMonitor::memory_usage(performance_monitor)
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
  
  // Force garbage collection if available
  PerformanceMonitor::force_gc(performance_monitor)
  
  let memory_after = PerformanceMonitor::memory_usage(performance_monitor)
  let memory_diff = memory_after - memory_before
  
  // Memory usage should not increase excessively
  assert_true(memory_diff < 1000000)  // Less than 1MB increase
  
  // Test batch processing performance
  let batch_processing_start = PerformanceMonitor::current_time(performance_monitor)
  
  let exporter = Exporter::new("json")
  let processor = BatchSpanProcessor::new(exporter, 2000, 1000)  // max_queue_size=2000, schedule_delay=1000ms
  
  // Create and end 2000 spans rapidly
  for i in 0..2000 {
    let span = Tracer::start_span(tracer, "batch-span-" + i.to_string())
    Span::set_attribute(span, "batch.index", i.to_string())
    Span::end(span)
  }
  
  // Force export
  BatchSpanProcessor::force_flush(processor)
  
  let batch_processing_end = PerformanceMonitor::current_time(performance_monitor)
  let batch_processing_duration = batch_processing_end - batch_processing_start
  
  // Verify batch processing performance
  assert_true(batch_processing_duration < 200000000)  // Less than 200ms
  
  // Verify all spans were exported
  let exported_spans = Exporter::get_exported_spans(exporter)
  assert_eq(exported_spans.length(), 2000)
}

// Test 9: Telemetry Error Handling and Resilience
test "telemetry error handling and resilience" {
  let error_handler = ErrorHandler::new()
  
  // Test span error handling
  let tracer = Tracer::new("error-test")
  
  // Create a span and set error status
  let span = Tracer::start_span(tracer, "error-prone-operation")
  
  try {
    // Simulate an error condition
    Span::set_attribute(span, "operation.type", "database-query")
    Span::add_event(span, "query.started")
    
    // Simulate an error
    let error = Error::new("database.connection.failed", "Failed to connect to database")
    Span::set_status(span, Error, Some(error.message))
    Span::add_event_with_attributes(span, "error.occurred", [("error.type", error.code), ("error.message", error.message)])
    
    // Record the error
    ErrorHandler::record_error(error_handler, error)
  } catch {
    e => {
      Span::set_status(span, Error, Some("Unexpected error: " + e.message))
      Span::add_event_with_attributes(span, "exception.occurred", [("exception.type", e.type), ("exception.message", e.message)])
    }
  }
  
  Span::end(span)
  
  // Verify error was recorded
  let errors = ErrorHandler::get_errors(error_handler)
  assert_eq(errors.length(), 1)
  assert_eq(errors[0].code, "database.connection.failed")
  assert_eq(errors[0].message, "Failed to connect to database")
  
  // Test exporter error handling
  let failing_exporter = FailingExporter::new("simulated export failure")
  let processor = BatchSpanProcessor::new(failing_exporter, 100, 1000)
  
  // Create and end spans
  for i in 0..10 {
    let span = Tracer::start_span(tracer, "span-" + i.to_string())
    Span::end(span)
  }
  
  // Force export (should fail)
  let flush_result = BatchSpanProcessor::force_flush(processor)
  assert_eq(flush_result, Failure)
  
  // Verify error was handled
  let exporter_errors = FailingExporter::get_errors(failing_exporter)
  assert_true(exporter_errors.length() > 0)
  
  // Test recovery with fallback exporter
  let fallback_exporter = Exporter::new("json")
  let resilient_processor = ResilientBatchSpanProcessor::new(
    failing_exporter,
    fallback_exporter,
    3,  // max_retries
    100  // retry_delay
  )
  
  // Create and end spans
  for i in 0..5 {
    let span = Tracer::start_span(tracer, "resilient-span-" + i.to_string())
    Span::end(span)
  }
  
  // Force export (should succeed after retries and fallback)
  let resilient_flush_result = ResilientBatchSpanProcessor::force_flush(resilient_processor)
  assert_eq(resilient_flush_result, Success)
  
  // Verify spans were exported to fallback
  let fallback_exported_spans = Exporter::get_exported_spans(fallback_exporter)
  assert_eq(fallback_exported_spans.length(), 5)
}

// Test 10: Telemetry Configuration and Dynamic Updates
test "telemetry configuration and dynamic updates" {
  let config_manager = ConfigManager::new()
  
  // Set initial configuration
  let initial_config = {
    sampling: {
      strategy: "ratio",
      ratio: 0.1
    },
    batch_span_processor: {
      max_queue_size: 1000,
      schedule_delay: 5000,
      max_export_batch_size: 100
    },
    exporter: {
      type: "json",
      endpoint: "http://localhost:4318/v1/traces"
    },
    resource: {
      "service.name": "azimuth-service",
      "service.version": "1.0.0",
      "deployment.environment": "production"
    }
  }
  
  ConfigManager::update_config(config_manager, initial_config)
  
  // Verify initial configuration
  let current_config = ConfigManager::get_config(config_manager)
  assert_eq(current_config.sampling.strategy, "ratio")
  assert_eq(current_config.sampling.ratio, 0.1)
  assert_eq(current_config.batch_span_processor.max_queue_size, 1000)
  assert_eq(current_config.exporter.type, "json")
  assert_eq(current_config.resource["service.name"], "azimuth-service")
  
  // Create components with initial configuration
  let tracer = Tracer::from_config(config_manager)
  let exporter = Exporter::from_config(config_manager)
  let processor = BatchSpanProcessor::from_config(config_manager)
  
  // Verify components were created with initial configuration
  assert_eq(Tracer::get_sampling_ratio(tracer), 0.1)
  assert_eq(Exporter::get_type(exporter), "json")
  assert_eq(BatchSpanProcessor::get_max_queue_size(processor), 1000)
  
  // Update configuration dynamically
  let updated_config = {
    sampling: {
      strategy: "always_on",
      ratio: 1.0
    },
    batch_span_processor: {
      max_queue_size: 2000,
      schedule_delay: 3000,
      max_export_batch_size: 200
    },
    exporter: {
      type: "otlp",
      endpoint: "http://otel-collector:4317"
    },
    resource: {
      "service.name": "azimuth-service",
      "service.version": "1.1.0",  // Updated version
      "deployment.environment": "staging"  // Updated environment
    }
  }
  
  ConfigManager::update_config(config_manager, updated_config)
  
  // Verify configuration was updated
  let new_config = ConfigManager::get_config(config_manager)
  assert_eq(new_config.sampling.strategy, "always_on")
  assert_eq(new_config.sampling.ratio, 1.0)
  assert_eq(new_config.batch_span_processor.max_queue_size, 2000)
  assert_eq(new_config.exporter.type, "otlp")
  assert_eq(new_config.resource["service.version"], "1.1.0")
  assert_eq(new_config.resource["deployment.environment"], "staging")
  
  // Verify components were updated with new configuration
  assert_eq(Tracer::get_sampling_strategy(tracer), "always_on")
  assert_eq(Exporter::get_type(exporter), "otlp")
  assert_eq(BatchSpanProcessor::get_max_queue_size(processor), 2000)
  
  // Test configuration validation
  let invalid_config = {
    sampling: {
      strategy: "invalid_strategy",
      ratio: 1.5  // Invalid ratio (> 1.0)
    },
    batch_span_processor: {
      max_queue_size: -100,  // Invalid negative value
      schedule_delay: 3000,
      max_export_batch_size: 200
    },
    exporter: {
      type: "unknown_type",  // Invalid exporter type
      endpoint: "invalid-url"  // Invalid URL
    },
    resource: {
      "service.name": "",  // Empty service name
      "service.version": "1.1.0",
      "deployment.environment": "staging"
    }
  }
  
  let validation_result = ConfigManager::validate_config(config_manager, invalid_config)
  assert_false(validation_result.is_valid)
  
  // Verify validation errors
  let errors = validation_result.errors
  assert_true(errors.contains("Invalid sampling strategy: invalid_strategy"))
  assert_true(errors.contains("Sampling ratio must be between 0.0 and 1.0"))
  assert_true(errors.contains("Max queue size must be positive"))
  assert_true(errors.contains("Unknown exporter type: unknown_type"))
  assert_true(errors.contains("Invalid endpoint URL: invalid-url"))
  assert_true(errors.contains("Service name cannot be empty"))
  
  // Test configuration hot reload
  let config_file = "/tmp/azimuth-config.json"
  ConfigManager::save_config_to_file(config_manager, config_file)
  
  // Modify config file externally (simulated)
  let modified_config = {
    sampling: {
      strategy: "parent_based",
      ratio: 0.5
    },
    batch_span_processor: {
      max_queue_size: 1500,
      schedule_delay: 4000,
      max_export_batch_size: 150
    },
    exporter: {
      type: "json",
      endpoint: "http://localhost:4318/v1/traces"
    },
    resource: {
      "service.name": "azimuth-service",
      "service.version": "1.2.0",
      "deployment.environment": "development"
    }
  }
  
  ConfigManager::save_config_to_file(config_manager, modified_config, config_file)
  
  // Reload configuration from file
  let reload_result = ConfigManager::reload_from_file(config_manager, config_file)
  assert_true(reload_result)
  
  // Verify configuration was reloaded
  let reloaded_config = ConfigManager::get_config(config_manager)
  assert_eq(reloaded_config.sampling.strategy, "parent_based")
  assert_eq(reloaded_config.sampling.ratio, 0.5)
  assert_eq(reloaded_config.resource["service.version"], "1.2.0")
  assert_eq(reloaded_config.resource["deployment.environment"], "development")
}