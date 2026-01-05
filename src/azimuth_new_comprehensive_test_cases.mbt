// Azimuth Telemetry System - New Comprehensive Test Cases
// This file contains new comprehensive test cases for various telemetry functionalities

// Test 1: Advanced Attribute Value Conversions
test "advanced attribute value conversions" {
  // Test string to int conversion
  let string_val = StringValue("123")
  let converted_int = AttributeValue::to_int(string_val)
  match converted_int {
    Some(i) => assert_eq(i, 123)
    None => assert_true(false)
  }
  
  // Test string to float conversion
  let float_string = StringValue("3.14")
  let converted_float = AttributeValue::to_float(float_string)
  match converted_float {
    Some(f) => assert_eq(f, 3.14)
    None => assert_true(false)
  }
  
  // Test int to string conversion
  let int_val = IntValue(456)
  let converted_string = AttributeValue::to_string(int_val)
  match converted_string {
    Some(s) => assert_eq(s, "456")
    None => assert_true(false)
  }
  
  // Test bool to string conversion
  let bool_val = BoolValue(true)
  let bool_string = AttributeValue::to_string(bool_val)
  match bool_string {
    Some(s) => assert_eq(s, "true")
    None => assert_true(false)
  }
  
  // Test array operations
  let array_val = ArrayIntValue([1, 2, 3, 4, 5])
  let array_sum = AttributeValue::array_sum(array_val)
  match array_sum {
    Some(sum) => assert_eq(sum, 15)
    None => assert_true(false)
  }
}

// Test 2: Span Event Operations
test "span event operations" {
  let span_ctx = SpanContext::new("trace_event_test", "span_event_test", true, "")
  let span = Span::new("event_test_span", Internal, span_ctx)
  
  // Test adding events with attributes
  let event_attrs = [
    ("event.name", StringValue("user_action")),
    ("event.type", StringValue("click")),
    ("event.timestamp", IntValue(1234567890))
  ]
  
  Span::add_event(span, "user_clicked", Some(event_attrs))
  Span::add_event(span, "page_loaded", Some([("load_time", IntValue(2500))]))
  
  // Test adding event without attributes
  Span::add_event(span, "simple_event", None)
  
  // Test span status after events
  assert_eq(Span::status(span), Unset)
  
  // Set status and end span
  Span::set_status(span, Ok, Some("All events processed"))
  Span::end(span)
  assert_eq(Span::status(span), Ok)
}

// Test 3: Metrics Aggregation
test "metrics aggregation" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_test_meter")
  
  // Create counter for aggregation test
  let counter = Meter::create_counter(meter, "request_count", Some("Total requests"), Some("requests"))
  
  // Add multiple measurements
  Counter::add(counter, 1.0)
  Counter::add(counter, 2.0)
  Counter::add(counter, 3.0)
  
  // Create histogram for distribution test
  let histogram = Meter::create_histogram(meter, "response_time", Some("Response times"), Some("ms"))
  
  // Record multiple measurements
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 200.0)
  Histogram::record(histogram, 150.0)
  Histogram::record(histogram, 300.0)
  
  // Test updown counter with negative values
  let updown_counter = Meter::create_updown_counter(meter, "active_connections", Some("Active connections"), Some("connections"))
  
  UpDownCounter::add(updown_counter, 10.0)
  UpDownCounter::add(updown_counter, -2.0)
  UpDownCounter::add(updown_counter, 5.0)
  UpDownCounter::add(updown_counter, -3.0)
  
  // Test gauge with attributes
  let gauge = Meter::create_gauge(meter, "memory_usage", Some("Memory usage"), Some("bytes"))
  let memory_attrs = [
    ("memory.type", StringValue("heap")),
    ("process.id", IntValue(1234))
  ]
  Gauge::record(gauge, 1024000.0, Some(memory_attrs))
}

// Test 4: Context Propagation
test "context propagation" {
  // Create root context
  let root_ctx = Context::root()
  
  // Add baggage to context
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user123")
  let baggage_with_entries = Baggage::set_entry(baggage_with_entries, "request.id", "req456")
  
  let ctx_with_baggage = Context::with_value(root_ctx, ContextKey::new("baggage"), baggage_with_entries)
  
  // Create span context
  let span_ctx = SpanContext::new("prop_trace", "prop_span", true, "")
  let ctx_with_span = Context::with_value(ctx_with_baggage, ContextKey::new("span_context"), span_ctx)
  
  // Test context extraction
  match Context::get(ctx_with_span, ContextKey::new("span_context")) {
    Some(ctx) => {
      match ctx {
        SpanContext(sc) => {
          assert_eq(SpanContext::trace_id(sc), "prop_trace")
          assert_eq(SpanContext::span_id(sc), "prop_span")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Test baggage propagation
  match Context::get(ctx_with_span, ContextKey::new("baggage")) {
    Some(b) => {
      match b {
        Baggage(bg) => {
          let user_id = Baggage::get_entry(bg, "user.id")
          match user_id {
            Some(id) => assert_eq(id, "user123")
            None => assert_true(false)
          }
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 5: Log Record with Severity
test "log record with severity levels" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "severity_test_logger")
  
  // Test different severity levels
  let trace_log = LogRecord::new(Trace, "Trace level message")
  assert_eq(LogRecord::severity_number(trace_log), Trace)
  
  let debug_log = LogRecord::new(Debug, "Debug level message")
  assert_eq(LogRecord::severity_number(debug_log), Debug)
  
  let info_log = LogRecord::new(Info, "Info level message")
  assert_eq(LogRecord::severity_number(info_log), Info)
  
  let warn_log = LogRecord::new(Warn, "Warning level message")
  assert_eq(LogRecord::severity_number(warn_log), Warn)
  
  let error_log = LogRecord::new(Error, "Error level message")
  assert_eq(LogRecord::severity_number(error_log), Error)
  
  let fatal_log = LogRecord::new(Fatal, "Fatal level message")
  assert_eq(LogRecord::severity_number(fatal_log), Fatal)
  
  // Test log with attributes
  let log_attrs = [
    ("error.code", IntValue(500)),
    ("error.message", StringValue("Internal server error")),
    ("request.id", StringValue("req-789"))
  ]
  
  let error_log_with_attrs = LogRecord::new_with_attributes(
    Error,
    Some("Request failed"),
    Some(log_attrs)
  )
  
  Logger::emit(logger, trace_log)
  Logger::emit(logger, debug_log)
  Logger::emit(logger, info_log)
  Logger::emit(logger, warn_log)
  Logger::emit(logger, error_log)
  Logger::emit(logger, fatal_log)
  Logger::emit(logger, error_log_with_attrs)
}

// Test 6: Resource Attributes with Nested Structure
test "resource attributes with nested structure" {
  let base_resource = Resource::new()
  
  // Add service attributes
  let service_attrs = [
    ("service.name", StringValue("azimuth_telemetry")),
    ("service.version", StringValue("2.0.1")),
    ("service.namespace", StringValue("production"))
  ]
  
  let service_resource = Resource::with_attributes(base_resource, service_attrs)
  
  // Add host attributes
  let host_attrs = [
    ("host.name", StringValue("prod-server-01")),
    ("host.id", StringValue("host-abc123")),
    ("host.arch", StringValue("x86_64"))
  ]
  
  let host_resource = Resource::with_attributes(service_resource, host_attrs)
  
  // Add process attributes
  let process_attrs = [
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("azimuth")),
    ("process.executable.path", StringValue("/usr/bin/azimuth"))
  ]
  
  let full_resource = Resource::with_attributes(host_resource, process_attrs)
  
  // Test attribute retrieval
  let service_name = Resource::get_attribute(full_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth_telemetry")
    _ => assert_true(false)
  }
  
  let host_name = Resource::get_attribute(full_resource, "host.name")
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "prod-server-01")
    _ => assert_true(false)
  }
  
  let process_pid = Resource::get_attribute(full_resource, "process.pid")
  match process_pid {
    Some(IntValue(pid)) => assert_eq(pid, 12345)
    _ => assert_true(false)
  }
}

// Test 7: Span Links and Relationships
test "span links and relationships" {
  // Create parent span
  let parent_ctx = SpanContext::new("parent_trace", "parent_span", true, "")
  let parent_span = Span::new("parent_operation", Server, parent_ctx)
  
  // Create child span
  let child_ctx = SpanContext::new("parent_trace", "child_span", true, "")
  let child_span = Span::new("child_operation", Client, child_ctx)
  
  // Create linked span
  let linked_ctx = SpanContext::new("linked_trace", "linked_span", true, "")
  let linked_span = Span::new("linked_operation", Internal, linked_ctx)
  
  // Add link from child to parent
  let link_attrs = [
    ("link.type", StringValue("parent")),
    ("link.relationship", StringValue("child_of"))
  ]
  
  Span::add_link(child_span, parent_ctx, Some(link_attrs))
  
  // Add link from child to linked span
  let external_link_attrs = [
    ("link.type", StringValue("external")),
    ("link.service", StringValue("external_service"))
  ]
  
  Span::add_link(child_span, linked_ctx, Some(external_link_attrs))
  
  // Test span operations
  Span::set_status(parent_span, Ok, Some("Parent completed"))
  Span::set_status(child_span, Ok, Some("Child completed"))
  Span::set_status(linked_span, Error, Some("Linked operation failed"))
  
  Span::end(parent_span)
  Span::end(child_span)
  Span::end(linked_span)
}

// Test 8: Telemetry Pipeline Integration
test "telemetry pipeline integration" {
  // Create telemetry pipeline
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "pipeline_test_tracer")
  
  // Create span with telemetry data
  let span = Tracer::start_span(tracer, "pipeline_operation")
  
  // Add metrics to span
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "pipeline_meter")
  
  let operation_counter = Meter::create_counter(meter, "operation_count", Some("Operation count"), Some("operations"))
  Counter::add(operation_counter, 1.0)
  
  let operation_timer = Meter::create_histogram(meter, "operation_duration", Some("Operation duration"), Some("ms"))
  Histogram::record(operation_timer, 150.0)
  
  // Add events to span
  Span::add_event(span, "operation_started", Some([("timestamp", IntValue(1234567890))]))
  Span::add_event(span, "operation_completed", Some([("duration", IntValue(150))]))
  
  // Add logs to span
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "pipeline_logger")
  
  let start_log = LogRecord::new(Info, "Operation started successfully")
  let completion_log = LogRecord::new(Info, "Operation completed successfully")
  
  Logger::emit(logger, start_log)
  Logger::emit(logger, completion_log)
  
  // End span
  Span::end(span)
}

// Test 9: Error Handling and Recovery
test "error handling and recovery" {
  let span_ctx = SpanContext::new("error_trace", "error_span", true, "")
  let span = Span::new("error_operation", Internal, span_ctx)
  
  // Test error recording
  let error_attrs = [
    ("error.type", StringValue("TimeoutError")),
    ("error.message", StringValue("Operation timed out after 30 seconds")),
    ("error.retry_count", IntValue(3))
  ]
  
  Span::add_event(span, "error_occurred", Some(error_attrs))
  Span::set_status(span, Error, Some("Operation failed due to timeout"))
  
  // Test recovery span
  let recovery_ctx = SpanContext::new("error_trace", "recovery_span", true, "")
  let recovery_span = Span::new("recovery_operation", Internal, recovery_ctx)
  
  // Add recovery link
  let recovery_link_attrs = [
    ("recovery.from", StringValue("error_span")),
    ("recovery.strategy", StringValue("retry_with_backoff"))
  ]
  
  Span::add_link(recovery_span, span_ctx, Some(recovery_link_attrs))
  
  // Record recovery events
  Span::add_event(recovery_span, "recovery_started", Some([("strategy", StringValue("exponential_backoff"))]))
  Span::add_event(recovery_span, "recovery_completed", Some([("success", BoolValue(true))]))
  
  Span::set_status(recovery_span, Ok, Some("Recovery completed successfully"))
  
  Span::end(span)
  Span::end(recovery_span)
}

// Test 10: Performance Benchmarking
test "performance benchmarking" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "benchmark_meter")
  
  // Create performance metrics
  let throughput_counter = Meter::create_counter(meter, "operations_per_second", Some("Operations per second"), Some("ops"))
  let latency_histogram = Meter::create_histogram(meter, "operation_latency", Some("Operation latency"), Some("ms"))
  let memory_gauge = Meter::create_gauge(meter, "memory_usage", Some("Memory usage"), Some("bytes"))
  
  // Simulate performance measurements
  let operations = [100, 150, 200, 120, 180, 90, 210, 160, 140, 170]
  let latencies = [10.5, 15.2, 8.7, 12.3, 9.8, 16.1, 7.9, 11.4, 13.6, 10.2]
  let memory_usage = [1024000, 1125000, 980000, 1200000, 1050000, 1150000, 950000, 1100000, 1080000, 1180000]
  
  // Record throughput measurements
  for i in 0..operations.length() {
    Counter::add(throughput_counter, operations[i].to_float())
  }
  
  // Record latency measurements
  for i in 0..latencies.length() {
    Histogram::record(latency_histogram, latencies[i])
  }
  
  // Record memory usage measurements
  for i in 0..memory_usage.length() {
    Gauge::record(memory_gauge, memory_usage[i].to_float())
  }
  
  // Calculate and record average metrics
  let mut total_ops = 0.0
  for i in 0..operations.length() {
    total_ops = total_ops + operations[i].to_float()
  }
  let avg_ops = total_ops / operations.length().to_float()
  
  let avg_counter = Meter::create_counter(meter, "average_operations", Some("Average operations"), Some("ops"))
  Counter::add(avg_counter, avg_ops)
}