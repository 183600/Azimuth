// Azimuth Telemetry System - Premium Async Context Propagation Tests
// This file contains high-quality test cases for context propagation across async operations

// Test 1: Basic Async Context Propagation
test "basic async context propagation" {
  let root_ctx = Context::root()
  
  // Set up initial context
  let initial_key = ContextKey::new("operation_id")
  let user_key = ContextKey::new("user_id")
  let trace_key = ContextKey::new("trace_id")
  
  let ctx_with_operation = Context::with_value(root_ctx, initial_key, "op_12345")
  let ctx_with_user = Context::with_value(ctx_with_operation, user_key, "user_67890")
  let ctx_with_trace = Context::with_value(ctx_with_user, trace_key, "trace_abcde")
  
  // Simulate async operation 1
  let async1_key = ContextKey::new("async1_data")
  let ctx_after_async1 = Context::with_value(ctx_with_trace, async1_key, "async1_result")
  
  // Verify context is preserved after async operation 1
  match Context::get(ctx_after_async1, initial_key) {
    Some(value) => assert_eq(value, "op_12345")
    None => assert_true(false, "Initial operation ID should be preserved")
  }
  
  match Context::get(ctx_after_async1, user_key) {
    Some(value) => assert_eq(value, "user_67890")
    None => assert_true(false, "User ID should be preserved")
  }
  
  match Context::get(ctx_after_async1, trace_key) {
    Some(value) => assert_eq(value, "trace_abcde")
    None => assert_true(false, "Trace ID should be preserved")
  }
  
  match Context::get(ctx_after_async1, async1_key) {
    Some(value) => assert_eq(value, "async1_result")
    None => assert_true(false, "Async1 data should be present")
  }
  
  // Simulate async operation 2
  let async2_key = ContextKey::new("async2_data")
  let ctx_after_async2 = Context::with_value(ctx_after_async1, async2_key, "async2_result")
  
  // Verify all context is preserved through multiple async operations
  match Context::get(ctx_after_async2, initial_key) {
    Some(value) => assert_eq(value, "op_12345")
    None => assert_true(false, "Initial operation ID should be preserved through multiple async ops")
  }
  
  match Context::get(ctx_after_async2, async1_key) {
    Some(value) => assert_eq(value, "async1_result")
    None => assert_true(false, "Async1 data should be preserved through async2")
  }
  
  match Context::get(ctx_after_async2, async2_key) {
    Some(value) => assert_eq(value, "async2_result")
    None => assert_true(false, "Async2 data should be present")
  }
}

// Test 2: Span Context in Async Operations
test "span context in async operations" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "async_tracer")
  
  // Create parent span
  let parent_trace_id = "async_parent_trace_12345"
  let parent_span_id = "async_parent_span_67890"
  let parent_span_ctx = SpanContext::new(parent_trace_id, parent_span_id, true, "parent_state")
  let parent_span = Span::new("async_parent_span", Server, parent_span_ctx)
  
  // Add parent span events
  Span::add_event(parent_span, "parent_start", Some([
    ("operation", StringValue("async_workflow")),
    ("component", StringValue("test_framework"))
  ]))
  
  // Simulate async child operation 1
  let child1_trace_id = SpanContext::trace_id(parent_span_ctx)
  let child1_span_id = "async_child1_span_11111"
  let child1_span_ctx = SpanContext::new(child1_trace_id, child1_span_id, true, "child1_state")
  let child1_span = Span::new("async_child1_span", Internal, child1_span_ctx)
  
  Span::add_event(child1_span, "child1_start", Some([
    ("parent_span_id", StringValue(parent_span_id)),
    ("operation_type", StringValue("async_child"))
  ]))
  
  // Verify trace context propagation
  assert_eq(SpanContext::trace_id(child1_span_ctx), parent_trace_id)
  assert_true(SpanContext::is_sampled(child1_span_ctx))
  
  // Simulate async child operation 2
  let child2_trace_id = SpanContext::trace_id(parent_span_ctx)
  let child2_span_id = "async_child2_span_22222"
  let child2_span_ctx = SpanContext::new(child2_trace_id, child2_span_id, true, "child2_state")
  let child2_span = Span::new("async_child2_span", Client, child2_span_ctx)
  
  Span::add_event(child2_span, "child2_start", Some([
    ("parent_span_id", StringValue(parent_span_id)),
    ("sibling_span_id", StringValue(child1_span_id)),
    ("operation_type", StringValue("async_child"))
  ]))
  
  // Verify trace consistency across async operations
  assert_eq(SpanContext::trace_id(child2_span_ctx), parent_trace_id)
  assert_eq(SpanContext::trace_id(child1_span_ctx), SpanContext::trace_id(child2_span_ctx))
  
  // End child spans
  Span::set_status(child1_span, Ok, Some("Child1 async operation completed"))
  Span::end(child1_span)
  
  Span::set_status(child2_span, Ok, Some("Child2 async operation completed"))
  Span::end(child2_span)
  
  // End parent span
  Span::add_event(parent_span, "parent_end", Some([
    ("child_count", StringValue("2")),
    ("status", StringValue("all_children_completed"))
  ]))
  Span::end(parent_span)
  
  // Verify span states
  assert_false(Span::is_recording(child1_span))
  assert_false(Span::is_recording(child2_span))
  assert_false(Span::is_recording(parent_span))
}

// Test 3: Async Metrics with Context
test "async metrics with context" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "async_metrics_meter")
  
  // Create metrics for async operations
  let async_counter = Meter::create_counter(meter, "async_operations", Some("Async operation counter"), Some("operations"))
  let async_histogram = Meter::create_histogram(meter, "async_duration", Some("Async operation duration"), Some("ms"))
  let async_updown = Meter::create_updown_counter(meter, "async_active", Some("Active async operations"), Some("operations"))
  
  // Simulate async operation with metrics
  let operation_attrs = Attributes::new()
  Attributes::set(operation_attrs, "operation_type", StringValue("async_workflow"))
  Attributes::set(operation_attrs, "component", StringValue("test_framework"))
  Attributes::set(operation_attrs, "trace_id", StringValue("async_trace_12345"))
  
  // Increment active operations
  UpDownCounter::add(async_updown, 1.0, Some(operation_attrs))
  
  // Record operation start
  Counter::add(async_counter, 1.0, Some(operation_attrs))
  
  // Simulate async work with duration
  let duration_attrs = Attributes::new()
  Attributes::set(duration_attrs, "operation_type", StringValue("async_workflow"))
  Attributes::set(duration_attrs, "phase", StringValue("execution"))
  
  Histogram::record(async_histogram, 150.5, Some(duration_attrs))
  
  // Simulate nested async operation
  let nested_attrs = Attributes::new()
  Attributes::set(nested_attrs, "operation_type", StringValue("nested_async"))
  Attributes::set(nested_attrs, "parent_operation", StringValue("async_workflow"))
  Attributes::set(nested_attrs, "trace_id", StringValue("async_trace_12345"))
  
  UpDownCounter::add(async_updown, 1.0, Some(nested_attrs))
  Counter::add(async_counter, 1.0, Some(nested_attrs))
  
  let nested_duration_attrs = Attributes::new()
  Attributes::set(nested_duration_attrs, "operation_type", StringValue("nested_async"))
  Attributes::set(nested_duration_attrs, "phase", StringValue("execution"))
  
  Histogram::record(async_histogram, 75.2, Some(nested_duration_attrs))
  
  // Complete nested operation
  UpDownCounter::add(async_updown, -1.0, Some(nested_attrs))
  
  // Complete main operation
  let completion_attrs = Attributes::new()
  Attributes::set(completion_attrs, "operation_type", StringValue("async_workflow"))
  Attributes::set(completion_attrs, "phase", StringValue("completion"))
  
  Histogram::record(async_histogram, 25.3, Some(completion_attrs))
  UpDownCounter::add(async_updown, -1.0, Some(operation_attrs))
  
  // Verify instrument integrity
  let counter_instrument = Counter::as_instrument(async_counter)
  assert_eq(Instrument::name(counter_instrument), "async_operations")
  
  let histogram_instrument = Histogram::as_instrument(async_histogram)
  assert_eq(Instrument::name(histogram_instrument), "async_duration")
  
  let updown_instrument = UpDownCounter::as_instrument(async_updown)
  assert_eq(Instrument::name(updown_instrument), "async_active")
}

// Test 4: Async Logging with Context
test "async logging with context" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "async_logger")
  
  // Create context for async operation
  let async_ctx = Context::root()
  let operation_key = ContextKey::new("async_operation")
  let correlation_key = ContextKey::new("correlation_id")
  let user_key = ContextKey::new("user_id")
  
  let ctx_with_operation = Context::with_value(async_ctx, operation_key, "user_registration")
  let ctx_with_correlation = Context::with_value(ctx_with_operation, correlation_key, "corr_12345")
  let ctx_with_user = Context::with_value(ctx_with_correlation, user_key, "user_67890")
  
  // Log async operation start
  let start_attrs = Attributes::new()
  Attributes::set(start_attrs, "operation", StringValue("user_registration"))
  Attributes::set(start_attrs, "phase", StringValue("start"))
  Attributes::set(start_attrs, "correlation_id", StringValue("corr_12345"))
  Attributes::set(start_attrs, "user_id", StringValue("user_67890"))
  
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Async user registration started"),
    Some(start_attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("async_trace_12345"),
    Some("async_span_67890"),
    Some(ctx_with_user)
  )
  
  Logger::emit(logger, start_log)
  
  // Simulate async validation step
  let validation_attrs = Attributes::new()
  Attributes::set(validation_attrs, "operation", StringValue("user_registration"))
  Attributes::set(validation_attrs, "phase", StringValue("validation"))
  Attributes::set(validation_attrs, "correlation_id", StringValue("corr_12345"))
  Attributes::set(validation_attrs, "validation_type", StringValue("email_format"))
  
  let validation_log = LogRecord::new_with_context(
    Debug,
    Some("Async email validation in progress"),
    Some(validation_attrs),
    Some(1234567892L),
    Some(1234567893L),
    Some("async_trace_12345"),
    Some("async_span_67891"),
    Some(ctx_with_user)
  )
  
  Logger::emit(logger, validation_log)
  
  // Simulate async database operation
  let db_attrs = Attributes::new()
  Attributes::set(db_attrs, "operation", StringValue("user_registration"))
  Attributes::set(db_attrs, "phase", StringValue("database"))
  Attributes::set(db_attrs, "correlation_id", StringValue("corr_12345"))
  Attributes::set(db_attrs, "db_operation", StringValue("insert"))
  Attributes::set(db_attrs, "table", StringValue("users"))
  
  let db_log = LogRecord::new_with_context(
    Info,
    Some("Async database insert operation"),
    Some(db_attrs),
    Some(1234567894L),
    Some(1234567895L),
    Some("async_trace_12345"),
    Some("async_span_67892"),
    Some(ctx_with_user)
  )
  
  Logger::emit(logger, db_log)
  
  // Simulate async notification step
  let notification_attrs = Attributes::new()
  Attributes::set(notification_attrs, "operation", StringValue("user_registration"))
  Attributes::set(notification_attrs, "phase", StringValue("notification"))
  Attributes::set(notification_attrs, "correlation_id", StringValue("corr_12345"))
  Attributes::set(notification_attrs, "notification_type", StringValue("welcome_email"))
  
  let notification_log = LogRecord::new_with_context(
    Info,
    Some("Async welcome email notification sent"),
    Some(notification_attrs),
    Some(1234567896L),
    Some(1234567897L),
    Some("async_trace_12345"),
    Some("async_span_67893"),
    Some(ctx_with_user)
  )
  
  Logger::emit(logger, notification_log)
  
  // Log async operation completion
  let completion_attrs = Attributes::new()
  Attributes::set(completion_attrs, "operation", StringValue("user_registration"))
  Attributes::set(completion_attrs, "phase", StringValue("completion"))
  Attributes::set(completion_attrs, "correlation_id", StringValue("corr_12345"))
  Attributes::set(completion_attrs, "status", StringValue("success"))
  Attributes::set(completion_attrs, "duration_ms", StringValue("250"))
  
  let completion_log = LogRecord::new_with_context(
    Info,
    Some("Async user registration completed successfully"),
    Some(completion_attrs),
    Some(1234567898L),
    Some(1234567899L),
    Some("async_trace_12345"),
    Some("async_span_67894"),
    Some(ctx_with_user)
  )
  
  Logger::emit(logger, completion_log)
  
  // Verify log record integrity
  assert_eq(LogRecord::severity_number(start_log), Info)
  assert_eq(LogRecord::severity_number(validation_log), Debug)
  assert_eq(LogRecord::severity_number(db_log), Info)
  assert_eq(LogRecord::severity_number(notification_log), Info)
  assert_eq(LogRecord::severity_number(completion_log), Info)
  
  match LogRecord::trace_id(completion_log) {
    Some(trace_id) => assert_eq(trace_id, "async_trace_12345")
    None => assert_true(false, "Trace ID should be preserved")
  }
}

// Test 5: Async Baggage Propagation
test "async baggage propagation" {
  let baggage = Baggage::new()
  
  // Set initial baggage entries
  let baggage_with_request = Baggage::set_entry(baggage, "request_id", "req_12345")
  let baggage_with_user = Baggage::set_entry(baggage_with_request, "user_id", "user_67890")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session_id", "sess_abcde")
  
  // Simulate async operation 1 with baggage
  let async1_baggage = Baggage::set_entry(baggage_with_session, "async1_operation", "data_processing")
  
  // Verify baggage propagation
  match Baggage::get_entry(async1_baggage, "request_id") {
    Some(value) => assert_eq(value, "req_12345")
    None => assert_true(false, "Request ID should be preserved in async1")
  }
  
  match Baggage::get_entry(async1_baggage, "user_id") {
    Some(value) => assert_eq(value, "user_67890")
    None => assert_true(false, "User ID should be preserved in async1")
  }
  
  match Baggage::get_entry(async1_baggage, "session_id") {
    Some(value) => assert_eq(value, "sess_abcde")
    None => assert_true(false, "Session ID should be preserved in async1")
  }
  
  match Baggage::get_entry(async1_baggage, "async1_operation") {
    Some(value) => assert_eq(value, "data_processing")
    None => assert_true(false, "Async1 operation should be present")
  }
  
  // Simulate async operation 2 with baggage
  let async2_baggage = Baggage::set_entry(async1_baggage, "async2_operation", "notification_sending")
  
  // Verify baggage propagation through multiple async operations
  match Baggage::get_entry(async2_baggage, "request_id") {
    Some(value) => assert_eq(value, "req_12345")
    None => assert_true(false, "Request ID should be preserved through async2")
  }
  
  match Baggage::get_entry(async2_baggage, "async1_operation") {
    Some(value) => assert_eq(value, "data_processing")
    None => assert_true(false, "Async1 operation should be preserved in async2")
  }
  
  match Baggage::get_entry(async2_baggage, "async2_operation") {
    Some(value) => assert_eq(value, "notification_sending")
    None => assert_true(false, "Async2 operation should be present")
  }
  
  // Simulate async operation 3 that modifies existing baggage
  let async3_baggage = Baggage::set_entry(async2_baggage, "user_id", "user_modified_99999")
  
  // Verify baggage modification
  match Baggage::get_entry(async3_baggage, "user_id") {
    Some(value) => assert_eq(value, "user_modified_99999")
    None => assert_true(false, "User ID should be modified in async3")
  }
  
  // Other entries should still be preserved
  match Baggage::get_entry(async3_baggage, "request_id") {
    Some(value) => assert_eq(value, "req_12345")
    None => assert_true(false, "Request ID should still be preserved after modification")
  }
}

// Test 6: Async Resource Context
test "async resource context" {
  // Create base resource
  let base_attrs = [
    ("service.name", StringValue("async_service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("async_instance_123")),
    ("host.name", StringValue("async-host")),
    ("process.pid", IntValue(12345))
  ]
  
  let base_resource = Resource::with_attributes(Resource::new(), base_attrs)
  
  // Simulate async operation with resource context
  let async_attrs = [
    ("async.operation_id", StringValue("async_op_456")),
    ("async.operation_type", StringValue("data_processing")),
    ("async.thread_id", StringValue("thread_789")),
    ("async.correlation_id", StringValue("corr_async_123"))
  ]
  
  let async_resource = Resource::with_attributes(base_resource, async_attrs)
  
  // Verify base resource attributes are preserved
  match Resource::get_attribute(async_resource, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "async_service")
    None => assert_true(false, "Service name should be preserved in async resource")
  }
  
  match Resource::get_attribute(async_resource, "service.version") {
    Some(StringValue(value)) => assert_eq(value, "1.0.0")
    None => assert_true(false, "Service version should be preserved in async resource")
  }
  
  // Verify async-specific attributes are added
  match Resource::get_attribute(async_resource, "async.operation_id") {
    Some(StringValue(value)) => assert_eq(value, "async_op_456")
    None => assert_true(false, "Async operation ID should be present")
  }
  
  match Resource::get_attribute(async_resource, "async.operation_type") {
    Some(StringValue(value)) => assert_eq(value, "data_processing")
    None => assert_true(false, "Async operation type should be present")
  }
  
  // Simulate nested async operation with additional resource context
  let nested_async_attrs = [
    ("nested.async.operation_id", StringValue("nested_async_op_999")),
    ("nested.async.parent_id", StringValue("async_op_456")),
    ("nested.async.depth", StringValue("2"))
  ]
  
  let nested_async_resource = Resource::with_attributes(async_resource, nested_async_attrs)
  
  // Verify all context levels are preserved
  match Resource::get_attribute(nested_async_resource, "service.name") {
    Some(StringValue(value)) => assert_eq(value, "async_service")
    None => assert_true(false, "Service name should be preserved in nested async resource")
  }
  
  match Resource::get_attribute(nested_async_resource, "async.operation_id") {
    Some(StringValue(value)) => assert_eq(value, "async_op_456")
    None => assert_true(false, "Parent async operation ID should be preserved in nested resource")
  }
  
  match Resource::get_attribute(nested_async_resource, "nested.async.operation_id") {
    Some(StringValue(value)) => assert_eq(value, "nested_async_op_999")
    None => assert_true(false, "Nested async operation ID should be present")
  }
  
  match Resource::get_attribute(nested_async_resource, "nested.async.parent_id") {
    Some(StringValue(value)) => assert_eq(value, "async_op_456")
    None => assert_true(false, "Nested async parent ID should be present")
  }
}

// Test 7: Async Error Handling with Context
test "async error handling with context" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "async_error_tracer")
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "async_error_logger")
  
  // Create span for async operation that will encounter errors
  let error_span_ctx = SpanContext::new("error_async_trace", "error_async_span", true, "error_state")
  let error_span = Span::new("error_async_operation", Internal, error_span_ctx)
  
  Span::add_event(error_span, "operation_start", Some([
    ("operation_type", StringValue("error_prone_async")),
    ("component", StringValue("test_framework"))
  ]))
  
  // Simulate async error in first phase
  Span::add_event(error_span, "error_phase1", Some([
    ("error_type", StringValue("validation_error")),
    ("error_message", StringValue("Invalid input data")),
    ("error_code", StringValue("ERR_1001"))
  ]))
  
  Span::set_status(error_span, Error, Some("Validation failed in phase 1"))
  
  // Log error with context
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "error_type", StringValue("validation_error"))
  Attributes::set(error_attrs, "error_message", StringValue("Invalid input data"))
  Attributes::set(error_attrs, "error_code", StringValue("ERR_1001"))
  Attributes::set(error_attrs, "operation_phase", StringValue("1"))
  Attributes::set(error_attrs, "trace_id", StringValue("error_async_trace"))
  Attributes::set(error_attrs, "span_id", StringValue("error_async_span"))
  
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Async validation error occurred"),
    Some(error_attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("error_async_trace"),
    Some("error_async_span"),
    Some(Context::root())
  )
  
  Logger::emit(logger, error_log)
  
  // Simulate recovery and continuation
  Span::add_event(error_span, "recovery_attempt", Some([
    ("recovery_type", StringValue("data_sanitization")),
    ("recovery_status", StringValue("in_progress"))
  ]))
  
  // Simulate successful recovery
  Span::add_event(error_span, "recovery_success", Some([
    ("recovery_type", StringValue("data_sanitization")),
    ("recovery_status", StringValue("completed"))
  ]))
  
  Span::set_status(error_span, Ok, Some("Operation completed after recovery"))
  Span::end(error_span)
  
  // Verify error handling integrity
  assert_false(Span::is_recording(error_span))
  assert_eq(Span::status(error_span), Ok)
  
  // Verify log integrity
  assert_eq(LogRecord::severity_number(error_log), Error)
  match LogRecord::body(error_log) {
    Some(body) => assert_eq(body, "Async validation error occurred")
    None => assert_true(false, "Error log body should be preserved")
  }
  
  match LogRecord::trace_id(error_log) {
    Some(trace_id) => assert_eq(trace_id, "error_async_trace")
    None => assert_true(false, "Error log trace ID should be preserved")
  }
}