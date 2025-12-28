// Cross-Service Propagation Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for cross-service telemetry propagation

test "cross-service trace context propagation" {
  // Create initial trace context
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "key1=value1")
  
  // Create context with span information
  let ctx = Context::root()
  let trace_key = ContextKey::new("trace.context")
  let ctx_with_trace = Context::with_value(ctx, trace_key, trace_id + ":" + span_id)
  
  // Create propagator
  let propagator = W3CTraceContextPropagator::new()
  let composite_propagator = CompositePropagator::new([propagator])
  
  // Create carrier for propagation
  let carrier = TextMapCarrier::new()
  
  // Inject context into carrier
  CompositePropagator::inject(composite_propagator, ctx_with_trace, carrier)
  
  // Extract context from carrier
  let extracted_ctx = CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify propagation worked
  let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
  assert_eq(extracted_value, Some("true"))
}

test "cross-service baggage propagation" {
  // Create initial baggage
  let baggage = Baggage::new()
  let baggage_with_entries = Baggage::set_entry(baggage, "user.id", "user-123")
  let baggage_with_more_entries = Baggage::set_entry(baggage_with_entries, "request.id", "req-456")
  
  // Create context with baggage
  let ctx = Context::root()
  let baggage_key = ContextKey::new("baggage")
  let ctx_with_baggage = Context::with_value(ctx, baggage_key, "user.id=user-123,request.id=req-456")
  
  // Test baggage propagation through multiple services
  let service1_ctx = Context::with_value(ctx_with_baggage, ContextKey::new("service.name"), "service-1")
  let service2_ctx = Context::with_value(service1_ctx, ContextKey::new("service.name"), "service-2")
  let service3_ctx = Context::with_value(service2_ctx, ContextKey::new("service.name"), "service-3")
  
  // Verify baggage is preserved across services
  let final_baggage = Context::get(service3_ctx, baggage_key)
  assert_eq(final_baggage, Some("user.id=user-123,request.id=req-456"))
  
  // Verify service context is properly updated
  let service_name = Context::get(service3_ctx, ContextKey::new("service.name"))
  assert_eq(service_name, Some("service-3"))
}

test "cross-service HTTP header propagation" {
  // Create HTTP request with trace headers
  let trace_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "userId=user-123,sessionId=session-456")
  ]
  
  let request = HttpRequest::new("GET", "https://api.service.com/data", trace_headers)
  
  // Simulate service-to-service call
  let response_headers = [
    ("x-trace-id", "0af7651916cd43dd8448eb211c80319c"),
    ("x-span-id", "b7ad6b7169203331"),
    ("x-service-name", "api-service")
  ]
  
  let response = HttpResponse::new(200, response_headers)
  
  // Verify request headers
  assert_eq(HttpRequest::http_method(request), "GET")
  assert_eq(HttpRequest::url(request), "https://api.service.com/data")
  
  // Verify response headers
  assert_eq(HttpResponse::status_code(response), 200)
}

test "cross-service trace consistency" {
  // Create trace that spans multiple services
  let root_trace_id = "a1b2c3d4e5f678901234567890123456"
  let root_span_id = "1111111111111111"
  
  // Service 1: Creates root span
  let service1_span_ctx = SpanContext::new(root_trace_id, root_span_id, true, "")
  let service1_ctx = Context::with_value(Context::root(), ContextKey::new("trace.id"), root_trace_id)
  
  // Service 2: Creates child span
  let service2_span_id = "2222222222222222"
  let service2_span_ctx = SpanContext::new(root_trace_id, service2_span_id, true, "")
  let service2_ctx = Context::with_value(service1_ctx, ContextKey::new("parent.span"), root_span_id)
  let service2_ctx_with_span = Context::with_value(service2_ctx, ContextKey::new("span.id"), service2_span_id)
  
  // Service 3: Creates another child span
  let service3_span_id = "3333333333333333"
  let service3_span_ctx = SpanContext::new(root_trace_id, service3_span_id, true, "")
  let service3_ctx = Context::with_value(service2_ctx_with_span, ContextKey::new("span.id"), service3_span_id)
  
  // Verify trace consistency across services
  let final_trace_id = Context::get(service3_ctx, ContextKey::new("trace.id"))
  let final_parent_span = Context::get(service3_ctx, ContextKey::new("parent.span"))
  let final_span_id = Context::get(service3_ctx, ContextKey::new("span.id"))
  
  assert_eq(final_trace_id, Some(root_trace_id))
  assert_eq(final_parent_span, Some(root_span_id))
  assert_eq(final_span_id, Some(service3_span_id))
}

test "cross-service metric correlation" {
  // Create correlated metrics across services
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "cross-service-meter")
  
  // Service 1 metrics
  let service1_counter = Meter::create_counter(meter, "service1.requests.total")
  let service1_histogram = Meter::create_histogram(meter, "service1.response.time")
  
  Counter::add(service1_counter, 10.0)
  Histogram::record(service1_histogram, 150.5)
  
  // Service 2 metrics (correlated with same trace)
  let service2_counter = Meter::create_counter(meter, "service2.requests.total")
  let service2_histogram = Meter::create_histogram(meter, "service2.response.time")
  
  Counter::add(service2_counter, 8.0)
  Histogram::record(service2_histogram, 200.75)
  
  // Service 3 metrics (correlated with same trace)
  let service3_counter = Meter::create_counter(meter, "service3.requests.total")
  let service3_histogram = Meter::create_histogram(meter, "service3.response.time")
  
  Counter::add(service3_counter, 12.0)
  Histogram::record(service3_histogram, 100.25)
  
  // Verify metric creation
  assert_eq(service1_counter.name, "service1.requests.total")
  assert_eq(service2_counter.name, "service2.requests.total")
  assert_eq(service3_counter.name, "service3.requests.total")
}

test "cross-service log correlation" {
  // Create correlated log records across services
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "cross-service-logger")
  
  let trace_id = "trace-123456789"
  let span_id = "span-987654321"
  
  // Service 1 log
  let service1_log = LogRecord::new_with_context(
    Info,
    Some("Service 1: Request received"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some(trace_id),
    Some(span_id),
    None
  )
  
  // Service 2 log
  let service2_log = LogRecord::new_with_context(
    Info,
    Some("Service 2: Processing request"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some(trace_id),
    Some("span-456789123"),
    None
  )
  
  // Service 3 log
  let service3_log = LogRecord::new_with_context(
    Warn,
    Some("Service 3: Request completed with warnings"),
    None,
    Some(1735689600000000004L),
    Some(1735689600000000005L),
    Some(trace_id),
    Some("span-789123456"),
    None
  )
  
  // Verify log correlation
  assert_eq(LogRecord::trace_id(service1_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(service2_log), Some(trace_id))
  assert_eq(LogRecord::trace_id(service3_log), Some(trace_id))
  
  assert_eq(LogRecord::span_id(service1_log), Some(span_id))
  assert_eq(LogRecord::span_id(service2_log), Some("span-456789123"))
  assert_eq(LogRecord::span_id(service3_log), Some("span-789123456"))
}

test "cross-service error propagation" {
  // Simulate error propagation across services
  let error_ctx = Context::root()
  
  // Service 1: Error occurs
  let service1_error_ctx = Context::with_value(error_ctx, ContextKey::new("error.code"), "500")
  let service1_error_ctx_with_msg = Context::with_value(service1_error_ctx, ContextKey::new("error.message"), "Internal server error")
  
  // Service 2: Error is propagated
  let service2_error_ctx = Context::with_value(service1_error_ctx_with_msg, ContextKey::new("error.service"), "service-2")
  let service2_error_ctx_with_stack = Context::with_value(service2_error_ctx, ContextKey::new("error.stack"), "stack trace here")
  
  // Service 3: Error is handled
  let service3_error_ctx = Context::with_value(service2_error_ctx_with_stack, ContextKey::new("error.handled"), "true")
  let service3_error_ctx_with_recovery = Context::with_value(service3_error_ctx, ContextKey::new("error.recovery.action"), "retry")
  
  // Verify error propagation chain
  let error_code = Context::get(service3_error_ctx_with_recovery, ContextKey::new("error.code"))
  let error_message = Context::get(service3_error_ctx_with_recovery, ContextKey::new("error.message"))
  let error_service = Context::get(service3_error_ctx_with_recovery, ContextKey::new("error.service"))
  let error_handled = Context::get(service3_error_ctx_with_recovery, ContextKey::new("error.handled"))
  let recovery_action = Context::get(service3_error_ctx_with_recovery, ContextKey::new("error.recovery.action"))
  
  assert_eq(error_code, Some("500"))
  assert_eq(error_message, Some("Internal server error"))
  assert_eq(error_service, Some("service-2"))
  assert_eq(error_handled, Some("true"))
  assert_eq(recovery_action, Some("retry"))
}

test "cross-service async operation propagation" {
  // Simulate async operations across services
  let async_ctx = Context::root()
  
  // Start async operation
  let operation_id = "op-123456789"
  let async_start_ctx = Context::with_value(async_ctx, ContextKey::new("async.operation.id"), operation_id)
  let async_start_ctx_with_time = Context::with_value(async_start_ctx, ContextKey::new("async.start.time"), "2025-01-01T00:00:00Z")
  
  // Service 1: Processes part of async operation
  let service1_async_ctx = Context::with_value(async_start_ctx_with_time, ContextKey::new("service1.async.status"), "processing")
  let service1_async_ctx_with_progress = Context::with_value(service1_async_ctx, ContextKey::new("service1.progress"), "25")
  
  // Service 2: Continues async operation
  let service2_async_ctx = Context::with_value(service1_async_ctx_with_progress, ContextKey::new("service2.async.status"), "processing")
  let service2_async_ctx_with_progress = Context::with_value(service2_async_ctx, ContextKey::new("service2.progress"), "50")
  
  // Service 3: Completes async operation
  let service3_async_ctx = Context::with_value(service2_async_ctx_with_progress, ContextKey::new("service3.async.status"), "completed")
  let service3_async_ctx_with_progress = Context::with_value(service3_async_ctx, ContextKey::new("service3.progress"), "100")
  let service3_async_ctx_with_end_time = Context::with_value(service3_async_ctx_with_progress, ContextKey::new("async.end.time"), "2025-01-01T00:05:00Z")
  
  // Verify async operation propagation
  let final_operation_id = Context::get(service3_async_ctx_with_end_time, ContextKey::new("async.operation.id"))
  let final_start_time = Context::get(service3_async_ctx_with_end_time, ContextKey::new("async.start.time"))
  let final_end_time = Context::get(service3_async_ctx_with_end_time, ContextKey::new("async.end.time"))
  let final_status = Context::get(service3_async_ctx_with_end_time, ContextKey::new("service3.async.status"))
  let final_progress = Context::get(service3_async_ctx_with_end_time, ContextKey::new("service3.progress"))
  
  assert_eq(final_operation_id, Some(operation_id))
  assert_eq(final_start_time, Some("2025-01-01T00:00:00Z"))
  assert_eq(final_end_time, Some("2025-01-01T00:05:00Z"))
  assert_eq(final_status, Some("completed"))
  assert_eq(final_progress, Some("100"))
}

test "cross-service distributed transaction propagation" {
  // Simulate distributed transaction across services
  let transaction_ctx = Context::root()
  
  // Start distributed transaction
  let transaction_id = "tx-987654321"
  let transaction_start_ctx = Context::with_value(transaction_ctx, ContextKey::new("transaction.id"), transaction_id)
  let transaction_start_ctx_with_coordinator = Context::with_value(transaction_start_ctx, ContextKey::new("transaction.coordinator"), "service-1")
  
  // Service 1: Participates in transaction
  let service1_tx_ctx = Context::with_value(transaction_start_ctx_with_coordinator, ContextKey::new("service1.transaction.status"), "prepared")
  let service1_tx_ctx_with_participant = Context::with_value(service1_tx_ctx, ContextKey::new("service1.participant.id"), "p1")
  
  // Service 2: Participates in transaction
  let service2_tx_ctx = Context::with_value(service1_tx_ctx_with_participant, ContextKey::new("service2.transaction.status"), "prepared")
  let service2_tx_ctx_with_participant = Context::with_value(service2_tx_ctx, ContextKey::new("service2.participant.id"), "p2")
  
  // Service 3: Participates in transaction
  let service3_tx_ctx = Context::with_value(service2_tx_ctx_with_participant, ContextKey::new("service3.transaction.status"), "prepared")
  let service3_tx_ctx_with_participant = Context::with_value(service3_tx_ctx, ContextKey::new("service3.participant.id"), "p3")
  
  // Commit transaction
  let commit_ctx = Context::with_value(service3_tx_ctx_with_participant, ContextKey::new("transaction.status"), "committed")
  let commit_ctx_with_timestamp = Context::with_value(commit_ctx, ContextKey::new("transaction.commit.timestamp"), "2025-01-01T00:10:00Z")
  
  // Verify distributed transaction propagation
  let final_transaction_id = Context::get(commit_ctx_with_timestamp, ContextKey::new("transaction.id"))
  let final_coordinator = Context::get(commit_ctx_with_timestamp, ContextKey::new("transaction.coordinator"))
  let final_status = Context::get(commit_ctx_with_timestamp, ContextKey::new("transaction.status"))
  let final_commit_timestamp = Context::get(commit_ctx_with_timestamp, ContextKey::new("transaction.commit.timestamp"))
  
  assert_eq(final_transaction_id, Some(transaction_id))
  assert_eq(final_coordinator, Some("service-1"))
  assert_eq(final_status, Some("committed"))
  assert_eq(final_commit_timestamp, Some("2025-01-01T00:10:00Z"))
}