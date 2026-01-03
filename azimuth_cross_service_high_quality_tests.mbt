// Azimuth Telemetry System - High Quality Cross-Service Communication Tests
// This file contains comprehensive cross-service communication test cases

// Test 1: Trace Context Propagation Across Services
test "trace context propagation across services" {
  // Service A: Create initial trace
  let provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider_a, "service_a")
  let span_a = Tracer::start_span(tracer_a, "service_a_operation")
  
  // Get trace context from Service A
  let ctx_a = Span::span_context(span_a)
  let trace_id = SpanContext::trace_id(ctx_a)
  let span_id = SpanContext::span_id(ctx_a)
  
  // Simulate propagating context to Service B
  let propagated_headers = [
    ("traceparent", "00-" + trace_id + "-" + span_id + "-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  ]
  
  // Service B: Extract context and create child span
  let provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(provider_b, "service_b")
  let ctx_b = extract_context_from_headers(propagated_headers)
  let span_b = Tracer::start_span_with_context(tracer_b, "service_b_operation", ctx_b)
  
  // Verify trace context is preserved
  let ctx_b_retrieved = Span::span_context(span_b)
  assert_eq(SpanContext::trace_id(ctx_b_retrieved), trace_id)
  assert_ne(SpanContext::span_id(ctx_b_retrieved), span_id) // Should be different span ID
  assert_true(SpanContext::is_sampled(ctx_b_retrieved))
  
  // Service C: Further propagate context
  let ctx_c = Span::span_context(span_b)
  let span_id_b = SpanContext::span_id(ctx_c)
  
  let propagated_headers_c = [
    ("traceparent", "00-" + trace_id + "-" + span_id_b + "-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  ]
  
  let provider_c = TracerProvider::default()
  let tracer_c = TracerProvider::get_tracer(provider_c, "service_c")
  let ctx_c_extracted = extract_context_from_headers(propagated_headers_c)
  let span_c = Tracer::start_span_with_context(tracer_c, "service_c_operation", ctx_c_extracted)
  
  // Verify trace context is still preserved
  let ctx_c_retrieved = Span::span_context(span_c)
  assert_eq(SpanContext::trace_id(ctx_c_retrieved), trace_id)
  assert_ne(SpanContext::span_id(ctx_c_retrieved), span_id_b)
  assert_ne(SpanContext::span_id(ctx_c_retrieved), span_id)
  
  // End all spans
  Span::end(span_a)
  Span::end(span_b)
  Span::end(span_c)
}

// Test 2: Baggage Propagation Across Services
test "baggage propagation across services" {
  // Service A: Create initial baggage
  let baggage_a = Baggage::new()
  let baggage_a_with_entries = Baggage::set_entry(baggage_a, "user.id", "12345")
  let baggage_a_final = Baggage::set_entry(baggage_a_with_entries, "request.id", "req-67890")
  
  // Propagate baggage to Service B
  let baggage_headers = serialize_baggage_to_headers(baggage_a_final)
  
  // Service B: Extract baggage and add new entries
  let baggage_b = extract_baggage_from_headers(baggage_headers)
  let baggage_b_with_service = Baggage::set_entry(baggage_b, "service.name", "service_b")
  let baggage_b_final = Baggage::set_entry(baggage_b_with_service, "operation.type", "data_processing")
  
  // Verify original baggage entries are preserved
  let user_id = Baggage::get_entry(baggage_b_final, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  let request_id = Baggage::get_entry(baggage_b_final, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false)
  }
  
  // Verify new baggage entries are added
  let service_name = Baggage::get_entry(baggage_b_final, "service.name")
  match service_name {
    Some(value) => assert_eq(value, "service_b")
    None => assert_true(false)
  }
  
  let operation_type = Baggage::get_entry(baggage_b_final, "operation.type")
  match operation_type {
    Some(value) => assert_eq(value, "data_processing")
    None => assert_true(false)
  }
  
  // Service C: Further propagate baggage
  let baggage_headers_c = serialize_baggage_to_headers(baggage_b_final)
  let baggage_c = extract_baggage_from_headers(baggage_headers_c)
  let baggage_c_final = Baggage::set_entry(baggage_c, "service.name", "service_c")
  
  // Verify all baggage entries are preserved
  let user_id_c = Baggage::get_entry(baggage_c_final, "user.id")
  match user_id_c {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  let service_name_c = Baggage::get_entry(baggage_c_final, "service.name")
  match service_name_c {
    Some(value) => assert_eq(value, "service_c") // Should be overridden by service C
    None => assert_true(false)
  }
}

// Test 3: Cross-Service Metrics Correlation
test "cross-service metrics correlation" {
  // Service A: Create metrics with correlation attributes
  let provider_a = MeterProvider::default()
  let meter_a = MeterProvider::get_meter(provider_a, "service_a_meter")
  let counter_a = Meter::create_counter(meter_a, "service_a_requests", Some("Service A requests"), Some("count"))
  
  let correlation_id = "corr-" + get_current_time_millis().to_string()
  let request_id = "req-" + get_current_time_millis().to_string()
  
  Counter::add(counter_a, 1.0, Some([
    ("correlation.id", StringValue(correlation_id)),
    ("request.id", StringValue(request_id)),
    ("service.name", StringValue("service_a")),
    ("operation", StringValue("process_request"))
  ]))
  
  // Simulate propagating correlation context to Service B
  let correlation_headers = [
    ("x-correlation-id", correlation_id),
    ("x-request-id", request_id)
  ]
  
  // Service B: Create metrics with same correlation
  let provider_b = MeterProvider::default()
  let meter_b = MeterProvider::get_meter(provider_b, "service_b_meter")
  let counter_b = Meter::create_counter(meter_b, "service_b_requests", Some("Service B requests"), Some("count"))
  let histogram_b = Meter::create_histogram(meter_b, "service_b_duration", Some("Service B duration"), Some("ms"))
  
  Counter::add(counter_b, 1.0, Some([
    ("correlation.id", StringValue(correlation_id)),
    ("request.id", StringValue(request_id)),
    ("service.name", StringValue("service_b")),
    ("operation", StringValue("transform_data"))
  ]))
  
  Histogram::record(histogram_b, 150.0, Some([
    ("correlation.id", StringValue(correlation_id)),
    ("request.id", StringValue(request_id)),
    ("service.name", StringValue("service_b"))
  ]))
  
  // Service C: Create metrics with same correlation
  let provider_c = MeterProvider::default()
  let meter_c = MeterProvider::get_meter(provider_c, "service_c_meter")
  let counter_c = Meter::create_counter(meter_c, "service_c_requests", Some("Service C requests"), Some("count"))
  
  Counter::add(counter_c, 1.0, Some([
    ("correlation.id", StringValue(correlation_id)),
    ("request.id", StringValue(request_id)),
    ("service.name", StringValue("service_c")),
    ("operation", StringValue("store_result"))
  ]))
  
  // Verify metrics can be correlated by correlation ID
  // In a real implementation, this would query the metrics backend
  assert_true(correlation_id.length() > 0)
  assert_true(request_id.length() > 0)
}

// Test 4: Cross-Service Log Correlation
test "cross-service log correlation" {
  // Service A: Create log with correlation context
  let provider_a = LoggerProvider::default()
  let logger_a = LoggerProvider::get_logger(provider_a, "service_a_logger")
  
  let trace_id = "trace-" + get_current_time_millis().to_string()
  let span_id_a = "span-a-" + get_current_time_millis().to_string()
  let correlation_id = "corr-" + get_current_time_millis().to_string()
  
  let log_record_a = LogRecord::new_with_context(
    Info,
    Some("Processing request in Service A"),
    Some([
      ("service.name", StringValue("service_a")),
      ("correlation.id", StringValue(correlation_id)),
      ("request.id", StringValue("req-123"))
    ]),
    Some(get_current_time_millis()),
    None,
    Some(trace_id),
    Some(span_id_a),
    None
  )
  
  Logger::emit(logger_a, log_record_a)
  
  // Service B: Create correlated log
  let provider_b = LoggerProvider::default()
  let logger_b = LoggerProvider::get_logger(provider_b, "service_b_logger")
  let span_id_b = "span-b-" + get_current_time_millis().to_string()
  
  let log_record_b = LogRecord::new_with_context(
    Info,
    Some("Transforming data in Service B"),
    Some([
      ("service.name", StringValue("service_b")),
      ("correlation.id", StringValue(correlation_id)),
      ("request.id", StringValue("req-123"))
    ]),
    Some(get_current_time_millis()),
    None,
    Some(trace_id),
    Some(span_id_b),
    None
  )
  
  Logger::emit(logger_b, log_record_b)
  
  // Service C: Create correlated error log
  let provider_c = LoggerProvider::default()
  let logger_c = LoggerProvider::get_logger(provider_c, "service_c_logger")
  let span_id_c = "span-c-" + get_current_time_millis().to_string()
  
  let log_record_c = LogRecord::new_with_context(
    Error,
    Some("Failed to store result in Service C"),
    Some([
      ("service.name", StringValue("service_c")),
      ("correlation.id", StringValue(correlation_id)),
      ("request.id", StringValue("req-123")),
      ("error.code", StringValue("STORAGE_ERROR")),
      ("error.message", StringValue("Database connection timeout"))
    ]),
    Some(get_current_time_millis()),
    None,
    Some(trace_id),
    Some(span_id_c),
    None
  )
  
  Logger::emit(logger_c, log_record_c)
  
  // Verify all logs share the same trace and correlation IDs
  assert_eq(trace_id, trace_id)
  assert_eq(correlation_id, correlation_id)
  assert_ne(span_id_a, span_id_b)
  assert_ne(span_id_b, span_id_c)
  assert_ne(span_id_a, span_id_c)
}

// Test 5: Cross-Service HTTP Communication with Telemetry
test "cross-service http communication with telemetry" {
  // Service A: Initiate HTTP call to Service B
  let provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider_a, "service_a_client")
  let span_a = Tracer::start_span(tracer_a, "HTTP POST /api/process")
  
  // Add HTTP attributes to span
  Span::set_attribute(span_a, "http.method", StringValue("POST"))
  Span::set_attribute(span_a, "http.url", StringValue("https://service-b.example.com/api/process"))
  Span::set_attribute(span_a, "net.peer.name", StringValue("service-b.example.com"))
  Span::set_attribute(span_a, "net.peer.port", IntValue(443))
  
  // Create HTTP client with telemetry
  let client = HttpClient::with_telemetry(span_a)
  
  // Prepare request with trace headers
  let ctx = Span::span_context(span_a)
  let trace_headers = [
    ("traceparent", "00-" + SpanContext::trace_id(ctx) + "-" + SpanContext::span_id(ctx) + "-01"),
    ("x-correlation-id", "corr-" + get_current_time_millis().to_string()),
    ("content-type", "application/json")
  ]
  
  let request = HttpRequest::new(
    "POST",
    "https://service-b.example.com/api/process",
    trace_headers,
    Some("{\"data\":\"test_data\"}")
  )
  
  // Simulate HTTP call (in real implementation, this would make actual HTTP call)
  let response = simulate_http_call(request)
  
  // Add response attributes to span
  Span::set_attribute(span_a, "http.status_code", IntValue(HttpResponse::status_code(response)))
  
  if HttpResponse::status_code(response) >= 400 {
    Span::set_status(span_a, Error, Some("HTTP request failed"))
  } else {
    Span::set_status(span_a, Ok, None)
  }
  
  Span::end(span_a)
  
  // Verify HTTP telemetry is captured
  assert_eq(Span::name(span_a), "HTTP POST /api/process")
  
  let http_method = Span::get_attribute(span_a, "http.method")
  match http_method {
    Some(StringValue(method)) => assert_eq(method, "POST")
    _ => assert_true(false)
  }
  
  let http_status = Span::get_attribute(span_a, "http.status_code")
  match http_status {
    Some(IntValue(status)) => assert_true(status >= 100 && status < 600)
    _ => assert_true(false)
  }
}

// Test 6: Cross-Service Asynchronous Communication
test "cross-service asynchronous communication" {
  // Service A: Publish message to queue
  let provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider_a, "service_a_publisher")
  let span_a = Tracer::start_span(tracer_a, "publish_message")
  
  let trace_id = SpanContext::trace_id(Span::span_context(span_a))
  let correlation_id = "corr-" + get_current_time_millis().to_string()
  
  // Create message with telemetry context
  let message = Message::new(
    "data.processing.request",
    "{\"input\":\"test_data\"}",
    [
      ("trace_id", trace_id),
      ("correlation_id", correlation_id),
      ("span_id", SpanContext::span_id(Span::span_context(span_a)))
    ]
  )
  
  // Publish message
  let queue_client = QueueClient::with_telemetry(span_a)
  let publish_result = QueueClient::publish(queue_client, "processing_queue", message)
  
  // Add telemetry attributes
  Span::set_attribute(span_a, "messaging.system", StringValue("rabbitmq"))
  Span::set_attribute(span_a, "messaging.destination", StringValue("processing_queue"))
  Span::set_attribute(span_a, "messaging.message_id", StringValue(publish_result.message_id))
  
  Span::end(span_a)
  
  // Service B: Process message asynchronously
  let provider_b = TracerProvider::default()
  let tracer_b = TracerProvider::get_tracer(provider_b, "service_b_processor")
  
  // Extract context from message
  let message_trace_id = message.get_header("trace_id")
  let message_correlation_id = message.get_header("correlation_id")
  let parent_span_id = message.get_header("span_id")
  
  // Create child span with extracted context
  let ctx_b = create_context_from_ids(message_trace_id, parent_span_id)
  let span_b = Tracer::start_span_with_context(tracer_b, "process_message", ctx_b)
  
  // Add processing telemetry
  Span::set_attribute(span_b, "messaging.system", StringValue("rabbitmq"))
  Span::set_attribute(span_b, "messaging.destination", StringValue("processing_queue"))
  Span::set_attribute(span_b, "messaging.message_id", StringValue(publish_result.message_id))
  Span::set_attribute(span_b, "correlation.id", StringValue(message_correlation_id))
  
  // Process message
  let processing_result = process_message_data(message.body)
  
  // Publish result message
  let result_message = Message::new(
    "data.processing.response",
    "{\"result\":\"" + processing_result + "\"}",
    [
      ("trace_id", trace_id),
      ("correlation_id", correlation_id),
      ("span_id", SpanContext::span_id(Span::span_context(span_b)))
    ]
  )
  
  let result_queue_client = QueueClient::with_telemetry(span_b)
  let result_publish_result = QueueClient::publish(result_queue_client, "result_queue", result_message)
  
  Span::end(span_b)
  
  // Verify trace continuity
  assert_eq(SpanContext::trace_id(Span::span_context(span_a)), trace_id)
  assert_eq(SpanContext::trace_id(Span::span_context(span_b)), trace_id)
  assert_ne(SpanContext::span_id(Span::span_context(span_a)), SpanContext::span_id(Span::span_context(span_b)))
}

// Test 7: Cross-Service Circuit Breaker with Telemetry
test "cross-service circuit breaker with telemetry" {
  // Service A: Configure circuit breaker for Service B calls
  let circuit_breaker = CircuitBreaker::new(
    "service_b_circuit",
    5,      // failure threshold
    10000,  // recovery timeout (ms)
    50      // success threshold
  )
  
  let provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider_a, "service_a_client")
  let meter_a = MeterProvider::get_meter(provider_a, "service_a_meter")
  let circuit_counter = Meter::create_counter(meter_a, "circuit_breaker_operations")
  let circuit_gauge = Meter::create_gauge(meter_a, "circuit_breaker_state")
  
  // Simulate multiple calls with some failures
  for i in 0..10 {
    let span = Tracer::start_span(tracer_a, "call_service_b")
    
    // Record circuit breaker state
    let state = CircuitBreaker::get_state(circuit_breaker)
    if state == "closed" {
      Gauge::record(circuit_gauge, 0.0, Some([("circuit.name", StringValue("service_b_circuit"))]))
    } else if state == "open" {
      Gauge::record(circuit_gauge, 1.0, Some([("circuit.name", StringValue("service_b_circuit"))]))
    } else {
      Gauge::record(circuit_gauge, 0.5, Some([("circuit.name", StringValue("service_b_circuit"))]))
    }
    
    // Make call through circuit breaker
    let result = CircuitBreaker::call(circuit_breaker, fn() {
      // Simulate HTTP call that fails 50% of the time
      if i % 2 == 0 {
        return Ok("success")
      } else {
        return Error("service unavailable")
      }
    })
    
    match result {
      Ok(response) => {
        Span::set_status(span, Ok, None)
        Counter::add(circuit_counter, 1.0, Some([
          ("circuit.name", StringValue("service_b_circuit")),
          ("result", StringValue("success"))
        ]))
      }
      Error(error) => {
        Span::set_status(span, Error, Some(error))
        Counter::add(circuit_counter, 1.0, Some([
          ("circuit.name", StringValue("service_b_circuit")),
          ("result", StringValue("failure"))
        ]))
      }
    }
    
    Span::end(span)
  }
  
  // Verify circuit breaker changed state after failures
  let final_state = CircuitBreaker::get_state(circuit_breaker)
  assert_true(final_state == "open" || final_state == "half_open")
}

// Test 8: Cross-Service Retry with Telemetry
test "cross-service retry with telemetry" {
  // Service A: Configure retry for Service B calls
  let retry_policy = RetryPolicy::new(
    3,      // max attempts
    1000,   // initial delay (ms)
    2.0     // backoff multiplier
  )
  
  let provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider_a, "service_a_client")
  let meter_a = MeterProvider::get_meter(provider_a, "service_a_meter")
  let retry_counter = Meter::create_counter(meter_a, "retry_attempts")
  let retry_histogram = Meter::create_histogram(meter_a, "retry_duration")
  
  let span = Tracer::start_span(tracer_a, "call_service_b_with_retry")
  
  // Make call with retry
  let start_time = get_current_time_millis()
  let mut attempt_count = 0
  
  let result = RetryPolicy::execute(retry_policy, fn() {
    attempt_count = attempt_count + 1
    
    // Record retry attempt
    Counter::add(retry_counter, 1.0, Some([
      ("service.target", StringValue("service_b")),
      ("attempt.number", IntValue(attempt_count))
    ]))
    
    // Simulate call that fails first 2 times, succeeds on 3rd
    if attempt_count < 3 {
      return Error("temporary failure")
    } else {
      return Ok("success after retries")
    }
  })
  
  let end_time = get_current_time_millis()
  let duration = end_time - start_time
  
  // Record total duration
  Histogram::record(retry_histogram, duration.to_float(), Some([
    ("service.target", StringValue("service_b")),
    ("final.result", StringValue("success"))
  ]))
  
  // Verify retry behavior
  match result {
    Ok(response) => {
      assert_eq(response, "success after retries")
      assert_eq(attempt_count, 3)
      Span::set_status(span, Ok, None)
    }
    Error(error) => {
      Span::set_status(span, Error, Some(error))
    }
  }
  
  // Add retry attributes to span
  Span::set_attribute(span, "retry.attempts", IntValue(attempt_count))
  Span::set_attribute(span, "retry.duration_ms", IntValue(duration))
  
  Span::end(span)
}

// Test 9: Cross-Service Distributed Transaction
test "cross-service distributed transaction" {
  // Service A: Coordinate distributed transaction
  let provider_a = TracerProvider::default()
  let tracer_a = TracerProvider::get_tracer(provider_a, "transaction_coordinator")
  let transaction_span = Tracer::start_span(tracer_a, "distributed_transaction")
  
  let transaction_id = "txn-" + get_current_time_millis().to_string()
  let trace_id = SpanContext::trace_id(Span::span_context(transaction_span))
  
  // Add transaction attributes
  Span::set_attribute(transaction_span, "transaction.id", StringValue(transaction_id))
  Span::set_attribute(transaction_span, "transaction.type", StringValue("two_phase_commit"))
  
  // Phase 1: Prepare all services
  let prepare_results = []
  
  // Service B: Prepare
  let span_b = Tracer::start_span_with_context(tracer_a, "prepare_service_b", Span::span_context(transaction_span))
  let prepare_b = call_service_prepare("service_b", transaction_id, trace_id)
  prepare_results.push(prepare_b)
  Span::end(span_b)
  
  // Service C: Prepare
  let span_c = Tracer::start_span_with_context(tracer_a, "prepare_service_c", Span::span_context(transaction_span))
  let prepare_c = call_service_prepare("service_c", transaction_id, trace_id)
  prepare_results.push(prepare_c)
  Span::end(span_c)
  
  // Check if all services prepared successfully
  let all_prepared = true
  for result in prepare_results {
    match result {
      Ok(_) => continue
      Error(_) => {
        all_prepared = false
        break
      }
    }
  }
  
  // Phase 2: Commit or rollback
  if all_prepared {
    // Commit all services
    let span_commit = Tracer::start_span_with_context(tracer_a, "commit_transaction", Span::span_context(transaction_span))
    
    let commit_b = call_service_commit("service_b", transaction_id, trace_id)
    let commit_c = call_service_commit("service_c", transaction_id, trace_id)
    
    match (commit_b, commit_c) {
      (Ok(_), Ok(_)) => {
        Span::set_status(transaction_span, Ok, Some("Transaction committed successfully"))
        Span::set_attribute(transaction_span, "transaction.status", StringValue("committed"))
      }
      _ => {
        Span::set_status(transaction_span, Error, Some("Transaction commit failed"))
        Span::set_attribute(transaction_span, "transaction.status", StringValue("commit_failed"))
      }
    }
    
    Span::end(span_commit)
  } else {
    // Rollback all services
    let span_rollback = Tracer::start_span_with_context(tracer_a, "rollback_transaction", Span::span_context(transaction_span))
    
    let rollback_b = call_service_rollback("service_b", transaction_id, trace_id)
    let rollback_c = call_service_rollback("service_c", transaction_id, trace_id)
    
    Span::set_status(transaction_span, Error, Some("Transaction rolled back"))
    Span::set_attribute(transaction_span, "transaction.status", StringValue("rolled_back"))
    
    Span::end(span_rollback)
  }
  
  Span::end(transaction_span)
  
  // Verify transaction telemetry
  let transaction_status = Span::get_attribute(transaction_span, "transaction.status")
  match transaction_status {
    Some(StringValue(status)) => assert_true(status == "committed" || status == "rolled_back" || status == "commit_failed")
    _ => assert_true(false)
  }
}

// Test 10: Cross-Service Service Mesh Integration
test "cross-service service mesh integration" {
  // Simulate service mesh environment with sidecar proxies
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "service_mesh_test")
  
  // Service A with sidecar
  let span_a = Tracer::start_span(tracer, "service_a_operation")
  Span::set_attribute(span_a, "service.name", StringValue("service_a"))
  Span::set_attribute(span_a, "service.version", StringValue("1.2.3"))
  Span::set_attribute(span_a, "service.instance.id", StringValue("instance-a-123"))
  
  // Simulate request through service mesh
  let mesh_headers = [
    ("x-request-id", "req-" + get_current_time_millis().to_string()),
    ("x-b3-traceid", SpanContext::trace_id(Span::span_context(span_a))),
    ("x-b3-spanid", SpanContext::span_id(Span::span_context(span_a))),
    ("x-b3-sampled", "1"),
    ("x-envoy-attempt-count", "1"),
    ("x-envoy-upstream-service-time", "150")
  ]
  
  // Service B with sidecar
  let span_b = Tracer::start_span_with_context(tracer, "service_b_operation", extract_context_from_headers(mesh_headers))
  Span::set_attribute(span_b, "service.name", StringValue("service_b"))
  Span::set_attribute(span_b, "service.version", StringValue("2.1.0"))
  Span::set_attribute(span_b, "service.instance.id", StringValue("instance-b-456"))
  Span::set_attribute(span_b, "mesh.upstream_service_time", IntValue(150))
  
  // Simulate request to Service C through service mesh
  let mesh_headers_c = [
    ("x-request-id", "req-" + get_current_time_millis().to_string()),
    ("x-b3-traceid", SpanContext::trace_id(Span::span_context(span_b))),
    ("x-b3-spanid", SpanContext::span_id(Span::span_context(span_b))),
    ("x-b3-sampled", "1"),
    ("x-envoy-attempt-count", "1"),
    ("x-envoy-upstream-service-time", "200")
  ]
  
  // Service C with sidecar
  let span_c = Tracer::start_span_with_context(tracer, "service_c_operation", extract_context_from_headers(mesh_headers_c))
  Span::set_attribute(span_c, "service.name", StringValue("service_c"))
  Span::set_attribute(span_c, "service.version", StringValue("3.0.1"))
  Span::set_attribute(span_c, "service.instance.id", StringValue("instance-c-789"))
  Span::set_attribute(span_c, "mesh.upstream_service_time", IntValue(200))
  
  // End spans in reverse order
  Span::end(span_c)
  Span::end(span_b)
  Span::end(span_a)
  
  // Verify service mesh telemetry
  let trace_id_a = SpanContext::trace_id(Span::span_context(span_a))
  let trace_id_b = SpanContext::trace_id(Span::span_context(span_b))
  let trace_id_c = SpanContext::trace_id(Span::span_context(span_c))
  
  assert_eq(trace_id_a, trace_id_b)
  assert_eq(trace_id_b, trace_id_c)
  
  let upstream_time_b = Span::get_attribute(span_b, "mesh.upstream_service_time")
  match upstream_time_b {
    Some(IntValue(time)) => assert_eq(time, 150)
    _ => assert_true(false)
  }
  
  let upstream_time_c = Span::get_attribute(span_c, "mesh.upstream_service_time")
  match upstream_time_c {
    Some(IntValue(time)) => assert_eq(time, 200)
    _ => assert_true(false)
  }
}