// Azimuth Error Handling Comprehensive Test Suite
// This file contains comprehensive test cases for error handling scenarios

// Test 1: Span Error Handling
test "span error handling" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "error_test_tracer")
  
  // Create a span that encounters an error
  let span = Tracer::start_span(tracer, "operation_with_error")
  
  // Simulate an error condition
  let error_occurred = true
  let error_message = "Database connection failed"
  let error_type = "connection_error"
  
  if error_occurred {
    // Record error in span
    Span::set_status(span, Error, Some(error_message))
    Span::add_event(span, "exception", Some([
      ("exception.type", error_type),
      ("exception.message", error_message),
      ("exception.stacktrace", "at Database.connect(line:123)")
    ]))
    
    // Add error attributes
    Span::add_attribute(span, "error", "true")
    Span::add_attribute(span, "error.type", error_type)
  }
  
  // End the span
  Span::end(span)
  
  // Verify error was recorded correctly
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let error_span = spans[0]
  assert_eq(error_span.status, Error)
  assert_eq(error_span.status_message, Some(error_message))
  assert_true(error_span.attributes.contains(("error", "true")))
  assert_true(error_span.attributes.contains(("error.type", error_type)))
  
  // Verify error event was recorded
  assert_eq(error_span.events.length(), 1)
  let error_event = error_span.events[0]
  assert_eq(error_event.name, "exception")
  assert_true(error_event.attributes.contains(("exception.type", error_type)))
  assert_true(error_event.attributes.contains(("exception.message", error_message)))
}

// Test 2: Metric Error Handling
test "metric error handling" {
  let provider = TelemetryProvider::new()
  let meter = TelemetryProvider::get_meter(provider, "error_test_meter")
  
  // Create a counter that might encounter errors
  let counter = Meter::create_counter(meter, "error_operations", Some("Operations that resulted in errors"), Some("count"))
  
  // Simulate operations with different outcomes
  let operations = [
    { success: true, error_type: None },
    { success: false, error_type: Some("timeout") },
    { success: false, error_type: Some("connection_failed") },
    { success: true, error_type: None },
    { success: false, error_type: Some("invalid_input") }
  ]
  
  for op in operations {
    if op.success {
      // Record successful operation
      let attrs = Attributes::new()
      Attributes::set(attrs, "status", "success")
      Counter::add(counter, 1.0, Some(attrs))
    } else {
      // Record failed operation
      let attrs = Attributes::new()
      Attributes::set(attrs, "status", "error")
      match op.error_type {
        Some(error_type) => Attributes::set(attrs, "error.type", error_type)
        None => {}
      }
      Counter::add(counter, 1.0, Some(attrs))
    }
  }
  
  // Force metrics export
  TelemetryProvider::force_flush(provider)
  
  // Verify error metrics were recorded correctly
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  let error_metrics = metrics.filter(fn(m) { m.name == "error_operations" })
  
  assert_eq(error_metrics.length(), operations.length())
  
  let success_metrics = error_metrics.filter(fn(m) { m.attributes.contains(("status", "success")) })
  let error_type_metrics = error_metrics.filter(fn(m) { m.attributes.contains(("status", "error")) })
  
  assert_eq(success_metrics.length(), 2)
  assert_eq(error_type_metrics.length(), 3)
  
  // Verify error types
  assert_true(error_type_metrics.any(fn(m) { m.attributes.contains(("error.type", "timeout")) }))
  assert_true(error_type_metrics.any(fn(m) { m.attributes.contains(("error.type", "connection_failed")) }))
  assert_true(error_type_metrics.any(fn(m) { m.attributes.contains(("error.type", "invalid_input")) }))
}

// Test 3: Log Error Handling
test "log error handling" {
  let provider = TelemetryProvider::new()
  let logger = TelemetryProvider::get_logger(provider, "error_test_logger")
  
  // Create logs with different error levels
  let log_entries = [
    { severity: Debug, message: "Debug message", error: None },
    { severity: Info, message: "Info message", error: None },
    { severity: Warning, message: "Warning occurred", error: Some("deprecated_api") },
    { severity: Error, message: "Error occurred", error: Some("validation_failed") },
    { severity: Error, message: "Critical error", error: Some("system_crash") }
  ]
  
  for entry in log_entries {
    let log_record = Logger::create_log_record(logger, entry.severity, entry.message)
    
    match entry.error {
      Some(error_type) => {
        LogRecord::add_attribute(log_record, "error.type", error_type)
        LogRecord::add_attribute(log_record, "error", "true")
      }
      None => {}
    }
    
    Logger::emit(logger, log_record)
  }
  
  // Force log export
  TelemetryProvider::force_flush(provider)
  
  // Verify logs were recorded correctly
  let logs = TelemetryProvider::get_collected_logs(provider)
  assert_eq(logs.length(), log_entries.length())
  
  let error_logs = logs.filter(fn(l) { l.severity == Error })
  let warning_logs = logs.filter(fn(l) { l.severity == Warning })
  let non_error_logs = logs.filter(fn(l) { l.severity != Error && l.severity != Warning })
  
  assert_eq(error_logs.length(), 2)
  assert_eq(warning_logs.length(), 1)
  assert_eq(non_error_logs.length(), 2)
  
  // Verify error attributes
  for log in error_logs {
    assert_true(log.attributes.contains(("error", "true")))
    assert_true(log.attributes.contains(("error.type")))
  }
  
  for log in warning_logs {
    assert_true(log.attributes.contains(("error.type", "deprecated_api")))
  }
  
  for log in non_error_logs {
    assert_false(log.attributes.contains(("error", "true")))
  }
}

// Test 4: Error Recovery Mechanisms
test "error recovery mechanisms" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "recovery_test_tracer")
  
  // Simulate an operation with error recovery
  let span = Tracer::start_span(tracer, "operation_with_recovery")
  
  // Record initial attempt
  Span::add_event(span, "attempt", Some([("attempt_number", "1")]))
  
  // Simulate first failure
  Span::add_event(span, "error", Some([
    ("error.type", "connection_timeout"),
    ("error.message", "Connection timed out after 30 seconds")
  ]))
  
  // Record retry attempt
  Span::add_event(span, "attempt", Some([("attempt_number", "2")]))
  
  // Simulate second failure
  Span::add_event(span, "error", Some([
    ("error.type", "connection_refused"),
    ("error.message", "Connection refused by server")
  ]))
  
  // Record final successful attempt
  Span::add_event(span, "attempt", Some([("attempt_number", "3")]))
  Span::add_event(span, "success", Some([("retry_count", "2")]))
  
  // Set final status
  Span::set_status(span, Ok, Some("Operation succeeded after 2 retries"))
  Span::add_attribute(span, "retry_count", "2")
  Span::add_attribute(span, "final_status", "success")
  
  // End the span
  Span::end(span)
  
  // Verify recovery was recorded correctly
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 1)
  
  let recovery_span = spans[0]
  assert_eq(recovery_span.status, Ok)
  assert_eq(recovery_span.status_message, Some("Operation succeeded after 2 retries"))
  assert_true(recovery_span.attributes.contains(("retry_count", "2")))
  assert_true(recovery_span.attributes.contains(("final_status", "success")))
  
  // Verify events
  assert_eq(recovery_span.events.length(), 5)
  
  let attempt_events = recovery_span.events.filter(fn(e) { e.name == "attempt" })
  let error_events = recovery_span.events.filter(fn(e) { e.name == "error" })
  let success_events = recovery_span.events.filter(fn(e) { e.name == "success" })
  
  assert_eq(attempt_events.length(), 3)
  assert_eq(error_events.length(), 2)
  assert_eq(success_events.length(), 1)
  
  // Verify attempt numbers
  assert_true(attempt_events.any(fn(e) { e.attributes.contains(("attempt_number", "1")) }))
  assert_true(attempt_events.any(fn(e) { e.attributes.contains(("attempt_number", "2")) }))
  assert_true(attempt_events.any(fn(e) { e.attributes.contains(("attempt_number", "3")) }))
}

// Test 5: Error Boundary Handling
test "error boundary handling" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "boundary_test_tracer")
  
  // Create a parent span
  let parent_span = Tracer::start_span(tracer, "parent_operation")
  
  // Create child spans with different error outcomes
  let child1 = Tracer::start_span_with_parent(tracer, "child_operation_1", parent_span)
  Span::set_status(child1, Ok, None)
  Span::end(child1)
  
  let child2 = Tracer::start_span_with_parent(tracer, "child_operation_2", parent_span)
  Span::set_status(child2, Error, Some("Child operation failed"))
  Span::end(child2)
  
  let child3 = Tracer::start_span_with_parent(tracer, "child_operation_3", parent_span)
  Span::set_status(child3, Ok, None)
  Span::end(child3)
  
  // Parent span should reflect the error state
  Span::set_status(parent_span, Error, Some("One or more child operations failed"))
  Span::add_attribute(parent_span, "failed_operations", "1")
  Span::add_attribute(parent_span, "total_operations", "3")
  
  // End parent span
  Span::end(parent_span)
  
  // Verify error boundary handling
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 4)
  
  let parent_span_collected = spans.find(fn(s) { s.name == "parent_operation" })
  let child_spans = spans.filter(fn(s) { s.name.starts_with("child_operation") })
  
  match parent_span_collected {
    Some(parent) => {
      assert_eq(parent.status, Error)
      assert_eq(parent.status_message, Some("One or more child operations failed"))
      assert_true(parent.attributes.contains(("failed_operations", "1")))
      assert_true(parent.attributes.contains(("total_operations", "3")))
    }
    None => assert_true(false)
  }
  
  assert_eq(child_spans.length(), 3)
  
  let successful_children = child_spans.filter(fn(s) { s.status == Ok })
  let failed_children = child_spans.filter(fn(s) { s.status == Error })
  
  assert_eq(successful_children.length(), 2)
  assert_eq(failed_children.length(), 1)
  
  // Verify parent-child relationships
  for child in child_spans {
    match child.parent_span_id {
      Some(parent_id) => assert_eq(parent_id, Span::span_id(parent_span))
      None => assert_true(false)
    }
  }
}

// Test 6: Error Context Propagation
test "error context propagation" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "context_test_tracer")
  
  // Create a root span with error context
  let root_span = Tracer::start_span(tracer, "root_operation")
  Span::add_attribute(root_span, "user.id", "user123")
  Span::add_attribute(root_span, "request.id", "req456")
  
  // Create a child span that encounters an error
  let child_span = Tracer::start_span_with_parent(tracer, "child_operation", root_span)
  Span::add_event(child_span, "error", Some([
    ("error.type", "validation_error"),
    ("error.message", "Invalid input parameter"),
    ("error.parameter", "user_id")
  ]))
  Span::set_status(child_span, Error, Some("Validation failed"))
  Span::end(child_span)
  
  // Create another child span that handles the error
  let recovery_span = Tracer::start_span_with_parent(tracer, "recovery_operation", root_span)
  Span::add_event(recovery_span, "recovery_attempt", Some([
    ("original_error", "validation_error"),
    ("recovery_strategy", "use_default_value")
  ]))
  Span::set_status(recovery_span, Ok, Some("Recovery successful"))
  Span::end(recovery_span)
  
  // Root span should reflect the overall operation status
  Span::set_status(root_span, Ok, Some("Operation completed with recovery"))
  Span::add_attribute(root_span, "error_occurred", "true")
  Span::add_attribute(root_span, "recovery_successful", "true")
  
  // End root span
  Span::end(root_span)
  
  // Verify error context propagation
  let spans = TelemetryProvider::get_collected_spans(provider)
  assert_eq(spans.length(), 3)
  
  let root_span_collected = spans.find(fn(s) { s.name == "root_operation" })
  let child_span_collected = spans.find(fn(s) { s.name == "child_operation" })
  let recovery_span_collected = spans.find(fn(s) { s.name == "recovery_operation" })
  
  match (root_span_collected, child_span_collected, recovery_span_collected) {
    (Some(root), Some(child), Some(recovery)) => {
      // Verify root span context
      assert_true(root.attributes.contains(("user.id", "user123")))
      assert_true(root.attributes.contains(("request.id", "req456")))
      assert_true(root.attributes.contains(("error_occurred", "true")))
      assert_true(root.attributes.contains(("recovery_successful", "true")))
      
      // Verify child span error
      assert_eq(child.status, Error)
      assert_eq(child.status_message, Some("Validation failed"))
      assert_eq(child.events.length(), 1)
      assert_eq(child.events[0].name, "error")
      assert_true(child.events[0].attributes.contains(("error.type", "validation_error")))
      
      // Verify recovery span
      assert_eq(recovery.status, Ok)
      assert_eq(recovery.status_message, Some("Recovery successful"))
      assert_eq(recovery.events.length(), 1)
      assert_eq(recovery.events[0].name, "recovery_attempt")
      assert_true(recovery.events[0].attributes.contains(("original_error", "validation_error")))
      
      // Verify trace propagation
      assert_eq(root.trace_id, child.trace_id)
      assert_eq(root.trace_id, recovery.trace_id)
    }
    _ => assert_true(false)
  }
}

// Test 7: Error Aggregation and Analysis
test "error aggregation and analysis" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "aggregation_test_tracer")
  let meter = TelemetryProvider::get_meter(provider, "aggregation_test_meter")
  
  // Create metrics for error aggregation
  let error_counter = Meter::create_counter(meter, "errors_total", Some("Total number of errors"), Some("count"))
  let error_histogram = Meter::create_histogram(meter, "error_duration", Some("Error handling duration"), Some("ms"))
  
  // Simulate various error scenarios
  let error_scenarios = [
    { type: "timeout", message: "Operation timed out", duration: 5000 },
    { type: "connection_failed", message: "Database connection failed", duration: 1000 },
    { type: "validation_error", message: "Input validation failed", duration: 100 },
    { type: "timeout", message: "Operation timed out", duration: 3000 },
    { type: "permission_denied", message: "Access denied", duration: 50 },
    { type: "connection_failed", message: "Database connection failed", duration: 2000 },
    { type: "validation_error", message: "Input validation failed", duration: 150 },
    { type: "timeout", message: "Operation timed out", duration: 4000 }
  ]
  
  for scenario in error_scenarios {
    // Create a span for the error scenario
    let span = Tracer::start_span(tracer, "error_scenario")
    Span::add_attribute(span, "error.type", scenario.type)
    Span::add_attribute(span, "error.message", scenario.message)
    
    // Record error in span
    Span::set_status(span, Error, Some(scenario.message))
    Span::add_event(span, "error", Some([
      ("error.type", scenario.type),
      ("error.message", scenario.message)
    ]))
    
    // End span
    Span::end(span)
    
    // Record error metrics
    let attrs = Attributes::new()
    Attributes::set(attrs, "error.type", scenario.type)
    Counter::add(error_counter, 1.0, Some(attrs))
    Histogram::record(error_histogram, scenario.duration.to_float(), Some(attrs))
  }
  
  // Force telemetry export
  TelemetryProvider::force_flush(provider)
  
  // Verify error aggregation
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  assert_eq(spans.length(), error_scenarios.length())
  
  // Verify error type distribution
  let timeout_spans = spans.filter(fn(s) { 
    s.attributes.contains(("error.type", "timeout")) 
  })
  let connection_spans = spans.filter(fn(s) { 
    s.attributes.contains(("error.type", "connection_failed")) 
  })
  let validation_spans = spans.filter(fn(s) { 
    s.attributes.contains(("error.type", "validation_error")) 
  })
  let permission_spans = spans.filter(fn(s) { 
    s.attributes.contains(("error.type", "permission_denied")) 
  })
  
  assert_eq(timeout_spans.length(), 3)
  assert_eq(connection_spans.length(), 2)
  assert_eq(validation_spans.length(), 2)
  assert_eq(permission_spans.length(), 1)
  
  // Verify error metrics
  let error_metrics = metrics.filter(fn(m) { m.name == "errors_total" })
  assert_eq(error_metrics.length(), error_scenarios.length())
  
  let timeout_metrics = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "timeout")) 
  })
  let connection_metrics = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "connection_failed")) 
  })
  let validation_metrics = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "validation_error")) 
  })
  let permission_metrics = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "permission_denied")) 
  })
  
  assert_eq(timeout_metrics.length(), 3)
  assert_eq(connection_metrics.length(), 2)
  assert_eq(validation_metrics.length(), 2)
  assert_eq(permission_metrics.length(), 1)
  
  // Verify duration metrics
  let duration_metrics = metrics.filter(fn(m) { m.name == "error_duration" })
  assert_eq(duration_metrics.length(), error_scenarios.length())
  
  // Verify duration values
  for i in 0..error_scenarios.length() {
    let scenario = error_scenarios[i]
    let metric = duration_metrics.find(fn(m) { 
      m.attributes.contains(("error.type", scenario.type)) && 
      m.value == scenario.duration.to_float()
    })
    assert_true(metric.is_some())
  }
}

// Test 8: Error Handling with Circuit Breaker Pattern
test "error handling with circuit breaker pattern" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "circuit_breaker_test_tracer")
  let meter = TelemetryProvider::get_meter(provider, "circuit_breaker_test_meter")
  
  // Create metrics for circuit breaker
  let circuit_state_gauge = Meter::create_gauge(meter, "circuit_breaker_state")
  let failure_counter = Meter::create_counter(meter, "circuit_breaker_failures")
  let success_counter = Meter::create_counter(meter, "circuit_breaker_successes")
  
  // Simulate circuit breaker states
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  let mut circuit_state = CircuitState::Closed
  let mut failure_count = 0
  let failure_threshold = 3
  
  // Simulate operations with circuit breaker
  let operations = [
    { success: false }, // 1st failure
    { success: false }, // 2nd failure
    { success: false }, // 3rd failure - circuit opens
    { success: false }, // Circuit open - rejected
    { success: false }, // Circuit open - rejected
    { success: true },  // Circuit open - rejected
    { success: true },  // Circuit half-open - success
    { success: true },  // Circuit closed again
    { success: true }
  ]
  
  for op in operations {
    let span = Tracer::start_span(tracer, "circuit_breaker_operation")
    
    match circuit_state {
      CircuitState::Closed => {
        if op.success {
          // Record success
          Span::set_status(span, Ok, None)
          Counter::add(success_counter, 1.0)
        } else {
          // Record failure
          Span::set_status(span, Error, Some("Operation failed"))
          Span::add_event(span, "circuit_breaker", Some([
            ("event", "failure_recorded"),
            ("failure_count", (failure_count + 1).to_string())
          ]))
          Counter::add(failure_counter, 1.0)
          failure_count = failure_count + 1
          
          // Check if circuit should open
          if failure_count >= failure_threshold {
            circuit_state = CircuitState::Open
            Span::add_event(span, "circuit_breaker", Some([
              ("event", "circuit_opened"),
              ("failure_count", failure_count.to_string())
            ]))
            Gauge::set(circuit_state_gauge, 1.0) // 1 = Open
          }
        }
      }
      CircuitState::Open => {
        // Circuit is open, reject operation
        Span::set_status(span, Error, Some("Circuit breaker is open"))
        Span::add_event(span, "circuit_breaker", Some([
          ("event", "operation_rejected"),
          ("reason", "circuit_open")
        ]))
        
        // Simulate circuit transitioning to half-open after some time
        circuit_state = CircuitState::HalfOpen
        Span::add_event(span, "circuit_breaker", Some([
          ("event", "circuit_half_open")
        ]))
        Gauge::set(circuit_state_gauge, 0.5) // 0.5 = HalfOpen
      }
      CircuitState::HalfOpen => {
        if op.success {
          // Record success and close circuit
          Span::set_status(span, Ok, None)
          Span::add_event(span, "circuit_breaker", Some([
            ("event", "circuit_closed")
          ]))
          Counter::add(success_counter, 1.0)
          circuit_state = CircuitState::Closed
          failure_count = 0
          Gauge::set(circuit_state_gauge, 0.0) // 0 = Closed
        } else {
          // Record failure and open circuit again
          Span::set_status(span, Error, Some("Operation failed in half-open state"))
          Span::add_event(span, "circuit_breaker", Some([
            ("event", "circuit_opened_again")
          ]))
          Counter::add(failure_counter, 1.0)
          circuit_state = CircuitState::Open
          Gauge::set(circuit_state_gauge, 1.0) // 1 = Open
        }
      }
    }
    
    Span::end(span)
  }
  
  // Force telemetry export
  TelemetryProvider::force_flush(provider)
  
  // Verify circuit breaker behavior
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  assert_eq(spans.length(), operations.length())
  
  // Verify circuit state transitions
  let circuit_opened_events = spans.filter(fn(s) { 
    s.events.any(fn(e) { 
      e.name == "circuit_breaker" && e.attributes.contains(("event", "circuit_opened"))
    })
  })
  let circuit_half_open_events = spans.filter(fn(s) { 
    s.events.any(fn(e) { 
      e.name == "circuit_breaker" && e.attributes.contains(("event", "circuit_half_open"))
    })
  })
  let circuit_closed_events = spans.filter(fn(s) { 
    s.events.any(fn(e) { 
      e.name == "circuit_breaker" && e.attributes.contains(("event", "circuit_closed"))
    })
  })
  
  assert_eq(circuit_opened_events.length(), 2) // Opened twice
  assert_eq(circuit_half_open_events.length(), 1)
  assert_eq(circuit_closed_events.length(), 1)
  
  // Verify circuit state gauge
  let state_metrics = metrics.filter(fn(m) { m.name == "circuit_breaker_state" })
  assert_eq(state_metrics.length(), 1)
  
  // Final state should be closed (0.0)
  let final_state = state_metrics[0]
  assert_eq(final_state.value, 0.0)
}

// Test 9: Error Handling with Retry Pattern
test "error handling with retry pattern" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "retry_test_tracer")
  let meter = TelemetryProvider::get_meter(provider, "retry_test_meter")
  
  // Create metrics for retry pattern
  let retry_counter = Meter::create_counter(meter, "retry_attempts")
  let success_counter = Meter::create_counter(meter, "retry_successes")
  let retry_histogram = Meter::create_histogram(meter, "retry_duration")
  
  // Simulate operation with retries
  let max_retries = 3
  let mut attempt = 0
  let operation_successful = false // Will succeed on 3rd attempt
  
  let span = Tracer::start_span(tracer, "operation_with_retries")
  
  while attempt <= max_retries && !operation_successful {
    attempt = attempt + 1
    
    // Record attempt
    Span::add_event(span, "retry_attempt", Some([
      ("attempt_number", attempt.to_string()),
      ("max_retries", max_retries.to_string())
    ]))
    
    // Simulate operation duration
    let duration = 1000 * attempt // Increasing duration for each attempt
    Histogram::record(retry_histogram, duration.to_float())
    
    if attempt == 3 {
      // Operation succeeds on 3rd attempt
      Span::set_status(span, Ok, Some("Operation succeeded after retries"))
      Span::add_event(span, "retry_success", Some([
        ("attempt_number", attempt.to_string()),
        ("total_attempts", attempt.to_string())
      ]))
      Counter::add(success_counter, 1.0)
      break
    } else {
      // Operation fails
      let error_type = match attempt {
        1 => "timeout"
        2 => "connection_failed"
        _ => "unknown"
      }
      
      Span::add_event(span, "retry_failure", Some([
        ("attempt_number", attempt.to_string()),
        ("error.type", error_type)
      ]))
      Counter::add(retry_counter, 1.0)
    }
  }
  
  Span::add_attribute(span, "total_attempts", attempt.to_string())
  Span::add_attribute(span, "max_retries", max_retries.to_string())
  
  // End span
  Span::end(span)
  
  // Force telemetry export
  TelemetryProvider::force_flush(provider)
  
  // Verify retry behavior
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  assert_eq(spans.length(), 1)
  
  let retry_span = spans[0]
  assert_eq(retry_span.status, Ok)
  assert_eq(retry_span.status_message, Some("Operation succeeded after retries"))
  assert_true(retry_span.attributes.contains(("total_attempts", "3")))
  assert_true(retry_span.attributes.contains(("max_retries", "3")))
  
  // Verify retry events
  let attempt_events = retry_span.events.filter(fn(e) { e.name == "retry_attempt" })
  let failure_events = retry_span.events.filter(fn(e) { e.name == "retry_failure" })
  let success_events = retry_span.events.filter(fn(e) { e.name == "retry_success" })
  
  assert_eq(attempt_events.length(), 3)
  assert_eq(failure_events.length(), 2)
  assert_eq(success_events.length(), 1)
  
  // Verify retry metrics
  let retry_metrics = metrics.filter(fn(m) { m.name == "retry_attempts" })
  let success_metrics = metrics.filter(fn(m) { m.name == "retry_successes" })
  let duration_metrics = metrics.filter(fn(m) { m.name == "retry_duration" })
  
  assert_eq(retry_metrics.length(), 2) // 2 failed attempts
  assert_eq(success_metrics.length(), 1) // 1 successful attempt
  assert_eq(duration_metrics.length(), 3) // 3 attempts with duration
  
  // Verify retry duration values
  let duration_values = duration_metrics.map(fn(m) { m.value })
  assert_true(duration_values.contains(1000.0))
  assert_true(duration_values.contains(2000.0))
  assert_true(duration_values.contains(3000.0))
}

// Test 10: Error Handling with Fallback Mechanisms
test "error handling with fallback mechanisms" {
  let provider = TelemetryProvider::new()
  let tracer = TelemetryProvider::get_tracer(provider, "fallback_test_tracer")
  let meter = TelemetryProvider::get_meter(provider, "fallback_test_meter")
  
  // Create metrics for fallback mechanisms
  let primary_counter = Meter::create_counter(meter, "primary_operations")
  let fallback_counter = Meter::create_counter(meter, "fallback_operations")
  let fallback_histogram = Meter::create_histogram(meter, "fallback_latency")
  
  // Simulate operation with fallback
  let span = Tracer::start_span(tracer, "operation_with_fallback")
  
  // Try primary operation
  Span::add_event(span, "primary_attempt", Some([
    ("operation", "database_query")
  ]))
  
  let primary_success = false // Simulate primary failure
  
  if primary_success {
    // Primary operation succeeded
    Span::set_status(span, Ok, Some("Primary operation succeeded"))
    Span::add_event(span, "primary_success", Some([
      ("operation", "database_query")
    ]))
    Counter::add(primary_counter, 1.0)
  } else {
    // Primary operation failed, try fallback
    Span::add_event(span, "primary_failure", Some([
      ("operation", "database_query"),
      ("error.type", "connection_timeout")
    ]))
    
    Span::add_event(span, "fallback_attempt", Some([
      ("operation", "cache_lookup")
    ]))
    
    let fallback_success = true // Fallback succeeds
    
    if fallback_success {
      // Fallback operation succeeded
      Span::set_status(span, Ok, Some("Operation succeeded with fallback"))
      Span::add_event(span, "fallback_success", Some([
        ("operation", "cache_lookup"),
        ("fallback_reason", "primary_timeout")
      ]))
      Counter::add(fallback_counter, 1.0)
      
      // Record fallback latency
      Histogram::record(fallback_histogram, 50.0)
    } else {
      // Fallback also failed
      Span::set_status(span, Error, Some("Both primary and fallback failed"))
      Span::add_event(span, "fallback_failure", Some([
        ("operation", "cache_lookup"),
        ("error.type", "cache_miss")
      ]))
    }
  }
  
  Span::add_attribute(span, "fallback_used", (if !primary_success { "true" } else { "false" }))
  
  // End span
  Span::end(span)
  
  // Force telemetry export
  TelemetryProvider::force_flush(provider)
  
  // Verify fallback behavior
  let spans = TelemetryProvider::get_collected_spans(provider)
  let metrics = TelemetryProvider::get_collected_metrics(provider)
  
  assert_eq(spans.length(), 1)
  
  let fallback_span = spans[0]
  assert_eq(fallback_span.status, Ok)
  assert_eq(fallback_span.status_message, Some("Operation succeeded with fallback"))
  assert_true(fallback_span.attributes.contains(("fallback_used", "true")))
  
  // Verify fallback events
  let primary_attempt_events = fallback_span.events.filter(fn(e) { e.name == "primary_attempt" })
  let primary_failure_events = fallback_span.events.filter(fn(e) { e.name == "primary_failure" })
  let fallback_attempt_events = fallback_span.events.filter(fn(e) { e.name == "fallback_attempt" })
  let fallback_success_events = fallback_span.events.filter(fn(e) { e.name == "fallback_success" })
  
  assert_eq(primary_attempt_events.length(), 1)
  assert_eq(primary_failure_events.length(), 1)
  assert_eq(fallback_attempt_events.length(), 1)
  assert_eq(fallback_success_events.length(), 1)
  
  // Verify fallback metrics
  let primary_metrics = metrics.filter(fn(m) { m.name == "primary_operations" })
  let fallback_metrics = metrics.filter(fn(m) { m.name == "fallback_operations" })
  let latency_metrics = metrics.filter(fn(m) { m.name == "fallback_latency" })
  
  assert_eq(primary_metrics.length(), 0) // Primary failed
  assert_eq(fallback_metrics.length(), 1) // Fallback succeeded
  assert_eq(latency_metrics.length(), 1) // Fallback latency recorded
  
  // Verify fallback metric value
  assert_eq(fallback_metrics[0].value, 1.0)
  assert_eq(latency_metrics[0].value, 50.0)
}