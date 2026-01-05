// Azimuth Telemetry System - Circuit Breaker Tests
// This file contains test cases for circuit breaker functionality

// Test 1: Basic Circuit Breaker Operations
test "basic circuit breaker operations" {
  let circuit_breaker = CircuitBreaker::new("test_service", 3, 5000)  // 3 failures, 5s timeout
  
  // Initial state should be closed
  assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
  
  // Successful operation should not change state
  let result = CircuitBreaker::execute(circuit_breaker, fn() { "success" })
  match result {
    Ok(value) => assert_eq(value, "success")
    Error(_) => assert_true(false)
  }
  
  assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
}

// Test 2: Circuit Breaker Failure Threshold
test "circuit breaker failure threshold" {
  let circuit_breaker = CircuitBreaker::new("failing_service", 3, 5000)  // 3 failures, 5s timeout
  
  // First failure
  let result1 = CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  match result1 {
    Ok(_) => assert_true(false)
    Error(_) => {
      assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
      assert_eq(CircuitBreaker::failure_count(circuit_breaker), 1)
    }
  }
  
  // Second failure
  let result2 = CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  match result2 {
    Ok(_) => assert_true(false)
    Error(_) => {
      assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
      assert_eq(CircuitBreaker::failure_count(circuit_breaker), 2)
    }
  }
  
  // Third failure - should trip the circuit
  let result3 = CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  match result3 {
    Ok(_) => assert_true(false)
    Error(_) => {
      assert_eq(CircuitBreaker::state(circuit_breaker), Open)
      assert_eq(CircuitBreaker::failure_count(circuit_breaker), 3)
    }
  }
}

// Test 3: Circuit Breaker Open State
test "circuit breaker open state" {
  let circuit_breaker = CircuitBreaker::new("open_service", 2, 1000)  // 2 failures, 1s timeout
  
  // Trip the circuit
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  
  // Should be open now
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
  
  // Any operation should fail immediately without executing
  let result = CircuitBreaker::execute(circuit_breaker, fn() { "should_not_execute" })
  match result {
    Ok(_) => assert_true(false)
    Error(error) => assert_eq(error, "Circuit breaker is open")
  }
}

// Test 4: Circuit Breaker Half-Open State
test "circuit breaker half-open state" {
  let circuit_breaker = CircuitBreaker::new("half_open_service", 2, 100)  // 2 failures, 100ms timeout
  
  // Trip the circuit
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
  
  // Wait for timeout
  Time::sleep(150)  // 150ms > 100ms timeout
  
  // Next operation should put it in half-open state
  let result = CircuitBreaker::execute(circuit_breaker, fn() { "success_after_timeout" })
  match result {
    Ok(value) => assert_eq(value, "success_after_timeout")
    Error(_) => assert_true(false)
  }
  
  // Should be in half-open state after one successful call
  assert_eq(CircuitBreaker::state(circuit_breaker), HalfOpen)
  
  // Another successful call should close the circuit
  let result2 = CircuitBreaker::execute(circuit_breaker, fn() { "another_success" })
  match result2 {
    Ok(value) => assert_eq(value, "another_success")
    Error(_) => assert_true(false)
  }
  
  // Should be closed now
  assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
  assert_eq(CircuitBreaker::failure_count(circuit_breaker), 0)
}

// Test 5: Circuit Breaker Half-Open Failure
test "circuit breaker half-open failure" {
  let circuit_breaker = CircuitBreaker::new("half_open_fail_service", 2, 100)  // 2 failures, 100ms timeout
  
  // Trip the circuit
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
  
  // Wait for timeout
  Time::sleep(150)  // 150ms > 100ms timeout
  
  // First call in half-open state succeeds
  let result1 = CircuitBreaker::execute(circuit_breaker, fn() { "success" })
  match result1 {
    Ok(value) => assert_eq(value, "success")
    Error(_) => assert_true(false)
  }
  
  assert_eq(CircuitBreaker::state(circuit_breaker), HalfOpen)
  
  // Second call in half-open state fails - should open again
  let result2 = CircuitBreaker::execute(circuit_breaker, fn() { Error("Service still unavailable") })
  match result2 {
    Ok(_) => assert_true(false)
    Error(_) => assert_true(true)
  }
  
  // Should be open again
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
}

// Test 6: Circuit Breaker with Timeouts
test "circuit breaker with timeouts" {
  let circuit_breaker = CircuitBreaker::new("timeout_service", 2, 200)  // 2 failures, 200ms timeout
  
  // Trip the circuit
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
  
  // Should still be open before timeout
  let result_before = CircuitBreaker::execute(circuit_breaker, fn() { "should_not_execute" })
  match result_before {
    Ok(_) => assert_true(false)
    Error(error) => assert_eq(error, "Circuit breaker is open")
  }
  
  // Wait for timeout
  Time::sleep(250)  // 250ms > 200ms timeout
  
  // Should transition to half-open on next call
  let result_after = CircuitBreaker::execute(circuit_breaker, fn() { "success_after_timeout" })
  match result_after {
    Ok(value) => assert_eq(value, "success_after_timeout")
    Error(_) => assert_true(false)
  }
  
  assert_eq(CircuitBreaker::state(circuit_breaker), HalfOpen)
}

// Test 7: Circuit Breaker Statistics
test "circuit breaker statistics" {
  let circuit_breaker = CircuitBreaker::new("stats_service", 3, 5000)
  
  // Initial statistics
  let stats = CircuitBreaker::get_statistics(circuit_breaker)
  assert_eq(stats.requests, 0)
  assert_eq(stats.successes, 0)
  assert_eq(stats.failures, 0)
  assert_eq(stats.timeouts, 0)
  assert_eq(stats.state_transitions, 0)
  
  // Successful operation
  CircuitBreaker::execute(circuit_breaker, fn() { "success" })
  
  let after_success = CircuitBreaker::get_statistics(circuit_breaker)
  assert_eq(after_success.requests, 1)
  assert_eq(after_success.successes, 1)
  assert_eq(after_success.failures, 0)
  
  // Failed operations
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Failure 1") })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Failure 2") })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Failure 3") })
  
  let after_failures = CircuitBreaker::get_statistics(circuit_breaker)
  assert_eq(after_failures.requests, 4)
  assert_eq(after_failures.successes, 1)
  assert_eq(after_failures.failures, 3)
  assert_eq(after_failures.state_transitions, 1)  // Closed to Open
  
  // Should be open now
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
}

// Test 8: Circuit Breaker with Custom Exception Handling
test "circuit breaker with custom exception handling" {
  let circuit_breaker = CircuitBreaker::new_with_predicate(
    "custom_service", 
    2, 
    5000,
    fn(error) { 
      match error {
        "NetworkError" => true    // Count as failure
        "BusinessError" => false  // Don't count as failure
        _ => true
      }
    }
  )
  
  // Business error should not count toward failure threshold
  let result1 = CircuitBreaker::execute(circuit_breaker, fn() { Error("BusinessError") })
  match result1 {
    Ok(_) => assert_true(false)
    Error(error) => assert_eq(error, "BusinessError")
  }
  
  // Should still be closed
  assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
  assert_eq(CircuitBreaker::failure_count(circuit_breaker), 0)
  
  // Network error should count toward failure threshold
  let result2 = CircuitBreaker::execute(circuit_breaker, fn() { Error("NetworkError") })
  match result2 {
    Ok(_) => assert_true(false)
    Error(error) => assert_eq(error, "NetworkError")
  }
  
  // Should still be closed
  assert_eq(CircuitBreaker::state(circuit_breaker), Closed)
  assert_eq(CircuitBreaker::failure_count(circuit_breaker), 1)
  
  // Another network error should trip the circuit
  let result3 = CircuitBreaker::execute(circuit_breaker, fn() { Error("NetworkError") })
  match result3 {
    Ok(_) => assert_true(false)
    Error(error) => assert_eq(error, "NetworkError")
  }
  
  // Should be open now
  assert_eq(CircuitBreaker::state(circuit_breaker), Open)
  assert_eq(CircuitBreaker::failure_count(circuit_breaker), 2)
}

// Test 9: Circuit Breaker with Telemetry Integration
test "circuit breaker with telemetry integration" {
  let circuit_breaker = CircuitBreaker::new("telemetry_service", 2, 5000)
  
  // Create a mock telemetry recorder
  let telemetry_recorder = MockTelemetryRecorder::new()
  
  // Configure circuit breaker to record telemetry
  CircuitBreaker::with_telemetry(circuit_breaker, telemetry_recorder)
  
  // Execute some operations
  CircuitBreaker::execute(circuit_breaker, fn() { "success" })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  CircuitBreaker::execute(circuit_breaker, fn() { Error("Service unavailable") })
  
  // Check telemetry events
  let events = MockTelemetryRecorder::get_events(telemetry_recorder)
  assert_eq(events.length(), 3)
  
  // First event should be success
  match events[0] {
    TelemetryEvent::CircuitBreakerSuccess(service) => assert_eq(service, "telemetry_service")
    _ => assert_true(false)
  }
  
  // Next two events should be failures
  match events[1] {
    TelemetryEvent::CircuitBreakerFailure(service) => assert_eq(service, "telemetry_service")
    _ => assert_true(false)
  }
  
  match events[2] {
    TelemetryEvent::CircuitBreakerFailure(service) => assert_eq(service, "telemetry_service")
    _ => assert_true(false)
  }
  
  // Should also have a state transition event
  match events[3] {
    TelemetryEvent::CircuitBreakerStateTransition(service, from_state, to_state) => {
      assert_eq(service, "telemetry_service")
      assert_eq(from_state, "Closed")
      assert_eq(to_state, "Open")
    }
    _ => assert_true(false)
  }
}

// Test 10: Circuit Breaker with Multiple Services
test "circuit breaker with multiple services" {
  // Create circuit breakers for different services
  let primary_cb = CircuitBreaker::new("primary_service", 2, 5000)
  let secondary_cb = CircuitBreaker::new("secondary_service", 3, 3000)
  let fallback_cb = CircuitBreaker::new("fallback_service", 1, 1000)
  
  // Trip primary service circuit breaker
  CircuitBreaker::execute(primary_cb, fn() { Error("Primary unavailable") })
  CircuitBreaker::execute(primary_cb, fn() { Error("Primary unavailable") })
  
  assert_eq(CircuitBreaker::state(primary_cb), Open)
  assert_eq(CircuitBreaker::state(secondary_cb), Closed)
  assert_eq(CircuitBreaker::state(fallback_cb), Closed)
  
  // Trip secondary service circuit breaker
  CircuitBreaker::execute(secondary_cb, fn() { Error("Secondary unavailable") })
  CircuitBreaker::execute(secondary_cb, fn() { Error("Secondary unavailable") })
  CircuitBreaker::execute(secondary_cb, fn() { Error("Secondary unavailable") })
  
  assert_eq(CircuitBreaker::state(primary_cb), Open)
  assert_eq(CircuitBreaker::state(secondary_cb), Open)
  assert_eq(CircuitBreaker::state(fallback_cb), Closed)
  
  // Trip fallback service circuit breaker
  CircuitBreaker::execute(fallback_cb, fn() { Error("Fallback unavailable") })
  
  assert_eq(CircuitBreaker::state(primary_cb), Open)
  assert_eq(CircuitBreaker::state(secondary_cb), Open)
  assert_eq(CircuitBreaker::state(fallback_cb), Open)
  
  // Test service selection based on circuit state
  let service_result = ServiceSelector::select_available_service([
    ("primary", primary_cb),
    ("secondary", secondary_cb),
    ("fallback", fallback_cb)
  ])
  
  match service_result {
    None => assert_true(true)  // All services should be unavailable
    Some(service) => assert_true(false)
  }
  
  // Wait for fallback to recover (shortest timeout)
  Time::sleep(1100)  // 1100ms > 1000ms timeout
  
  // Try fallback service again
  let fallback_result = CircuitBreaker::execute(fallback_cb, fn() { "fallback_success" })
  match fallback_result {
    Ok(value) => assert_eq(value, "fallback_success")
    Error(_) => assert_true(false)
  }
  
  assert_eq(CircuitBreaker::state(fallback_cb), HalfOpen)
  
  // Another successful call should close it
  CircuitBreaker::execute(fallback_cb, fn() { "another_fallback_success" })
  
  assert_eq(CircuitBreaker::state(fallback_cb), Closed)
  
  // Now service selection should find the fallback
  let service_result_after = ServiceSelector::select_available_service([
    ("primary", primary_cb),
    ("secondary", secondary_cb),
    ("fallback", fallback_cb)
  ])
  
  match service_result_after {
    Some(service) => assert_eq(service, "fallback")
    None => assert_true(false)
  }
}