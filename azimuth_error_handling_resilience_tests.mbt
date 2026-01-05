// Azimuth Error Handling and Resilience Test Suite
// This file contains test cases for error handling and resilience mechanisms in Azimuth

// Test 1: Error Type Definitions and Handling
test "error type definitions and handling patterns" {
  // Define error types
  enum TelemetryError {
    InvalidSpanId(String)
    TraceNotFound(String)
    SerializationError(String)
    NetworkTimeout(Int)
    DatabaseConnectionError(String)
    AuthenticationError(String)
    RateLimitExceeded(Int, Int)  // current, limit
    ResourceExhausted(String)
    ConfigurationError(String)
    ValidationError(String)
  }
  
  // Define result type
  type Result[T] = {
    success: Bool,
    data: Option[T],
    error: Option[TelemetryError]
  }
  
  // Create success result
  let create_success = fn(data: T) {
    {
      success: true,
      data: Some(data),
      error: None
    }
  }
  
  // Create error result
  let create_error = fn(error: TelemetryError) {
    {
      success: false,
      data: None,
      error: Some(error)
    }
  }
  
  // Test success result
  let success_result = create_success("span-12345")
  assert_true(success_result.success)
  assert_eq(success_result.data, Some("span-12345"))
  assert_eq(success_result.error, None)
  
  // Test error result
  let error_result = create_error(TelemetryError::InvalidSpanId("invalid-id"))
  assert_false(error_result.success)
  assert_eq(error_result.data, None)
  
  match error_result.error {
    Some(TelemetryError::InvalidSpanId(id)) => assert_eq(id, "invalid-id")
    _ => assert_true(false)
  }
  
  // Test different error types
  let timeout_error = create_error(TelemetryError::NetworkTimeout(5000))
  match timeout_error.error {
    Some(TelemetryError::NetworkTimeout(timeout)) => assert_eq(timeout, 5000)
    _ => assert_true(false)
  }
  
  let rate_limit_error = create_error(TelemetryError::RateLimitExceeded(150, 100))
  match rate_limit_error.error {
    Some(TelemetryError::RateLimitExceeded(current, limit)) => {
      assert_eq(current, 150)
      assert_eq(limit, 100)
    }
    _ => assert_true(false)
  }
}

// Test 2: Error Recovery Mechanisms
test "error recovery mechanisms with retry logic" {
  // Define operation result
  type OperationResult = {
    success: Bool,
    data: String,
    error_message: Option[String]
  }
  
  // Simulate operation that might fail
  let simulate_operation = fn(should_fail: Bool, attempt: Int) {
    if should_fail && attempt < 3 {
      {
        success: false,
        data: "",
        error_message: Some("Operation failed on attempt " + attempt.to_string())
      }
    } else {
      {
        success: true,
        data: "success-data-" + attempt.to_string(),
        error_message: None
      }
    }
  }
  
  // Retry mechanism with exponential backoff
  let retry_with_backoff = fn(operation: () -> OperationResult, max_attempts: Int) {
    let mut attempt = 1
    let mut result = operation()
    let mut backoff_time = 100  // Initial backoff in milliseconds
    
    while not(result.success) && attempt < max_attempts {
      attempt = attempt + 1
      backoff_time = backoff_time * 2  // Exponential backoff
      result = operation()
    }
    
    {
      result,
      attempts_made: attempt,
      final_backoff: backoff_time
    }
  }
  
  // Test successful retry
  let retry_result = retry_with_backoff(fn() { simulate_operation(true, 1) }, 5)
  assert_true(retry_result.result.success)
  assert_eq(retry_result.attempts_made, 3)  // Should succeed on 3rd attempt
  assert_eq(retry_result.result.data, "success-data-3")
  
  // Test immediate success
  let immediate_result = retry_with_backoff(fn() { simulate_operation(false, 1) }, 5)
  assert_true(immediate_result.result.success)
  assert_eq(immediate_result.attempts_made, 1)  // Should succeed on 1st attempt
  assert_eq(immediate_result.result.data, "success-data-1")
  
  // Test max attempts reached
  let max_attempts_result = retry_with_backoff(fn() { simulate_operation(true, 1) }, 2)
  assert_false(max_attempts_result.result.success)
  assert_eq(max_attempts_result.attempts_made, 2)  // Should reach max attempts
}

// Test 3: Circuit Breaker Pattern
test "circuit breaker pattern for service resilience" {
  // Define circuit breaker states
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // Define circuit breaker
  type CircuitBreaker = {
    state: CircuitState,
    failure_count: Int,
    failure_threshold: Int,
    success_threshold: Int,
    timeout: Int,
    last_failure_time: Int,
    current_time: Int
  }
  
  // Create circuit breaker
  let create_circuit_breaker = fn(failure_threshold: Int, success_threshold: Int, timeout: Int) {
    {
      state: CircuitState::Closed,
      failure_count: 0,
      failure_threshold,
      success_threshold,
      timeout,
      last_failure_time: 0,
      current_time: 0
    }
  }
  
  // Call through circuit breaker
  let circuit_breaker_call = fn(cb: CircuitBreaker, operation: () -> { success: Bool, data: String }) {
    let current_time = cb.current_time
    let state = cb.state
    let failure_count = cb.failure_count
    
    match state {
      CircuitState::Open => {
        // Check if timeout has passed
        if current_time - cb.last_failure_time >= cb.timeout {
          // Try to transition to half-open
          let result = operation()
          if result.success {
            // Success, transition to closed
            {
              result: Some(result),
              updated_cb: { cb | state: CircuitState::Closed, failure_count: 0 }
            }
          } else {
            // Still failing, stay open
            {
              result: None,
              updated_cb: { cb | last_failure_time: current_time }
            }
          }
        } else {
          // Still in open state
          {
            result: None,
            updated_cb: cb
          }
        }
      }
      CircuitState::HalfOpen => {
        let result = operation()
        if result.success {
          // Success, transition to closed
          {
            result: Some(result),
            updated_cb: { cb | state: CircuitState::Closed, failure_count: 0 }
          }
        } else {
          // Failure, transition back to open
          {
            result: None,
            updated_cb: { 
              cb | 
              state: CircuitState::Open, 
              failure_count: failure_count + 1,
              last_failure_time: current_time
            }
          }
        }
      }
      CircuitState::Closed => {
        let result = operation()
        if result.success {
          // Success, stay closed
          {
            result: Some(result),
            updated_cb: cb
          }
        } else {
          // Failure, check if should open
          let new_failure_count = failure_count + 1
          if new_failure_count >= cb.failure_threshold {
            // Open circuit
            {
              result: None,
              updated_cb: {
                cb |
                state: CircuitState::Open,
                failure_count: new_failure_count,
                last_failure_time: current_time
              }
            }
          } else {
            // Stay closed
            {
              result: None,
              updated_cb: { cb | failure_count: new_failure_count }
            }
          }
        }
      }
    }
  }
  
  // Create circuit breaker
  let mut circuit_breaker = create_circuit_breaker(3, 2, 5000)
  
  // Test initial closed state
  assert_eq(circuit_breaker.state, CircuitState::Closed)
  assert_eq(circuit_breaker.failure_count, 0)
  
  // Test successful operation
  let success_operation = fn() { { success: true, data: "success" } }
  let call_result1 = circuit_breaker_call(circuit_breaker, success_operation)
  assert_true(call_result1.result.is_some())
  assert_eq(call_result1.updated_cb.state, CircuitState::Closed)
  assert_eq(call_result1.updated_cb.failure_count, 0)
  circuit_breaker = call_result1.updated_cb
  
  // Test failing operations
  let fail_operation = fn() { { success: false, data: "" } }
  
  // First failure
  let call_result2 = circuit_breaker_call(circuit_breaker, fail_operation)
  assert_false(call_result2.result.is_some())
  assert_eq(call_result2.updated_cb.state, CircuitState::Closed)
  assert_eq(call_result2.updated_cb.failure_count, 1)
  circuit_breaker = call_result2.updated_cb
  
  // Second failure
  let call_result3 = circuit_breaker_call(circuit_breaker, fail_operation)
  assert_false(call_result3.result.is_some())
  assert_eq(call_result3.updated_cb.state, CircuitState::Closed)
  assert_eq(call_result3.updated_cb.failure_count, 2)
  circuit_breaker = call_result3.updated_cb
  
  // Third failure - should open circuit
  let call_result4 = circuit_breaker_call(circuit_breaker, fail_operation)
  assert_false(call_result4.result.is_some())
  assert_eq(call_result4.updated_cb.state, CircuitState::Open)
  assert_eq(call_result4.updated_cb.failure_count, 3)
  circuit_breaker = call_result4.updated_cb
  
  // Test open state - should not call operation
  let call_result5 = circuit_breaker_call(circuit_breaker, success_operation)
  assert_false(call_result5.result.is_some())
  assert_eq(call_result5.updated_cb.state, CircuitState::Open)
}

// Test 4: Bulkhead Pattern for Resource Isolation
test "bulkhead pattern for resource isolation" {
  // Define bulkhead
  type Bulkhead = {
    name: String,
    max_concurrent: Int,
    current_count: Int,
    queue_size: Int,
    queue_length: Int
  }
  
  // Create bulkhead
  let create_bulkhead = fn(name: String, max_concurrent: Int, queue_size: Int) {
    {
      name,
      max_concurrent,
      current_count: 0,
      queue_size,
      queue_length: 0
    }
  }
  
  // Try to acquire resource from bulkhead
  let bulkhead_acquire = fn(bulkhead: Bulkhead) {
    if bulkhead.current_count < bulkhead.max_concurrent {
      // Resource available
      {
        success: true,
        updated_bulkhead: { bulkhead | current_count: bulkhead.current_count + 1 }
      }
    } else if bulkhead.queue_length < bulkhead.queue_size {
      // Queue space available
      {
        success: true,
        updated_bulkhead: { bulkhead | queue_length: bulkhead.queue_length + 1 }
      }
    } else {
      // No resources or queue space available
      {
        success: false,
        updated_bulkhead: bulkhead
      }
    }
  }
  
  // Release resource from bulkhead
  let bulkhead_release = fn(bulkhead: Bulkhead) {
    if bulkhead.current_count > 0 {
      // Release active resource
      {
        updated_bulkhead: { bulkhead | current_count: bulkhead.current_count - 1 }
      }
    } else if bulkhead.queue_length > 0 {
      // Move from queue to active
      {
        updated_bulkhead: { 
          bulkhead | 
          queue_length: bulkhead.queue_length - 1,
          current_count: bulkhead.current_count + 1
        }
      }
    } else {
      // Nothing to release
      {
        updated_bulkhead: bulkhead
      }
    }
  }
  
  // Create bulkheads for different services
  let telemetry_bulkhead = create_bulkhead("telemetry", 3, 5)
  let payment_bulkhead = create_bulkhead("payment", 2, 3)
  
  // Test telemetry bulkhead
  let mut current_bulkhead = telemetry_bulkhead
  
  // Acquire resources
  let acquire1 = bulkhead_acquire(current_bulkhead)
  assert_true(acquire1.success)
  assert_eq(acquire1.updated_bulkhead.current_count, 1)
  current_bulkhead = acquire1.updated_bulkhead
  
  let acquire2 = bulkhead_acquire(current_bulkhead)
  assert_true(acquire2.success)
  assert_eq(acquire2.updated_bulkhead.current_count, 2)
  current_bulkhead = acquire2.updated_bulkhead
  
  let acquire3 = bulkhead_acquire(current_bulkhead)
  assert_true(acquire3.success)
  assert_eq(acquire3.updated_bulkhead.current_count, 3)
  current_bulkhead = acquire3.updated_bulkhead
  
  // Should now be at capacity
  let acquire4 = bulkhead_acquire(current_bulkhead)
  assert_true(acquire4.success)
  assert_eq(acquire4.updated_bulkhead.current_count, 3)
  assert_eq(acquire4.updated_bulkhead.queue_length, 1)
  current_bulkhead = acquire4.updated_bulkhead
  
  // Release resource
  let release1 = bulkhead_release(current_bulkhead)
  assert_eq(release1.updated_bulkhead.current_count, 3)  // Should move from queue
  assert_eq(release1.updated_bulkhead.queue_length, 0)
  current_bulkhead = release1.updated_bulkhead
  
  // Test payment bulkhead with smaller capacity
  let mut payment_current = payment_bulkhead
  
  let payment_acquire1 = bulkhead_acquire(payment_current)
  assert_true(payment_acquire1.success)
  assert_eq(payment_acquire1.updated_bulkhead.current_count, 1)
  payment_current = payment_acquire1.updated_bulkhead
  
  let payment_acquire2 = bulkhead_acquire(payment_current)
  assert_true(payment_acquire2.success)
  assert_eq(payment_acquire2.updated_bulkhead.current_count, 2)
  payment_current = payment_acquire2.updated_bulkhead
  
  // Should now be at capacity
  let payment_acquire3 = bulkhead_acquire(payment_current)
  assert_true(payment_acquire3.success)
  assert_eq(payment_acquire3.updated_bulkhead.current_count, 2)
  assert_eq(payment_acquire3.updated_bulkhead.queue_length, 1)
  payment_current = payment_acquire3.updated_bulkhead
}

// Test 5: Timeout Handling
test "timeout handling for long-running operations" {
  // Define operation with timeout
  let operation_with_timeout = fn(operation: () -> String, timeout_ms: Int) {
    let start_time = 0  // Simulated start time
    let operation_result = operation()
    let end_time = 100   // Simulated end time (100ms)
    let duration = end_time - start_time
    
    if duration > timeout_ms {
      {
        success: false,
        data: "",
        error: Some("Operation timed out after " + timeout_ms.to_string() + "ms")
      }
    } else {
      {
        success: true,
        data: operation_result,
        error: None
      }
    }
  }
  
  // Test fast operation
  let fast_operation = fn() { "fast-result" }
  let fast_result = operation_with_timeout(fast_operation, 200)
  assert_true(fast_result.success)
  assert_eq(fast_result.data, "fast-result")
  assert_eq(fast_result.error, None)
  
  // Test slow operation
  let slow_operation = fn() { "slow-result" }
  let slow_result = operation_with_timeout(slow_operation, 50)
  assert_false(slow_result.success)
  assert_eq(slow_result.data, "")
  assert_eq(slow_result.error, Some("Operation timed out after 50ms"))
  
  // Test operation with exact timeout
  let exact_result = operation_with_timeout(fast_operation, 100)
  assert_true(exact_result.success)
  assert_eq(exact_result.data, "fast-result")
  assert_eq(exact_result.error, None)
}

// Test 6: Graceful Degradation
test "graceful degradation when services are unavailable" {
  // Define service availability
  type ServiceStatus = {
    name: String,
    available: Bool,
    degraded: Bool,
    message: Option[String]
  }
  
  // Define service response
  type ServiceResponse = {
    success: Bool,
    data: Option[String],
    fallback_used: Bool,
    fallback_data: Option[String]
  }
  
  // Service call with fallback
  let service_call_with_fallback = fn(
    primary_service: ServiceStatus,
    fallback_service: ServiceStatus,
    primary_operation: () -> String,
    fallback_operation: () -> String
  ) {
    if primary_service.available {
      // Try primary service
      if not(primary_service.degraded) {
        // Full functionality
        {
          success: true,
          data: Some(primary_operation()),
          fallback_used: false,
          fallback_data: None
        }
      } else {
        // Degraded service, use fallback for some features
        {
          success: true,
          data: Some(primary_operation()),
          fallback_used: true,
          fallback_data: Some(fallback_operation())
        }
      }
    } else if fallback_service.available {
      // Use fallback service
      {
        success: true,
        data: Some(fallback_operation()),
        fallback_used: true,
        fallback_data: None
      }
    } else {
      // No services available
      {
        success: false,
        data: None,
        fallback_used: false,
        fallback_data: None
      }
    }
  }
  
  // Test normal operation
  let primary_normal = { name: "telemetry", available: true, degraded: false, message: None }
  let fallback_normal = { name: "cache", available: true, degraded: false, message: None }
  
  let normal_result = service_call_with_fallback(
    primary_normal,
    fallback_normal,
    fn() { "primary-data" },
    fn() { "fallback-data" }
  )
  
  assert_true(normal_result.success)
  assert_eq(normal_result.data, Some("primary-data"))
  assert_false(normal_result.fallback_used)
  
  // Test degraded service
  let primary_degraded = { name: "telemetry", available: true, degraded: true, message: Some("High load") }
  
  let degraded_result = service_call_with_fallback(
    primary_degraded,
    fallback_normal,
    fn() { "primary-data" },
    fn() { "fallback-data" }
  )
  
  assert_true(degraded_result.success)
  assert_eq(degraded_result.data, Some("primary-data"))
  assert_true(degraded_result.fallback_used)
  assert_eq(degraded_result.fallback_data, Some("fallback-data"))
  
  // Test primary unavailable
  let primary_unavailable = { name: "telemetry", available: false, degraded: false, message: Some("Service down") }
  
  let unavailable_result = service_call_with_fallback(
    primary_unavailable,
    fallback_normal,
    fn() { "primary-data" },
    fn() { "fallback-data" }
  )
  
  assert_true(unavailable_result.success)
  assert_eq(unavailable_result.data, Some("fallback-data"))
  assert_true(unavailable_result.fallback_used)
  
  // Test all services unavailable
  let fallback_unavailable = { name: "cache", available: false, degraded: false, message: Some("Service down") }
  
  let all_unavailable_result = service_call_with_fallback(
    primary_unavailable,
    fallback_unavailable,
    fn() { "primary-data" },
    fn() { "fallback-data" }
  )
  
  assert_false(all_unavailable_result.success)
  assert_eq(all_unavailable_result.data, None)
  assert_false(all_unavailable_result.fallback_used)
}

// Test 7: Error Aggregation and Reporting
test "error aggregation and reporting" {
  // Define error entry
  type ErrorEntry = {
    timestamp: Int,
    error_type: String,
    message: String,
    service: String,
    severity: String  // "low", "medium", "high", "critical"
  }
  
  // Define error report
  type ErrorReport = {
    total_errors: Int,
    errors_by_type: Array[(String, Int)],
    errors_by_service: Array[(String, Int)],
    errors_by_severity: Array[(String, Int)],
    recent_errors: Array[ErrorEntry]
  }
  
  // Aggregate errors
  let aggregate_errors = fn(errors: Array[ErrorEntry]) {
    let mut total_errors = errors.length()
    let mut errors_by_type = []
    let mut errors_by_service = []
    let mut errors_by_severity = []
    
    // Count errors by type
    for error in errors {
      let mut found_type = false
      for i in 0..errors_by_type.length() {
        if errors_by_type[i].0 == error.error_type {
          errors_by_type[i] = (errors_by_type[i].0, errors_by_type[i].1 + 1)
          found_type = true
        }
      }
      if not(found_type) {
        errors_by_type = errors_by_type.push((error.error_type, 1))
      }
      
      // Count errors by service
      let mut found_service = false
      for i in 0..errors_by_service.length() {
        if errors_by_service[i].0 == error.service {
          errors_by_service[i] = (errors_by_service[i].0, errors_by_service[i].1 + 1)
          found_service = true
        }
      }
      if not(found_service) {
        errors_by_service = errors_by_service.push((error.service, 1))
      }
      
      // Count errors by severity
      let mut found_severity = false
      for i in 0..errors_by_severity.length() {
        if errors_by_severity[i].0 == error.severity {
          errors_by_severity[i] = (errors_by_severity[i].0, errors_by_severity[i].1 + 1)
          found_severity = true
        }
      }
      if not(found_severity) {
        errors_by_severity = errors_by_severity.push((error.severity, 1))
      }
    }
    
    // Get recent errors (last 5)
    let recent_errors = if errors.length() > 5 {
      errors.slice(errors.length() - 5, errors.length())
    } else {
      errors
    }
    
    {
      total_errors,
      errors_by_type,
      errors_by_service,
      errors_by_severity,
      recent_errors
    }
  }
  
  // Create test errors
  let errors = [
    { timestamp: 1640995200, error_type: "NetworkTimeout", message: "Connection timeout", service: "telemetry", severity: "medium" },
    { timestamp: 1640995250, error_type: "DatabaseError", message: "Connection failed", service: "payment", severity: "high" },
    { timestamp: 1640995300, error_type: "ValidationError", message: "Invalid input", service: "api", severity: "low" },
    { timestamp: 1640995350, error_type: "NetworkTimeout", message: "Read timeout", service: "user", severity: "medium" },
    { timestamp: 1640995400, error_type: "AuthenticationError", message: "Invalid token", service: "auth", severity: "high" },
    { timestamp: 1640995450, error_type: "NetworkTimeout", message: "Write timeout", service: "notification", severity: "critical" },
    { timestamp: 1640995500, error_type: "ValidationError", message: "Missing field", service: "order", severity: "low" }
  ]
  
  // Aggregate errors
  let report = aggregate_errors(errors)
  
  // Test error report
  assert_eq(report.total_errors, 7)
  
  // Test errors by type
  assert_eq(report.errors_by_type.length(), 4)  // NetworkTimeout, DatabaseError, ValidationError, AuthenticationError
  assert_true(report.errors_by_type.contains(("NetworkTimeout", 3)))
  assert_true(report.errors_by_type.contains(("ValidationError", 2)))
  assert_true(report.errors_by_type.contains(("DatabaseError", 1)))
  assert_true(report.errors_by_type.contains(("AuthenticationError", 1)))
  
  // Test errors by service
  assert_eq(report.errors_by_service.length(), 7)  // All different services
  assert_true(report.errors_by_service.contains(("telemetry", 1)))
  assert_true(report.errors_by_service.contains(("payment", 1)))
  assert_true(report.errors_by_service.contains(("api", 1)))
  
  // Test errors by severity
  assert_eq(report.errors_by_severity.length(), 4)  // low, medium, high, critical
  assert_true(report.errors_by_severity.contains(("low", 2)))
  assert_true(report.errors_by_severity.contains(("medium", 2)))
  assert_true(report.errors_by_severity.contains(("high", 2)))
  assert_true(report.errors_by_severity.contains(("critical", 1)))
  
  // Test recent errors
  assert_eq(report.recent_errors.length(), 5)
  assert_eq(report.recent_errors[4].error_type, "ValidationError")
  assert_eq(report.recent_errors[4].service, "order")
}

// Test 8: Dead Letter Queue for Failed Messages
test "dead letter queue for failed telemetry messages" {
  // Define message
  type Message = {
    id: String,
    payload: String,
    timestamp: Int,
    retry_count: Int
  }
  
  // Define dead letter queue
  type DeadLetterQueue = {
    messages: Array[Message],
    max_size: Int,
    processing_errors: Array[(String, String)]  // (message_id, error_reason)
  }
  
  // Create dead letter queue
  let create_dlq = fn(max_size: Int) {
    {
      messages: [],
      max_size,
      processing_errors: []
    }
  }
  
  // Add message to dead letter queue
  let dlq_add = fn(dlq: DeadLetterQueue, message: Message, error_reason: String) {
    if dlq.messages.length() < dlq.max_size {
      {
        messages: dlq.messages.push(message),
        max_size: dlq.max_size,
        processing_errors: dlq.processing_errors.push((message.id, error_reason))
      }
    } else {
      // Queue is full, remove oldest message
      let updated_messages = dlq.messages.slice(1, dlq.messages.length())
      let updated_errors = dlq.processing_errors.slice(1, dlq.processing_errors.length())
      
      {
        messages: updated_messages.push(message),
        max_size: dlq.max_size,
        processing_errors: updated_errors.push((message.id, error_reason))
      }
    }
  }
  
  // Retry message from dead letter queue
  let dlq_retry = fn(dlq: DeadLetterQueue, message_id: String) {
    let mut found = false
    let mut updated_messages = []
    let mut updated_errors = []
    
    for i in 0..dlq.messages.length() {
      if dlq.messages[i].id == message_id {
        found = true
      } else {
        updated_messages = updated_messages.push(dlq.messages[i])
        updated_errors = updated_errors.push(dlq.processing_errors[i])
      }
    }
    
    if found {
      {
        success: true,
        updated_dlq: {
          messages: updated_messages,
          max_size: dlq.max_size,
          processing_errors: updated_errors
        }
      }
    } else {
      {
        success: false,
        updated_dlq: dlq
      }
    }
  }
  
  // Create dead letter queue
  let mut dlq = create_dlq(5)
  
  // Test empty queue
  assert_eq(dlq.messages.length(), 0)
  assert_eq(dlq.processing_errors.length(), 0)
  
  // Add messages to dead letter queue
  let message1 = { id: "msg-1", payload: "telemetry-data-1", timestamp: 1640995200, retry_count: 3 }
  let message2 = { id: "msg-2", payload: "telemetry-data-2", timestamp: 1640995250, retry_count: 2 }
  let message3 = { id: "msg-3", payload: "telemetry-data-3", timestamp: 1640995300, retry_count: 4 }
  
  dlq = dlq_add(dlq, message1, "Network timeout")
  dlq = dlq_add(dlq, message2, "Service unavailable")
  dlq = dlq_add(dlq, message3, "Data validation failed")
  
  // Test queue after adding messages
  assert_eq(dlq.messages.length(), 3)
  assert_eq(dlq.processing_errors.length(), 3)
  assert_eq(dlq.messages[0].id, "msg-1")
  assert_eq(dlq.messages[1].id, "msg-2")
  assert_eq(dlq.messages[2].id, "msg-3")
  assert_eq(dlq.processing_errors[0], ("msg-1", "Network timeout"))
  assert_eq(dlq.processing_errors[1], ("msg-2", "Service unavailable"))
  assert_eq(dlq.processing_errors[2], ("msg-3", "Data validation failed"))
  
  // Test retry message
  let retry_result = dlq_retry(dlq, "msg-2")
  assert_true(retry_result.success)
  assert_eq(retry_result.updated_dlq.messages.length(), 2)
  assert_eq(retry_result.updated_dlq.messages[0].id, "msg-1")
  assert_eq(retry_result.updated_dlq.messages[1].id, "msg-3")
  dlq = retry_result.updated_dlq
  
  // Test retry non-existent message
  let retry_missing = dlq_retry(dlq, "msg-999")
  assert_false(retry_missing.success)
  assert_eq(retry_missing.updated_dlq.messages.length(), 2)
  
  // Test queue overflow
  let message4 = { id: "msg-4", payload: "telemetry-data-4", timestamp: 1640995350, retry_count: 1 }
  let message5 = { id: "msg-5", payload: "telemetry-data-5", timestamp: 1640995400, retry_count: 2 }
  let message6 = { id: "msg-6", payload: "telemetry-data-6", timestamp: 1640995450, retry_count: 3 }
  
  dlq = dlq_add(dlq, message4, "Rate limit exceeded")
  dlq = dlq_add(dlq, message5, "Authentication failed")
  dlq = dlq_add(dlq, message6, "Server error")  // This should remove msg-1 (oldest)
  
  assert_eq(dlq.messages.length(), 5)  // Should still be at max capacity
  assert_eq(dlq.messages[0].id, "msg-3")  // msg-1 should be removed
  assert_eq(dlq.messages[4].id, "msg-6")
}

// Test 9: Health Check and Recovery
test "health check and recovery mechanisms" {
  // Define health status
  enum HealthStatus {
    Healthy
    Degraded
    Unhealthy
    Unknown
  }
  
  // Define health check result
  type HealthCheckResult = {
    service: String,
    status: HealthStatus,
    message: String,
    timestamp: Int,
    response_time: Int
  }
  
  // Define recovery action
  type RecoveryAction = {
    name: String,
    description: String,
    executed: Bool,
    success: Bool,
    timestamp: Int
  }
  
  // Perform health check
  let health_check = fn(service_name: String) {
    // Simulate health check with different scenarios
    match service_name {
      "telemetry" => {
        {
          service: service_name,
          status: HealthStatus::Healthy,
          message: "All systems operational",
          timestamp: 1640995200,
          response_time: 50
        }
      }
      "payment" => {
        {
          service: service_name,
          status: HealthStatus::Degraded,
          message: "High latency detected",
          timestamp: 1640995200,
          response_time: 500
        }
      }
      "notification" => {
        {
          service: service_name,
          status: HealthStatus::Unhealthy,
          message: "Database connection failed",
          timestamp: 1640995200,
          response_time: 5000  // Timeout
        }
      }
      _ => {
        {
          service: service_name,
          status: HealthStatus::Unknown,
          message: "Service not responding",
          timestamp: 1640995200,
          response_time: 10000  // Timeout
        }
      }
    }
  }
  
  // Execute recovery action
  let execute_recovery = fn(service_name: String, health_status: HealthStatus) {
    match health_status {
      HealthStatus::Healthy => {
        {
          name: "no_action",
          description: "No recovery needed",
          executed: false,
          success: true,
          timestamp: 1640995200
        }
      }
      HealthStatus::Degraded => {
        {
          name: "restart_service",
          description: "Restarting service to improve performance",
          executed: true,
          success: true,
          timestamp: 1640995200
        }
      }
      HealthStatus::Unhealthy => {
        {
          name: "failover",
          description: "Initiating failover to backup instance",
          executed: true,
          success: true,
          timestamp: 1640995200
        }
      }
      HealthStatus::Unknown => {
        {
          name: "investigate",
          description: "Manual investigation required",
          executed: false,
          success: false,
          timestamp: 1640995200
        }
      }
    }
  }
  
  // Test health checks
  let telemetry_health = health_check("telemetry")
  assert_eq(telemetry_health.service, "telemetry")
  assert_eq(telemetry_health.status, HealthStatus::Healthy)
  assert_eq(telemetry_health.message, "All systems operational")
  assert_eq(telemetry_health.response_time, 50)
  
  let payment_health = health_check("payment")
  assert_eq(payment_health.service, "payment")
  assert_eq(payment_health.status, HealthStatus::Degraded)
  assert_eq(payment_health.message, "High latency detected")
  assert_eq(payment_health.response_time, 500)
  
  let notification_health = health_check("notification")
  assert_eq(notification_health.service, "notification")
  assert_eq(notification_health.status, HealthStatus::Unhealthy)
  assert_eq(notification_health.message, "Database connection failed")
  assert_eq(notification_health.response_time, 5000)
  
  let unknown_health = health_check("unknown-service")
  assert_eq(unknown_health.service, "unknown-service")
  assert_eq(unknown_health.status, HealthStatus::Unknown)
  assert_eq(unknown_health.message, "Service not responding")
  assert_eq(unknown_health.response_time, 10000)
  
  // Test recovery actions
  let telemetry_recovery = execute_recovery("telemetry", telemetry_health.status)
  assert_eq(telemetry_recovery.name, "no_action")
  assert_false(telemetry_recovery.executed)
  
  let payment_recovery = execute_recovery("payment", payment_health.status)
  assert_eq(payment_recovery.name, "restart_service")
  assert_true(payment_recovery.executed)
  assert_true(payment_recovery.success)
  
  let notification_recovery = execute_recovery("notification", notification_health.status)
  assert_eq(notification_recovery.name, "failover")
  assert_true(notification_recovery.executed)
  assert_true(notification_recovery.success)
  
  let unknown_recovery = execute_recovery("unknown-service", unknown_health.status)
  assert_eq(unknown_recovery.name, "investigate")
  assert_false(unknown_recovery.executed)
  assert_false(unknown_recovery.success)
}

// Test 10: Error Context and Correlation
test "error context and correlation across services" {
  // Define error context
  type ErrorContext = {
    trace_id: String,
    span_id: String,
    service_name: String,
    operation: String,
    user_id: Option[String],
    request_id: String,
    timestamp: Int
  }
  
  // Define correlated error
  type CorrelatedError = {
    error_id: String,
    error_type: String,
    message: String,
    context: ErrorContext,
    caused_by: Option[String],  // ID of the error that caused this one
    stack_trace: Array[String]
  }
  
  // Create error context
  let create_context = fn(trace_id: String, span_id: String, service: String, operation: String) {
    {
      trace_id,
      span_id,
      service_name: service,
      operation,
      user_id: None,
      request_id: "req-" + span_id,
      timestamp: 1640995200
    }
  }
  
  // Create correlated error
  let create_error = fn(error_id: String, error_type: String, message: String, context: ErrorContext, caused_by: Option[String>) {
    {
      error_id,
      error_type,
      message,
      context,
      caused_by,
      stack_trace: [
        context.service_name + "." + context.operation + " at line 42",
        "internal.process_data at line 123",
        "external.api_call at line 67"
      ]
    }
  }
  
  // Trace error propagation
  let trace_error_propagation = fn(errors: Array[CorrelatedError], trace_id: String) {
    let mut trace_errors = []
    
    for error in errors {
      if error.context.trace_id == trace_id {
        trace_errors = trace_errors.push(error)
      }
    }
    
    trace_errors
  }
  
  // Find root cause
  let find_root_cause = fn(errors: Array[CorrelatedError]) {
    let mut root_cause = None
    
    for error in errors {
      match error.caused_by {
        None => root_cause = Some(error)
        Some(_) => ()  // Has a cause, not root
      }
    }
    
    root_cause
  }
  
  // Create error contexts
  let api_context = create_context("trace-123", "span-abc", "api-service", "process_request")
  let db_context = create_context("trace-123", "span-def", "database-service", "execute_query")
  let cache_context = create_context("trace-123", "span-ghi", "cache-service", "get_data")
  
  // Create correlated errors
  let db_error = create_error("error-1", "DatabaseError", "Connection timeout", db_context, None)
  let cache_error = create_error("error-2", "CacheError", "Cache miss", cache_context, Some("error-1"))
  let api_error = create_error("error-3", "ValidationError", "Invalid response data", api_context, Some("error-2"))
  
  let errors = [db_error, cache_error, api_error]
  
  // Test error correlation
  let trace_errors = trace_error_propagation(errors, "trace-123")
  assert_eq(trace_errors.length(), 3)
  
  // Test root cause detection
  let root_cause = find_root_cause(trace_errors)
  assert_true(root_cause.is_some())
  
  match root_cause {
    Some(error) => {
      assert_eq(error.error_id, "error-1")
      assert_eq(error.error_type, "DatabaseError")
      assert_eq(error.message, "Connection timeout")
      assert_eq(error.caused_by, None)
    }
    None => assert_true(false)
  }
  
  // Test error chain
  let api_error_detail = trace_errors[2]
  assert_eq(api_error_detail.error_id, "error-3")
  assert_eq(api_error_detail.error_type, "ValidationError")
  assert_eq(api_error_detail.caused_by, Some("error-2"))
  
  let cache_error_detail = trace_errors[1]
  assert_eq(cache_error_detail.error_id, "error-2")
  assert_eq(cache_error_detail.error_type, "CacheError")
  assert_eq(cache_error_detail.caused_by, Some("error-1"))
  
  // Test stack trace
  assert_eq(api_error_detail.stack_trace.length(), 3)
  assert_eq(api_error_detail.stack_trace[0], "api-service.process_request at line 42")
  assert_eq(api_error_detail.stack_trace[1], "internal.process_data at line 123")
  assert_eq(api_error_detail.stack_trace[2], "external.api_call at line 67")
}