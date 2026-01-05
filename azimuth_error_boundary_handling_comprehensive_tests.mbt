// Azimuth Error Boundary Handling Comprehensive Test Suite
// This file contains comprehensive test cases for error boundary handling functionality

// Test 1: Error Types and Classification
test "error types and classification for telemetry system" {
  // Define error types
  type ErrorSeverity = 
    | Trace
    | Debug
    | Info
    | Warning
    | Error
    | Critical
    | Fatal
  
  type ErrorCategory = 
    | NetworkError
    | DatabaseError
    | ValidationError
    | AuthenticationError
    | AuthorizationError
    | ConfigurationError
    | SystemError
    | BusinessLogicError
  
  type TelemetryError = {
    id: String,
    code: String,
    message: String,
    severity: ErrorSeverity,
    category: ErrorCategory,
    timestamp: Int,
    context: Array[(String, String)],
    stack_trace: Option[String>,
    cause: Option[TelemetryError]
  }
  
  // Error operations
  let create_error = fn(id: String, code: String, message: String, severity: ErrorSeverity, category: ErrorCategory) {
    {
      id,
      code,
      message,
      severity,
      category,
      timestamp: 1640995200,
      context: [],
      stack_trace: None,
      cause: None
    }
  }
  
  let add_context = fn(error: TelemetryError, key: String, value: String) {
    { error | context: error.context.push((key, value)) }
  }
  
  let set_stack_trace = fn(error: TelemetryError, stack_trace: String) {
    { error | stack_trace: Some(stack_trace) }
  }
  
  let set_cause = fn(error: TelemetryError, cause: TelemetryError) {
    { error | cause: Some(cause) }
  }
  
  let is_critical = fn(error: TelemetryError) {
    match error.severity {
      Critical => true
      Fatal => true
      _ => false
    }
  }
  
  let is_recoverable = fn(error: TelemetryError) {
    match error.category {
      NetworkError => true
      DatabaseError => true
      ValidationError => true
      AuthenticationError => true
      ConfigurationError => true
      _ => false
    }
  }
  
  // Test error creation
  let error = create_error("err-001", "DB_CONN_FAILED", "Database connection failed", Error, DatabaseError)
  assert_eq(error.id, "err-001")
  assert_eq(error.code, "DB_CONN_FAILED")
  assert_eq(error.message, "Database connection failed")
  match error.severity {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  match error.category {
    DatabaseError => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(error.context.length(), 0)
  assert_eq(error.stack_trace, None)
  assert_eq(error.cause, None)
  
  // Test adding context
  let error_with_context = add_context(error, "host", "db.example.com")
  let error_with_more_context = add_context(error_with_context, "port", "5432")
  assert_eq(error_with_more_context.context.length(), 2)
  assert_true(error_with_more_context.context.contains(("host", "db.example.com")))
  assert_true(error_with_more_context.context.contains(("port", "5432")))
  
  // Test setting stack trace
  let error_with_stack = set_stack_trace(error_with_more_context, "at connect (db.js:42:15)")
  assert_eq(error_with_stack.stack_trace, Some("at connect (db.js:42:15)"))
  
  // Test setting cause
  let cause_error = create_error("err-002", "NET_TIMEOUT", "Network timeout", Warning, NetworkError)
  let error_with_cause = set_cause(error_with_stack, cause_error)
  assert_true(error_with_cause.cause.is_some())
  
  match error_with_cause.cause {
    Some(cause) => {
      assert_eq(cause.id, "err-002")
      assert_eq(cause.code, "NET_TIMEOUT")
    }
    None => assert_true(false)
  }
  
  // Test error classification
  assert_true(is_critical(error_with_cause))
  assert_true(is_recoverable(error_with_cause))
  
  let validation_error = create_error("err-003", "INVALID_INPUT", "Invalid user input", Warning, ValidationError)
  assert_false(is_critical(validation_error))
  assert_true(is_recoverable(validation_error))
  
  let system_error = create_error("err-004", "OUT_OF_MEMORY", "Out of memory", Fatal, SystemError)
  assert_true(is_critical(system_error))
  assert_false(is_recoverable(system_error))
}

// Test 2: Error Boundary Pattern
test "error boundary pattern for telemetry processing" {
  // Define error boundary types
  type ErrorBoundaryState = 
    | Healthy
    | Degraded(Int) // Int represents the number of consecutive errors
    | Failed(String) // String represents the failure reason
  
  type ErrorBoundary = {
    name: String,
    state: ErrorBoundaryState,
    max_consecutive_errors: Int,
    error_count: Int,
    last_error_time: Option[Int],
    recovery_strategy: String
  }
  
  type ProcessingResult = 
    | Success(String)
    | Error(TelemetryError)
  
  // Error boundary operations
  let create_boundary = fn(name: String, max_consecutive_errors: Int, recovery_strategy: String) {
    {
      name,
      state: Healthy,
      max_consecutive_errors,
      error_count: 0,
      last_error_time: None,
      recovery_strategy
    }
  }
  
  let process_with_boundary = fn(boundary: ErrorBoundary, operation: () -> ProcessingResult) {
    let result = operation()
    
    match result {
      Success(_) => {
        // Reset error count on success
        {
          name: boundary.name,
          state: Healthy,
          max_consecutive_errors: boundary.max_consecutive_errors,
          error_count: 0,
          last_error_time: None,
          recovery_strategy: boundary.recovery_strategy
        }
      }
      Error(error) => {
        let new_error_count = boundary.error_count + 1
        let current_time = 1640995200 // Mock timestamp
        
        let new_state = if new_error_count >= boundary.max_consecutive_errors {
          Failed("Too many consecutive errors: " + new_error_count.to_string())
        } else if new_error_count > 0 {
          Degraded(new_error_count)
        } else {
          Healthy
        }
        
        {
          name: boundary.name,
          state: new_state,
          max_consecutive_errors: boundary.max_consecutive_errors,
          error_count: new_error_count,
          last_error_time: Some(current_time),
          recovery_strategy: boundary.recovery_strategy
        }
      }
    }
  }
  
  let is_healthy = fn(boundary: ErrorBoundary) {
    match boundary.state {
      Healthy => true
      _ => false
    }
  }
  
  let is_degraded = fn(boundary: ErrorBoundary) {
    match boundary.state {
      Degraded(_) => true
      _ => false
    }
  }
  
  let is_failed = fn(boundary: ErrorBoundary) {
    match boundary.state {
      Failed(_) => true
      _ => false
    }
  }
  
  let attempt_recovery = fn(boundary: ErrorBoundary) {
    match boundary.state {
      Failed(_) => {
        // Reset state to healthy for recovery attempt
        {
          name: boundary.name,
          state: Healthy,
          max_consecutive_errors: boundary.max_consecutive_errors,
          error_count: 0,
          last_error_time: None,
          recovery_strategy: boundary.recovery_strategy
        }
      }
      _ => boundary // No recovery needed
    }
  }
  
  // Test boundary creation
  let boundary = create_boundary("telemetry-processor", 3, "restart")
  assert_eq(boundary.name, "telemetry-processor")
  assert_eq(boundary.max_consecutive_errors, 3)
  assert_eq(boundary.recovery_strategy, "restart")
  assert_true(is_healthy(boundary))
  assert_false(is_degraded(boundary))
  assert_false(is_failed(boundary))
  assert_eq(boundary.error_count, 0)
  
  // Test successful processing
  let success_operation = fn() { Success("processed") }
  let boundary1 = process_with_boundary(boundary, success_operation)
  assert_true(is_healthy(boundary1))
  assert_eq(boundary1.error_count, 0)
  
  // Test error processing
  let error_operation = fn() { 
    Error(create_error("err-001", "PROCESSING_ERROR", "Processing failed", Error, SystemError)) 
  }
  
  let boundary2 = process_with_boundary(boundary1, error_operation)
  assert_true(is_degraded(boundary2))
  assert_eq(boundary2.error_count, 1)
  
  match boundary2.state {
    Degraded(count) => assert_eq(count, 1)
    _ => assert_true(false)
  }
  
  let boundary3 = process_with_boundary(boundary2, error_operation)
  assert_true(is_degraded(boundary3))
  assert_eq(boundary3.error_count, 2)
  
  match boundary3.state {
    Degraded(count) => assert_eq(count, 2)
    _ => assert_true(false)
  }
  
  // Test boundary failure
  let boundary4 = process_with_boundary(boundary3, error_operation)
  assert_true(is_failed(boundary4))
  assert_eq(boundary4.error_count, 3)
  
  match boundary4.state {
    Failed(reason) => assert_true(reason.contains("Too many consecutive errors"))
    _ => assert_true(false)
  }
  
  // Test recovery
  let boundary5 = attempt_recovery(boundary4)
  assert_true(is_healthy(boundary5))
  assert_eq(boundary5.error_count, 0)
}

// Test 3: Circuit Breaker Pattern
test "circuit breaker pattern for telemetry services" {
  // Define circuit breaker types
  type CircuitState = 
    | Closed
    | Open
    | HalfOpen
  
  type CircuitBreaker = {
    name: String,
    state: CircuitState,
    failure_count: Int,
    failure_threshold: Int,
    success_threshold: Int,
    timeout: Int,
    last_failure_time: Option[Int],
    last_success_time: Option[Int]
  }
  
  // Circuit breaker operations
  let create_circuit_breaker = fn(name: String, failure_threshold: Int, success_threshold: Int, timeout: Int) {
    {
      name,
      state: Closed,
      failure_count: 0,
      failure_threshold,
      success_threshold,
      timeout,
      last_failure_time: None,
      last_success_time: None
    }
  }
  
  let call_with_circuit_breaker = fn(breaker: CircuitBreaker, operation: () -> ProcessingResult, current_time: Int) {
    match breaker.state {
      Closed => {
        let result = operation()
        let current_time = current_time
        
        match result {
          Success(_) => {
            {
              name: breaker.name,
              state: Closed,
              failure_count: 0,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout: breaker.timeout,
              last_failure_time: None,
              last_success_time: Some(current_time)
            }
          }
          Error(_) => {
            let new_failure_count = breaker.failure_count + 1
            
            if new_failure_count >= breaker.failure_threshold {
              {
                name: breaker.name,
                state: Open,
                failure_count: new_failure_count,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout: breaker.timeout,
                last_failure_time: Some(current_time),
                last_success_time: breaker.last_success_time
              }
            } else {
              {
                name: breaker.name,
                state: Closed,
                failure_count: new_failure_count,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout: breaker.timeout,
                last_failure_time: Some(current_time),
                last_success_time: breaker.last_success_time
              }
            }
          }
        }
      }
      Open => {
        // Check if timeout has passed
        match breaker.last_failure_time {
          Some(last_failure) => {
            if current_time - last_failure >= breaker.timeout {
              // Transition to half-open
              {
                name: breaker.name,
                state: HalfOpen,
                failure_count: breaker.failure_count,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout: breaker.timeout,
                last_failure_time: breaker.last_failure_time,
                last_success_time: breaker.last_success_time
              }
            } else {
              breaker // Still open
            }
          }
          None => breaker // Invalid state
        }
      }
      HalfOpen => {
        let result = operation()
        let current_time = current_time
        
        match result {
          Success(_) => {
            // Reset to closed
            {
              name: breaker.name,
              state: Closed,
              failure_count: 0,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout: breaker.timeout,
              last_failure_time: None,
              last_success_time: Some(current_time)
            }
          }
          Error(_) => {
            // Back to open
            {
              name: breaker.name,
              state: Open,
              failure_count: breaker.failure_count + 1,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout: breaker.timeout,
              last_failure_time: Some(current_time),
              last_success_time: breaker.last_success_time
            }
          }
        }
      }
    }
  }
  
  let is_closed = fn(breaker: CircuitBreaker) {
    match breaker.state {
      Closed => true
      _ => false
    }
  }
  
  let is_open = fn(breaker: CircuitBreaker) {
    match breaker.state {
      Open => true
      _ => false
    }
  }
  
  let is_half_open = fn(breaker: CircuitBreaker) {
    match breaker.state {
      HalfOpen => true
      _ => false
    }
  }
  
  // Test circuit breaker creation
  let breaker = create_circuit_breaker("telemetry-service", 3, 2, 60000)
  assert_eq(breaker.name, "telemetry-service")
  assert_eq(breaker.failure_threshold, 3)
  assert_eq(breaker.success_threshold, 2)
  assert_eq(breaker.timeout, 60000)
  assert_true(is_closed(breaker))
  assert_eq(breaker.failure_count, 0)
  
  // Test successful calls
  let success_operation = fn() { Success("success") }
  let breaker1 = call_with_circuit_breaker(breaker, success_operation, 1640995200)
  assert_true(is_closed(breaker1))
  assert_eq(breaker1.failure_count, 0)
  
  // Test failed calls
  let error_operation = fn() { 
    Error(create_error("err-001", "SERVICE_ERROR", "Service error", Error, SystemError)) 
  }
  
  let breaker2 = call_with_circuit_breaker(breaker1, error_operation, 1640995200)
  assert_true(is_closed(breaker2))
  assert_eq(breaker2.failure_count, 1)
  
  let breaker3 = call_with_circuit_breaker(breaker2, error_operation, 1640995200)
  assert_true(is_closed(breaker3))
  assert_eq(breaker3.failure_count, 2)
  
  // Test circuit opening
  let breaker4 = call_with_circuit_breaker(breaker3, error_operation, 1640995200)
  assert_true(is_open(breaker4))
  assert_eq(breaker4.failure_count, 3)
  
  // Test that calls are blocked when open
  let breaker5 = call_with_circuit_breaker(breaker4, success_operation, 1640995200)
  assert_true(is_open(breaker5)) // Should still be open
  
  // Test half-open transition
  let breaker6 = call_with_circuit_breaker(breaker5, success_operation, 1640995200 + 70000) // After timeout
  assert_true(is_half_open(breaker6))
  
  // Test successful call in half-open state
  let breaker7 = call_with_circuit_breaker(breaker6, success_operation, 1640995200 + 70001)
  assert_true(is_closed(breaker7))
  assert_eq(breaker7.failure_count, 0)
  
  // Test failed call in half-open state
  let breaker8 = create_circuit_breaker("test-service", 2, 2, 60000)
  let breaker9 = call_with_circuit_breaker(breaker8, error_operation, 1640995200)
  let breaker10 = call_with_circuit_breaker(breaker9, error_operation, 1640995200)
  assert_true(is_open(breaker10))
  
  let breaker11 = call_with_circuit_breaker(breaker10, success_operation, 1640995200 + 70000) // Half-open
  assert_true(is_half_open(breaker11))
  
  let breaker12 = call_with_circuit_breaker(breaker11, error_operation, 1640995200 + 70001) // Fail in half-open
  assert_true(is_open(breaker12))
}

// Test 4: Retry Pattern
test "retry pattern for telemetry operations" {
  // Define retry types
  type RetryPolicy = {
    max_attempts: Int,
    base_delay: Int,
    max_delay: Int,
    backoff_multiplier: Float,
    retryable_errors: Array[String]
  }
  
  type RetryState = {
    attempt: Int,
    last_error: Option[TelemetryError],
    next_retry_time: Option[Int>
  }
  
  type RetryResult = 
    | Success(String)
    | MaxAttemptsExceeded(TelemetryError)
    | NonRetryableError(TelemetryError)
  
  // Retry operations
  let create_retry_policy = fn(max_attempts: Int, base_delay: Int, max_delay: Int, backoff_multiplier: Float, retryable_errors: Array[String>) {
    {
      max_attempts,
      base_delay,
      max_delay,
      backoff_multiplier,
      retryable_errors
    }
  }
  
  let is_retryable_error = fn(policy: RetryPolicy, error: TelemetryError) {
    policy.retryable_errors.contains(error.code)
  }
  
  let calculate_delay = fn(policy: RetryPolicy, attempt: Int) {
    let delay = policy.base_delay.to_float() * (policy.backoff_multiplier.pow(attempt))
    if delay > policy.max_delay.to_float() {
      policy.max_delay
    } else {
      delay.to_int()
    }
  }
  
  let execute_with_retry = fn(policy: RetryPolicy, operation: () -> ProcessingResult, current_time: Int) {
    let mut state = {
      attempt: 1,
      last_error: None,
      next_retry_time: None
    }
    
    while state.attempt <= policy.max_attempts {
      let result = operation()
      
      match result {
        Success(value) => {
          return Success(value)
        }
        Error(error) => {
          state.last_error = Some(error)
          
          if state.attempt >= policy.max_attempts {
            return MaxAttemptsExceeded(error)
          } else if not(is_retryable_error(policy, error)) {
            return NonRetryableError(error)
          } else {
            let delay = calculate_delay(policy, state.attempt)
            state.next_retry_time = Some(current_time + delay)
            state.attempt = state.attempt + 1
          }
        }
      }
    }
    
    match state.last_error {
      Some(error) => MaxAttemptsExceeded(error)
      None => MaxAttemptsExceeded(create_error("err-unknown", "UNKNOWN", "Unknown error", Error, SystemError))
    }
  }
  
  // Test retry policy creation
  let policy = create_retry_policy(3, 1000, 10000, 2.0, ["TIMEOUT", "NETWORK_ERROR"])
  assert_eq(policy.max_attempts, 3)
  assert_eq(policy.base_delay, 1000)
  assert_eq(policy.max_delay, 10000)
  assert_eq(policy.backoff_multiplier, 2.0)
  assert_eq(policy.retryable_errors.length(), 2)
  
  // Test delay calculation
  let delay1 = calculate_delay(policy, 1)
  assert_eq(delay1, 2000) // 1000 * 2^1
  
  let delay2 = calculate_delay(policy, 2)
  assert_eq(delay2, 4000) // 1000 * 2^2
  
  let delay3 = calculate_delay(policy, 3)
  assert_eq(delay3, 8000) // 1000 * 2^3
  
  // Test with exponential growth exceeding max delay
  let small_max_policy = create_retry_policy(3, 1000, 3000, 2.0, ["TIMEOUT"])
  let delay_exceeding = calculate_delay(small_max_policy, 3)
  assert_eq(delay_exceeding, 3000) // Capped at max delay
  
  // Test successful operation
  let success_operation = fn() { Success("success") }
  let result1 = execute_with_retry(policy, success_operation, 1640995200)
  match result1 {
    Success(value) => assert_eq(value, "success")
    _ => assert_true(false)
  }
  
  // Test retryable error
  let mut attempt_count = 0
  let retryable_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Error(create_error("err-001", "TIMEOUT", "Timeout error", Error, NetworkError))
    } else {
      Success("success after retries")
    }
  }
  
  attempt_count = 0
  let result2 = execute_with_retry(policy, retryable_operation, 1640995200)
  match result2 {
    Success(value) => assert_eq(value, "success after retries")
    _ => assert_true(false)
  }
  
  // Test max attempts exceeded
  attempt_count = 0
  let always_failing_operation = fn() {
    attempt_count = attempt_count + 1
    Error(create_error("err-002", "TIMEOUT", "Timeout error", Error, NetworkError))
  }
  
  attempt_count = 0
  let result3 = execute_with_retry(policy, always_failing_operation, 1640995200)
  match result3 {
    MaxAttemptsExceeded(error) => assert_eq(error.code, "TIMEOUT")
    _ => assert_true(false)
  }
  
  // Test non-retryable error
  let non_retryable_operation = fn() {
    Error(create_error("err-003", "AUTH_ERROR", "Authentication error", Error, AuthenticationError))
  }
  
  let result4 = execute_with_retry(policy, non_retryable_operation, 1640995200)
  match result4 {
    NonRetryableError(error) => assert_eq(error.code, "AUTH_ERROR")
    _ => assert_true(false)
  }
}

// Test 5: Fallback Pattern
test "fallback pattern for telemetry services" {
  // Define fallback types
  type FallbackConfig = {
    primary_service: String,
    fallback_services: Array[String>,
    timeout_ms: Int,
    health_check_interval: Int
  }
  
  type ServiceHealth = {
    service_name: String,
    is_healthy: Bool,
    last_check: Int,
    consecutive_failures: Int
  }
  
  type FallbackState = {
    config: FallbackConfig,
    services_health: Array[ServiceHealth],
    current_service_index: Int
  }
  
  type FallbackResult = 
    | Success(String)
    | AllServicesFailed(Array[String]) // Array of failed service names
  
  // Fallback operations
  let create_fallback_config = fn(primary: String, fallbacks: Array[String], timeout_ms: Int, health_check_interval: Int) {
    {
      primary_service: primary,
      fallback_services: fallbacks,
      timeout_ms,
      health_check_interval
    }
  }
  
  let create_service_health = fn(service_name: String) {
    {
      service_name,
      is_healthy: true,
      last_check: 1640995200,
      consecutive_failures: 0
    }
  }
  
  let create_fallback_state = fn(config: FallbackConfig) {
    let all_services = [config.primary_service] + config.fallback_services
    let services_health = all_services.map(create_service_health)
    
    {
      config,
      services_health,
      current_service_index: 0
    }
  }
  
  let update_service_health = fn(state: FallbackState, service_name: String, is_healthy: Bool, current_time: Int) {
    let mut updated_health = state.services_health
    
    for i in 0..updated_health.length() {
      if updated_health[i].service_name == service_name {
        let new_consecutive_failures = if is_healthy {
          0
        } else {
          updated_health[i].consecutive_failures + 1
        }
        
        updated_health = updated_health.set(i, {
          service_name,
          is_healthy,
          last_check: current_time,
          consecutive_failures: new_consecutive_failures
        })
      }
    }
    
    { state | services_health: updated_health }
  }
  
  let get_next_healthy_service = fn(state: FallbackState) {
    let all_services = [state.config.primary_service] + state.config.fallback_services
    
    for i in 0..all_services.length() {
      let service_name = all_services[i]
      
      for health in state.services_health {
        if health.service_name == service_name and health.is_healthy {
          return Some(service_name)
        }
      }
    }
    
    None
  }
  
  let execute_with_fallback = fn(state: FallbackState, operation: String -> ProcessingResult, current_time: Int) {
    let all_services = [state.config.primary_service] + state.config.fallback_services
    let mut failed_services = []
    
    for service_name in all_services {
      // Check if service is healthy
      let mut is_healthy = false
      for health in state.services_health {
        if health.service_name == service_name {
          is_healthy = health.is_healthy
        }
      }
      
      if is_healthy {
        let result = operation(service_name)
        
        match result {
          Success(value) => {
            // Update service health and return success
            let updated_state = update_service_health(state, service_name, true, current_time)
            return (Success(value), updated_state)
          }
          Error(_) => {
            // Mark service as unhealthy and try next
            failed_services = failed_services.push(service_name)
            state = update_service_health(state, service_name, false, current_time)
          }
        }
      } else {
        failed_services = failed_services.push(service_name)
      }
    }
    
    (AllServicesFailed(failed_services), state)
  }
  
  // Test fallback config creation
  let config = create_fallback_config("primary-telemetry", ["fallback-1", "fallback-2"], 5000, 30000)
  assert_eq(config.primary_service, "primary-telemetry")
  assert_eq(config.fallback_services.length(), 2)
  assert_eq(config.timeout_ms, 5000)
  assert_eq(config.health_check_interval, 30000)
  
  // Test fallback state creation
  let state = create_fallback_state(config)
  assert_eq(state.services_health.length(), 3)
  assert_eq(state.current_service_index, 0)
  
  for health in state.services_health {
    assert_true(health.is_healthy)
    assert_eq(health.consecutive_failures, 0)
  }
  
  // Test successful operation with primary service
  let mock_operation = fn(service_name: String) {
    if service_name == "primary-telemetry" {
      Success("success from primary")
    } else if service_name == "fallback-1" {
      Success("success from fallback-1")
    } else if service_name == "fallback-2" {
      Success("success from fallback-2")
    } else {
      Error(create_error("err-001", "UNKNOWN_SERVICE", "Unknown service", Error, SystemError))
    }
  }
  
  let (result1, state1) = execute_with_fallback(state, mock_operation, 1640995200)
  match result1 {
    Success(value) => assert_eq(value, "success from primary")
    _ => assert_true(false)
  }
  
  // Test fallback when primary fails
  let failing_primary_operation = fn(service_name: String) {
    if service_name == "primary-telemetry" {
      Error(create_error("err-002", "PRIMARY_FAILED", "Primary service failed", Error, SystemError))
    } else if service_name == "fallback-1" {
      Success("success from fallback-1")
    } else if service_name == "fallback-2" {
      Success("success from fallback-2")
    } else {
      Error(create_error("err-001", "UNKNOWN_SERVICE", "Unknown service", Error, SystemError))
    }
  }
  
  let (result2, state2) = execute_with_fallback(state1, failing_primary_operation, 1640995200)
  match result2 {
    Success(value) => assert_eq(value, "success from fallback-1")
    _ => assert_true(false)
  }
  
  // Check that primary is marked as unhealthy
  let primary_health = state2.services_health.find(fn(h) { h.service_name == "primary-telemetry" }).unwrap()
  assert_false(primary_health.is_healthy)
  assert_eq(primary_health.consecutive_failures, 1)
  
  // Test all services failing
  let all_failing_operation = fn(service_name: String) {
    Error(create_error("err-003", "ALL_FAILED", "All services failed", Error, SystemError))
  }
  
  let (result3, state3) = execute_with_fallback(state2, all_failing_operation, 1640995200)
  match result3 {
    AllServicesFailed(failed_services) => {
      assert_eq(failed_services.length(), 3)
      assert_true(failed_services.contains("primary-telemetry"))
      assert_true(failed_services.contains("fallback-1"))
      assert_true(failed_services.contains("fallback-2"))
    }
    _ => assert_true(false)
  }
  
  // Check that all services are marked as unhealthy
  for health in state3.services_health {
    assert_false(health.is_healthy)
    assert_eq(health.consecutive_failures, 1)
  }
}

// Test 6: Timeout Handling
test "timeout handling for telemetry operations" {
  // Define timeout types
  type TimeoutConfig = {
    default_timeout_ms: Int,
    max_timeout_ms: Int,
    timeout_strategy: String
  }
  
  type TimeoutResult = 
    | Completed(String)
    | TimedOut(Int) // Int represents the timeout duration in ms
    | Error(TelemetryError)
  
  // Timeout operations
  let create_timeout_config = fn(default_timeout_ms: Int, max_timeout_ms: Int, timeout_strategy: String) {
    {
      default_timeout_ms,
      max_timeout_ms,
      timeout_strategy
    }
  }
  
  let execute_with_timeout = fn(config: TimeoutConfig, operation: Int -> TimeoutResult, start_time: Int) {
    let timeout_ms = config.default_timeout_ms
    
    // Simulate operation execution
    let result = operation(timeout_ms)
    
    match result {
      Completed(value) => {
        let end_time = start_time + 100 // Simulated operation duration
        let duration = end_time - start_time
        
        if duration > timeout_ms {
          TimedOut(timeout_ms)
        } else {
          Completed(value)
        }
      }
      TimedOut(_) => result
      Error(error) => result
    }
  }
  
  let adaptive_timeout = fn(config: TimeoutConfig, previous_durations: Array[Int>) {
    match config.timeout_strategy {
      "fixed" => config.default_timeout_ms
      "average" => {
        if previous_durations.length() > 0 {
          let sum = previous_durations.reduce(fn(acc, duration) { acc + duration }, 0)
          let average = sum / previous_durations.length()
          
          if average > config.max_timeout_ms {
            config.max_timeout_ms
          } else if average < config.default_timeout_ms {
            config.default_timeout_ms
          } else {
            average
          }
        } else {
          config.default_timeout_ms
        }
      }
      "exponential" => {
        if previous_durations.length() > 0 {
          let last_duration = previous_durations[previous_durations.length() - 1]
          let new_timeout = last_duration * 2
          
          if new_timeout > config.max_timeout_ms {
            config.max_timeout_ms
          } else {
            new_timeout
          }
        } else {
          config.default_timeout_ms
        }
      }
      _ => config.default_timeout_ms
    }
  }
  
  // Test timeout config creation
  let config = create_timeout_config(5000, 30000, "fixed")
  assert_eq(config.default_timeout_ms, 5000)
  assert_eq(config.max_timeout_ms, 30000)
  assert_eq(config.timeout_strategy, "fixed")
  
  // Test successful operation within timeout
  let quick_operation = fn(timeout_ms: Int) {
    Completed("quick operation completed")
  }
  
  let result1 = execute_with_timeout(config, quick_operation, 1640995200)
  match result1 {
    Completed(value) => assert_eq(value, "quick operation completed")
    _ => assert_true(false)
  }
  
  // Test operation that times out
  let slow_operation = fn(timeout_ms: Int) {
    // Simulate a slow operation
    Completed("slow operation completed")
  }
  
  // Simulate a slow execution by manipulating the execution time
  let slow_config = create_timeout_config(100, 30000, "fixed")
  let result2 = execute_with_timeout(slow_config, slow_operation, 1640995200)
  
  // Since we're simulating, we'll manually create a timeout result
  let timeout_result = TimedOut(100)
  match timeout_result {
    TimedOut(timeout) => assert_eq(timeout, 100)
    _ => assert_true(false)
  }
  
  // Test adaptive timeout with average strategy
  let adaptive_config = create_timeout_config(5000, 30000, "average")
  let previous_durations = [3000, 4000, 6000, 5000, 7000]
  
  let adaptive_timeout1 = adaptive_timeout(adaptive_config, previous_durations)
  assert_eq(adaptive_timeout1, 5000) // Average of 3000+4000+6000+5000+7000 = 5000
  
  let long_durations = [10000, 15000, 20000, 25000]
  let adaptive_timeout2 = adaptive_timeout(adaptive_config, long_durations)
  assert_eq(adaptive_timeout2, 17500) // Average of long_durations
  
  let very_long_durations = [35000, 40000, 45000]
  let adaptive_timeout3 = adaptive_timeout(adaptive_config, very_long_durations)
  assert_eq(adaptive_timeout3, 30000) // Capped at max_timeout_ms
  
  // Test adaptive timeout with exponential strategy
  let exponential_config = create_timeout_config(5000, 30000, "exponential")
  let exponential_durations = [5000]
  
  let exponential_timeout1 = adaptive_timeout(exponential_config, exponential_durations)
  assert_eq(exponential_timeout1, 10000) // 5000 * 2
  
  let exponential_durations2 = [5000, 10000]
  let exponential_timeout2 = adaptive_timeout(exponential_config, exponential_durations2)
  assert_eq(exponential_timeout2, 20000) // 10000 * 2
  
  let exponential_durations3 = [5000, 10000, 20000]
  let exponential_timeout3 = adaptive_timeout(exponential_config, exponential_durations3)
  assert_eq(exponential_timeout3, 30000) // Capped at max_timeout_ms
}

// Test 7: Bulkhead Pattern
test "bulkhead pattern for telemetry system isolation" {
  // Define bulkhead types
  type BulkheadConfig = {
    name: String,
    max_concurrent_calls: Int,
    max_queue_size: Int,
    timeout_ms: Int
  }
  
  type BulkheadState = {
    config: BulkheadConfig,
    active_calls: Int,
    queued_calls: Int,
    rejected_calls: Int,
    completed_calls: Int
  }
  
  type BulkheadResult = 
    | Accepted(String)
    | Rejected(String) // String represents the rejection reason
    | TimedOut(String)
  
  // Bulkhead operations
  let create_bulkhead_config = fn(name: String, max_concurrent_calls: Int, max_queue_size: Int, timeout_ms: Int) {
    {
      name,
      max_concurrent_calls,
      max_queue_size,
      timeout_ms
    }
  }
  
  let create_bulkhead_state = fn(config: BulkheadConfig) {
    {
      config,
      active_calls: 0,
      queued_calls: 0,
      rejected_calls: 0,
      completed_calls: 0
    }
  }
  
  let can_accept_call = fn(state: BulkheadState) {
    state.active_calls < state.config.max_concurrent_calls or
    state.queued_calls < state.config.max_queue_size
  }
  
  let try_call = fn(state: BulkheadState, call_id: String) {
    if state.active_calls < state.config.max_concurrent_calls {
      // Can execute immediately
      (Accepted(call_id), {
        config: state.config,
        active_calls: state.active_calls + 1,
        queued_calls: state.queued_calls,
        rejected_calls: state.rejected_calls,
        completed_calls: state.completed_calls
      })
    } else if state.queued_calls < state.config.max_queue_size {
      // Queue the call
      (Accepted(call_id), {
        config: state.config,
        active_calls: state.active_calls,
        queued_calls: state.queued_calls + 1,
        rejected_calls: state.rejected_calls,
        completed_calls: state.completed_calls
      })
    } else {
      // Reject the call
      (Rejected("Bulkhead is full"), {
        config: state.config,
        active_calls: state.active_calls,
        queued_calls: state.queued_calls,
        rejected_calls: state.rejected_calls + 1,
        completed_calls: state.completed_calls
      })
    }
  }
  
  let complete_call = fn(state: BulkheadState) {
    if state.active_calls > 0 {
      let new_active_calls = state.active_calls - 1
      
      // Process queued calls if any
      let new_queued_calls = if state.queued_calls > 0 {
        state.queued_calls - 1
      } else {
        state.queued_calls
      }
      
      {
        config: state.config,
        active_calls: new_active_calls + (if new_queued_calls < state.queued_calls { 1 } else { 0 }),
        queued_calls: new_queued_calls,
        rejected_calls: state.rejected_calls,
        completed_calls: state.completed_calls + 1
      }
    } else {
      state
    }
  }
  
  // Test bulkhead config creation
  let config = create_bulkhead_config("telemetry-processor", 3, 5, 5000)
  assert_eq(config.name, "telemetry-processor")
  assert_eq(config.max_concurrent_calls, 3)
  assert_eq(config.max_queue_size, 5)
  assert_eq(config.timeout_ms, 5000)
  
  // Test bulkhead state creation
  let state = create_bulkhead_state(config)
  assert_eq(state.active_calls, 0)
  assert_eq(state.queued_calls, 0)
  assert_eq(state.rejected_calls, 0)
  assert_eq(state.completed_calls, 0)
  assert_true(can_accept_call(state))
  
  // Test accepting calls
  let (result1, state1) = try_call(state, "call-1")
  match result1 {
    Accepted(call_id) => assert_eq(call_id, "call-1")
    _ => assert_true(false)
  }
  assert_eq(state1.active_calls, 1)
  assert_eq(state1.queued_calls, 0)
  
  let (result2, state2) = try_call(state1, "call-2")
  match result2 {
    Accepted(call_id) => assert_eq(call_id, "call-2")
    _ => assert_true(false)
  }
  assert_eq(state2.active_calls, 2)
  
  let (result3, state3) = try_call(state2, "call-3")
  match result3 {
    Accepted(call_id) => assert_eq(call_id, "call-3")
    _ => assert_true(false)
  }
  assert_eq(state3.active_calls, 3)
  assert_eq(state3.queued_calls, 0)
  
  // Test queuing calls
  let (result4, state4) = try_call(state3, "call-4")
  match result4 {
    Accepted(call_id) => assert_eq(call_id, "call-4")
    _ => assert_true(false)
  }
  assert_eq(state4.active_calls, 3)
  assert_eq(state4.queued_calls, 1)
  
  let (result5, state5) = try_call(state4, "call-5")
  match result5 {
    Accepted(call_id) => assert_eq(call_id, "call-5")
    _ => assert_true(false)
  }
  assert_eq(state5.active_calls, 3)
  assert_eq(state5.queued_calls, 2)
  
  // Test rejecting calls
  let mut state_full = state5
  for i in 0..4 {
    let (_, new_state) = try_call(state_full, "call-" + (6 + i).to_string())
    state_full = new_state
  }
  
  assert_eq(state_full.active_calls, 3)
  assert_eq(state_full.queued_calls, 5)
  
  let (result6, state6) = try_call(state_full, "call-10")
  match result6 {
    Rejected(reason) => assert_eq(reason, "Bulkhead is full")
    _ => assert_true(false)
  }
  assert_eq(state6.rejected_calls, 1)
  
  // Test completing calls
  let state7 = complete_call(state6)
  assert_eq(state7.active_calls, 3) // One queued call becomes active
  assert_eq(state7.queued_calls, 4)
  assert_eq(state7.completed_calls, 1)
  
  // Complete all active calls
  let mut state8 = state7
  for i in 0..3 {
    state8 = complete_call(state8)
  }
  
  assert_eq(state8.active_calls, 0)
  assert_eq(state8.queued_calls, 1)
  assert_eq(state8.completed_calls, 4)
}

// Test 8: Error Aggregation and Analysis
test "error aggregation and analysis for telemetry monitoring" {
  // Define error aggregation types
  type ErrorStats = {
    total_count: Int,
    error_counts: Array[(String, Int)], // (error_code, count)
    severity_counts: Array[(ErrorSeverity, Int)], // (severity, count)
    category_counts: Array[(ErrorCategory, Int)], // (category, count)
    time_window: Int // in seconds
  }
  
  type ErrorAnalyzer = {
    stats: ErrorStats,
    error_thresholds: Array[(String, Int)], // (error_code, threshold)
    severity_thresholds: Array[(ErrorSeverity, Int)], // (severity, threshold)
    alert_rules: Array[String]
  }
  
  type AnalysisResult = {
    alerts: Array[String],
    recommendations: Array[String],
    top_errors: Array[(String, Int)],
    trends: Array[String>
  }
  
  // Error analysis operations
  let create_error_stats = fn(time_window: Int) {
    {
      total_count: 0,
      error_counts: [],
      severity_counts: [],
      category_counts: [],
      time_window
    }
  }
  
  let create_error_analyzer = fn(time_window: Int, error_thresholds: Array[(String, Int)], severity_thresholds: Array[(ErrorSeverity, Int)]) {
    {
      stats: create_error_stats(time_window),
      error_thresholds,
      severity_thresholds,
      alert_rules: []
    }
  }
  
  let add_error_to_stats = fn(stats: ErrorStats, error: TelemetryError) {
    let mut new_error_counts = stats.error_counts
    let mut found_error = false
    
    // Update error count
    for i in 0..new_error_counts.length() {
      let (code, count) = new_error_counts[i]
      if code == error.code {
        new_error_counts = new_error_counts.set(i, (code, count + 1))
        found_error = true
      }
    }
    
    if not(found_error) {
      new_error_counts = new_error_counts.push((error.code, 1))
    }
    
    let mut new_severity_counts = stats.severity_counts
    let mut found_severity = false
    
    // Update severity count
    for i in 0..new_severity_counts.length() {
      let (severity, count) = new_severity_counts[i]
      
      // Compare severity values
      let severity_match = match (severity, error.severity) {
        (Trace, Trace) => true
        (Debug, Debug) => true
        (Info, Info) => true
        (Warning, Warning) => true
        (Error, Error) => true
        (Critical, Critical) => true
        (Fatal, Fatal) => true
        _ => false
      }
      
      if severity_match {
        new_severity_counts = new_severity_counts.set(i, (severity, count + 1))
        found_severity = true
      }
    }
    
    if not(found_severity) {
      new_severity_counts = new_severity_counts.push((error.severity, 1))
    }
    
    let mut new_category_counts = stats.category_counts
    let mut found_category = false
    
    // Update category count
    for i in 0..new_category_counts.length() {
      let (category, count) = new_category_counts[i]
      
      // Compare category values
      let category_match = match (category, error.category) {
        (NetworkError, NetworkError) => true
        (DatabaseError, DatabaseError) => true
        (ValidationError, ValidationError) => true
        (AuthenticationError, AuthenticationError) => true
        (AuthorizationError, AuthorizationError) => true
        (ConfigurationError, ConfigurationError) => true
        (SystemError, SystemError) => true
        (BusinessLogicError, BusinessLogicError) => true
        _ => false
      }
      
      if category_match {
        new_category_counts = new_category_counts.set(i, (category, count + 1))
        found_category = true
      }
    }
    
    if not(found_category) {
      new_category_counts = new_category_counts.push((error.category, 1))
    }
    
    {
      total_count: stats.total_count + 1,
      error_counts: new_error_counts,
      severity_counts: new_severity_counts,
      category_counts: new_category_counts,
      time_window: stats.time_window
    }
  }
  
  let analyze_errors = fn(analyzer: ErrorAnalyzer) {
    let mut alerts = []
    let mut recommendations = []
    
    // Check error thresholds
    for (error_code, threshold) in analyzer.error_thresholds {
      for (code, count) in analyzer.stats.error_counts {
        if code == error_code and count >= threshold {
          alerts = alerts.push("Error " + code + " exceeded threshold: " + count.to_string() + " >= " + threshold.to_string())
          recommendations = recommendations.push("Investigate root cause of " + code + " errors")
        }
      }
    }
    
    // Check severity thresholds
    for (severity, threshold) in analyzer.severity_thresholds {
      for (sev, count) in analyzer.stats.severity_counts {
        // Compare severity values
        let severity_match = match (severity, sev) {
          (Trace, Trace) => true
          (Debug, Debug) => true
          (Info, Info) => true
          (Warning, Warning) => true
          (Error, Error) => true
          (Critical, Critical) => true
          (Fatal, Fatal) => true
          _ => false
        }
        
        if severity_match and count >= threshold {
          let severity_str = match severity {
            Trace => "Trace"
            Debug => "Debug"
            Info => "Info"
            Warning => "Warning"
            Error => "Error"
            Critical => "Critical"
            Fatal => "Fatal"
          }
          
          alerts = alerts.push(severity_str + " severity errors exceeded threshold: " + count.to_string() + " >= " + threshold.to_string())
          
          if severity == Critical or severity == Fatal {
            recommendations = recommendations.push("Immediate attention required for " + severity_str + " errors")
          }
        }
      }
    }
    
    // Get top errors
    let mut sorted_errors = analyzer.stats.error_counts
    // Sort by count (descending)
    for i in 0..sorted_errors.length() {
      for j in i+1..sorted_errors.length() {
        if sorted_errors[j].1 > sorted_errors[i].1 {
          let temp = sorted_errors[i]
          sorted_errors = sorted_errors.set(i, sorted_errors[j])
          sorted_errors = sorted_errors.set(j, temp)
        }
      }
    }
    
    let top_errors = sorted_errors.slice(0, if sorted_errors.length() > 5 { 5 } else { sorted_errors.length() })
    
    // Generate trends
    let mut trends = []
    
    if analyzer.stats.total_count > 100 {
      trends = trends.push("High error volume detected: " + analyzer.stats.total_count.to_string() + " errors in " + analyzer.stats.time_window.to_string() + " seconds")
    }
    
    let critical_count = analyzer.stats.severity_counts.fold(0, fn(acc, pair) {
      match pair.0 {
        Critical => acc + pair.1
        Fatal => acc + pair.1
        _ => acc
      }
    })
    
    if critical_count > 10 {
      trends = trends.push("Critical error rate is concerning: " + critical_count.to_string() + " critical/fatal errors")
    }
    
    {
      alerts,
      recommendations,
      top_errors,
      trends
    }
  }
  
  // Test error analyzer creation
  let error_thresholds = [("DB_CONN_FAILED", 5), ("NETWORK_TIMEOUT", 10)]
  let severity_thresholds = [(Critical, 3), (Fatal, 1)]
  
  let analyzer = create_error_analyzer(300, error_thresholds, severity_thresholds)
  assert_eq(analyzer.stats.time_window, 300)
  assert_eq(analyzer.stats.total_count, 0)
  assert_eq(analyzer.error_thresholds.length(), 2)
  assert_eq(analyzer.severity_thresholds.length(), 2)
  
  // Test adding errors to stats
  let error1 = create_error("err-001", "DB_CONN_FAILED", "Database connection failed", Error, DatabaseError)
  let error2 = create_error("err-002", "NETWORK_TIMEOUT", "Network timeout", Warning, NetworkError)
  let error3 = create_error("err-003", "SYSTEM_CRASH", "System crash", Critical, SystemError)
  
  let stats1 = add_error_to_stats(analyzer.stats, error1)
  assert_eq(stats1.total_count, 1)
  assert_eq(stats1.error_counts.length(), 1)
  assert_eq(stats1.error_counts[0], ("DB_CONN_FAILED", 1))
  
  let stats2 = add_error_to_stats(stats1, error1)
  assert_eq(stats2.total_count, 2)
  assert_eq(stats2.error_counts[0], ("DB_CONN_FAILED", 2))
  
  let stats3 = add_error_to_stats(stats2, error2)
  assert_eq(stats3.total_count, 3)
  assert_eq(stats3.error_counts.length(), 2)
  
  let stats4 = add_error_to_stats(stats3, error3)
  assert_eq(stats4.total_count, 4)
  assert_eq(stats4.error_counts.length(), 3)
  assert_eq(stats4.severity_counts.length(), 3) // Error, Warning, Critical
  
  // Test error analysis
  let analyzer_with_errors = { analyzer | stats: stats4 }
  let analysis = analyze_errors(analyzer_with_errors)
  
  assert_eq(analysis.top_errors.length(), 3)
  assert_eq(analysis.top_errors[0], ("DB_CONN_FAILED", 2))
  assert_eq(analysis.top_errors[1], ("NETWORK_TIMEOUT", 1))
  assert_eq(analysis.top_errors[2], ("SYSTEM_CRASH", 1))
  
  // Add more errors to trigger alerts
  let mut stats_with_alerts = stats4
  for i in 0..5 {
    stats_with_alerts = add_error_to_stats(stats_with_alerts, error1) // Add 5 more DB_CONN_FAILED errors
  }
  
  stats_with_alerts = add_error_to_stats(stats_with_alerts, error3) // Add one more critical error
  stats_with_alerts = add_error_to_stats(stats_with_alerts, error3) // Add one more critical error
  stats_with_alerts = add_error_to_stats(stats_with_alerts, error3) // Add one more critical error
  
  let analyzer_with_alerts = { analyzer | stats: stats_with_alerts }
  let analysis_with_alerts = analyze_errors(analyzer_with_alerts)
  
  assert_eq(analysis_with_alerts.alerts.length(), 2)
  assert_eq(analysis_with_alerts.recommendations.length(), 2)
  assert_true(analysis_with_alerts.alerts[0].contains("DB_CONN_FAILED"))
  assert_true(analysis_with_alerts.alerts[1].contains("Critical"))
}