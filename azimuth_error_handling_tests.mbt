// Azimuth Error Handling and Recovery Tests
// This file contains test cases for error handling and recovery functionality

// Test 1: Error Types and Classification
test "error types and classification" {
  // Define error types
  enum TelemetryError {
    NetworkError(String)
    SerializationError(String)
    ValidationError(String)
    TimeoutError(Int)
    ResourceExhaustedError(String)
    AuthenticationError(String)
    AuthorizationError(String)
    InternalError(String)
  }
  
  // Define error severity levels
  enum ErrorSeverity {
    Low
    Medium
    High
    Critical
  }
  
  // Define error structure
  type Error = {
    error_type: TelemetryError,
    severity: ErrorSeverity,
    message: String,
    timestamp: Int,
    context: Array[(String, String)]
  }
  
  // Create error classification function
  let classify_error = fn(error: TelemetryError) {
    match error {
      TelemetryError::NetworkError(_) => ErrorSeverity::Medium
      TelemetryError::SerializationError(_) => ErrorSeverity::Low
      TelemetryError::ValidationError(_) => ErrorSeverity::Medium
      TelemetryError::TimeoutError(_) => ErrorSeverity::High
      TelemetryError::ResourceExhaustedError(_) => ErrorSeverity::High
      TelemetryError::AuthenticationError(_) => ErrorSeverity::High
      TelemetryError::AuthorizationError(_) => ErrorSeverity::High
      TelemetryError::InternalError(_) => ErrorSeverity::Critical
    }
  }
  
  // Create error from type and message
  let create_error = fn(error_type: TelemetryError, message: String, context: Array[(String, String)]) {
    {
      error_type,
      severity: classify_error(error_type),
      message,
      timestamp: 1640995200,
      context
    }
  }
  
  // Test error creation and classification
  let network_error = create_error(
    TelemetryError::NetworkError("Connection refused"),
    "Failed to connect to telemetry collector",
    [("service", "user-service"), ("endpoint", "http://collector:9411/api/v2/spans")]
  )
  
  assert_eq(network_error.message, "Failed to connect to telemetry collector")
  assert_eq(network_error.severity, ErrorSeverity::Medium)
  assert_eq(network_error.context.length(), 2)
  
  match network_error.error_type {
    TelemetryError::NetworkError(msg) => assert_eq(msg, "Connection refused")
    _ => assert_true(false)
  }
  
  // Test timeout error
  let timeout_error = create_error(
    TelemetryError::TimeoutError(5000),
    "Operation timed out after 5000ms",
    [("operation", "database.query"), ("table", "users")]
  )
  
  assert_eq(timeout_error.severity, ErrorSeverity::High)
  
  match timeout_error.error_type {
    TelemetryError::TimeoutError(timeout) => assert_eq(timeout, 5000)
    _ => assert_true(false)
  }
  
  // Test internal error
  let internal_error = create_error(
    TelemetryError::InternalError("Null pointer exception"),
    "Unexpected internal error occurred",
    [("component", "trace.aggregator"), ("version", "1.2.3")]
  )
  
  assert_eq(internal_error.severity, ErrorSeverity::Critical)
  
  // Test error filtering by severity
  let filter_by_severity = fn(errors: Array[Error], severity: ErrorSeverity) {
    errors.filter(fn(e) { e.severity == severity })
  }
  
  let errors = [network_error, timeout_error, internal_error]
  
  let critical_errors = filter_by_severity(errors, ErrorSeverity::Critical)
  let high_errors = filter_by_severity(errors, ErrorSeverity::High)
  let medium_errors = filter_by_severity(errors, ErrorSeverity::Medium)
  
  assert_eq(critical_errors.length(), 1)
  assert_eq(high_errors.length(), 1)
  assert_eq(medium_errors.length(), 1)
}

// Test 2: Error Recovery Strategies
test "error recovery strategies" {
  // Define operation result type
  type Result[T] = 
    Success(T)
    | Failure(String)
  
  // Define retry configuration
  type RetryConfig = {
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float
  }
  
  // Define circuit breaker state
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
    recovery_timeout_ms: Int,
    last_failure_time: Option[Int]
  }
  
  // Create retry configuration
  let default_retry_config = {
    max_attempts: 3,
    base_delay_ms: 100,
    max_delay_ms: 1000,
    backoff_multiplier: 2.0
  }
  
  // Calculate retry delay with exponential backoff
  let calculate_delay = fn(attempt: Int, config: RetryConfig) {
    let delay = config.base_delay_ms.to_float() * 
      config.backoff_multiplier.pow((attempt - 1).to_float())
    delay.min(config.max_delay_ms.to_float()).to_int()
  }
  
  // Execute operation with retry
  let execute_with_retry = fn[T](
    operation: () -> Result[T],
    config: RetryConfig
  ) {
    let mut attempt = 1
    let mut last_result = Failure("No attempts made")
    
    while attempt <= config.max_attempts {
      last_result = operation()
      
      match last_result {
        Success(_) => return last_result
        Failure(_) => {
          if attempt < config.max_attempts {
            let delay = calculate_delay(attempt, config)
            // In a real implementation, we would wait for the delay here
            attempt = attempt + 1
          } else {
            return last_result
          }
        }
      }
    }
    
    last_result
  }
  
  // Test exponential backoff calculation
  assert_eq(calculate_delay(1, default_retry_config), 100)
  assert_eq(calculate_delay(2, default_retry_config), 200)
  assert_eq(calculate_delay(3, default_retry_config), 400)
  assert_eq(calculate_delay(4, default_retry_config), 800) // Capped at max_delay_ms
  
  // Test retry with successful operation
  let success_operation = fn() { Success("operation succeeded") }
  let retry_result = execute_with_retry(success_operation, default_retry_config)
  
  match retry_result {
    Success(value) => assert_eq(value, "operation succeeded")
    Failure(_) => assert_true(false)
  }
  
  // Test retry with failing operation
  let mut attempt_count = 0
  let failing_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Failure("Operation failed")
    } else {
      Success("operation succeeded after retries")
    }
  }
  
  attempt_count = 0  // Reset counter
  let retry_success_result = execute_with_retry(failing_operation, default_retry_config)
  
  match retry_success_result {
    Success(value) => assert_eq(value, "operation succeeded after retries")
    Failure(_) => assert_true(false)
  }
  assert_eq(attempt_count, 3)
  
  // Test circuit breaker
  let create_circuit_breaker = fn(threshold: Int, timeout_ms: Int) {
    {
      state: CircuitState::Closed,
      failure_count: 0,
      failure_threshold: threshold,
      recovery_timeout_ms: timeout_ms,
      last_failure_time: None
    }
  }
  
  let check_circuit_state = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Open => {
        match breaker.last_failure_time {
          Some(last_time) => {
            if current_time - last_time > breaker.recovery_timeout_ms {
              CircuitState::HalfOpen
            } else {
              CircuitState::Open
            }
          }
          None => CircuitState::Open
        }
      }
      _ => breaker.state
    }
  }
  
  let update_circuit_breaker = fn(breaker: CircuitBreaker, success: Bool, current_time: Int) {
    let new_state = check_circuit_state(breaker, current_time)
    
    match new_state {
      CircuitState::Closed => {
        if success {
          { breaker | failure_count: 0 }
        } else {
          let new_failure_count = breaker.failure_count + 1
          let new_state = if new_failure_count >= breaker.failure_threshold {
            CircuitState::Open
          } else {
            CircuitState::Closed
          }
          
          {
            state: new_state,
            failure_count: new_failure_count,
            failure_threshold: breaker.failure_threshold,
            recovery_timeout_ms: breaker.recovery_timeout_ms,
            last_failure_time: if new_state == CircuitState::Open { Some(current_time) } else { breaker.last_failure_time }
          }
        }
      }
      CircuitState::Open => breaker
      CircuitState::HalfOpen => {
        if success {
          { breaker | state: CircuitState::Closed, failure_count: 0 }
        } else {
          { breaker | state: CircuitState::Open, last_failure_time: Some(current_time) }
        }
      }
    }
  }
  
  // Test circuit breaker behavior
  let breaker = create_circuit_breaker(3, 5000)
  assert_eq(breaker.state, CircuitState::Closed)
  
  // Simulate failures
  let breaker_after_fail_1 = update_circuit_breaker(breaker, false, 1640995200)
  assert_eq(breaker_after_fail_1.state, CircuitState::Closed)
  assert_eq(breaker_after_fail_1.failure_count, 1)
  
  let breaker_after_fail_2 = update_circuit_breaker(breaker_after_fail_1, false, 1640995201)
  assert_eq(breaker_after_fail_2.state, CircuitState::Closed)
  assert_eq(breaker_after_fail_2.failure_count, 2)
  
  // Third failure should open the circuit
  let breaker_after_fail_3 = update_circuit_breaker(breaker_after_fail_2, false, 1640995202)
  assert_eq(breaker_after_fail_3.state, CircuitState::Open)
  assert_eq(breaker_after_fail_3.failure_count, 3)
  assert_eq(breaker_after_fail_3.last_failure_time, Some(1640995202))
  
  // Circuit should remain open during recovery timeout
  let breaker_during_timeout = update_circuit_breaker(breaker_after_fail_3, true, 1640995203)
  assert_eq(breaker_during_timeout.state, CircuitState::Open)
  
  // After recovery timeout, circuit should be half-open
  let breaker_after_timeout = update_circuit_breaker(breaker_during_timeout, true, 1640997203) // 2000 seconds later
  assert_eq(breaker_after_timeout.state, CircuitState::Closed) // Success in half-open state closes circuit
  
  // Test failure in half-open state
  let breaker_half_open = { breaker_after_fail_3 | state: CircuitState::HalfOpen }
  let breaker_failed_half_open = update_circuit_breaker(breaker_half_open, false, 1640995203)
  assert_eq(breaker_failed_half_open.state, CircuitState::Open)
}

// Test 3: Error Context and Propagation
test "error context and propagation" {
  // Define error context
  type ErrorContext = {
    operation: String,
    service: String,
    component: String,
    trace_id: Option[String],
    span_id: Option[String],
    user_id: Option[String],
    request_id: Option[String],
    additional_data: Array[(String, String)]
  }
  
  // Define enriched error
  type EnrichedError = {
    error_code: String,
    message: String,
    cause: Option[String],
    context: ErrorContext,
    timestamp: Int,
    stack_trace: Array[String]
  }
  
  // Create error context
  let create_context = fn(
    operation: String,
    service: String,
    component: String,
    trace_id: Option[String],
    span_id: Option[String],
    user_id: Option[String],
    request_id: Option[String],
    additional_data: Array[(String, String)]
  ) {
    {
      operation,
      service,
      component,
      trace_id,
      span_id,
      user_id,
      request_id,
      additional_data
    }
  }
  
  // Create enriched error
  let create_enriched_error = fn(
    error_code: String,
    message: String,
    cause: Option[String],
    context: ErrorContext,
    stack_trace: Array[String]
  ) {
    {
      error_code,
      message,
      cause,
      context,
      timestamp: 1640995200,
      stack_trace
    }
  }
  
  // Propagate error with additional context
  let propagate_error = fn(
    error: EnrichedError,
    new_operation: String,
    new_component: String,
    additional_context: Array[(String, String)]
  ) {
    let updated_context = {
      operation: new_operation,
      service: error.context.service,
      component: new_component,
      trace_id: error.context.trace_id,
      span_id: error.context.span_id,
      user_id: error.context.user_id,
      request_id: error.context.request_id,
      additional_data: error.context.additional_data + additional_context
    }
    
    {
      error_code: error.error_code,
      message: error.message,
      cause: Some(error.message),
      context: updated_context,
      timestamp: 1640995201,
      stack_trace: error.stack_trace.push(new_component + "." + new_operation)
    }
  }
  
  // Create initial error context
  let initial_context = create_context(
    "validate_token",
    "auth-service",
    "jwt_validator",
    Some("trace-123"),
    Some("span-456"),
    Some("user-789"),
    Some("request-001"),
    [("token_type", "bearer"), ("algorithm", "RS256")]
  )
  
  // Create initial error
  let initial_error = create_enriched_error(
    "AUTH_001",
    "Invalid JWT token signature",
    None,
    initial_context,
    ["jwt_validator.validate_token", "crypto.verify_signature"]
  )
  
  // Test initial error
  assert_eq(initial_error.error_code, "AUTH_001")
  assert_eq(initial_error.message, "Invalid JWT token signature")
  assert_eq(initial_error.context.operation, "validate_token")
  assert_eq(initial_error.context.service, "auth-service")
  assert_eq(initial_error.context.component, "jwt_validator")
  assert_eq(initial_error.context.trace_id, Some("trace-123"))
  assert_eq(initial_error.context.span_id, Some("span-456"))
  assert_eq(initial_error.context.user_id, Some("user-789"))
  assert_eq(initial_error.context.request_id, Some("request-001"))
  assert_eq(initial_error.context.additional_data.length(), 2)
  assert_eq(initial_error.stack_trace.length(), 2)
  
  // Propagate error to another component
  let propagated_error = propagate_error(
    initial_error,
    "authenticate_user",
    "auth_handler",
    [("endpoint", "/api/auth/login"), ("method", "POST")]
  )
  
  // Test propagated error
  assert_eq(propagated_error.error_code, "AUTH_001")
  assert_eq(propagated_error.message, "Invalid JWT token signature")
  assert_eq(propagated_error.cause, Some("Invalid JWT token signature"))
  assert_eq(propagated_error.context.operation, "authenticate_user")
  assert_eq(propagated_error.context.service, "auth-service")
  assert_eq(propagated_error.context.component, "auth_handler")
  assert_eq(propagated_error.context.trace_id, Some("trace-123"))
  assert_eq(propagated_error.context.span_id, Some("span-456"))
  assert_eq(propagated_error.context.user_id, Some("user-789"))
  assert_eq(propagated_error.context.request_id, Some("request-001"))
  assert_eq(propagated_error.context.additional_data.length(), 4) // 2 original + 2 new
  assert_eq(propagated_error.stack_trace.length(), 3) // 2 original + 1 new
  
  // Verify additional context is preserved
  assert_true(propagated_error.context.additional_data.contains(("token_type", "bearer")))
  assert_true(propagated_error.context.additional_data.contains(("algorithm", "RS256")))
  assert_true(propagated_error.context.additional_data.contains(("endpoint", "/api/auth/login")))
  assert_true(propagated_error.context.additional_data.contains(("method", "POST")))
  
  // Propagate error again
  let double_propagated = propagate_error(
    propagated_error,
    "process_login",
    "login_controller",
    [("controller", "auth"), ("version", "v1")]
  )
  
  // Test double propagated error
  assert_eq(double_propagated.context.operation, "process_login")
  assert_eq(double_propagated.context.component, "login_controller")
  assert_eq(double_propagated.context.additional_data.length(), 6) // 4 previous + 2 new
  assert_eq(double_propagated.stack_trace.length(), 4) // 3 previous + 1 new
  assert_eq(double_propagated.cause, Some("Invalid JWT token signature"))
}

// Test 4: Error Aggregation and Analysis
test "error aggregation and analysis" {
  // Define error occurrence
  type ErrorOccurrence = {
    error_code: String,
    service: String,
    component: String,
    timestamp: Int,
    context: Array[(String, String)]
  }
  
  // Define error statistics
  type ErrorStats = {
    error_code: String,
    total_count: Int,
    services: Array[(String, Int)],
    components: Array[(String, Int)],
    first_occurrence: Int,
    last_occurrence: Int,
    error_rate: Float
  }
  
  // Create sample error occurrences
  let error_occurrences = [
    { error_code: "NET_001", service: "user-service", component: "http_client", timestamp: 1640995200, context: [("endpoint", "/api/auth")] },
    { error_code: "NET_001", service: "user-service", component: "http_client", timestamp: 1640995210, context: [("endpoint", "/api/auth")] },
    { error_code: "DB_001", service: "user-service", component: "database", timestamp: 1640995220, context: [("query", "SELECT * FROM users")] },
    { error_code: "NET_001", service: "order-service", component: "http_client", timestamp: 1640995230, context: [("endpoint", "/api/payments")] },
    { error_code: "DB_001", service: "order-service", component: "database", timestamp: 1640995240, context: [("query", "INSERT INTO orders")] },
    { error_code: "AUTH_001", service: "auth-service", component: "jwt_validator", timestamp: 1640995250, context: [("token_type", "bearer")] },
    { error_code: "NET_001", service: "user-service", component: "http_client", timestamp: 1640995260, context: [("endpoint", "/api/auth")] },
    { error_code: "DB_001", service: "user-service", component: "database", timestamp: 1640995270, context: [("query", "SELECT * FROM users")] }
  ]
  
  // Group errors by error code
  let group_by_error_code = fn(occurrences: Array[ErrorOccurrence>) {
    let mut result = []
    let mut processed = []
    
    for occurrence in occurrences {
      if not(processed.contains(occurrence.error_code)) {
        let same_code_occurrences = occurrences.filter(fn(o) { o.error_code == occurrence.error_code })
        result = result.push((occurrence.error_code, same_code_occurrences))
        processed = processed.push(occurrence.error_code)
      }
    }
    result
  }
  
  // Calculate error statistics
  let calculate_error_stats = fn(error_code: String, occurrences: Array[ErrorOccurrence], total_operations: Int) {
    let total_count = occurrences.length()
    
    // Group by service
    let mut services = []
    let mut processed_services = []
    
    for occurrence in occurrences {
      if not(processed_services.contains(occurrence.service)) {
        let service_occurrences = occurrences.filter(fn(o) { o.service == occurrence.service })
        let service_count = service_occurrences.length()
        services = services.push((occurrence.service, service_count))
        processed_services = processed_services.push(occurrence.service)
      }
    }
    
    // Group by component
    let mut components = []
    let mut processed_components = []
    
    for occurrence in occurrences {
      if not(processed_components.contains(occurrence.component)) {
        let component_occurrences = occurrences.filter(fn(o) { o.component == occurrence.component })
        let component_count = component_occurrences.length()
        components = components.push((occurrence.component, component_count))
        processed_components = processed_components.push(occurrence.component)
      }
    }
    
    // Find first and last occurrence
    let timestamps = occurrences.map(fn(o) { o.timestamp })
    let first_occurrence = timestamps.reduce(fn(min, t) { if t < min { t } else { min } }, timestamps[0])
    let last_occurrence = timestamps.reduce(fn(max, t) { if t > max { t } else { max } }, timestamps[0])
    
    // Calculate error rate
    let error_rate = total_count.to_float() / total_operations.to_float()
    
    {
      error_code,
      total_count,
      services,
      components,
      first_occurrence,
      last_occurrence,
      error_rate
    }
  }
  
  // Test grouping by error code
  let error_groups = group_by_error_code(error_occurrences)
  assert_eq(error_groups.length(), 3)
  
  let net_errors = error_groups.filter(fn(g) { g.0 == "NET_001" })[0].1
  let db_errors = error_groups.filter(fn(g) { g.0 == "DB_001" })[0].1
  let auth_errors = error_groups.filter(fn(g) { g.0 == "AUTH_001" })[0].1
  
  assert_eq(net_errors.length(), 4)
  assert_eq(db_errors.length(), 3)
  assert_eq(auth_errors.length(), 1)
  
  // Test error statistics calculation
  let total_operations = 1000  // Assume 1000 total operations in the time window
  
  let net_stats = calculate_error_stats("NET_001", net_errors, total_operations)
  let db_stats = calculate_error_stats("DB_001", db_errors, total_operations)
  let auth_stats = calculate_error_stats("AUTH_001", auth_errors, total_operations)
  
  // Test NET_001 statistics
  assert_eq(net_stats.error_code, "NET_001")
  assert_eq(net_stats.total_count, 4)
  assert_eq(net_stats.services.length(), 2)  // user-service, order-service
  assert_eq(net_stats.components.length(), 1)  // http_client
  assert_eq(net_stats.first_occurrence, 1640995200)
  assert_eq(net_stats.last_occurrence, 1640995260)
  assert_eq(net_stats.error_rate, 0.004)  // 4 / 1000
  
  // Test DB_001 statistics
  assert_eq(db_stats.error_code, "DB_001")
  assert_eq(db_stats.total_count, 3)
  assert_eq(db_stats.services.length(), 2)  // user-service, order-service
  assert_eq(db_stats.components.length(), 1)  // database
  assert_eq(db_stats.first_occurrence, 1640995220)
  assert_eq(db_stats.last_occurrence, 1640995270)
  assert_eq(db_stats.error_rate, 0.003)  // 3 / 1000
  
  // Test AUTH_001 statistics
  assert_eq(auth_stats.error_code, "AUTH_001")
  assert_eq(auth_stats.total_count, 1)
  assert_eq(auth_stats.services.length(), 1)  // auth-service
  assert_eq(auth_stats.components.length(), 1)  // jwt_validator
  assert_eq(auth_stats.first_occurrence, 1640995250)
  assert_eq(auth_stats.last_occurrence, 1640995250)
  assert_eq(auth_stats.error_rate, 0.001)  // 1 / 1000
  
  // Find error patterns
  let find_error_patterns = fn(occurrences: Array[ErrorOccurrence]) {
    // Find errors that occur in the same service and component
    let service_component_errors = occurrences.group_by(fn(o) { o.service + ":" + o.component })
    
    // Find errors that occur at regular intervals
    let time_patterns = []
    
    for (service_component, errors) in service_component_errors {
      if errors.length() >= 2 {
        let sorted_errors = errors.sort(fn(a, b) { a.timestamp <= b.timestamp })
        let intervals = []
        
        for i in 1 .. sorted_errors.length() {
          let interval = sorted_errors[i].timestamp - sorted_errors[i-1].timestamp
          intervals = intervals.push(interval)
        }
        
        if intervals.length() > 0 {
          let avg_interval = intervals.reduce(fn(sum, interval) { sum + interval }, 0) / intervals.length()
          time_patterns = time_patterns.push((service_component, errors.length(), avg_interval))
        }
      }
    }
    
    time_patterns
  }
  
  let error_patterns = find_error_patterns(error_occurrences)
  assert_eq(error_patterns.length(), 3)  // user-service:http_client, user-service:database, order-service:database
  
  // Check if any error patterns indicate a recurring issue
  let recurring_issues = error_patterns.filter(fn(p) { p.1 >= 3 })  // Errors that occurred 3+ times
  assert_eq(recurring_issues.length(), 2)  // user-service:http_client, user-service:database
}

// Test 5: Error Recovery Automation
test "error recovery automation" {
  // Define recovery action
  type RecoveryAction = 
    RestartService(String)
    | ClearCache(String)
    | ReconnectDatabase(String)
    | ScaleService(String, Int)
    | FallbackToBackup(String)
    | CustomAction(String)
  
  // Define recovery rule
  type RecoveryRule = {
    error_pattern: String,
    threshold: Int,
    time_window_ms: Int,
    actions: Array[RecoveryAction>
    cooldown_ms: Int
  }
  
  // Define recovery execution
  type RecoveryExecution = {
    rule_name: String,
    actions_executed: Array[RecoveryAction>,
    timestamp: Int,
    success: Bool,
    result_message: String
  }
  
  // Define recovery state
  type RecoveryState = {
    error_counts: Array[(String, Int)],
    last_recovery_times: Array[(String, Int)],
    recent_executions: Array[RecoveryExecution]
  }
  
  // Create recovery rules
  let recovery_rules = [
    {
      error_pattern: "NET_001",
      threshold: 3,
      time_window_ms: 60000,  // 1 minute
      actions: [ReconnectDatabase("user-service-db")],
      cooldown_ms: 300000  // 5 minutes
    },
    {
      error_pattern: "DB_001",
      threshold: 2,
      time_window_ms: 30000,  // 30 seconds
      actions: [RestartService("database-service")],
      cooldown_ms: 600000  // 10 minutes
    },
    {
      error_pattern: "MEM_001",
      threshold: 5,
      time_window_ms: 120000,  // 2 minutes
      actions: [ClearCache("user-service"), ScaleService("user-service", 2)],
      cooldown_ms: 900000  // 15 minutes
    }
  ]
  
  // Initialize recovery state
  let initial_recovery_state = {
    error_counts: [],
    last_recovery_times: [],
    recent_executions: []
  }
  
  // Check if recovery should be triggered
  let should_trigger_recovery = fn(
    error_code: String,
    current_time: Int,
    state: RecoveryState,
    rules: Array[RecoveryRule>
  ) {
    // Find matching rule
    let matching_rules = rules.filter(fn(r) { r.error_pattern == error_code })
    
    if matching_rules.length() == 0 {
      return None
    }
    
    let rule = matching_rules[0]
    
    // Check if we're in cooldown period
    let last_recovery = state.last_recovery_times.filter(fn(t) { t.0 == error_code })
    if last_recovery.length() > 0 {
      let time_since_last_recovery = current_time - last_recovery[0].1
      if time_since_last_recovery < rule.cooldown_ms {
        return None
      }
    }
    
    // Check error count threshold
    let error_count = state.error_counts.filter(fn(c) { c.0 == error_code })
    if error_count.length() > 0 and error_count[0].1 >= rule.threshold {
      Some(rule)
    } else {
      None
    }
  }
  
  // Update error counts
  let update_error_counts = fn(
    state: RecoveryState,
    error_code: String,
    current_time: Int,
    time_window_ms: Int
  ) {
    // In a real implementation, we would filter by time window
    let existing_count = state.error_counts.filter(fn(c) { c.0 == error_code })
    
    if existing_count.length() > 0 {
      let new_count = existing_count[0].1 + 1
      let updated_counts = state.error_counts.map(fn(c) {
        if c.0 == error_code { (error_code, new_count) } else { c }
      })
      { state | error_counts: updated_counts }
    } else {
      { state | error_counts: state.error_counts.push((error_code, 1)) }
    }
  }
  
  // Execute recovery actions
  let execute_recovery_actions = fn(
    rule: RecoveryRule,
    current_time: Int
  ) {
    // Simulate executing actions
    let action_results = rule.actions.map(fn(action) {
      match action {
        RestartService(service) => "Restarted service: " + service
        ClearCache(service) => "Cleared cache for: " + service
        ReconnectDatabase(db) => "Reconnected to database: " + db
        ScaleService(service, replicas) => "Scaled service " + service + " to " + replicas.to_string() + " replicas"
        FallbackToBackup(service) => "Switched " + service + " to backup"
        CustomAction(description) => "Executed custom action: " + description
      }
    })
    
    let result_message = action_results.reduce(fn(msg, result) { msg + "; " + result }, "Recovery actions executed: ")
    
    {
      rule_name: rule.error_pattern,
      actions_executed: rule.actions,
      timestamp: current_time,
      success: true,  // Assume success for simplicity
      result_message
    }
  }
  
  // Update recovery state after execution
  let update_recovery_state = fn(
    state: RecoveryState,
    error_code: String,
    execution: RecoveryExecution
  ) {
    // Reset error count for this error code
    let updated_counts = state.error_counts.map(fn(c) {
      if c.0 == error_code { (error_code, 0) } else { c }
    })
    
    // Update last recovery time
    let updated_recovery_times = state.last_recovery_times
      .filter(fn(t) { t.0 != error_code })
      .push((error_code, execution.timestamp))
    
    // Add to recent executions (keep only last 10)
    let updated_executions = state.recent_executions
      .push(execution)
      .slice(-10)  // Keep only last 10 executions
    
    {
      error_counts: updated_counts,
      last_recovery_times: updated_recovery_times,
      recent_executions: updated_executions
    }
  }
  
  // Test error count updates
  let state_with_errors = initial_recovery_state
    |> update_error_counts("NET_001", 1640995200, 60000)
    |> update_error_counts("NET_001", 1640995210, 60000)
    |> update_error_counts("NET_001", 1640995220, 60000)
    |> update_error_counts("DB_001", 1640995230, 30000)
    |> update_error_counts("DB_001", 1640995240, 30000)
  
  // Check error counts
  let net_error_count = state_with_errors.error_counts.filter(fn(c) { c.0 == "NET_001" })[0].1
  let db_error_count = state_with_errors.error_counts.filter(fn(c) { c.0 == "DB_001" })[0].1
  
  assert_eq(net_error_count, 3)
  assert_eq(db_error_count, 2)
  
  // Test recovery triggering
  let net_recovery_rule = should_trigger_recovery("NET_001", 1640995250, state_with_errors, recovery_rules)
  let db_recovery_rule = should_trigger_recovery("DB_001", 1640995250, state_with_errors, recovery_rules)
  let mem_recovery_rule = should_trigger_recovery("MEM_001", 1640995250, state_with_errors, recovery_rules)
  
  assert_eq(net_recovery_rule, Some(recovery_rules[0]))
  assert_eq(db_recovery_rule, Some(recovery_rules[1]))
  assert_eq(mem_recovery_rule, None)  // No MEM_001 errors
  
  // Test recovery execution
  match net_recovery_rule {
    Some(rule) => {
      let net_recovery_execution = execute_recovery_actions(rule, 1640995250)
      assert_eq(net_recovery_execution.rule_name, "NET_001")
      assert_eq(net_recovery_execution.actions_executed.length(), 1)
      assert_true(net_recovery_execution.result_message.contains("Reconnected to database: user-service-db"))
      assert_true(net_recovery_execution.success)
    }
    None => assert_true(false)
  }
  
  match db_recovery_rule {
    Some(rule) => {
      let db_recovery_execution = execute_recovery_actions(rule, 1640995250)
      assert_eq(db_recovery_execution.rule_name, "DB_001")
      assert_eq(db_recovery_execution.actions_executed.length(), 1)
      assert_true(db_recovery_execution.result_message.contains("Restarted service: database-service"))
      assert_true(db_recovery_execution.success)
    }
    None => assert_true(false)
  }
  
  // Test recovery state update
  let net_execution = execute_recovery_actions(recovery_rules[0], 1640995250)
  let updated_state = update_recovery_state(state_with_errors, "NET_001", net_execution)
  
  // Check that error count was reset
  let updated_net_error_count = updated_state.error_counts.filter(fn(c) { c.0 == "NET_001" })[0].1
  assert_eq(updated_net_error_count, 0)
  
  // Check that last recovery time was updated
  let last_recovery_time = updated_state.last_recovery_times.filter(fn(t) { t.0 == "NET_001" })[0].1
  assert_eq(last_recovery_time, 1640995250)
  
  // Check that execution was added to recent executions
  assert_eq(updated_state.recent_executions.length(), 1)
  assert_eq(updated_state.recent_executions[0].rule_name, "NET_001")
  
  // Test cooldown period
  let cooldown_state = update_recovery_state(state_with_errors, "NET_001", net_execution)
  let cooldown_recovery_rule = should_trigger_recovery("NET_001", 1640995300, cooldown_state, recovery_rules)
  
  assert_eq(cooldown_recovery_rule, None)  // Should be in cooldown period
  
  // Test after cooldown period
  let after_cooldown_recovery_rule = should_trigger_recovery("NET_001", 1640995800, cooldown_state, recovery_rules)
  
  // Should still be None because error count was reset to 0
  assert_eq(after_cooldown_recovery_rule, None)
  
  // But if we get more errors, it should trigger again
  let state_with_new_errors = cooldown_state
    |> update_error_counts("NET_001", 1640995800, 60000)
    |> update_error_counts("NET_001", 1640995810, 60000)
    |> update_error_counts("NET_001", 1640995820, 60000)
  
  let new_recovery_rule = should_trigger_recovery("NET_001", 1640995830, state_with_new_errors, recovery_rules)
  assert_eq(new_recovery_rule, Some(recovery_rules[0]))
}