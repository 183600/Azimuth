// Azimuth Error Boundary Handling Comprehensive Tests
// This file contains comprehensive error boundary handling and recovery tests

// Test 1: Telemetry Error Classification
test "telemetry error classification and categorization" {
  // Define error types
  type TelemetryError = {
    error_id: String,
    error_type: String,
    severity: String,
    source: String,
    message: String,
    timestamp: Int,
    context: Array<(String, String)>,
    recoverable: Bool,
    retry_count: Int
  }
  
  // Define error classifier
  type ErrorClassifier = {
    classify: (String, String, Array[(String, String)]) -> TelemetryError
  }
  
  // Create error classifier
  let classifier = {
    classify: fn(error_message: String, source: String, context: Array[(String, String)>) {
      let error_type = if error_message.contains("timeout") {
        "timeout"
      } else if error_message.contains("connection") {
        "connection_error"
      } else if error_message.contains("authentication") {
        "authentication_error"
      } else if error_message.contains("permission") {
        "authorization_error"
      } else if error_message.contains("not found") {
        "not_found_error"
      } else if error_message.contains("validation") {
        "validation_error"
      } else if error_message.contains("rate limit") {
        "rate_limit_error"
      } else {
        "unknown_error"
      }
      
      let severity = if error_type == "timeout" or error_type == "connection_error" {
        "high"
      } else if error_type == "authentication_error" or error_type == "authorization_error" {
        "critical"
      } else if error_type == "rate_limit_error" {
        "medium"
      } else {
        "low"
      }
      
      let recoverable = match error_type {
        "timeout" => true,
        "connection_error" => true,
        "rate_limit_error" => true,
        "not_found_error" => false,
        "validation_error" => false,
        "authentication_error" => false,
        "authorization_error" => false,
        _ => false
      }
      
      {
        error_id: "error-" + (1640995200 + context.length()).to_string(),
        error_type,
        severity,
        source,
        message: error_message,
        timestamp: 1640995200,
        context,
        recoverable,
        retry_count: 0
      }
    }
  }
  
  // Test error classification
  let timeout_error = classifier.classify("Request timeout after 30 seconds", "api-gateway", [
    ("request.id", "req-123"),
    ("endpoint", "/api/users")
  ])
  
  let auth_error = classifier.classify("Authentication failed: invalid token", "auth-service", [
    ("user.id", "user-456"),
    ("token.status", "expired")
  ])
  
  let validation_error = classifier.classify("Validation error: missing required field 'email'", "user-service", [
    ("operation", "create_user"),
    ("missing.field", "email")
  ])
  
  let rate_limit_error = classifier.classify("Rate limit exceeded: 100 requests per minute", "api-gateway", [
    ("client.ip", "192.168.1.1"),
    ("limit", "100")
  ])
  
  // Verify classification results
  assert_eq(timeout_error.error_type, "timeout")
  assert_eq(timeout_error.severity, "high")
  assert_true(timeout_error.recoverable)
  
  assert_eq(auth_error.error_type, "authentication_error")
  assert_eq(auth_error.severity, "critical")
  assert_false(auth_error.recoverable)
  
  assert_eq(validation_error.error_type, "validation_error")
  assert_eq(validation_error.severity, "low")
  assert_false(validation_error.recoverable)
  
  assert_eq(rate_limit_error.error_type, "rate_limit_error")
  assert_eq(rate_limit_error.severity, "medium")
  assert_true(rate_limit_error.recoverable)
  
  // Test error aggregation
  let aggregate_errors = fn(errors: Array[TelemetryError>) {
    let mut error_counts = {}
    let mut severity_counts = {}
    let mut source_counts = {}
    
    for error in errors {
      // Count by error type
      let current_count = error_counts.get_or(error.error_type, 0)
      error_counts[error.error_type] = current_count + 1
      
      // Count by severity
      let current_severity_count = severity_counts.get_or(error.severity, 0)
      severity_counts[error.severity] = current_severity_count + 1
      
      // Count by source
      let current_source_count = source_counts.get_or(error.source, 0)
      source_counts[error.source] = current_source_count + 1
    }
    
    {
      total_errors: errors.length(),
      error_type_counts: error_counts,
      severity_counts: severity_counts,
      source_counts: source_counts
    }
  }
  
  let all_errors = [timeout_error, auth_error, validation_error, rate_limit_error]
  let aggregation = aggregate_errors(all_errors)
  
  assert_eq(aggregation.total_errors, 4)
  assert_eq(aggregation.error_type_counts["timeout"], 1)
  assert_eq(aggregation.error_type_counts["authentication_error"], 1)
  assert_eq(aggregation.error_type_counts["validation_error"], 1)
  assert_eq(aggregation.error_type_counts["rate_limit_error"], 1)
  
  assert_eq(aggregation.severity_counts["critical"], 1)
  assert_eq(aggregation.severity_counts["high"], 1)
  assert_eq(aggregation.severity_counts["medium"], 1)
  assert_eq(aggregation.severity_counts["low"], 1)
}

// Test 2: Error Recovery Strategies
test "error recovery strategies and mechanisms" {
  // Define recovery strategy
  type RecoveryStrategy = {
    name: String,
    applicable_errors: Array[String>,
    max_retries: Int,
    backoff_strategy: String,
    execute: (TelemetryError) -> String
  }
  
  // Define telemetry error (reuse from previous test)
  type TelemetryError = {
    error_id: String,
    error_type: String,
    severity: String,
    source: String,
    message: String,
    timestamp: Int,
    context: Array[(String, String)>,
    recoverable: Bool,
    retry_count: Int
  }
  
  // Create recovery strategies
  let retry_strategy = {
    name: "retry_with_backoff",
    applicable_errors: ["timeout", "connection_error", "rate_limit_error"],
    max_retries: 3,
    backoff_strategy: "exponential",
    execute: fn(error: TelemetryError) {
      if error.retry_count < 3 {
        let backoff_ms = 1000 * (2 ^ error.retry_count)  // Exponential backoff
        "retry_after_" + backoff_ms.to_string() + "ms"
      } else {
        "max_retries_exceeded"
      }
    }
  }
  
  let fallback_strategy = {
    name: "fallback_service",
    applicable_errors: ["timeout", "connection_error"],
    max_retries: 1,
    backoff_strategy: "immediate",
    execute: fn(error: TelemetryError) {
      "fallback_to_backup_service"
    }
  }
  
  let circuit_breaker_strategy = {
    name: "circuit_breaker",
    applicable_errors: ["connection_error", "timeout"],
    max_retries: 0,
    backoff_strategy: "none",
    execute: fn(error: TelemetryError) {
      "open_circuit_breaker"
    }
  }
  
  let graceful_degradation_strategy = {
    name: "graceful_degradation",
    applicable_errors: ["not_found_error", "validation_error"],
    max_retries: 0,
    backoff_strategy: "none",
    execute: fn(error: TelemetryError) {
      "provide_default_response"
    }
  }
  
  let strategies = [retry_strategy, fallback_strategy, circuit_breaker_strategy, graceful_degradation_strategy]
  
  // Create error handler
  let handle_error = fn(error: TelemetryError, strategies: Array[RecoveryStrategy>) {
    let mut applicable_strategies = []
    for strategy in strategies {
      if strategy.applicable_errors.contains(error.error_type) {
        applicable_strategies = applicable_strategies.push(strategy)
      }
    }
    
    if applicable_strategies.length() > 0 {
      let strategy = applicable_strategies[0]  // Use first applicable strategy
      let recovery_action = strategy.execute(error)
      
      {
        handled: true,
        strategy_name: strategy.name,
        recovery_action,
        updated_error: { error | retry_count: error.retry_count + 1 }
      }
    } else {
      {
        handled: false,
        strategy_name: "none",
        recovery_action: "no_applicable_strategy",
        updated_error: error
      }
    }
  }
  
  // Test error handling
  let timeout_error = {
    error_id: "error-123",
    error_type: "timeout",
    severity: "high",
    source: "api-gateway",
    message: "Request timeout after 30 seconds",
    timestamp: 1640995200,
    context: [("request.id", "req-123")],
    recoverable: true,
    retry_count: 0
  }
  
  let auth_error = {
    error_id: "error-456",
    error_type: "authentication_error",
    severity: "critical",
    source: "auth-service",
    message: "Authentication failed: invalid token",
    timestamp: 1640995200,
    context: [("user.id", "user-456")],
    recoverable: false,
    retry_count: 0
  }
  
  // Test timeout error handling
  let timeout_result = handle_error(timeout_error, strategies)
  assert_true(timeout_result.handled)
  assert_eq(timeout_result.strategy_name, "retry_with_backoff")
  assert_eq(timeout_result.recovery_action, "retry_after_1000ms")
  assert_eq(timeout_result.updated_error.retry_count, 1)
  
  // Test authentication error handling
  let auth_result = handle_error(auth_error, strategies)
  assert_false(auth_result.handled)
  assert_eq(auth_result.strategy_name, "none")
  assert_eq(auth_result.recovery_action, "no_applicable_strategy")
  
  // Test multiple retries
  let mut retry_error = timeout_error
  let mut retry_results = []
  
  for i in 0..4 {
    let result = handle_error(retry_error, strategies)
    retry_results = retry_results.push(result)
    retry_error = result.updated_error
  }
  
  // First 3 attempts should retry
  assert_true(retry_results[0].handled)
  assert_eq(retry_results[0].recovery_action, "retry_after_1000ms")
  
  assert_true(retry_results[1].handled)
  assert_eq(retry_results[1].recovery_action, "retry_after_2000ms")
  
  assert_true(retry_results[2].handled)
  assert_eq(retry_results[2].recovery_action, "retry_after_4000ms")
  
  // 4th attempt should exceed max retries
  assert_true(retry_results[3].handled)
  assert_eq(retry_results[3].recovery_action, "max_retries_exceeded")
  
  // Test strategy priority
  let prioritized_handle_error = fn(error: TelemetryError, strategies: Array[RecoveryStrategy>) {
    let mut applicable_strategies = []
    for strategy in strategies {
      if strategy.applicable_errors.contains(error.error_type) {
        applicable_strategies = applicable_strategies.push(strategy)
      }
    }
    
    // Prioritize strategies: circuit_breaker > fallback > retry > graceful_degradation
    let priority_order = ["circuit_breaker", "fallback_service", "retry_with_backoff", "graceful_degradation"]
    let mut selected_strategy = None
    
    for priority in priority_order {
      for strategy in applicable_strategies {
        if strategy.name == priority {
          selected_strategy = Some(strategy)
          break
        }
      }
      if selected_strategy.length > 0 {
        break
      }
    }
    
    match selected_strategy {
      Some(strategy) => {
        let recovery_action = strategy.execute(error)
        {
          handled: true,
          strategy_name: strategy.name,
          recovery_action,
          updated_error: { error | retry_count: error.retry_count + 1 }
        }
      }
      None => {
        {
          handled: false,
          strategy_name: "none",
          recovery_action: "no_applicable_strategy",
          updated_error: error
        }
      }
    }
  }
  
  // Test prioritized handling
  let prioritized_result = prioritized_handle_error(timeout_error, strategies)
  assert_true(prioritized_result.handled)
  assert_eq(prioritized_result.strategy_name, "circuit_breaker")
  assert_eq(prioritized_result.recovery_action, "open_circuit_breaker")
}

// Test 3: Error Boundary Isolation
test "error boundary isolation and containment" {
  // Define service boundary
  type ServiceBoundary = {
    service_name: String,
    error_threshold: Int,  // Max errors per minute
    recovery_time: Int,    # Time to wait before recovery attempts
    circuit_state: String,  // "closed", "open", "half_open"
    error_count: Int,
    last_error_time: Int,
    last_success_time: Int
  }
  
  // Define boundary manager
  type BoundaryManager = {
    check_boundary: (ServiceBoundary, TelemetryError) -> ServiceBoundary,
    can_process: (ServiceBoundary) -> Bool,
    attempt_recovery: (ServiceBoundary) -> ServiceBoundary
  }
  
  // Define telemetry error
  type TelemetryError = {
    error_id: String,
    error_type: String,
    severity: String,
    source: String,
    message: String,
    timestamp: Int,
    context: Array<(String, String)>,
    recoverable: Bool,
    retry_count: Int
  }
  
  // Create boundary manager
  let manager = {
    check_boundary: fn(boundary: ServiceBoundary, error: TelemetryError) {
      let current_time = error.timestamp
      let time_since_last_error = current_time - boundary.last_error_time
      let time_since_last_success = current_time - boundary.last_success_time
      
      let new_boundary = match boundary.circuit_state {
        "closed" => {
          // In closed state, count errors
          let new_error_count = if time_since_last_error < 60 {
            boundary.error_count + 1
          } else {
            1  // Reset count if errors are not recent
          }
          
          if new_error_count >= boundary.error_threshold {
            // Open circuit
            {
              service_name: boundary.service_name,
              error_threshold: boundary.error_threshold,
              recovery_time: boundary.recovery_time,
              circuit_state: "open",
              error_count: new_error_count,
              last_error_time: current_time,
              last_success_time: boundary.last_success_time
            }
          } else {
            // Keep circuit closed
            {
              service_name: boundary.service_name,
              error_threshold: boundary.error_threshold,
              recovery_time: boundary.recovery_time,
              circuit_state: "closed",
              error_count: new_error_count,
              last_error_time: current_time,
              last_success_time: boundary.last_success_time
            }
          }
        }
        "open" => {
          // In open state, check if recovery time has passed
          if time_since_last_error >= boundary.recovery_time {
            // Try half-open state
            {
              service_name: boundary.service_name,
              error_threshold: boundary.error_threshold,
              recovery_time: boundary.recovery_time,
              circuit_state: "half_open",
              error_count: boundary.error_count,
              last_error_time: boundary.last_error_time,
              last_success_time: boundary.last_success_time
            }
          } else {
            // Keep circuit open
            boundary
          }
        }
        "half_open" => {
          // In half-open state, any error opens circuit again
          {
            service_name: boundary.service_name,
            error_threshold: boundary.error_threshold,
            recovery_time: boundary.recovery_time,
            circuit_state: "open",
            error_count: boundary.error_count + 1,
            last_error_time: current_time,
            last_success_time: boundary.last_success_time
          }
        }
        _ => boundary
      }
      
      new_boundary
    },
    
    can_process: fn(boundary: ServiceBoundary) {
      boundary.circuit_state == "closed" or boundary.circuit_state == "half_open"
    },
    
    attempt_recovery: fn(boundary: ServiceBoundary) {
      let current_time = 1640995300  // Current time for recovery attempt
      let time_since_last_error = current_time - boundary.last_error_time
      
      if boundary.circuit_state == "open" and time_since_last_error >= boundary.recovery_time {
        {
          service_name: boundary.service_name,
          error_threshold: boundary.error_threshold,
          recovery_time: boundary.recovery_time,
          circuit_state: "half_open",
          error_count: boundary.error_count,
          last_error_time: boundary.last_error_time,
          last_success_time: boundary.last_success_time
        }
      } else {
        boundary
      }
    }
  }
  
  // Create service boundaries
  let api_gateway_boundary = {
    service_name: "api-gateway",
    error_threshold: 5,
    recovery_time: 60,  # 60 seconds
    circuit_state: "closed",
    error_count: 0,
    last_error_time: 0,
    last_success_time: 1640995200
  }
  
  let auth_service_boundary = {
    service_name: "auth-service",
    error_threshold: 3,
    recovery_time: 120,  # 120 seconds
    circuit_state: "closed",
    error_count: 0,
    last_error_time: 0,
    last_success_time: 1640995200
  }
  
  // Test error boundary isolation
  let api_errors = [
    {
      error_id: "error-1",
      error_type: "timeout",
      severity: "high",
      source: "api-gateway",
      message: "Request timeout",
      timestamp: 1640995210,
      context: [],
      recoverable: true,
      retry_count: 0
    },
    {
      error_id: "error-2",
      error_type: "connection_error",
      severity: "high",
      source: "api-gateway",
      message: "Connection failed",
      timestamp: 1640995220,
      context: [],
      recoverable: true,
      retry_count: 0
    },
    {
      error_id: "error-3",
      error_type: "timeout",
      severity: "high",
      source: "api-gateway",
      message: "Request timeout",
      timestamp: 1640995230,
      context: [],
      recoverable: true,
      retry_count: 0
    },
    {
      error_id: "error-4",
      error_type: "connection_error",
      severity: "high",
      source: "api-gateway",
      message: "Connection failed",
      timestamp: 1640995240,
      context: [],
      recoverable: true,
      retry_count: 0
    },
    {
      error_id: "error-5",
      error_type: "timeout",
      severity: "high",
      source: "api-gateway",
      message: "Request timeout",
      timestamp: 1640995250,
      context: [],
      recoverable: true,
      retry_count: 0
    }
  ]
  
  // Process errors and check boundary state
  let mut current_boundary = api_gateway_boundary
  for error in api_errors {
    current_boundary = manager.check_boundary(current_boundary, error)
  }
  
  // After 5 errors, circuit should be open
  assert_eq(current_boundary.circuit_state, "open")
  assert_eq(current_boundary.error_count, 5)
  assert_false(manager.can_process(current_boundary))
  
  // Test recovery attempt
  let recovered_boundary = manager.attempt_recovery({
    service_name: "api-gateway",
    error_threshold: 5,
    recovery_time: 60,
    circuit_state: "open",
    error_count: 5,
    last_error_time: 1640995250,
    last_success_time: 1640995200
  })
  
  // With timestamp 1640995300, 50 seconds have passed (less than 60s recovery time)
  assert_eq(recovered_boundary.circuit_state, "open")  # Should still be open
  
  // Test successful recovery
  let fully_recovered_boundary = manager.attempt_recovery({
    service_name: "api-gateway",
    error_threshold: 5,
    recovery_time: 60,
    circuit_state: "open",
    error_count: 5,
    last_error_time: 1640995200,  # Earlier timestamp
    last_success_time: 1640995200
  })
  
  // With timestamp 1640995300, 100 seconds have passed (more than 60s recovery time)
  assert_eq(fully_recovered_boundary.circuit_state, "half_open")
  assert_true(manager.can_process(fully_recovered_boundary))
  
  // Test error in half-open state
  let half_open_error = {
    error_id: "error-6",
    error_type: "timeout",
    severity: "high",
    source: "api-gateway",
    message: "Request timeout",
    timestamp: 1640995300,
    context: [],
    recoverable: true,
    retry_count: 0
  }
  
  let reopened_boundary = manager.check_boundary(fully_recovered_boundary, half_open_error)
  assert_eq(reopened_boundary.circuit_state, "open")
  assert_false(manager.can_process(reopened_boundary))
  
  // Test successful operation in half-open state
  let record_success = fn(boundary: ServiceBoundary) {
    {
      service_name: boundary.service_name,
      error_threshold: boundary.error_threshold,
      recovery_time: boundary.recovery_time,
      circuit_state: "closed",  # Close circuit on success
      error_count: 0,  # Reset error count
      last_error_time: boundary.last_error_time,
      last_success_time: 1640995300
    }
  }
  
  let success_boundary = record_success(fully_recovered_boundary)
  assert_eq(success_boundary.circuit_state, "closed")
  assert_eq(success_boundary.error_count, 0)
  assert_true(manager.can_process(success_boundary))
}

// Test 4: Error Propagation Control
test "error propagation control and containment" {
  // Define error propagation policy
  type PropagationPolicy = {
    error_types: Array[String],
    propagate_upstream: Bool,
    propagate_downstream: Bool,
    max_propagation_depth: Int,
    transform_message: (String) -> String
  }
  
  // Define error context
  type ErrorContext = {
    service_name: String,
    operation_name: String,
    parent_context: Option[ErrorContext],
    error_chain: Array[String>
  }
  
  // Define propagated error
  type PropagatedError = {
    original_error: TelemetryError,
    current_context: ErrorContext,
    propagation_path: Array[String>,
    transformed_message: String
  }
  
  // Define telemetry error
  type TelemetryError = {
    error_id: String,
    error_type: String,
    severity: String,
    source: String,
    message: String,
    timestamp: Int,
    context: Array<(String, String)>,
    recoverable: Bool,
    retry_count: Int
  }
  
  // Create propagation policies
  let critical_error_policy = {
    error_types: ["authentication_error", "authorization_error", "security_breach"],
    propagate_upstream: true,
    propagate_downstream: false,
    max_propagation_depth: 10,
    transform_message: fn(message: String) {
      "SECURITY_ERROR: " + message
    }
  }
  
  let business_error_policy = {
    error_types: ["validation_error", "not_found_error", "business_rule_violation"],
    propagate_upstream: true,
    propagate_downstream: true,
    max_propagation_depth: 5,
    transform_message: fn(message: String) {
      "BUSINESS_ERROR: " + message
    }
  }
  
  let infrastructure_error_policy = {
    error_types: ["timeout", "connection_error", "resource_exhausted"],
    propagate_upstream: false,
    propagate_downstream: false,
    max_propagation_depth: 0,
    transform_message: fn(message: String) {
      "INFRASTRUCTURE_ERROR: " + message
    }
  }
  
  let policies = [critical_error_policy, business_error_policy, infrastructure_error_policy]
  
  // Create error propagator
  let propagate_error = fn(error: TelemetryError, context: ErrorContext, policies: Array[PropagationPolicy]) {
    let mut applicable_policies = []
    for policy in policies {
      if policy.error_types.contains(error.error_type) {
        applicable_policies = applicable_policies.push(policy)
      }
    }
    
    if applicable_policies.length() > 0 {
      let policy = applicable_policies[0]  # Use first applicable policy
      let transformed_message = policy.transform_message(error.message)
      
      // Build propagation path
      let mut propagation_path = []
      let mut current_context = context
      while true {
        propagation_path = propagation_path.push(current_context.service_name + ":" + current_context.operation_name)
        match current_context.parent_context {
          Some(parent) => current_context = parent
          None => break
        }
      }
      
      {
        original_error: error,
        current_context,
        propagation_path,
        transformed_message
      }
    } else {
      // No propagation
      {
        original_error: error,
        current_context,
        propagation_path: [],
        transformed_message: error.message
      }
    }
  }
  
  // Create test contexts
  let gateway_context = {
    service_name: "api-gateway",
    operation_name: "authenticate_request",
    parent_context: None,
    error_chain: []
  }
  
  let auth_context = {
    service_name: "auth-service",
    operation_name: "validate_token",
    parent_context: Some(gateway_context),
    error_chain: []
  }
  
  let user_context = {
    service_name: "user-service",
    operation_name: "get_profile",
    parent_context: Some(auth_context),
    error_chain: []
  }
  
  // Create test errors
  let auth_error = {
    error_id: "error-auth-123",
    error_type: "authentication_error",
    severity: "critical",
    source: "auth-service",
    message: "Invalid authentication token",
    timestamp: 1640995200,
    context: [("token.id", "token-456")],
    recoverable: false,
    retry_count: 0
  }
  
  let validation_error = {
    error_id: "error-validation-789",
    error_type: "validation_error",
    severity: "low",
    source: "user-service",
    message: "Missing required field: user_id",
    timestamp: 1640995200,
    context: [("operation", "get_profile")],
    recoverable: false,
    retry_count: 0
  }
  
  let timeout_error = {
    error_id: "error-timeout-101",
    error_type: "timeout",
    severity: "high",
    source: "database",
    message: "Database query timeout",
    timestamp: 1640995200,
    context: [("query", "SELECT * FROM users")],
    recoverable: true,
    retry_count: 0
  }
  
  // Test error propagation
  let auth_propagation = propagate_error(auth_error, auth_context, policies)
  assert_eq(auth_propagation.transformed_message, "SECURITY_ERROR: Invalid authentication token")
  assert_eq(auth_propagation.propagation_path.length(), 2)  # auth-service:validate_token -> api-gateway:authenticate_request
  
  let validation_propagation = propagate_error(validation_error, user_context, policies)
  assert_eq(validation_propagation.transformed_message, "BUSINESS_ERROR: Missing required field: user_id")
  assert_eq(validation_propagation.propagation_path.length(), 3)  # user-service:get_profile -> auth-service:validate_token -> api-gateway:authenticate_request
  
  let timeout_propagation = propagate_error(timeout_error, user_context, policies)
  assert_eq(timeout_propagation.transformed_message, "INFRASTRUCTURE_ERROR: Database query timeout")
  assert_eq(timeout_propagation.propagation_path.length(), 0)  # No propagation for infrastructure errors
  
  // Test propagation depth control
  let create_deep_context = fn(depth: Int) {
    if depth <= 0 {
      {
        service_name: "service-" + depth.to_string(),
        operation_name: "operation-" + depth.to_string(),
        parent_context: None,
        error_chain: []
      }
    } else {
      let parent = create_deep_context(depth - 1)
      {
        service_name: "service-" + depth.to_string(),
        operation_name: "operation-" + depth.to_string(),
        parent_context: Some(parent),
        error_chain: []
      }
    }
  }
  
  let deep_context = create_deep_context(8)  # 8 levels deep
  let deep_propagation = propagate_error(auth_error, deep_context, policies)
  
  # Critical errors should propagate up to max depth
  assert_eq(deep_propagation.propagation_path.length(), 8)
  
  # Test business error with depth limit
  let deep_business_propagation = propagate_error(validation_error, deep_context, policies)
  assert_eq(deep_business_propagation.propagation_path.length(), 5)  # Limited by max_propagation_depth
  
  # Test error containment
  let should_propagate = fn(error: TelemetryError, context: ErrorContext, direction: String) {
    let mut applicable_policies = []
    for policy in policies {
      if policy.error_types.contains(error.error_type) {
        applicable_policies = applicable_policies.push(policy)
      }
    }
    
    if applicable_policies.length() > 0 {
      let policy = applicable_policies[0]
      match direction {
        "upstream" => policy.propagate_upstream,
        "downstream" => policy.propagate_downstream,
        _ => false
      }
    } else {
      false
    }
  }
  
  # Critical errors should propagate upstream
  assert_true(should_propagate(auth_error, auth_context, "upstream"))
  assert_false(should_propagate(auth_error, auth_context, "downstream"))
  
  # Business errors should propagate both directions
  assert_true(should_propagate(validation_error, user_context, "upstream"))
  assert_true(should_propagate(validation_error, user_context, "downstream"))
  
  # Infrastructure errors should not propagate
  assert_false(should_propagate(timeout_error, user_context, "upstream"))
  assert_false(should_propagate(timeout_error, user_context, "downstream"))
}

// Test 5: Error Recovery Validation
test "error recovery validation and effectiveness" {
  // Define recovery attempt
  type RecoveryAttempt = {
    attempt_id: String,
    error_id: String,
    strategy_name: String,
    timestamp: Int,
    success: Bool,
    duration_ms: Int,
    result_message: String
  }
  
  // Define recovery validation
  type RecoveryValidation = {
    total_attempts: Int,
    successful_attempts: Int,
    failed_attempts: Int,
    success_rate: Float,
    avg_recovery_time_ms: Int,
    most_effective_strategy: String,
    recovery_trend: String  # "improving", "stable", "degrading"
  }
  
  // Define telemetry error
  type TelemetryError = {
    error_id: String,
    error_type: String,
    severity: String,
    source: String,
    message: String,
    timestamp: Int,
    context: Array<(String, String)>,
    recoverable: Bool,
    retry_count: Int
  }
  
  // Create recovery tracker
  let track_recovery_attempts = fn(attempts: Array[RecoveryAttempt>) {
    let successful_attempts = attempts.filter(fn(a) { a.success }).length()
    let failed_attempts = attempts.filter(fn(a) { not(a.success) }).length()
    let success_rate = successful_attempts.to_float() / attempts.length().to_float()
    
    let total_recovery_time = attempts.reduce(fn(sum, a) { sum + a.duration_ms }, 0)
    let avg_recovery_time_ms = if attempts.length() > 0 {
      total_recovery_time / attempts.length()
    } else {
      0
    }
    
    # Find most effective strategy
    let strategy_performance = {}
    for attempt in attempts {
      let current_count = strategy_performance.get_or(attempt.strategy_name, { success: 0, total: 0 })
      let new_count = if attempt.success {
        { success: current_count.success + 1, total: current_count.total + 1 }
      } else {
        { success: current_count.success, total: current_count.total + 1 }
      }
      strategy_performance[attempt.strategy_name] = new_count
    }
    
    let mut most_effective_strategy = "none"
    let mut best_success_rate = 0.0
    
    for (strategy, performance) in strategy_performance {
      let strategy_success_rate = performance.success.to_float() / performance.total.to_float()
      if strategy_success_rate > best_success_rate {
        best_success_rate = strategy_success_rate
        most_effective_strategy = strategy
      }
    }
    
    # Analyze recovery trend
    let recovery_trend = if attempts.length() >= 10 {
      let recent_attempts = attempts.slice(attempts.length() - 5, attempts.length())
      let recent_success_rate = recent_attempts.filter(fn(a) { a.success }).length().to_float() / recent_attempts.length().to_float()
      
      let earlier_attempts = attempts.slice(0, 5)
      let earlier_success_rate = earlier_attempts.filter(fn(a) { a.success }).length().to_float() / earlier_attempts.length().to_float()
      
      if recent_success_rate > earlier_success_rate + 0.1 {
        "improving"
      } else if recent_success_rate < earlier_success_rate - 0.1 {
        "degrading"
      } else {
        "stable"
      }
    } else {
      "insufficient_data"
    }
    
    {
      total_attempts: attempts.length(),
      successful_attempts,
      failed_attempts,
      success_rate,
      avg_recovery_time_ms,
      most_effective_strategy,
      recovery_trend
    }
  }
  
  # Create test recovery attempts
  let recovery_attempts = [
    {
      attempt_id: "attempt-1",
      error_id: "error-timeout-123",
      strategy_name: "retry_with_backoff",
      timestamp: 1640995210,
      success: false,
      duration_ms: 1200,
      result_message: "retry_after_1000ms"
    },
    {
      attempt_id: "attempt-2",
      error_id: "error-timeout-123",
      strategy_name: "retry_with_backoff",
      timestamp: 1640995220,
      success: false,
      duration_ms: 2200,
      result_message: "retry_after_2000ms"
    },
    {
      attempt_id: "attempt-3",
      error_id: "error-timeout-123",
      strategy_name: "retry_with_backoff",
      timestamp: 1640995230,
      success: true,
      duration_ms: 4200,
      result_message: "success"
    },
    {
      attempt_id: "attempt-4",
      error_id: "error-connection-456",
      strategy_name: "fallback_service",
      timestamp: 1640995240,
      success: true,
      duration_ms: 800,
      result_message: "fallback_to_backup_service"
    },
    {
      attempt_id: "attempt-5",
      error_id: "error-connection-789",
      strategy_name: "fallback_service",
      timestamp: 1640995250,
      success: true,
      duration_ms: 750,
      result_message: "fallback_to_backup_service"
    },
    {
      attempt_id: "attempt-6",
      error_id: "error-connection-101",
      strategy_name: "fallback_service",
      timestamp: 1640995260,
      success: false,
      duration_ms: 900,
      result_message: "fallback_service_unavailable"
    },
    {
      attempt_id: "attempt-7",
      error_id: "error-validation-202",
      strategy_name: "graceful_degradation",
      timestamp: 1640995270,
      success: true,
      duration_ms: 300,
      result_message: "provide_default_response"
    },
    {
      attempt_id: "attempt-8",
      error_id: "error-validation-303",
      strategy_name: "graceful_degradation",
      timestamp: 1640995280,
      success: true,
      duration_ms: 280,
      result_message: "provide_default_response"
    },
    {
      attempt_id: "attempt-9",
      error_id: "error-validation-404",
      strategy_name: "graceful_degradation",
      timestamp: 1640995290,
      success: true,
      duration_ms: 320,
      result_message: "provide_default_response"
    },
    {
      attempt_id: "attempt-10",
      error_id: "error-timeout-505",
      strategy_name: "retry_with_backoff",
      timestamp: 1640995300,
      success: false,
      duration_ms: 1500,
      result_message: "max_retries_exceeded"
    },
    {
      attempt_id: "attempt-11",
      error_id: "error-timeout-606",
      strategy_name: "retry_with_backoff",
      timestamp: 1640995310,
      success: false,
      duration_ms: 1300,
      result_message: "max_retries_exceeded"
    },
    {
      attempt_id: "attempt-12",
      error_id: "error-timeout-707",
      strategy_name: "retry_with_backoff",
      timestamp: 1640995320,
      success: true,
      duration_ms: 1100,
      result_message: "success"
    }
  ]
  
  # Validate recovery effectiveness
  let validation = track_recovery_attempts(recovery_attempts)
  
  assert_eq(validation.total_attempts, 12)
  assert_eq(validation.successful_attempts, 8)
  assert_eq(validation.failed_attempts, 4)
  assert_eq(validation.success_rate, 8.0 / 12.0)
  
  # Average recovery time should be reasonable
  assert_true(validation.avg_recovery_time_ms > 0)
  assert_true(validation.avg_recovery_time_ms < 5000)  # Less than 5 seconds on average
  
  # Most effective strategy should be graceful_degradation (100% success rate)
  assert_eq(validation.most_effective_strategy, "graceful_degradation")
  
  # Test recovery effectiveness by error type
  let analyze_by_error_type = fn(attempts: Array[RecoveryAttempt>) {
    let mut error_type_analysis = {}
    
    for attempt in attempts {
      let error_type = if attempt.error_id.contains("timeout") {
        "timeout"
      } else if attempt.error_id.contains("connection") {
        "connection_error"
      } else if attempt.error_id.contains("validation") {
        "validation_error"
      } else {
        "unknown"
      }
      
      let current_analysis = error_type_analysis.get_or(error_type, {
        total_attempts: 0,
        successful_attempts: 0,
        strategies: {}
      })
      
      let mut strategies = current_analysis.strategies
      let current_strategy_attempts = strategies.get_or(attempt.strategy_name, { success: 0, total: 0 })
      let new_strategy_attempts = if attempt.success {
        { success: current_strategy_attempts.success + 1, total: current_strategy_attempts.total + 1 }
      } else {
        { success: current_strategy_attempts.success, total: current_strategy_attempts.total + 1 }
      }
      strategies[attempt.strategy_name] = new_strategy_attempts
      
      error_type_analysis[error_type] = {
        total_attempts: current_analysis.total_attempts + 1,
        successful_attempts: current_analysis.successful_attempts + (if attempt.success { 1 } else { 0 }),
        strategies
      }
    }
    
    error_type_analysis
  }
  
  let error_type_analysis = analyze_by_error_type(recovery_attempts)
  
  # Timeout errors should have mixed success with retry strategy
  let timeout_analysis = error_type_analysis["timeout"]
  assert_eq(timeout_analysis.total_attempts, 5)
  assert_eq(timeout_analysis.successful_attempts, 2)
  
  # Connection errors should have good success with fallback strategy
  let connection_analysis = error_type_analysis["connection_error"]
  assert_eq(connection_analysis.total_attempts, 3)
  assert_eq(connection_analysis.successful_attempts, 2)
  
  # Validation errors should have 100% success with graceful degradation
  let validation_analysis = error_type_analysis["validation_error"]
  assert_eq(validation_analysis.total_attempts, 3)
  assert_eq(validation_analysis.successful_attempts, 3)
  
  # Test recovery time analysis
  let analyze_recovery_times = fn(attempts: Array[RecoveryAttempt>) {
    let successful_attempts = attempts.filter(fn(a) { a.success })
    let failed_attempts = attempts.filter(fn(a) { not(a.success) })
    
    let successful_times = successful_attempts.map(fn(a) { a.duration_ms })
    let failed_times = failed_attempts.map(fn(a) { a.duration_ms })
    
    let successful_avg = if successful_times.length() > 0 {
      successful_times.reduce(fn(sum, time) { sum + time }, 0) / successful_times.length()
    } else {
      0
    }
    
    let failed_avg = if failed_times.length() > 0 {
      failed_times.reduce(fn(sum, time) { sum + time }, 0) / failed_times.length()
    } else {
      0
    }
    
    {
      successful_avg_time_ms: successful_avg,
      failed_avg_time_ms: failed_avg,
      time_difference_ms: successful_avg - failed_avg
    }
  }
  
  let time_analysis = analyze_recovery_times(recovery_attempts)
  
  # Failed attempts might take longer (due to timeouts, etc.) or shorter (if they fail quickly)
  assert_true(time_analysis.successful_avg_time_ms > 0)
  assert_true(time_analysis.failed_avg_time_ms > 0)
  
  # Test recovery recommendation system
  let recommend_recovery_strategy = fn(error_type: String, analysis: Object) {
    match error_type {
      "timeout" => "retry_with_backoff"
      "connection_error" => "fallback_service"
      "validation_error" => "graceful_degradation"
      "authentication_error" => "none"
      "authorization_error" => "none"
      _ => "retry_with_backoff"
    }
  }
  
  assert_eq(recommend_recovery_strategy("timeout", error_type_analysis), "retry_with_backoff")
  assert_eq(recommend_recovery_strategy("connection_error", error_type_analysis), "fallback_service")
  assert_eq(recommend_recovery_strategy("validation_error", error_type_analysis), "graceful_degradation")
  assert_eq(recommend_recovery_strategy("authentication_error", error_type_analysis), "none")
}