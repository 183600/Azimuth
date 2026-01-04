// Azimuth Premium Error Boundary Handling Tests
// This file contains comprehensive test cases for error boundary handling and recovery

// Test 1: Error Boundary with Exception Handling
test "error boundary with exception handling" {
  // Define error types
  enum ErrorType {
    ValidationError(String)
    NetworkError(String)
    DatabaseError(String)
    TimeoutError(String)
    UnknownError(String)
  }
  
  // Define error boundary state
  type ErrorBoundaryState = {
    has_error: Bool,
    error: Option[ErrorType],
    error_info: Option[String],
    retry_count: Int,
    max_retries: Int,
    last_error_time: Int
  }
  
  // Define operation result
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[ErrorType],
    recovered: Bool
  }
  
  // Create error boundary
  let create_error_boundary = fn(max_retries: Int) {
    {
      has_error: false,
      error: None,
      error_info: None,
      retry_count: 0,
      max_retries,
      last_error_time: 0
    }
  }
  
  // Execute operation with error boundary
  let execute_with_boundary = fn[T](
    boundary: ErrorBoundaryState,
    operation: () -> OperationResult[T],
    current_time: Int,
    error_handler: ErrorType -> String
  ) {
    let mut updated_boundary = boundary
    
    if boundary.retry_count >= boundary.max_retries {
      return (updated_boundary, {
        success: false,
        data: None,
        error: Some(ErrorType::UnknownError("Max retries exceeded")),
        recovered: false
      })
    }
    
    // Attempt operation
    let result = operation()
    
    if result.success {
      // Operation succeeded, reset boundary state
      updated_boundary = {
        has_error: false,
        error: None,
        error_info: None,
        retry_count: 0,
        max_retries: boundary.max_retries,
        last_error_time: 0
      }
      
      (updated_boundary, result)
    } else {
      // Operation failed, update boundary state
      let error_info = match result.error {
        Some(err) => error_handler(err),
        None => "Unknown error occurred"
      }
      
      updated_boundary = {
        has_error: true,
        error: result.error,
        error_info: Some(error_info),
        retry_count: boundary.retry_count + 1,
        max_retries: boundary.max_retries,
        last_error_time: current_time
      }
      
      (updated_boundary, {
        success: false,
        data: None,
        error: result.error,
        recovered: false
      })
    }
  }
  
  // Test error boundary creation
  let boundary = create_error_boundary(3)
  assert_false(boundary.has_error)
  assert_eq(boundary.retry_count, 0)
  assert_eq(boundary.max_retries, 3)
  
  // Test successful operation
  let success_operation = fn() {
    {
      success: true,
      data: Some("operation result"),
      error: None,
      recovered: false
    }
  }
  
  let error_handler = fn(error) {
    match error {
      ErrorType::ValidationError(msg) => "Validation error: " + msg
      ErrorType::NetworkError(msg) => "Network error: " + msg
      ErrorType::DatabaseError(msg) => "Database error: " + msg
      ErrorType::TimeoutError(msg) => "Timeout error: " + msg
      ErrorType::UnknownError(msg) => "Unknown error: " + msg
    }
  }
  
  let (boundary1, result1) = execute_with_boundary(boundary, success_operation, 1640995200000, error_handler)
  assert_true(result1.success)
  match result1.data {
    Some(data) => assert_eq(data, "operation result")
    None => assert_true(false)
  }
  assert_false(boundary1.has_error)
  assert_eq(boundary1.retry_count, 0)
  
  // Test failed operation with retry
  let mut retry_count = 0
  let failing_operation = fn() {
    retry_count = retry_count + 1
    if retry_count < 3 {
      {
        success: false,
        data: None,
        error: Some(ErrorType::NetworkError("Connection failed")),
        recovered: false
      }
    } else {
      {
        success: true,
        data: Some("retry success"),
        error: None,
        recovered: true
      }
    }
  }
  
  let (boundary2, result2) = execute_with_boundary(boundary1, failing_operation, 1640995201000, error_handler)
  assert_true(result2.success)
  match result2.data {
    Some(data) => assert_eq(data, "retry success")
    None => assert_true(false)
  }
  assert_true(result2.recovered)
  assert_false(boundary2.has_error)
  assert_eq(boundary2.retry_count, 0)  // Reset after success
  
  // Test max retries exceeded
  let always_failing_operation = fn() {
    {
      success: false,
      data: None,
      error: Some(ErrorType::DatabaseError("Connection pool exhausted")),
      recovered: false
    }
  }
  
  let boundary3 = create_error_boundary(2)
  let (boundary4, result3) = execute_with_boundary(boundary3, always_failing_operation, 1640995202000, error_handler)
  assert_false(result3.success)
  assert_eq(boundary4.retry_count, 1)
  assert_true(boundary4.has_error)
  
  // Execute again to increment retry count
  let (boundary5, result4) = execute_with_boundary(boundary4, always_failing_operation, 1640995203000, error_handler)
  assert_false(result4.success)
  assert_eq(boundary5.retry_count, 2)
  
  // Execute again to exceed max retries
  let (boundary6, result5) = execute_with_boundary(boundary5, always_failing_operation, 1640995204000, error_handler)
  assert_false(result5.success)
  match result5.error {
    Some(ErrorType::UnknownError(msg)) => assert_eq(msg, "Max retries exceeded")
    _ => assert_true(false)
  }
}

// Test 2: Circuit Breaker Pattern
test "circuit breaker pattern implementation" {
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
    success_count: Int,
    failure_threshold: Int,
    success_threshold: Int,
    timeout_ms: Int,
    last_failure_time: Int,
    last_state_change: Int
  }
  
  // Create circuit breaker
  let create_circuit_breaker = fn(failure_threshold: Int, success_threshold: Int, timeout_ms: Int) {
    {
      state: CircuitState::Closed,
      failure_count: 0,
      success_count: 0,
      failure_threshold,
      success_threshold,
      timeout_ms,
      last_failure_time: 0,
      last_state_change: 1640995200000
    }
  }
  
  // Execute operation through circuit breaker
  let execute_through_breaker = fn[T](
    breaker: CircuitBreaker,
    operation: () -> T,
    current_time: Int,
    is_success: T -> Bool
  ) {
    match breaker.state {
      CircuitState::Open => {
        // Check if timeout has passed
        if current_time - breaker.last_failure_time > breaker.timeout_ms {
          // Transition to half-open
          let updated_breaker = {
            state: CircuitState::HalfOpen,
            failure_count: breaker.failure_count,
            success_count: 0,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            timeout_ms: breaker.timeout_ms,
            last_failure_time: breaker.last_failure_time,
            last_state_change: current_time
          }
          
          // Try the operation
          let result = operation()
          if is_success(result) {
            // Success, transition to closed
            let final_breaker = {
              state: CircuitState::Closed,
              failure_count: 0,
              success_count: 1,
              failure_threshold: updated_breaker.failure_threshold,
              success_threshold: updated_breaker.success_threshold,
              timeout_ms: updated_breaker.timeout_ms,
              last_failure_time: updated_breaker.last_failure_time,
              last_state_change: current_time
            }
            
            (final_breaker, Some(result))
          } else {
            // Failure, open again
            let final_breaker = {
              state: CircuitState::Open,
              failure_count: updated_breaker.failure_count + 1,
              success_count: 0,
              failure_threshold: updated_breaker.failure_threshold,
              success_threshold: updated_breaker.success_threshold,
              timeout_ms: updated_breaker.timeout_ms,
              last_failure_time: current_time,
              last_state_change: current_time
            }
            
            (final_breaker, None)
          }
        } else {
          // Still open, reject operation
          (breaker, None)
        }
      }
      CircuitState::Closed => {
        // Try the operation
        let result = operation()
        if is_success(result) {
          // Success, stay closed
          let updated_breaker = {
            state: CircuitState::Closed,
            failure_count: 0,
            success_count: breaker.success_count + 1,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            timeout_ms: breaker.timeout_ms,
            last_failure_time: breaker.last_failure_time,
            last_state_change: breaker.last_state_change
          }
          
          (updated_breaker, Some(result))
        } else {
          // Failure, increment count
          let new_failure_count = breaker.failure_count + 1
          if new_failure_count >= breaker.failure_threshold {
            // Open the circuit
            let updated_breaker = {
              state: CircuitState::Open,
              failure_count: new_failure_count,
              success_count: 0,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: current_time,
              last_state_change: current_time
            }
            
            (updated_breaker, None)
          } else {
            // Stay closed
            let updated_breaker = {
              state: CircuitState::Closed,
              failure_count: new_failure_count,
              success_count: breaker.success_count,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: breaker.last_failure_time,
              last_state_change: breaker.last_state_change
            }
            
            (updated_breaker, None)
          }
        }
      }
      CircuitState::HalfOpen => {
        // Try the operation
        let result = operation()
        if is_success(result) {
          // Success, increment count
          let new_success_count = breaker.success_count + 1
          if new_success_count >= breaker.success_threshold {
            // Close the circuit
            let updated_breaker = {
              state: CircuitState::Closed,
              failure_count: 0,
              success_count: new_success_count,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: breaker.last_failure_time,
              last_state_change: current_time
            }
            
            (updated_breaker, Some(result))
          } else {
            // Stay half-open
            let updated_breaker = {
              state: CircuitState::HalfOpen,
              failure_count: breaker.failure_count,
              success_count: new_success_count,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: breaker.last_failure_time,
              last_state_change: breaker.last_state_change
            }
            
            (updated_breaker, Some(result))
          }
        } else {
          // Failure, open again
          let updated_breaker = {
            state: CircuitState::Open,
            failure_count: breaker.failure_count + 1,
            success_count: 0,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            timeout_ms: breaker.timeout_ms,
            last_failure_time: current_time,
            last_state_change: current_time
          }
          
          (updated_breaker, None)
        }
      }
    }
  }
  
  // Test circuit breaker creation
  let breaker = create_circuit_breaker(3, 2, 5000)  // 3 failures to open, 2 successes to close, 5s timeout
  match breaker.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker.failure_count, 0)
  assert_eq(breaker.success_count, 0)
  
  // Test successful operations
  let success_operation = fn() { "success" }
  let is_success = fn(result) { result == "success" }
  
  let (breaker1, result1) = execute_through_breaker(breaker, success_operation, 1640995201000, is_success)
  match result1 {
    Some(res) => assert_eq(res, "success")
    None => assert_true(false)
  }
  match breaker1.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker1.success_count, 1)
  
  // Test failures to open circuit
  let failure_operation = fn() { "failure" }
  let is_failure_success = fn(result) { result == "success" }
  
  let (breaker2, result2) = execute_through_breaker(breaker1, failure_operation, 1640995202000, is_failure_success)
  match result2 {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  match breaker2.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker2.failure_count, 1)
  
  let (breaker3, result3) = execute_through_breaker(breaker2, failure_operation, 1640995203000, is_failure_success)
  match result3 {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  match breaker3.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker3.failure_count, 2)
  
  let (breaker4, result4) = execute_through_breaker(breaker3, failure_operation, 1640995204000, is_failure_success)
  match result4 {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  match breaker4.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker4.failure_count, 3)
  
  // Test rejection when circuit is open
  let (breaker5, result5) = execute_through_breaker(breaker4, success_operation, 1640995204500, is_success)
  match result5 {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  match breaker5.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test half-open state after timeout
  let future_time = 1640995210000  // 10 minutes later
  let (breaker6, result6) = execute_through_breaker(breaker5, success_operation, future_time, is_success)
  match result6 {
    Some(res) => assert_eq(res, "success")
    None => assert_true(false)
  }
  match breaker6.state {
    CircuitState::HalfOpen => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker6.success_count, 1)
  
  // Test closing circuit after sufficient successes
  let (breaker7, result7) = execute_through_breaker(breaker6, success_operation, future_time + 1000, is_success)
  match result7 {
    Some(res) => assert_eq(res, "success")
    None => assert_true(false)
  }
  match breaker7.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker7.failure_count, 0)
  assert_eq(breaker7.success_count, 2)
}

// Test 3: Fallback Mechanism
test "fallback mechanism implementation" {
  // Define fallback strategy
  enum FallbackStrategy {
    PrimaryOnly
    PrimaryWithFallback
    FallbackOnly
    MultipleFallbacks
  }
  
  // Define operation result
  type FallbackResult[T] = {
    data: Option[T],
    source: String,
    success: Bool,
    error: Option[String]
  }
  
  // Define fallback manager
  type FallbackManager[T] = {
    primary: () -> FallbackResult[T],
    fallbacks: Array[() -> FallbackResult[T]],
    strategy: FallbackStrategy,
    attempts: Int,
    max_attempts: Int
  }
  
  // Create fallback manager
  let create_fallback_manager = fn[T](
    primary: () -> FallbackResult[T],
    fallbacks: Array[() -> FallbackResult[T]],
    strategy: FallbackStrategy,
    max_attempts: Int
  ) {
    {
      primary,
      fallbacks,
      strategy,
      attempts: 0,
      max_attempts
    }
  }
  
  // Execute with fallback
  let execute_with_fallback = fn[T](manager: FallbackManager[T]) {
    let mut updated_manager = manager
    let mut result = manager.primary()
    
    if !result.success && updated_manager.attempts < updated_manager.max_attempts {
      match manager.strategy {
        FallbackStrategy::PrimaryOnly => {
          // Don't use fallbacks
          updated_manager.attempts = updated_manager.attempts + 1
        }
        FallbackStrategy::PrimaryWithFallback => {
          // Try fallbacks in order
          for fallback in manager.fallbacks {
            if !result.success && updated_manager.attempts < updated_manager.max_attempts {
              result = fallback()
              updated_manager.attempts = updated_manager.attempts + 1
              if result.success {
                break
              }
            }
          }
        }
        FallbackStrategy::FallbackOnly => {
          // Skip primary, use only fallbacks
          for fallback in manager.fallbacks {
            if !result.success && updated_manager.attempts < updated_manager.max_attempts {
              result = fallback()
              updated_manager.attempts = updated_manager.attempts + 1
              if result.success {
                break
              }
            }
          }
        }
        FallbackStrategy::MultipleFallbacks => {
          // Try all fallbacks even if some succeed
          let mut any_success = false
          for fallback in manager.fallbacks {
            if updated_manager.attempts < updated_manager.max_attempts {
              let fallback_result = fallback()
              updated_manager.attempts = updated_manager.attempts + 1
              if fallback_result.success && !any_success {
                result = fallback_result
                any_success = true
              }
            }
          }
        }
      }
    }
    
    (updated_manager, result)
  }
  
  // Test fallback manager creation
  let primary_operation = fn() {
    {
      data: None,
      source: "primary",
      success: false,
      error: Some("Primary service unavailable")
    }
  }
  
  let fallback1 = fn() {
    {
      data: Some("fallback1 result"),
      source: "fallback1",
      success: true,
      error: None
    }
  }
  
  let fallback2 = fn() {
    {
      data: Some("fallback2 result"),
      source: "fallback2",
      success: true,
      error: None
    }
  }
  
  let failing_fallback = fn() {
    {
      data: None,
      source: "failing_fallback",
      success: false,
      error: Some("Fallback service also unavailable")
    }
  }
  
  let manager = create_fallback_manager(
    primary_operation,
    [fallback1, fallback2],
    FallbackStrategy::PrimaryWithFallback,
    3
  )
  
  assert_eq(manager.fallbacks.length(), 2)
  assert_eq(manager.attempts, 0)
  assert_eq(manager.max_attempts, 3)
  
  // Test primary with fallback strategy
  let (manager1, result1) = execute_with_fallback(manager)
  match result1.data {
    Some(data) => assert_eq(data, "fallback1 result")
    None => assert_true(false)
  }
  assert_eq(result1.source, "fallback1")
  assert_true(result1.success)
  assert_eq(manager1.attempts, 2)  // primary + fallback1
  
  // Test fallback only strategy
  let fallback_only_manager = create_fallback_manager(
    primary_operation,
    [fallback1, failing_fallback],
    FallbackStrategy::FallbackOnly,
    3
  )
  
  let (manager2, result2) = execute_with_fallback(fallback_only_manager)
  match result2.data {
    Some(data) => assert_eq(data, "fallback1 result")
    None => assert_true(false)
  }
  assert_eq(result2.source, "fallback1")
  assert_true(result2.success)
  assert_eq(manager2.attempts, 1)  // only fallback1
  
  // Test multiple fallbacks strategy
  let multiple_fallbacks_manager = create_fallback_manager(
    primary_operation,
    [fallback1, fallback2],
    FallbackStrategy::MultipleFallbacks,
    3
  )
  
  let (manager3, result3) = execute_with_fallback(multiple_fallbacks_manager)
  match result3.data {
    Some(data) => assert_eq(data, "fallback1 result")
    None => assert_true(false)
  }
  assert_eq(result3.source, "fallback1")
  assert_true(result3.success)
  assert_eq(manager3.attempts, 2)  // fallback1 + fallback2
  
  // Test all fallbacks failing
  let all_failing_manager = create_fallback_manager(
    primary_operation,
    [failing_fallback],
    FallbackStrategy::PrimaryWithFallback,
    2
  )
  
  let (manager4, result4) = execute_with_fallback(all_failing_manager)
  match result4.data {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  assert_false(result4.success)
  match result4.error {
    Some(error) => assert_eq(error, "Fallback service also unavailable")
    None => assert_true(false)
  }
  assert_eq(manager4.attempts, 2)  // primary + failing_fallback
}

// Test 4: Error Recovery and Self-Healing
test "error recovery and self-healing mechanisms" {
  // Define health status
  enum HealthStatus {
    Healthy
    Degraded
    Unhealthy
    Recovering
  }
  
  // Define health check
  type HealthCheck = {
    component: String,
    status: HealthStatus,
    last_check: Int,
    consecutive_failures: Int,
    consecutive_successes: Int,
    error_message: Option[String]
  }
  
  // Define recovery strategy
  type RecoveryStrategy = {
    name: String,
    action: () -> Bool,
    max_attempts: Int,
    cooldown_ms: Int
  }
  
  // Define self-healing manager
  type SelfHealingManager = {
    health_checks: Array[HealthCheck],
    recovery_strategies: Array[RecoveryStrategy],
    healing_enabled: Bool,
    auto_recovery_threshold: Int
  }
  
  // Create self-healing manager
  let create_self_healing_manager = fn(auto_recovery_threshold: Int) {
    {
      health_checks: [],
      recovery_strategies: [],
      healing_enabled: true,
      auto_recovery_threshold
    }
  }
  
  // Add health check
  let add_health_check = fn(manager: SelfHealingManager, component: String, initial_status: HealthStatus, current_time: Int) {
    let health_check = {
      component,
      status: initial_status,
      last_check: current_time,
      consecutive_failures: 0,
      consecutive_successes: 0,
      error_message: None
    }
    
    {
      health_checks: manager.health_checks.push(health_check),
      recovery_strategies: manager.recovery_strategies,
      healing_enabled: manager.healing_enabled,
      auto_recovery_threshold: manager.auto_recovery_threshold
    }
  }
  
  // Add recovery strategy
  let add_recovery_strategy = fn(manager: SelfHealingManager, strategy: RecoveryStrategy) {
    {
      health_checks: manager.health_checks,
      recovery_strategies: manager.recovery_strategies.push(strategy),
      healing_enabled: manager.healing_enabled,
      auto_recovery_threshold: manager.auto_recovery_threshold
    }
  }
  
  // Update health check
  let update_health_check = fn(manager: SelfHealingManager, component: String, status: HealthStatus, error_message: Option[String], current_time: Int) {
    match manager.health_checks.find_index(fn(check) { check.component == component }) {
      Some(index) => {
        let current_check = manager.health_checks[index]
        
        let (new_failures, new_successes) = match (current_check.status, status) {
          (HealthStatus::Healthy, HealthStatus::Unhealthy) => (current_check.consecutive_failures + 1, 0)
          (HealthStatus::Degraded, HealthStatus::Unhealthy) => (current_check.consecutive_failures + 1, 0)
          (HealthStatus::Unhealthy, HealthStatus::Healthy) => (0, current_check.consecutive_successes + 1)
          (HealthStatus::Unhealthy, HealthStatus::Degraded) => (0, current_check.consecutive_successes + 1)
          (HealthStatus::Degraded, HealthStatus::Healthy) => (0, current_check.consecutive_successes + 1)
          (HealthStatus::Recovering, HealthStatus::Healthy) => (0, current_check.consecutive_successes + 1)
          _ => (current_check.consecutive_failures, current_check.consecutive_successes)
        }
        
        let updated_check = {
          component,
          status,
          last_check: current_time,
          consecutive_failures: new_failures,
          consecutive_successes: new_successes,
          error_message
        }
        
        let updated_checks = manager.health_checks.update(index, updated_check)
        
        {
          health_checks: updated_checks,
          recovery_strategies: manager.recovery_strategies,
          healing_enabled: manager.healing_enabled,
          auto_recovery_threshold: manager.auto_recovery_threshold
        }
      }
      None => manager
    }
  }
  
  // Check if recovery is needed
  let check_recovery_needed = fn(manager: SelfHealingManager) {
    manager.health_checks.filter(fn(check) {
      check.status == HealthStatus::Unhealthy && 
      check.consecutive_failures >= manager.auto_recovery_threshold
    })
  }
  
  // Attempt recovery
  let attempt_recovery = fn(manager: SelfHealingManager, component: String, current_time: Int) {
    if !manager.healing_enabled {
      return (manager, false)
    }
    
    let unhealthy_checks = check_recovery_needed(manager)
    let component_unhealthy = unhealthy_checks.any(fn(check) { check.component == component })
    
    if component_unhealthy {
      // Try recovery strategies
      let mut recovery_successful = false
      
      for strategy in manager.recovery_strategies {
        if !recovery_successful {
          let strategy_result = strategy.action()
          if strategy_result {
            recovery_successful = true
            
            // Update health status to recovering
            let updated_manager = update_health_check(
              manager,
              component,
              HealthStatus::Recovering,
              Some("Recovery initiated"),
              current_time
            )
            
            return (updated_manager, true)
          }
        }
      }
      
      (manager, false)
    } else {
      (manager, false)
    }
  }
  
  // Test self-healing manager creation
  let manager = create_self_healing_manager(3)  // Auto-recover after 3 consecutive failures
  assert_eq(manager.health_checks.length(), 0)
  assert_eq(manager.recovery_strategies.length(), 0)
  assert_true(manager.healing_enabled)
  assert_eq(manager.auto_recovery_threshold, 3)
  
  // Add health checks
  let manager1 = add_health_check(manager, "database", HealthStatus::Healthy, 1640995200000)
  let manager2 = add_health_check(manager1, "cache", HealthStatus::Healthy, 1640995200000)
  
  assert_eq(manager2.health_checks.length(), 2)
  
  // Add recovery strategies
  let restart_database = {
    name: "restart_database",
    action: fn() { true },  // Simulate successful restart
    max_attempts: 3,
    cooldown_ms: 30000
  }
  
  let clear_cache = {
    name: "clear_cache",
    action: fn() { true },  // Simulate successful cache clear
    max_attempts: 1,
    cooldown_ms: 5000
  }
  
  let manager3 = add_recovery_strategy(manager2, restart_database)
  let manager4 = add_recovery_strategy(manager3, clear_cache)
  
  assert_eq(manager4.recovery_strategies.length(), 2)
  
  // Simulate component degradation and failures
  let manager5 = update_health_check(
    manager4,
    "database",
    HealthStatus::Degraded,
    Some("Slow response time"),
    1640995201000
  )
  
  let manager6 = update_health_check(
    manager5,
    "database",
    HealthStatus::Unhealthy,
    Some("Connection timeout"),
    1640995202000
  )
  
  let manager7 = update_health_check(
    manager6,
    "database",
    HealthStatus::Unhealthy,
    Some("Connection timeout"),
    1640995203000
  )
  
  // Check if recovery is needed
  let recovery_needed = check_recovery_needed(manager7)
  assert_eq(recovery_needed.length(), 1)
  assert_eq(recovery_needed[0].component, "database")
  assert_eq(recovery_needed[0].consecutive_failures, 3)
  
  // Attempt recovery
  let (manager8, recovery_successful) = attempt_recovery(manager7, "database", 1640995204000)
  assert_true(recovery_successful)
  
  // Check updated health status
  match manager8.health_checks.find(fn(check) { check.component == "database" }) {
    Some(check) => {
      match check.status {
        HealthStatus::Recovering => assert_true(true)
        _ => assert_true(false)
      }
      match check.error_message {
        Some(msg) => assert_eq(msg, "Recovery initiated")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Simulate successful recovery
  let manager9 = update_health_check(
    manager8,
    "database",
    HealthStatus::Healthy,
    None,
    1640995205000
  )
  
  match manager9.health_checks.find(fn(check) { check.component == "database" }) {
    Some(check) => {
      match check.status {
        HealthStatus::Healthy => assert_true(true)
        _ => assert_true(false)
      }
      assert_eq(check.consecutive_successes, 1)
      assert_eq(check.consecutive_failures, 0)
    }
    None => assert_true(false)
  }
}

// Test 5: Error Aggregation and Alerting
test "error aggregation and alerting" {
  // Define error severity
  enum ErrorSeverity {
    Low
    Medium
    High
    Critical
  }
  
  // Define error record
  type ErrorRecord = {
    id: String,
    error_type: String,
    message: String,
    severity: ErrorSeverity,
    timestamp: Int,
    component: String,
    context: Array[(String, String)]
  }
  
  // Define alert rule
  type AlertRule = {
    name: String,
    condition: Array[ErrorRecord] -> Bool,
    severity: ErrorSeverity,
    cooldown_ms: Int,
    last_triggered: Int,
    enabled: Bool
  }
  
  // Define error aggregator
  type ErrorAggregator = {
    errors: Array[ErrorRecord],
    alert_rules: Array[AlertRule],
    triggered_alerts: Int,
    suppressed_alerts: Int,
    max_errors: Int
  }
  
  // Create error aggregator
  let create_error_aggregator = fn(max_errors: Int) {
    {
      errors: [],
      alert_rules: [],
      triggered_alerts: 0,
      suppressed_alerts: 0,
      max_errors
    }
  }
  
  // Add error record
  let add_error = fn(aggregator: ErrorAggregator, error: ErrorRecord) {
    let updated_errors = if aggregator.errors.length() >= aggregator.max_errors {
      // Remove oldest error
      aggregator.errors.slice(1, aggregator.errors.length()).push(error)
    } else {
      aggregator.errors.push(error)
    }
    
    {
      errors: updated_errors,
      alert_rules: aggregator.alert_rules,
      triggered_alerts: aggregator.triggered_alerts,
      suppressed_alerts: aggregator.suppressed_alerts,
      max_errors: aggregator.max_errors
    }
  }
  
  // Add alert rule
  let add_alert_rule = fn(aggregator: ErrorAggregator, rule: AlertRule) {
    {
      errors: aggregator.errors,
      alert_rules: aggregator.alert_rules.push(rule),
      triggered_alerts: aggregator.triggered_alerts,
      suppressed_alerts: aggregator.suppressed_alerts,
      max_errors: aggregator.max_errors
    }
  }
  
  // Check alert rules
  let check_alerts = fn(aggregator: ErrorAggregator, current_time: Int) {
    let mut triggered_count = 0
    let mut suppressed_count = 0
    let mut updated_rules = []
    
    for rule in aggregator.alert_rules {
      if rule.enabled {
        let cooldown_passed = current_time - rule.last_triggered > rule.cooldown_ms
        
        if cooldown_passed && rule.condition(aggregator.errors) {
          triggered_count = triggered_count + 1
          updated_rules = updated_rules.push({ rule | last_triggered: current_time })
        } else if !cooldown_passed && rule.condition(aggregator.errors) {
          suppressed_count = suppressed_count + 1
          updated_rules = updated_rules.push(rule)
        } else {
          updated_rules = updated_rules.push(rule)
        }
      } else {
        updated_rules = updated_rules.push(rule)
      }
    }
    
    ({
      errors: aggregator.errors,
      alert_rules: updated_rules,
      triggered_alerts: aggregator.triggered_alerts + triggered_count,
      suppressed_alerts: aggregator.suppressed_alerts + suppressed_count,
      max_errors: aggregator.max_errors
    }, triggered_count)
  }
  
  // Test error aggregator creation
  let aggregator = create_error_aggregator(100)
  assert_eq(aggregator.errors.length(), 0)
  assert_eq(aggregator.alert_rules.length(), 0)
  assert_eq(aggregator.triggered_alerts, 0)
  assert_eq(aggregator.suppressed_alerts, 0)
  
  // Add error records
  let error1 = {
    id: "error-1",
    error_type: "DatabaseConnectionError",
    message: "Failed to connect to database",
    severity: ErrorSeverity::High,
    timestamp: 1640995200000,
    component: "database-service",
    context: [("host", "db.example.com"), ("port", "5432")]
  }
  
  let error2 = {
    id: "error-2",
    error_type: "CacheMissError",
    message: "Cache miss for key user-123",
    severity: ErrorSeverity::Low,
    timestamp: 1640995200100,
    component: "cache-service",
    context: [("key", "user-123"), ("cache", "user-cache")]
  }
  
  let error3 = {
    id: "error-3",
    error_type: "DatabaseConnectionError",
    message: "Failed to connect to database",
    severity: ErrorSeverity::High,
    timestamp: 1640995200200,
    component: "database-service",
    context: [("host", "db.example.com"), ("port", "5432")]
  }
  
  let aggregator1 = add_error(aggregator, error1)
  let aggregator2 = add_error(aggregator1, error2)
  let aggregator3 = add_error(aggregator2, error3)
  
  assert_eq(aggregator3.errors.length(), 3)
  
  // Add alert rules
  let high_error_count_rule = {
    name: "High Error Count",
    condition: fn(errors) { errors.length() >= 5 },
    severity: ErrorSeverity::Medium,
    cooldown_ms: 60000,
    last_triggered: 0,
    enabled: true
  }
  
  let database_error_rule = {
    name: "Database Errors",
    condition: fn(errors) { 
      errors.filter(fn(e) { e.error_type == "DatabaseConnectionError" }).length() >= 2 
    },
    severity: ErrorSeverity::High,
    cooldown_ms: 30000,
    last_triggered: 0,
    enabled: true
  }
  
  let aggregator4 = add_alert_rule(aggregator3, high_error_count_rule)
  let aggregator5 = add_alert_rule(aggregator4, database_error_rule)
  
  assert_eq(aggregator5.alert_rules.length(), 2)
  
  // Check alerts (should trigger database error rule)
  let (aggregator6, triggered_count) = check_alerts(aggregator5, 1640995201000)
  assert_eq(triggered_count, 1)
  assert_eq(aggregator6.triggered_alerts, 1)
  assert_eq(aggregator6.suppressed_alerts, 0)
  
  // Check that database error rule was triggered
  match aggregator6.alert_rules.find(fn(rule) { rule.name == "Database Errors" }) {
    Some(rule) => assert_eq(rule.last_triggered, 1640995201000)
    None => assert_true(false)
  }
  
  // Add more errors to trigger high error count rule
  let error4 = {
    id: "error-4",
    error_type: "ValidationError",
    message: "Invalid input parameter",
    severity: ErrorSeverity::Low,
    timestamp: 1640995200300,
    component: "api-service",
    context: [("parameter", "user_id"), ("value", "invalid")]
  }
  
  let error5 = {
    id: "error-5",
    error_type: "NetworkTimeoutError",
    message: "Request timeout after 30 seconds",
    severity: ErrorSeverity::Medium,
    timestamp: 1640995200400,
    component: "api-service",
    context: [("endpoint", "/api/users"), ("timeout", "30s")]
  }
  
  let aggregator7 = add_error(aggregator6, error4)
  let aggregator8 = add_error(aggregator7, error5)
  
  // Check alerts again (should trigger high error count rule)
  let (aggregator9, triggered_count2) = check_alerts(aggregator8, 1640995202000)
  assert_eq(triggered_count2, 1)
  assert_eq(aggregator9.triggered_alerts, 2)
  
  // Check that high error count rule was triggered
  match aggregator9.alert_rules.find(fn(rule) { rule.name == "High Error Count" }) {
    Some(rule) => assert_eq(rule.last_triggered, 1640995202000)
    None => assert_true(false)
  }
  
  // Test cooldown suppression
  let (aggregator10, triggered_count3) = check_alerts(aggregator9, 1640995202500)  // Within cooldown
  assert_eq(triggered_count3, 0)
  assert_eq(aggregator10.triggered_alerts, 2)  // No new alerts
  assert_eq(aggregator10.suppressed_alerts, 1)  // Database error rule suppressed
  
  // Test after cooldown period
  let (aggregator11, triggered_count4) = check_alerts(aggregator10, 1640995300000)  // After cooldown
  assert_eq(triggered_count4, 1)  // Database error rule triggers again
  assert_eq(aggregator11.triggered_alerts, 3)
}