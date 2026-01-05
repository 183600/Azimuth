// Azimuth Exception Boundary Recovery Tests
// This file contains test cases for exception boundary and recovery functionality

// Test 1: Basic Exception Boundary Detection
test "basic exception boundary detection" {
  let boundary_manager = ExceptionBoundaryManager::new()
  
  // Test normal operation (no exceptions)
  let normal_operation = fn() {
    return "success"
  }
  
  let result = ExceptionBoundaryManager::execute(boundary_manager, normal_operation)
  match result {
    OperationResult::Success(value) => assert_eq(value, "success")
    OperationResult::Error(_) => assert_true(false)
  }
  
  // Test operation that throws exception
  let throwing_operation = fn() {
    raise Exception::new("Test exception")
  }
  
  let error_result = ExceptionBoundaryManager::execute(boundary_manager, throwing_operation)
  match error_result {
    OperationResult::Success(_) => assert_true(false)
    OperationResult::Error(error) => {
      assert_eq(error.message, "Test exception")
      assert_eq(error.type, "Exception")
    }
  }
  
  // Test operation that returns error
  let error_operation = fn() {
    return Error::new("Operation failed")
  }
  
  let error_op_result = ExceptionBoundaryManager::execute(boundary_manager, error_operation)
  match error_op_result {
    OperationResult::Success(_) => assert_true(false)
    OperationResult::Error(error) => {
      assert_eq(error.message, "Operation failed")
    }
  }
  
  // Test nested operations
  let nested_operation = fn() {
    let inner_result = ExceptionBoundaryManager::execute(boundary_manager, normal_operation)
    match inner_result {
      OperationResult::Success(value) => return value + "_nested"
      OperationResult::Error(_) => raise Exception::new("Nested failed")
    }
  }
  
  let nested_result = ExceptionBoundaryManager::execute(boundary_manager, nested_operation)
  match nested_result {
    OperationResult::Success(value) => assert_eq(value, "success_nested")
    OperationResult::Error(_) => assert_true(false)
  }
}

// Test 2: Exception Recovery Strategies
test "exception recovery strategies" {
  let recovery_manager = ExceptionRecoveryManager::new()
  
  // Test retry strategy
  let mut attempt_count = 0
  let flaky_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      raise Exception::new("Temporary failure")
    }
    return "success_after_retry"
  }
  
  let retry_strategy = RetryStrategy::new()
    .with_max_attempts(3)
    .with_backoff(BackoffType::Fixed, 100)  // 100ms fixed delay
  
  let retry_result = RecoveryManager::execute_with_strategy(recovery_manager, flaky_operation, retry_strategy)
  match retry_result {
    OperationResult::Success(value) => assert_eq(value, "success_after_retry")
    OperationResult::Error(_) => assert_true(false)
  }
  
  assert_eq(attempt_count, 3)
  
  // Test fallback strategy
  let failing_operation = fn() {
    raise Exception::new("Primary operation failed")
  }
  
  let fallback_operation = fn() {
    return "fallback_success"
  }
  
  let fallback_strategy = FallbackStrategy::new(fallback_operation)
  let fallback_result = RecoveryManager::execute_with_strategy(recovery_manager, failing_operation, fallback_strategy)
  match fallback_result {
    OperationResult::Success(value) => assert_eq(value, "fallback_success")
    OperationResult::Error(_) => assert_true(false)
  }
  
  // Test circuit breaker strategy
  let circuit_breaker = CircuitBreakerStrategy::new()
    .with_failure_threshold(2)
    .with_recovery_timeout(1000)  // 1 second recovery timeout
  
  let always_failing = fn() {
    raise Exception::new("Always fails")
  }
  
  // First two attempts should trigger circuit breaker
  RecoveryManager::execute_with_strategy(recovery_manager, always_failing, circuit_breaker)
  RecoveryManager::execute_with_strategy(recovery_manager, always_failing, circuit_breaker)
  
  // Third attempt should fail immediately due to open circuit
  let circuit_result = RecoveryManager::execute_with_strategy(recovery_manager, always_failing, circuit_breaker)
  match circuit_result {
    OperationResult::Success(_) => assert_true(false)
    OperationResult::Error(error) => {
      assert_eq(error.type, "CircuitBreakerOpen")
    }
  }
  
  // Test timeout strategy
  let slow_operation = fn() {
    Clock::sleep(200)  // 200ms operation
    return "slow_success"
  }
  
  let timeout_strategy = TimeoutStrategy::new(100)  // 100ms timeout
  let timeout_result = RecoveryManager::execute_with_strategy(recovery_manager, slow_operation, timeout_strategy)
  match timeout_result {
    OperationResult::Success(_) => assert_true(false)
    OperationResult::Error(error) => {
      assert_eq(error.type, "Timeout")
    }
  }
}

// Test 3: Exception Classification and Handling
test "exception classification and handling" {
  let classifier = ExceptionClassifier::new()
  
  // Define exception categories
  classifier.add_category("network", [
    "NetworkException",
    "TimeoutException",
    "ConnectionException"
  ])
  
  classifier.add_category("database", [
    "DatabaseException",
    "QueryException",
    "ConnectionPoolException"
  ])
  
  classifier.add_category("business", [
    "ValidationException",
    "BusinessRuleException",
    "AuthorizationException"
  ])
  
  // Test classification
  let network_exception = Exception::new("Network failure")
  network_exception.type = "NetworkException"
  
  let network_category = classifier.classify(network_exception)
  assert_eq(network_category, "network")
  
  let db_exception = Exception::new("Database error")
  db_exception.type = "QueryException"
  
  let db_category = classifier.classify(db_exception)
  assert_eq(db_category, "database")
  
  let business_exception = Exception::new("Validation failed")
  business_exception.type = "ValidationException"
  
  let business_category = classifier.classify(business_exception)
  assert_eq(business_category, "business")
  
  // Test unknown exception
  let unknown_exception = Exception::new("Unknown error")
  unknown_exception.type = "UnknownException"
  
  let unknown_category = classifier.classify(unknown_exception)
  assert_eq(unknown_category, "unclassified")
  
  // Test category-specific handlers
  let handler_manager = ExceptionHandlerManager::new()
  
  let network_handler = fn(exception) {
    assert_eq(exception.type, "NetworkException")
    return "network_handled"
  }
  
  let database_handler = fn(exception) {
    assert_eq(exception.type, "QueryException")
    return "database_handled"
  }
  
  let default_handler = fn(exception) {
    return "default_handled"
  }
  
  handler_manager.add_handler("network", network_handler)
  handler_manager.add_handler("database", database_handler)
  handler_manager.set_default_handler(default_handler)
  
  // Test handlers
  let network_result = handler_manager.handle(network_exception)
  assert_eq(network_result, "network_handled")
  
  let db_result = handler_manager.handle(db_exception)
  assert_eq(db_result, "database_handled")
  
  let business_result = handler_manager.handle(business_exception)
  assert_eq(business_result, "default_handled")  // Uses default handler
}

// Test 4: Exception Context and Propagation
test "exception context and propagation" {
  let context_manager = ExceptionContextManager::new()
  
  // Test context creation
  let base_context = context_manager.create_context("operation1")
  context_manager.add_data(base_context, "user_id", "12345")
  context_manager.add_data(base_context, "request_id", "req-abc-123")
  
  // Test exception with context
  let contextual_operation = fn() {
    let current_context = context_manager.get_current_context()
    context_manager.add_data(current_context, "step", "processing")
    
    raise Exception::with_context("Processing failed", current_context)
  }
  
  context_manager.set_context(base_context)
  let result = context_manager.execute(contextual_operation)
  
  match result {
    OperationResult::Error(error) => {
      assert_eq(error.message, "Processing failed")
      assert_eq(error.context.get("user_id"), Some("12345"))
      assert_eq(error.context.get("request_id"), Some("req-abc-123"))
      assert_eq(error.context.get("step"), Some("processing"))
    }
    _ => assert_true(false)
  }
  
  // Test context propagation through nested calls
  let inner_operation = fn() {
    let current_context = context_manager.get_current_context()
    context_manager.add_data(current_context, "inner_step", "validation")
    
    raise Exception::with_context("Inner validation failed", current_context)
  }
  
  let outer_operation = fn() {
    let current_context = context_manager.get_current_context()
    context_manager.add_data(current_context, "outer_step", "preprocessing")
    
    return context_manager.execute(inner_operation)
  }
  
  let nested_result = context_manager.execute(outer_operation)
  
  match nested_result {
    OperationResult::Error(error) => {
      assert_eq(error.message, "Inner validation failed")
      assert_eq(error.context.get("user_id"), Some("12345"))
      assert_eq(error.context.get("request_id"), Some("req-abc-123"))
      assert_eq(error.context.get("outer_step"), Some("preprocessing"))
      assert_eq(error.context.get("inner_step"), Some("validation"))
    }
    _ => assert_true(false)
  }
  
  // Test context isolation
  let isolated_operation = fn() {
    let isolated_context = context_manager.create_isolated_context("isolated_op")
    context_manager.add_data(isolated_context, "isolated_data", "secret")
    
    return context_manager.execute_in_context(isolated_context, fn() {
      raise Exception::with_context("Isolated failure", isolated_context)
    })
  }
  
  let isolated_result = context_manager.execute(isolated_operation)
  
  match isolated_result {
    OperationResult::Error(error) => {
      assert_eq(error.context.get("isolated_data"), Some("secret"))
      assert_eq(error.context.get("user_id"), None)  // Should not leak from parent context
    }
    _ => assert_true(false)
  }
}

// Test 5: Exception Aggregation and Analysis
test "exception aggregation and analysis" {
  let aggregator = ExceptionAggregator::new()
  
  // Generate various exceptions
  let network_exception1 = Exception::new("Network timeout")
  network_exception1.type = "NetworkException"
  network_exception1.timestamp = Clock::now()
  
  let network_exception2 = Exception::new("Connection refused")
  network_exception2.type = "NetworkException"
  network_exception2.timestamp = Clock::now()
  
  let db_exception = Exception::new("Query failed")
  db_exception.type = "DatabaseException"
  db_exception.timestamp = Clock::now()
  
  let validation_exception = Exception::new("Invalid input")
  validation_exception.type = "ValidationException"
  validation_exception.timestamp = Clock::now()
  
  // Add exceptions to aggregator
  aggregator.add_exception(network_exception1)
  aggregator.add_exception(network_exception2)
  aggregator.add_exception(db_exception)
  aggregator.add_exception(validation_exception)
  
  // Add duplicate network exception to test frequency
  aggregator.add_exception(network_exception1)
  
  // Test exception counting
  let total_count = aggregator.get_total_count()
  assert_eq(total_count, 5)
  
  // Test exception type distribution
  let type_distribution = aggregator.get_type_distribution()
  assert_eq(type_distribution.get("NetworkException"), Some(3))
  assert_eq(type_distribution.get("DatabaseException"), Some(1))
  assert_eq(type_distribution.get("ValidationException"), Some(1))
  
  // Test most frequent exceptions
  let most_frequent = aggregator.get_most_frequent(3)
  assert_eq(most_frequent.length(), 2)  // Only 2 unique types
  assert_eq(most_frequent[0].exception_type, "NetworkException")
  assert_eq(most_frequent[0].count, 3)
  
  // Test exception rate calculation
  let time_window = 60000  // 1 minute window
  let rate = aggregator.get_exception_rate(time_window)
  assert_eq(rate > 0, true)
  
  // Test exception pattern detection
  let patterns = aggregator.detect_patterns()
  assert_eq(patterns.length(), 1)  // Should detect network exception pattern
  
  let network_pattern = patterns[0]
  assert_eq(network_pattern.type, "FrequencyPattern")
  assert_eq(network_pattern.exception_type, "NetworkException")
  assert_eq(network_pattern.count, 3)
  
  // Test exception filtering
  let network_exceptions = aggregator.filter_by_type("NetworkException")
  assert_eq(network_exceptions.length(), 3)
  
  let recent_exceptions = aggregator.filter_by_time_range(
    Clock::now() - 30000,  // Last 30 seconds
    Clock::now()
  )
  assert_eq(recent_exceptions.length(), 5)  // All exceptions are recent
}

// Test 6: Exception Recovery with State Management
test "exception recovery with state management" {
  let state_manager = RecoveryStateManager::new()
  
  // Define operation with state
  let stateful_operation = fn() {
    let current_state = state_manager.get_state()
    
    match current_state.get("attempt") {
      Some(attempt) => {
        let attempt_num = attempt.to_int()
        if attempt_num < 3 {
          state_manager.update_state("attempt", (attempt_num + 1).to_string())
          raise Exception::new("Attempt " + attempt_num + " failed")
        }
        return "success_after_" + attempt_num.to_string() + "_attempts"
      }
      None => {
        state_manager.update_state("attempt", "1")
        raise Exception::new("First attempt failed")
      }
    }
  }
  
  // Execute with stateful recovery
  let recovery_strategy = RetryStrategy::new()
    .with_max_attempts(5)
    .with_backoff(BackoffType::Exponential, 100)
  
  let result = state_manager.execute_with_recovery(stateful_operation, recovery_strategy)
  
  match result {
    OperationResult::Success(value) => assert_eq(value, "success_after_3_attempts")
    OperationResult::Error(_) => assert_true(false)
  }
  
  // Verify final state
  let final_state = state_manager.get_state()
  assert_eq(final_state.get("attempt"), Some("3"))
  
  // Test state rollback on failure
  let rollback_operation = fn() {
    state_manager.save_checkpoint("before_operation")
    state_manager.update_state("data", "modified")
    
    raise Exception::new("Operation failed, should rollback")
  }
  
  let rollback_result = state_manager.execute_with_rollback(rollback_operation)
  
  match rollback_result {
    OperationResult::Error(_) => assert_true(true)  // Expected to fail
    OperationResult::Success(_) => assert_true(false)
  }
  
  // State should be rolled back
  let rolled_back_state = state_manager.get_state()
  assert_eq(rolled_back_state.get("data"), None)  // Should be rolled back
  
  // Test state persistence
  state_manager.update_state("persistent_data", "should_persist")
  state_manager.persist_state("test_state")
  
  // Create new state manager and restore
  let new_state_manager = RecoveryStateManager::new()
  new_state_manager.restore_state("test_state")
  
  let restored_state = new_state_manager.get_state()
  assert_eq(restored_state.get("persistent_data"), Some("should_persist"))
}

// Test 7: Exception Boundary in Concurrent Operations
test "exception boundary in concurrent operations" {
  let concurrent_manager = ConcurrentExceptionManager::new()
  
  // Define operations that may fail concurrently
  let operation1 = fn() {
    Clock::sleep(50)
    return "op1_success"
  }
  
  let operation2 = fn() {
    Clock::sleep(30)
    raise Exception::new("Operation 2 failed")
  }
  
  let operation3 = fn() {
    Clock::sleep(70)
    return "op3_success"
  }
  
  let operation4 = fn() {
    Clock::sleep(20)
    raise Exception::new("Operation 4 failed")
  }
  
  // Execute operations concurrently
  let operations = [operation1, operation2, operation3, operation4]
  let concurrent_results = concurrent_manager.execute_concurrent(operations)
  
  assert_eq(concurrent_results.length(), 4)
  
  // Check results
  let mut success_count = 0
  let mut error_count = 0
  
  for result in concurrent_results {
    match result {
      OperationResult::Success(value) => {
        success_count = success_count + 1
        assert_eq(value == "op1_success" || value == "op3_success", true)
      }
      OperationResult::Error(error) => {
        error_count = error_count + 1
        assert_eq(error.message == "Operation 2 failed" || error.message == "Operation 4 failed", true)
      }
    }
  }
  
  assert_eq(success_count, 2)
  assert_eq(error_count, 2)
  
  // Test exception isolation in concurrent operations
  let isolated_operation1 = fn() {
    Clock::sleep(40)
    raise Exception::new("Isolated failure 1")
  }
  
  let isolated_operation2 = fn() {
    Clock::sleep(60)
    return "isolated_success_2"
  }
  
  let isolated_operations = [isolated_operation1, isolated_operation2]
  let isolated_results = concurrent_manager.execute_concurrent_isolated(isolated_operations)
  
  // Even with isolation, we should still get both results
  assert_eq(isolated_results.length(), 2)
  
  // Test concurrent exception aggregation
  let mut aggregated_exceptions = []
  for result in isolated_results {
    match result {
      OperationResult::Error(error) => aggregated_exceptions.push(error)
      _ => assert_true(true)
    }
  }
  
  assert_eq(aggregated_exceptions.length(), 1)
  assert_eq(aggregated_exceptions[0].message, "Isolated failure 1")
  
  // Test concurrent recovery coordination
  let coordinator_operation1 = fn() {
    Clock::sleep(30)
    raise Exception::new("Coordinator failure 1")
  }
  
  let coordinator_operation2 = fn() {
    Clock::sleep(50)
    return "coordinator_success_2"
  }
  
  let coordinator_operations = [coordinator_operation1, coordinator_operation2]
  let coordinator_strategy = CoordinatedRetryStrategy::new()
    .with_max_attempts(2)
    .with_coordination_timeout(200)  // 200ms coordination timeout
  
  let coordinated_results = concurrent_manager.execute_concurrent_with_coordination(
    coordinator_operations,
    coordinator_strategy
  )
  
  // Should have results for both operations
  assert_eq(coordinated_results.length(), 2)
}

// Test 8: Exception Boundary with Resource Management
test "exception boundary with resource management" {
  let resource_manager = ResourceManager::new()
  
  // Test resource cleanup on exception
  let resource_using_operation = fn() {
    let resource = resource_manager.acquire("database_connection")
    
    // Simulate operation that fails
    raise Exception::new("Database operation failed")
    
    // This should not be reached due to exception
    resource_manager.release(resource)
  }
  
  let boundary_manager = ExceptionBoundaryManager::new()
  boundary_manager.register_resource_manager(resource_manager)
  
  let result = boundary_manager.execute_with_cleanup(resource_using_operation)
  
  match result {
    OperationResult::Error(error) => {
      assert_eq(error.message, "Database operation failed")
    }
    _ => assert_true(false)
  }
  
  // Resource should be automatically cleaned up despite exception
  assert_eq(resource_manager.is_acquired("database_connection"), false)
  
  // Test multiple resource management
  let multi_resource_operation = fn() {
    let db_resource = resource_manager.acquire("database_connection")
    let file_resource = resource_manager.acquire("file_handle")
    let network_resource = resource_manager.acquire("network_socket")
    
    // Simulate partial failure
    raise Exception::new("Multi-resource operation failed")
  }
  
  let multi_result = boundary_manager.execute_with_cleanup(multi_resource_operation)
  
  match multi_result {
    OperationResult::Error(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  // All resources should be cleaned up
  assert_eq(resource_manager.is_acquired("database_connection"), false)
  assert_eq(resource_manager.is_acquired("file_handle"), false)
  assert_eq(resource_manager.is_acquired("network_socket"), false)
  
  // Test resource leak detection
  let leaky_operation = fn() {
    let resource = resource_manager.acquire("leaky_resource")
    // Forget to release resource (simulating leak)
    return "success_but_leaky"
  }
  
  let leaky_result = boundary_manager.execute_with_cleanup(leaky_operation)
  
  match leaky_result {
    OperationResult::Success(value) => assert_eq(value, "success_but_leaky")
    _ => assert_true(false)
  }
  
  // Even though operation succeeded, boundary manager should detect and clean up leaks
  assert_eq(resource_manager.is_acquired("leaky_resource"), false)
  
  // Test resource acquisition timeout
  let slow_operation = fn() {
    let resource = resource_manager.acquire_with_timeout("slow_resource", 50)  // 50ms timeout
    return "slow_success"
  }
  
  let slow_result = boundary_manager.execute_with_cleanup(slow_operation)
  
  match slow_result {
    OperationResult::Error(error) => {
      assert_eq(error.type, "ResourceAcquisitionTimeout")
    }
    _ => assert_true(false)
  }
}

// Test 9: Exception Boundary with Monitoring and Alerting
test "exception boundary with monitoring and alerting" {
  let monitoring_manager = ExceptionMonitoringManager::new()
  
  // Configure alerting rules
  let network_alert_rule = AlertRule::new()
    .with_exception_type("NetworkException")
    .with_threshold(3)  // Alert after 3 network exceptions
    .with_time_window(60000)  // Within 1 minute
    .with_alert_level(AlertLevel::Warning)
  
  let critical_alert_rule = AlertRule::new()
    .with_exception_type("DatabaseException")
    .with_threshold(1)  // Alert immediately for database exceptions
    .with_time_window(300000)  // Within 5 minutes
    .with_alert_level(AlertLevel::Critical)
  
  monitoring_manager.add_alert_rule(network_alert_rule)
  monitoring_manager.add_alert_rule(critical_alert_rule)
  
  // Test exception monitoring
  let boundary_manager = ExceptionBoundaryManager::new()
  boundary_manager.set_monitoring_manager(monitoring_manager)
  
  // Generate network exceptions
  for i = 0; i < 3; i = i + 1 {
    let network_exception = Exception::new("Network failure " + (i + 1).to_string())
    network_exception.type = "NetworkException"
    
    let failing_operation = fn() {
      raise network_exception
    }
    
    boundary_manager.execute(failing_operation)
  }
  
  // Should trigger network alert
  let alerts = monitoring_manager.get_active_alerts()
  assert_eq(alerts.length(), 1)
  assert_eq(alerts[0].level, AlertLevel::Warning)
  assert_eq(alerts[0].exception_type, "NetworkException")
  assert_eq(alerts[0].count, 3)
  
  // Generate database exception
  let db_exception = Exception::new("Database connection failed")
  db_exception.type = "DatabaseException"
  
  let db_failing_operation = fn() {
    raise db_exception
  }
  
  boundary_manager.execute(db_failing_operation)
  
  // Should trigger critical alert
  let updated_alerts = monitoring_manager.get_active_alerts()
  assert_eq(updated_alerts.length(), 2)
  
  let mut critical_alert_found = false
  for alert in updated_alerts {
    if alert.level == AlertLevel::Critical && alert.exception_type == "DatabaseException" {
      critical_alert_found = true
      break
    }
  }
  assert_eq(critical_alert_found, true)
  
  // Test exception metrics collection
  let metrics = monitoring_manager.get_metrics()
  assert_eq(metrics.total_exceptions, 4)
  assert_eq(metrics.exceptions_by_type.get("NetworkException"), Some(3))
  assert_eq(metrics.exceptions_by_type.get("DatabaseException"), Some(1))
  assert_eq(metrics.active_alerts, 2)
  
  // Test alert resolution
  Clock::sleep(100)  // Wait a bit
  
  // Reset exception count (simulating recovery)
  monitoring_manager.reset_exception_count("NetworkException")
  
  let resolved_alerts = monitoring_manager.get_resolved_alerts()
  assert_eq(resolved_alerts.length(), 1)
  assert_eq(resolved_alerts[0].exception_type, "NetworkException")
}

// Test 10: Exception Boundary with Custom Recovery Policies
test "exception boundary with custom recovery policies" {
  let policy_manager = RecoveryPolicyManager::new()
  
  // Define custom recovery policies
  let network_recovery_policy = CustomRecoveryPolicy::new("NetworkException")
    .with_max_attempts(5)
    .with_backoff_strategy(BackoffStrategy::Exponential(100, 2000))  // 100ms to 2s
    .with_retry_condition(fn(exception) {
      return exception.message.contains("timeout") || exception.message.contains("connection")
    })
    .with_on_retry fn(attempt, exception) {
      // Log retry attempt
      assert_eq(attempt > 0, true)
      assert_eq(exception.type, "NetworkException")
    })
    .with_on_failure fn(exception, attempts) {
      // Handle final failure
      assert_eq(attempts, 5)
    }
  
  let database_recovery_policy = CustomRecoveryPolicy::new("DatabaseException")
    .with_max_attempts(3)
    .with_backoff_strategy(BackoffStrategy::Fixed(500))  // 500ms fixed
    .with_fallback_operation fn(exception) {
      return "fallback_database_result"
    }
  
  policy_manager.add_policy(network_recovery_policy)
  policy_manager.add_policy(database_recovery_policy)
  
  let boundary_manager = ExceptionBoundaryManager::new()
  boundary_manager.set_policy_manager(policy_manager)
  
  // Test network recovery with custom policy
  let mut network_attempts = 0
  let network_operation = fn() {
    network_attempts = network_attempts + 1
    if network_attempts < 4 {
      let exception = Exception::new("Network timeout occurred")
      exception.type = "NetworkException"
      raise exception
    }
    return "network_success_after_" + network_attempts.to_string() + "_attempts"
  }
  
  let network_result = boundary_manager.execute_with_policy(network_operation)
  match network_result {
    OperationResult::Success(value) => assert_eq(value, "network_success_after_4_attempts")
    OperationResult::Error(_) => assert_true(false)
  }
  
  // Test database recovery with fallback
  let mut db_attempts = 0
  let database_operation = fn() {
    db_attempts = db_attempts + 1
    let exception = Exception::new("Database connection failed")
    exception.type = "DatabaseException"
    raise exception
  }
  
  let db_result = boundary_manager.execute_with_policy(database_operation)
  match db_result {
    OperationResult::Success(value) => assert_eq(value, "fallback_database_result")
    OperationResult::Error(_) => assert_true(false)
  }
  
  assert_eq(db_attempts, 3)  // Should have attempted 3 times before fallback
  
  // Test policy chaining
  let chained_policy = ChainedRecoveryPolicy::new()
    .add_policy(RetryStrategy::new().with_max_attempts(2))
    .add_policy(FallbackStrategy::new(fn() { return "chained_fallback" }))
  
  policy_manager.add_policy("ChainedException", chained_policy)
  
  let mut chained_attempts = 0
  let chained_operation = fn() {
    chained_attempts = chained_attempts + 1
    let exception = Exception::new("Chained operation failed")
    exception.type = "ChainedException"
    raise exception
  }
  
  let chained_result = boundary_manager.execute_with_policy(chained_operation)
  match chained_result {
    OperationResult::Success(value) => assert_eq(value, "chained_fallback")
    OperationResult::Error(_) => assert_true(false)
  }
  
  assert_eq(chained_attempts, 2)  // Should have retried 2 times before fallback
  
  // Test policy effectiveness metrics
  let policy_metrics = policy_manager.get_effectiveness_metrics()
  
  assert_eq(policy_metrics.total_policies, 3)
  assert_eq(policy_metrics.policy_success_rate.get("NetworkException"), Some(1.0))  // 100% success
  assert_eq(policy_metrics.policy_success_rate.get("DatabaseException"), Some(1.0))  // 100% success via fallback
  assert_eq(policy_metrics.policy_success_rate.get("ChainedException"), Some(1.0))  // 100% success via fallback
}