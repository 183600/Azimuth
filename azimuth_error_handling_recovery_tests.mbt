// Azimuth Telemetry System - Error Handling and Recovery Tests
// This file contains test cases for error handling and recovery functionality

// Test 1: Error Classification and Categorization
test "error classification and categorization" {
  // Create different types of errors
  let network_error = NetworkError::new("Connection timeout", ErrorSeverity::High, ErrorCategory::Transient)
  let database_error = DatabaseError::new("Connection failed", ErrorSeverity::Critical, ErrorCategory::Persistent)
  let validation_error = ValidationError::new("Invalid input", ErrorSeverity::Low, ErrorCategory::UserError)
  let system_error = SystemError::new("Out of memory", ErrorSeverity::Critical, ErrorCategory::System)
  
  // Verify error classification
  assert_eq(Error::type_name(network_error), "NetworkError")
  assert_eq(Error::type_name(database_error), "DatabaseError")
  assert_eq(Error::type_name(validation_error), "ValidationError")
  assert_eq(Error::type_name(system_error), "SystemError")
  
  // Verify error severity
  assert_eq(Error::severity(network_error), ErrorSeverity::High)
  assert_eq(Error::severity(database_error), ErrorSeverity::Critical)
  assert_eq(Error::severity(validation_error), ErrorSeverity::Low)
  assert_eq(Error::severity(system_error), ErrorSeverity::Critical)
  
  // Verify error category
  assert_eq(Error::category(network_error), ErrorCategory::Transient)
  assert_eq(Error::category(database_error), ErrorCategory::Persistent)
  assert_eq(Error::category(validation_error), ErrorCategory::UserError)
  assert_eq(Error::category(system_error), ErrorCategory::System)
  
  // Verify error messages
  assert_eq(Error::message(network_error), "Connection timeout")
  assert_eq(Error::message(database_error), "Connection failed")
  assert_eq(Error::message(validation_error), "Invalid input")
  assert_eq(Error::message(system_error), "Out of memory")
}

// Test 2: Error Context and Metadata
test "error context and metadata" {
  // Create error with context
  let error = DatabaseError::new("Query failed", ErrorSeverity::Medium, ErrorCategory::Transient)
  
  // Add context information
  let context = ErrorContext::new()
  ErrorContext::add(context, "query", "SELECT * FROM users WHERE id = ?")
  ErrorContext::add(context, "user_id", "12345")
  ErrorContext::add(context, "timestamp", "1640995200000")
  ErrorContext::add(context, "retry_count", "3")
  
  // Attach context to error
  let error_with_context = Error::with_context(error, context)
  
  // Verify context is preserved
  let retrieved_context = Error::context(error_with_context)
  match ErrorContext::get(retrieved_context, "query") {
    Some(value) => assert_eq(value, "SELECT * FROM users WHERE id = ?")
    None => assert_true(false)
  }
  
  match ErrorContext::get(retrieved_context, "user_id") {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  // Verify error serialization with context
  let serialized = Error::to_json(error_with_context)
  assert_true(serialized.contains("Query failed"))
  assert_true(serialized.contains("SELECT * FROM users WHERE id = ?"))
  assert_true(serialized.contains("user_id"))
}

// Test 3: Error Recovery Strategies
test "error recovery strategies" {
  // Create error recovery manager
  let recovery_manager = ErrorRecoveryManager::new()
  
  // Register recovery strategies for different error types
  let retry_strategy = RetryStrategy::new(3, 1000)  // 3 retries with 1s delay
  let fallback_strategy = FallbackStrategy::new("backup_database")
  let circuit_breaker_strategy = CircuitBreakerStrategy::new(5, 60000)  // 5 failures, 1min timeout
  
  ErrorRecoveryManager::register_strategy(recovery_manager, "NetworkError", retry_strategy)
  ErrorRecoveryManager::register_strategy(recovery_manager, "DatabaseError", fallback_strategy)
  ErrorRecoveryManager::register_strategy(recovery_manager, "ServiceError", circuit_breaker_strategy)
  
  // Test retry strategy
  let network_error = NetworkError::new("Connection timeout", ErrorSeverity::High, ErrorCategory::Transient)
  let retry_result = ErrorRecoveryManager::handle_error(recovery_manager, network_error, || {
    // Simulate operation that might fail
    true  // Success on retry
  })
  
  match retry_result {
    RecoveryResult::Success => assert_true(true)
    RecoveryResult::Failed => assert_true(false)
    RecoveryResult::RetriesExhausted => assert_true(false)
  }
  
  // Test fallback strategy
  let database_error = DatabaseError::new("Primary DB down", ErrorSeverity::Critical, ErrorCategory::Persistent)
  let fallback_result = ErrorRecoveryManager::handle_error(recovery_manager, database_error, || {
    // Simulate operation that fails
    false  // Primary operation fails
  })
  
  match fallback_result {
    RecoveryResult::FallbackUsed(fallback) => assert_eq(fallback, "backup_database")
    _ => assert_true(false)
  }
  
  // Test circuit breaker strategy
  let service_error = ServiceError::new("Service unavailable", ErrorSeverity::Medium, ErrorCategory::Transient)
  
  // Simulate multiple failures to trigger circuit breaker
  for i in 0..=5 {
    let result = ErrorRecoveryManager::handle_error(recovery_manager, service_error, || {
      false  // Operation fails
    })
    
    if i < 4 {
      match result {
        RecoveryResult::Failed => assert_true(true)
        _ => assert_true(false)
      }
    } else {
      match result {
        RecoveryResult::CircuitBreakerOpen => assert_true(true)
        _ => assert_true(false)
      }
    }
  }
}

// Test 4: Error Aggregation and Analysis
test "error aggregation and analysis" {
  // Create error aggregator
  let error_aggregator = ErrorAggregator::new()
  
  // Add various errors
  let errors = [
    NetworkError::new("Timeout", ErrorSeverity::Medium, ErrorCategory::Transient),
    DatabaseError::new("Connection failed", ErrorSeverity::High, ErrorCategory::Persistent),
    NetworkError::new("DNS resolution failed", ErrorSeverity::Low, ErrorCategory::Transient),
    ValidationError::new("Invalid input", ErrorSeverity::Low, ErrorCategory::UserError),
    DatabaseError::new("Query timeout", ErrorSeverity::Medium, ErrorCategory::Transient),
    SystemError::new("Disk full", ErrorSeverity::Critical, ErrorCategory::System),
    NetworkError::new("Connection refused", ErrorSeverity::Medium, ErrorCategory::Transient)
  ]
  
  // Add errors to aggregator
  for error in errors {
    ErrorAggregator::add_error(error_aggregator, error)
  }
  
  // Analyze errors
  let analysis = ErrorAggregator::analyze(error_aggregator)
  
  // Verify error counts by type
  assert_eq(ErrorAnalysis::count_by_type(analysis, "NetworkError"), 3)
  assert_eq(ErrorAnalysis::count_by_type(analysis, "DatabaseError"), 2)
  assert_eq(ErrorAnalysis::count_by_type(analysis, "ValidationError"), 1)
  assert_eq(ErrorAnalysis::count_by_type(analysis, "SystemError"), 1)
  
  // Verify error counts by severity
  assert_eq(ErrorAnalysis::count_by_severity(analysis, ErrorSeverity::Critical), 1)
  assert_eq(ErrorAnalysis::count_by_severity(analysis, ErrorSeverity::High), 1)
  assert_eq(ErrorAnalysis::count_by_severity(analysis, ErrorSeverity::Medium), 3)
  assert_eq(ErrorAnalysis::count_by_severity(analysis, ErrorSeverity::Low), 2)
  
  // Verify error counts by category
  assert_eq(ErrorAnalysis::count_by_category(analysis, ErrorCategory::Transient), 4)
  assert_eq(ErrorAnalysis::count_by_category(analysis, ErrorCategory::Persistent), 2)
  assert_eq(ErrorAnalysis::count_by_category(analysis, ErrorCategory::UserError), 1)
  assert_eq(ErrorAnalysis::count_by_category(analysis, ErrorCategory::System), 1)
  
  // Verify most common error
  match ErrorAnalysis::most_common_error(analysis) {
    Some((error_type, count)) => {
      assert_eq(error_type, "NetworkError")
      assert_eq(count, 3)
    }
    None => assert_true(false)
  }
}

// Test 5: Error Rate Monitoring and Alerting
test "error rate monitoring and alerting" {
  // Create error rate monitor
  let error_monitor = ErrorRateMonitor::new()
  
  // Configure alerting thresholds
  ErrorRateMonitor::set_threshold(error_monitor, "error_rate", 0.1)  // 10% error rate
  ErrorRateMonitor::set_threshold(error_monitor, "critical_errors", 2)  // 2 critical errors
  ErrorRateMonitor::set_threshold(error_monitor, "error_burst", 5)    // 5 errors in 1 minute
  
  // Add operations and errors
  let base_time = @time()
  
  // Add successful operations
  for i in 0..=89 {
    ErrorRateMonitor::record_operation(error_monitor, "api_request", true, base_time + (i * 1000))
  }
  
  // Add errors (10% error rate)
  for i in 0..=9 {
    let error = NetworkError::new("Timeout", ErrorSeverity::Medium, ErrorCategory::Transient)
    ErrorRateMonitor::record_operation(error_monitor, "api_request", false, base_time + (i * 1000))
    ErrorRateMonitor::record_error(error_monitor, error)
  }
  
  // Add critical errors
  let critical_error1 = SystemError::new("Out of memory", ErrorSeverity::Critical, ErrorCategory::System)
  let critical_error2 = DatabaseError::new("Connection failed", ErrorSeverity::Critical, ErrorCategory::Persistent)
  
  ErrorRateMonitor::record_error(error_monitor, critical_error1)
  ErrorRateMonitor::record_error(error_monitor, critical_error2)
  
  // Check for alerts
  let alerts = ErrorRateMonitor::check_alerts(error_monitor)
  
  // Verify alerts
  assert_true(alerts.length() >= 2)  // Should have alerts for error rate and critical errors
  
  // Verify error rate alert
  let error_rate_alert = alerts.find(@(a) Alert::type(a) == "error_rate")
  match error_rate_alert {
    Some(alert) => {
      assert_eq(Alert::severity(alert), AlertSeverity::Warning)
      assert_true(Alert::message(alert).contains("Error rate"))
    }
    None => assert_true(false)
  }
  
  // Verify critical errors alert
  let critical_alert = alerts.find(@(a) Alert::type(a) == "critical_errors")
  match critical_alert {
    Some(alert) => {
      assert_eq(Alert::severity(alert), AlertSeverity::Critical)
      assert_true(Alert::message(alert).contains("Critical errors"))
    }
    None => assert_true(false)
  }
}

// Test 6: Error Recovery with Exponential Backoff
test "error recovery with exponential backoff" {
  // Create exponential backoff strategy
  let backoff_strategy = ExponentialBackoffStrategy::new(1000, 30000, 2.0)  // 1s base, 30s max, 2x multiplier
  let retry_manager = RetryManager::new(backoff_strategy)
  
  // Simulate operation that fails twice then succeeds
  let mut attempt_count = 0
  let operation = || {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err(NetworkError::new("Connection timeout", ErrorSeverity::Medium, ErrorCategory::Transient))
    } else {
      Ok("Operation successful")
    }
  }
  
  // Execute operation with retry
  let result = RetryManager::execute(retry_manager, operation)
  
  // Verify eventual success
  match result {
    Ok(value) => assert_eq(value, "Operation successful")
    Err(_) => assert_true(false)
  }
  
  // Verify retry attempts
  assert_eq(attempt_count, 3)
  
  // Test with operation that always fails
  let mut always_fail_count = 0
  let failing_operation = || {
    always_fail_count = always_fail_count + 1
    Err(DatabaseError::new("Connection failed", ErrorSeverity::High, ErrorCategory::Persistent))
  }
  
  let failing_result = RetryManager::execute(retry_manager, failing_operation)
  
  // Verify failure after max retries
  match failing_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(Error::type_name(error), "DatabaseError")
  }
  
  // Verify max retry attempts
  assert_eq(always_fail_count, 5)  // Default max retries
}

// Test 7: Error Recovery with Circuit Breaker
test "error recovery with circuit breaker" {
  // Create circuit breaker
  let circuit_breaker = CircuitBreaker::new("database_service", 3, 30000)  // 3 failures, 30s timeout
  
  // Test successful operations
  for i in 0..=4 {
    let result = CircuitBreaker::execute(circuit_breaker, || {
      Ok("Query result")
    })
    
    match result {
      Ok(value) => assert_eq(value, "Query result")
      Err(_) => assert_true(false)
    }
  }
  
  // Verify circuit breaker state
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Closed)
  assert_eq(CircuitBreaker::failure_count(circuit_breaker), 0)
  assert_eq(CircuitBreaker::success_count(circuit_breaker), 5)
  
  // Test failing operations to trigger circuit breaker
  for i in 0..=2 {
    let result = CircuitBreaker::execute(circuit_breaker, || {
      Err(DatabaseError::new("Connection failed", ErrorSeverity::High, ErrorCategory::Persistent))
    })
    
    match result {
      Ok(_) => assert_true(false)
      Err(error) => assert_eq(Error::type_name(error), "DatabaseError")
    }
  }
  
  // Verify circuit breaker is now open
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Open)
  assert_eq(CircuitBreaker::failure_count(circuit_breaker), 3)
  
  // Test operation when circuit breaker is open
  let open_result = CircuitBreaker::execute(circuit_breaker, || {
    Ok("Should not execute")
  })
  
  match open_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(Error::type_name(error), "CircuitBreakerOpenError")
  }
  
  // Simulate time passing for circuit breaker to half-open
  // CircuitBreaker::advance_time(circuit_breaker, 35000)  // 35 seconds
  
  // Test operation in half-open state
  let half_open_result = CircuitBreaker::execute(circuit_breaker, || {
    Ok("Recovery successful")
  })
  
  match half_open_result {
    Ok(value) => assert_eq(value, "Recovery successful")
    Err(_) => assert_true(false)
  }
  
  // Verify circuit breaker returns to closed state
  assert_eq(CircuitBreaker::state(circuit_breaker), CircuitBreakerState::Closed)
}

// Test 8: Error Recovery with Bulkhead Pattern
test "error recovery with bulkhead pattern" {
  // Create bulkhead isolation
  let bulkhead = Bulkhead::new("database_operations", 3, 10)  // 3 concurrent, 10 queued
  
  // Test operations within limits
  let results = []
  for i in 0..=2 {
    let result = Bulkhead::execute(bulkhead, || {
      // Simulate database operation
      @sleep(100)  // 100ms operation
      Ok("Operation " + i.to_string())
    })
    results.push(result)
  }
  
  // Verify all operations succeed
  for result in results {
    match result {
      Ok(_) => assert_true(true)
      Err(_) => assert_true(false)
    }
  }
  
  // Test operation exceeding concurrent limit
  let excess_result = Bulkhead::execute(bulkhead, || {
    Ok("Should not execute immediately")
  })
  
  // Should either succeed (if queued) or fail (if queue full)
  match excess_result {
    Ok(_) => assert_true(true)  // Queued successfully
    Err(error) => {
      // Either rejected due to limits or executed
      assert_true(
        Error::type_name(error) == "BulkheadRejectedError" ||
        Error::type_name(error) == "BulkheadFullError"
      )
    }
  }
  
  // Test operation when bulkhead is full
  let operations = []
  for i in 0..=14 {  // 15 operations (3 concurrent + 10 queued + 2 rejected)
    let result = Bulkhead::execute(bulkhead, || {
      @sleep(1000)  // 1s operation
      Ok("Operation " + i.to_string())
    })
    operations.push(result)
  }
  
  // Count successful and rejected operations
  let mut success_count = 0
  let mut rejected_count = 0
  
  for result in operations {
    match result {
      Ok(_) => success_count = success_count + 1
      Err(error) => {
        if Error::type_name(error) == "BulkheadRejectedError" {
          rejected_count = rejected_count + 1
        }
      }
    }
  }
  
  // Verify bulkhead limits
  assert_eq(success_count, 13)  // 3 concurrent + 10 queued
  assert_eq(rejected_count, 2)   // 2 rejected
}

// Test 9: Error Recovery with Timeout Pattern
test "error recovery with timeout pattern" {
  // Create timeout manager
  let timeout_manager = TimeoutManager::new()
  
  // Test operation that completes within timeout
  let fast_result = TimeoutManager::execute_with_timeout(timeout_manager, || {
    @sleep(100)  // 100ms operation
    Ok("Fast operation completed")
  }, 500)  // 500ms timeout
  
  match fast_result {
    Ok(value) => assert_eq(value, "Fast operation completed")
    Err(_) => assert_true(false)
  }
  
  // Test operation that exceeds timeout
  let slow_result = TimeoutManager::execute_with_timeout(timeout_manager, || {
    @sleep(1000)  // 1s operation
    Ok("Slow operation completed")
  }, 500)  // 500ms timeout
  
  match slow_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(Error::type_name(error), "TimeoutError")
  }
  
  // Test operation with custom timeout strategy
  let adaptive_timeout = AdaptiveTimeoutStrategy::new(1000, 5000, 2.0)  // 1s base, 5s max, 2x multiplier
  let adaptive_manager = TimeoutManager::with_strategy(adaptive_timeout)
  
  // First attempt with base timeout
  let first_result = TimeoutManager::execute_with_timeout(adaptive_manager, || {
    @sleep(1500)  // 1.5s operation (exceeds base timeout)
    Ok("First attempt")
  }, 0)  // Use strategy timeout
  
  match first_result {
    Ok(_) => assert_true(false)
    Err(error) => assert_eq(Error::type_name(error), "TimeoutError")
  }
  
  // Second attempt with adjusted timeout
  let second_result = TimeoutManager::execute_with_timeout(adaptive_manager, || {
    @sleep(1500)  // 1.5s operation (within adjusted timeout)
    Ok("Second attempt")
  }, 0)  // Use strategy timeout
  
  match second_result {
    Ok(value) => assert_eq(value, "Second attempt")
    Err(_) => assert_true(false)
  }
}

// Test 10: Comprehensive Error Recovery Pipeline
test "comprehensive error recovery pipeline" {
  // Create comprehensive recovery pipeline
  let pipeline = ErrorRecoveryPipeline::new()
  
  // Add recovery stages
  let retry_stage = RetryStage::new(3, 1000)  // 3 retries, 1s delay
  let circuit_breaker_stage = CircuitBreakerStage::new("external_service", 5, 30000)
  let fallback_stage = FallbackStage::new(|| {
    Ok("Fallback response")
  })
  let timeout_stage = TimeoutStage::new(5000)  // 5s timeout
  
  ErrorRecoveryPipeline::add_stage(pipeline, retry_stage)
  ErrorRecoveryPipeline::add_stage(pipeline, circuit_breaker_stage)
  ErrorRecoveryPipeline::add_stage(pipeline, timeout_stage)
  ErrorRecoveryPipeline::add_stage(pipeline, fallback_stage)
  
  // Test successful operation
  let success_result = ErrorRecoveryPipeline::execute(pipeline, || {
    Ok("Primary operation successful")
  })
  
  match success_result {
    Ok(value) => assert_eq(value, "Primary operation successful")
    Err(_) => assert_true(false)
  }
  
  // Test operation that fails but succeeds on retry
  let mut retry_count = 0
  let retry_result = ErrorRecoveryPipeline::execute(pipeline, || {
    retry_count = retry_count + 1
    if retry_count < 2 {
      Err(NetworkError::new("Temporary failure", ErrorSeverity::Medium, ErrorCategory::Transient))
    } else {
      Ok("Retry successful")
    }
  })
  
  match retry_result {
    Ok(value) => assert_eq(value, "Retry successful")
    Err(_) => assert_true(false)
  }
  
  assert_eq(retry_count, 2)
  
  // Test operation that always fails and triggers fallback
  let fail_result = ErrorRecoveryPipeline::execute(pipeline, || {
    Err(DatabaseError::new("Persistent failure", ErrorSeverity::High, ErrorCategory::Persistent))
  })
  
  match fail_result {
    Ok(value) => assert_eq(value, "Fallback response")
    Err(_) => assert_true(false)
  }
  
  // Test operation that times out
  let timeout_result = ErrorRecoveryPipeline::execute(pipeline, || {
    @sleep(6000)  // 6s operation (exceeds 5s timeout)
    Ok("Should not complete")
  })
  
  match timeout_result {
    Ok(_) => assert_true(false)
    Err(error) => {
      // Should be timeout error or fallback response
      assert_true(
        Error::type_name(error) == "TimeoutError" ||
        Error::type_name(error) == "FallbackUsed"
      )
    }
  }
  
  // Verify pipeline metrics
  let metrics = ErrorRecoveryPipeline::get_metrics(pipeline)
  assert_eq(PipelineMetrics::total_operations(metrics), 4)
  assert_eq(PipelineMetrics::successful_operations(metrics), 2)
  assert_eq(PipelineMetrics::fallback_used(metrics), 2)
  assert_true(PipelineMetrics::retry_attempts(metrics) > 0)
}