// Azimuth High-Quality Error Handling and Boundary Condition Tests
// This file contains comprehensive test cases for error handling and boundary conditions

// Test 1: Exception Handling and Recovery
test "exception handling and recovery" {
  let exception_handler = ExceptionHandler::new()
  
  // Test exception catching and handling
  let risky_operation = || {
    // Simulate an operation that might fail
    let random = generate_random_number()
    if random > 0.8 {
      raise RuntimeError::new("Simulated operation failure")
    } else {
      "success"
    }
  }
  
  // Test successful execution
  let success_result = exception_handler.execute_with_recovery(
    risky_operation,
    RecoveryStrategy::Retry(3),
    "default_value"
  )
  
  assert_true(success_result.is_ok())
  
  // Test exception handling with recovery
  let failing_operation = || {
    raise NetworkError::new("Connection timeout")
  }
  
  let failure_result = exception_handler.execute_with_recovery(
    failing_operation,
    RecoveryStrategy::Fallback("fallback_value"),
    "default_value"
  )
  
  assert_true(failure_result.is_ok())
  assert_eq(failure_result.unwrap(), "fallback_value")
  
  // Test exception chaining
  let nested_operation = || {
    try {
      risky_operation()
    } catch {
      e: RuntimeError => {
        raise ValidationError::new("Validation failed: " + e.message)
      }
    }
  }
  
  let nested_result = exception_handler.execute_with_catch_all(
    nested_operation,
    "handled_nested_error"
  )
  
  assert_true(nested_result.is_ok())
  assert_eq(nested_result.unwrap(), "handled_nested_error")
  
  // Test exception metrics
  let exception_metrics = exception_handler.get_metrics()
  
  assert_true(exception_metrics.total_exceptions > 0)
  assert_true(exception_metrics.handled_exceptions > 0)
  assert_true(exception_metrics.recovered_operations >= 0)
  assert_true(exception_metrics.exception_types.length() > 0)
}

// Test 2: Boundary Value Testing
test "boundary value testing" {
  let boundary_tester = BoundaryTester::new()
  
  // Test numeric boundaries
  let numeric_validator = NumericValidator::new(0.0, 100.0) // Range [0, 100]
  
  // Test boundary values
  assert_true(numeric_validator.is_valid(0.0)) // Lower boundary
  assert_true(numeric_validator.is_valid(100.0)) // Upper boundary
  assert_true(numeric_validator.is_valid(50.0)) // Middle value
  
  assert_false(numeric_validator.is_valid(-0.1)) // Just below lower boundary
  assert_false(numeric_validator.is_valid(100.1)) // Just above upper boundary
  
  // Test edge cases with floating point precision
  let very_small_positive = 0.0000001
  let very_small_negative = -0.0000001
  
  assert_true(numeric_validator.is_valid(very_small_positive))
  assert_false(numeric_validator.is_valid(very_small_negative))
  
  // Test string length boundaries
  let string_validator = StringLengthValidator::new(1, 50) // Length [1, 50]
  
  assert_true(string_validator.is_valid("a")) // Minimum length
  assert_true(string_validator.is_valid("a".repeat(50))) // Maximum length
  assert_false(string_validator.is_valid("")) // Empty string
  assert_false(string_validator.is_valid("a".repeat(51))) // Exceeds maximum
  
  // Test array size boundaries
  let array_validator = ArraySizeValidator::new(1, 10) // Size [1, 10]
  
  assert_true(array_validator.is_valid([1])) // Minimum size
  assert_true(array_validator.is_valid([1, 2, 3, 4, 5, 6, 7, 8, 9, 10])) // Maximum size
  assert_false(array_validator.is_valid([])) // Empty array
  assert_false(array_validator.is_valid([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])) // Exceeds maximum
  
  // Test timestamp boundaries
  let timestamp_validator = TimestampValidator::new(
    Timestamp::from_string("2024-01-01T00:00:00Z"),
    Timestamp::from_string("2024-12-31T23:59:59Z")
  )
  
  assert_true(timestamp_validator.is_valid(Timestamp::from_string("2024-01-01T00:00:00Z"))) // Start boundary
  assert_true(timestamp_validator.is_valid(Timestamp::from_string("2024-12-31T23:59:59Z"))) // End boundary
  assert_false(timestamp_validator.is_valid(Timestamp::from_string("2023-12-31T23:59:59Z"))) // Before start
  assert_false(timestamp_validator.is_valid(Timestamp::from_string("2025-01-01T00:00:00Z"))) // After end
}

// Test 3: Resource Exhaustion Handling
test "resource exhaustion handling" {
  let resource_manager = ResourceManager::new()
  
  // Test memory exhaustion handling
  let memory_monitor = MemoryMonitor::new()
  let initial_memory = memory_monitor.get_usage()
  
  // Simulate memory-intensive operation
  let memory_intensive_operation = || {
    let large_data = allocate_large_memory_chunk(100 * 1024 * 1024) // 100MB
    process_large_data(large_data)
    large_data // Return data to prevent optimization
  }
  
  // Execute with memory monitoring
  let memory_result = resource_manager.execute_with_monitoring(
    memory_intensive_operation,
    ResourceMonitor::Memory(150 * 1024 * 1024), // 150MB limit
    ResourceExhaustionStrategy::FailFast
  )
  
  match memory_result {
    Ok(data) => {
      assert_true(data.size() > 0)
      // Clean up
      deallocate_memory(data)
    }
    Err(ResourceExhaustionError::Memory(limit)) => {
      assert_true(limit > 0)
    }
    Err(_) => {
      assert_true(false) // Unexpected error
    }
  }
  
  // Test file handle exhaustion handling
  let file_handle_monitor = FileHandleMonitor::new()
  let file_operations = []
  
  // Open many files to test handle exhaustion
  for i in 0..=100 {
    let file_result = resource_manager.open_file_with_limit(
      "test_file_" + i.to_string(),
      FileOpenMode::Read,
      50 // Limit to 50 open files
    )
    
    match file_result {
      Ok(handle) => {
        file_operations.push(handle)
      }
      Err(ResourceExhaustionError::FileHandles(limit)) => {
        assert_true(file_operations.length() >= 50)
        break
      }
      Err(_) => {
        assert_true(false) // Unexpected error
      }
    }
  }
  
  // Clean up file handles
  for handle in file_operations {
    resource_manager.close_file(handle)
  }
  
  // Test network connection exhaustion
  let connection_pool = ConnectionPool::new(10) // Limit to 10 connections
  
  let connections = []
  for i in 0..=15 {
    let connection_result = connection_pool.get_connection("localhost:8080")
    
    match connection_result {
      Ok(conn) => {
        connections.push(conn)
      }
      Err(ConnectionPoolError::Exhausted) => {
        assert_true(connections.length() >= 10)
        break
      }
      Err(_) => {
        assert_true(false) // Unexpected error
      }
    }
  }
  
  // Return connections to pool
  for conn in connections {
    connection_pool.return_connection(conn)
  }
}

// Test 4: Timeout and Deadline Handling
test "timeout and deadline handling" {
  let timeout_manager = TimeoutManager::new()
  
  // Test operation timeout
  let slow_operation = || {
    sleep(Duration::from_seconds(5)) // 5 second operation
    "completed"
  }
  
  let timeout_result = timeout_manager.execute_with_timeout(
    slow_operation,
    Duration::from_seconds(2) // 2 second timeout
  )
  
  match timeout_result {
    Ok(_) => {
      assert_true(false) // Should have timed out
    }
    Err(TimeoutError::OperationTimeout(duration)) => {
      assert_true(duration.to_seconds() >= 2)
    }
    Err(_) => {
      assert_true(false) // Unexpected error
    }
  }
  
  // Test fast operation within timeout
  let fast_operation = || {
    sleep(Duration::from_millis(100)) // 100ms operation
    "quick_result"
  }
  
  let fast_result = timeout_manager.execute_with_timeout(
    fast_operation,
    Duration::from_seconds(1) // 1 second timeout
  )
  
  assert_true(fast_result.is_ok())
  assert_eq(fast_result.unwrap(), "quick_result")
  
  // Test deadline propagation
  let deadline = Deadline::from_now(Duration::from_seconds(3))
  let nested_operation = || {
    timeout_manager.execute_with_deadline(|| {
      sleep(Duration::from_seconds(1))
      "nested_completed"
    }, deadline)
  }
  
  let nested_result = nested_operation()
  assert_true(nested_result.is_ok())
  assert_eq(nested_result.unwrap(), "nested_completed")
  
  // Test deadline exceeded in nested operation
  let long_nested_operation = || {
    timeout_manager.execute_with_deadline(|| {
      sleep(Duration::from_seconds(5))
      "should_not_complete"
    }, deadline)
  }
  
  let long_nested_result = long_nested_operation()
  match long_nested_result {
    Ok(_) => {
      assert_true(false) // Should have exceeded deadline
    }
    Err(TimeoutError::DeadlineExceeded) => {
      assert_true(true) // Expected error
    }
    Err(_) => {
      assert_true(false) // Unexpected error
    }
  }
  
  // Test timeout metrics
  let timeout_metrics = timeout_manager.get_metrics()
  
  assert_true(timeout_metrics.total_operations > 0)
  assert_true(timeout_metrics.timed_out_operations > 0)
  assert_true(timeout_metrics.completed_operations > 0)
  assert_true(timeout_metrics.average_execution_time_ms > 0)
}

// Test 5: Circuit Breaker Pattern
test "circuit breaker pattern" {
  let circuit_breaker = CircuitBreaker::new(
    "telemetry_service",
    5, // Failure threshold
    Duration::from_seconds(30), // Recovery timeout
    50 // Half-open max calls
  )
  
  // Test circuit breaker in closed state
  assert_eq(circuit_breaker.get_state(), CircuitBreakerState::Closed)
  
  // Simulate successful operations
  for i in 0..=3 {
    let result = circuit_breaker.execute(|| {
      "success_" + i.to_string()
    })
    
    assert_true(result.is_ok())
    assert_eq(result.unwrap(), "success_" + i.to_string())
  }
  
  // Still closed after some successes
  assert_eq(circuit_breaker.get_state(), CircuitBreakerState::Closed)
  
  // Simulate failures to trip the circuit
  for i in 0..=5 {
    let result = circuit_breaker.execute(|| {
      raise ServiceError::new("Service unavailable")
    })
    
    assert_true(result.is_error())
  }
  
  // Circuit should now be open
  assert_eq(circuit_breaker.get_state(), CircuitBreakerState::Open)
  
  // Test that operations fail fast when circuit is open
  let open_circuit_result = circuit_breaker.execute(|| {
    "should_not_execute"
  })
  
  match open_circuit_result {
    Ok(_) => {
      assert_true(false) // Should not execute when circuit is open
    }
    Err(CircuitBreakerError::CircuitOpen) => {
      assert_true(true) // Expected error
    }
    Err(_) => {
      assert_true(false) // Unexpected error
    }
  }
  
  // Test circuit breaker metrics
  let metrics = circuit_breaker.get_metrics()
  
  assert_true(metrics.failure_count >= 5)
  assert_true(metrics.success_count >= 3)
  assert_true(metrics.total_calls > 0)
  assert_true(metrics.failure_rate > 0.5) // Should be high after failures
  
  // Test circuit recovery (simulate time passing)
  // In real implementation, we would need to manipulate time
  // For this test, we'll manually transition to half-open
  circuit_breaker.transition_to_half_open()
  assert_eq(circuit_breaker.get_state(), CircuitBreakerState::HalfOpen)
  
  // Test half-open behavior
  let half_open_result = circuit_breaker.execute(|| {
    "recovery_success"
  })
  
  assert_true(half_open_result.is_ok())
  assert_eq(half_open_result.unwrap(), "recovery_success")
  
  // After successful call, should transition back to closed
  assert_eq(circuit_breaker.get_state(), CircuitBreakerState::Closed)
}

// Test 6: Rate Limiting and Throttling
test "rate limiting and throttling" {
  let rate_limiter = RateLimiter::new(
    RateLimitStrategy::TokenBucket(10, 2) // 10 tokens, refill 2 per second
  )
  
  // Test rate limiting within limits
  let successful_requests = []
  for i in 0..=9 {
    let result = rate_limiter.check_limit("api_call")
    if result.is_allowed {
      successful_requests.push(i)
    }
  }
  
  assert_eq(successful_requests.length(), 10) // All should be allowed
  
  // Next request should be rate limited
  let limited_result = rate_limiter.check_limit("api_call")
  assert_false(limited_result.is_allowed)
  assert_true(limited_result.retry_after_ms > 0)
  
  // Test different rate limits for different operations
  rate_limiter.set_rate_limit("admin_operation", RateLimitStrategy::TokenBucket(5, 1))
  
  let admin_requests = []
  for i in 0..=4 {
    let result = rate_limiter.check_limit("admin_operation")
    if result.is_allowed {
      admin_requests.push(i)
    }
  }
  
  assert_eq(admin_requests.length(), 5) // All admin requests should be allowed
  
  let admin_limited_result = rate_limiter.check_limit("admin_operation")
  assert_false(admin_limited_result.is_allowed)
  
  // Test rate limit recovery over time
  // In real implementation, we would need to wait or manipulate time
  // For this test, we'll manually add tokens
  rate_limiter.add_tokens("api_call", 2)
  
  let recovered_result = rate_limiter.check_limit("api_call")
  assert_true(recovered_result.is_allowed)
  
  // Test rate limit metrics
  let metrics = rate_limiter.get_metrics()
  
  assert_true(metrics.contains_key("api_call"))
  assert_true(metrics.contains_key("admin_operation"))
  
  let api_metrics = metrics.get("api_call")
  assert_true(api_metrics.total_requests > 0)
  assert_true(api_metrics.allowed_requests > 0)
  assert_true(api_metrics.denied_requests > 0)
  assert_true(api_metrics.current_tokens >= 0)
}

// Test 7: Backpressure and Flow Control
test "backpressure and flow control" {
  let flow_controller = FlowController::new(
    BufferSize::from_items(100), // 100 item buffer
    BackpressureStrategy::DropOldest
  )
  
  // Test flow control with slow consumer
  let producer = Producer::new(flow_controller)
  let consumer = SlowConsumer::new(Duration::from_millis(10)) // 10ms processing time
  
  // Start producer and consumer
  producer.start()
  consumer.start()
  
  // Produce items faster than consumption
  for i in 0..=150 {
    producer.produce("item_" + i.to_string())
  }
  
  // Wait for processing
  sleep(Duration::from_seconds(2))
  
  // Stop producer and consumer
  producer.stop()
  consumer.stop()
  
  // Check flow control metrics
  let metrics = flow_controller.get_metrics()
  
  assert_true(metrics.produced_items > 0)
  assert_true(metrics.consumed_items > 0)
  assert_true(metrics.dropped_items > 0) // Should have dropped items due to buffer overflow
  assert_true(metrics.buffer_utilization > 0.8) // Buffer should have been highly utilized
  
  // Test different backpressure strategies
  let blocking_flow_controller = FlowController::new(
    BufferSize::from_items(50),
    BackpressureStrategy::Block
  )
  
  let blocking_producer = Producer::new(blocking_flow_controller)
  let fast_consumer = FastConsumer::new() // Very fast consumer
  
  blocking_producer.start()
  fast_consumer.start()
  
  let start_time = get_current_timestamp()
  
  // Produce items that should block when buffer is full
  for i in 0..=60 {
    blocking_producer.produce("blocking_item_" + i.to_string())
  }
  
  let end_time = get_current_timestamp()
  let elapsed = end_time - start_time
  
  // Should have taken longer due to blocking
  assert_true(elapsed.to_millis() > 100) // At least 100ms due to blocking
  
  blocking_producer.stop()
  fast_consumer.stop()
  
  // Test adaptive flow control
  let adaptive_flow_controller = AdaptiveFlowController::new(
    InitialBufferSize::from_items(100),
    MinBufferSize::from_items(10),
    MaxBufferSize::from_items(1000)
  )
  
  let adaptive_producer = Producer::new(adaptive_flow_controller)
  let variable_consumer = VariableConsumer::new() // Consumer with variable processing time
  
  adaptive_producer.start()
  variable_consumer.start()
  
  // Produce items with variable consumption rate
  for i in 0..=200 {
    adaptive_producer.produce("adaptive_item_" + i.to_string())
  }
  
  sleep(Duration::from_seconds(3))
  
  adaptive_producer.stop()
  variable_consumer.stop()
  
  // Check adaptive metrics
  let adaptive_metrics = adaptive_flow_controller.get_metrics()
  
  assert_true(adaptive_metrics.buffer_size_adjustments > 0)
  assert_true(adaptive_metrics.current_buffer_size >= 10)
  assert_true(adaptive_metrics.current_buffer_size <= 1000)
}

// Test 8: Graceful Degradation
test "graceful degradation" {
  let degradation_manager = DegradationManager::new()
  
  // Define service levels
  let full_service = ServiceLevel::new("full", 100)
    .with_feature("advanced_analytics", true)
    .with_feature("real_time_processing", true)
    .with_feature("detailed_logging", true)
    .with_feature("caching", true)
  
  let degraded_service = ServiceLevel::new("degraded", 70)
    .with_feature("advanced_analytics", false)
    .with_feature("real_time_processing", false)
    .with_feature("detailed_logging", true)
    .with_feature("caching", true)
  
  let minimal_service = ServiceLevel::new("minimal", 40)
    .with_feature("advanced_analytics", false)
    .with_feature("real_time_processing", false)
    .with_feature("detailed_logging", false)
    .with_feature("caching", false)
  
  degradation_manager.add_service_level(full_service)
  degradation_manager.add_service_level(degraded_service)
  degradation_manager.add_service_level(minimal_service)
  
  // Start at full service level
  degradation_manager.set_current_level("full")
  assert_eq(degradation_manager.get_current_level().name, "full")
  
  // Test feature availability at different levels
  assert_true(degradation_manager.is_feature_enabled("advanced_analytics"))
  assert_true(degradation_manager.is_feature_enabled("real_time_processing"))
  assert_true(degradation_manager.is_feature_enabled("detailed_logging"))
  assert_true(degradation_manager.is_feature_enabled("caching"))
  
  // Simulate system stress and trigger degradation
  let system_metrics = SystemMetrics::new()
    .with_cpu_usage(85.0) // High CPU
    .with_memory_usage(80.0) // High memory
    .with_response_time(500.0) // Slow response
    .with_error_rate(0.15) // 15% error rate
  
  let recommended_level = degradation_manager.recommend_level(system_metrics)
  assert_eq(recommended_level.name, "degraded")
  
  // Apply degradation
  degradation_manager.set_current_level("degraded")
  assert_eq(degradation_manager.get_current_level().name, "degraded")
  
  // Verify feature changes
  assert_false(degradation_manager.is_feature_enabled("advanced_analytics"))
  assert_false(degradation_manager.is_feature_enabled("real_time_processing"))
  assert_true(degradation_manager.is_feature_enabled("detailed_logging"))
  assert_true(degradation_manager.is_feature_enabled("caching"))
  
  // Simulate extreme system stress
  let critical_metrics = SystemMetrics::new()
    .with_cpu_usage(95.0) // Critical CPU
    .with_memory_usage(90.0) // Critical memory
    .with_response_time(2000.0) // Very slow response
    .with_error_rate(0.30) // 30% error rate
  
  let critical_level = degradation_manager.recommend_level(critical_metrics)
  assert_eq(critical_level.name, "minimal")
  
  // Apply minimal service level
  degradation_manager.set_current_level("minimal")
  assert_eq(degradation_manager.get_current_level().name, "minimal")
  
  // Verify minimal feature set
  assert_false(degradation_manager.is_feature_enabled("advanced_analytics"))
  assert_false(degradation_manager.is_feature_enabled("real_time_processing"))
  assert_false(degradation_manager.is_feature_enabled("detailed_logging"))
  assert_false(degradation_manager.is_feature_enabled("caching"))
  
  // Test recovery to higher service levels
  let healthy_metrics = SystemMetrics::new()
    .with_cpu_usage(30.0) // Normal CPU
    .with_memory_usage(40.0) // Normal memory
    .with_response_time(100.0) // Normal response
    .with_error_rate(0.01) // 1% error rate
  
  let recovery_level = degradation_manager.recommend_level(healthy_metrics)
  assert_eq(recovery_level.name, "full")
  
  // Test degradation metrics
  let degradation_metrics = degradation_manager.get_metrics()
  
  assert_true(degradation_metrics.level_changes > 0)
  assert_true(degradation_metrics.feature_disables > 0)
  assert_true(degradation_metrics.feature_enables > 0)
  assert_true(degradation_metrics.time_in_degraded_mode > 0)
}

// Test 9: Idempotency and Safe Retries
test "idempotency and safe retries" {
  let idempotency_manager = IdempotencyManager::new()
  
  // Test idempotent operation
  let idempotent_operation = |request_id| {
    idempotency_manager.execute_once(request_id, || {
      // Simulate an operation that should only run once
      let result = process_payment(request_id, 100.0)
      result
    })
  }
  
  // Execute the same operation multiple times with the same ID
  let result1 = idempotent_operation("payment_123")
  let result2 = idempotent_operation("payment_123")
  let result3 = idempotent_operation("payment_123")
  
  // All results should be the same (operation only executed once)
  assert_eq(result1.transaction_id, result2.transaction_id)
  assert_eq(result2.transaction_id, result3.transaction_id)
  assert_eq(result1.amount, result2.amount)
  assert_eq(result2.amount, result3.amount)
  
  // Execute with different IDs
  let result4 = idempotent_operation("payment_456")
  let result5 = idempotent_operation("payment_789")
  
  // Different IDs should produce different results
  assert_ne(result4.transaction_id, result5.transaction_id)
  
  // Test retry logic with idempotency
  let retry_manager = RetryManager::new()
    .with_max_attempts(3)
    .with_backoff(BackoffStrategy::Exponential(100)) // Start with 100ms
    .with_retry_condition(|error| {
      match error {
        NetworkError::Timeout => true,
        NetworkError::ConnectionRefused => true,
        _ => false
      }
    })
  
  let flaky_operation = || {
    let random = generate_random_number()
    if random < 0.7 {
      raise NetworkError::Timeout("Simulated timeout")
    } else {
      "success"
    }
  }
  
  let retry_result = retry_manager.execute_with_idempotency(
    "flaky_operation_123",
    flaky_operation
  )
  
  assert_true(retry_result.is_ok())
  assert_eq(retry_result.unwrap(), "success")
  
  // Test retry metrics
  let retry_metrics = retry_manager.get_metrics()
  
  assert_true(retry_metrics.total_attempts > 0)
  assert_true(retry_metrics.successful_retries > 0)
  assert_true(retry_metrics.failed_retries >= 0)
  assert_true(retry_metrics.average_attempts_per_operation > 1.0)
  
  // Test non-retryable errors
  let non_retryable_operation = || {
    raise ValidationError::new("Invalid input data")
  }
  
  let non_retryable_result = retry_manager.execute_with_idempotency(
    "validation_operation_123",
    non_retryable_operation
  )
  
  assert_true(non_retryable_result.is_error())
  
  // Verify it wasn't retried
  let non_retryable_metrics = retry_manager.get_operation_metrics("validation_operation_123")
  assert_eq(non_retryable_metrics.attempt_count, 1)
}

// Test 10: Deadlock Detection and Prevention
test "deadlock detection and prevention" {
  let deadlock_detector = DeadlockDetector::new()
  
  // Test deadlock detection with resource acquisition ordering
  let resource_a = MutexResource::new("resource_a")
  let resource_b = MutexResource::new("resource_b")
  
  // Safe acquisition order (always acquire A then B)
  let safe_operation = || {
    let lock_a = resource_a.lock()
    sleep(Duration::from_millis(10)) // Simulate some work
    let lock_b = resource_b.lock()
    sleep(Duration::from_millis(10)) // Simulate more work
    // Locks are released automatically when they go out of scope
    "completed_safely"
  }
  
  let safe_result = deadlock_detector.execute_monitored(safe_operation)
  assert_true(safe_result.is_ok())
  assert_eq(safe_result.unwrap(), "completed_safely")
  
  // Test potential deadlock scenario (acquire resources in different orders)
  let deadlock_prone_operation_1 = || {
    let lock_a = resource_a.lock()
    sleep(Duration::from_millis(50)) // Hold lock A for a while
    let lock_b = resource_b.lock()
    "operation_1_completed"
  }
  
  let deadlock_prone_operation_2 = || {
    let lock_b = resource_b.lock()
    sleep(Duration::from_millis(50)) // Hold lock B for a while
    let lock_a = resource_a.lock()
    "operation_2_completed"
  }
  
  // Execute operations concurrently
  let handle_1 = deadlock_detector.execute_concurrent(deadlock_prone_operation_1)
  let handle_2 = deadlock_detector.execute_concurrent(deadlock_prone_operation_2)
  
  // Wait for completion or timeout
  let result1 = handle_1.wait_with_timeout(Duration::from_seconds(5))
  let result2 = handle_2.wait_with_timeout(Duration::from_seconds(5))
  
  // At least one should have completed (or both if no deadlock)
  assert_true(result1.is_ok() || result2.is_ok())
  
  // Test deadlock prevention with timeout
  let timeout_operation = || {
    deadlock_detector.execute_with_timeout(|| {
      let lock_a = resource_a.lock_with_timeout(Duration::from_seconds(1))
      let lock_b = resource_b.lock_with_timeout(Duration::from_seconds(1))
      "timeout_operation_completed"
    }, Duration::from_seconds(2))
  }
  
  let timeout_result = timeout_operation()
  assert_true(timeout_result.is_ok())
  
  // Test deadlock detection metrics
  let deadlock_metrics = deadlock_detector.get_metrics()
  
  assert_true(deadlock_metrics.total_operations > 0)
  assert_true(deadlock_metrics.detected_deadlocks >= 0)
  assert_true(deadlock_metrics.prevented_deadlocks >= 0)
  assert_true(deadlock_metrics.timeout_occurrences >= 0)
  
  // Test resource dependency graph analysis
  let dependency_graph = deadlock_detector.build_dependency_graph()
  
  assert_true(dependency_graph.nodes.length() > 0)
  assert_true(dependency_graph.edges.length() >= 0)
  
  // Check for cycles in the dependency graph
  let has_cycles = dependency_graph.has_cycles()
  assert_false(has_cycles) // Should not have cycles after proper analysis
  
  // Test deadlock prevention strategies
  let prevention_manager = DeadlockPreventionManager::new()
  
  // Configure prevention strategies
  prevention_manager.add_strategy(ResourceOrderingStrategy::Global)
  prevention_manager.add_strategy(LockTimeoutStrategy::Adaptive)
  prevention_manager.add_strategy(WaitDieStrategy::Enabled)
  
  // Test prevention with complex resource acquisition
  let complex_operation = || {
    let resources = [resource_a, resource_b]
    prevention_manager.acquire_resources_ordered(resources, || {
      "complex_operation_completed"
    })
  }
  
  let complex_result = complex_operation()
  assert_true(complex_result.is_ok())
  assert_eq(complex_result.unwrap(), "complex_operation_completed")
}