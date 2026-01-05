// Azimuth Telemetry System - Error Handling and Resilience Tests
// This file contains comprehensive test cases for error handling and resilience mechanisms

// Test 1: Result Type Error Handling
test "result type error handling patterns" {
  enum Result[T, E] {
    Ok(T)
    Err(E)
  }
  
  // Safe division with result type
  let safe_divide = fn(a: Int, b: Int) -> Result[Int, String] {
    if b == 0 {
      Err("Division by zero")
    } else {
      Ok(a / b)
    }
  }
  
  // Safe array access with result type
  let safe_array_get = fn[T](arr: Array[T], index: Int) -> Result[T, String] {
    if index < 0 || index >= arr.length() {
      Err("Index out of bounds")
    } else {
      Ok(arr[index])
    }
  }
  
  // Safe string to int conversion with result type
  let safe_string_to_int = fn(s: String) -> Result[Int, String] {
    let mut result = 0
    let mut valid = true
    
    for char in s.to_char_array() {
      if char < '0' || char > '9' {
        valid = false
        break
      }
      result = result * 10 + (char.to_int() - '0'.to_int())
    }
    
    if valid {
      Ok(result)
    } else {
      Err("Invalid number format")
    }
  }
  
  // Test safe division
  match safe_divide(10, 2) {
    Ok(result) => assert_eq(result, 5)
    Err(_) => assert_true(false)
  }
  
  match safe_divide(10, 0) {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Division by zero")
  }
  
  // Test safe array access
  let arr = [1, 2, 3]
  
  match safe_array_get(arr, 1) {
    Ok(value) => assert_eq(value, 2)
    Err(_) => assert_true(false)
  }
  
  match safe_array_get(arr, 5) {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Index out of bounds")
  }
  
  // Test safe string to int conversion
  match safe_string_to_int("123") {
    Ok(value) => assert_eq(value, 123)
    Err(_) => assert_true(false)
  }
  
  match safe_string_to_int("abc") {
    Ok(_) => assert_true(false)
    Err(msg) => assert_eq(msg, "Invalid number format")
  }
}

// Test 2: Error Chaining and Composition
test "error chaining and composition" {
  enum Error {
    ValidationError(String)
    ProcessingError(String)
    SystemError(String)
  }
  
  type Result[T] = {
    value: Option[T],
    error: Option[Error]
  }
  
  let ok = fn[T](value: T) -> Result[T] {
    { value: Some(value), error: None }
  }
  
  let err = fn[T](error: Error) -> Result[T] {
    { value: None, error: Some(error) }
  }
  
  let map = fn[T, U](result: Result[T], f: (T) -> U) -> Result[U] {
    match result.value {
      Some(v) => ok(f(v))
      None => { value: None, error: result.error }
    }
  }
  
  let flat_map = fn[T, U](result: Result[T], f: (T) -> Result[U]) -> Result[U] {
    match result.value {
      Some(v) => f(v)
      None => { value: None, error: result.error }
    }
  }
  
  // Validate input
  let validate = fn(input: String) -> Result[String] {
    if input.length() == 0 {
      err(ValidationError("Input cannot be empty"))
    } else if input.length() > 10 {
      err(ValidationError("Input too long"))
    } else {
      ok(input)
    }
  }
  
  // Process input
  let process = fn(input: String) -> Result[Int] {
    let mut sum = 0
    for char in input.to_char_array() {
      if char < '0' || char > '9' {
        return err(ProcessingError("Input contains non-numeric characters"))
      }
      sum = sum + (char.to_int() - '0'.to_int())
    }
    ok(sum)
  }
  
  // Complete pipeline
  let pipeline = fn(input: String) -> Result[Int] {
    let validated = validate(input)
    flat_map(validated, process)
  }
  
  // Test successful pipeline
  match pipeline("123") {
    { value: Some(v), error: None } => assert_eq(v, 6)
    _ => assert_true(false)
  }
  
  // Test validation error
  match pipeline("") {
    { value: None, error: Some(ValidationError(msg)) } => assert_eq(msg, "Input cannot be empty")
    _ => assert_true(false)
  }
  
  // Test processing error
  match pipeline("12a") {
    { value: None, error: Some(ProcessingError(msg)) } => assert_eq(msg, "Input contains non-numeric characters")
    _ => assert_true(false)
  }
}

// Test 3: Circuit Breaker Pattern
test "circuit breaker pattern for resilience" {
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  type CircuitBreaker = {
    state: CircuitState,
    failure_count: Int,
    failure_threshold: Int,
    timeout: Int,
    last_failure_time: Int
  }
  
  let circuit_breaker_new = fn(threshold: Int, timeout: Int) -> CircuitBreaker {
    {
      state: Closed,
      failure_count: 0,
      failure_threshold: threshold,
      timeout: timeout,
      last_failure_time: 0
    }
  }
  
  let circuit_breaker_call = fn[T](cb: CircuitBreaker, operation: () -> Result[T, String], current_time: Int) -> (CircuitBreaker, Result[T, String]) {
    let CircuitBreaker(state, failure_count, failure_threshold, timeout, last_failure_time) = cb
    
    match state {
      Closed => {
        match operation() {
          Ok(value) => {
            // Success, reset failure count
            let new_cb = {
              state: Closed,
              failure_count: 0,
              failure_threshold,
              timeout,
              last_failure_time
            }
            (new_cb, Ok(value))
          }
          Err(error) => {
            // Failure, increment count
            let new_failure_count = failure_count + 1
            let new_state = if new_failure_count >= failure_threshold {
              Open
            } else {
              Closed
            }
            
            let new_cb = {
              state: new_state,
              failure_count: new_failure_count,
              failure_threshold,
              timeout,
              last_failure_time: current_time
            }
            (new_cb, Err(error))
          }
        }
      }
      Open => {
        // Check if timeout has passed
        if current_time - last_failure_time >= timeout {
          // Try half-open state
          let new_cb = {
            state: HalfOpen,
            failure_count,
            failure_threshold,
            timeout,
            last_failure_time
          }
          
          match operation() {
            Ok(value) => {
              // Success, close the circuit
              let closed_cb = {
                state: Closed,
                failure_count: 0,
                failure_threshold,
                timeout,
                last_failure_time
              }
              (closed_cb, Ok(value))
            }
            Err(error) => {
              // Failure, open again
              let open_cb = {
                state: Open,
                failure_count: failure_count + 1,
                failure_threshold,
                timeout,
                last_failure_time: current_time
              }
              (open_cb, Err(error))
            }
          }
        } else {
          // Still in timeout, fail fast
          (cb, Err("Circuit breaker is open"))
        }
      }
      HalfOpen => {
        match operation() {
          Ok(value) => {
            // Success, close the circuit
            let new_cb = {
              state: Closed,
              failure_count: 0,
              failure_threshold,
              timeout,
              last_failure_time
            }
            (new_cb, Ok(value))
          }
          Err(error) => {
            // Failure, open again
            let new_cb = {
              state: Open,
              failure_count: failure_count + 1,
              failure_threshold,
              timeout,
              last_failure_time: current_time
            }
            (new_cb, Err(error))
          }
        }
      }
    }
  }
  
  // Test circuit breaker
  let mut success_count = 0
  let mut failure_count = 0
  
  let operation = fn() -> Result[String, String] {
    if failure_count < 3 {
      failure_count = failure_count + 1
      Err("Simulated failure")
    } else {
      success_count = success_count + 1
      Ok("Success")
    }
  }
  
  let cb = circuit_breaker_new(3, 100)
  
  // First few calls should fail but circuit remains closed
  let (cb1, result1) = circuit_breaker_call(cb, operation, 0)
  match result1 {
    Err(_) => assert_true(true)
    Ok(_) => assert_true(false)
  }
  
  let (cb2, result2) = circuit_breaker_call(cb1, operation, 10)
  match result2 {
    Err(_) => assert_true(true)
    Ok(_) => assert_true(false)
  }
  
  let (cb3, result3) = circuit_breaker_call(cb2, operation, 20)
  match result3 {
    Err(_) => assert_true(true)
    Ok(_) => assert_true(false)
  }
  
  // Circuit should now be open
  let (cb4, result4) = circuit_breaker_call(cb3, operation, 30)
  match result4 {
    Err(msg) => assert_eq(msg, "Circuit breaker is open")
    Ok(_) => assert_true(false)
  }
  
  // After timeout, should try half-open
  let (cb5, result5) = circuit_breaker_call(cb4, operation, 150)
  match result5 {
    Ok(value) => assert_eq(value, "Success")
    Err(_) => assert_true(false)
  }
  
  // Circuit should be closed again
  let (cb6, result6) = circuit_breaker_call(cb5, operation, 160)
  match result6 {
    Ok(value) => assert_eq(value, "Success")
    Err(_) => assert_true(false)
  }
}

// Test 4: Retry Mechanism
test "retry mechanism with exponential backoff" {
  type RetryConfig = {
    max_attempts: Int,
    base_delay: Int,
    max_delay: Int,
    backoff_multiplier: Float
  }
  
  let retry_config_default = fn() -> RetryConfig {
    {
      max_attempts: 3,
      base_delay: 100,
      max_delay: 1000,
      backoff_multiplier: 2.0
    }
  }
  
  let calculate_delay = fn(attempt: Int, config: RetryConfig) -> Int {
    let delay = Float::from_int(config.base_delay) * 
                (config.backoff_multiplier ** Float::from_int(attempt))
    
    let delay_int = delay.to_int()
    if delay_int > config.max_delay {
      config.max_delay
    } else {
      delay_int
    }
  }
  
  let retry_with_backoff = fn[T](config: RetryConfig, operation: () -> Result[T, String]) -> Result[T, String] {
    let mut attempt = 0
    
    while attempt < config.max_attempts {
      match operation() {
        Ok(value) => return Ok(value),
        Err(error) => {
          if attempt == config.max_attempts - 1 {
            return Err(error)
          }
          
          // Calculate delay and wait (simulated)
          let delay = calculate_delay(attempt, config)
          // In real implementation, would wait for 'delay' milliseconds
          attempt = attempt + 1
        }
      }
    }
    
    Err("Max attempts exceeded")
  }
  
  // Test retry configuration
  let config = retry_config_default()
  
  assert_eq(calculate_delay(0, config), 100)
  assert_eq(calculate_delay(1, config), 200)
  assert_eq(calculate_delay(2, config), 400)
  
  // Test with max delay
  let config_with_max = {
    max_attempts: 3,
    base_delay: 100,
    max_delay: 300,
    backoff_multiplier: 2.0
  }
  
  assert_eq(calculate_delay(0, config_with_max), 100)
  assert_eq(calculate_delay(1, config_with_max), 200)
  assert_eq(calculate_delay(2, config_with_max), 300)  // Capped at max
  
  // Test retry mechanism
  let mut call_count = 0
  
  let failing_operation = fn() -> Result[String, String] {
    call_count = call_count + 1
    if call_count < 3 {
      Err("Not ready yet")
    } else {
      Ok("Success after retries")
    }
  }
  
  let result = retry_with_backoff(config, failing_operation)
  match result {
    Ok(value) => {
      assert_eq(value, "Success after retries")
      assert_eq(call_count, 3)
    }
    Err(_) => assert_true(false)
  }
  
  // Reset for next test
  call_count = 0
  
  let always_failing_operation = fn() -> Result[String, String] {
    call_count = call_count + 1
    Err("Always fails")
  }
  
  let result2 = retry_with_backoff(config, always_failing_operation)
  match result2 {
    Err(msg) => {
      assert_eq(msg, "Always fails")
      assert_eq(call_count, 3)  // Should have tried max attempts
    }
    Ok(_) => assert_true(false)
  }
}

// Test 5: Timeout Handling
test "timeout handling for operations" {
  type TimeoutConfig = {
    timeout_ms: Int
  }
  
  let execute_with_timeout = fn[T](config: TimeoutConfig, operation: () -> T, current_time: Int) -> Result[T, String] {
    let start_time = current_time
    
    // Simulate operation execution time
    let execution_time = if operation() == "fast" { 50 } else { 200 }
    let end_time = start_time + execution_time
    
    if end_time - start_time > config.timeout_ms {
      Err("Operation timed out")
    } else {
      Ok(operation())
    }
  }
  
  // Test timeout configuration
  let fast_config = { timeout_ms: 100 }
  let slow_config = { timeout_ms: 150 }
  
  // Test fast operation
  let fast_operation = fn() -> String { "fast" }
  
  match execute_with_timeout(fast_config, fast_operation, 0) {
    Ok(value) => assert_eq(value, "fast")
    Err(_) => assert_true(false)
  }
  
  // Test slow operation with sufficient timeout
  match execute_with_timeout(slow_config, fast_operation, 0) {
    Ok(value) => assert_eq(value, "fast")
    Err(_) => assert_true(false)
  }
  
  // Test slow operation with insufficient timeout
  let slow_operation = fn() -> String { "slow" }
  
  match execute_with_timeout(fast_config, slow_operation, 0) {
    Err(msg) => assert_eq(msg, "Operation timed out")
    Ok(_) => assert_true(false)
  }
}

// Test 6: Bulkhead Pattern
test "bulkhead pattern for resource isolation" {
  type Bulkhead = {
    max_concurrent: Int,
    current_concurrent: Int,
    queue: Array[String>
  }
  
  let bulkhead_new = fn(max_concurrent: Int) -> Bulkhead {
    {
      max_concurrent,
      current_concurrent: 0,
      queue: []
    }
  }
  
  let bulkhead_acquire = fn(bh: Bulkhead, operation_id: String) -> Result[Bulkhead, String> {
    let Bulkhead(max_concurrent, current_concurrent, queue) = bh
    
    if current_concurrent < max_concurrent {
      // Can execute immediately
      Ok({
        max_concurrent,
        current_concurrent: current_concurrent + 1,
        queue
      })
    } else if queue.length() < 10 {
      // Add to queue
      Ok({
        max_concurrent,
        current_concurrent,
        queue: queue + [operation_id]
      })
    } else {
      // Queue is full
      Err("Bulkhead queue is full")
    }
  }
  
  let bulkhead_release = fn(bh: Bulkhead) -> Bulkhead {
    let Bulkhead(max_concurrent, current_concurrent, queue) = bh
    
    if current_concurrent > 0 {
      if queue.length() > 0 {
        // Process next from queue
        let new_queue = queue.slice(1, queue.length())
        {
          max_concurrent,
          current_concurrent: current_concurrent,  // One finishes, one starts
          queue: new_queue
        }
      } else {
        {
          max_concurrent,
          current_concurrent: current_concurrent - 1,
          queue
        }
      }
    } else {
      bh  // Nothing to release
    }
  }
  
  // Test bulkhead
  let bh = bulkhead_new(2)
  
  // Acquire first slot
  match bulkhead_acquire(bh, "op1") {
    Ok(new_bh) => {
      assert_eq(new_bh.current_concurrent, 1)
      
      // Acquire second slot
      match bulkhead_acquire(new_bh, "op2") {
        Ok(new_bh2) => {
          assert_eq(new_bh2.current_concurrent, 2)
          
          // Try to acquire third, should queue
          match bulkhead_acquire(new_bh2, "op3") {
            Ok(new_bh3) => {
              assert_eq(new_bh3.current_concurrent, 2)
              assert_eq(new_bh3.queue.length(), 1)
              assert_eq(new_bh3.queue[0], "op3")
              
              // Release one
              let bh4 = bulkhead_release(new_bh3)
              assert_eq(bh4.current_concurrent, 2)  // One finishes, queued one starts
              assert_eq(bh4.queue.length(), 0)
              
              // Release another
              let bh5 = bulkhead_release(bh4)
              assert_eq(bh5.current_concurrent, 1)
              
              // Release last
              let bh6 = bulkhead_release(bh5)
              assert_eq(bh6.current_concurrent, 0)
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 7: Graceful Degradation
test "graceful degradation mechanisms" {
  enum ServiceLevel {
    Full
    Degraded
    Minimal
    Unavailable
  }
  
  type ServiceStatus = {
    level: ServiceLevel,
    message: String
  }
  
  let check_service_health = fn(dependency_healthy: Bool, load_factor: Float) -> ServiceStatus {
    if !dependency_healthy {
      return { level: Unavailable, message: "Critical dependency unavailable" }
    }
    
    if load_factor > 0.9 {
      return { level: Minimal, message: "Service under extreme load" }
    }
    
    if load_factor > 0.7 {
      return { level: Degraded, message: "Service under high load" }
    }
    
    { level: Full, message: "Service operating normally" }
  }
  
  let get_feature_set = fn(level: ServiceLevel) -> Array[String] {
    match level {
      Full => ["feature1", "feature2", "feature3", "feature4", "feature5"]
      Degraded => ["feature1", "feature2", "feature3"]
      Minimal => ["feature1", "feature2"]
      Unavailable => []
    }
  }
  
  // Test service health checks
  let status1 = check_service_health(true, 0.5)
  match status1.level {
    Full => assert_true(true)
    _ => assert_true(false)
  }
  
  let status2 = check_service_health(true, 0.8)
  match status2.level {
    Degraded => assert_true(true)
    _ => assert_true(false)
  }
  
  let status3 = check_service_health(true, 0.95)
  match status3.level {
    Minimal => assert_true(true)
    _ => assert_true(false)
  }
  
  let status4 = check_service_health(false, 0.5)
  match status4.level {
    Unavailable => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test feature sets
  let full_features = get_feature_set(Full)
  assert_eq(full_features.length(), 5)
  
  let degraded_features = get_feature_set(Degraded)
  assert_eq(degraded_features.length(), 3)
  
  let minimal_features = get_feature_set(Minimal)
  assert_eq(minimal_features.length(), 2)
  
  let unavailable_features = get_feature_set(Unavailable)
  assert_eq(unavailable_features.length(), 0)
}

// Test 8: Error Recovery Strategies
test "error recovery strategies" {
  enum RecoveryStrategy {
    Retry
    Fallback
    CircuitBreak
    Degraded
  }
  
  type RecoveryAction = {
    strategy: RecoveryStrategy,
    max_attempts: Int,
    fallback_value: Option[String]
  }
  
  let execute_with_recovery = fn(action: RecoveryAction, operation: () -> Result[String, String>) -> Result[String, String] {
    match action.strategy {
      Retry => {
        let mut attempts = 0
        while attempts < action.max_attempts {
          match operation() {
            Ok(value) => return Ok(value),
            Err(_) => {
              attempts = attempts + 1
              if attempts >= action.max_attempts {
                break
              }
            }
          }
        }
        Err("Operation failed after retries")
      }
      Fallback => {
        match operation() {
          Ok(value) => Ok(value),
          Err(_) => {
            match action.fallback_value {
              Some(fallback) => Ok(fallback),
              None => Err("No fallback value available")
            }
          }
        }
      }
      CircuitBreak => {
        // Simplified circuit breaker - just fail fast
        Err("Circuit breaker is open")
      }
      Degraded => {
        // Try operation once, if fails return degraded response
        match operation() {
          Ok(value) => Ok(value),
          Err(_) => Ok("Degraded response")
        }
      }
    }
  }
  
  // Test recovery strategies
  let mut call_count = 0
  
  let flaky_operation = fn() -> Result[String, String] {
    call_count = call_count + 1
    if call_count < 3 {
      Err("Temporary failure")
    } else {
      Ok("Success")
    }
  }
  
  // Test retry strategy
  let retry_action = { strategy: Retry, max_attempts: 3, fallback_value: None }
  let retry_result = execute_with_recovery(retry_action, flaky_operation)
  match retry_result {
    Ok(value) => assert_eq(value, "Success")
    Err(_) => assert_true(false)
  }
  
  // Reset for next test
  call_count = 0
  
  // Test fallback strategy
  let fallback_action = { strategy: Fallback, max_attempts: 1, fallback_value: Some("Fallback value") }
  let fallback_result = execute_with_recovery(fallback_action, flaky_operation)
  match fallback_result {
    Ok(value) => assert_eq(value, "Fallback value")
    Err(_) => assert_true(false)
  }
  
  // Test circuit breaker strategy
  let circuit_action = { strategy: CircuitBreak, max_attempts: 1, fallback_value: None }
  let circuit_result = execute_with_recovery(circuit_action, flaky_operation)
  match circuit_result {
    Err(msg) => assert_eq(msg, "Circuit breaker is open")
    Ok(_) => assert_true(false)
  }
  
  // Test degraded strategy
  let degraded_action = { strategy: Degraded, max_attempts: 1, fallback_value: None }
  let degraded_result = execute_with_recovery(degraded_action, flaky_operation)
  match degraded_result {
    Ok(value) => assert_eq(value, "Degraded response")
    Err(_) => assert_true(false)
  }
}

// Test 9: Error Aggregation and Reporting
test "error aggregation and reporting" {
  type ErrorInfo = {
    error_type: String,
    message: String,
    timestamp: Int,
    count: Int
  }
  
  type ErrorReport = {
    total_errors: Int,
    error_types: Array[String],
    recent_errors: Array[ErrorInfo],
    most_common: ErrorInfo
  }
  
  let error_report_new = fn() -> ErrorReport {
    {
      total_errors: 0,
      error_types: [],
      recent_errors: [],
      most_common: { error_type: "", message: "", timestamp: 0, count: 0 }
    }
  }
  
  let error_report_add = fn(report: ErrorReport, error_type: String, message: String, timestamp: Int) -> ErrorReport {
    let error_info = { error_type, message, timestamp, count: 1 }
    
    // Update recent errors (keep last 10)
    let new_recent_errors = if report.recent_errors.length() >= 10 {
      report.recent_errors.slice(1, report.recent_errors.length()) + [error_info]
    } else {
      report.recent_errors + [error_info]
    }
    
    // Update error types
    let mut new_error_types = report.error_types
    let mut found = false
    for et in report.error_types {
      if et == error_type {
        found = true
        break
      }
    }
    if !found {
      new_error_types = new_error_types + [error_type]
    }
    
    // Update most common (simplified)
    let new_most_common = if report.total_errors == 0 {
      error_info
    } else {
      report.most_common
    }
    
    {
      total_errors: report.total_errors + 1,
      error_types: new_error_types,
      recent_errors: new_recent_errors,
      most_common: new_most_common
    }
  }
  
  // Test error reporting
  let report = error_report_new()
  
  let report1 = error_report_add(report, "ValidationError", "Invalid input", 1000)
  assert_eq(report1.total_errors, 1)
  assert_eq(report1.error_types.length(), 1)
  assert_eq(report1.recent_errors.length(), 1)
  
  let report2 = error_report_add(report1, "NetworkError", "Connection failed", 1010)
  assert_eq(report2.total_errors, 2)
  assert_eq(report2.error_types.length(), 2)
  assert_eq(report2.recent_errors.length(), 2)
  
  let report3 = error_report_add(report2, "ValidationError", "Missing field", 1020)
  assert_eq(report3.total_errors, 3)
  assert_eq(report3.error_types.length(), 2)  // ValidationError already exists
  assert_eq(report3.recent_errors.length(), 3)
  
  // Add more errors to test truncation
  let mut report4 = report3
  for i in 0..=8 {
    report4 = error_report_add(report4, "TestError", "Test message " + i.to_string(), 2000 + i)
  }
  
  assert_eq(report4.total_errors, 12)
  assert_eq(report4.recent_errors.length(), 10)  // Should be truncated to 10
}

// Test 10: Panic Recovery and System Stability
test "panic recovery and system stability" {
  // Safe operation wrapper
  let safe_execute = fn[T](operation: () -> T, fallback: T) -> T {
    // In a real implementation, this would catch panics
    // For testing, we'll simulate with a condition
    let mut should_panic = false
    
    // Simulate checking if operation might panic
    if should_panic {
      fallback
    } else {
      operation()
    }
  }
  
  // Resource cleanup guard
  type ResourceGuard[T] = {
    resource: T,
    cleanup: (T) -> Unit
  }
  
  let with_resource = fn[T, U](create: () -> T, cleanup: (T) -> Unit, use_resource: (T) -> U) -> U {
    let resource = create()
    let result = use_resource(resource)
    cleanup(resource)
    result
  }
  
  // Test safe execution
  let safe_operation = fn() -> String { "Safe result" }
  let result = safe_execute(safe_operation, "Fallback result")
  assert_eq(result, "Safe result")
  
  // Test resource guard
  let mut cleanup_called = false
  
  let create_resource = fn() -> Int { 42 }
  let cleanup_resource = fn(resource: Int) -> Unit { cleanup_called = true }
  let use_resource = fn(resource: Int) -> String { "Resource value: " + resource.to_string() }
  
  let guarded_result = with_resource(create_resource, cleanup_resource, use_resource)
  assert_eq(guarded_result, "Resource value: 42")
  assert_true(cleanup_called)
  
  // Test system stability monitors
  type SystemHealth = {
    memory_usage: Float,
    cpu_usage: Float,
    error_rate: Float,
    response_time: Float
  }
  
  let check_system_health = fn(health: SystemHealth) -> String {
    if health.memory_usage > 0.9 || health.cpu_usage > 0.9 || health.error_rate > 0.1 {
      return "Critical"
    }
    
    if health.memory_usage > 0.7 || health.cpu_usage > 0.7 || health.error_rate > 0.05 || health.response_time > 1000.0 {
      return "Warning"
    }
    
    "Healthy"
  }
  
  // Test system health checks
  let healthy_system = { memory_usage: 0.5, cpu_usage: 0.3, error_rate: 0.01, response_time: 100.0 }
  assert_eq(check_system_health(healthy_system), "Healthy")
  
  let warning_system = { memory_usage: 0.8, cpu_usage: 0.3, error_rate: 0.01, response_time: 100.0 }
  assert_eq(check_system_health(warning_system), "Warning")
  
  let critical_system = { memory_usage: 0.95, cpu_usage: 0.3, error_rate: 0.01, response_time: 100.0 }
  assert_eq(check_system_health(critical_system), "Critical")
}