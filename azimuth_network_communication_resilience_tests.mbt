// Azimuth Telemetry System - Network Communication Resilience Tests
// This file contains comprehensive test cases for network communication resilience

// Test 1: Connection Pool Management
test "connection pool management for telemetry services" {
  type Connection = {
    id: String,
    host: String,
    port: Int,
    active: Bool,
    last_used: Int
  }
  
  type ConnectionPool = {
    connections: Array[Connection>,
    max_connections: Int,
    active_connections: Int
  }
  
  let connection_pool_new = fn(max_connections: Int) -> ConnectionPool {
    {
      connections: [],
      max_connections,
      active_connections: 0
    }
  }
  
  let connection_pool_acquire = fn(pool: ConnectionPool, host: String, port: Int, current_time: Int) -> (ConnectionPool, Option[Connection]) {
    // Look for existing connection to the same host:port
    for i in 0..=pool.connections.length()-1 {
      let conn = pool.connections[i]
      if conn.host == host && conn.port == port && !conn.active {
        // Reactivate connection
        let reactivated = {
          id: conn.id,
          host,
          port,
          active: true,
          last_used: current_time
        }
        
        let new_connections = pool.connections.with(i, reactivated)
        let new_pool = {
          connections: new_connections,
          max_connections: pool.max_connections,
          active_connections: pool.active_connections + 1
        }
        
        return (new_pool, Some(reactivated))
      }
    }
    
    // Create new connection if we have capacity
    if pool.active_connections < pool.max_connections {
      let new_conn = {
        id: "conn_" + pool.connections.length().to_string(),
        host,
        port,
        active: true,
        last_used: current_time
      }
      
      let new_pool = {
        connections: pool.connections + [new_conn],
        max_connections: pool.max_connections,
        active_connections: pool.active_connections + 1
      }
      
      return (new_pool, Some(new_conn))
    }
    
    // No available connections
    (pool, None)
  }
  
  let connection_pool_release = fn(pool: ConnectionPool, connection_id: String) -> ConnectionPool {
    let mut new_connections = []
    let mut found = false
    
    for conn in pool.connections {
      if conn.id == connection_id {
        found = true
        new_connections = new_connections + [{
          id: conn.id,
          host: conn.host,
          port: conn.port,
          active: false,
          last_used: conn.last_used
        }]
      } else {
        new_connections = new_connections + [conn]
      }
    }
    
    let new_active = if found { pool.active_connections - 1 } else { pool.active_connections }
    
    {
      connections: new_connections,
      max_connections: pool.max_connections,
      active_connections: new_active
    }
  }
  
  // Test connection pool
  let pool1 = connection_pool_new(2)
  
  // Acquire first connection
  let (pool2, conn1) = connection_pool_acquire(pool1, "localhost", 8080, 1000)
  match conn1 {
    Some(c) => {
      assert_eq(c.host, "localhost")
      assert_eq(c.port, 8080)
      assert_true(c.active)
    }
    None => assert_true(false)
  }
  assert_eq(pool2.active_connections, 1)
  
  // Acquire second connection
  let (pool3, conn2) = connection_pool_acquire(pool2, "localhost", 8080, 1010)
  match conn2 {
    Some(c) => {
      assert_eq(c.host, "localhost")
      assert_eq(c.port, 8080)
      assert_true(c.active)
    }
    None => assert_true(false)
  }
  assert_eq(pool3.active_connections, 2)
  
  // Try to acquire third connection (should fail)
  let (pool4, conn3) = connection_pool_acquire(pool3, "localhost", 8080, 1020)
  match conn3 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  assert_eq(pool4.active_connections, 2)
  
  // Release first connection
  let pool5 = connection_pool_release(pool4, conn1.unwrap().id)
  assert_eq(pool5.active_connections, 1)
  
  // Acquire connection again (should reuse the released one)
  let (pool6, conn4) = connection_pool_acquire(pool5, "localhost", 8080, 1030)
  match conn4 {
    Some(c) => {
      assert_eq(c.host, "localhost")
      assert_eq(c.port, 8080)
      assert_true(c.active)
    }
    None => assert_true(false)
  }
  assert_eq(pool6.active_connections, 2)
}

// Test 2: Retry with Exponential Backoff
test "retry with exponential backoff for network requests" {
  type RetryConfig = {
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float
  }
  
  let retry_config_default = fn() -> RetryConfig {
    {
      max_attempts: 3,
      base_delay_ms: 100,
      max_delay_ms: 1000,
      backoff_multiplier: 2.0
    }
  }
  
  let calculate_delay = fn(attempt: Int, config: RetryConfig) -> Int {
    let delay = Float::from_int(config.base_delay_ms) * 
                (config.backoff_multiplier ** Float::from_int(attempt))
    
    let delay_int = delay.to_int()
    if delay_int > config.max_delay_ms {
      config.max_delay_ms
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
          
          // Calculate delay (in real implementation, would wait)
          let delay = calculate_delay(attempt, config)
          attempt = attempt + 1
          
          // In a real implementation, we would wait for 'delay' milliseconds
          // For testing, we just continue
        }
      }
    }
    
    Err("Max attempts exceeded")
  }
  
  // Test retry with backoff
  let mut call_count = 0
  
  let flaky_operation = fn() -> Result[String, String] {
    call_count = call_count + 1
    if call_count < 3 {
      Err("Temporary failure")
    } else {
      Ok("Success")
    }
  }
  
  let config = retry_config_default()
  let result = retry_with_backoff(config, flaky_operation)
  
  match result {
    Ok(value) => {
      assert_eq(value, "Success")
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
      assert_eq(call_count, 3)
    }
    Ok(_) => assert_true(false)
  }
  
  // Test delay calculation
  assert_eq(calculate_delay(0, config), 100)
  assert_eq(calculate_delay(1, config), 200)
  assert_eq(calculate_delay(2, config), 400)
  assert_eq(calculate_delay(3, config), 800)
  assert_eq(calculate_delay(4, config), 1000)  // Capped at max
  
  // Test with custom config
  let custom_config = {
    max_attempts: 5,
    base_delay_ms: 50,
    max_delay_ms: 500,
    backoff_multiplier: 1.5
  }
  
  assert_eq(calculate_delay(0, custom_config), 50)
  assert_eq(calculate_delay(1, custom_config), 75)
  assert_eq(calculate_delay(2, custom_config), 112)  // 112.5 rounded down
  assert_eq(calculate_delay(3, custom_config), 168)  // 168.75 rounded down
  assert_eq(calculate_delay(4, custom_config), 253)  // 253.125 rounded down
  assert_eq(calculate_delay(5, custom_config), 379)  // 379.6875 rounded down
  assert_eq(calculate_delay(6, custom_config), 500)  // Capped at max
}

// Test 3: Circuit Breaker Pattern
test "circuit breaker pattern for service resilience" {
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  type CircuitBreaker = {
    state: CircuitState,
    failure_count: Int,
    failure_threshold: Int,
    timeout_ms: Int,
    last_failure_time: Int,
    success_threshold: Int
  }
  
  let circuit_breaker_new = fn(failure_threshold: Int, timeout_ms: Int) -> CircuitBreaker {
    {
      state: Closed,
      failure_count: 0,
      failure_threshold,
      timeout_ms,
      last_failure_time: 0,
      success_threshold: 2
    }
  }
  
  let circuit_breaker_call = fn[T](breaker: CircuitBreaker, operation: () -> Result[T, String], current_time: Int) -> (CircuitBreaker, Result[T, String]) {
    match breaker.state {
      Closed => {
        match operation() {
          Ok(value) => {
            // Success, reset failure count
            let new_breaker = {
              state: Closed,
              failure_count: 0,
              failure_threshold: breaker.failure_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: breaker.last_failure_time,
              success_threshold: breaker.success_threshold
            }
            (new_breaker, Ok(value))
          }
          Err(error) => {
            // Failure, increment count
            let new_failure_count = breaker.failure_count + 1
            let new_state = if new_failure_count >= breaker.failure_threshold {
              Open
            } else {
              Closed
            }
            
            let new_breaker = {
              state: new_state,
              failure_count: new_failure_count,
              failure_threshold: breaker.failure_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: current_time,
              success_threshold: breaker.success_threshold
            }
            (new_breaker, Err(error))
          }
        }
      }
      Open => {
        // Check if timeout has passed
        if current_time - breaker.last_failure_time >= breaker.timeout_ms {
          // Try half-open state
          let new_breaker = {
            state: HalfOpen,
            failure_count: breaker.failure_count,
            failure_threshold: breaker.failure_threshold,
            timeout_ms: breaker.timeout_ms,
            last_failure_time: breaker.last_failure_time,
            success_threshold: breaker.success_threshold
          }
          
          match operation() {
            Ok(value) => {
              // Success, close the circuit
              let closed_breaker = {
                state: Closed,
                failure_count: 0,
                failure_threshold: breaker.failure_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: breaker.last_failure_time,
                success_threshold: breaker.success_threshold
              }
              (closed_breaker, Ok(value))
            }
            Err(error) => {
              // Failure, open again
              let open_breaker = {
                state: Open,
                failure_count: breaker.failure_count + 1,
                failure_threshold: breaker.failure_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: current_time,
                success_threshold: breaker.success_threshold
              }
              (open_breaker, Err(error))
            }
          }
        } else {
          // Still in timeout, fail fast
          (breaker, Err("Circuit breaker is open"))
        }
      }
      HalfOpen => {
        match operation() {
          Ok(value) => {
            // Success, close the circuit
            let new_breaker = {
              state: Closed,
              failure_count: 0,
              failure_threshold: breaker.failure_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: breaker.last_failure_time,
              success_threshold: breaker.success_threshold
            }
            (new_breaker, Ok(value))
          }
          Err(error) => {
            // Failure, open again
            let new_breaker = {
              state: Open,
              failure_count: breaker.failure_count + 1,
              failure_threshold: breaker.failure_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: current_time,
              success_threshold: breaker.success_threshold
            }
            (new_breaker, Err(error))
          }
        }
      }
    }
  }
  
  // Test circuit breaker
  let mut failure_count = 0
  
  let failing_operation = fn() -> Result[String, String] {
    failure_count = failure_count + 1
    if failure_count <= 3 {
      Err("Service failure")
    } else {
      Ok("Service recovered")
    }
  }
  
  let breaker1 = circuit_breaker_new(3, 1000)
  
  // First few calls should fail but circuit remains closed
  let (breaker2, result1) = circuit_breaker_call(breaker1, failing_operation, 1000)
  match result1 {
    Err(_) => assert_true(true)
    Ok(_) => assert_true(false)
  }
  
  let (breaker3, result2) = circuit_breaker_call(breaker2, failing_operation, 1100)
  match result2 {
    Err(_) => assert_true(true)
    Ok(_) => assert_true(false)
  }
  
  let (breaker4, result3) = circuit_breaker_call(breaker3, failing_operation, 1200)
  match result3 {
    Err(_) => assert_true(true)
    Ok(_) => assert_true(false)
  }
  
  // Circuit should now be open
  match breaker4.state {
    Open => assert_true(true)
    _ => assert_true(false)
  }
  
  // Next call should fail fast
  let (breaker5, result4) = circuit_breaker_call(breaker4, failing_operation, 1300)
  match result4 {
    Err(msg) => assert_eq(msg, "Circuit breaker is open")
    Ok(_) => assert_true(false)
  }
  
  // After timeout, should try half-open
  let (breaker6, result5) = circuit_breaker_call(breaker5, failing_operation, 2500)
  match result5 {
    Ok(value) => assert_eq(value, "Service recovered")
    Err(_) => assert_true(false)
  }
  
  // Circuit should be closed again
  match breaker6.state {
    Closed => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 4: Timeout Handling
test "timeout handling for network operations" {
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

// Test 5: Bulkhead Pattern
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
  
  let bulkhead_acquire = fn(bulkhead: Bulkhead, operation_id: String) -> Result[Bulkhead, String] {
    if bulkhead.current_concurrent < bulkhead.max_concurrent {
      // Can execute immediately
      Ok({
        max_concurrent: bulkhead.max_concurrent,
        current_concurrent: bulkhead.current_concurrent + 1,
        queue: bulkhead.queue
      })
    } else if bulkhead.queue.length() < 10 {
      // Add to queue
      Ok({
        max_concurrent: bulkhead.max_concurrent,
        current_concurrent: bulkhead.current_concurrent,
        queue: bulkhead.queue + [operation_id]
      })
    } else {
      // Queue is full
      Err("Bulkhead queue is full")
    }
  }
  
  let bulkhead_release = fn(bulkhead: Bulkhead) -> Bulkhead {
    if bulkhead.current_concurrent > 0 {
      if bulkhead.queue.length() > 0 {
        // Process next from queue
        let new_queue = bulkhead.queue.slice(1, bulkhead.queue.length())
        {
          max_concurrent: bulkhead.max_concurrent,
          current_concurrent: bulkhead.current_concurrent,  // One finishes, one starts
          queue: new_queue
        }
      } else {
        {
          max_concurrent: bulkhead.max_concurrent,
          current_concurrent: bulkhead.current_concurrent - 1,
          queue: bulkhead.queue
        }
      }
    } else {
      bulkhead  // Nothing to release
    }
  }
  
  // Test bulkhead
  let bulkhead1 = bulkhead_new(2)
  
  // Acquire first slot
  match bulkhead_acquire(bulkhead1, "op1") {
    Ok(bulkhead2) => {
      assert_eq(bulkhead2.current_concurrent, 1)
      
      // Acquire second slot
      match bulkhead_acquire(bulkhead2, "op2") {
        Ok(bulkhead3) => {
          assert_eq(bulkhead3.current_concurrent, 2)
          
          // Try to acquire third, should queue
          match bulkhead_acquire(bulkhead3, "op3") {
            Ok(bulkhead4) => {
              assert_eq(bulkhead4.current_concurrent, 2)
              assert_eq(bulkhead4.queue.length(), 1)
              assert_eq(bulkhead4.queue[0], "op3")
              
              // Release one
              let bulkhead5 = bulkhead_release(bulkhead4)
              assert_eq(bulkhead5.current_concurrent, 2)  // One finishes, queued one starts
              assert_eq(bulkhead5.queue.length(), 0)
              
              // Release another
              let bulkhead6 = bulkhead_release(bulkhead5)
              assert_eq(bulkhead6.current_concurrent, 1)
              
              // Release last
              let bulkhead7 = bulkhead_release(bulkhead6)
              assert_eq(bulkhead7.current_concurrent, 0)
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

// Test 6: Request Batching
test "request batching for telemetry data" {
  type BatchRequest = {
    requests: Array[String>,
    max_batch_size: Int,
    max_wait_time_ms: Int
  }
  
  let batch_request_new = fn(max_batch_size: Int, max_wait_time_ms: Int) -> BatchRequest {
    {
      requests: [],
      max_batch_size,
      max_wait_time_ms
    }
  }
  
  let batch_request_add = fn(batch: BatchRequest, request: String) -> BatchRequest {
    {
      requests: batch.requests + [request],
      max_batch_size: batch.max_batch_size,
      max_wait_time_ms: batch.max_wait_time_ms
    }
  }
  
  let batch_request_should_flush = fn(batch: BatchRequest, elapsed_time_ms: Int) -> Bool {
    batch.requests.length() >= batch.max_batch_size || elapsed_time_ms >= batch.max_wait_time_ms
  }
  
  let batch_request_flush = fn(batch: BatchRequest) -> (BatchRequest, Array[String]) {
    let requests = batch.requests
    let new_batch = {
      requests: [],
      max_batch_size: batch.max_batch_size,
      max_wait_time_ms: batch.max_wait_time_ms
    }
    
    (new_batch, requests)
  }
  
  // Test request batching
  let batch1 = batch_request_new(3, 100)
  
  // Add requests
  let batch2 = batch_request_add(batch1, "req1")
  let batch3 = batch_request_add(batch2, "req2")
  
  // Should not flush yet
  assert_false(batch_request_should_flush(batch3, 50))
  
  let batch4 = batch_request_add(batch3, "req3")
  
  // Should flush due to batch size
  assert_true(batch_request_should_flush(batch4, 50))
  
  let (batch5, flushed_requests) = batch_request_flush(batch4)
  assert_eq(flushed_requests.length(), 3)
  assert_eq(flushed_requests[0], "req1")
  assert_eq(flushed_requests[1], "req2")
  assert_eq(flushed_requests[2], "req3")
  assert_eq(batch5.requests.length(), 0)
  
  // Add more requests
  let batch6 = batch_request_add(batch5, "req4")
  let batch7 = batch_request_add(batch6, "req5")
  
  // Should flush due to time
  assert_true(batch_request_should_flush(batch7, 150))
  
  let (batch8, flushed_requests2) = batch_request_flush(batch7)
  assert_eq(flushed_requests2.length(), 2)
  assert_eq(flushed_requests2[0], "req4")
  assert_eq(flushed_requests2[1], "req5")
}

// Test 7: Adaptive Throttling
test "adaptive throttling for rate limiting" {
  type Throttler = {
    requests_per_second: Int,
    current_capacity: Int,
    last_refill_time: Int,
    max_capacity: Int
  }
  
  let throttler_new = fn(requests_per_second: Int, max_capacity: Int) -> Throttler {
    {
      requests_per_second,
      current_capacity: max_capacity,
      last_refill_time: 0,
      max_capacity
    }
  }
  
  let throttler_refill = fn(throttler: Throttler, current_time: Int) -> Throttler {
    let time_passed = current_time - throttler.last_refill_time
    let tokens_to_add = time_passed * throttler.requests_per_second / 1000  // Convert ms to seconds
    
    let new_capacity = min(throttler.current_capacity + tokens_to_add, throttler.max_capacity)
    
    {
      requests_per_second: throttler.requests_per_second,
      current_capacity: new_capacity,
      last_refill_time: current_time,
      max_capacity: throttler.max_capacity
    }
  }
  
  let throttler_try_acquire = fn(throttler: Throttler, current_time: Int) -> (Throttler, Bool) {
    let refilled = throttler_refill(throttler, current_time)
    
    if refilled.current_capacity > 0 {
      let updated = {
        requests_per_second: refilled.requests_per_second,
        current_capacity: refilled.current_capacity - 1,
        last_refill_time: refilled.last_refill_time,
        max_capacity: refilled.max_capacity
      }
      (updated, true)
    } else {
      (refilled, false)
    }
  }
  
  // Test throttling
  let throttler1 = throttler_new(10, 20)  // 10 requests per second, max 20 capacity
  
  // Try to acquire tokens
  let (throttler2, acquired1) = throttler_try_acquire(throttler1, 1000)
  assert_true(acquired1)
  assert_eq(throttler2.current_capacity, 19)
  
  let (throttler3, acquired2) = throttler_try_acquire(throttler2, 1000)
  assert_true(acquired2)
  assert_eq(throttler3.current_capacity, 18)
  
  // Fast forward time and refill
  let (throttler4, acquired3) = throttler_try_acquire(throttler3, 2000)  // 1 second later
  assert_true(acquired3)
  assert_eq(throttler4.current_capacity, 27)  // 18 + 10 - 1 = 27, but capped at 20, so 20 - 1 = 19
  
  // Exhaust capacity
  let mut throttler = throttler4
  let mut acquired_count = 0
  
  for i in 0..=30 {
    let (new_throttler, acquired) = throttler_try_acquire(throttler, 2000)
    throttler = new_throttler
    if acquired {
      acquired_count = acquired_count + 1
    } else {
      break
    }
  }
  
  assert_eq(acquired_count, 19)  // Should have 19 tokens available
  assert_eq(throttler.current_capacity, 0)
  
  // Next request should be denied
  let (throttler5, acquired4) = throttler_try_acquire(throttler, 2000)
  assert_false(acquired4)
  assert_eq(throttler5.current_capacity, 0)
}

// Test 8: Health Check Monitoring
test "health check monitoring for service dependencies" {
  type HealthStatus = {
    healthy: Bool,
    last_check_time: Int,
    consecutive_failures: Int,
    response_time_ms: Int
  }
  
  type HealthChecker = {
    services: Array[(String, HealthStatus)>
  }
  
  let health_checker_new = fn() -> HealthChecker {
    { services: [] }
  }
  
  let health_checker_add_service = fn(checker: HealthChecker, service_name: String) -> HealthChecker {
    let status = {
      healthy: true,
      last_check_time: 0,
      consecutive_failures: 0,
      response_time_ms: 0
    }
    
    { services: checker.services + [(service_name, status)] }
  }
  
  let health_checker_update = fn(checker: HealthChecker, service_name: String, is_healthy: Bool, response_time_ms: Int, current_time: Int) -> HealthChecker {
    let mut new_services = []
    
    for (name, status) in checker.services {
      if name == service_name {
        let new_failures = if is_healthy { 0 } else { status.consecutive_failures + 1 }
        
        let new_status = {
          healthy: is_healthy,
          last_check_time: current_time,
          consecutive_failures: new_failures,
          response_time_ms
        }
        
        new_services = new_services + [(name, new_status)]
      } else {
        new_services = new_services + [(name, status)]
      }
    }
    
    { services: new_services }
  }
  
  let health_checker_is_healthy = fn(checker: HealthChecker, service_name: String) -> Bool {
    for (name, status) in checker.services {
      if name == service_name {
        return status.healthy
      }
    }
    false  // Service not found
  }
  
  let health_checker_get_status = fn(checker: HealthChecker, service_name: String) -> Option[HealthStatus] {
    for (name, status) in checker.services {
      if name == service_name {
        return Some(status)
      }
    }
    None
  }
  
  // Test health checker
  let checker1 = health_checker_new()
  let checker2 = health_checker_add_service(checker1, "service1")
  let checker3 = health_checker_add_service(checker2, "service2")
  
  // Initial status should be healthy
  assert_true(health_checker_is_healthy(checker3, "service1"))
  assert_true(health_checker_is_healthy(checker3, "service2"))
  
  // Update service1 as unhealthy
  let checker4 = health_checker_update(checker3, "service1", false, 500, 1000)
  
  assert_false(health_checker_is_healthy(checker4, "service1"))
  assert_true(health_checker_is_healthy(checker4, "service2"))
  
  // Check status details
  match health_checker_get_status(checker4, "service1") {
    Some(status) => {
      assert_false(status.healthy)
      assert_eq(status.last_check_time, 1000)
      assert_eq(status.consecutive_failures, 1)
      assert_eq(status.response_time_ms, 500)
    }
    None => assert_true(false)
  }
  
  // Update service1 as healthy again
  let checker5 = health_checker_update(checker4, "service1", true, 100, 2000)
  
  assert_true(health_checker_is_healthy(checker5, "service1"))
  
  match health_checker_get_status(checker5, "service1") {
    Some(status) => {
      assert_true(status.healthy)
      assert_eq(status.last_check_time, 2000)
      assert_eq(status.consecutive_failures, 0)
      assert_eq(status.response_time_ms, 100)
    }
    None => assert_true(false)
  }
}

// Test 9: Graceful Degradation
test "graceful degradation for service failures" {
  enum ServiceLevel {
    Full
    Degraded
    Minimal
    Unavailable
  }
  
  type ServiceStatus = {
    level: ServiceLevel,
    message: String,
    features: Array[String]
  }
  
  let check_service_health = fn(dependency_healthy: Bool, load_factor: Float) -> ServiceStatus {
    if !dependency_healthy {
      return {
        level: Unavailable,
        message: "Critical dependency unavailable",
        features: []
      }
    }
    
    if load_factor > 0.9 {
      return {
        level: Minimal,
        message: "Service under extreme load",
        features: ["basic_read"]
      }
    }
    
    if load_factor > 0.7 {
      return {
        level: Degraded,
        message: "Service under high load",
        features: ["basic_read", "basic_write"]
      }
    }
    
    {
      level: Full,
      message: "Service operating normally",
      features: ["basic_read", "basic_write", "advanced_analytics", "real_time_updates"]
    }
  }
  
  let is_feature_available = fn(status: ServiceStatus, feature: String) -> Bool {
    for f in status.features {
      if f == feature {
        return true
      }
    }
    false
  }
  
  // Test service health checks
  let status1 = check_service_health(true, 0.5)
  match status1.level {
    Full => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(is_feature_available(status1, "basic_read"))
  assert_true(is_feature_available(status1, "basic_write"))
  assert_true(is_feature_available(status1, "advanced_analytics"))
  assert_true(is_feature_available(status1, "real_time_updates"))
  
  let status2 = check_service_health(true, 0.8)
  match status2.level {
    Degraded => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(is_feature_available(status2, "basic_read"))
  assert_true(is_feature_available(status2, "basic_write"))
  assert_false(is_feature_available(status2, "advanced_analytics"))
  assert_false(is_feature_available(status2, "real_time_updates"))
  
  let status3 = check_service_health(true, 0.95)
  match status3.level {
    Minimal => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(is_feature_available(status3, "basic_read"))
  assert_false(is_feature_available(status3, "basic_write"))
  assert_false(is_feature_available(status3, "advanced_analytics"))
  assert_false(is_feature_available(status3, "real_time_updates"))
  
  let status4 = check_service_health(false, 0.5)
  match status4.level {
    Unavailable => assert_true(true)
    _ => assert_true(false)
  }
  assert_false(is_feature_available(status4, "basic_read"))
  assert_false(is_feature_available(status4, "basic_write"))
  assert_false(is_feature_available(status4, "advanced_analytics"))
  assert_false(is_feature_available(status4, "real_time_updates"))
}

// Test 10: Network Partition Detection
test "network partition detection and recovery" {
  type NetworkNode = {
    id: String,
    reachable: Bool,
    last_seen: Int
  }
  
  type NetworkPartitionDetector = {
    nodes: Array[NetworkNode>,
    partition_threshold_ms: Int
  }
  
  let network_partition_detector_new = fn(threshold_ms: Int) -> NetworkPartitionDetector {
    { nodes: [], partition_threshold_ms: threshold_ms }
  }
  
  let network_partition_detector_add_node = fn(detector: NetworkPartitionDetector, node_id: String, current_time: Int) -> NetworkPartitionDetector {
    let node = {
      id: node_id,
      reachable: true,
      last_seen: current_time
    }
    
    { nodes: detector.nodes + [node], partition_threshold_ms: detector.partition_threshold_ms }
  }
  
  let network_partition_detector_update_node = fn(detector: NetworkPartitionDetector, node_id: String, reachable: Bool, current_time: Int) -> NetworkPartitionDetector {
    let mut new_nodes = []
    
    for node in detector.nodes {
      if node.id == node_id {
        let updated_node = {
          id: node_id,
          reachable,
          last_seen: current_time
        }
        new_nodes = new_nodes + [updated_node]
      } else {
        new_nodes = new_nodes + [node]
      }
    }
    
    { nodes: new_nodes, partition_threshold_ms: detector.partition_threshold_ms }
  }
  
  let network_partition_detector_check_partitions = fn(detector: NetworkPartitionDetector, current_time: Int) -> Array[String] {
    let mut partitioned_nodes = []
    
    for node in detector.nodes {
      if current_time - node.last_seen > detector.partition_threshold_ms {
        partitioned_nodes = partitioned_nodes + [node.id]
      }
    }
    
    partitioned_nodes
  }
  
  let network_partition_detector_is_partitioned = fn(detector: NetworkPartitionDetector, node_id: String, current_time: Int) -> Bool {
    for node in detector.nodes {
      if node.id == node_id {
        return current_time - node.last_seen > detector.partition_threshold_ms
      }
    }
    false
  }
  
  // Test network partition detection
  let detector1 = network_partition_detector_new(5000)  // 5 seconds threshold
  let detector2 = network_partition_detector_add_node(detector1, "node1", 1000)
  let detector3 = network_partition_detector_add_node(detector2, "node2", 1000)
  let detector4 = network_partition_detector_add_node(detector3, "node3", 1000)
  
  // Initially, no partitions
  let partitions1 = network_partition_detector_check_partitions(detector4, 2000)
  assert_eq(partitions1.length(), 0)
  
  assert_false(network_partition_detector_is_partitioned(detector4, "node1", 2000))
  assert_false(network_partition_detector_is_partitioned(detector4, "node2", 2000))
  assert_false(network_partition_detector_is_partitioned(detector4, "node3", 2000))
  
  // Update node1 as unreachable
  let detector5 = network_partition_detector_update_node(detector4, "node1", false, 2000)
  
  // Still not partitioned (within threshold)
  assert_false(network_partition_detector_is_partitioned(detector5, "node1", 2000))
  
  // Fast forward time beyond threshold
  let partitions2 = network_partition_detector_check_partitions(detector5, 8000)
  assert_eq(partitions2.length(), 1)
  assert_eq(partitions2[0], "node1")
  
  assert_true(network_partition_detector_is_partitioned(detector5, "node1", 8000))
  assert_false(network_partition_detector_is_partitioned(detector5, "node2", 8000))
  assert_false(network_partition_detector_is_partitioned(detector5, "node3", 8000))
  
  // Node1 comes back online
  let detector6 = network_partition_detector_update_node(detector5, "node1", true, 8000)
  
  // No longer partitioned
  assert_false(network_partition_detector_is_partitioned(detector6, "node1", 8000))
  
  let partitions3 = network_partition_detector_check_partitions(detector6, 8000)
  assert_eq(partitions3.length(), 0)
}