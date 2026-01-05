// Azimuth Network Communication and Timeout Test Suite
// This file contains tests for network communication and timeout handling

// Test 1: HTTP Client Request/Response
test "http client request and response handling" {
  // HTTP request structure
  type HttpRequest = {
    method: String,
    url: String,
    headers: Array[(String, String)],
    body: Option[String],
    timeout: Int
  }
  
  // HTTP response structure
  type HttpResponse = {
    status_code: Int,
    headers: Array[(String, String)],
    body: Option[String],
    duration: Int
  }
  
  // Network error types
  enum NetworkError {
    TimeoutError
    ConnectionError
    DNSError
    SSLError
    RateLimitError
  }
  
  // Result type
  type Result[T] = {
    success: Bool,
    data: Option[T],
    error: Option[NetworkError]
  }
  
  // Create HTTP request
  let create_request = fn(method: String, url: String, timeout: Int) {
    {
      method,
      url,
      headers: [],
      body: None,
      timeout
    }
  }
  
  // Add header to request
  let add_header = fn(request: HttpRequest, key: String, value: String) {
    {
      request |
      headers: request.headers.push((key, value))
    }
  }
  
  // Set request body
  let set_body = fn(request: HttpRequest, body: String) {
    {
      request |
      body: Some(body)
    }
  }
  
  // Simulate HTTP request
  let send_request = fn(request: HttpRequest) {
    // Simulate network delay based on URL
    let base_delay = 100
    let url_delay = if request.url.contains("slow") { 500 } else { 0 }
    let total_delay = base_delay + url_delay
    
    // Simulate timeout
    if total_delay > request.timeout {
      return {
        success: false,
        data: None,
        error: Some(NetworkError::TimeoutError)
      }
    }
    
    // Simulate different responses based on URL
    let status_code = if request.url.contains("404") {
      404
    } else if request.url.contains("500") {
      500
    } else if request.url.contains("rate-limit") {
      429
    } else {
      200
    }
    
    let response_body = if status_code == 200 {
      Some("{ \"message\": \"success\", \"data\": \"sample data\" }")
    } else {
      Some("{ \"error\": \"request failed\" }")
    }
    
    let response_headers = [
      ("content-type", "application/json"),
      ("content-length", response_body.unwrap().length().to_string())
    ]
    
    {
      success: true,
      data: Some({
        status_code,
        headers: response_headers,
        body: response_body,
        duration: total_delay
      }),
      error: None
    }
  }
  
  // Test request creation
  let request = create_request("GET", "https://api.example.com/data", 1000)
  assert_eq(request.method, "GET")
  assert_eq(request.url, "https://api.example.com/data")
  assert_eq(request.timeout, 1000)
  assert_eq(request.headers.length(), 0)
  assert_eq(request.body, None)
  
  // Test adding headers
  let request_with_headers = add_header(add_header(request, "Authorization", "Bearer token123"), "Accept", "application/json")
  assert_eq(request_with_headers.headers.length(), 2)
  assert_true(request_with_headers.headers.contains(("Authorization", "Bearer token123")))
  assert_true(request_with_headers.headers.contains(("Accept", "application/json")))
  
  // Test setting body
  let request_with_body = set_body(request_with_headers, "{ \"key\": \"value\" }")
  assert_eq(request_with_body.body, Some("{ \"key\": \"value\" }"))
  
  // Test successful request
  let success_result = send_request(request_with_body)
  assert_true(success_result.success)
  assert_true(success_result.data.is_some())
  assert_eq(success_result.error, None)
  
  let response = success_result.data.unwrap()
  assert_eq(response.status_code, 200)
  assert_eq(response.duration, 100)
  assert_true(response.body.is_some())
  
  // Test timeout
  let timeout_request = create_request("GET", "https://slow-api.example.com/data", 200)
  let timeout_result = send_request(timeout_request)
  assert_false(timeout_result.success)
  assert_eq(timeout_result.data, None)
  assert_eq(timeout_result.error, Some(NetworkError::TimeoutError))
  
  // Test error responses
  let not_found_request = create_request("GET", "https://api.example.com/404", 1000)
  let not_found_result = send_request(not_found_request)
  assert_true(not_found_result.success)
  assert_eq(not_found_result.data.unwrap().status_code, 404)
  
  let server_error_request = create_request("GET", "https://api.example.com/500", 1000)
  let server_error_result = send_request(server_error_request)
  assert_true(server_error_result.success)
  assert_eq(server_error_result.data.unwrap().status_code, 500)
  
  let rate_limit_request = create_request("GET", "https://api.example.com/rate-limit", 1000)
  let rate_limit_result = send_request(rate_limit_request)
  assert_true(rate_limit_result.success)
  assert_eq(rate_limit_result.data.unwrap().status_code, 429)
}

// Test 2: Retry Mechanism with Backoff
test "retry mechanism with exponential backoff" {
  // Retry configuration
  type RetryConfig = {
    max_attempts: Int,
    base_delay: Int,
    max_delay: Int,
    backoff_multiplier: Float,
    retryable_errors: Array[NetworkError]
  }
  
  // Retry attempt information
  type RetryAttempt = {
    attempt_number: Int,
    delay: Int,
    error: Option[NetworkError]
  }
  
  // Retry result
  type RetryResult = {
    success: Bool,
    attempts: Array[RetryAttempt],
    total_duration: Int
  }
  
  // Network error types
  enum NetworkError {
    TimeoutError
    ConnectionError
    DNSError
    SSLError
    RateLimitError
  }
  
  // Default retry configuration
  let default_retry_config = {
    max_attempts: 3,
    base_delay: 100,
    max_delay: 5000,
    backoff_multiplier: 2.0,
    retryable_errors: [
      NetworkError::TimeoutError,
      NetworkError::ConnectionError,
      NetworkError::DNSError
    ]
  }
  
  // Calculate delay for retry attempt
  let calculate_delay = fn(attempt: Int, config: RetryConfig) {
    let exponential_delay = (config.base_delay.to_float() * 
      config.backoff_multiplier.pow((attempt - 1).to_float())).to_int()
    
    if exponential_delay > config.max_delay {
      config.max_delay
    } else {
      exponential_delay
    }
  }
  
  // Check if error is retryable
  let is_retryable = fn(error: NetworkError, config: RetryConfig) {
    config.retryable_errors.any(fn(retryable_error) { 
      match (error, retryable_error) {
        (NetworkError::TimeoutError, NetworkError::TimeoutError) => true
        (NetworkError::ConnectionError, NetworkError::ConnectionError) => true
        (NetworkError::DNSError, NetworkError::DNSError) => true
        (NetworkError::SSLError, NetworkError::SSLError) => true
        (NetworkError::RateLimitError, NetworkError::RateLimitError) => true
        _ => false
      }
    })
  }
  
  // Simulate operation that may fail
  let simulate_operation = fn(attempt: Int, should_fail_until: Int, error_type: NetworkError) {
    if attempt < should_fail_until {
      {
        success: false,
        data: None,
        error: Some(error_type)
      }
    } else {
      {
        success: true,
        data: Some("operation result"),
        error: None
      }
    }
  }
  
  // Execute operation with retry
  let execute_with_retry = fn(should_fail_until: Int, error_type: NetworkError, config: RetryConfig) {
    let mut attempts = []
    let mut total_duration = 0
    let mut success = false
    let mut attempt_number = 1
    
    while attempt_number <= config.max_attempts {
      let delay = if attempt_number > 1 {
        calculate_delay(attempt_number - 1, config)
      } else {
        0
      }
      
      total_duration = total_duration + delay
      
      let result = simulate_operation(attempt_number, should_fail_until, error_type)
      
      attempts = attempts.push({
        attempt_number,
        delay,
        error: result.error
      })
      
      if result.success {
        success = true
        break
      } else {
        let retryable = match result.error {
          Some(error) => is_retryable(error, config)
          None => false
        }
        
        if not(retryable) or attempt_number >= config.max_attempts {
          break
        }
      }
      
      attempt_number = attempt_number + 1
    }
    
    {
      success,
      attempts,
      total_duration
    }
  }
  
  // Test delay calculation
  assert_eq(calculate_delay(1, default_retry_config), 100)
  assert_eq(calculate_delay(2, default_retry_config), 200)
  assert_eq(calculate_delay(3, default_retry_config), 400)
  assert_eq(calculate_delay(4, default_retry_config), 800)
  
  // Test max delay cap
  let config_with_max_delay = {
    default_retry_config |
    max_delay: 300
  }
  
  assert_eq(calculate_delay(3, config_with_max_delay), 300)  // Would be 400, but capped at 300
  
  // Test retryable error check
  assert_true(is_retryable(NetworkError::TimeoutError, default_retry_config))
  assert_true(is_retryable(NetworkError::ConnectionError, default_retry_config))
  assert_false(is_retryable(NetworkError::RateLimitError, default_retry_config))  // Not in retryable list
  
  // Test successful retry (succeeds on 2nd attempt)
  let success_retry = execute_with_retry(2, NetworkError::TimeoutError, default_retry_config)
  assert_true(success_retry.success)
  assert_eq(success_retry.attempts.length(), 2)
  assert_eq(success_retry.total_duration, 100)  // Only one retry delay
  assert_eq(success_retry.attempts[0].error, Some(NetworkError::TimeoutError))
  assert_eq(success_retry.attempts[1].error, None)
  
  // Test failed retry (max attempts reached)
  let failed_retry = execute_with_retry(5, NetworkError::TimeoutError, default_retry_config)
  assert_false(failed_retry.success)
  assert_eq(failed_retry.attempts.length(), 3)
  assert_eq(failed_retry.total_duration, 100 + 200)  // Two retry delays
  
  // Test non-retryable error
  let config_with_rate_limit = {
    default_retry_config |
    retryable_errors: [
      NetworkError::TimeoutError,
      NetworkError::ConnectionError,
      NetworkError::DNSError,
      NetworkError::RateLimitError
    ]
  }
  
  let non_retryable_result = execute_with_retry(2, NetworkError::SSLError, config_with_rate_limit)
  assert_false(non_retryable_result.success)
  assert_eq(non_retryable_result.attempts.length(), 1)
  assert_eq(non_retryable_result.total_duration, 0)
  
  // Test immediate success (no retries needed)
  let immediate_success = execute_with_retry(1, NetworkError::TimeoutError, default_retry_config)
  assert_true(immediate_success.success)
  assert_eq(immediate_success.attempts.length(), 1)
  assert_eq(immediate_success.total_duration, 0)
}

// Test 3: Circuit Breaker Pattern
test "circuit breaker pattern implementation" {
  // Circuit breaker states
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // Circuit breaker configuration
  type CircuitBreakerConfig = {
    failure_threshold: Int,
    success_threshold: Int,
    timeout: Int
  }
  
  // Circuit breaker
  type CircuitBreaker = {
    state: CircuitState,
    failure_count: Int,
    success_count: Int,
    last_failure_time: Int,
    config: CircuitBreakerConfig
  }
  
  // Result type
  type Result[T] = {
    success: Bool,
    data: Option[T],
    circuit_open: Bool
  }
  
  // Create circuit breaker
  let create_circuit_breaker = fn(config: CircuitBreakerConfig) {
    {
      state: CircuitState::Closed,
      failure_count: 0,
      success_count: 0,
      last_failure_time: 0,
      config
    }
  }
  
  // Check if circuit should be attempted
  let can_attempt = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Closed => true
      CircuitState::Open => {
        current_time - breaker.last_failure_time > breaker.config.timeout
      }
      CircuitState::HalfOpen => true
    }
  }
  
  // Execute operation through circuit breaker
  let execute = fn(breaker: CircuitBreaker, operation_succeeds: Bool, current_time: Int) {
    if not(can_attempt(breaker, current_time)) {
      return {
        result: {
          success: false,
          data: None,
          circuit_open: true
        },
        updated_breaker: breaker
      }
    }
    
    let mut updated_breaker = breaker
    
    if operation_succeeds {
      match breaker.state {
        CircuitState::Closed => {
          updated_breaker.failure_count = 0
        }
        CircuitState::HalfOpen => {
          updated_breaker.success_count = updated_breaker.success_count + 1
          if updated_breaker.success_count >= breaker.config.success_threshold {
            updated_breaker.state = CircuitState::Closed
            updated_breaker.success_count = 0
            updated_breaker.failure_count = 0
          }
        }
        CircuitState::Open => {
          // Shouldn't reach here
        }
      }
      
      {
        result: {
          success: true,
          data: Some("operation result"),
          circuit_open: false
        },
        updated_breaker
      }
    } else {
      updated_breaker.failure_count = updated_breaker.failure_count + 1
      updated_breaker.last_failure_time = current_time
      
      match breaker.state {
        CircuitState::Closed => {
          if updated_breaker.failure_count >= breaker.config.failure_threshold {
            updated_breaker.state = CircuitState::Open
          }
        }
        CircuitState::HalfOpen => {
          updated_breaker.state = CircuitState::Open
          updated_breaker.success_count = 0
        }
        CircuitState::Open => {
          // Shouldn't reach here
        }
      }
      
      {
        result: {
          success: false,
          data: None,
          circuit_open: updated_breaker.state == CircuitState::Open
        },
        updated_breaker
      }
    }
  }
  
  // Test circuit breaker creation
  let config = {
    failure_threshold: 3,
    success_threshold: 2,
    timeout: 1000
  }
  
  let breaker = create_circuit_breaker(config)
  assert_eq(breaker.state, CircuitState::Closed)
  assert_eq(breaker.failure_count, 0)
  assert_eq(breaker.success_count, 0)
  
  // Test normal operation (circuit closed)
  let (result1, breaker1) = execute(breaker, true, 100)
  assert_true(result1.success)
  assert_false(result1.circuit_open)
  assert_eq(breaker1.state, CircuitState::Closed)
  assert_eq(breaker1.failure_count, 0)
  
  // Test failures in closed state
  let (result2, breaker2) = execute(breaker1, false, 200)
  assert_false(result2.success)
  assert_false(result2.circuit_open)
  assert_eq(breaker2.state, CircuitState::Closed)
  assert_eq(breaker2.failure_count, 1)
  
  let (result3, breaker3) = execute(breaker2, false, 300)
  assert_false(result3.success)
  assert_false(result3.circuit_open)
  assert_eq(breaker3.state, CircuitState::Closed)
  assert_eq(breaker3.failure_count, 2)
  
  // Test circuit opening after threshold
  let (result4, breaker4) = execute(breaker3, false, 400)
  assert_false(result4.success)
  assert_true(result4.circuit_open)
  assert_eq(breaker4.state, CircuitState::Open)
  assert_eq(breaker4.failure_count, 3)
  assert_eq(breaker4.last_failure_time, 400)
  
  // Test operation blocked when circuit is open
  let (result5, breaker5) = execute(breaker4, true, 500)
  assert_false(result5.success)
  assert_true(result5.circuit_open)
  assert_eq(breaker5.state, CircuitState::Open)
  
  // Test circuit transitioning to half-open after timeout
  let (result6, breaker6) = execute(breaker5, true, 1500)  // After timeout
  assert_true(result6.success)
  assert_false(result6.circuit_open)
  assert_eq(breaker6.state, CircuitState::HalfOpen)
  assert_eq(breaker6.success_count, 1)
  
  // Test success threshold in half-open state
  let (result7, breaker7) = execute(breaker6, true, 1600)
  assert_true(result7.success)
  assert_false(result7.circuit_open)
  assert_eq(breaker7.state, CircuitState::Closed)
  assert_eq(breaker7.failure_count, 0)
  assert_eq(breaker7.success_count, 0)
  
  // Test circuit opening again from half-open
  let breaker8 = { breaker7 | state: CircuitState::HalfOpen, success_count: 0 }
  let (result9, breaker9) = execute(breaker8, false, 1700)
  assert_false(result9.success)
  assert_true(result9.circuit_open)
  assert_eq(breaker9.state, CircuitState::Open)
  assert_eq(breaker9.success_count, 0)
}

// Test 4: Connection Pool Management
test "connection pool management" {
  // Connection structure
  type Connection = {
    id: String,
    created_at: Int,
    last_used: Int,
    in_use: Bool,
    is_valid: Bool
  }
  
  // Connection pool
  type ConnectionPool = {
    connections: Array[Connection],
    max_size: Int,
    min_size: Int,
    current_time: Int
  }
  
  // Pool statistics
  type PoolStats = {
    total_connections: Int,
    active_connections: Int,
    idle_connections: Int,
    invalid_connections: Int
  }
  
  // Create connection pool
  let create_pool = fn(min_size: Int, max_size: Int) {
    let mut connections = []
    
    for i in 0..min_size {
      connections = connections.push({
        id: "conn-" + i.to_string(),
        created_at: 0,
        last_used: 0,
        in_use: false,
        is_valid: true
      })
    }
    
    {
      connections,
      max_size,
      min_size,
      current_time: 0
    }
  }
  
  // Get pool statistics
  let get_stats = fn(pool: ConnectionPool) {
    let active = pool.connections.filter(fn(conn) { conn.in_use }).length()
    let idle = pool.connections.filter(fn(conn) { not(conn.in_use) and conn.is_valid }).length()
    let invalid = pool.connections.filter(fn(conn) { not(conn.is_valid) }).length()
    
    {
      total_connections: pool.connections.length(),
      active_connections: active,
      idle_connections: idle,
      invalid_connections: invalid
    }
  }
  
  // Acquire connection from pool
  let acquire_connection = fn(pool: ConnectionPool) {
    let mut updated_pool = { pool | current_time: pool.current_time + 1 }
    let mut acquired_connection = None
    
    // Find an available, valid connection
    for i in 0..pool.connections.length() {
      if not(pool.connections[i].in_use) and pool.connections[i].is_valid {
        acquired_connection = Some(pool.connections[i].id)
        updated_pool.connections[i] = {
          pool.connections[i] |
          in_use: true,
          last_used: updated_pool.current_time
        }
        break
      }
    }
    
    // If no available connection and pool not at max size, create new one
    if acquired_connection.is_none() and pool.connections.length() < pool.max_size {
      let new_id = "conn-" + pool.connections.length().to_string()
      let new_connection = {
        id: new_id,
        created_at: updated_pool.current_time,
        last_used: updated_pool.current_time,
        in_use: true,
        is_valid: true
      }
      
      acquired_connection = Some(new_id)
      updated_pool.connections = updated_pool.connections.push(new_connection)
    }
    
    (updated_pool, acquired_connection)
  }
  
  // Release connection back to pool
  let release_connection = fn(pool: ConnectionPool, connection_id: String) {
    let mut updated_pool = pool
    let mut found = false
    
    for i in 0..pool.connections.length() {
      if pool.connections[i].id == connection_id and pool.connections[i].in_use {
        updated_pool.connections[i] = {
          pool.connections[i] |
          in_use: false,
          last_used: updated_pool.current_time
        }
        found = true
        break
      }
    }
    
    if found {
      updated_pool
    } else {
      pool  // Connection not found or not in use
    }
  }
  
  // Validate connection
  let validate_connection = fn(pool: ConnectionPool, connection_id: String, is_valid: Bool) {
    let mut updated_pool = pool
    
    for i in 0..pool.connections.length() {
      if pool.connections[i].id == connection_id {
        updated_pool.connections[i] = {
          pool.connections[i] |
          is_valid
        }
        break
      }
    }
    
    updated_pool
  }
  
  // Clean up invalid connections
  let cleanup_connections = fn(pool: ConnectionPool) {
    let valid_connections = pool.connections.filter(fn(conn) { conn.is_valid })
    
    // Ensure minimum number of connections
    let mut result = { pool | connections: valid_connections }
    
    while result.connections.length() < result.min_size {
      let new_id = "conn-" + result.current_time.to_string() + "-" + result.connections.length().to_string()
      let new_connection = {
        id: new_id,
        created_at: result.current_time,
        last_used: result.current_time,
        in_use: false,
        is_valid: true
      }
      
      result.connections = result.connections.push(new_connection)
    }
    
    result
  }
  
  // Test pool creation
  let pool = create_pool(2, 5)
  assert_eq(pool.min_size, 2)
  assert_eq(pool.max_size, 5)
  assert_eq(pool.connections.length(), 2)
  
  let stats = get_stats(pool)
  assert_eq(stats.total_connections, 2)
  assert_eq(stats.active_connections, 0)
  assert_eq(stats.idle_connections, 2)
  assert_eq(stats.invalid_connections, 0)
  
  // Test acquiring connections
  let (pool1, conn1) = acquire_connection(pool)
  assert_true(conn1.is_some())
  assert_eq(conn1.unwrap(), "conn-0")
  
  let stats1 = get_stats(pool1)
  assert_eq(stats1.active_connections, 1)
  assert_eq(stats1.idle_connections, 1)
  
  let (pool2, conn2) = acquire_connection(pool1)
  assert_true(conn2.is_some())
  assert_eq(conn2.unwrap(), "conn-1")
  
  let stats2 = get_stats(pool2)
  assert_eq(stats2.active_connections, 2)
  assert_eq(stats2.idle_connections, 0)
  
  // Test creating new connection when pool is exhausted
  let (pool3, conn3) = acquire_connection(pool2)
  assert_true(conn3.is_some())
  assert_eq(conn3.unwrap(), "conn-2")  // New connection created
  
  let stats3 = get_stats(pool3)
  assert_eq(stats3.total_connections, 3)
  assert_eq(stats3.active_connections, 3)
  
  // Test releasing connections
  let pool4 = release_connection(pool3, "conn-0")
  let stats4 = get_stats(pool4)
  assert_eq(stats4.active_connections, 2)
  assert_eq(stats4.idle_connections, 1)
  
  // Test connection validation
  let pool5 = validate_connection(pool4, "conn-1", false)
  let stats5 = get_stats(pool5)
  assert_eq(stats5.invalid_connections, 1)
  
  // Test cleanup of invalid connections
  let pool6 = cleanup_connections(pool5)
  let stats6 = get_stats(pool6)
  assert_eq(stats6.invalid_connections, 0)
  assert_true(stats6.total_connections >= stats6.min_size)
  
  // Test max size limit
  let mut pool7 = pool6
  let mut acquired_connections = []
  
  // Acquire connections up to max size
  for i in 0..5 {
    let (new_pool, conn) = acquire_connection(pool7)
    pool7 = new_pool
    if conn.is_some() {
      acquired_connections = acquired_connections.push(conn.unwrap())
    }
  }
  
  let stats7 = get_stats(pool7)
  assert_eq(stats7.total_connections, 5)  // Should not exceed max_size
  
  // Try to acquire one more (should fail)
  let (pool8, conn8) = acquire_connection(pool7)
  assert_true(conn8.is_none())
  
  // Release all connections
  let mut pool9 = pool8
  for conn_id in acquired_connections {
    pool9 = release_connection(pool9, conn_id)
  }
  
  let stats9 = get_stats(pool9)
  assert_eq(stats9.active_connections, 0)
  assert_eq(stats9.idle_connections, stats9.total_connections)
}

// Test 5: Request Timeout and Cancellation
test "request timeout and cancellation" {
  // Request state
  enum RequestState {
    Pending
    Running
    Completed
    Cancelled
    TimedOut
  }
  
  // Request structure
  type Request = {
    id: String,
    state: RequestState,
    start_time: Int,
    timeout: Int,
    result: Option[String]
  }
  
  // Request manager
  type RequestManager = {
    requests: Array[Request],
    current_time: Int
  }
  
  // Create request manager
  let create_manager = fn() {
    {
      requests: [],
      current_time: 0
    }
  }
  
  // Create new request
  let create_request = fn(manager: RequestManager, timeout: Int) {
    let new_request = {
      id: "req-" + manager.requests.length().to_string(),
      state: RequestState::Pending,
      start_time: manager.current_time,
      timeout,
      result: None
    }
    
    {
      manager |
      requests: manager.requests.push(new_request)
    }
  }
  
  // Start request execution
  let start_request = fn(manager: RequestManager, request_id: String) {
    let mut updated_manager = manager
    
    for i in 0..manager.requests.length() {
      if manager.requests[i].id == request_id {
        updated_manager.requests[i] = {
          manager.requests[i] |
          state: RequestState::Running
        }
        break
      }
    }
    
    updated_manager
  }
  
  // Complete request
  let complete_request = fn(manager: RequestManager, request_id: String, result: String) {
    let mut updated_manager = manager
    
    for i in 0..manager.requests.length() {
      if manager.requests[i].id == request_id {
        updated_manager.requests[i] = {
          manager.requests[i] |
          state: RequestState::Completed,
          result: Some(result)
        }
        break
      }
    }
    
    updated_manager
  }
  
  // Cancel request
  let cancel_request = fn(manager: RequestManager, request_id: String) {
    let mut updated_manager = manager
    
    for i in 0..manager.requests.length() {
      if manager.requests[i].id == request_id {
        updated_manager.requests[i] = {
          manager.requests[i] |
          state: RequestState::Cancelled
        }
        break
      }
    }
    
    updated_manager
  }
  
  // Check for timeouts
  let check_timeouts = fn(manager: RequestManager) {
    let mut updated_manager = manager
    
    for i in 0..manager.requests.length() {
      let request = manager.requests[i]
      if request.state == RequestState::Running {
        let elapsed = manager.current_time - request.start_time
        if elapsed >= request.timeout {
          updated_manager.requests[i] = {
            request |
            state: RequestState::TimedOut
          }
        }
      }
    }
    
    updated_manager
  }
  
  // Advance time
  let advance_time = fn(manager: RequestManager, seconds: Int) {
    { manager | current_time: manager.current_time + seconds }
  }
  
  // Get request by ID
  let get_request = fn(manager: RequestManager, request_id: String) {
    let mut found_request = None
    
    for request in manager.requests {
      if request.id == request_id {
        found_request = Some(request)
        break
      }
    }
    
    found_request
  }
  
  // Test request creation
  let manager = create_manager()
  assert_eq(manager.requests.length(), 0)
  
  let manager1 = create_request(manager, 1000)
  assert_eq(manager1.requests.length(), 1)
  assert_eq(manager1.requests[0].state, RequestState::Pending)
  assert_eq(manager1.requests[0].timeout, 1000)
  
  // Test starting request
  let manager2 = start_request(manager1, "req-0")
  assert_eq(manager2.requests[0].state, RequestState::Running)
  
  // Test completing request
  let manager3 = complete_request(manager2, "req-0", "success result")
  assert_eq(manager3.requests[0].state, RequestState::Completed)
  assert_eq(manager3.requests[0].result, Some("success result"))
  
  // Test cancelling request
  let manager4 = create_request(manager3, 1000)
  let manager5 = start_request(manager4, "req-1")
  let manager6 = cancel_request(manager5, "req-1")
  
  assert_eq(manager6.requests[1].state, RequestState::Cancelled)
  
  // Test timeout detection
  let manager7 = create_request(manager6, 500)
  let manager8 = start_request(manager7, "req-2")
  
  // Advance time but not enough for timeout
  let manager9 = advance_time(manager8, 400)
  let manager10 = check_timeouts(manager9)
  
  assert_eq(manager10.requests[2].state, RequestState::Running)
  
  // Advance time past timeout
  let manager11 = advance_time(manager10, 200)
  let manager12 = check_timeouts(manager11)
  
  assert_eq(manager12.requests[2].state, RequestState::TimedOut)
  
  // Test multiple requests with different timeouts
  let manager13 = create_request(manager12, 300)
  let manager14 = create_request(manager13, 600)
  let manager15 = start_request(manager13, "req-3")
  let manager16 = start_request(manager14, "req-4")
  
  // Advance time and check timeouts
  let manager17 = advance_time(manager16, 400)
  let manager18 = check_timeouts(manager17)
  
  // req-3 should timeout, req-4 should still be running
  assert_eq(manager18.requests[3].state, RequestState::TimedOut)
  assert_eq(manager18.requests[4].state, RequestState::Running)
  
  // Complete the remaining request
  let manager19 = complete_request(manager18, "req-4", "final result")
  assert_eq(manager19.requests[4].state, RequestState::Completed)
  
  // Test request retrieval
  let request = get_request(manager19, "req-4")
  assert_true(request.is_some())
  assert_eq(request.unwrap().result, Some("final result"))
  
  let missing_request = get_request(manager19, "req-999")
  assert_true(missing_request.is_none())
}