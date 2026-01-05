// Azimuth Premium Network Communication Tests
// This file contains high-quality test cases for network communication and timeout handling

// Test 1: HTTP Client Operations
test "http client operations" {
  // HTTP request and response types
  type HttpMethod {
    Get
    Post
    Put
    Delete
  }
  
  type HttpRequest {
    method : HttpMethod
    url : String
    headers : Array[(String, String)]
    body : Option[String]
  }
  
  type HttpResponse {
    status_code : Int
    headers : Array[(String, String)]
    body : String
  }
  
  type NetworkError {
    Timeout
    ConnectionFailed
    HostNotFound
    InvalidResponse
  }
  
  // Create HTTP request
  let create_request = fn(method : HttpMethod, url : String) -> HttpRequest {
    HttpRequest {
      method: method,
      url: url,
      headers: [],
      body: None
    }
  }
  
  let add_header = fn(request : HttpRequest, name : String, value : String) -> HttpRequest {
    HttpRequest {
      method: request.method,
      url: request.url,
      headers: request.headers.push((name, value)),
      body: request.body
    }
  }
  
  let set_body = fn(request : HttpRequest, body : String) -> HttpRequest {
    HttpRequest {
      method: request.method,
      url: request.url,
      headers: request.headers,
      body: Some(body)
    }
  }
  
  // Simulate HTTP client
  let send_request = fn(request : HttpRequest) -> Result[HttpResponse, NetworkError] {
    // Simulate network behavior based on URL
    if request.url.contains("timeout") {
      Err(Timeout)
    } else if request.url.contains("invalid") {
      Err(InvalidResponse)
    } else if request.url.contains("notfound") {
      Err(HostNotFound)
    } else {
      let status_code = match request.method {
        Get => 200
        Post => 201
        Put => 200
        Delete => 204
      }
      
      let response_body = match request.method {
        Get => "GET response for " + request.url
        Post => match request.body {
          Some(body) => "POST received: " + body
          None => "POST received: empty body"
        }
        Put => match request.body {
          Some(body) => "PUT updated: " + body
          None => "PUT updated: empty body"
        }
        Delete => "Resource deleted"
      }
      
      Ok(HttpResponse {
        status_code: status_code,
        headers: [("Content-Type", "text/plain"), ("Content-Length", response_body.length().to_string())],
        body: response_body
      })
    }
  }
  
  // Test HTTP request creation
  let get_request = create_request(Get, "https://api.example.com/data")
  assert_eq(get_request.url, "https://api.example.com/data")
  
  let post_request = create_request(Post, "https://api.example.com/data")
    |> add_header("Content-Type", "application/json")
    |> set_body("{\"name\":\"test\"}")
  
  assert_eq(post_request.url, "https://api.example.com/data")
  assert_eq(post_request.headers.length(), 1)
  assert_eq(post_request.headers[0], ("Content-Type", "application/json"))
  assert_eq(post_request.body, Some("{\"name\":\"test\"}"))
  
  // Test successful requests
  let get_result = send_request(get_request)
  match get_result {
    Ok(response) => {
      assert_eq(response.status_code, 200)
      assert_eq(response.body, "GET response for https://api.example.com/data")
      assert_eq(response.headers[0], ("Content-Type", "text/plain"))
    }
    Err(_) => assert_true(false)
  }
  
  let post_result = send_request(post_request)
  match post_result {
    Ok(response) => {
      assert_eq(response.status_code, 201)
      assert_eq(response.body, "POST received: {\"name\":\"test\"}")
    }
    Err(_) => assert_true(false)
  }
  
  // Test error conditions
  let timeout_request = create_request(Get, "https://api.example.com/timeout")
  let timeout_result = send_request(timeout_request)
  match timeout_result {
    Ok(_) => assert_true(false)
    Err(Timeout) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  let invalid_request = create_request(Get, "https://api.example.com/invalid")
  let invalid_result = send_request(invalid_request)
  match invalid_result {
    Ok(_) => assert_true(false)
    Err(InvalidResponse) => assert_true(true)
    Err(_) => assert_true(false)
  }
  
  let notfound_request = create_request(Get, "https://notfound.example.com")
  let notfound_result = send_request(notfound_request)
  match notfound_result {
    Ok(_) => assert_true(false)
    Err(HostNotFound) => assert_true(true)
    Err(_) => assert_true(false)
  }
}

// Test 2: Timeout Handling
test "timeout handling" {
  // Timeout configuration
  type TimeoutConfig {
    connect_timeout : Int  // milliseconds
    read_timeout : Int     // milliseconds
    total_timeout : Int    // milliseconds
  }
  
  type OperationResult {
    Success(String)
    Timeout
    Error(String)
  }
  
  let create_timeout_config = fn(connect : Int, read : Int, total : Int) -> TimeoutConfig {
    TimeoutConfig {
      connect_timeout: connect,
      read_timeout: read,
      total_timeout: total
    }
  }
  
  // Simulate operation with timeout
  let execute_with_timeout = fn(operation : String, config : TimeoutConfig) -> OperationResult {
    // Simulate different operation durations
    let operation_duration = match operation {
      "fast" => 100
      "medium" => 1000
      "slow" => 5000
      "error" => -1 // Error case
      _ => 2000
    }
    
    if operation_duration < 0 {
      return Error("Operation failed")
    }
    
    if operation_duration > config.total_timeout {
      return Timeout
    }
    
    Success("Operation completed in " + operation_duration.to_string() + "ms")
  }
  
  // Test timeout configurations
  let fast_config = create_timeout_config(1000, 2000, 3000)
  let slow_config = create_timeout_config(500, 1000, 1500)
  
  // Test fast operation
  let fast_result = execute_with_timeout("fast", fast_config)
  match fast_result {
    Success(message) => assert_eq(message, "Operation completed in 100ms")
    _ => assert_true(false)
  }
  
  // Test slow operation with generous timeout
  let slow_result = execute_with_timeout("slow", fast_config)
  match slow_result {
    Success(message) => assert_eq(message, "Operation completed in 5000ms")
    _ => assert_true(false)
  }
  
  // Test slow operation with strict timeout
  let timeout_result = execute_with_timeout("slow", slow_config)
  match timeout_result {
    Timeout => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test error case
  let error_result = execute_with_timeout("error", fast_config)
  match error_result {
    Error(message) => assert_eq(message, "Operation failed")
    _ => assert_true(false)
  }
  
  // Test retry mechanism with timeout
  let retry_with_timeout = fn(operation : String, config : TimeoutConfig, max_retries : Int) -> OperationResult {
    let mut attempts = 0
    
    while attempts < max_retries {
      let result = execute_with_timeout(operation, config)
      
      match result {
        Success(_) => return result
        Timeout => {
          attempts = attempts + 1
          if attempts >= max_retries {
            return Timeout
          }
        }
        Error(_) => return result
      }
    }
    
    Timeout
  }
  
  // Test retry with fast operation (should succeed on first try)
  let retry_fast = retry_with_timeout("fast", slow_config, 3)
  match retry_fast {
    Success(_) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test retry with slow operation (should timeout after retries)
  let retry_slow = retry_with_timeout("slow", slow_config, 3)
  match retry_slow {
    Timeout => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 3: Connection Pooling
test "connection pooling" {
  // Connection pool
  type Connection {
    id : Int
    host : String
    port : Int
    is_active : Bool
    last_used : Int
  }
  
  type ConnectionPool {
    connections : Array[Connection]
    max_connections : Int
    next_id : Int
  }
  
  let create_connection_pool = fn(max_connections : Int) -> ConnectionPool {
    ConnectionPool {
      connections: [],
      max_connections: max_connections,
      next_id: 1
    }
  }
  
  let create_connection = fn(pool : ConnectionPool, host : String, port : Int) -> (Option[Connection], ConnectionPool) {
    if pool.connections.length() >= pool.max_connections {
      return (None, pool)
    }
    
    let connection = Connection {
      id: pool.next_id,
      host: host,
      port: port,
      is_active: true,
      last_used: 0 // Current time
    }
    
    let new_pool = ConnectionPool {
      connections: pool.connections.push(connection),
      max_connections: pool.max_connections,
      next_id: pool.next_id + 1
    }
    
    (Some(connection), new_pool)
  }
  
  let get_connection = fn(pool : ConnectionPool, host : String, port : Int) -> (Option[Connection], ConnectionPool) {
    // Try to find existing connection
    for i = 0; i < pool.connections.length(); i = i + 1 {
      let conn = pool.connections[i]
      if conn.host == host && conn.port == port && conn.is_active {
        // Update last used time
        let mut new_connections = pool.connections
        new_connections[i] = Connection {
          id: conn.id,
          host: conn.host,
          port: conn.port,
          is_active: conn.is_active,
          last_used: 1 // Updated time
        }
        
        let new_pool = ConnectionPool {
          connections: new_connections,
          max_connections: pool.max_connections,
          next_id: pool.next_id
        }
        
        return (Some(conn), new_pool)
      }
    }
    
    // Create new connection if available
    create_connection(pool, host, port)
  }
  
  let release_connection = fn(pool : ConnectionPool, connection_id : Int) -> ConnectionPool {
    let mut new_connections = []
    
    for conn in pool.connections {
      if conn.id == connection_id {
        new_connections.push(Connection {
          id: conn.id,
          host: conn.host,
          port: conn.port,
          is_active: false, // Mark as inactive
          last_used: conn.last_used
        })
      } else {
        new_connections.push(conn)
      }
    }
    
    ConnectionPool {
      connections: new_connections,
      max_connections: pool.max_connections,
      next_id: pool.next_id
    }
  }
  
  let cleanup_inactive = fn(pool : ConnectionPool) -> ConnectionPool {
    let mut active_connections = []
    
    for conn in pool.connections {
      if conn.is_active {
        active_connections.push(conn)
      }
    }
    
    ConnectionPool {
      connections: active_connections,
      max_connections: pool.max_connections,
      next_id: pool.next_id
    }
  }
  
  // Test connection pooling
  let pool = create_connection_pool(3)
  assert_eq(pool.max_connections, 3)
  assert_eq(pool.connections.length(), 0)
  
  // Get first connection (will create new)
  let (conn1, pool1) = get_connection(pool, "localhost", 8080)
  match conn1 {
    Some(c) => {
      assert_eq(c.host, "localhost")
      assert_eq(c.port, 8080)
      assert_true(c.is_active)
    }
    None => assert_true(false)
  }
  assert_eq(pool1.connections.length(), 1)
  
  // Get same connection (should reuse)
  let (conn2, pool2) = get_connection(pool1, "localhost", 8080)
  match conn2 {
    Some(c) => {
      assert_eq(c.id, conn1.unwrap().id) // Same connection
      assert_eq(c.last_used, 1) // Updated timestamp
    }
    None => assert_true(false)
  }
  assert_eq(pool2.connections.length(), 1)
  
  // Get different connection (will create new)
  let (conn3, pool3) = get_connection(pool2, "localhost", 9090)
  match conn3 {
    Some(c) => {
      assert_eq(c.host, "localhost")
      assert_eq(c.port, 9090)
      assert_not_eq(c.id, conn1.unwrap().id) // Different connection
    }
    None => assert_true(false)
  }
  assert_eq(pool3.connections.length(), 2)
  
  // Release connection
  let pool4 = release_connection(pool3, conn1.unwrap().id)
  assert_eq(pool4.connections.length(), 2)
  
  // Find released connection
  let mut found_inactive = false
  for conn in pool4.connections {
    if conn.id == conn1.unwrap().id {
      assert_false(conn.is_active)
      found_inactive = true
    }
  }
  assert_true(found_inactive)
  
  // Cleanup inactive connections
  let pool5 = cleanup_inactive(pool4)
  assert_eq(pool5.connections.length(), 1)
  
  // Test pool limit
  let (conn4, pool6) = get_connection(pool5, "host1", 8080)
  let (conn5, pool7) = get_connection(pool6, "host2", 8080)
  let (conn6, pool8) = get_connection(pool7, "host3", 8080)
  let (conn7, pool9) = get_connection(pool8, "host4", 8080) // Should fail
  
  match conn7 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Network Retry Strategies
test "network retry strategies" {
  // Retry strategy types
  type RetryStrategy {
    FixedDelay(Int)    // Fixed delay between retries in ms
    ExponentialBackoff(Int, Int)  // Initial delay and max delay
    LinearBackoff(Int, Int)       // Initial delay and increment
  }
  
  type RetryResult {
    Success(String)
    MaxRetriesExceeded
    PermanentError(String)
  }
  
  // Calculate delay based on strategy
  let calculate_delay = fn(strategy : RetryStrategy, attempt : Int) -> Int {
    match strategy {
      FixedDelay(delay) => delay
      ExponentialBackoff(initial, max) => {
        let delay = initial * (2 ^ attempt)
        if delay > max { max } else { delay }
      }
      LinearBackoff(initial, increment) => initial + (attempt * increment)
    }
  }
  
  // Simulate network operation with failure probability
  let network_operation = fn(attempt : Int, failure_until : Int) -> Result[String, String] {
    if attempt < failure_until {
      Err("Temporary failure")
    } else {
      Ok("Success on attempt " + attempt.to_string())
    }
  }
  
  // Execute with retry strategy
  let execute_with_retry = fn(strategy : RetryStrategy, max_retries : Int, failure_until : Int) -> RetryResult {
    let mut attempt = 0
    
    while attempt <= max_retries {
      let result = network_operation(attempt, failure_until)
      
      match result {
        Ok(message) => return Success(message)
        Err(error) => {
          if attempt == max_retries {
            return MaxRetriesExceeded
          }
          
          // Calculate delay for next attempt
          let delay = calculate_delay(strategy, attempt)
          // In a real implementation, we would wait here
          attempt = attempt + 1
        }
      }
    }
    
    MaxRetriesExceeded
  }
  
  // Test fixed delay strategy
  let fixed_strategy = FixedDelay(100)
  let fixed_success = execute_with_retry(fixed_strategy, 3, 2) // Succeeds on attempt 2
  match fixed_success {
    Success(message) => assert_eq(message, "Success on attempt 2")
    _ => assert_true(false)
  }
  
  let fixed_failure = execute_with_retry(fixed_strategy, 3, 5) // Never succeeds
  match fixed_failure {
    MaxRetriesExceeded => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test exponential backoff strategy
  let exp_strategy = ExponentialBackoff(100, 1000)
  let exp_success = execute_with_retry(exp_strategy, 3, 2) // Succeeds on attempt 2
  match exp_success {
    Success(message) => assert_eq(message, "Success on attempt 2")
    _ => assert_true(false)
  }
  
  // Test delay calculations
  assert_eq(calculate_delay(FixedDelay(500), 0), 500)
  assert_eq(calculate_delay(FixedDelay(500), 5), 500)
  
  assert_eq(calculate_delay(ExponentialBackoff(100, 800), 0), 100)
  assert_eq(calculate_delay(ExponentialBackoff(100, 800), 1), 200)
  assert_eq(calculate_delay(ExponentialBackoff(100, 800), 2), 400)
  assert_eq(calculate_delay(ExponentialBackoff(100, 800), 3), 800) // Capped at max
  assert_eq(calculate_delay(ExponentialBackoff(100, 800), 4), 800) // Still capped
  
  assert_eq(calculate_delay(LinearBackoff(100, 50), 0), 100)
  assert_eq(calculate_delay(LinearBackoff(100, 50), 1), 150)
  assert_eq(calculate_delay(LinearBackoff(100, 50), 2), 200)
  assert_eq(calculate_delay(LinearBackoff(100, 50), 3), 250)
}

// Test 5: Circuit Breaker Pattern
test "circuit breaker pattern" {
  // Circuit breaker states
  type CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  type CircuitBreaker {
    state : CircuitState
    failure_count : Int
    failure_threshold : Int
    success_threshold : Int
    timeout : Int
    last_failure_time : Int
  }
  
  let create_circuit_breaker = fn(failure_threshold : Int, success_threshold : Int, timeout : Int) -> CircuitBreaker {
    CircuitBreaker {
      state: Closed,
      failure_count: 0,
      failure_threshold: failure_threshold,
      success_threshold: success_threshold,
      timeout: timeout,
      last_failure_time: 0
    }
  }
  
  let should_allow_request = fn(breaker : CircuitBreaker, current_time : Int) -> Bool {
    match breaker.state {
      Closed => true
      Open => current_time - breaker.last_failure_time > breaker.timeout
      HalfOpen => true
    }
  }
  
  let record_success = fn(breaker : CircuitBreaker) -> CircuitBreaker {
    match breaker.state {
      Closed => {
        // Reset failure count on success
        CircuitBreaker {
          state: Closed,
          failure_count: 0,
          failure_threshold: breaker.failure_threshold,
          success_threshold: breaker.success_threshold,
          timeout: breaker.timeout,
          last_failure_time: breaker.last_failure_time
        }
      }
      HalfOpen => {
        // Check if we've reached success threshold
        if breaker.failure_count >= breaker.success_threshold {
          CircuitBreaker {
            state: Closed,
            failure_count: 0,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            timeout: breaker.timeout,
            last_failure_time: breaker.last_failure_time
          }
        } else {
          CircuitBreaker {
            state: HalfOpen,
            failure_count: breaker.failure_count + 1,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            timeout: breaker.timeout,
            last_failure_time: breaker.last_failure_time
          }
        }
      }
      Open => breaker // Shouldn't happen
    }
  }
  
  let record_failure = fn(breaker : CircuitBreaker, current_time : Int) -> CircuitBreaker {
    match breaker.state {
      Closed => {
        let new_failure_count = breaker.failure_count + 1
        if new_failure_count >= breaker.failure_threshold {
          // Trip the circuit
          CircuitBreaker {
            state: Open,
            failure_count: new_failure_count,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            timeout: breaker.timeout,
            last_failure_time: current_time
          }
        } else {
          CircuitBreaker {
            state: Closed,
            failure_count: new_failure_count,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            timeout: breaker.timeout,
            last_failure_time: breaker.last_failure_time
          }
        }
      }
      HalfOpen => {
        // Trip back to open on failure
        CircuitBreaker {
          state: Open,
          failure_count: breaker.failure_threshold,
          failure_threshold: breaker.failure_threshold,
          success_threshold: breaker.success_threshold,
          timeout: breaker.timeout,
          last_failure_time: current_time
        }
      }
      Open => breaker // Already open
    }
  }
  
  let attempt_reset = fn(breaker : CircuitBreaker, current_time : Int) -> CircuitBreaker {
    if breaker.state == Open && current_time - breaker.last_failure_time > breaker.timeout {
      CircuitBreaker {
        state: HalfOpen,
        failure_count: 0,
        failure_threshold: breaker.failure_threshold,
        success_threshold: breaker.success_threshold,
        timeout: breaker.timeout,
        last_failure_time: breaker.last_failure_time
      }
    } else {
      breaker
    }
  }
  
  // Test circuit breaker
  let breaker = create_circuit_breaker(3, 2, 1000)
  
  // Initially closed, should allow requests
  assert_true(should_allow_request(breaker, 0))
  
  // Record failures
  let breaker1 = record_failure(breaker, 100)
  assert_eq(breaker1.state, Closed)
  assert_eq(breaker1.failure_count, 1)
  
  let breaker2 = record_failure(breaker1, 200)
  assert_eq(breaker2.state, Closed)
  assert_eq(breaker2.failure_count, 2)
  
  let breaker3 = record_failure(breaker2, 300)
  assert_eq(breaker3.state, Open) // Should trip
  assert_eq(breaker3.failure_count, 3)
  assert_eq(breaker3.last_failure_time, 300)
  
  // Should not allow requests while open
  assert_false(should_allow_request(breaker3, 400))
  assert_false(should_allow_request(breaker3, 1200)) // Still within timeout
  
  // Should allow requests after timeout
  assert_true(should_allow_request(breaker3, 1400))
  
  // Attempt reset
  let breaker4 = attempt_reset(breaker3, 1400)
  assert_eq(breaker4.state, HalfOpen)
  
  // Record success in half-open state
  let breaker5 = record_success(breaker4)
  assert_eq(breaker5.state, HalfOpen)
  assert_eq(breaker5.failure_count, 1)
  
  let breaker6 = record_success(breaker5)
  assert_eq(breaker6.state, Closed) // Should close after success threshold
  assert_eq(breaker6.failure_count, 0)
  
  // Test failure in half-open state
  let breaker7 = create_circuit_breaker(3, 2, 1000)
  let breaker8 = record_failure(record_failure(record_failure(breaker7, 100), 200), 300) // Trip
  let breaker9 = attempt_reset(breaker8, 1400) // Half open
  
  let breaker10 = record_failure(breaker9, 1500) // Failure in half-open
  assert_eq(breaker10.state, Open) // Should trip back to open
}

// Test 6: Network Protocol Handling
test "network protocol handling" {
  // Protocol types
  type Protocol {
    Http
    Https
    WebSocket
    Tcp
    Udp
  }
  
  type Message {
    protocol : Protocol
    headers : Array[(String, String)]
    payload : Array[Byte]
  }
  
  let create_message = fn(protocol : Protocol, payload : Array[Byte]) -> Message {
    Message {
      protocol: protocol,
      headers: [],
      payload: payload
    }
  }
  
  let add_header = fn(message : Message, name : String, value : String) -> Message {
    Message {
      protocol: message.protocol,
      headers: message.headers.push((name, value)),
      payload: message.payload
    }
  }
  
  // Protocol-specific encoding
  let encode_message = fn(message : Message) -> Array[Byte] {
    match message.protocol {
      Http => {
        // Simple HTTP encoding
        let status_line = "HTTP/1.1 200 OK\r\n"
        let mut headers_text = ""
        
        for (name, value) in message.headers {
          headers_text = headers_text + name + ": " + value + "\r\n"
        }
        
        let headers_section = headers_text + "\r\n"
        let status_bytes = status_line.to_byte_array()
        let headers_bytes = headers_section.to_byte_array()
        
        let mut result = []
        for byte in status_bytes {
          result.push(byte)
        }
        for byte in headers_bytes {
          result.push(byte)
        }
        for byte in message.payload {
          result.push(byte)
        }
        
        result
      }
      WebSocket => {
        // Simple WebSocket frame format
        let mut result = []
        
        // FIN + opcode (1 byte)
        result.push(0x81) // FIN=1, opcode=1 (text)
        
        // Payload length
        let payload_len = message.payload.length()
        if payload_len < 126 {
          result.push(payload_len.to_byte())
        } else if payload_len < 65536 {
          result.push(126)
          result.push((payload_len >> 8) & 0xFF)
          result.push(payload_len & 0xFF)
        } else {
          result.push(127)
          // Extended payload length (simplified)
          for i = 56; i >= 0; i = i - 8 {
            result.push((payload_len >> i) & 0xFF)
          }
        }
        
        // Payload
        for byte in message.payload {
          result.push(byte)
        }
        
        result
      }
      Tcp => {
        // Raw TCP (just payload)
        message.payload
      }
      Udp => {
        // UDP with checksum (simplified)
        let mut result = []
        
        // Simple checksum
        let mut checksum = 0
        for byte in message.payload {
          checksum = checksum + byte
        }
        
        // Add checksum
        result.push((checksum >> 8) & 0xFF)
        result.push(checksum & 0xFF)
        
        // Add payload
        for byte in message.payload {
          result.push(byte)
        }
        
        result
      }
      Https => {
        // HTTPS would be encrypted, simplified here
        let mut encrypted = []
        for byte in message.payload {
          encrypted.push(byte ^ 0x42) // Simple XOR "encryption"
        }
        encrypted
      }
    }
  }
  
  // Test message creation
  let http_message = create_message(Http, "Hello World".to_byte_array())
    |> add_header("Content-Type", "text/plain")
    |> add_header("Content-Length", "11")
  
  assert_eq(http_message.protocol, Http)
  assert_eq(http_message.headers.length(), 2)
  assert_eq(http_message.headers[0], ("Content-Type", "text/plain"))
  assert_eq(http_message.headers[1], ("Content-Length", "11"))
  
  // Test HTTP encoding
  let http_encoded = encode_message(http_message)
  assert_true(http_encoded.length() > 11) // Should include headers
  
  // Test WebSocket encoding
  let ws_message = create_message(WebSocket, "WebSocket message".to_byte_array())
  let ws_encoded = encode_message(ws_message)
  
  // WebSocket frame should start with 0x81
  assert_eq(ws_encoded[0], 0x81)
  
  // Test TCP encoding
  let tcp_message = create_message(Tcp, "TCP data".to_byte_array())
  let tcp_encoded = encode_message(tcp_message)
  assert_eq(tcp_encoded, "TCP data".to_byte_array())
  
  // Test UDP encoding
  let udp_message = create_message(Udp, "UDP data".to_byte_array())
  let udp_encoded = encode_message(udp_message)
  
  // UDP should have 2-byte checksum prefix
  assert_eq(udp_encoded.length(), "UDP data".length() + 2)
  
  // Test HTTPS encoding
  let https_message = create_message(Https, "Secret data".to_byte_array())
  let https_encoded = encode_message(https_message)
  
  // HTTPS should be "encrypted" (different from original)
  assert_false(https_encoded == "Secret data".to_byte_array())
}