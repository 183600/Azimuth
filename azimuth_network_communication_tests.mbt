// Azimuth Network Communication Tests
// This file contains test cases for network communication functionality

// Test 1: HTTP Client Request Handling
test "http client request handling" {
  // Define HTTP request and response types
  type HTTPRequest = {
    method: String,
    url: String,
    headers: Array[(String, String)],
    body: Option<String>,
    timeout_ms: Int
  }
  
  type HTTPResponse = {
    status_code: Int,
    headers: Array[(String, String)],
    body: Option<String>,
    response_time_ms: Int
  }
  
  type HTTPError = {
    type: String,
    message: String,
    retry_after: Option<Int>
  }
  
  type HTTPResult = {
    success: Bool,
    response: Option<HTTPResponse>,
    error: Option<HTTPError>
  }
  
  // Simulate HTTP request execution
  let execute_http_request = fn(request: HTTPRequest, should_fail: Bool, response_delay_ms: Int) -> HTTPResult {
    if should_fail {
      {
        success: false,
        response: None,
        error: Some({
          type: "NetworkTimeout",
          message: "Request timed out after " + request.timeout_ms.to_string() + "ms",
          retry_after: Some(1000)
        })
      }
    } else {
      // Simulate successful response
      let response_headers = [
        ("Content-Type", "application/json"),
        ("Content-Length", "123"),
        ("Cache-Control", "no-cache")
      ]
      
      let response_body = Some("{\"status\": \"ok\", \"data\": {\"id\": 123, \"name\": \"test\"}}")
      
      {
        success: true,
        response: Some({
          status_code: 200,
          headers: response_headers,
          body: response_body,
          response_time_ms: response_delay_ms
        }),
        error: None
      }
    }
  }
  
  // Create test requests
  let get_request = {
    method: "GET",
    url: "https://api.example.com/users/123",
    headers: [("Accept", "application/json"), ("User-Agent", "Azimuth/1.0")],
    body: None,
    timeout_ms: 5000
  }
  
  let post_request = {
    method: "POST",
    url: "https://api.example.com/users",
    headers: [("Content-Type", "application/json"), ("User-Agent", "Azimuth/1.0")],
    body: Some("{\"name\": \"John Doe\", \"email\": \"john@example.com\"}"),
    timeout_ms: 10000
  }
  
  // Test successful GET request
  let get_result = execute_http_request(get_request, false, 250)
  assert_true(get_result.success)
  assert_eq(get_result.error, None)
  
  match get_result.response {
    Some(response) => {
      assert_eq(response.status_code, 200)
      assert_eq(response.headers.length(), 3)
      assert_eq(response.response_time_ms, 250)
      assert_eq(response.body, Some("{\"status\": \"ok\", \"data\": {\"id\": 123, \"name\": \"test\"}}"))
    }
    None => assert_true(false)
  }
  
  // Test successful POST request
  let post_result = execute_http_request(post_request, false, 500)
  assert_true(post_result.success)
  assert_eq(post_result.error, None)
  
  match post_result.response {
    Some(response) => {
      assert_eq(response.status_code, 200)
      assert_eq(response.headers.length(), 3)
      assert_eq(response.response_time_ms, 500)
    }
    None => assert_true(false)
  }
  
  // Test failed request
  let failed_result = execute_http_request(get_request, true, 0)
  assert_false(failed_result.success)
  assert_eq(failed_result.response, None)
  
  match failed_result.error {
    Some(error) => {
      assert_eq(error.type, "NetworkTimeout")
      assert_eq(error.message, "Request timed out after 5000ms")
      assert_eq(error.retry_after, Some(1000))
    }
    None => assert_true(false)
  }
}

// Test 2: WebSocket Connection Management
test "websocket connection management" {
  // Define WebSocket connection types
  enum WebSocketState {
    Connecting
    Connected
    Disconnecting
    Disconnected
    Error
  }
  
  type WebSocketMessage = {
    type: String,  // "text", "binary", "ping", "pong"
    data: String,
    timestamp: Int
  }
  
  type WebSocketConnection = {
    id: String,
    url: String,
    state: WebSocketState,
    last_ping: Int,
    last_pong: Int,
    messages_sent: Int,
    messages_received: Int,
    error_message: Option<String>
  }
  
  // Create a WebSocket connection
  let create_websocket_connection = fn(url: String) -> WebSocketConnection {
    {
      id: "ws_" + (1640995200).to_string(),
      url: url,
      state: WebSocketState::Connecting,
      last_ping: 0,
      last_pong: 0,
      messages_sent: 0,
      messages_received: 0,
      error_message: None
    }
  }
  
  // Connect to WebSocket
  let connect_websocket = fn(connection: WebSocketConnection, should_fail: Bool) -> WebSocketConnection {
    if should_fail {
      {
        id: connection.id,
        url: connection.url,
        state: WebSocketState::Error,
        last_ping: connection.last_ping,
        last_pong: connection.last_pong,
        messages_sent: connection.messages_sent,
        messages_received: connection.messages_received,
        error_message: Some("Connection failed: Unable to establish connection")
      }
    } else {
      {
        id: connection.id,
        url: connection.url,
        state: WebSocketState::Connected,
        last_ping: 1640995200,
        last_pong: 1640995200,
        messages_sent: connection.messages_sent,
        messages_received: connection.messages_received,
        error_message: None
      }
    }
  }
  
  // Send a message
  let send_message = fn(connection: WebSocketConnection, message: String) -> WebSocketConnection {
    if connection.state == WebSocketState::Connected {
      {
        id: connection.id,
        url: connection.url,
        state: connection.state,
        last_ping: connection.last_ping,
        last_pong: connection.last_pong,
        messages_sent: connection.messages_sent + 1,
        messages_received: connection.messages_received,
        error_message: None
      }
    } else {
      connection
    }
  }
  
  // Receive a message
  let receive_message = fn(connection: WebSocketConnection, message: String) -> WebSocketConnection {
    if connection.state == WebSocketState::Connected {
      {
        id: connection.id,
        url: connection.url,
        state: connection.state,
        last_ping: connection.last_ping,
        last_pong: connection.last_pong,
        messages_sent: connection.messages_sent,
        messages_received: connection.messages_received + 1,
        error_message: None
      }
    } else {
      connection
    }
  }
  
  // Disconnect from WebSocket
  let disconnect_websocket = fn(connection: WebSocketConnection) -> WebSocketConnection {
    {
      id: connection.id,
      url: connection.url,
      state: WebSocketState::Disconnected,
      last_ping: connection.last_ping,
      last_pong: connection.last_pong,
      messages_sent: connection.messages_sent,
      messages_received: connection.messages_received,
      error_message: None
    }
  }
  
  // Test WebSocket connection lifecycle
  let ws1 = create_websocket_connection("wss://example.com/ws")
  assert_eq(ws1.state, WebSocketState::Connecting)
  assert_eq(ws1.messages_sent, 0)
  assert_eq(ws1.messages_received, 0)
  
  // Connect successfully
  let ws2 = connect_websocket(ws1, false)
  assert_eq(ws2.state, WebSocketState::Connected)
  assert_eq(ws2.last_ping, 1640995200)
  assert_eq(ws2.last_pong, 1640995200)
  
  // Send messages
  let ws3 = send_message(ws2, "Hello, WebSocket!")
  assert_eq(ws3.messages_sent, 1)
  
  let ws4 = send_message(ws3, "Another message")
  assert_eq(ws4.messages_sent, 2)
  
  // Receive messages
  let ws5 = receive_message(ws4, "Response 1")
  assert_eq(ws5.messages_received, 1)
  
  let ws6 = receive_message(ws5, "Response 2")
  assert_eq(ws6.messages_received, 2)
  
  // Disconnect
  let ws7 = disconnect_websocket(ws6)
  assert_eq(ws7.state, WebSocketState::Disconnected)
  assert_eq(ws7.messages_sent, 2)
  assert_eq(ws7.messages_received, 2)
  
  // Test connection failure
  let ws8 = create_websocket_connection("wss://invalid.example.com/ws")
  let ws9 = connect_websocket(ws8, true)
  assert_eq(ws9.state, WebSocketState::Error)
  assert_eq(ws9.error_message, Some("Connection failed: Unable to establish connection"))
  
  // Try to send message on failed connection
  let ws10 = send_message(ws9, "This should fail")
  assert_eq(ws10.messages_sent, 0)  // Unchanged
}

// Test 3: Network Retry Strategy
test "network retry strategy" {
  // Define retry strategy types
  type RetryStrategy = {
    max_attempts: Int,
    initial_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float,
    retryable_errors: Array[String]
  }
  
  type NetworkOperation = {
    id: String,
    attempt: Int,
    success: Bool,
    error_type: Option<String>
  }
  
  // Execute operation with retry strategy
  let execute_with_retry = fn(
    operation: () -> NetworkOperation,
    strategy: RetryStrategy
  ) -> NetworkOperation {
    let mut current_attempt = 1
    let mut current_delay = strategy.initial_delay_ms
    let mut last_result = operation()
    
    while current_attempt < strategy.max_attempts and not(last_result.success) {
      match last_result.error_type {
        Some(error_type) => {
          if strategy.retryable_errors.contains(error_type) {
            // Simulate delay (in real implementation, this would be an actual delay)
            current_attempt = current_attempt + 1
            
            // Calculate next delay with exponential backoff
            current_delay = (current_delay.to_float() * strategy.backoff_multiplier).to_int()
            if current_delay > strategy.max_delay_ms {
              current_delay = strategy.max_delay_ms
            }
            
            // Retry the operation
            last_result = operation()
            last_result.attempt = current_attempt
          } else {
            // Don't retry this type of error
            break
          }
        }
        None => {
          // No error, but success is false (shouldn't happen)
          break
        }
      }
    }
    
    last_result
  }
  
  // Create test operations
  let attempt_counter = { mut count: 0 }
  let test_operation = fn() -> NetworkOperation {
    attempt_counter.count = attempt_counter.count + 1
    
    if attempt_counter.count < 3 {
      {
        id: "op_123",
        attempt: attempt_counter.count,
        success: false,
        error_type: Some("NetworkTimeout")
      }
    } else {
      {
        id: "op_123",
        attempt: attempt_counter.count,
        success: true,
        error_type: None
      }
    }
  }
  
  // Define retry strategy
  let retry_strategy = {
    max_attempts: 5,
    initial_delay_ms: 100,
    max_delay_ms: 1000,
    backoff_multiplier: 2.0,
    retryable_errors: ["NetworkTimeout", "ConnectionRefused", "ServiceUnavailable"]
  }
  
  // Execute operation with retry
  attempt_counter.count = 0
  let result1 = execute_with_retry(test_operation, retry_strategy)
  
  assert_true(result1.success)
  assert_eq(result1.attempt, 3)
  assert_eq(result1.error_type, None)
  
  // Test with non-retryable error
  let non_retryable_operation = fn() -> NetworkOperation {
    attempt_counter.count = attempt_counter.count + 1
    
    {
      id: "op_456",
      attempt: attempt_counter.count,
      success: false,
      error_type: Some("AuthenticationError")
    }
  }
  
  attempt_counter.count = 0
  let result2 = execute_with_retry(non_retryable_operation, retry_strategy)
  
  assert_false(result2.success)
  assert_eq(result2.attempt, 1)  // Only attempted once
  assert_eq(result2.error_type, Some("AuthenticationError"))
  
  // Test with max attempts reached
  let always_failing_operation = fn() -> NetworkOperation {
    attempt_counter.count = attempt_counter.count + 1
    
    {
      id: "op_789",
      attempt: attempt_counter.count,
      success: false,
      error_type: Some("NetworkTimeout")
    }
  }
  
  attempt_counter.count = 0
  let result3 = execute_with_retry(always_failing_operation, retry_strategy)
  
  assert_false(result3.success)
  assert_eq(result3.attempt, 5)  // Max attempts reached
  assert_eq(result3.error_type, Some("NetworkTimeout"))
}

// Test 4: Connection Pool Management
test "connection pool management" {
  // Define connection types
  type Connection = {
    id: String,
    host: String,
    port: Int,
    created_time: Int,
    last_used_time: Int,
    in_use: Bool,
    is_healthy: Bool
  }
  
  type ConnectionPool = {
    max_connections: Int,
    min_connections: Int,
    active_connections: Int,
    idle_connections: Int,
    connections: Array[Connection]
  }
  
  // Create a connection pool
  let create_connection_pool = fn(max_connections: Int, min_connections: Int) -> ConnectionPool {
    let mut initial_connections = []
    
    // Create minimum connections
    for i in 0..min_connections {
      let connection_id = "conn_" + i.to_string()
      initial_connections = initial_connections.push({
        id: connection_id,
        host: "db.example.com",
        port: 5432,
        created_time: 1640995200,
        last_used_time: 1640995200,
        in_use: false,
        is_healthy: true
      })
    }
    
    {
      max_connections: max_connections,
      min_connections: min_connections,
      active_connections: min_connections,
      idle_connections: min_connections,
      connections: initial_connections
    }
  }
  
  // Get a connection from the pool
  let get_connection = fn(pool: ConnectionPool) -> (ConnectionPool, Option[String]) {
    // Find an idle, healthy connection
    let mut found_idle = false
    let mut updated_connections = []
    let mut connection_id = None
    
    for connection in pool.connections {
      if connection.is_healthy and not(connection.in_use) and not(found_idle) {
        found_idle = true
        connection_id = Some(connection.id)
        
        updated_connections = updated_connections.push({
          id: connection.id,
          host: connection.host,
          port: connection.port,
          created_time: connection.created_time,
          last_used_time: 1640995300,
          in_use: true,
          is_healthy: connection.is_healthy
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    if found_idle {
      let updated_pool = {
        max_connections: pool.max_connections,
        min_connections: pool.min_connections,
        active_connections: pool.active_connections,
        idle_connections: pool.idle_connections - 1,
        connections: updated_connections
      }
      
      (updated_pool, connection_id)
    } else if pool.active_connections < pool.max_connections {
      // Create a new connection
      let new_connection_id = "conn_" + pool.active_connections.to_string()
      let new_connection = {
        id: new_connection_id,
        host: "db.example.com",
        port: 5432,
        created_time: 1640995300,
        last_used_time: 1640995300,
        in_use: true,
        is_healthy: true
      }
      
      let updated_pool = {
        max_connections: pool.max_connections,
        min_connections: pool.min_connections,
        active_connections: pool.active_connections + 1,
        idle_connections: pool.idle_connections,
        connections: pool.connections.push(new_connection)
      }
      
      (updated_pool, Some(new_connection_id))
    } else {
      // No available connections
      (pool, None)
    }
  }
  
  // Release a connection back to the pool
  let release_connection = fn(pool: ConnectionPool, connection_id: String) -> ConnectionPool {
    let mut updated_connections = []
    let mut found = false
    
    for connection in pool.connections {
      if connection.id == connection_id and connection.in_use {
        found = true
        
        updated_connections = updated_connections.push({
          id: connection.id,
          host: connection.host,
          port: connection.port,
          created_time: connection.created_time,
          last_used_time: 1640995400,
          in_use: false,
          is_healthy: connection.is_healthy
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    if found {
      {
        max_connections: pool.max_connections,
        min_connections: pool.min_connections,
        active_connections: pool.active_connections,
        idle_connections: pool.idle_connections + 1,
        connections: updated_connections
      }
    } else {
      pool
    }
  }
  
  // Mark a connection as unhealthy
  let mark_connection_unhealthy = fn(pool: ConnectionPool, connection_id: String) -> ConnectionPool {
    let updated_connections = []
    
    for connection in pool.connections {
      if connection.id == connection_id {
        updated_connections = updated_connections.push({
          id: connection.id,
          host: connection.host,
          port: connection.port,
          created_time: connection.created_time,
          last_used_time: connection.last_used_time,
          in_use: connection.in_use,
          is_healthy: false
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    {
      max_connections: pool.max_connections,
      min_connections: pool.min_connections,
      active_connections: pool.active_connections,
      idle_connections: pool.idle_connections,
      connections: updated_connections
    }
  }
  
  // Test connection pool operations
  let pool1 = create_connection_pool(5, 2)
  assert_eq(pool1.max_connections, 5)
  assert_eq(pool1.min_connections, 2)
  assert_eq(pool1.active_connections, 2)
  assert_eq(pool1.idle_connections, 2)
  assert_eq(pool1.connections.length(), 2)
  
  // Get connections
  let (pool2, conn1_id) = get_connection(pool1)
  assert_eq(pool2.active_connections, 2)
  assert_eq(pool2.idle_connections, 1)
  assert_eq(conn1_id, Some("conn_0"))
  
  let (pool3, conn2_id) = get_connection(pool2)
  assert_eq(pool3.active_connections, 2)
  assert_eq(pool3.idle_connections, 0)
  assert_eq(conn2_id, Some("conn_1"))
  
  // Get more connections (will create new ones)
  let (pool4, conn3_id) = get_connection(pool3)
  assert_eq(pool4.active_connections, 3)
  assert_eq(pool4.idle_connections, 0)
  assert_eq(conn3_id, Some("conn_2"))
  
  let (pool5, conn4_id) = get_connection(pool4)
  assert_eq(pool5.active_connections, 4)
  assert_eq(pool5.idle_connections, 0)
  assert_eq(conn4_id, Some("conn_3"))
  
  let (pool6, conn5_id) = get_connection(pool5)
  assert_eq(pool6.active_connections, 5)
  assert_eq(pool6.idle_connections, 0)
  assert_eq(conn5_id, Some("conn_4"))
  
  // Try to get another connection (should fail)
  let (pool7, conn6_id) = get_connection(pool6)
  assert_eq(pool7.active_connections, 5)  // Unchanged
  assert_eq(pool7.idle_connections, 0)  // Unchanged
  assert_eq(conn6_id, None)
  
  // Release connections
  let pool8 = release_connection(pool7, "conn_0")
  assert_eq(pool8.active_connections, 5)
  assert_eq(pool8.idle_connections, 1)
  
  let pool9 = release_connection(pool8, "conn_1")
  assert_eq(pool9.active_connections, 5)
  assert_eq(pool9.idle_connections, 2)
  
  // Now we can get connections again
  let (pool10, conn7_id) = get_connection(pool9)
  assert_eq(pool10.active_connections, 5)
  assert_eq(pool10.idle_connections, 1)
  assert_eq(conn7_id, Some("conn_0"))
  
  // Mark a connection as unhealthy
  let pool11 = mark_connection_unhealthy(pool10, "conn_2")
  assert_false(pool11.connections[2].is_healthy)
  
  // Release the unhealthy connection
  let pool12 = release_connection(pool11, "conn_2")
  assert_eq(pool12.active_connections, 5)
  assert_eq(pool12.idle_connections, 2)
  
  // Get connections (should skip the unhealthy one)
  let (pool13, conn8_id) = get_connection(pool12)
  assert_eq(pool13.active_connections, 5)
  assert_eq(pool13.idle_connections, 1)
  assert_eq(conn8_id, Some("conn_0"))
}

// Test 5: Network Load Balancing
test "network load balancing" {
  // Define server and load balancer types
  type Server = {
    id: String,
    host: String,
    port: Int,
    weight: Int,
    current_connections: Int,
    max_connections: Int,
    is_healthy: Bool,
    response_time_ms: Int
  }
  
  enum LoadBalancingStrategy {
    RoundRobin
    LeastConnections
    WeightedRoundRobin
    ResponseTime
  }
  
  type LoadBalancer = {
    servers: Array[Server],
    strategy: LoadBalancingStrategy,
    current_index: Int
  }
  
  // Create a load balancer
  let create_load_balancer = fn(servers: Array[Server], strategy: LoadBalancingStrategy) -> LoadBalancer {
    {
      servers: servers,
      strategy: strategy,
      current_index: 0
    }
  }
  
  // Select server using round-robin strategy
  let select_server_round_robin = fn(lb: LoadBalancer) -> (LoadBalancer, Option[Server]) {
    if lb.servers.length() == 0 {
      (lb, None)
    } else {
      let server = lb.servers[lb.current_index]
      let next_index = (lb.current_index + 1) % lb.servers.length()
      
      ({
        servers: lb.servers,
        strategy: lb.strategy,
        current_index: next_index
      }, Some(server))
    }
  }
  
  // Select server using least connections strategy
  let select_server_least_connections = fn(lb: LoadBalancer) -> (LoadBalancer, Option[Server]) {
    if lb.servers.length() == 0 {
      (lb, None)
    } else {
      let mut min_connections = lb.servers[0].current_connections
      let mut selected_server = lb.servers[0]
      
      for server in lb.servers {
        if server.is_healthy and server.current_connections < min_connections {
          min_connections = server.current_connections
          selected_server = server
        }
      }
      
      (lb, Some(selected_server))
    }
  }
  
  // Select server using weighted round-robin strategy
  let select_server_weighted_round_robin = fn(lb: LoadBalancer, current_weight: Int) -> (LoadBalancer, Option[Server], Int) {
    if lb.servers.length() == 0 {
      (lb, None, current_weight)
    } else {
      let mut selected_server = None
      let mut new_weight = current_weight
      
      for server in lb.servers {
        if server.is_healthy {
          new_weight = new_weight + server.weight
          if new_weight >= 100 {
            selected_server = Some(server)
            new_weight = new_weight - 100
            break
          }
        }
      }
      
      if selected_server.is_none() and lb.servers.length() > 0 {
        // Fallback to first healthy server
        for server in lb.servers {
          if server.is_healthy {
            selected_server = Some(server)
            break
          }
        }
      }
      
      (lb, selected_server, new_weight)
    }
  }
  
  // Select server using response time strategy
  let select_server_response_time = fn(lb: LoadBalancer) -> (LoadBalancer, Option[Server]) {
    if lb.servers.length() == 0 {
      (lb, None)
    } else {
      let mut min_response_time = 999999
      let mut selected_server = None
      
      for server in lb.servers {
        if server.is_healthy and server.response_time_ms < min_response_time {
          min_response_time = server.response_time_ms
          selected_server = Some(server)
        }
      }
      
      (lb, selected_server)
    }
  }
  
  // Update server connection count
  let update_server_connections = fn(lb: LoadBalancer, server_id: String, delta: Int) -> LoadBalancer {
    let updated_servers = []
    
    for server in lb.servers {
      if server.id == server_id {
        updated_servers = updated_servers.push({
          id: server.id,
          host: server.host,
          port: server.port,
          weight: server.weight,
          current_connections: server.current_connections + delta,
          max_connections: server.max_connections,
          is_healthy: server.is_healthy,
          response_time_ms: server.response_time_ms
        })
      } else {
        updated_servers = updated_servers.push(server)
      }
    }
    
    {
      servers: updated_servers,
      strategy: lb.strategy,
      current_index: lb.current_index
    }
  }
  
  // Create test servers
  let servers = [
    {
      id: "server1",
      host: "server1.example.com",
      port: 8080,
      weight: 50,
      current_connections: 0,
      max_connections: 100,
      is_healthy: true,
      response_time_ms: 100
    },
    {
      id: "server2",
      host: "server2.example.com",
      port: 8080,
      weight: 30,
      current_connections: 0,
      max_connections: 100,
      is_healthy: true,
      response_time_ms: 150
    },
    {
      id: "server3",
      host: "server3.example.com",
      port: 8080,
      weight: 20,
      current_connections: 0,
      max_connections: 100,
      is_healthy: true,
      response_time_ms: 200
    }
  ]
  
  // Test round-robin strategy
  let lb_rr = create_load_balancer(servers, LoadBalancingStrategy::RoundRobin)
  
  let (lb_rr1, server1) = select_server_round_robin(lb_rr)
  assert_eq(server1.unwrap().id, "server1")
  assert_eq(lb_rr1.current_index, 1)
  
  let (lb_rr2, server2) = select_server_round_robin(lb_rr1)
  assert_eq(server2.unwrap().id, "server2")
  assert_eq(lb_rr2.current_index, 2)
  
  let (lb_rr3, server3) = select_server_round_robin(lb_rr2)
  assert_eq(server3.unwrap().id, "server3")
  assert_eq(lb_rr3.current_index, 0)
  
  let (lb_rr4, server4) = select_server_round_robin(lb_rr3)
  assert_eq(server4.unwrap().id, "server1")  // Back to first server
  assert_eq(lb_rr4.current_index, 1)
  
  // Test least connections strategy
  let lb_lc = create_load_balancer(servers, LoadBalancingStrategy::LeastConnections)
  
  // Simulate some connections
  let lb_lc1 = update_server_connections(lb_lc, "server1", 10)
  let lb_lc2 = update_server_connections(lb_lc1, "server2", 5)
  
  let (_, lc_server) = select_server_least_connections(lb_lc2)
  assert_eq(lc_server.unwrap().id, "server3")  // Has 0 connections
  
  // Test weighted round-robin strategy
  let lb_wrr = create_load_balancer(servers, LoadBalancingStrategy::WeightedRoundRobin)
  
  let (lb_wrr1, wrr_server1, weight1) = select_server_weighted_round_robin(lb_wrr, 0)
  assert_eq(wrr_server1.unwrap().id, "server1")  // Weight 50
  assert_eq(weight1, 0)
  
  let (lb_wrr2, wrr_server2, weight2) = select_server_weighted_round_robin(lb_wrr1, 0)
  assert_eq(wrr_server2.unwrap().id, "server1")  // Still server1 due to weight
  assert_eq(weight2, 0)
  
  // Test response time strategy
  let lb_rt = create_load_balancer(servers, LoadBalancingStrategy::ResponseTime)
  
  let (_, rt_server) = select_server_response_time(lb_rt)
  assert_eq(rt_server.unwrap().id, "server1")  // Lowest response time (100ms)
}