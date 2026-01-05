// Azimuth Network Communication Test Suite
// This file contains test cases for network communication in Azimuth telemetry system

// Test 1: HTTP Client Request/Response
test "HTTP client request/response handling" {
  // Define HTTP request
  type HttpRequest = {
    method: String,
    url: String,
    headers: Array[(String, String)],
    body: Option[String]
  }
  
  // Define HTTP response
  type HttpResponse = {
    status_code: Int,
    headers: Array[(String, String)],
    body: String
  }
  
  // Define HTTP client
  type HttpClient = {
    base_url: String,
    default_headers: Array[(String, String)],
    timeout_ms: Int
  }
  
  // Create HTTP client
  let create_client = fn(base_url: String, timeout_ms: Int) {
    {
      base_url,
      default_headers: [
        ("User-Agent", "Azimuth-Telemetry/1.0.0"),
        ("Accept", "application/json")
      ],
      timeout_ms
    }
  }
  
  // Send HTTP request (simulated)
  let send_request = fn(client: HttpClient, request: HttpRequest) {
    // Simulate network request
    let full_url = if request.url.starts_with("http") {
      request.url
    } else {
      client.base_url + request.url
    }
    
    // Merge default headers with request headers
    let mut all_headers = client.default_headers
    for header in request.headers {
      all_headers = all_headers.push(header)
    }
    
    // Simulate response based on request
    let (status_code, response_body) = match request.method {
      "GET" => {
        if full_url.contains("/api/spans") {
          (200, "{\"spans\":[{\"id\":\"span-1\",\"trace_id\":\"trace-1\"}]}")
        } else if full_url.contains("/api/metrics") {
          (200, "{\"metrics\":[{\"name\":\"http_requests\",\"value\":123}]}")
        } else {
          (404, "{\"error\":\"Not found\"}")
        }
      }
      "POST" => {
        if full_url.contains("/api/spans") {
          (201, "{\"id\":\"span-2\",\"trace_id\":\"trace-2\"}")
        } else if full_url.contains("/api/metrics") {
          (201, "{\"status\":\"created\"}")
        } else {
          (400, "{\"error\":\"Bad request\"}")
        }
      }
      "PUT" => {
        if full_url.contains("/api/spans/") {
          (200, "{\"id\":\"span-1\",\"trace_id\":\"trace-1-updated\"}")
        } else {
          (404, "{\"error\":\"Not found\"}")
        }
      }
      "DELETE" => {
        if full_url.contains("/api/spans/") {
          (204, "")
        } else {
          (404, "{\"error\":\"Not found\"}")
        }
      }
      _ => (405, "{\"error\":\"Method not allowed\"}")
    }
    
    // Create response headers
    let response_headers = [
      ("Content-Type", "application/json"),
      ("Content-Length", response_body.length().to_string()),
      ("X-Request-ID", "req-" + request.method.to_string() + "-" + request.url.length().to_string())
    ]
    
    {
      status_code,
      headers: response_headers,
      body: response_body
    }
  }
  
  // Create HTTP client
  let client = create_client("https://api.telemetry.example.com", 5000)
  
  // Test GET request
  let get_request = {
    method: "GET",
    url: "/api/spans",
    headers: [("Authorization", "Bearer token123")],
    body: None
  }
  
  let get_response = send_request(client, get_request)
  assert_eq(get_response.status_code, 200)
  assert_eq(get_response.body, "{\"spans\":[{\"id\":\"span-1\",\"trace_id\":\"trace-1\"}]}")
  
  // Test POST request
  let post_request = {
    method: "POST",
    url: "/api/spans",
    headers: [("Content-Type", "application/json")],
    body: Some("{\"trace_id\":\"trace-2\",\"operation\":\"test\"}")
  }
  
  let post_response = send_request(client, post_request)
  assert_eq(post_response.status_code, 201)
  assert_eq(post_response.body, "{\"id\":\"span-2\",\"trace_id\":\"trace-2\"}")
  
  // Test PUT request
  let put_request = {
    method: "PUT",
    url: "/api/spans/span-1",
    headers: [("Content-Type", "application/json")],
    body: Some("{\"trace_id\":\"trace-1-updated\"}")
  }
  
  let put_response = send_request(client, put_request)
  assert_eq(put_response.status_code, 200)
  assert_eq(put_response.body, "{\"id\":\"span-1\",\"trace_id\":\"trace-1-updated\"}")
  
  // Test DELETE request
  let delete_request = {
    method: "DELETE",
    url: "/api/spans/span-1",
    headers: [],
    body: None
  }
  
  let delete_response = send_request(client, delete_request)
  assert_eq(delete_response.status_code, 204)
  assert_eq(delete_response.body, "")
  
  // Test 404 error
  let not_found_request = {
    method: "GET",
    url: "/api/nonexistent",
    headers: [],
    body: None
  }
  
  let not_found_response = send_request(client, not_found_request)
  assert_eq(not_found_response.status_code, 404)
  assert_eq(not_found_response.body, "{\"error\":\"Not found\"}")
  
  // Test response headers
  assert_true(get_response.headers.contains(("Content-Type", "application/json")))
  assert_true(get_response.headers.contains(("Content-Length", "44")))
  assert_true(get_response.headers.contains(("X-Request-ID", "req-3-9")))
}

// Test 2: WebSocket Communication
test "WebSocket communication for real-time telemetry" {
  // Define WebSocket message
  type WebSocketMessage = {
    type: String,  // "text", "binary"
    data: String
  }
  
  // Define WebSocket connection
  type WebSocketConnection = {
    url: String,
    state: String,  // "connecting", "open", "closing", "closed"
    messages_sent: Array[WebSocketMessage],
    messages_received: Array[WebSocketMessage]
  }
  
  // Create WebSocket connection
  let create_connection = fn(url: String) {
    {
      url,
      state: "connecting",
      messages_sent: [],
      messages_received: []
    }
  }
  
  // Connect WebSocket (simulated)
  let connect = fn(connection: WebSocketConnection) {
    // Simulate connection process
    {
      url: connection.url,
      state: "open",
      messages_sent: connection.messages_sent,
      messages_received: connection.messages_received
    }
  }
  
  // Send message (simulated)
  let send_message = fn(connection: WebSocketConnection, message: WebSocketMessage) {
    if connection.state == "open" {
      {
        url: connection.url,
        state: connection.state,
        messages_sent: connection.messages_sent.push(message),
        messages_received: connection.messages_received
      }
    } else {
      connection
    }
  }
  
  // Receive message (simulated)
  let receive_message = fn(connection: WebSocketConnection, message: WebSocketMessage) {
    if connection.state == "open" {
      {
        url: connection.url,
        state: connection.state,
        messages_sent: connection.messages_sent,
        messages_received: connection.messages_received.push(message)
      }
    } else {
      connection
    }
  }
  
  // Close connection (simulated)
  let close_connection = fn(connection: WebSocketConnection) {
    {
      url: connection.url,
      state: "closed",
      messages_sent: connection.messages_sent,
      messages_received: connection.messages_received
    }
  }
  
  // Create WebSocket connection
  let mut connection = create_connection("wss://telemetry.example.com/ws")
  
  // Test initial state
  assert_eq(connection.state, "connecting")
  assert_eq(connection.messages_sent.length(), 0)
  assert_eq(connection.messages_received.length(), 0)
  
  // Connect
  connection = connect(connection)
  assert_eq(connection.state, "open")
  
  // Send messages
  let subscribe_message = {
    type: "text",
    data: "{\"action\":\"subscribe\",\"channel\":\"spans\"}"
  }
  
  connection = send_message(connection, subscribe_message)
  assert_eq(connection.messages_sent.length(), 1)
  assert_eq(connection.messages_sent[0].type, "text")
  assert_eq(connection.messages_sent[0].data, "{\"action\":\"subscribe\",\"channel\":\"spans\"}")
  
  // Receive messages
  let span_message = {
    type: "text",
    data: "{\"type\":\"span\",\"id\":\"span-123\",\"trace_id\":\"trace-456\"}"
  }
  
  connection = receive_message(connection, span_message)
  assert_eq(connection.messages_received.length(), 1)
  assert_eq(connection.messages_received[0].type, "text")
  assert_eq(connection.messages_received[0].data, "{\"type\":\"span\",\"id\":\"span-123\",\"trace_id\":\"trace-456\"}")
  
  // Send binary message
  let binary_message = {
    type: "binary",
    data: "binary-data-here"
  }
  
  connection = send_message(connection, binary_message)
  assert_eq(connection.messages_sent.length(), 2)
  assert_eq(connection.messages_sent[1].type, "binary")
  assert_eq(connection.messages_sent[1].data, "binary-data-here")
  
  // Receive more messages
  let metric_message = {
    type: "text",
    data: "{\"type\":\"metric\",\"name\":\"http_requests\",\"value\":42}"
  }
  
  connection = receive_message(connection, metric_message)
  assert_eq(connection.messages_received.length(), 2)
  assert_eq(connection.messages_received[1].type, "text")
  assert_eq(connection.messages_received[1].data, "{\"type\":\"metric\",\"name\":\"http_requests\",\"value\":42}")
  
  // Close connection
  connection = close_connection(connection)
  assert_eq(connection.state, "closed")
  
  // Test that messages can't be sent after closing
  let closed_connection = send_message(connection, {
    type: "text",
    data: "should not be sent"
  })
  assert_eq(closed_connection.messages_sent.length(), 2)  // Unchanged
}

// Test 3: gRPC Communication
test "gRPC communication for telemetry services" {
  // Define gRPC method
  type GrpcMethod = {
    service: String,
    method: String,
    request_type: String,
    response_type: String
  }
  
  // Define gRPC message
  type GrpcMessage = {
    data: String,
    metadata: Array[(String, String)]
  }
  
  // Define gRPC call
  type GrpcCall = {
    method: GrpcMethod,
    request: GrpcMessage,
    timeout_ms: Int
  }
  
  // Define gRPC response
  type GrpcResponse = {
    status_code: Int,  // gRPC status code
    message: GrpcMessage,
    trailers: Array[(String, String)]
  }
  
  // Define gRPC client
  type GrpcClient = {
    host: String,
    port: Int,
    default_timeout_ms: Int
  }
  
  // Create gRPC client
  let create_grpc_client = fn(host: String, port: Int) {
    {
      host,
      port,
      default_timeout_ms: 5000
    }
  }
  
  // Make gRPC call (simulated)
  let make_call = fn(client: GrpcClient, call: GrpcCall) {
    // Simulate gRPC call based on method
    let (status_code, response_data) = match call.method.service {
      "TelemetryService" => {
        match call.method.method {
          "GetSpan" => {
            if call.request.data.contains("span-123") {
              (0, "{\"id\":\"span-123\",\"trace_id\":\"trace-456\",\"operation\":\"test\"}")
            } else {
              (5, "{\"error\":\"Span not found\"}")  // NOT_FOUND
            }
          }
          "ListSpans" => {
            (0, "{\"spans\":[{\"id\":\"span-123\",\"trace_id\":\"trace-456\"},{\"id\":\"span-789\",\"trace_id\":\"trace-456\"}]}")
          }
          "CreateSpan" => {
            (0, "{\"id\":\"span-new\",\"trace_id\":\"trace-new\"}")
          }
          _ => (12, "{\"error\":\"Method not found\"}")  // UNIMPLEMENTED
        }
      }
      "MetricsService" => {
        match call.method.method {
          "GetMetric" => {
            (0, "{\"name\":\"http_requests\",\"value\":123,\"unit\":\"count\"}")
          }
          "ListMetrics" => {
            (0, "{\"metrics\":[{\"name\":\"http_requests\",\"value\":123},{\"name\":\"response_time\",\"value\":45.6}]}")
          }
          _ => (12, "{\"error\":\"Method not found\"}")  // UNIMPLEMENTED
        }
      }
      _ => (12, "{\"error\":\"Service not found\"}")  // UNIMPLEMENTED
    }
    
    // Create response metadata
    let response_metadata = [
      ("content-type", "application/grpc"),
      ("grpc-status", status_code.to_string()),
      ("grpc-message", if status_code == 0 { "OK" } else { "Error" })
    ]
    
    // Create response trailers
    let trailers = [
      ("grpc-status", status_code.to_string()),
      ("grpc-message", if status_code == 0 { "OK" } else { "Error" })
    ]
    
    {
      status_code,
      message: {
        data: response_data,
        metadata: response_metadata
      },
      trailers
    }
  }
  
  // Create gRPC client
  let client = create_grpc_client("telemetry.example.com", 50051)
  
  // Test GetSpan call
  let get_span_method = {
    service: "TelemetryService",
    method: "GetSpan",
    request_type: "GetSpanRequest",
    response_type: "GetSpanResponse"
  }
  
  let get_span_call = {
    method: get_span_method,
    request: {
      data: "{\"span_id\":\"span-123\"}",
      metadata: [("authorization", "Bearer token123")]
    },
    timeout_ms: 3000
  }
  
  let get_span_response = make_call(client, get_span_call)
  assert_eq(get_span_response.status_code, 0)  // OK
  assert_eq(get_span_response.message.data, "{\"id\":\"span-123\",\"trace_id\":\"trace-456\",\"operation\":\"test\"}")
  
  // Test ListSpans call
  let list_spans_method = {
    service: "TelemetryService",
    method: "ListSpans",
    request_type: "ListSpansRequest",
    response_type: "ListSpansResponse"
  }
  
  let list_spans_call = {
    method: list_spans_method,
    request: {
      data: "{\"trace_id\":\"trace-456\"}",
      metadata: [("authorization", "Bearer token123")]
    },
    timeout_ms: 3000
  }
  
  let list_spans_response = make_call(client, list_spans_call)
  assert_eq(list_spans_response.status_code, 0)  // OK
  assert_eq(list_spans_response.message.data, "{\"spans\":[{\"id\":\"span-123\",\"trace_id\":\"trace-456\"},{\"id\":\"span-789\",\"trace_id\":\"trace-456\"}]}")
  
  // Test CreateSpan call
  let create_span_method = {
    service: "TelemetryService",
    method: "CreateSpan",
    request_type: "CreateSpanRequest",
    response_type: "CreateSpanResponse"
  }
  
  let create_span_call = {
    method: create_span_method,
    request: {
      data: "{\"trace_id\":\"trace-new\",\"operation\":\"new-operation\"}",
      metadata: [("authorization", "Bearer token123")]
    },
    timeout_ms: 3000
  }
  
  let create_span_response = make_call(client, create_span_call)
  assert_eq(create_span_response.status_code, 0)  // OK
  assert_eq(create_span_response.message.data, "{\"id\":\"span-new\",\"trace_id\":\"trace-new\"}")
  
  // Test error case (span not found)
  let get_missing_span_call = {
    method: get_span_method,
    request: {
      data: "{\"span_id\":\"span-missing\"}",
      metadata: [("authorization", "Bearer token123")]
    },
    timeout_ms: 3000
  }
  
  let get_missing_span_response = make_call(client, get_missing_span_call)
  assert_eq(get_missing_span_response.status_code, 5)  // NOT_FOUND
  assert_eq(get_missing_span_response.message.data, "{\"error\":\"Span not found\"}")
  
  // Test MetricsService
  let get_metric_method = {
    service: "MetricsService",
    method: "GetMetric",
    request_type: "GetMetricRequest",
    response_type: "GetMetricResponse"
  }
  
  let get_metric_call = {
    method: get_metric_method,
    request: {
      data: "{\"metric_name\":\"http_requests\"}",
      metadata: [("authorization", "Bearer token123")]
    },
    timeout_ms: 3000
  }
  
  let get_metric_response = make_call(client, get_metric_call)
  assert_eq(get_metric_response.status_code, 0)  // OK
  assert_eq(get_metric_response.message.data, "{\"name\":\"http_requests\",\"value\":123,\"unit\":\"count\"}")
  
  // Test response metadata
  assert_true(get_span_response.message.metadata.contains(("content-type", "application/grpc")))
  assert_true(get_span_response.message.metadata.contains(("grpc-status", "0")))
  assert_true(get_span_response.message.metadata.contains(("grpc-message", "OK")))
  
  // Test response trailers
  assert_true(get_span_response.trailers.contains(("grpc-status", "0")))
  assert_true(get_span_response.trailers.contains(("grpc-message", "OK")))
}

// Test 4: TCP Socket Communication
test "TCP socket communication for telemetry data" {
  // Define TCP packet
  type TCPPacket = {
    data: String,
    sequence_number: Int,
    ack_number: Int,
    flags: Array[String]  // "SYN", "ACK", "FIN", etc.
  }
  
  // Define TCP connection
  type TCPConnection = {
    local_address: String,
    remote_address: String,
    local_port: Int,
    remote_port: Int,
    state: String,  // "CLOSED", "LISTEN", "SYN_SENT", "ESTABLISHED", "FIN_WAIT_1", "CLOSE_WAIT"
    send_buffer: Array[TCPPacket],
    receive_buffer: Array[TCPPacket],
    next_sequence: Int,
    next_ack: Int
  }
  
  // Create TCP connection
  let create_tcp_connection = fn(local_address: String, local_port: Int, remote_address: String, remote_port: Int) {
    {
      local_address,
      remote_address,
      local_port,
      remote_port,
      state: "CLOSED",
      send_buffer: [],
      receive_buffer: [],
      next_sequence: 1000,
      next_ack: 2000
    }
  }
  
  // Connect (simulated three-way handshake)
  let connect = fn(connection: TCPConnection) {
    if connection.state == "CLOSED" {
      // Send SYN
      let syn_packet = {
        data: "",
        sequence_number: connection.next_sequence,
        ack_number: 0,
        flags: ["SYN"]
      }
      
      {
        local_address: connection.local_address,
        remote_address: connection.remote_address,
        local_port: connection.local_port,
        remote_port: connection.remote_port,
        state: "SYN_SENT",
        send_buffer: connection.send_buffer.push(syn_packet),
        receive_buffer: connection.receive_buffer,
        next_sequence: connection.next_sequence + 1,
        next_ack: connection.next_ack
      }
    } else {
      connection
    }
  }
  
  // Receive SYN-ACK (simulated)
  let receive_syn_ack = fn(connection: TCPConnection) {
    if connection.state == "SYN_SENT" {
      // Send ACK
      let ack_packet = {
        data: "",
        sequence_number: connection.next_sequence,
        ack_number: connection.next_ack,
        flags: ["ACK"]
      }
      
      {
        local_address: connection.local_address,
        remote_address: connection.remote_address,
        local_port: connection.local_port,
        remote_port: connection.remote_port,
        state: "ESTABLISHED",
        send_buffer: connection.send_buffer.push(ack_packet),
        receive_buffer: connection.receive_buffer,
        next_sequence: connection.next_sequence,
        next_ack: connection.next_ack + 1
      }
    } else {
      connection
    }
  }
  
  // Send data
  let send_data = fn(connection: TCPConnection, data: String) {
    if connection.state == "ESTABLISHED" {
      let data_packet = {
        data,
        sequence_number: connection.next_sequence,
        ack_number: connection.next_ack,
        flags: ["ACK", "PSH"]
      }
      
      {
        local_address: connection.local_address,
        remote_address: connection.remote_address,
        local_port: connection.local_port,
        remote_port: connection.remote_port,
        state: connection.state,
        send_buffer: connection.send_buffer.push(data_packet),
        receive_buffer: connection.receive_buffer,
        next_sequence: connection.next_sequence + data.length(),
        next_ack: connection.next_ack
      }
    } else {
      connection
    }
  }
  
  // Receive data
  let receive_data = fn(connection: TCPConnection, data: String) {
    if connection.state == "ESTABLISHED" {
      let data_packet = {
        data,
        sequence_number: connection.next_ack,
        ack_number: 0,
        flags: ["ACK", "PSH"]
      }
      
      // Send ACK
      let ack_packet = {
        data: "",
        sequence_number: connection.next_sequence,
        ack_number: connection.next_ack + data.length(),
        flags: ["ACK"]
      }
      
      {
        local_address: connection.local_address,
        remote_address: connection.remote_address,
        local_port: connection.local_port,
        remote_port: connection.remote_port,
        state: connection.state,
        send_buffer: connection.send_buffer.push(ack_packet),
        receive_buffer: connection.receive_buffer.push(data_packet),
        next_sequence: connection.next_sequence,
        next_ack: connection.next_ack + data.length()
      }
    } else {
      connection
    }
  }
  
  // Close connection (simulated)
  let close = fn(connection: TCPConnection) {
    if connection.state == "ESTABLISHED" {
      // Send FIN
      let fin_packet = {
        data: "",
        sequence_number: connection.next_sequence,
        ack_number: connection.next_ack,
        flags: ["FIN", "ACK"]
      }
      
      {
        local_address: connection.local_address,
        remote_address: connection.remote_address,
        local_port: connection.local_port,
        remote_port: connection.remote_port,
        state: "FIN_WAIT_1",
        send_buffer: connection.send_buffer.push(fin_packet),
        receive_buffer: connection.receive_buffer,
        next_sequence: connection.next_sequence,
        next_ack: connection.next_ack
      }
    } else {
      connection
    }
  }
  
  // Create TCP connection
  let mut connection = create_tcp_connection("192.168.1.100", 12345, "192.168.1.200", 8080)
  
  // Test initial state
  assert_eq(connection.state, "CLOSED")
  assert_eq(connection.send_buffer.length(), 0)
  assert_eq(connection.receive_buffer.length(), 0)
  assert_eq(connection.next_sequence, 1000)
  assert_eq(connection.next_ack, 2000)
  
  // Connect (send SYN)
  connection = connect(connection)
  assert_eq(connection.state, "SYN_SENT")
  assert_eq(connection.send_buffer.length(), 1)
  assert_eq(connection.send_buffer[0].flags, ["SYN"])
  assert_eq(connection.next_sequence, 1001)
  
  // Receive SYN-ACK
  connection = receive_syn_ack(connection)
  assert_eq(connection.state, "ESTABLISHED")
  assert_eq(connection.send_buffer.length(), 2)
  assert_eq(connection.send_buffer[1].flags, ["ACK"])
  assert_eq(connection.next_ack, 2001)
  
  // Send data
  connection = send_data(connection, "telemetry data");
  assert_eq(connection.state, "ESTABLISHED")
  assert_eq(connection.send_buffer.length(), 3)
  assert_eq(connection.send_buffer[2].data, "telemetry data")
  assert_eq(connection.send_buffer[2].flags, ["ACK", "PSH"])
  assert_eq(connection.next_sequence, 1001 + "telemetry data".length())
  
  // Receive data
  connection = receive_data(connection, "response data");
  assert_eq(connection.state, "ESTABLISHED")
  assert_eq(connection.receive_buffer.length(), 1)
  assert_eq(connection.receive_buffer[0].data, "response data")
  assert_eq(connection.send_buffer.length(), 4)  // ACK sent
  assert_eq(connection.next_ack, 2001 + "response data".length())
  
  // Send more data
  connection = send_data(connection, "more telemetry data");
  assert_eq(connection.send_buffer.length(), 5)
  assert_eq(connection.send_buffer[4].data, "more telemetry data")
  
  // Close connection
  connection = close(connection);
  assert_eq(connection.state, "FIN_WAIT_1")
  assert_eq(connection.send_buffer.length(), 6)
  assert_eq(connection.send_buffer[5].flags, ["FIN", "ACK"])
}

// Test 5: UDP Datagram Communication
test "UDP datagram communication for lightweight telemetry" {
  // Define UDP datagram
  type UDPPacket = {
    data: String,
    source_address: String,
    source_port: Int,
    destination_address: String,
    destination_port: Int
  }
  
  // Define UDP socket
  type UDPSocket = {
    local_address: String,
    local_port: Int,
    packets_sent: Array[UDPPacket],
    packets_received: Array[UDPPacket]
  }
  
  // Create UDP socket
  let create_udp_socket = fn(address: String, port: Int) {
    {
      local_address: address,
      local_port: port,
      packets_sent: [],
      packets_received: []
    }
  }
  
  // Send datagram
  let send_datagram = fn(socket: UDPSocket, destination_address: String, destination_port: Int, data: String) {
    let packet = {
      data,
      source_address: socket.local_address,
      source_port: socket.local_port,
      destination_address,
      destination_port
    }
    
    {
      local_address: socket.local_address,
      local_port: socket.local_port,
      packets_sent: socket.packets_sent.push(packet),
      packets_received: socket.packets_received
    }
  }
  
  // Receive datagram
  let receive_datagram = fn(socket: UDPSocket, source_address: String, source_port: Int, data: String) {
    let packet = {
      data,
      source_address,
      source_port,
      destination_address: socket.local_address,
      destination_port: socket.local_port
    }
    
    {
      local_address: socket.local_address,
      local_port: socket.local_port,
      packets_sent: socket.packets_sent,
      packets_received: socket.packets_received.push(packet)
    }
  }
  
  // Create UDP socket
  let mut socket = create_udp_socket("192.168.1.100", 12345)
  
  // Test initial state
  assert_eq(socket.local_address, "192.168.1.100")
  assert_eq(socket.local_port, 12345)
  assert_eq(socket.packets_sent.length(), 0)
  assert_eq(socket.packets_received.length(), 0)
  
  // Send datagrams
  socket = send_datagram(socket, "192.168.1.200", 8080, "metric data 1");
  socket = send_datagram(socket, "192.168.1.200", 8080, "metric data 2");
  socket = send_datagram(socket, "192.168.1.201", 9090, "log data");
  
  // Test sent packets
  assert_eq(socket.packets_sent.length(), 3)
  assert_eq(socket.packets_sent[0].data, "metric data 1")
  assert_eq(socket.packets_sent[0].source_address, "192.168.1.100")
  assert_eq(socket.packets_sent[0].source_port, 12345)
  assert_eq(socket.packets_sent[0].destination_address, "192.168.1.200")
  assert_eq(socket.packets_sent[0].destination_port, 8080)
  
  assert_eq(socket.packets_sent[1].data, "metric data 2")
  assert_eq(socket.packets_sent[1].destination_address, "192.168.1.200")
  assert_eq(socket.packets_sent[1].destination_port, 8080)
  
  assert_eq(socket.packets_sent[2].data, "log data")
  assert_eq(socket.packets_sent[2].destination_address, "192.168.1.201")
  assert_eq(socket.packets_sent[2].destination_port, 9090)
  
  // Receive datagrams
  socket = receive_datagram(socket, "192.168.1.200", 8080, "response 1");
  socket = receive_datagram(socket, "192.168.1.201", 9090, "response 2");
  
  // Test received packets
  assert_eq(socket.packets_received.length(), 2)
  assert_eq(socket.packets_received[0].data, "response 1")
  assert_eq(socket.packets_received[0].source_address, "192.168.1.200")
  assert_eq(socket.packets_received[0].source_port, 8080)
  assert_eq(socket.packets_received[0].destination_address, "192.168.1.100")
  assert_eq(socket.packets_received[0].destination_port, 12345)
  
  assert_eq(socket.packets_received[1].data, "response 2")
  assert_eq(socket.packets_received[1].source_address, "192.168.1.201")
  assert_eq(socket.packets_received[1].source_port, 9090)
}

// Test 6: Network Retry and Timeout Handling
test "network retry and timeout handling" {
  // Define network operation result
  type NetworkResult = {
    success: Bool,
    data: String,
    error_code: Option[Int],
    error_message: Option[String]
  }
  
  // Define retry policy
  type RetryPolicy = {
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float
  }
  
  // Define network client with retry
  type NetworkClient = {
    retry_policy: RetryPolicy,
    failure_rate: Float  // Simulated failure rate (0.0-1.0)
  }
  
  // Create network client
  let create_network_client = fn(max_attempts: Int, base_delay_ms: Int, max_delay_ms: Int, failure_rate: Float) {
    {
      retry_policy: {
        max_attempts,
        base_delay_ms,
        max_delay_ms,
        backoff_multiplier: 2.0
      },
      failure_rate
    }
  }
  
  // Execute network operation with retry (simulated)
  let execute_with_retry = fn(client: NetworkClient, operation: () -> NetworkResult) {
    let mut attempts = 0
    let mut current_delay = client.retry_policy.base_delay_ms
    let mut last_result = {
      success: false,
      data: "",
      error_code: Some(-1),
      error_message: Some("Not attempted")
    }
    
    while attempts < client.retry_policy.max_attempts {
      attempts = attempts + 1
      
      // Simulate random failure based on failure_rate
      let random_value = 0.5  // Simulated random value
      let should_fail = random_value < client.failure_rate
      
      let result = if should_fail {
        {
          success: false,
          data: "",
          error_code: Some(500),
          error_message: Some("Simulated network error")
        }
      } else {
        operation()
      }
      
      if result.success {
        return result
      } else {
        last_result = result
        
        if attempts < client.retry_policy.max_attempts {
          // Calculate delay for next attempt
          current_delay = if current_delay * client.retry_policy.backoff_multiplier.to_int() < client.retry_policy.max_delay_ms {
            (current_delay.to_float() * client.retry_policy.backoff_multiplier).to_int()
          } else {
            client.retry_policy.max_delay_ms
          }
        }
      }
    }
    
    last_result
  }
  
  // Test successful operation
  let successful_operation = fn() {
    {
      success: true,
      data: "operation successful",
      error_code: None,
      error_message: None
    }
  }
  
  // Test failing operation
  let failing_operation = fn() {
    {
      success: false,
      data: "",
      error_code: Some(500),
      error_message: Some("Operation failed")
    }
  }
  
  // Create client with no failures
  let reliable_client = create_network_client(3, 100, 1000, 0.0)
  
  let reliable_result = execute_with_retry(reliable_client, successful_operation)
  assert_true(reliable_result.success)
  assert_eq(reliable_result.data, "operation successful")
  
  // Create client with high failure rate
  let unreliable_client = create_network_client(3, 100, 1000, 1.0)
  
  let unreliable_result = execute_with_retry(unreliable_client, successful_operation)
  assert_false(unreliable_result.success)
  assert_eq(unreliable_result.error_code, Some(500))
  assert_eq(unreliable_result.error_message, Some("Simulated network error"))
  
  // Create client with moderate failure rate and operation that eventually succeeds
  let moderate_client = create_network_client(3, 100, 1000, 0.8)
  
  // Simulate operation that succeeds on third attempt
  let mut attempt_count = 0
  let eventual_success_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count >= 3 {
      {
        success: true,
        data: "eventual success",
        error_code: None,
        error_message: None
      }
    } else {
      {
        success: false,
        data: "",
        error_code: Some(500),
        error_message: Some("Operation failed")
      }
    }
  }
  
  // Reset attempt count
  attempt_count = 0
  let eventual_result = execute_with_retry(moderate_client, eventual_success_operation)
  assert_true(eventual_result.success)
  assert_eq(eventual_result.data, "eventual success")
  
  // Test with max attempts of 1 (no retry)
  let no_retry_client = create_network_client(1, 100, 1000, 0.8)
  
  // Reset attempt count
  attempt_count = 0
  let no_retry_result = execute_with_retry(no_retry_client, eventual_success_operation)
  assert_false(no_retry_result.success)
}

// Test 7: Network Load Balancing
test "network load balancing for telemetry services" {
  // Define service endpoint
  type ServiceEndpoint = {
    host: String,
    port: Int,
    weight: Int,
    healthy: Bool,
    current_connections: Int,
    max_connections: Int
  }
  
  // Define load balancer
  type LoadBalancer = {
    algorithm: String,  // "round_robin", "weighted_round_robin", "least_connections"
    endpoints: Array[ServiceEndpoint],
    current_index: Int
  }
  
  // Create load balancer
  let create_load_balancer = fn(algorithm: String, endpoints: Array[ServiceEndpoint]) {
    {
      algorithm,
      endpoints,
      current_index: 0
    }
  }
  
  // Select endpoint using round-robin
  let select_round_robin = fn(lb: LoadBalancer) {
    if lb.endpoints.length() == 0 {
      None
    } else {
      let endpoint = lb.endpoints[lb.current_index]
      let next_index = (lb.current_index + 1) % lb.endpoints.length()
      
      Some({
        endpoint,
        updated_lb: {
          algorithm: lb.algorithm,
          endpoints: lb.endpoints,
          current_index: next_index
        }
      })
    }
  }
  
  // Select endpoint using weighted round-robin
  let select_weighted_round_robin = fn(lb: LoadBalancer) {
    if lb.endpoints.length() == 0 {
      None
    } else {
      // Calculate total weight
      let mut total_weight = 0
      for endpoint in lb.endpoints {
        if endpoint.healthy {
          total_weight = total_weight + endpoint.weight
        }
      }
      
      if total_weight == 0 {
        None
      } else {
        // Select based on weight (simplified)
        let mut weight_sum = 0
        let random_value = lb.current_index % total_weight  // Simulated random value
        
        for i in 0..lb.endpoints.length() {
          if lb.endpoints[i].healthy {
            weight_sum = weight_sum + lb.endpoints[i].weight
            if random_value < weight_sum {
              return Some({
                endpoint: lb.endpoints[i],
                updated_lb: {
                  algorithm: lb.algorithm,
                  endpoints: lb.endpoints,
                  current_index: (lb.current_index + 1) % total_weight
                }
              })
            }
          }
        }
        
        None
      }
    }
  }
  
  // Select endpoint using least connections
  let select_least_connections = fn(lb: LoadBalancer) {
    if lb.endpoints.length() == 0 {
      None
    } else {
      let mut min_connections = 999999
      let mut selected_endpoint = None
      let mut selected_index = -1
      
      for i in 0..lb.endpoints.length() {
        let endpoint = lb.endpoints[i]
        if endpoint.healthy && endpoint.current_connections < min_connections && endpoint.current_connections < endpoint.max_connections {
          min_connections = endpoint.current_connections
          selected_endpoint = Some(endpoint)
          selected_index = i
        }
      }
      
      match selected_endpoint {
        Some(endpoint) => {
          let mut updated_endpoints = lb.endpoints
          updated_endpoints[selected_index] = {
            host: endpoint.host,
            port: endpoint.port,
            weight: endpoint.weight,
            healthy: endpoint.healthy,
            current_connections: endpoint.current_connections + 1,
            max_connections: endpoint.max_connections
          }
          
          Some({
            endpoint,
            updated_lb: {
              algorithm: lb.algorithm,
              endpoints: updated_endpoints,
              current_index: selected_index
            }
          })
        }
        None => None
      }
    }
  }
  
  // Release connection
  let release_connection = fn(lb: LoadBalancer, host: String, port: Int) {
    let mut updated_endpoints = lb.endpoints
    
    for i in 0..updated_endpoints.length() {
      if updated_endpoints[i].host == host && updated_endpoints[i].port == port && updated_endpoints[i].current_connections > 0 {
        updated_endpoints[i] = {
          host: updated_endpoints[i].host,
          port: updated_endpoints[i].port,
          weight: updated_endpoints[i].weight,
          healthy: updated_endpoints[i].healthy,
          current_connections: updated_endpoints[i].current_connections - 1,
          max_connections: updated_endpoints[i].max_connections
        }
      }
    }
    
    {
      algorithm: lb.algorithm,
      endpoints: updated_endpoints,
      current_index: lb.current_index
    }
  }
  
  // Create test endpoints
  let endpoints = [
    {
      host: "service1.example.com",
      port: 8080,
      weight: 1,
      healthy: true,
      current_connections: 0,
      max_connections: 100
    },
    {
      host: "service2.example.com",
      port: 8080,
      weight: 2,
      healthy: true,
      current_connections: 0,
      max_connections: 100
    },
    {
      host: "service3.example.com",
      port: 8080,
      weight: 3,
      healthy: true,
      current_connections: 0,
      max_connections: 100
    }
  ]
  
  // Test round-robin
  let mut rr_lb = create_load_balancer("round_robin", endpoints)
  
  let rr_result1 = select_round_robin(rr_lb)
  assert_true(rr_result1.is_some())
  assert_eq(rr_result1.unwrap().endpoint.host, "service1.example.com")
  rr_lb = rr_result1.unwrap().updated_lb
  
  let rr_result2 = select_round_robin(rr_lb)
  assert_true(rr_result2.is_some())
  assert_eq(rr_result2.unwrap().endpoint.host, "service2.example.com")
  rr_lb = rr_result2.unwrap().updated_lb
  
  let rr_result3 = select_round_robin(rr_lb)
  assert_true(rr_result3.is_some())
  assert_eq(rr_result3.unwrap().endpoint.host, "service3.example.com")
  rr_lb = rr_result3.unwrap().updated_lb
  
  let rr_result4 = select_round_robin(rr_lb)
  assert_true(rr_result4.is_some())
  assert_eq(rr_result4.unwrap().endpoint.host, "service1.example.com")  // Back to first
  
  // Test weighted round-robin
  let mut wrr_lb = create_load_balancer("weighted_round_robin", endpoints)
  
  let wrr_result1 = select_weighted_round_robin(wrr_lb)
  assert_true(wrr_result1.is_some())
  wrr_lb = wrr_result1.unwrap().updated_lb
  
  let wrr_result2 = select_weighted_round_robin(wrr_lb)
  assert_true(wrr_result2.is_some())
  wrr_lb = wrr_result2.unwrap().updated_lb
  
  let wrr_result3 = select_weighted_round_robin(wrr_lb)
  assert_true(wrr_result3.is_some())
  wrr_lb = wrr_result3.unwrap().updated_lb
  
  // Test least connections
  let mut lc_lb = create_load_balancer("least_connections", endpoints)
  
  let lc_result1 = select_least_connections(lc_lb)
  assert_true(lc_result1.is_some())
  assert_eq(lc_result1.unwrap().endpoint.host, "service1.example.com")  // All have 0 connections, picks first
  lc_lb = lc_result1.unwrap().updated_lb
  
  let lc_result2 = select_least_connections(lc_lb)
  assert_true(lc_result2.is_some())
  assert_eq(lc_result2.unwrap().endpoint.host, "service2.example.com")  // service1 has 1 connection
  lc_lb = lc_result2.unwrap().updated_lb
  
  let lc_result3 = select_least_connections(lc_lb)
  assert_true(lc_result3.is_some())
  assert_eq(lc_result3.unwrap().endpoint.host, "service3.example.com")  // service1 and service2 have 1 connection each
  lc_lb = lc_result3.unwrap().updated_lb
  
  // Test connection release
  lc_lb = release_connection(lc_lb, "service1.example.com", 8080);
  
  let lc_result4 = select_least_connections(lc_lb)
  assert_true(lc_result4.is_some())
  assert_eq(lc_result4.unwrap().endpoint.host, "service1.example.com")  // service1 has 0 connections again
}

// Test 8: Network Circuit Breaking
test "network circuit breaking for fault tolerance" {
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
    current_time: Int
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
      current_time: 0
    }
  }
  
  // Execute operation through circuit breaker
  let execute_through_breaker = fn(breaker: CircuitBreaker, operation: () -> { success: Bool, data: String }) {
    let current_time = breaker.current_time
    
    match breaker.state {
      CircuitState::Open => {
        // Check if timeout has passed
        if current_time - breaker.last_failure_time >= breaker.timeout_ms {
          // Try to transition to half-open
          let result = operation()
          if result.success {
            // Success, transition to closed
            {
              result: Some(result),
              updated_breaker: {
                state: CircuitState::Closed,
                failure_count: 0,
                success_count: 1,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: breaker.last_failure_time,
                current_time
              }
            }
          } else {
            // Still failing, stay open
            {
              result: None,
              updated_breaker: {
                state: CircuitState::Open,
                failure_count: breaker.failure_count + 1,
                success_count: 0,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: current_time,
                current_time
              }
            }
          }
        } else {
          // Still in open state
          {
            result: None,
            updated_breaker: breaker
          }
        }
      }
      CircuitState::HalfOpen => {
        let result = operation()
        if result.success {
          // Success, increment success count
          let new_success_count = breaker.success_count + 1
          if new_success_count >= breaker.success_threshold {
            // Transition to closed
            {
              result: Some(result),
              updated_breaker: {
                state: CircuitState::Closed,
                failure_count: 0,
                success_count: 0,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: breaker.last_failure_time,
                current_time
              }
            }
          } else {
            // Still in half-open
            {
              result: Some(result),
              updated_breaker: {
                state: CircuitState::HalfOpen,
                failure_count: breaker.failure_count,
                success_count: new_success_count,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: breaker.last_failure_time,
                current_time
              }
            }
          }
        } else {
          // Failure, transition back to open
          {
            result: Some(result),
            updated_breaker: {
              state: CircuitState::Open,
              failure_count: breaker.failure_count + 1,
              success_count: 0,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: current_time,
              current_time
            }
          }
        }
      }
      CircuitState::Closed => {
        let result = operation()
        if result.success {
          // Success, stay closed
          {
            result: Some(result),
            updated_breaker: {
              state: CircuitState::Closed,
              failure_count: 0,
              success_count: 0,
              failure_threshold: breaker.failure_threshold,
              success_threshold: breaker.success_threshold,
              timeout_ms: breaker.timeout_ms,
              last_failure_time: breaker.last_failure_time,
              current_time
            }
          }
        } else {
          // Failure, increment failure count
          let new_failure_count = breaker.failure_count + 1
          if new_failure_count >= breaker.failure_threshold {
            // Open circuit
            {
              result: Some(result),
              updated_breaker: {
                state: CircuitState::Open,
                failure_count: new_failure_count,
                success_count: 0,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: current_time,
                current_time
              }
            }
          } else {
            // Stay closed
            {
              result: Some(result),
              updated_breaker: {
                state: CircuitState::Closed,
                failure_count: new_failure_count,
                success_count: 0,
                failure_threshold: breaker.failure_threshold,
                success_threshold: breaker.success_threshold,
                timeout_ms: breaker.timeout_ms,
                last_failure_time: breaker.last_failure_time,
                current_time
              }
            }
          }
        }
      }
    }
  }
  
  // Create circuit breaker
  let mut breaker = create_circuit_breaker(3, 2, 5000)
  
  // Test successful operation
  let success_operation = fn() {
    {
      success: true,
      data: "operation successful"
    }
  }
  
  let success_result = execute_through_breaker(breaker, success_operation)
  assert_true(success_result.result.is_some())
  assert_true(success_result.unwrap().result.success)
  assert_eq(success_result.unwrap().updated_breaker.state, CircuitState::Closed)
  breaker = success_result.unwrap().updated_breaker
  
  // Test failing operations
  let fail_operation = fn() {
    {
      success: false,
      data: ""
    }
  }
  
  // First failure
  let fail_result1 = execute_through_breaker(breaker, fail_operation)
  assert_true(fail_result1.result.is_some())
  assert_false(fail_result1.unwrap().result.success)
  assert_eq(fail_result1.unwrap().updated_breaker.state, CircuitState::Closed)
  assert_eq(fail_result1.unwrap().updated_breaker.failure_count, 1)
  breaker = fail_result1.unwrap().updated_breaker
  
  // Second failure
  let fail_result2 = execute_through_breaker(breaker, fail_operation)
  assert_true(fail_result2.result.is_some())
  assert_false(fail_result2.unwrap().result.success)
  assert_eq(fail_result2.unwrap().updated_breaker.state, CircuitState::Closed)
  assert_eq(fail_result2.unwrap().updated_breaker.failure_count, 2)
  breaker = fail_result2.unwrap().updated_breaker
  
  // Third failure - should open circuit
  let fail_result3 = execute_through_breaker(breaker, fail_operation)
  assert_true(fail_result3.result.is_some())
  assert_false(fail_result3.unwrap().result.success)
  assert_eq(fail_result3.unwrap().updated_breaker.state, CircuitState::Open)
  assert_eq(fail_result3.unwrap().updated_breaker.failure_count, 3)
  breaker = fail_result3.unwrap().updated_breaker
  
  // Try operation while circuit is open (before timeout)
  breaker.current_time = breaker.last_failure_time + 1000  // 1 second later, less than timeout
  let open_result = execute_through_breaker(breaker, success_operation)
  assert_eq(open_result.result, None)  // Circuit is open, no result
  assert_eq(open_result.updated_breaker.state, CircuitState::Open)
  
  // Try operation after timeout (transition to half-open)
  breaker.current_time = breaker.last_failure_time + 6000  // 6 seconds later, more than timeout
  let half_open_result = execute_through_breaker(breaker, fail_operation)
  assert_true(half_open_result.result.is_some())
  assert_false(half_open_result.unwrap().result.success)
  assert_eq(half_open_result.unwrap().updated_breaker.state, CircuitState::Open)  // Back to open
  breaker = half_open_result.unwrap().updated_breaker
  
  // Try operation after timeout again (success this time)
  breaker.current_time = breaker.last_failure_time + 6000  // 6 seconds later, more than timeout
  let half_open_success_result = execute_through_breaker(breaker, success_operation)
  assert_true(half_open_success_result.result.is_some())
  assert_true(half_open_success_result.unwrap().result.success)
  assert_eq(half_open_success_result.unwrap().updated_breaker.state, CircuitState::HalfOpen)  // Still half-open
  assert_eq(half_open_success_result.unwrap().updated_breaker.success_count, 1)
  breaker = half_open_success_result.unwrap().updated_breaker
  
  // Another success - should close circuit
  breaker.current_time = breaker.current_time + 100
  let close_result = execute_through_breaker(breaker, success_operation)
  assert_true(close_result.result.is_some())
  assert_true(close_result.unwrap().result.success)
  assert_eq(close_result.unwrap().updated_breaker.state, CircuitState::Closed)  // Back to closed
}

// Test 9: Network Metrics Collection
test "network metrics collection for monitoring" {
  // Define network metrics
  type NetworkMetrics = {
    requests_sent: Int,
    requests_received: Int,
    bytes_sent: Int,
    bytes_received: Int,
    response_time_sum_ms: Int,
    response_time_count: Int,
    errors: Int,
    timeouts: Int
  }
  
  // Define metrics collector
  type MetricsCollector = {
    metrics: NetworkMetrics,
    start_time: Int,
    current_time: Int
  }
  
  // Create metrics collector
  let create_metrics_collector = fn(start_time: Int) {
    {
      metrics: {
        requests_sent: 0,
        requests_received: 0,
        bytes_sent: 0,
        bytes_received: 0,
        response_time_sum_ms: 0,
        response_time_count: 0,
        errors: 0,
        timeouts: 0
      },
      start_time,
      current_time: start_time
    }
  }
  
  // Record sent request
  let record_sent_request = fn(collector: MetricsCollector, bytes: Int) {
    {
      metrics: {
        requests_sent: collector.metrics.requests_sent + 1,
        requests_received: collector.metrics.requests_received,
        bytes_sent: collector.metrics.bytes_sent + bytes,
        bytes_received: collector.metrics.bytes_received,
        response_time_sum_ms: collector.metrics.response_time_sum_ms,
        response_time_count: collector.metrics.response_time_count,
        errors: collector.metrics.errors,
        timeouts: collector.metrics.timeouts
      },
      start_time: collector.start_time,
      current_time: collector.current_time
    }
  }
  
  // Record received response
  let record_received_response = fn(collector: MetricsCollector, bytes: Int, response_time_ms: Int) {
    {
      metrics: {
        requests_sent: collector.metrics.requests_sent,
        requests_received: collector.metrics.requests_received + 1,
        bytes_sent: collector.metrics.bytes_sent,
        bytes_received: collector.metrics.bytes_received + bytes,
        response_time_sum_ms: collector.metrics.response_time_sum_ms + response_time_ms,
        response_time_count: collector.metrics.response_time_count + 1,
        errors: collector.metrics.errors,
        timeouts: collector.metrics.timeouts
      },
      start_time: collector.start_time,
      current_time: collector.current_time
    }
  }
  
  // Record error
  let record_error = fn(collector: MetricsCollector) {
    {
      metrics: {
        requests_sent: collector.metrics.requests_sent,
        requests_received: collector.metrics.requests_received,
        bytes_sent: collector.metrics.bytes_sent,
        bytes_received: collector.metrics.bytes_received,
        response_time_sum_ms: collector.metrics.response_time_sum_ms,
        response_time_count: collector.metrics.response_time_count,
        errors: collector.metrics.errors + 1,
        timeouts: collector.metrics.timeouts
      },
      start_time: collector.start_time,
      current_time: collector.current_time
    }
  }
  
  // Record timeout
  let record_timeout = fn(collector: MetricsCollector) {
    {
      metrics: {
        requests_sent: collector.metrics.requests_sent,
        requests_received: collector.metrics.requests_received,
        bytes_sent: collector.metrics.bytes_sent,
        bytes_received: collector.metrics.bytes_received,
        response_time_sum_ms: collector.metrics.response_time_sum_ms,
        response_time_count: collector.metrics.response_time_count,
        errors: collector.metrics.errors,
        timeouts: collector.metrics.timeouts + 1
      },
      start_time: collector.start_time,
      current_time: collector.current_time
    }
  }
  
  // Calculate average response time
  let average_response_time = fn(collector: MetricsCollector) {
    if collector.metrics.response_time_count > 0 {
      collector.metrics.response_time_sum_ms / collector.metrics.response_time_count
    } else {
      0
    }
  }
  
  // Calculate error rate
  let error_rate = fn(collector: MetricsCollector) {
    if collector.metrics.requests_sent > 0 {
      (collector.metrics.errors.to_float() / collector.metrics.requests_sent.to_float()) * 100.0
    } else {
      0.0
    }
  }
  
  // Calculate throughput (requests per second)
  let throughput = fn(collector: MetricsCollector) {
    let duration_seconds = (collector.current_time - collector.start_time) / 1000
    if duration_seconds > 0 {
      collector.metrics.requests_sent.to_float() / duration_seconds.to_float()
    } else {
      0.0
    }
  }
  
  // Create metrics collector
  let mut collector = create_metrics_collector(1640995200000)
  
  // Record some operations
  collector = record_sent_request(collector, 100);
  collector.current_time = collector.current_time + 50
  collector = record_received_response(collector, 200, 50);
  
  collector = record_sent_request(collector, 150);
  collector.current_time = collector.current_time + 75
  collector = record_received_response(collector, 300, 75);
  
  collector = record_sent_request(collector, 120);
  collector.current_time = collector.current_time + 100
  collector = record_error(collector);
  
  collector = record_sent_request(collector, 80);
  collector.current_time = collector.current_time + 5000  // Timeout
  collector = record_timeout(collector);
  
  collector = record_sent_request(collector, 90);
  collector.current_time = collector.current_time + 30
  collector = record_received_response(collector, 250, 30);
  
  // Test metrics
  assert_eq(collector.metrics.requests_sent, 5)
  assert_eq(collector.metrics.requests_received, 3)
  assert_eq(collector.metrics.bytes_sent, 540)  // 100 + 150 + 120 + 80 + 90
  assert_eq(collector.metrics.bytes_received, 750)  // 200 + 300 + 250
  assert_eq(collector.metrics.response_time_sum_ms, 155)  // 50 + 75 + 30
  assert_eq(collector.metrics.response_time_count, 3)
  assert_eq(collector.metrics.errors, 1)
  assert_eq(collector.metrics.timeouts, 1)
  
  // Test calculated metrics
  assert_eq(average_response_time(collector), 51)  // 155 / 3 = 51.67, truncated
  assert_eq(error_rate(collector), 40.0)  // 2 errors (1 error + 1 timeout) / 5 requests * 100 = 40%
  assert_eq(throughput(collector), 0.000999)  // 5 requests / 5005 seconds ≈ 0.000999 requests/second
}

// Test 10: Network Connection Pooling
test "network connection pooling for efficiency" {
  // Define connection
  type Connection = {
    id: String,
    host: String,
    port: Int,
    created_at: Int,
    last_used_at: Int,
    in_use: Bool,
    use_count: Int
  }
  
  // Define connection pool
  type ConnectionPool = {
    connections: Array[Connection],
    max_size: Int,
    max_idle_time_ms: Int,
    max_lifetime_ms: Int,
    current_time: Int
  }
  
  // Create connection pool
  let create_connection_pool = fn(max_size: Int, max_idle_time_ms: Int, max_lifetime_ms: Int) {
    {
      connections: [],
      max_size,
      max_idle_time_ms,
      max_lifetime_ms,
      current_time: 0
    }
  }
  
  // Get connection from pool
  let get_connection = fn(pool: ConnectionPool, host: String, port: Int) {
    // Find existing unused connection to the same host/port
    let mut found_index = -1
    for i in 0..pool.connections.length() {
      let conn = pool.connections[i]
      if not(conn.in_use) && conn.host == host && conn.port == port {
        // Check if connection is still valid
        let idle_time = pool.current_time - conn.last_used_at
        let lifetime = pool.current_time - conn.created_at
        
        if idle_time < pool.max_idle_time_ms && lifetime < pool.max_lifetime_ms {
          found_index = i
        }
      }
    }
    
    if found_index >= 0 {
      // Use existing connection
      let mut updated_connections = pool.connections
      updated_connections[found_index] = {
        id: updated_connections[found_index].id,
        host: updated_connections[found_index].host,
        port: updated_connections[found_index].port,
        created_at: updated_connections[found_index].created_at,
        last_used_at: pool.current_time,
        in_use: true,
        use_count: updated_connections[found_index].use_count + 1
      }
      
      {
        connection: Some(updated_connections[found_index]),
        updated_pool: {
          connections: updated_connections,
          max_size: pool.max_size,
          max_idle_time_ms: pool.max_idle_time_ms,
          max_lifetime_ms: pool.max_lifetime_ms,
          current_time: pool.current_time
        }
      }
    } else if pool.connections.length() < pool.max_size {
      // Create new connection
      let new_connection = {
        id: "conn-" + pool.connections.length().to_string(),
        host,
        port,
        created_at: pool.current_time,
        last_used_at: pool.current_time,
        in_use: true,
        use_count: 1
      }
      
      {
        connection: Some(new_connection),
        updated_pool: {
          connections: pool.connections.push(new_connection),
          max_size: pool.max_size,
          max_idle_time_ms: pool.max_idle_time_ms,
          max_lifetime_ms: pool.max_lifetime_ms,
          current_time: pool.current_time
        }
      }
    } else {
      // Pool at capacity
      {
        connection: None,
        updated_pool: pool
      }
    }
  }
  
  // Release connection back to pool
  let release_connection = fn(pool: ConnectionPool, connection_id: String) {
    let mut updated_connections = pool.connections
    
    for i in 0..updated_connections.length() {
      if updated_connections[i].id == connection_id {
        updated_connections[i] = {
          id: updated_connections[i].id,
          host: updated_connections[i].host,
          port: updated_connections[i].port,
          created_at: updated_connections[i].created_at,
          last_used_at: pool.current_time,
          in_use: false,
          use_count: updated_connections[i].use_count
        }
      }
    }
    
    {
      connections: updated_connections,
      max_size: pool.max_size,
      max_idle_time_ms: pool.max_idle_time_ms,
      max_lifetime_ms: pool.max_lifetime_ms,
      current_time: pool.current_time
    }
  }
  
  // Clean up expired connections
  let cleanup_expired_connections = fn(pool: ConnectionPool) {
    let mut valid_connections = []
    
    for conn in pool.connections {
      let idle_time = pool.current_time - conn.last_used_at
      let lifetime = pool.current_time - conn.created_at
      
      if not(conn.in_use) && idle_time < pool.max_idle_time_ms && lifetime < pool.max_lifetime_ms {
        valid_connections = valid_connections.push(conn)
      }
    }
    
    {
      connections: valid_connections,
      max_size: pool.max_size,
      max_idle_time_ms: pool.max_idle_time_ms,
      max_lifetime_ms: pool.max_lifetime_ms,
      current_time: pool.current_time
    }
  }
  
  // Create connection pool
  let mut pool = create_connection_pool(3, 30000, 300000)  // 30 seconds idle, 5 minutes lifetime
  
  // Test initial state
  assert_eq(pool.connections.length(), 0)
  
  // Get connections
  pool.current_time = 1000
  let get_result1 = get_connection(pool, "host1.example.com", 8080)
  assert_true(get_result1.connection.is_some())
  assert_eq(get_result1.connection.unwrap().host, "host1.example.com")
  assert_eq(get_result1.connection.unwrap().port, 8080)
  assert_eq(get_result1.connection.unwrap().in_use, true)
  assert_eq(get_result1.connection.unwrap().use_count, 1)
  pool = get_result1.updated_pool
  
  let get_result2 = get_connection(pool, "host2.example.com", 8080)
  assert_true(get_result2.connection.is_some())
  assert_eq(get_result2.connection.unwrap().host, "host2.example.com")
  pool = get_result2.updated_pool
  
  let get_result3 = get_connection(pool, "host1.example.com", 8080)
  assert_true(get_result3.connection.is_some())
  assert_eq(get_result3.connection.unwrap().host, "host1.example.com")  // Different connection
  pool = get_result3.updated_pool
  
  // Pool should be at capacity
  assert_eq(pool.connections.length(), 3)
  
  // Try to get another connection (should fail)
  let get_result4 = get_connection(pool, "host3.example.com", 8080)
  assert_false(get_result4.connection.is_some())
  
  // Release connections
  pool.current_time = 2000
  pool = release_connection(pool, "conn-0");
  pool = release_connection(pool, "conn-1");
  pool = release_connection(pool, "conn-2");
  
  // All connections should be available
  for conn in pool.connections {
    assert_false(conn.in_use)
  }
  
  // Get connection again (should reuse)
  pool.current_time = 3000
  let get_result5 = get_connection(pool, "host1.example.com", 8080)
  assert_true(get_result5.connection.is_some())
  assert_eq(get_result5.connection.unwrap().id, "conn-0")  // Reused connection
  assert_eq(get_result5.connection.unwrap().use_count, 2)  // Used twice
  pool = get_result5.updated_pool
  
  // Test connection expiration
  pool.current_time = 35000  // 35 seconds after creation, exceeds idle time
  pool = cleanup_expired_connections(pool);
  
  // Connections should be cleaned up
  assert_eq(pool.connections.length(), 2)  // conn-0 is in use, others expired
  
  // Test lifetime expiration
  pool.current_time = 400000  // 400 seconds after creation, exceeds lifetime
  pool = release_connection(pool, "conn-0");  // Release first
  pool = cleanup_expired_connections(pool);
  
  // All connections should be expired
  assert_eq(pool.connections.length(), 0)
}