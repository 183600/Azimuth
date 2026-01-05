// Azimuth Telemetry System - Network Communication and Telemetry Tests
// This file contains test cases for network communication and telemetry functionality

// Test 1: HTTP Client Telemetry Integration
test "http client telemetry integration" {
  // Create an HTTP client with telemetry enabled
  let telemetry_config = HttpTelemetryConfig::new()
  HttpTelemetryConfig::enable_request_logging(telemetry_config, true)
  HttpTelemetryConfig::enable_response_logging(telemetry_config, true)
  HttpTelemetryConfig::enable_performance_metrics(telemetry_config, true)
  
  let http_client = HttpClient::with_telemetry(telemetry_config)
  
  // Create an HTTP request
  let headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer test-token"),
    ("X-Request-ID", "req-12345")
  ]
  
  let request_body = "{\"test\": \"data\", \"value\": 42}"
  let request = HttpRequest::new("POST", "https://api.example.com/telemetry", headers, Some(request_body))
  
  // Add telemetry context to the request
  let span_ctx = SpanContext::new("trace-123", "span-456", true, "test")
  let telemetry_request = HttpClient::add_telemetry_context(http_client, request, span_ctx)
  
  // Verify telemetry headers are added
  let telemetry_headers = HttpRequest::headers(telemetry_request)
  assert_true(HttpRequest::has_header(telemetry_headers, "X-Trace-ID"))
  assert_true(HttpRequest::has_header(telemetry_headers, "X-Span-ID"))
  
  // Simulate sending the request and receiving a response
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "150ms"),
    ("X-Server-ID", "server-789")
  ]
  
  let response_body = "{\"status\": \"success\", \"data\": {\"id\": 123}}"
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // Process telemetry from the response
  let telemetry_data = HttpClient::extract_telemetry(http_client, response)
  
  // Verify telemetry data is captured
  assert_true(TelemetryData::has_metric(telemetry_data, "http.request.duration"))
  assert_true(TelemetryData::has_attribute(telemetry_data, "http.status_code"))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "http.status_code"), Some(IntValue(200)))
}

// Test 2: WebSocket Telemetry Communication
test "websocket telemetry communication" {
  // Create a WebSocket client with telemetry
  let ws_config = WebSocketTelemetryConfig::new()
  WebSocketTelemetryConfig::enable_connection_metrics(ws_config, true)
  WebSocketTelemetryConfig::enable_message_tracking(ws_config, true)
  WebSocketTelemetryConfig::enable_latency_measurement(ws_config, true)
  
  let ws_client = WebSocketClient::with_telemetry(ws_config)
  
  // Connect to WebSocket server
  let connection_url = "wss://telemetry.example.com/ws"
  let connection_result = WebSocketClient::connect(ws_client, connection_url)
  
  match connection_result {
    Ok(connection) => {
      // Verify connection telemetry is captured
      let connection_telemetry = WebSocketClient::get_connection_telemetry(ws_client, connection)
      assert_true(TelemetryData::has_metric(connection_telemetry, "websocket.connection.established"))
      assert_true(TelemetryData::has_attribute(connection_telemetry, "websocket.url"))
      
      // Send telemetry data through WebSocket
      let telemetry_payload = TelemetryPayload::new()
      TelemetryPayload::add_metric(telemetry_payload, "cpu.usage", 75.5)
      TelemetryPayload::add_metric(telemetry_payload, "memory.usage", 60.2)
      TelemetryPayload::add_event(telemetry_payload, "system.startup", Attributes::new())
      
      let send_result = WebSocketClient::send_telemetry(ws_client, connection, telemetry_payload)
      assert_true(send_result.is_ok())
      
      // Verify send telemetry is captured
      let send_telemetry = WebSocketClient::get_send_telemetry(ws_client, connection)
      assert_true(TelemetryData::has_metric(send_telemetry, "websocket.message.sent"))
      assert_true(TelemetryData::has_metric(send_telemetry, "websocket.message.size"))
      
      // Simulate receiving telemetry data
      let received_payload = "{\"metrics\": [{\"name\": \"disk.usage\", \"value\": 45.8}]}"
      let receive_result = WebSocketClient::receive_telemetry(ws_client, connection, received_payload)
      assert_true(receive_result.is_ok())
      
      // Verify receive telemetry is captured
      let receive_telemetry = WebSocketClient::get_receive_telemetry(ws_client, connection)
      assert_true(TelemetryData::has_metric(receive_telemetry, "websocket.message.received"))
      assert_true(TelemetryData::has_attribute(receive_telemetry, "websocket.message.type"))
      
      // Close the connection
      WebSocketClient::close(ws_client, connection)
      
      // Verify close telemetry is captured
      let close_telemetry = WebSocketClient::get_close_telemetry(ws_client, connection)
      assert_true(TelemetryData::has_metric(close_telemetry, "websocket.connection.closed"))
      assert_true(TelemetryData::has_metric(close_telemetry, "websocket.connection.duration"))
    }
    Err(_) => assert_true(false) // Connection should succeed
  }
}

// Test 3: gRPC Telemetry Integration
test "grpc telemetry integration" {
  // Create a gRPC client with telemetry
  let grpc_config = GrpcTelemetryConfig::new()
  GrpcTelemetryConfig::enable_request_response_tracking(grpc_config, true)
  GrpcTelemetryConfig::enable_streaming_metrics(grpc_config, true)
  GrpcTelemetryConfig::enable_error_propagation(grpc_config, true)
  
  let grpc_client = GrpcClient::with_telemetry(grpc_config)
  
  // Create a gRPC request
  let method_name = "telemetry.Collector/CollectMetrics"
  let request_metadata = [
    ("authorization", "Bearer grpc-token"),
    ("x-client-id", "client-123")
  ]
  
  let request_message = "{\"service\": \"test-service\", \"metrics\": [{\"name\": \"test.metric\", \"value\": 42.5}]}"
  let grpc_request = GrpcRequest::new(method_name, request_message, request_metadata)
  
  // Add telemetry context to the request
  let span_ctx = SpanContext::new("grpc-trace-123", "grpc-span-456", true, "grpc-test")
  let telemetry_request = GrpcClient::add_telemetry_context(grpc_client, grpc_request, span_ctx)
  
  // Verify telemetry metadata is added
  let telemetry_metadata = GrpcRequest::metadata(telemetry_request)
  assert_true(GrpcRequest::has_metadata(telemetry_metadata, "x-trace-id"))
  assert_true(GrpcRequest::has_metadata(telemetry_metadata, "x-span-id"))
  
  // Simulate sending the request and receiving a response
  let response_metadata = [
    ("x-grpc-time", "120ms"),
    ("x-server-version", "1.2.3")
  ]
  
  let response_message = "{\"success\": true, \"processed\": 1}"
  let grpc_response = GrpcResponse::new(0, response_message, response_metadata) // 0 = OK status
  
  // Process telemetry from the response
  let telemetry_data = GrpcClient::extract_telemetry(grpc_client, grpc_response)
  
  // Verify telemetry data is captured
  assert_true(TelemetryData::has_metric(telemetry_data, "grpc.request.duration"))
  assert_true(TelemetryData::has_attribute(telemetry_data, "grpc.method"))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "grpc.method"), Some(StringValue(method_name)))
  assert_eq(TelemetryData::get_attribute(telemetry_data, "grpc.status_code"), Some(IntValue(0)))
}

// Test 4: Network Telemetry Aggregation
test "network telemetry aggregation" {
  // Create a telemetry aggregator
  let aggregator = NetworkTelemetryAggregator::new()
  NetworkTelemetryAggregator::enable_real_time_processing(aggregator, true)
  NetworkTelemetryAggregator::set_aggregation_interval(aggregator, 5000) // 5 seconds
  
  // Add multiple telemetry data points
  for i in 0..=99 {
    let telemetry_data = TelemetryData::new()
    
    // Add HTTP request metrics
    TelemetryData::add_metric(telemetry_data, "http.request.duration", 100.0 + i.to_float())
    TelemetryData::add_attribute(telemetry_data, "http.method", StringValue("GET"))
    TelemetryData::add_attribute(telemetry_data, "http.status_code", IntValue(200))
    
    // Add network metrics
    TelemetryData::add_metric(telemetry_data, "network.bytes.sent", 1024.0 * i.to_float())
    TelemetryData::add_metric(telemetry_data, "network.bytes.received", 2048.0 * i.to_float())
    
    // Add connection metrics
    TelemetryData::add_metric(telemetry_data, "network.connections.active", 10.0 + (i % 10).to_float())
    
    NetworkTelemetryAggregator::add_telemetry(aggregator, telemetry_data)
  }
  
  // Process aggregation
  let aggregated_data = NetworkTelemetryAggregator::process(aggregator)
  
  // Verify aggregated metrics
  assert_true(TelemetryData::has_metric(aggregated_data, "http.request.duration.avg"))
  assert_true(TelemetryData::has_metric(aggregated_data, "http.request.duration.min"))
  assert_true(TelemetryData::has_metric(aggregated_data, "http.request.duration.max"))
  assert_true(TelemetryData::has_metric(aggregated_data, "network.bytes.sent.total"))
  assert_true(TelemetryData::has_metric(aggregated_data, "network.bytes.received.total"))
  assert_true(TelemetryData::has_metric(aggregated_data, "network.connections.active.avg"))
  
  // Verify aggregated values
  let avg_duration = TelemetryData::get_metric(aggregated_data, "http.request.duration.avg")
  match avg_duration {
    Some(value) => assert_true(value > 100.0 && value < 200.0)
    None => assert_true(false)
  }
  
  let total_bytes_sent = TelemetryData::get_metric(aggregated_data, "network.bytes.sent.total")
  match total_bytes_sent {
    Some(value) => assert_eq(value, 1024.0 * 4950.0) // Sum of 0 to 99 * 1024
    None => assert_true(false)
  }
}

// Test 5: Network Telemetry Filtering
test "network telemetry filtering" {
  // Create a telemetry filter
  let filter = NetworkTelemetryFilter::new()
  
  // Add filter rules
  NetworkTelemetryFilter::add_include_rule(filter, "http.request.duration")
  NetworkTelemetryFilter::add_include_rule(filter, "network.bytes.*")
  NetworkTelemetryFilter::add_exclude_rule(filter, "network.bytes.internal")
  NetworkTelemetryFilter::add_attribute_filter(filter, "service.name", "production")
  
  // Create test telemetry data
  let telemetry_data = TelemetryData::new()
  
  // Add metrics that should be included
  TelemetryData::add_metric(telemetry_data, "http.request.duration", 150.0)
  TelemetryData::add_metric(telemetry_data, "network.bytes.sent", 1024.0)
  TelemetryData::add_metric(telemetry_data, "network.bytes.received", 2048.0)
  
  // Add metrics that should be excluded
  TelemetryData::add_metric(telemetry_data, "network.bytes.internal", 512.0)
  TelemetryData::add_metric(telemetry_data, "cpu.usage", 75.5)
  
  // Add attributes
  TelemetryData::add_attribute(telemetry_data, "service.name", StringValue("production"))
  TelemetryData::add_attribute(telemetry_data, "service.version", StringValue("1.2.3"))
  
  // Apply the filter
  let filtered_data = NetworkTelemetryFilter::apply(filter, telemetry_data)
  
  // Verify filtered data
  assert_true(TelemetryData::has_metric(filtered_data, "http.request.duration"))
  assert_true(TelemetryData::has_metric(filtered_data, "network.bytes.sent"))
  assert_true(TelemetryData::has_metric(filtered_data, "network.bytes.received"))
  assert_false(TelemetryData::has_metric(filtered_data, "network.bytes.internal"))
  assert_false(TelemetryData::has_metric(filtered_data, "cpu.usage"))
  
  // Verify attributes are preserved
  assert_eq(TelemetryData::get_attribute(filtered_data, "service.name"), Some(StringValue("production")))
  assert_eq(TelemetryData::get_attribute(filtered_data, "service.version"), Some(StringValue("1.2.3")))
}

// Test 6: Network Telemetry Buffering
test "network telemetry buffering" {
  // Create a telemetry buffer
  let buffer = NetworkTelemetryBuffer::new(1000) // Max 1000 items
  NetworkTelemetryBuffer::set_flush_interval(buffer, 10000) // 10 seconds
  NetworkTelemetryBuffer::enable_compression(buffer, true)
  
  // Add telemetry data to the buffer
  for i in 0..=99 {
    let telemetry_data = TelemetryData::new()
    TelemetryData::add_metric(telemetry_data, "test.metric", i.to_float())
    TelemetryData::add_attribute(telemetry_data, "batch.id", IntValue(i))
    
    let buffer_result = NetworkTelemetryBuffer::add(buffer, telemetry_data)
    assert_true(buffer_result.is_ok())
  }
  
  // Verify buffer stats
  assert_eq(NetworkTelemetryBuffer::size(buffer), 100)
  assert_false(NetworkTelemetryBuffer::is_full(buffer))
  assert_false(NetworkTelemetryBuffer::should_flush(buffer)) // Not time-based flush yet
  
  // Force a flush
  let flushed_data = NetworkTelemetryBuffer::flush(buffer)
  
  // Verify flushed data
  assert_eq(flushed_data.length(), 100)
  assert_eq(NetworkTelemetryBuffer::size(buffer), 0)
  
  // Verify data integrity
  for i in 0..=99 {
    let item = flushed_data[i]
    let metric_value = TelemetryData::get_metric(item, "test.metric")
    match metric_value {
      Some(value) => assert_eq(value, i.to_float())
      None => assert_true(false)
    }
    
    let batch_id = TelemetryData::get_attribute(item, "batch.id")
    match batch_id {
      IntValue(id) => assert_eq(id, i)
      _ => assert_true(false)
    }
  }
}

// Test 7: Network Telemetry Retry Logic
test "network telemetry retry logic" {
  // Create a telemetry sender with retry logic
  let retry_config = RetryConfig::new()
  RetryConfig::set_max_retries(retry_config, 3)
  RetryConfig::set_initial_delay(retry_config, 1000) // 1 second
  RetryConfig::set_max_delay(retry_config, 10000) // 10 seconds
  RetryConfig::set_backoff_multiplier(retry_config, 2.0)
  
  let sender = NetworkTelemetrySender::with_retry(retry_config)
  
  // Create test telemetry data
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_metric(telemetry_data, "test.metric", 42.0)
  TelemetryData::add_event(telemetry_data, "test.event", Attributes::new())
  
  // Mock a failure response for the first two attempts
  let mock_response_1 = MockHttpResponse::new(500, Some("Server Error"))
  let mock_response_2 = MockHttpResponse::new(503, Some("Service Unavailable"))
  let mock_response_3 = MockHttpResponse::new(200, Some("Success"))
  
  // Configure the sender to use mock responses
  NetworkTelemetrySender::add_mock_response(sender, mock_response_1)
  NetworkTelemetrySender::add_mock_response(sender, mock_response_2)
  NetworkTelemetrySender::add_mock_response(sender, mock_response_3)
  
  // Send telemetry data
  let send_result = NetworkTelemetrySender::send(sender, telemetry_data)
  
  // Verify the send eventually succeeds
  match send_result {
    Ok(response) => assert_eq(MockHttpResponse::status_code(response), 200)
    Err(_) => assert_true(false) // Should eventually succeed
  }
  
  // Verify retry metrics
  let retry_metrics = NetworkTelemetrySender::get_retry_metrics(sender)
  assert_true(TelemetryData::has_metric(retry_metrics, "telemetry.send.attempts"))
  assert_eq(TelemetryData::get_metric(retry_metrics, "telemetry.send.attempts"), Some(3.0))
  assert_true(TelemetryData::has_metric(retry_metrics, "telemetry.send.retries"))
  assert_eq(TelemetryData::get_metric(retry_metrics, "telemetry.send.retries"), Some(2.0))
}

// Test 8: Network Telemetry Protocol Detection
test "network telemetry protocol detection" {
  // Create a protocol detector
  let detector = NetworkProtocolDetector::new()
  
  // Test HTTP protocol detection
  let http_request = "GET /api/telemetry HTTP/1.1\r\nHost: example.com\r\n\r\n"
  let detected_http = NetworkProtocolDetector::detect(detector, http_request.to_bytes())
  match detected_http {
    Some(HttpProtocol(version)) => assert_eq(version, "1.1")
    _ => assert_true(false)
  }
  
  // Test HTTPS protocol detection (via SNI)
  let https_client_hello = [0x16, 0x03, 0x01] // TLS Handshake, version 1.0
  let detected_https = NetworkProtocolDetector::detect(detector, https_client_hello)
  match detected_https {
    Some(HttpsProtocol) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test WebSocket protocol detection
  let websocket_request = "GET /ws HTTP/1.1\r\nUpgrade: websocket\r\nConnection: Upgrade\r\n\r\n"
  let detected_websocket = NetworkProtocolDetector::detect(detector, websocket_request.to_bytes())
  match detected_websocket {
    Some(WebSocketProtocol) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test gRPC protocol detection
  let grpc_request = [0x00, 0x00, 0x00, 0x00, 0x10] // gRPC prefix
  let detected_grpc = NetworkProtocolDetector::detect(detector, grpc_request)
  match detected_grpc {
    Some(GrpcProtocol) => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test unknown protocol
  let unknown_data = [0xFF, 0xFE, 0xFD, 0xFC]
  let detected_unknown = NetworkProtocolDetector::detect(detector, unknown_data)
  match detected_unknown {
    Some(_) => assert_true(false) // Should not detect a known protocol
    None => assert_true(true)     // Should return None for unknown protocol
  }
}