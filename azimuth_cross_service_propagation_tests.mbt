// Azimuth Cross-Service Propagation Tests
// This file contains high-quality test cases for cross-service propagation

// Test 1: HTTP Header Trace Context Propagation
test "http header trace context propagation" {
  // Test case 1: W3C Trace Context format
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let parent_span_id = "00f067aa0ba902b7"
  
  let span_ctx = SpanContext::new(trace_id, span_id, true, "http_test_state")
  
  // Extract HTTP headers
  let headers = HttpPropagator::extract_headers(span_ctx)
  
  // Verify traceparent header
  let traceparent = HttpPropagator::get_header(headers, "traceparent")
  match traceparent {
    Some(value) => {
      // Format: 00-trace_id-span_id-flags
      let expected = "00-" + trace_id + "-" + span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Verify tracestate header
  let tracestate = HttpPropagator::get_header(headers, "tracestate")
  match tracestate {
    Some(value) => {
      // Should contain state information
      assert_true(value.length() > 0)
    }
    None => {
      // tracestate is optional
      assert_true(true)
    }
  }
  
  // Test case 2: Inject context into HTTP request
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token123")
  ]
  
  let request = HttpRequest::new("GET", "https://api.example.com/data", request_headers, None)
  let injected_request = HttpPropagator::inject_context(request, span_ctx)
  
  // Verify headers are injected
  let injected_traceparent = HttpPropagator::get_header(HttpRequest::headers(injected_request), "traceparent")
  match injected_traceparent {
    Some(value) => {
      let expected = "00-" + trace_id + "-" + span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 3: Extract context from HTTP request
  let incoming_headers = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("Content-Type", "application/json")
  ]
  
  let incoming_request = HttpRequest::new("POST", "/api/process", incoming_headers, Some("request data"))
  let extracted_ctx = HttpPropagator::extract_context(incoming_request)
  
  // Verify extracted context
  assert_eq(SpanContext::trace_id(extracted_ctx), trace_id)
  assert_eq(SpanContext::span_id(extracted_ctx), span_id)
  assert_true(SpanContext::is_sampled(extracted_ctx))
  
  // Test case 4: B3 format propagation
  let b3_headers = HttpPropagator::extract_b3_headers(span_ctx)
  
  let b3_trace_id = HttpPropagator::get_header(b3_headers, "X-B3-TraceId")
  match b3_trace_id {
    Some(value) => assert_eq(value, trace_id)
    None => assert_true(false)
  }
  
  let b3_span_id = HttpPropagator::get_header(b3_headers, "X-B3-SpanId")
  match b3_span_id {
    Some(value) => assert_eq(value, span_id)
    None => assert_true(false)
  }
  
  let b3_sampled = HttpPropagator::get_header(b3_headers, "X-B3-Sampled")
  match b3_sampled {
    Some(value) => assert_eq(value, "1")
    None => assert_true(false)
  }
}

// Test 2: Baggage Propagation Across Services
test "baggage propagation across services" {
  // Test case 1: Initial baggage setup
  let initial_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user123")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session456")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_session, "tenant.id", "tenant789")
  
  // Test case 2: Service A adds service-specific baggage
  let service_a_baggage = Baggage::set_entry(baggage_with_tenant, "service.a.version", "1.2.3")
  let service_a_baggage_with_request = Baggage::set_entry(service_a_baggage, "request.id", "req123")
  
  // Propagate to Service B
  let service_b_headers = BaggagePropagator::extract_headers(service_a_baggage_with_request)
  let service_b_baggage = BaggagePropagator::extract_from_headers(service_b_headers)
  
  // Verify baggage propagated to Service B
  let user_id = Baggage::get_entry(service_b_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "user123")
    None => assert_true(false)
  }
  
  let session_id = Baggage::get_entry(service_b_baggage, "session.id")
  match session_id {
    Some(value) => assert_eq(value, "session456")
    None => assert_true(false)
  }
  
  let tenant_id = Baggage::get_entry(service_b_baggage, "tenant.id")
  match tenant_id {
    Some(value) => assert_eq(value, "tenant789")
    None => assert_true(false)
  }
  
  let service_a_version = Baggage::get_entry(service_b_baggage, "service.a.version")
  match service_a_version {
    Some(value) => assert_eq(value, "1.2.3")
    None => assert_true(false)
  }
  
  // Test case 3: Service B adds its own baggage
  let service_b_baggage_enhanced = Baggage::set_entry(service_b_baggage, "service.b.version", "2.3.4")
  let service_b_baggage_with_operation = Baggage::set_entry(service_b_baggage_enhanced, "operation.type", "data_processing")
  
  // Propagate to Service C
  let service_c_headers = BaggagePropagator::extract_headers(service_b_baggage_with_operation)
  let service_c_baggage = BaggagePropagator::extract_from_headers(service_c_headers)
  
  // Verify all baggage propagated to Service C
  let service_b_version = Baggage::get_entry(service_c_baggage, "service.b.version")
  match service_b_version {
    Some(value) => assert_eq(value, "2.3.4")
    None => assert_true(false)
  }
  
  let operation_type = Baggage::get_entry(service_c_baggage, "operation.type")
  match operation_type {
    Some(value) => assert_eq(value, "data_processing")
    None => assert_true(false)
  }
  
  // Test case 4: Service C adds final baggage
  let service_c_baggage_final = Baggage::set_entry(service_c_baggage, "service.c.version", "3.4.5")
  let service_c_baggage_with_result = Baggage::set_entry(service_c_baggage_final, "operation.result", "success")
  
  // Verify final baggage contains all entries
  let final_entries = [
    ("user.id", "user123"),
    ("session.id", "session456"),
    ("tenant.id", "tenant789"),
    ("service.a.version", "1.2.3"),
    ("request.id", "req123"),
    ("service.b.version", "2.3.4"),
    ("operation.type", "data_processing"),
    ("service.c.version", "3.4.5"),
    ("operation.result", "success")
  ]
  
  for (key, expected_value) in final_entries {
    let value = Baggage::get_entry(service_c_baggage_with_result, key)
    match value {
      Some(actual_value) => assert_eq(actual_value, expected_value)
      None => assert_true(false)
    }
  }
}

// Test 3: Message Queue Trace Context Propagation
test "message queue trace context propagation" {
  // Test case 1: Producer injects trace context into message
  let trace_id = "msg_trace_12345"
  let span_id = "msg_span_67890"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "message_test_state")
  
  let message_payload = "{ \"event\": \"user_created\", \"user_id\": 12345 }"
  let message_headers = [
    ("Content-Type", "application/json"),
    ("Message-Type", "event")
  ]
  
  let message = Message::new("user.events", message_payload, message_headers)
  let traced_message = MessagePropagator::inject_context(message, span_ctx)
  
  // Verify trace context is injected into message
  let traced_headers = Message::headers(traced_message)
  let trace_header = MessagePropagator::get_header(traced_headers, "traceparent")
  match trace_header {
    Some(value) => {
      let expected = "00-" + trace_id + "-" + span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 2: Consumer extracts trace context from message
  let consumer_span_ctx = MessagePropagator::extract_context(traced_message)
  
  // Verify extracted context
  assert_eq(SpanContext::trace_id(consumer_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(consumer_span_ctx), span_id)
  assert_true(SpanContext::is_sampled(consumer_span_ctx))
  
  // Test case 3: Consumer creates child span and propagates to next service
  let consumer_span_id = "consumer_span_abc123"
  let consumer_span = SpanContext::new(trace_id, consumer_span_id, true, "consumer_state")
  
  // Create new message for next service
  let next_message_payload = "{ \"action\": \"process_user\", \"user_id\": 12345 }"
  let next_message_headers = [
    ("Content-Type", "application/json"),
    ("Action-Type", "command")
  ]
  
  let next_message = Message::new("user.commands", next_message_payload, next_message_headers)
  let next_traced_message = MessagePropagator::inject_context(next_message, consumer_span)
  
  // Verify next message has correct trace context
  let next_traced_headers = Message::headers(next_traced_message)
  let next_trace_header = MessagePropagator::get_header(next_traced_headers, "traceparent")
  match next_trace_header {
    Some(value) => {
      let expected = "00-" + trace_id + "-" + consumer_span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 4: Message with baggage
  let baggage = Baggage::new()
  let baggage_with_correlation = Baggage::set_entry(baggage, "correlation.id", "corr123")
  let baggage_with_priority = Baggage::set_entry(baggage_with_correlation, "message.priority", "high")
  
  let message_with_baggage = Message::new("priority.events", "{ \"event\": \"high_priority\" }", [])
  let traced_message_with_baggage = MessagePropagator::inject_context_with_baggage(
    message_with_baggage, 
    span_ctx, 
    baggage_with_priority
  )
  
  // Extract and verify baggage
  let extracted_baggage = MessagePropagator::extract_baggage(traced_message_with_baggage)
  
  let correlation_id = Baggage::get_entry(extracted_baggage, "correlation.id")
  match correlation_id {
    Some(value) => assert_eq(value, "corr123")
    None => assert_true(false)
  }
  
  let message_priority = Baggage::get_entry(extracted_baggage, "message.priority")
  match message_priority {
    Some(value) => assert_eq(value, "high")
    None => assert_true(false)
  }
}

// Test 4: gRPC Metadata Trace Context Propagation
test "grpc metadata trace context propagation" {
  // Test case 1: Client injects trace context into gRPC metadata
  let trace_id = "grpc_trace_98765"
  let span_id = "grpc_span_43210"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "grpc_test_state")
  
  let initial_metadata = [
    ("authorization", "Bearer grpc_token"),
    ("user-agent", "grpc-client/1.0.0")
  ]
  
  let call_metadata = GrpcPropagator::inject_context(initial_metadata, span_ctx)
  
  // Verify trace context is injected into metadata
  let traceparent_meta = GrpcPropagator::get_metadata(call_metadata, "traceparent")
  match traceparent_meta {
    Some(value) => {
      let expected = "00-" + trace_id + "-" + span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 2: Server extracts trace context from gRPC metadata
  let server_span_ctx = GrpcPropagator::extract_context(call_metadata)
  
  // Verify extracted context
  assert_eq(SpanContext::trace_id(server_span_ctx), trace_id)
  assert_eq(SpanContext::span_id(server_span_ctx), span_id)
  assert_true(SpanContext::is_sampled(server_span_ctx))
  
  // Test case 3: Server creates child span for processing
  let server_span_id = "server_span_56789"
  let server_span = SpanContext::new(trace_id, server_span_id, true, "server_processing_state")
  
  // Server makes downstream gRPC call
  let downstream_metadata = [
    ("deadline", "2023-01-01T00:00:00Z"),
    ("timeout", "30s")
  ]
  
  let downstream_call_metadata = GrpcPropagator::inject_context(downstream_metadata, server_span)
  
  // Verify downstream call has correct trace context
  let downstream_traceparent = GrpcPropagator::get_metadata(downstream_call_metadata, "traceparent")
  match downstream_traceparent {
    Some(value) => {
      let expected = "00-" + trace_id + "-" + server_span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 4: gRPC with binary metadata
  let binary_data = [0x01, 0x02, 0x03, 0x04]
  let binary_metadata = GrpcPropagator::create_binary_metadata("binary-data", binary_data)
  
  let mixed_metadata = Array::append(call_metadata, binary_metadata)
  let mixed_metadata_with_trace = GrpcPropagator::inject_context(mixed_metadata, span_ctx)
  
  // Verify binary metadata is preserved
  let preserved_binary = GrpcPropagator::get_binary_metadata(mixed_metadata_with_trace, "binary-data")
  match preserved_binary {
    Some(data) => assert_eq(data, binary_data)
    None => assert_true(false)
  }
  
  // Verify trace context is still present
  let mixed_traceparent = GrpcPropagator::get_metadata(mixed_metadata_with_trace, "traceparent")
  match mixed_traceparent {
    Some(value) => {
      let expected = "00-" + trace_id + "-" + span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
}

// Test 5: Cross-Protocol Trace Context Propagation
test "cross-protocol trace context propagation" {
  // Test case 1: HTTP to gRPC propagation
  let http_trace_id = "cross_protocol_trace_001"
  let http_span_id = "cross_protocol_span_001"
  let http_span_ctx = SpanContext::new(http_trace_id, http_span_id, true, "http_to_grpc")
  
  // HTTP request with trace context
  let http_headers = [
    ("traceparent", "00-" + http_trace_id + "-" + http_span_id + "-01"),
    ("tracestate", "rojo=00f067aa0ba902b7"),
    ("Content-Type", "application/json")
  ]
  
  let http_request = HttpRequest::new("POST", "/api/process", http_headers, Some("request data"))
  let extracted_http_ctx = HttpPropagator::extract_context(http_request)
  
  // Convert to gRPC metadata
  let grpc_metadata = GrpcPropagator::convert_from_http_context(extracted_http_ctx)
  let grpc_traceparent = GrpcPropagator::get_metadata(grpc_metadata, "traceparent")
  match grpc_traceparent {
    Some(value) => {
      let expected = "00-" + http_trace_id + "-" + http_span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 2: gRPC to Message Queue propagation
  let grpc_trace_id = "cross_protocol_trace_002"
  let grpc_span_id = "cross_protocol_span_002"
  let grpc_span_ctx = SpanContext::new(grpc_trace_id, grpc_span_id, true, "grpc_to_mq")
  
  let grpc_metadata = [
    ("traceparent", "00-" + grpc_trace_id + "-" + grpc_span_id + "-01"),
    ("authorization", "Bearer grpc_token")
  ]
  
  let extracted_grpc_ctx = GrpcPropagator::extract_context(grpc_metadata)
  
  // Convert to message headers
  let message_headers = MessagePropagator::convert_from_grpc_context(extracted_grpc_ctx)
  let message = Message::new("test.topic", "test payload", message_headers)
  let message_trace_header = MessagePropagator::get_header(Message::headers(message), "traceparent")
  match message_trace_header {
    Some(value) => {
      let expected = "00-" + grpc_trace_id + "-" + grpc_span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 3: Message Queue to HTTP propagation
  let mq_trace_id = "cross_protocol_trace_003"
  let mq_span_id = "cross_protocol_span_003"
  let mq_span_ctx = SpanContext::new(mq_trace_id, mq_span_id, true, "mq_to_http")
  
  let mq_headers = [
    ("traceparent", "00-" + mq_trace_id + "-" + mq_span_id + "-01"),
    ("Content-Type", "application/json")
  ]
  
  let mq_message = Message::new("events.topic", "event data", mq_headers)
  let extracted_mq_ctx = MessagePropagator::extract_context(mq_message)
  
  // Convert to HTTP headers
  let http_headers_from_mq = HttpPropagator::convert_from_message_context(extracted_mq_ctx)
  let http_request_from_mq = HttpRequest::new("POST", "/api/events", http_headers_from_mq, Some("event data"))
  let http_trace_header = HttpPropagator::get_header(HttpRequest::headers(http_request_from_mq), "traceparent")
  match http_trace_header {
    Some(value) => {
      let expected = "00-" + mq_trace_id + "-" + mq_span_id + "-01"
      assert_eq(value, expected)
    }
    None => assert_true(false)
  }
  
  // Test case 4: Baggage propagation across protocols
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "user456")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "session789")
  
  // Start with HTTP
  let http_request_with_baggage = HttpRequest::new("GET", "/api/data", [], None)
  let http_with_baggage = HttpPropagator::inject_context_with_baggage(
    http_request_with_baggage, 
    http_span_ctx, 
    baggage_with_session
  )
  
  // Extract baggage from HTTP
  let http_extracted_baggage = HttpPropagator::extract_baggage(http_with_baggage)
  
  // Convert to gRPC
  let grpc_with_baggage = GrpcPropagator::convert_from_http_context_with_baggage(
    http_span_ctx, 
    http_extracted_baggage
  )
  
  // Extract baggage from gRPC
  let grpc_extracted_baggage = GrpcPropagator::extract_baggage(grpc_with_baggage)
  
  // Convert to Message Queue
  let message_with_baggage = MessagePropagator::convert_from_grpc_context_with_baggage(
    http_span_ctx, 
    grpc_extracted_baggage
  )
  
  // Extract baggage from Message Queue
  let mq_extracted_baggage = MessagePropagator::extract_baggage(message_with_baggage)
  
  // Verify baggage is preserved across all protocols
  let final_user_id = Baggage::get_entry(mq_extracted_baggage, "user.id")
  match final_user_id {
    Some(value) => assert_eq(value, "user456")
    None => assert_true(false)
  }
  
  let final_session_id = Baggage::get_entry(mq_extracted_baggage, "session.id")
  match final_session_id {
    Some(value) => assert_eq(value, "session789")
    None => assert_true(false)
  }
}