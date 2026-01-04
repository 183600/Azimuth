// Azimuth Telemetry System - Cross-Service Context Propagation Boundary Tests
// This file contains comprehensive test cases for cross-service context propagation boundaries

// Test 1: Context Propagation Across Service Boundaries
test "context propagation across service boundaries" {
  // Initialize tracing providers for different services
  let service_a_provider = TracerProvider::new()
  let service_b_provider = TracerProvider::new()
  let service_c_provider = TracerProvider::new()
  
  let service_a_tracer = TracerProvider::get_tracer(service_a_provider, "service_a")
  let service_b_tracer = TracerProvider::get_tracer(service_b_provider, "service_b")
  let service_c_tracer = TracerProvider::get_tracer(service_c_provider, "service_c")
  
  // Create root span in Service A
  let service_a_root = Tracer::start_span(service_a_tracer, "service_a.root_operation")
  let service_a_ctx = Span::span_context(service_a_root)
  
  // Add baggage in Service A
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_tenant = Baggage::set_entry(baggage_with_user, "tenant.id", "tenant-abc")
  let baggage_with_region = Baggage::set_entry(baggage_with_tenant, "region", "us-west-2")
  
  // Service A calls Service B
  let service_a_to_b_headers = Headers::new()
  let propagator = TraceContextPropagator::new()
  
  // Inject context from Service A to Service B
  let ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    baggage_with_region
  )
  Propagator::inject(propagator, service_a_ctx, service_a_to_b_headers)
  
  // Service B extracts context
  let service_b_ctx = Propagator::extract(propagator, service_a_to_b_headers)
  let service_b_span = Tracer::start_span_with_context(
    service_b_tracer,
    "service_b.process_request",
    Some(service_b_ctx),
    Some(Server)
  )
  
  // Verify context propagation from Service A to Service B
  assert_eq(SpanContext::trace_id(service_b_ctx), SpanContext::trace_id(service_a_ctx))
  assert_eq(SpanContext::span_id(service_b_ctx), SpanContext::span_id(service_a_ctx))
  assert_eq(SpanContext::is_sampled(service_b_ctx), SpanContext::is_sampled(service_a_ctx))
  
  // Service B adds more baggage
  let service_b_baggage = Context::get(ctx_with_baggage, ContextKey::new("baggage"))
  let service_b_baggage_with_service = match service_b_baggage {
    Some(baggage) => Baggage::set_entry(baggage, "service.b.version", "2.1.0")
    None => Baggage::new()
  }
  
  // Service B calls Service C
  let service_b_to_c_headers = Headers::new()
  let service_b_ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    service_b_baggage_with_service
  )
  Propagator::inject(propagator, service_b_ctx, service_b_to_c_headers)
  
  // Service C extracts context
  let service_c_ctx = Propagator::extract(propagator, service_b_to_c_headers)
  let service_c_span = Tracer::start_span_with_context(
    service_c_tracer,
    "service_c.process_request",
    Some(service_c_ctx),
    Some(Server)
  )
  
  // Verify context propagation from Service B to Service C
  assert_eq(SpanContext::trace_id(service_c_ctx), SpanContext::trace_id(service_b_ctx))
  assert_eq(SpanContext::span_id(service_c_ctx), SpanContext::span_id(service_b_ctx))
  assert_eq(SpanContext::is_sampled(service_c_ctx), SpanContext::is_sampled(service_b_ctx))
  
  // Verify all services have the same trace ID
  assert_eq(SpanContext::trace_id(service_a_ctx), SpanContext::trace_id(service_b_ctx))
  assert_eq(SpanContext::trace_id(service_b_ctx), SpanContext::trace_id(service_c_ctx))
  
  // End spans
  Span::end(service_c_span)
  Span::end(service_b_span)
  Span::end(service_a_root)
}

// Test 2: Context Propagation Across Protocol Boundaries
test "context propagation across protocol boundaries" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "protocol_tracer")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "cross_protocol_operation")
  let root_ctx = Span::span_context(root_span)
  
  // Add baggage
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "request.id", "req-12345")
  let baggage_with_user = Baggage::set_entry(baggage_with_data, "user.id", "user-67890")
  
  let ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    baggage_with_user
  )
  
  // Test HTTP protocol propagation
  let http_headers = Headers::new()
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, root_ctx, http_headers)
  
  // Simulate HTTP request/response
  let http_request = HttpRequest::new(
    "GET",
    "https://api.example.com/data",
    http_headers.to_array(),
    Some("request body")
  )
  
  // Extract context from HTTP request
  let http_response_headers = HttpRequest::headers(http_request)
  let http_extracted_ctx = Propagator::extract(propagator, http_response_headers)
  
  // Verify HTTP context propagation
  assert_eq(SpanContext::trace_id(http_extracted_ctx), SpanContext::trace_id(root_ctx))
  assert_eq(SpanContext::span_id(http_extracted_ctx), SpanContext::span_id(root_ctx))
  assert_eq(SpanContext::is_sampled(http_extracted_ctx), SpanContext::is_sampled(root_ctx))
  
  // Test gRPC protocol propagation
  let grpc_metadata = Metadata::new()
  Propagator::inject(propagator, root_ctx, grpc_metadata)
  
  // Simulate gRPC call
  let grpc_request = GrpcRequest::new(
    "GetData",
    grpc_metadata,
    Some("grpc request data")
  )
  
  // Extract context from gRPC metadata
  let grpc_response_metadata = GrpcRequest::metadata(grpc_request)
  let grpc_extracted_ctx = Propagator::extract(propagator, grpc_response_metadata)
  
  // Verify gRPC context propagation
  assert_eq(SpanContext::trace_id(grpc_extracted_ctx), SpanContext::trace_id(root_ctx))
  assert_eq(SpanContext::span_id(grpc_extracted_ctx), SpanContext::span_id(root_ctx))
  assert_eq(SpanContext::is_sampled(grpc_extracted_ctx), SpanContext::is_sampled(root_ctx))
  
  // Test message queue protocol propagation
  let message_headers = MessageHeaders::new()
  Propagator::inject(propagator, root_ctx, message_headers)
  
  // Simulate message queue message
  let queue_message = QueueMessage::new(
    "processing.task",
    message_headers,
    Some("message payload")
  )
  
  // Extract context from message
  let message_headers_from_queue = QueueMessage::headers(queue_message)
  let message_extracted_ctx = Propagator::extract(propagator, message_headers_from_queue)
  
  // Verify message queue context propagation
  assert_eq(SpanContext::trace_id(message_extracted_ctx), SpanContext::trace_id(root_ctx))
  assert_eq(SpanContext::span_id(message_extracted_ctx), SpanContext::span_id(root_ctx))
  assert_eq(SpanContext::is_sampled(message_extracted_ctx), SpanContext::is_sampled(root_ctx))
  
  Span::end(root_span)
}

// Test 3: Context Propagation Across Network Boundaries
test "context propagation across network boundaries" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "network_tracer")
  
  // Create root span in source network
  let source_span = Tracer::start_span(tracer, "cross_network_operation")
  let source_ctx = Span::span_context(source_span)
  
  // Add network-specific baggage
  let baggage = Baggage::new()
  let baggage_with_network = Baggage::set_entry(baggage, "source.network", "network-a")
  let baggage_with_datacenter = Baggage::set_entry(baggage_with_network, "source.datacenter", "dc-west")
  
  let ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    baggage_with_datacenter
  )
  
  // Simulate cross-network call through load balancer
  let load_balancer_headers = Headers::new()
  let propagator = TraceContextPropagator::new()
  Propagator::inject(propagator, source_ctx, load_balancer_headers)
  
  // Load balancer adds its own headers
  Headers::set(load_balancer_headers, "X-Load-Balancer-ID", "lb-001")
  Headers::set(load_balancer_headers, "X-Forwarded-For", "10.0.0.1")
  
  // Extract context in destination network
  let destination_ctx = Propagator::extract(propagator, load_balancer_headers)
  let destination_span = Tracer::start_span_with_context(
    tracer,
    "destination_network_operation",
    Some(destination_ctx),
    Some(Server)
  )
  
  // Verify context propagation across networks
  assert_eq(SpanContext::trace_id(destination_ctx), SpanContext::trace_id(source_ctx))
  assert_eq(SpanContext::span_id(destination_ctx), SpanContext::span_id(source_ctx))
  assert_eq(SpanContext::is_sampled(destination_ctx), SpanContext::is_sampled(source_ctx))
  
  // Destination network adds its own baggage
  let destination_baggage = Context::get(ctx_with_baggage, ContextKey::new("baggage"))
  let destination_baggage_with_network = match destination_baggage {
    Some(baggage) => Baggage::set_entry(baggage, "destination.network", "network-b")
    None => Baggage::new()
  }
  
  // Response back through load balancer
  let response_headers = Headers::new()
  let destination_ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    destination_baggage_with_network
  )
  Propagator::inject(propagator, destination_ctx, response_headers)
  
  // Load balancer adds response headers
  Headers::set(response_headers, "X-Response-Time", "150ms")
  
  // Extract context back in source network
  let response_ctx = Propagator::extract(propagator, response_headers)
  
  // Verify round-trip context propagation
  assert_eq(SpanContext::trace_id(response_ctx), SpanContext::trace_id(source_ctx))
  assert_eq(SpanContext::span_id(response_ctx), SpanContext::span_id(source_ctx))
  
  Span::end(destination_span)
  Span::end(source_span)
}

// Test 4: Context Propagation With Security Boundaries
test "context propagation with security boundaries" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "security_tracer")
  
  // Create root span in trusted zone
  let trusted_span = Tracer::start_span(tracer, "trusted_zone_operation")
  let trusted_ctx = Span::span_context(trusted_span)
  
  // Add sensitive baggage
  let baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(baggage, "user.id", "12345")
  let baggage_with_token = Baggage::set_entry(baggage_with_user, "auth.token", "token-abc123")
  let baggage_with_permissions = Baggage::set_entry(baggage_with_token, "user.permissions", "read,write")
  
  let ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    baggage_with_permissions
  )
  
  // Configure security-aware propagator
  let security_config = SecurityPropagationConfig::new(
    sanitize_sensitive_data: true,
    allowed_baggage_keys: ["user.id", "request.id", "session.id"],
    denied_baggage_keys: ["auth.token", "user.permissions", "api.key"]
  )
  
  let security_propagator = SecurityAwarePropagator::new(security_config)
  
  // Propagate across security boundary
  let boundary_headers = Headers::new()
  SecurityAwarePropagator::inject(security_propagator, trusted_ctx, boundary_headers)
  
  // Verify sensitive data is not in headers
  let traceparent = Headers::get(boundary_headers, "traceparent")
  match traceparent {
    Some(_) => assert_true(true) // traceparent should be present
    None => assert_true(false)
  }
  
  let tracestate = Headers::get(boundary_headers, "tracestate")
  match tracestate {
    Some(state) => {
      // Verify sensitive baggage is not present
      assert_false(state.contains("auth.token"))
      assert_false(state.contains("user.permissions"))
    }
    None => assert_true(true) // tracestate might not be present
  }
  
  // Extract context in untrusted zone
  let untrusted_ctx = SecurityAwarePropagator::extract(security_propagator, boundary_headers)
  let untrusted_span = Tracer::start_span_with_context(
    tracer,
    "untrusted_zone_operation",
    Some(untrusted_ctx),
    Some(Server)
  )
  
  // Verify trace context is preserved
  assert_eq(SpanContext::trace_id(untrusted_ctx), SpanContext::trace_id(trusted_ctx))
  assert_eq(SpanContext::span_id(untrusted_ctx), SpanContext::span_id(trusted_ctx))
  
  // Untrusted zone adds non-sensitive baggage
  let untrusted_baggage = Baggage::new()
  let untrusted_baggage_with_operation = Baggage::set_entry(
    untrusted_baggage,
    "operation.type",
    "data_processing"
  )
  
  // Response back to trusted zone
  let response_headers = Headers::new()
  let untrusted_ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    untrusted_baggage_with_operation
  )
  SecurityAwarePropagator::inject(security_propagator, untrusted_ctx, response_headers)
  
  // Extract context back in trusted zone
  let trusted_response_ctx = SecurityAwarePropagator::extract(security_propagator, response_headers)
  
  // Verify round-trip context propagation
  assert_eq(SpanContext::trace_id(trusted_response_ctx), SpanContext::trace_id(trusted_ctx))
  assert_eq(SpanContext::span_id(trusted_response_ctx), SpanContext::span_id(trusted_ctx))
  
  Span::end(untrusted_span)
  Span::end(trusted_span)
}

// Test 5: Context Propagation With Format Boundaries
test "context propagation with format boundaries" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "format_tracer")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "cross_format_operation")
  let root_ctx = Span::span_context(root_span)
  
  // Add baggage
  let baggage = Baggage::new()
  let baggage_with_data = Baggage::set_entry(baggage, "operation.id", "op-12345")
  let baggage_with_format = Baggage::set_entry(baggage_with_data, "source.format", "json")
  
  let ctx_with_baggage = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    baggage_with_format
  )
  
  // Test JSON format propagation
  let json_propagator = JsonPropagator::new()
  let json_context = json_propagator.serialize_context(root_ctx, ctx_with_baggage)
  
  // Verify JSON context contains trace information
  assert_true(json_context.contains("trace_id"))
  assert_true(json_context.contains("span_id"))
  assert_true(json_context.contains("baggage"))
  
  // Extract context from JSON
  let json_extracted_ctx = json_propagator.deserialize_context(json_context)
  assert_eq(SpanContext::trace_id(json_extracted_ctx), SpanContext::trace_id(root_ctx))
  assert_eq(SpanContext::span_id(json_extracted_ctx), SpanContext::span_id(root_ctx))
  
  // Test XML format propagation
  let xml_propagator = XmlPropagator::new()
  let xml_context = xml_propagator.serialize_context(root_ctx, ctx_with_baggage)
  
  // Verify XML context contains trace information
  assert_true(xml_context.contains("<trace_id>"))
  assert_true(xml_context.contains("<span_id>"))
  assert_true(xml_context.contains("<baggage>"))
  
  // Extract context from XML
  let xml_extracted_ctx = xml_propagator.deserialize_context(xml_context)
  assert_eq(SpanContext::trace_id(xml_extracted_ctx), SpanContext::trace_id(root_ctx))
  assert_eq(SpanContext::span_id(xml_extracted_ctx), SpanContext::span_id(root_ctx))
  
  // Test binary format propagation
  let binary_propagator = BinaryPropagator::new()
  let binary_context = binary_propagator.serialize_context(root_ctx, ctx_with_baggage)
  
  // Extract context from binary
  let binary_extracted_ctx = binary_propagator.deserialize_context(binary_context)
  assert_eq(SpanContext::trace_id(binary_extracted_ctx), SpanContext::trace_id(root_ctx))
  assert_eq(SpanContext::span_id(binary_extracted_ctx), SpanContext::span_id(root_ctx))
  
  // Test format conversion
  let converted_json = binary_propagator.convert_to_json(binary_context)
  let converted_ctx = json_propagator.deserialize_context(converted_json)
  assert_eq(SpanContext::trace_id(converted_ctx), SpanContext::trace_id(root_ctx))
  
  Span::end(root_span)
}

// Test 6: Context Propagation Failure At Boundaries
test "context propagation failure at boundaries" {
  let tracer_provider = TracerProvider::new()
  let tracer = TracerProvider::get_tracer(tracer_provider, "failure_tracer")
  
  // Create root span
  let root_span = Tracer::start_span(tracer, "boundary_failure_operation")
  let root_ctx = Span::span_context(root_span)
  
  // Test malformed headers
  let malformed_headers = Headers::new()
  Headers::set(malformed_headers, "traceparent", "invalid-format")
  Headers::set(malformed_headers, "tracestate", "malformed-data")
  
  let propagator = TraceContextPropagator::new()
  
  // Try to extract context from malformed headers
  let malformed_ctx = Propagator::extract(propagator, malformed_headers)
  
  // Verify fallback behavior
  assert_false(SpanContext::is_valid(malformed_ctx))
  
  // Create new context with fallback
  let fallback_span = Tracer::start_span_with_context(
    tracer,
    "fallback_operation",
    Some(malformed_ctx),
    Some(Internal)
  )
  
  // Verify fallback span has valid context
  let fallback_ctx = Span::span_context(fallback_span)
  assert_true(SpanContext::is_valid(fallback_ctx))
  
  // Test missing headers
  let empty_headers = Headers::new()
  let empty_ctx = Propagator::extract(propagator, empty_headers)
  
  assert_false(SpanContext::is_valid(empty_ctx))
  
  // Test oversized baggage
  let large_baggage = Baggage::new()
  let oversized_baggage = Baggage::set_entry(
    large_baggage,
    "large.data",
    "x".repeat(10000) // Very large baggage entry
  )
  
  let oversized_headers = Headers::new()
  let ctx_with_oversized = Context::with_value(
    Context::root(),
    ContextKey::new("baggage"),
    oversized_baggage
  )
  
  // Try to inject oversized baggage
  Propagator::inject(propagator, root_ctx, oversized_headers)
  
  // Verify headers are not oversized
  let tracestate = Headers::get(oversized_headers, "tracestate")
  match tracestate {
    Some(state) => {
      assert_true(state.length() < 8192) // Should be under 8KB limit
    }
    None => assert_true(true)
  }
  
  // Test context corruption
  let corrupted_headers = Headers::new()
  Headers::set(corrupted_headers, "traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")
  Headers::set(corrupted_headers, "tracestate", "key1=value1,key2=value2")
  
  // Simulate header corruption
  let headers_array = corrupted_headers.to_array()
  headers_array[1] = ("traceparent", "00-corrupted-data-corrupted-data-corrupted-data-01")
  
  let corrupted_headers_fixed = Headers::from_array(headers_array)
  let corrupted_ctx = Propagator::extract(propagator, corrupted_headers_fixed)
  
  // Verify corruption handling
  assert_false(SpanContext::is_valid(corrupted_ctx))
  
  Span::end(fallback_span)
  Span::end(root_span)
}