// Azimuth Cross-Service Telemetry Propagation Tests
// This file contains comprehensive test cases for telemetry propagation across services

// Test 1: HTTP Header Context Propagation
test "http header context propagation" {
  let http_propagator = HTTPHeaderPropagator::new()
  
  // Create source context
  let source_context = Context::with_values([
    ("trace_id", "0af7651916cd43dd8448eb211c80319c"),
    ("span_id", "b7ad6b7169203331"),
    ("sampled", "true"),
    ("baggage.user_id", "user123"),
    ("baggage.request_id", "req456")
  ])
  
  // Inject context into HTTP headers
  let headers = Headers::new()
  HTTPHeaderPropagator::inject(http_propagator, source_context, headers)
  
  // Verify headers are properly set
  assert_true(headers.contains("traceparent"))
  assert_true(headers.contains("baggage"))
  
  let traceparent = headers.get("traceparent")
  assert_true(traceparent.contains("0af7651916cd43dd8448eb211c80319c"))
  assert_true(traceparent.contains("b7ad6b7169203331"))
  
  let baggage = headers.get("baggage")
  assert_true(baggage.contains("user_id=user123"))
  assert_true(baggage.contains("request_id=req456"))
  
  // Extract context from HTTP headers
  let extracted_context = HTTPHeaderPropagator::extract(http_propagator, headers)
  
  // Verify extracted context matches source
  assert_eq(
    Context::get_value(extracted_context, "trace_id"),
    Some("0af7651916cd43dd8448eb211c80319c")
  )
  
  assert_eq(
    Context::get_value(extracted_context, "span_id"),
    Some("b7ad6b7169203331")
  )
  
  assert_eq(
    Context::get_value(extracted_context, "sampled"),
    Some("true")
  )
  
  assert_eq(
    Context::get_value(extracted_context, "baggage.user_id"),
    Some("user123")
  )
  
  assert_eq(
    Context::get_value(extracted_context, "baggage.request_id"),
    Some("req456")
  )
}

// Test 2: Multi-Service Trace Chain
test "multi-service trace chain" {
  let trace_chain = TraceChain::new()
  
  // Service 1: API Gateway
  let gateway_span = TraceChain::create_span(trace_chain, "api-gateway", "handle_request")
  TraceChain::add_attribute(trace_chain, gateway_span, "http.method", "GET")
  TraceChain::add_attribute(trace_chain, gateway_span, "http.url", "/api/users")
  TraceChain::add_event(trace_chain, gateway_span, "request_received")
  
  // Service 2: User Service
  let user_service_span = TraceChain::create_child_span(trace_chain, gateway_span, "user-service", "get_user")
  TraceChain::add_attribute(trace_chain, user_service_span, "user.id", "12345")
  TraceChain::add_event(trace_chain, user_service_span, "database_query_start")
  
  // Service 3: Database
  let db_span = TraceChain::create_child_span(trace_chain, user_service_span, "database", "query")
  TraceChain::add_attribute(trace_chain, db_span, "db.statement", "SELECT * FROM users WHERE id = ?")
  TraceChain::add_event(trace_chain, db_span, "query_executed")
  TraceChain::end_span(trace_chain, db_span)
  
  // Continue user service
  TraceChain::add_event(trace_chain, user_service_span, "database_query_complete")
  TraceChain::end_span(trace_chain, user_service_span)
  
  // Service 4: Auth Service (parallel call)
  let auth_service_span = TraceChain::create_child_span(trace_chain, gateway_span, "auth-service", "validate_token")
  TraceChain::add_attribute(trace_chain, auth_service_span, "auth.token", "jwt_token_123")
  TraceChain::add_event(trace_chain, auth_service_span, "token_validated")
  TraceChain::end_span(trace_chain, auth_service_span)
  
  // Complete gateway span
  TraceChain::add_event(trace_chain, gateway_span, "response_sent")
  TraceChain::end_span(trace_chain, gateway_span)
  
  // Verify trace chain integrity
  let all_spans = TraceChain::get_all_spans(trace_chain)
  assert_eq(all_spans.length(), 4)
  
  // Verify parent-child relationships
  assert_eq(TraceChain::get_parent_id(trace_chain, user_service_span), TraceChain::get_span_id(gateway_span))
  assert_eq(TraceChain::get_parent_id(trace_chain, db_span), TraceChain::get_span_id(user_service_span))
  assert_eq(TraceChain::get_parent_id(trace_chain, auth_service_span), TraceChain::get_span_id(gateway_span))
  
  // Verify trace ID consistency
  let trace_id = TraceChain::get_trace_id(trace_chain)
  for span in all_spans {
    assert_eq(TraceChain::get_span_trace_id(span), trace_id)
  }
  
  // Verify service topology
  let topology = TraceChain::build_topology(trace_chain)
  assert_true(topology.has_service("api-gateway"))
  assert_true(topology.has_service("user-service"))
  assert_true(topology.has_service("database"))
  assert_true(topology.has_service("auth-service"))
  
  // Verify service dependencies
  assert_true(topology.has_dependency("api-gateway", "user-service"))
  assert_true(topology.has_dependency("user-service", "database"))
  assert_true(topology.has_dependency("api-gateway", "auth-service"))
}

// Test 3: Cross-Protocol Propagation
test "cross-protocol propagation" {
  let cross_protocol_propagator = CrossProtocolPropagator::new()
  
  // Create source context
  let source_context = Context::with_values([
    ("trace_id", "cross_protocol_trace"),
    ("span_id", "cross_protocol_span"),
    ("sampled", "true"),
    ("baggage.correlation_id", "corr123")
  ])
  
  // HTTP to gRPC propagation
  let http_headers = Headers::new()
  CrossProtocolPropagator::inject_http(cross_protocol_propagator, source_context, http_headers)
  
  let grpc_metadata = CrossProtocolPropagator::http_to_grpc(cross_protocol_propagator, http_headers)
  
  let grpc_context = CrossProtocolPropagator::extract_grpc(cross_protocol_propagator, grpc_metadata)
  
  assert_eq(
    Context::get_value(grpc_context, "trace_id"),
    Some("cross_protocol_trace")
  )
  
  assert_eq(
    Context::get_value(grpc_context, "baggage.correlation_id"),
    Some("corr123")
  )
  
  // gRPC to Message Queue propagation
  let message_headers = MessageHeaders::new()
  CrossProtocolPropagator::inject_grpc(cross_protocol_propagator, grpc_context, message_headers)
  
  let message_context = CrossProtocolPropagator::extract_message(cross_protocol_propagator, message_headers)
  
  assert_eq(
    Context::get_value(message_context, "trace_id"),
    Some("cross_protocol_trace")
  )
  
  // Message Queue back to HTTP
  let new_http_headers = Headers::new()
  CrossProtocolPropagator::inject_message(cross_protocol_propagator, message_context, new_http_headers)
  
  let final_context = CrossProtocolPropagator::extract_http(cross_protocol_propagator, new_http_headers)
  
  assert_eq(
    Context::get_value(final_context, "trace_id"),
    Some("cross_protocol_trace")
  )
  
  assert_eq(
    Context::get_value(final_context, "baggage.correlation_id"),
    Some("corr123")
  )
}

// Test 4: Distributed Baggage Propagation
test "distributed baggage propagation" {
  let baggage_propagator = DistributedBaggagePropagator::new()
  
  // Create initial baggage
  let initial_baggage = Baggage::new()
  Baggage::set_entry(initial_baggage, "user_id", "user123")
  Baggage::set_entry(initial_baggage, "session_id", "session456")
  Baggage::set_entry(initial_baggage, "request_id", "req789")
  Baggage::set_entry(initial_baggage, "tenant_id", "tenant001")
  
  // Service 1: Add baggage entries
  let service1_baggage = Baggage::copy(initial_baggage)
  Baggage::set_entry(service1_baggage, "service1.timestamp", "1640995200")
  Baggage::set_entry(service1_baggage, "service1.operation", "authenticate")
  
  // Service 2: Modify and add baggage entries
  let service2_baggage = Baggage::copy(service1_baggage)
  Baggage::set_entry(service2_baggage, "user_role", "admin")
  Baggage::set_entry(service2_baggage, "service2.cache_hit", "true")
  
  // Service 3: Add more baggage entries
  let service3_baggage = Baggage::copy(service2_baggage)
  Baggage::set_entry(service3_baggage, "service3.database", "primary")
  Baggage::set_entry(service3_baggage, "service3.query_time", "25ms")
  
  // Verify baggage propagation
  assert_eq(Baggage::get_entry(service3_baggage, "user_id"), Some("user123"))
  assert_eq(Baggage::get_entry(service3_baggage, "session_id"), Some("session456"))
  assert_eq(Baggage::get_entry(service3_baggage, "request_id"), Some("req789"))
  assert_eq(Baggage::get_entry(service3_baggage, "tenant_id"), Some("tenant001"))
  assert_eq(Baggage::get_entry(service3_baggage, "service1.timestamp"), Some("1640995200"))
  assert_eq(Baggage::get_entry(service3_baggage, "service1.operation"), Some("authenticate"))
  assert_eq(Baggage::get_entry(service3_baggage, "user_role"), Some("admin"))
  assert_eq(Baggage::get_entry(service3_baggage, "service2.cache_hit"), Some("true"))
  assert_eq(Baggage::get_entry(service3_baggage, "service3.database"), Some("primary"))
  assert_eq(Baggage::get_entry(service3_baggage, "service3.query_time"), Some("25ms"))
  
  // Test baggage size limits
  let large_baggage = Baggage::new()
  for i in 0..=200 {
    Baggage::set_entry(large_baggage, "key_" + i.to_string(), "value_" + i.to_string())
  }
  
  // Should enforce baggage size limits
  let truncated_baggage = DistributedBaggagePropagator::enforce_limits(baggage_propagator, large_baggage, 100)
  let baggage_entries = Baggage::get_all_entries(truncated_baggage)
  assert_true(baggage_entries.length() <= 100)
  
  // Test baggage entry size limits
  let very_long_value = "x".repeat(5000)
  Baggage::set_entry(large_baggage, "long_key", very_long_value)
  
  let size_limited_baggage = DistributedBaggagePropagator::enforce_entry_limits(baggage_propagator, large_baggage, 1000)
  let long_value = Baggage::get_entry(size_limited_baggage, "long_key")
  match long_value {
    Some(value) => assert_true(value.length() <= 1000),
    None => assert_true(true) // Entry might be removed if too large
  }
}

// Test 5: Cross-Service Sampling Decisions
test "cross-service sampling decisions" {
  let sampling_manager = CrossServiceSamplingManager::new()
  
  // Configure sampling strategies
  SamplingManager::set_strategy(sampling_manager, "trace_id", TraceIdSampling::new(0.1)) // 10% sampling
  SamplingManager::set_strategy(sampling_manager, "service_name", ServiceNameSampling::new([
    ("critical-service", 1.0), // 100% sampling
    ("normal-service", 0.2),   // 20% sampling
    ("debug-service", 0.5)     // 50% sampling
  ]))
  
  SamplingManager::set_strategy(sampling_manager, "error_sampling", ErrorSampling::new(1.0)) // 100% for errors
  
  // Test sampling decisions
  let test_cases = [
    ("trace_1", "critical-service", false),
    ("trace_2", "normal-service", false),
    ("trace_3", "debug-service", false),
    ("trace_4", "normal-service", true),
    ("trace_5", "unknown-service", false)
  ]
  
  let sampling_decisions = []
  
  for (trace_id, service_name, has_error) in test_cases {
    let context = Context::with_values([
      ("trace_id", trace_id),
      ("service_name", service_name),
      ("has_error", if has_error { "true" } else { "false" })
    ])
    
    let decision = SamplingManager::should_sample(sampling_manager, context)
    sampling_decisions.push((trace_id, service_name, has_error, decision))
  }
  
  // Verify sampling decisions
  for (trace_id, service_name, has_error, decision) in sampling_decisions {
    match service_name {
      "critical-service" => assert_true(decision), // Should always be sampled
      "normal-service" => {
        if has_error {
          assert_true(decision) // Should be sampled due to error
        }
        // Otherwise depends on sampling probability
      }
      "debug-service" => {
        // 50% probability, either decision is acceptable
      }
      "unknown-service" => {
        // Uses default sampling strategy
      }
      _ => assert_true(false)
    }
  }
  
  // Test sampling consistency across services
  let trace_id = "consistency_test"
  let initial_context = Context::with_values([
    ("trace_id", trace_id),
    ("service_name", "first-service")
  ])
  
  let initial_decision = SamplingManager::should_sample(sampling_manager, initial_context)
  
  // Propagate sampling decision
  let propagated_context = Context::with_values([
    ("trace_id", trace_id),
    ("service_name", "second-service"),
    ("sampling_decision", if initial_decision { "true" } else { "false" })
  ])
  
  let propagated_decision = SamplingManager::should_sample(sampling_manager, propagated_context)
  
  // Sampling decision should be consistent across services
  assert_eq(initial_decision, propagated_decision)
}

// Test 6: Service Mesh Integration
test "service mesh integration" {
  let service_mesh_integrator = ServiceMeshIntegrator::new()
  
  // Configure service mesh
  ServiceMeshIntegrator::configure(service_mesh_integrator, ServiceMeshConfig::new()
    .with_mesh_type("istio")
    .with_propagation_format("b3")
    .with_metadata_enabled(true)
  )
  
  // Simulate service mesh request flow
  let mesh_context = ServiceMeshContext::new()
  
  // Ingress gateway
  ServiceMeshIntegrator::add_mesh_headers(service_mesh_integrator, mesh_context, [
    ("x-request-id", "req-123"),
    ("x-b3-traceid", "abc123"),
    ("x-b3-spanid", "def456"),
    ("x-b3-parentspanid", "parent789"),
    ("x-b3-sampled", "1")
  ])
  
  // Service 1
  let service1_context = ServiceMeshIntegrator::extract_context(service_mesh_integrator, mesh_context)
  ServiceMeshIntegrator::add_service_metadata(service_mesh_integrator, service1_context, [
    ("service.name", "service1"),
    ("service.version", "1.2.3"),
    ("service.instance.id", "instance-001")
  ])
  
  // Service 2 (called by Service 1)
  let service2_context = ServiceMeshIntegrator::create_child_context(service_mesh_integrator, service1_context)
  ServiceMeshIntegrator::add_service_metadata(service_mesh_integrator, service2_context, [
    ("service.name", "service2"),
    ("service.version", "2.1.0"),
    ("service.instance.id", "instance-002")
  ])
  
  // Verify context propagation
  assert_eq(
    ServiceMeshContext::get_value(service2_context, "x-b3-traceid"),
    Some("abc123")
  )
  
  assert_eq(
    ServiceMeshContext::get_value(service2_context, "x-b3-parentspanid"),
    Some("def456")
  )
  
  assert_eq(
    ServiceMeshContext::get_value(service2_context, "service.name"),
    Some("service2")
  )
  
  // Generate service mesh telemetry
  let mesh_telemetry = ServiceMeshIntegrator::generate_telemetry(service_mesh_integrator, mesh_context)
  
  assert_true(mesh_telemetry.contains("service_mesh"))
  assert_true(mesh_telemetry.contains("istio"))
  assert_true(mesh_telemetry.contains("service1"))
  assert_true(mesh_telemetry.contains("service2"))
  
  // Test service mesh metrics
  let mesh_metrics = ServiceMeshIntegrator::collect_metrics(service_mesh_integrator)
  
  assert_true(mesh_metrics.contains_key("request_count"))
  assert_true(mesh_metrics.contains_key("response_time"))
  assert_true(mesh_metrics.contains_key("error_rate"))
}

// Test 7: Async Message Context Propagation
test "async message context propagation" {
  let async_propagator = AsyncMessagePropagator::new()
  
  // Create source context
  let source_context = Context::with_values([
    ("trace_id", "async_trace_123"),
    ("span_id", "async_span_456"),
    ("message_id", "msg_789"),
    ("correlation_id", "corr_001"),
    ("conversation_id", "conv_002")
  ])
  
  // Message Queue 1: RabbitMQ
  let rabbitmq_message = RabbitMQMessage::new("user.created", source_context)
  RabbitMQMessage::add_header(rabbitmq_message, "priority", "high")
  RabbitMQMessage::add_header(rabbitmq_message, "ttl", "60000")
  
  // Process message in Service 1
  let service1_context = AsyncMessagePropagator::extract_from_rabbitmq(async_propagator, rabbitmq_message)
  AsyncMessagePropagator::add_correlation_data(async_propagator, service1_context, [
    ("processing_service", "user-service"),
    ("processing_start", "1640995200")
  ])
  
  // Generate new message for Service 2
  let kafka_message = AsyncMessagePropagator::create_kafka_message(async_propagator, service1_context, "user.profile.updated")
  KafkaMessage::add_header(kafka_message, "partition", "0")
  KafkaMessage::add_header(kafka_message, "offset", "12345")
  
  // Process message in Service 2
  let service2_context = AsyncMessagePropagator::extract_from_kafka(async_propagator, kafka_message)
  AsyncMessagePropagator::add_correlation_data(async_propagator, service2_context, [
    ("processing_service", "profile-service"),
    ("processing_start", "1640995210")
  ])
  
  // Generate response message
  let response_message = AsyncMessagePropagator::create_response_message(async_propagator, service2_context, "user.profile.update_result")
  
  // Verify context propagation across async messages
  assert_eq(
    AsyncMessageContext::get_value(response_message, "trace_id"),
    Some("async_trace_123")
  )
  
  assert_eq(
    AsyncMessageContext::get_value(response_message, "message_id"),
    Some("msg_789")
  )
  
  assert_eq(
    AsyncMessageContext::get_value(response_message, "correlation_id"),
    Some("corr_001")
  )
  
  assert_eq(
    AsyncMessageContext::get_value(response_message, "processing_service"),
    Some("profile-service")
  )
  
  // Test message chain tracking
  let message_chain = AsyncMessagePropagator::build_message_chain(async_propagator, response_message)
  
  assert_eq(message_chain.length(), 3) // Original + Service 1 + Service 2
  assert_eq(message_chain[0].message_type, "user.created")
  assert_eq(message_chain[1].message_type, "user.profile.updated")
  assert_eq(message_chain[2].message_type, "user.profile.update_result")
  
  // Verify message timing
  assert_eq(message_chain[0].processing_service, "user-service")
  assert_eq(message_chain[1].processing_service, "profile-service")
  assert_true(message_chain[1].processing_start > message_chain[0].processing_start)
}

// Test 8: Cross-Cloud Provider Propagation
test "cross-cloud provider propagation" {
  let cloud_propagator = CrossCloudPropagator::new()
  
  // Configure cloud provider contexts
  let aws_context = CloudContext::aws()
    .with_region("us-west-1")
    .with_account_id("123456789012")
    .with_function_name("process_lambda")
    .with_function_version("1.0")
  
  let gcp_context = CloudContext::gcp()
    .with_region("us-central1")
    .with_project_id("my-gcp-project")
    .with_service_name("process_cloud_run")
    .with_service_version("v1")
  
  let azure_context = CloudContext::azure()
    .with_region("eastus")
    .with_subscription_id("azure-sub-123")
    .with_function_app("process-function")
    .with_function_name="process"
  
  // Create unified trace context
  let unified_context = CloudPropagator::create_unified_context(cloud_propagator, [
    ("trace_id", "cross_cloud_trace"),
    ("span_id", "cross_cloud_span"),
    ("correlation_id", "cross_cloud_corr")
  ])
  
  // Propagate from AWS to GCP
  let aws_to_gcp_context = CloudPropagator::propagate_to_cloud(
    cloud_propagator,
    unified_context,
    aws_context,
    gcp_context
  )
  
  assert_eq(
    CloudContext::get_value(aws_to_gcp_context, "trace_id"),
    Some("cross_cloud_trace")
  )
  
  assert_eq(
    CloudContext::get_value(aws_to_gcp_context, "source_cloud"),
    Some("aws")
  )
  
  assert_eq(
    CloudContext::get_value(aws_to_gcp_context, "source_region"),
    Some("us-west-1")
  )
  
  assert_eq(
    CloudContext::get_value(aws_to_gcp_context, "target_cloud"),
    Some("gcp")
  )
  
  assert_eq(
    CloudContext::get_value(aws_to_gcp_context, "target_region"),
    Some("us-central1")
  )
  
  // Propagate from GCP to Azure
  let gcp_to_azure_context = CloudPropagator::propagate_to_cloud(
    cloud_propagator,
    aws_to_gcp_context,
    gcp_context,
    azure_context
  )
  
  assert_eq(
    CloudContext::get_value(gcp_to_azure_context, "trace_id"),
    Some("cross_cloud_trace")
  )
  
  assert_eq(
    CloudContext::get_value(gcp_to_azure_context, "source_cloud"),
    Some("gcp")
  )
  
  assert_eq(
    CloudContext::get_value(gcp_to_azure_context, "target_cloud"),
    Some("azure")
  )
  
  // Build cross-cloud trace
  let cross_cloud_trace = CloudPropagator::build_cross_cloud_trace(cloud_propagator, gcp_to_azure_context)
  
  assert_eq(cross_cloud_trace.cloud_hops.length(), 2)
  assert_eq(cross_cloud_trace.cloud_hops[0].from_cloud, "aws")
  assert_eq(cross_cloud_trace.cloud_hops[0].to_cloud, "gcp")
  assert_eq(cross_cloud_trace.cloud_hops[1].from_cloud, "gcp")
  assert_eq(cross_cloud_trace.cloud_hops[1].to_cloud, "azure")
  
  // Test cloud-specific telemetry enrichment
  let enriched_telemetry = CloudPropagator::enrich_telemetry(cloud_propagator, gcp_to_azure_context)
  
  assert_true(enriched_telemetry.contains("aws"))
  assert_true(enriched_telemetry.contains("gcp"))
  assert_true(enriched_telemetry.contains("azure"))
  assert_true(enriched_telemetry.contains("us-west-1"))
  assert_true(enriched_telemetry.contains("us-central1"))
  assert_true(enriched_telemetry.contains("eastus"))
}

// Test 9: Legacy System Integration
test "legacy system integration" {
  let legacy_integrator = LegacySystemIntegrator::new()
  
  // Configure legacy system adapters
  LegacySystemIntegrator::register_adapter(legacy_integrator, "soap_service", SOAPAdapter::new())
  LegacySystemIntegrator::register_adapter(legacy_integrator, "xml_service", XMLAdapter::new())
  LegacySystemIntegrator::register_adapter(legacy_integrator, "custom_protocol", CustomProtocolAdapter::new())
  
  // Create modern telemetry context
  let modern_context = Context::with_values([
    ("trace_id", "legacy_integration_trace"),
    ("span_id", "legacy_integration_span"),
    ("service_name", "modern-service"),
    ("operation_name", "call_legacy_system")
  ])
  
  // Convert to SOAP headers
  let soap_headers = LegacySystemIntegrator::to_soap_headers(legacy_integrator, modern_context)
  
  assert_true(soap_headers.contains("trace_id"))
  assert_true(soap_headers.contains("legacy_integration_trace"))
  assert_true(soap_headers.contains("span_id"))
  assert_true(soap_headers.contains("legacy_integration_span"))
  
  // Simulate SOAP service response
  let soap_response = SOAPResponse::new()
    .with_header("trace_id", "legacy_integration_trace")
    .with_header("span_id", "legacy_span_response")
    .with_header("processing_time", "150ms")
    .with_header("status", "success")
  
  // Extract context from SOAP response
  let soap_context = LegacySystemIntegrator::from_soap_response(legacy_integrator, soap_response)
  
  assert_eq(
    Context::get_value(soap_context, "trace_id"),
    Some("legacy_integration_trace")
  )
  
  assert_eq(
    Context::get_value(soap_context, "processing_time"),
    Some("150ms")
  )
  
  // Convert to XML format
  let xml_request = LegacySystemIntegrator::to_xml(legacy_integrator, modern_context)
  
  assert_true(xml_request.contains("<trace_id>legacy_integration_trace</trace_id>"))
  assert_true(xml_request.contains("<span_id>legacy_integration_span</span_id>"))
  
  // Simulate XML service response
  let xml_response = XMLResponse::parse("
    <response>
      <trace_id>legacy_integration_trace</trace_id>
      <span_id>xml_span_response</span_id>
      <result>processed</result>
      <metadata>
        <server>legacy-xml-server</server>
        <version>1.5</version>
      </metadata>
    </response>
  ")
  
  let xml_context = LegacySystemIntegrator::from_xml_response(legacy_integrator, xml_response)
  
  assert_eq(
    Context::get_value(xml_context, "trace_id"),
    Some("legacy_integration_trace")
  )
  
  assert_eq(
    Context::get_value(xml_context, "result"),
    Some("processed")
  )
  
  assert_eq(
    Context::get_value(xml_context, "server"),
    Some("legacy-xml-server")
  )
  
  // Test custom protocol integration
  let custom_request = LegacySystemIntegrator::to_custom_protocol(legacy_integrator, modern_context)
  
  let custom_response = CustomProtocolResponse::new()
    .with_field("trace_id", "legacy_integration_trace")
    .with_field("correlation_id", "custom_corr_123")
    .with_field("status_code", "200")
    .with_field("response_data", "custom_response_payload")
  
  let custom_context = LegacySystemIntegrator::from_custom_response(legacy_integrator, custom_response)
  
  assert_eq(
    Context::get_value(custom_context, "trace_id"),
    Some("legacy_integration_trace")
  )
  
  assert_eq(
    Context::get_value(custom_context, "correlation_id"),
    Some("custom_corr_123")
  )
  
  // Build legacy integration trace
  let integration_trace = LegacySystemIntegrator::build_integration_trace(legacy_integrator, [
    ("modern-service", modern_context),
    ("soap-service", soap_context),
    ("xml-service", xml_context),
    ("custom-service", custom_context)
  ])
  
  assert_eq(integration_trace.services.length(), 4)
  assert_eq(integration_trace.services[0].name, "modern-service")
  assert_eq(integration_trace.services[1].name, "soap-service")
  assert_eq(integration_trace.services[2].name, "xml-service")
  assert_eq(integration_trace.services[3].name, "custom-service")
  
  // Verify trace continuity
  for service in integration_trace.services {
    assert_eq(
      Context::get_value(service.context, "trace_id"),
      Some("legacy_integration_trace")
    )
  }
}

// Test 10: Propagation Performance and Optimization
test "propagation performance and optimization" {
  let propagation_optimizer = PropagationOptimizer::new()
  
  // Configure optimization strategies
  PropagationOptimizer::enable_compression(propagation_optimizer, true)
  PropagationOptimizer::enable_caching(propagation_optimizer, true)
  PropagationOptimizer::set_cache_size(propagation_optimizer, 1000)
  PropagationOptimizer::enable_batching(propagation_optimizer, true)
  PropagationOptimizer::set_batch_size(propagation_optimizer, 50)
  
  // Create test context with many attributes
  let large_context = Context::new()
  for i in 0..=100 {
    Context::set_value(large_context, "attr_" + i.to_string(), "value_" + i.to_string())
  }
  
  // Test propagation without optimization
  let unoptimized_start = Time::now()
  for i in 0..=100 {
    let headers = Headers::new()
    HTTPHeaderPropagator::inject(HTTPHeaderPropagator::new(), large_context, headers)
    HTTPHeaderPropagator::extract(HTTPHeaderPropagator::new(), headers)
  }
  let unoptimized_time = Time::now() - unoptimized_start
  
  // Test propagation with optimization
  let optimized_start = Time::now()
  for i in 0..=100 {
    let headers = Headers::new()
    PropagationOptimizer::inject_optimized(propagation_optimizer, large_context, headers)
    PropagationOptimizer::extract_optimized(propagation_optimizer, headers)
  }
  let optimized_time = Time::now() - optimized_start
  
  // Optimization should improve performance
  assert_true(optimized_time < unoptimized_time)
  
  // Test compression effectiveness
  let uncompressed_headers = Headers::new()
  HTTPHeaderPropagator::inject(HTTPHeaderPropagator::new(), large_context, uncompressed_headers)
  
  let compressed_headers = Headers::new()
  PropagationOptimizer::inject_compressed(propagation_optimizer, large_context, compressed_headers)
  
  let uncompressed_size = Headers::total_size(uncompressed_headers)
  let compressed_size = Headers::total_size(compressed_headers)
  
  assert_true(compressed_size < uncompressed_size)
  
  // Test caching effectiveness
  let cache_start = Time::now()
  for i in 0..=1000 {
    let headers = Headers::new()
    PropagationOptimizer::inject_with_cache(propagation_optimizer, large_context, headers)
  }
  let cache_time = Time::now() - cache_start
  
  // Subsequent operations should be faster due to caching
  let cache_start_2 = Time::now()
  for i in 0..=1000 {
    let headers = Headers::new()
    PropagationOptimizer::inject_with_cache(propagation_optimizer, large_context, headers)
  }
  let cache_time_2 = Time::now() - cache_start_2
  
  assert_true(cache_time_2 < cache_time)
  
  // Test batching effectiveness
  let contexts = []
  for i in 0..=100 {
    contexts.push(Context::with_values([
      ("trace_id", "batch_trace_" + i.to_string()),
      ("span_id", "batch_span_" + i.to_string())
    ]))
  }
  
  let individual_start = Time::now()
  for context in contexts {
    let headers = Headers::new()
    HTTPHeaderPropagator::inject(HTTPHeaderPropagator::new(), context, headers)
  }
  let individual_time = Time::now() - individual_start
  
  let batch_start = Time::now()
  PropagationOptimizer::inject_batch(propagation_optimizer, contexts)
  let batch_time = Time::now() - batch_start
  
  // Batching should be more efficient
  assert_true(batch_time < individual_time)
  
  // Verify optimization metrics
  let metrics = PropagationOptimizer::get_metrics(propagation_optimizer)
  
  assert_true(metrics.cache_hit_count > 0)
  assert_true(metrics.compression_ratio > 0.0 && metrics.compression_ratio < 1.0)
  assert_true(metrics.batch_efficiency > 1.0)
}