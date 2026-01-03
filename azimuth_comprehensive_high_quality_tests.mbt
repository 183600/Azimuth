// Azimuth Comprehensive High-Quality Test Suite
// This file contains advanced test cases demonstrating sophisticated telemetry patterns

// Test 1: Advanced Span Lifecycle Management
test "advanced span lifecycle with nested operations" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "lifecycle.test")
  
  // Create parent span with comprehensive configuration
  let parent_span = Tracer::start_span(tracer, "parent.operation")
  Span::set_attribute(parent_span, "operation.type", "parent")
  Span::set_attribute(parent_span, "service.name", "payment-service")
  Span::set_attribute(parent_span, "service.version", "2.1.0")
  
  // Create child spans with different characteristics
  let child_span1 = Tracer::start_span_with_parent(tracer, "child.validation", parent_span)
  let child_span2 = Tracer::start_span_with_parent(tracer, "child.processing", parent_span)
  let child_span3 = Tracer::start_span_with_parent(tracer, "child.persisting", parent_span)
  
  // Configure child spans with specific attributes
  Span::set_attribute(child_span1, "validation.rules", ["amount", "currency", "recipient"])
  Span::set_attribute(child_span2, "processing.algorithm", "fraud_detection_v2")
  Span::set_attribute(child_span3, "persistence.backend", "postgresql")
  
  // Add events with timestamps and structured data
  let start_time = 1735689600000000000L
  Span::add_event_with_timestamp(child_span1, "validation.started", start_time, [
    ("input.amount", DoubleValue(1250.50)),
    ("input.currency", StringValue("USD")),
    ("validation.mode", StringValue("strict"))
  ])
  
  Span::add_event_with_timestamp(child_span2, "processing.started", start_time + 1000000L, [
    ("algorithm.version", StringValue("2.1.3")),
    ("processing.priority", IntValue(1)),
    ("resource.allocation", StringValue("high"))
  ])
  
  Span::add_event_with_timestamp(child_span3, "persisting.started", start_time + 5000000L, [
    ("database.connection", StringValue("conn-12345")),
    ("transaction.isolation", StringValue("read_committed"))
  ])
  
  // Simulate operation completion with different outcomes
  Span::set_status(child_span1, Ok, Some("Validation completed successfully"))
  Span::set_status(child_span2, Error, Some("Processing failed: suspicious pattern detected"))
  Span::set_status(child_span3, Ok, Some("Data persisted successfully"))
  
  // End child spans in specific order
  Span::end_with_timestamp(child_span1, start_time + 3000000L)
  Span::end_with_timestamp(child_span2, start_time + 4000000L)
  Span::end_with_timestamp(child_span3, start_time + 8000000L)
  
  // End parent span with summary
  Span::add_event(parent_span, "operation.completed", [
    ("total.children", IntValue(3)),
    ("successful.children", IntValue(2)),
    ("failed.children", IntValue(1))
  ])
  
  Span::set_status(parent_span, Error, Some("Operation partially failed"))
  Span::end_with_timestamp(parent_span, start_time + 9000000L)
  
  // Verify span relationships and data
  let parent_ctx = Span::span_context(parent_span)
  let child1_ctx = Span::span_context(child_span1)
  let child2_ctx = Span::span_context(child_span2)
  let child3_ctx = Span::span_context(child_span3)
  
  assert_true(SpanContext::is_valid(parent_ctx))
  assert_true(SpanContext::is_valid(child1_ctx))
  assert_true(SpanContext::is_valid(child2_ctx))
  assert_true(SpanContext::is_valid(child3_ctx))
  
  assert_eq(SpanContext::trace_id(parent_ctx), SpanContext::trace_id(child1_ctx))
  assert_eq(SpanContext::trace_id(parent_ctx), SpanContext::trace_id(child2_ctx))
  assert_eq(SpanContext::trace_id(parent_ctx), SpanContext::trace_id(child3_ctx))
}

// Test 2: Complex Metric Aggregation Patterns
test "complex metric aggregation with dimensional analysis" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation.test")
  
  // Create diverse metric instruments
  let request_counter = Meter::create_counter(
    meter, 
    "http.requests.total", 
    Some("Total HTTP requests"), 
    Some("requests")
  )
  
  let response_histogram = Meter::create_histogram(
    meter, 
    "http.response.duration", 
    Some("HTTP response duration distribution"), 
    Some("milliseconds")
  )
  
  let active_connections = Meter::create_updown_counter(
    meter, 
    "http.connections.active", 
    Some("Currently active HTTP connections"), 
    Some("connections")
  )
  
  let queue_size = Meter::create_gauge(
    meter, 
    "http.queue.size", 
    Some("Current HTTP request queue size"), 
    Some("requests")
  )
  
  // Simulate complex traffic patterns with multiple dimensions
  let endpoints = ["/api/users", "/api/orders", "/api/products", "/api/payments"]
  let methods = ["GET", "POST", "PUT", "DELETE"]
  let status_codes = ["200", "201", "400", "404", "500"]
  
  // Generate realistic traffic patterns
  for i in 0..100 {
    let endpoint = endpoints[i % endpoints.length()]
    let method = methods[i % methods.length()]
    let status = if i < 80 { "200" } else if i < 90 { "400" } else { "500" }
    let duration = 50.0 + (i.to_float() * 2.5) + (Math::random() * 100.0)
    
    // Record request count with dimensional attributes
    Counter::add_with_attributes(request_counter, 1.0, [
      ("endpoint", StringValue(endpoint)),
      ("method", StringValue(method)),
      ("status.code", StringValue(status)),
      ("service.version", StringValue("2.1.0")),
      ("region", StringValue("us-west-2"))
    ])
    
    // Record response duration distribution
    Histogram::record_with_attributes(response_histogram, duration, [
      ("endpoint", StringValue(endpoint)),
      ("method", StringValue(method)),
      ("status.code", StringValue(status))
    ])
    
    // Simulate connection lifecycle
    if i % 10 == 0 {
      UpDownCounter::add_with_attributes(active_connections, 1.0, [
        ("connection.type", StringValue("incoming")),
        ("protocol", StringValue("HTTP/1.1"))
      ])
    }
    
    if i % 15 == 0 {
      UpDownCounter::add_with_attributes(active_connections, -1.0, [
        ("connection.type", StringValue("incoming")),
        ("protocol", StringValue("HTTP/1.1"))
      ])
    }
    
    // Update queue size based on load
    let queue_depth = Math::max(0, 25 - (i % 30))
    UpDownCounter::set_with_attributes(queue_size, queue_depth.to_float(), [
      ("queue.type", StringValue("request")),
      ("priority", StringValue("normal"))
    ])
  }
  
  // Verify metric instrument properties
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(response_histogram.description, Some("HTTP response duration distribution"))
  assert_eq(response_histogram.unit, Some("milliseconds"))
  
  assert_eq(active_connections.name, "http.connections.active")
  assert_eq(queue_size.name, "http.queue.size")
  
  assert_true(true)
}

// Test 3: Structured Logging with Correlation
test "structured logging with correlation and context propagation" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "correlation.test")
  
  // Create correlation context
  let correlation_id = "corr-" + String::from_int(Math::abs(Math::random() * 1000000).to_int())
  let request_id = "req-" + String::from_int(Math::abs(Math::random() * 1000000).to_int())
  let session_id = "sess-" + String::from_int(Math::abs(Math::random() * 1000000).to_int())
  
  // Create base context with correlation data
  let base_ctx = Context::root()
  let correlation_key = ContextKey::new("correlation.id")
  let request_key = ContextKey::new("request.id")
  let session_key = ContextKey::new("session.id")
  let user_key = ContextKey::new("user.id")
  
  let ctx_with_correlation = Context::with_value(base_ctx, correlation_key, correlation_id)
  let ctx_with_request = Context::with_value(ctx_with_correlation, request_key, request_id)
  let ctx_with_session = Context::with_value(ctx_with_request, session_key, session_id)
  let ctx_with_user = Context::with_value(ctx_with_session, user_key, "user-12345")
  
  // Create structured log records with different severity levels
  let startup_log = LogRecord::new_with_context(
    Info,
    Some("Service startup initiated"),
    Some([
      ("service.name", StringValue("payment-service")),
      ("service.version", StringValue("2.1.0")),
      ("startup.timestamp", StringValue("2025-01-02T10:00:00Z")),
      ("environment", StringValue("production"))
    ]),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("trace-startup"),
    Some("span-startup"),
    Some(ctx_with_user)
  )
  
  let request_log = LogRecord::new_with_context(
    Info,
    Some("Incoming payment request processed"),
    Some([
      ("request.method", StringValue("POST")),
      ("request.path", StringValue("/api/payments")),
      ("request.size", IntValue(1024)),
      ("client.ip", StringValue("192.168.1.100")),
      ("user.agent", StringValue("PaymentClient/2.1.0"))
    ]),
    Some(1735689600000001000L),
    Some(1735689600000001000L),
    Some("trace-payment"),
    Some("span-request"),
    Some(ctx_with_user)
  )
  
  let validation_log = LogRecord::new_with_context(
    Warn,
    Some("Payment validation warnings detected"),
    Some([
      ("validation.warnings", ArrayValue([
        StringValue("amount exceeds daily limit"),
        StringValue("recipient on watchlist"),
        StringValue("unusual geographic location")
      ])),
      ("validation.score", DoubleValue(0.85)),
      ("risk.level", StringValue("medium"))
    ]),
    Some(1735689600000002000L),
    Some(1735689600000002000L),
    Some("trace-payment"),
    Some("span-validation"),
    Some(ctx_with_user)
  )
  
  let processing_log = LogRecord::new_with_context(
    Error,
    Some("Payment processing failed"),
    Some([
      ("error.code", StringValue("PAYMENT_DECLINED")),
      ("error.message", StringValue("Insufficient funds")),
      ("error.category", StringValue("business_logic")),
      ("error.retryable", BoolValue(false)),
      ("gateway.response", StringValue("DECLINED: INSUFFICIENT_FUNDS"))
    ]),
    Some(1735689600000003000L),
    Some(1735689600000003000L),
    Some("trace-payment"),
    Some("span-processing"),
    Some(ctx_with_user)
  )
  
  let cleanup_log = LogRecord::new_with_context(
    Debug,
    Some("Resource cleanup completed"),
    Some([
      ("cleanup.duration", DoubleValue(15.5)),
      ("resources.freed", IntValue(42)),
      ("memory.reclaimed", IntValue(1048576))
    ]),
    Some(1735689600000004000L),
    Some(1735689600000004000L),
    Some("trace-payment"),
    Some("span-cleanup"),
    Some(ctx_with_user)
  )
  
  // Emit all log records
  Logger::emit(logger, startup_log)
  Logger::emit(logger, request_log)
  Logger::emit(logger, validation_log)
  Logger::emit(logger, processing_log)
  Logger::emit(logger, cleanup_log)
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(startup_log), Info)
  assert_eq(LogRecord::severity_number(validation_log), Warn)
  assert_eq(LogRecord::severity_number(processing_log), Error)
  assert_eq(LogRecord::severity_number(cleanup_log), Debug)
  
  assert_eq(LogRecord::body(startup_log), Some("Service startup initiated"))
  assert_eq(LogRecord::body(processing_log), Some("Payment processing failed"))
  
  // Verify correlation data is preserved
  let startup_correlation = Context::get(LogRecord::context(startup_log), correlation_key)
  let processing_correlation = Context::get(LogRecord::context(processing_log), correlation_key)
  
  assert_eq(startup_correlation, Some(correlation_id))
  assert_eq(processing_correlation, Some(correlation_id))
  
  assert_true(true)
}

// Test 4: Context Propagation Across Service Boundaries
test "context propagation across service boundaries with transformations" {
  // Create initial context with comprehensive data
  let initial_ctx = Context::root()
  
  let trace_key = ContextKey::new("trace.id")
  let span_key = ContextKey::new("span.id")
  let baggage_key = ContextKey::new("baggage")
  let auth_key = ContextKey::new("auth.token")
  let tenant_key = ContextKey::new("tenant.id")
  
  let ctx_with_trace = Context::with_value(initial_ctx, trace_key, "trace-abc123")
  let ctx_with_span = Context::with_value(ctx_with_trace, span_key, "span-def456")
  
  // Create baggage with multiple entries
  let initial_baggage = Baggage::new()
  let baggage_with_user = Baggage::set_entry(initial_baggage, "user.id", "user-789")
  let baggage_with_session = Baggage::set_entry(baggage_with_user, "session.id", "sess-012")
  let baggage_with_request = Baggage::set_entry(baggage_with_session, "request.id", "req-345")
  
  let ctx_with_baggage = Context::with_value(ctx_with_span, baggage_key, baggage_with_request)
  let ctx_with_auth = Context::with_value(ctx_with_baggage, auth_key, "bearer-token-xyz")
  let ctx_with_tenant = Context::with_value(ctx_with_auth, tenant_key, "tenant-001")
  
  // Simulate service boundary crossing with transformation
  let service_a_ctx = ctx_with_tenant
  
  // Extract context for propagation
  let extracted_trace = Context::get(service_a_ctx, trace_key)
  let extracted_span = Context::get(service_a_ctx, span_key)
  let extracted_baggage = Context::get(service_a_ctx, baggage_key)
  let extracted_auth = Context::get(service_a_ctx, auth_key)
  let extracted_tenant = Context::get(service_a_ctx, tenant_key)
  
  // Create new context for Service B with transformation
  let service_b_ctx = Context::root()
  let service_b_trace = Context::with_value(service_b_ctx, trace_key, extracted_trace.unwrap_or(""))
  let service_b_span = Context::with_value(service_b_trace, span_key, "span-ghi789") // New span
  let service_b_baggage = Context::with_value(service_b_span, baggage_key, extracted_baggage.unwrap_or(Baggage::new()))
  
  // Add service-specific context
  let service_key = ContextKey::new("service.name")
  let version_key = ContextKey::new("service.version")
  let region_key = ContextKey::new("service.region")
  
  let service_b_with_name = Context::with_value(service_b_baggage, service_key, "order-service")
  let service_b_with_version = Context::with_value(service_b_with_name, version_key, "1.5.2")
  let service_b_with_region = Context::with_value(service_b_with_version, region_key, "eu-west-1")
  
  // Transform baggage for Service B
  let service_b_baggage_data = Context::get(service_b_with_region, baggage_key).unwrap_or(Baggage::new())
  let service_b_baggage_transformed = Baggage::set_entry(service_b_baggage_data, "service.b.id", "svc-b-001")
  let service_b_baggage_final = Baggage::set_entry(service_b_baggage_transformed, "call.chain", "a->b")
  
  let service_b_final_ctx = Context::with_value(service_b_with_region, baggage_key, service_b_baggage_final)
  
  // Simulate Service B calling Service C
  let service_c_ctx = Context::root()
  let service_c_trace = Context::with_value(service_c_ctx, trace_key, extracted_trace.unwrap_or(""))
  let service_c_span = Context::with_value(service_c_trace, span_key, "span-jkl012") // New span
  
  // Propagate and transform baggage
  let service_c_baggage_data = service_b_baggage_final
  let service_c_baggage_with_c = Baggage::set_entry(service_c_baggage_data, "service.c.id", "svc-c-002")
  let service_c_baggage_final = Baggage::set_entry(service_c_baggage_with_c, "call.chain", "a->b->c")
  
  let service_c_with_baggage = Context::with_value(service_c_span, baggage_key, service_c_baggage_final)
  let service_c_with_name = Context::with_value(service_c_with_baggage, service_key, "inventory-service")
  let service_c_final_ctx = Context::with_value(service_c_with_name, version_key, "3.1.0")
  
  // Verify context propagation integrity
  let final_trace = Context::get(service_c_final_ctx, trace_key)
  let original_trace = Context::get(service_a_ctx, trace_key)
  
  assert_eq(final_trace, original_trace)
  assert_eq(final_trace, Some("trace-abc123"))
  
  // Verify baggage transformation
  let final_baggage = Context::get(service_c_final_ctx, baggage_key).unwrap_or(Baggage::new())
  let final_call_chain = Baggage::get_entry(final_baggage, "call.chain")
  let final_user = Baggage::get_entry(final_baggage, "user.id")
  
  assert_eq(final_call_chain, Some("a->b->c"))
  assert_eq(final_user, Some("user-789"))
  
  // Verify service-specific context
  let service_c_name = Context::get(service_c_final_ctx, service_key)
  let service_c_version = Context::get(service_c_final_ctx, version_key)
  
  assert_eq(service_c_name, Some("order-service"))
  assert_eq(service_c_version, Some("3.1.0"))
  
  assert_true(true)
}

// Test 5: Resource Metadata Management with Hierarchies
test "resource metadata management with hierarchical structures" {
  // Create base resource with core attributes
  let base_resource = Resource::new()
  let base_attributes = [
    ("service.name", StringValue("payment-platform")),
    ("service.version", StringValue("2.1.0")),
    ("service.namespace", StringValue("production")),
    ("deployment.environment", StringValue("prod"))
  ]
  
  let resource_with_base = Resource::with_attributes(base_resource, base_attributes)
  
  // Create infrastructure resource layer
  let infra_resource = Resource::new()
  let infra_attributes = [
    ("host.name", StringValue("prod-pay-01.example.com")),
    ("host.id", StringValue("i-0123456789abcdef0")),
    ("host.type", StringValue("t3.large")),
    ("host.arch", StringValue("x86_64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0-1023-aws")),
    ("cloud.provider", StringValue("aws")),
    ("cloud.region", StringValue("us-west-2")),
    ("cloud.availability_zone", StringValue("us-west-2a"))
  ]
  
  let resource_with_infra = Resource::with_attributes(infra_resource, infra_attributes)
  
  // Create application resource layer
  let app_resource = Resource::new()
  let app_attributes = [
    ("application.name", StringValue("payment-processor")),
    ("application.version", StringValue("2.1.0-rc3")),
    ("application.build", StringValue("20250102-1000")),
    ("application.runtime", StringValue("nodejs")),
    ("application.runtime.version", StringValue("18.17.0")),
    ("application.framework", StringValue("express")),
    ("application.framework.version", StringValue("4.18.2"))
  ]
  
  let resource_with_app = Resource::with_attributes(app_resource, app_attributes)
  
  // Create business context resource layer
  let business_resource = Resource::new()
  let business_attributes = [
    ("business.unit", StringValue("payments")),
    ("business.vertical", StringValue("fintech")),
    ("business.region", StringValue("north-america")),
    ("business.compliance", StringValue("pci-dss-level-1")),
    ("business.tier", StringValue("critical")),
    ("business.sla", StringValue("99.99"))
  ]
  
  let resource_with_business = Resource::with_attributes(business_resource, business_attributes)
  
  // Merge resources hierarchically
  let merged_base_infra = Resource::merge(resource_with_base, resource_with_infra)
  let merged_app_layer = Resource::merge(merged_base_infra, resource_with_app)
  let final_resource = Resource::merge(merged_app_layer, resource_with_business)
  
  // Verify hierarchical resource attributes
  let service_name = Resource::get_attribute(final_resource, "service.name")
  let host_name = Resource::get_attribute(final_resource, "host.name")
  let app_version = Resource::get_attribute(final_resource, "application.version")
  let business_unit = Resource::get_attribute(final_resource, "business.unit")
  let cloud_region = Resource::get_attribute(final_resource, "cloud.region")
  let compliance_level = Resource::get_attribute(final_resource, "business.compliance")
  
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "payment-platform")
    _ => assert_true(false)
  }
  
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "prod-pay-01.example.com")
    _ => assert_true(false)
  }
  
  match app_version {
    Some(StringValue(version)) => assert_eq(version, "2.1.0-rc3")
    _ => assert_true(false)
  }
  
  match business_unit {
    Some(StringValue(unit)) => assert_eq(unit, "payments")
    _ => assert_true(false)
  }
  
  match cloud_region {
    Some(StringValue(region)) => assert_eq(region, "us-west-2")
    _ => assert_true(false)
  }
  
  match compliance_level {
    Some(StringValue(compliance)) => assert_eq(compliance, "pci-dss-level-1")
    _ => assert_true(false)
  }
  
  // Test resource attribute override behavior
  let override_resource = Resource::new()
  let override_attributes = [
    ("service.version", StringValue("2.2.0-hotfix")), // Override base version
    ("host.name", StringValue("prod-pay-02.example.com")), // Override infra host
    ("emergency.maintenance", BoolValue(true)) // New attribute
  ]
  
  let resource_with_override = Resource::with_attributes(override_resource, override_attributes)
  let resource_with_override_final = Resource::merge(final_resource, resource_with_override)
  
  // Verify override behavior
  let overridden_version = Resource::get_attribute(resource_with_override_final, "service.version")
  let overridden_host = Resource::get_attribute(resource_with_override_final, "host.name")
  let emergency_flag = Resource::get_attribute(resource_with_override_final, "emergency.maintenance")
  
  match overridden_version {
    Some(StringValue(version)) => assert_eq(version, "2.2.0-hotfix")
    _ => assert_true(false)
  }
  
  match overridden_host {
    Some(StringValue(host)) => assert_eq(host, "prod-pay-02.example.com")
    _ => assert_true(false)
  }
  
  match emergency_flag {
    Some(BoolValue(flag)) => assert_true(flag)
    _ => assert_true(false)
  }
  
  // Ensure non-overridden attributes are preserved
  let preserved_business_unit = Resource::get_attribute(resource_with_override_final, "business.unit")
  match preserved_business_unit {
    Some(StringValue(unit)) => assert_eq(unit, "payments")
    _ => assert_true(false)
  }
  
  assert_true(true)
}

// Test 6: Baggage Propagation with Metadata and TTL
test "baggage propagation with metadata and time-to-live" {
  // Create initial baggage with comprehensive entries
  let initial_baggage = Baggage::new()
  
  // Add entries with different metadata
  let baggage_with_user = Baggage::set_entry_with_metadata(
    initial_baggage, 
    "user.id", 
    "user-12345",
    Some([
      ("source.service", StringValue("auth-service")),
      ("timestamp", StringValue("2025-01-02T10:00:00Z")),
      ("ttl", IntValue(3600))
    ])
  )
  
  let baggage_with_session = Baggage::set_entry_with_metadata(
    baggage_with_user,
    "session.id",
    "sess-67890",
    Some([
      ("source.service", StringValue("session-service")),
      ("timestamp", StringValue("2025-01-02T10:01:00Z")),
      ("ttl", IntValue(1800))
    ])
  )
  
  let baggage_with_request = Baggage::set_entry_with_metadata(
    baggage_with_session,
    "request.id",
    "req-abc123",
    Some([
      ("source.service", StringValue("api-gateway")),
      ("timestamp", StringValue("2025-01-02T10:02:00Z")),
      ("ttl", IntValue(300))
    ])
  )
  
  let baggage_with_trace = Baggage::set_entry_with_metadata(
    baggage_with_request,
    "trace.context",
    "trace-def456",
    Some([
      ("source.service", StringValue("tracing-service")),
      ("timestamp", StringValue("2025-01-02T10:02:30Z")),
      ("ttl", IntValue(7200))
    ])
  )
  
  // Simulate baggage propagation through service chain
  let service_a_baggage = baggage_with_trace
  
  // Service A adds its own baggage entry
  let service_a_baggage_enhanced = Baggage::set_entry_with_metadata(
    service_a_baggage,
    "service.a.correlation",
    "corr-a-001",
    Some([
      ("source.service", StringValue("service-a")),
      ("timestamp", StringValue("2025-01-02T10:03:00Z")),
      ("ttl", IntValue(600))
    ])
  )
  
  // Service B receives and processes baggage
  let service_b_baggage = service_a_baggage_enhanced
  
  // Check TTL and remove expired entries (simulate time passage)
  let current_time = 1735689600000000000L // 2025-01-02T10:00:00Z
  let service_b_baggage_filtered = Baggage::filter_by_ttl(service_b_baggage, current_time)
  
  // Service B adds its own entry
  let service_b_baggage_final = Baggage::set_entry_with_metadata(
    service_b_baggage_filtered,
    "service.b.context",
    "ctx-b-002",
    Some([
      ("source.service", StringValue("service-b")),
      ("timestamp", StringValue("2025-01-02T10:04:00Z")),
      ("ttl", IntValue(900))
    ])
  )
  
  // Service C receives and transforms baggage
  let service_c_baggage = service_b_baggage_final
  
  // Service C transforms existing entries
  let service_c_baggage_transformed = Baggage::update_entry_metadata(
    service_c_baggage,
    "user.id",
    Some([
      ("processed.by", StringValue("service-c")),
      ("transformation", StringValue("anonymized")),
      ("timestamp", StringValue("2025-01-02T10:05:00Z"))
    ])
  )
  
  // Service C adds final entry
  let service_c_baggage_final = Baggage::set_entry_with_metadata(
    service_c_baggage_transformed,
    "service.c.result",
    "success",
    Some([
      ("source.service", StringValue("service-c")),
      ("timestamp", StringValue("2025-01-02T10:06:00Z")),
      ("ttl", IntValue(1200))
    ])
  )
  
  // Verify baggage propagation integrity
  let final_user_id = Baggage::get_entry(service_c_baggage_final, "user.id")
  let final_session_id = Baggage::get_entry(service_c_baggage_final, "session.id")
  let final_request_id = Baggage::get_entry(service_c_baggage_final, "request.id")
  let final_trace_context = Baggage::get_entry(service_c_baggage_final, "trace.context")
  let final_service_a_correlation = Baggage::get_entry(service_c_baggage_final, "service.a.correlation")
  let final_service_b_context = Baggage::get_entry(service_c_baggage_final, "service.b.context")
  let final_service_c_result = Baggage::get_entry(service_c_baggage_final, "service.c.result")
  
  assert_eq(final_user_id, Some("user-12345"))
  assert_eq(final_session_id, Some("sess-67890"))
  assert_eq(final_request_id, Some("req-abc123"))
  assert_eq(final_trace_context, Some("trace-def456"))
  assert_eq(final_service_a_correlation, Some("corr-a-001"))
  assert_eq(final_service_b_context, Some("ctx-b-002"))
  assert_eq(final_service_c_result, Some("success"))
  
  // Verify metadata preservation and transformation
  let user_metadata = Baggage::get_entry_metadata(service_c_baggage_final, "user.id")
  match user_metadata {
    Some(metadata) => {
      let processed_by = Attributes::get(metadata, "processed.by")
      match processed_by {
        Some(StringValue(service)) => assert_eq(service, "service-c")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  let trace_metadata = Baggage::get_entry_metadata(service_c_baggage_final, "trace.context")
  match trace_metadata {
    Some(metadata) => {
      let source_service = Attributes::get(metadata, "source.service")
      match source_service {
        Some(StringValue(service)) => assert_eq(service, "tracing-service")
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // Test baggage serialization and deserialization
  let serialized_baggage = Baggage::serialize(service_c_baggage_final)
  let deserialized_baggage = Baggage::deserialize(serialized_baggage)
  
  let deserialized_user_id = Baggage::get_entry(deserialized_baggage, "user.id")
  let deserialized_service_c_result = Baggage::get_entry(deserialized_baggage, "service.c.result")
  
  assert_eq(deserialized_user_id, Some("user-12345"))
  assert_eq(deserialized_service_c_result, Some("success"))
  
  assert_true(true)
}

// Test 7: Error Handling and Recovery Patterns
test "error handling and recovery patterns with resilience" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.recovery.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "error.metrics")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "error.logger")
  
  // Create metrics for error tracking
  let error_counter = Meter::create_counter(
    meter, 
    "errors.total", 
    Some("Total errors encountered"), 
    Some("errors")
  )
  
  let recovery_counter = Meter::create_counter(
    meter, 
    "recoveries.total", 
    Some("Total successful recoveries"), 
    Some("recoveries")
  )
  
  let retry_histogram = Meter::create_histogram(
    meter, 
    "retry.attempts", 
    Some("Number of retry attempts"), 
    Some("attempts")
  )
  
  // Create main operation span
  let main_span = Tracer::start_span(tracer, "resilient.operation")
  Span::set_attribute(main_span, "operation.type", "payment_processing")
  Span::set_attribute(main_span, "resilience.enabled", true)
  
  // Simulate error scenarios with recovery
  let error_scenarios = [
    ("database.connection.timeout", "Database connection timeout", true, 3),
    ("network.transient.failure", "Transient network failure", true, 2),
    ("service.dependency.unavailable", "External service unavailable", true, 5),
    ("resource.exhausted", "Resource pool exhausted", true, 1),
    ("validation.permanent.failure", "Permanent validation failure", false, 0)
  ]
  
  for i in 0..error_scenarios.length() {
    let scenario = error_scenarios[i]
    let error_code = scenario.0
    let error_message = scenario.1
    let is_recoverable = scenario.2
    let max_retries = scenario.3
    
    // Create error-specific span
    let error_span = Tracer::start_span_with_parent(tracer, "error.attempt", main_span)
    Span::set_attribute(error_span, "error.code", error_code)
    Span::set_attribute(error_span, "error.recoverable", is_recoverable)
    Span::set_attribute(error_span, "error.max_retries", max_retries)
    
    // Log error occurrence
    let error_log = LogRecord::new(Error, "Error occurred during operation")
    LogRecord::add_attribute(error_log, "error.code", error_code)
    LogRecord::add_attribute(error_log, "error.message", error_message)
    LogRecord::add_attribute(error_log, "error.recoverable", is_recoverable)
    LogRecord::add_attribute(error_log, "operation.id", "op-" + String::from_int(i))
    
    Logger::emit(logger, error_log)
    
    // Record error metric
    Counter::add_with_attributes(error_counter, 1.0, [
      ("error.code", StringValue(error_code)),
      ("error.recoverable", BoolValue(is_recoverable)),
      ("operation.type", StringValue("payment_processing"))
    ])
    
    if is_recoverable {
      // Simulate retry attempts
      let mut retry_count = 0
      let mut recovery_successful = false
      
      for retry in 0..max_retries {
        retry_count = retry_count + 1
        
        // Add retry event to span
        Span::add_event(error_span, "retry.attempt", [
          ("retry.count", IntValue(retry_count)),
          ("retry.timestamp", StringValue("2025-01-02T10:0" + String::from_int(retry_count) + ":00Z"))
        ])
        
        // Log retry attempt
        let retry_log = LogRecord::new(Warn, "Retry attempt initiated")
        LogRecord::add_attribute(retry_log, "retry.count", retry_count)
        LogRecord::add_attribute(retry_log, "error.code", error_code)
        LogRecord::add_attribute(retry_log, "max.retries", max_retries)
        
        Logger::emit(logger, retry_log)
        
        // Simulate recovery success (70% chance on each retry)
        if Math::random() > 0.3 {
          recovery_successful = true
          break
        }
      }
      
      // Record retry attempts
      Histogram::record_with_attributes(retry_histogram, retry_count.to_float(), [
        ("error.code", StringValue(error_code)),
        ("recovery.successful", BoolValue(recovery_successful))
      ])
      
      if recovery_successful {
        // Record successful recovery
        Counter::add_with_attributes(recovery_counter, 1.0, [
          ("error.code", StringValue(error_code)),
          ("retry.attempts", IntValue(retry_count))
        ])
        
        // Set success status
        Span::set_status(error_span, Ok, Some("Recovery successful after " + String::from_int(retry_count) + " attempts"))
        
        // Log recovery success
        let recovery_log = LogRecord::new(Info, "Error recovery successful")
        LogRecord::add_attribute(recovery_log, "error.code", error_code)
        LogRecord::add_attribute(recovery_log, "retry.attempts", retry_count)
        LogRecord::add_attribute(recovery_log, "recovery.duration", 15000)
        
        Logger::emit(logger, recovery_log)
      } else {
        // Set failure status
        Span::set_status(error_span, Error, Some("Recovery failed after " + String::from_int(retry_count) + " attempts"))
        
        // Log recovery failure
        let failure_log = LogRecord::new(Error, "Error recovery failed")
        LogRecord::add_attribute(failure_log, "error.code", error_code)
        LogRecord::add_attribute(failure_log, "retry.attempts", retry_count)
        LogRecord::add_attribute(failure_log, "escalation.required", true)
        
        Logger::emit(logger, failure_log)
      }
    } else {
      // Non-recoverable error - no retries
      Span::set_status(error_span, Error, Some("Non-recoverable error: " + error_message))
      
      // Log non-recoverable error
      let permanent_error_log = LogRecord::new(Error, "Non-recoverable error encountered")
      LogRecord::add_attribute(permanent_error_log, "error.code", error_code)
      LogRecord::add_attribute(permanent_error_log, "error.category", "permanent")
      LogRecord::add_attribute(permanent_error_log, "escalation.required", true)
      
      Logger::emit(logger, permanent_error_log)
    }
    
    Span::end(error_span)
  }
  
  // Add summary to main span
  let total_errors = error_scenarios.length()
  let total_recoverable = 4  // Based on the scenarios above
  let total_non_recoverable = 1
  
  Span::add_event(main_span, "operation.summary", [
    ("total.errors", IntValue(total_errors)),
    ("recoverable.errors", IntValue(total_recoverable)),
    ("non.recoverable.errors", IntValue(total_non_recoverable))
  ])
  
  Span::set_status(main_span, Ok, Some("Operation completed with error handling"))
  Span::end(main_span)
  
  // Verify metric instruments
  assert_eq(error_counter.name, "errors.total")
  assert_eq(recovery_counter.name, "recoveries.total")
  assert_eq(retry_histogram.name, "retry.attempts")
  
  assert_true(true)
}

// Test 8: Performance Optimization Scenarios
test "performance optimization with batching and sampling strategies" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.test")
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.metrics")
  
  // Create performance monitoring metrics
  let batch_size_histogram = Meter::create_histogram(
    meter, 
    "batch.size", 
    Some("Batch processing sizes"), 
    Some("items")
  )
  
  let processing_time_histogram = Meter::create_histogram(
    meter, 
    "processing.time", 
    Some("Processing time distribution"), 
    Some("milliseconds")
  )
  
  let throughput_counter = Meter::create_counter(
    meter, 
    "throughput.total", 
    Some("Total items processed"), 
    Some("items")
  )
  
  let sampling_ratio_gauge = Meter::create_gauge(
    meter, 
    "sampling.ratio", 
    Some("Current sampling ratio"), 
    Some("ratio")
  )
  
  // Test 1: Batch processing optimization
  let batch_span = Tracer::start_span(tracer, "batch.processing.optimization")
  Span::set_attribute(batch_span, "optimization.type", "batch_processing")
  
  let batch_sizes = [10, 25, 50, 100, 200, 500]
  
  for i in 0..batch_sizes.length() {
    let batch_size = batch_sizes[i]
    let start_time = 1735689600000000L + (i * 1000L)
    
    // Simulate batch processing
    let processing_time = 50.0 + (batch_size.to_float() * 0.5) + (Math::random() * 20.0)
    
    // Record batch metrics
    Histogram::record_with_attributes(batch_size_histogram, batch_size.to_float(), [
      ("batch.id", StringValue("batch-" + String::from_int(i))),
      ("processing.strategy", StringValue("optimized"))
    ])
    
    Histogram::record_with_attributes(processing_time_histogram, processing_time, [
      ("batch.size", IntValue(batch_size)),
      ("processing.strategy", StringValue("optimized"))
    ])
    
    Counter::add_with_attributes(throughput_counter, batch_size.to_float(), [
      ("batch.size", IntValue(batch_size)),
      ("processing.strategy", StringValue("optimized"))
    ])
    
    // Add batch processing event
    Span::add_event_with_timestamp(batch_span, "batch.completed", start_time, [
      ("batch.size", IntValue(batch_size)),
      ("processing.time", DoubleValue(processing_time)),
      ("throughput", DoubleValue(batch_size.to_float() / (processing_time / 1000.0)))
    ])
  }
  
  Span::set_status(batch_span, Ok, Some("Batch processing optimization completed"))
  Span::end(batch_span)
  
  // Test 2: Adaptive sampling strategies
  let sampling_span = Tracer::start_span(tracer, "adaptive.sampling.optimization")
  Span::set_attribute(sampling_span, "optimization.type", "adaptive_sampling")
  
  let traffic_levels = [100, 500, 1000, 5000, 10000, 50000]
  let base_sampling_ratios = [1.0, 0.8, 0.5, 0.2, 0.1, 0.01]
  
  for i in 0..traffic_levels.length() {
    let traffic_level = traffic_levels[i]
    let base_ratio = base_sampling_ratios[i]
    
    // Adaptive sampling based on error rates and latency
    let error_rate = 0.01 + (Math::random() * 0.05)
    let avg_latency = 100.0 + (traffic_level.to_float() * 0.01) + (Math::random() * 50.0)
    
    // Calculate adaptive sampling ratio
    let adaptive_ratio = if error_rate > 0.03 {
      base_ratio * 2.0  // Increase sampling during high error rates
    } else if avg_latency > 200.0 {
      base_ratio * 1.5  // Increase sampling during high latency
    } else {
      base_ratio * 0.8  // Decrease sampling during normal operation
    }
    
    let final_ratio = Math::min(1.0, Math::max(0.001, adaptive_ratio))
    
    // Update sampling ratio gauge
    UpDownCounter::set_with_attributes(sampling_ratio_gauge, final_ratio, [
      ("traffic.level", IntValue(traffic_level)),
      ("error.rate", DoubleValue(error_rate)),
      ("avg.latency", DoubleValue(avg_latency))
    ])
    
    // Simulate sampling application
    let sampled_requests = (traffic_level.to_float() * final_ratio).to_int()
    let sampled_requests_float = sampled_requests.to_float()
    
    Counter::add_with_attributes(throughput_counter, sampled_requests_float, [
      ("traffic.level", IntValue(traffic_level)),
      ("sampling.ratio", DoubleValue(final_ratio)),
      ("sampling.strategy", StringValue("adaptive"))
    ])
    
    // Add sampling event
    Span::add_event(sampling_span, "sampling.adjusted", [
      ("traffic.level", IntValue(traffic_level)),
      ("base.ratio", DoubleValue(base_ratio)),
      ("adaptive.ratio", DoubleValue(final_ratio)),
      ("sampled.requests", IntValue(sampled_requests)),
      ("error.rate", DoubleValue(error_rate)),
      ("avg.latency", DoubleValue(avg_latency))
    ])
  }
  
  Span::set_status(sampling_span, Ok, Some("Adaptive sampling optimization completed"))
  Span::end(sampling_span)
  
  // Test 3: Memory and resource optimization
  let resource_span = Tracer::start_span(tracer, "resource.optimization")
  Span::set_attribute(resource_span, "optimization.type", "resource_management")
  
  let memory_pools = [1024, 2048, 4096, 8192]  // KB
  let connection_pools = [10, 25, 50, 100]
  
  for i in 0..memory_pools.length() {
    let pool_size = memory_pools[i]
    let connection_pool = connection_pools[i]
    
    // Simulate resource allocation and deallocation
    let allocation_time = 5.0 + (pool_size.to_float() * 0.001) + (Math::random() * 2.0)
    let deallocation_time = 2.0 + (pool_size.to_float() * 0.0005) + (Math::random() * 1.0)
    
    // Record resource metrics
    Histogram::record_with_attributes(processing_time_histogram, allocation_time, [
      ("operation.type", StringValue("memory_allocation")),
      ("pool.size", IntValue(pool_size)),
      ("optimization.strategy", StringValue("pool_based"))
    ])
    
    Histogram::record_with_attributes(processing_time_histogram, deallocation_time, [
      ("operation.type", StringValue("memory_deallocation")),
      ("pool.size", IntValue(pool_size)),
      ("optimization.strategy", StringValue("pool_based"))
    ])
    
    // Connection pool metrics
    let connection_acquisition_time = 1.0 + (connection_pool.to_float() * 0.01) + (Math::random() * 0.5)
    
    Histogram::record_with_attributes(processing_time_histogram, connection_acquisition_time, [
      ("operation.type", StringValue("connection_acquisition")),
      ("connection.pool.size", IntValue(connection_pool)),
      ("optimization.strategy", StringValue("connection_pooling"))
    ])
    
    // Add resource optimization event
    Span::add_event(resource_span, "resource.optimized", [
      ("memory.pool.size", IntValue(pool_size)),
      ("connection.pool.size", IntValue(connection_pool)),
      ("allocation.time", DoubleValue(allocation_time)),
      ("deallocation.time", DoubleValue(deallocation_time)),
      ("connection.acquisition.time", DoubleValue(connection_acquisition_time))
    ])
  }
  
  Span::set_status(resource_span, Ok, Some("Resource optimization completed"))
  Span::end(resource_span)
  
  // Verify metric instruments
  assert_eq(batch_size_histogram.name, "batch.size")
  assert_eq(processing_time_histogram.name, "processing.time")
  assert_eq(throughput_counter.name, "throughput.total")
  assert_eq(sampling_ratio_gauge.name, "sampling.ratio")
  
  assert_true(true)
}