// Azimuth Telemetry System - Distributed Tracing Chain Tests
// This file contains test cases for distributed tracing chain functionality

// Test 1: Trace Context Creation and Propagation
test "trace context creation and propagation" {
  // Create a trace context with trace ID and span ID
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let trace_flags = 1  // Sampled
  
  let trace_context = TraceContext::new(trace_id, span_id, trace_flags)
  
  assert_eq(TraceContext::trace_id(trace_context), trace_id)
  assert_eq(TraceContext::span_id(trace_context), span_id)
  assert_eq(TraceContext::trace_flags(trace_context), trace_flags)
  assert_true(TraceContext::is_sampled(trace_context))
  
  // Create child context
  let child_span_id = "00f067aa0ba902b7"
  let child_context = TraceContext::create_child(trace_context, child_span_id)
  
  // Child should inherit trace ID and flags
  assert_eq(TraceContext::trace_id(child_context), trace_id)
  assert_eq(TraceContext::span_id(child_context), child_span_id)
  assert_eq(TraceContext::trace_flags(child_context), trace_flags)
  assert_true(TraceContext::is_sampled(child_context))
}

// Test 2: Trace Chain Builder
test "trace chain builder" {
  // Create a trace chain builder
  let builder = TraceChainBuilder::new("user_request")
  
  // Add root span
  let root_span = TraceChainBuilder::add_root_span(
    builder,
    "user_request",
    SpanKind::Server,
    [("http.method", StringValue("GET")), ("http.url", StringValue("/api/users"))]
  )
  
  // Add child spans
  let auth_span = TraceChainBuilder::add_child_span(
    builder,
    root_span,
    "authenticate_user",
    SpanKind::Internal,
    [("user.id", StringValue("12345"))]
  )
  
  let db_span = TraceChainBuilder::add_child_span(
    builder,
    auth_span,
    "database_query",
    SpanKind::Client,
    [("db.statement", StringValue("SELECT * FROM users WHERE id = ?"))]
  )
  
  let cache_span = TraceChainBuilder::add_child_span(
    builder,
    root_span,
    "cache_lookup",
    SpanKind::Internal,
    [("cache.key", StringValue("user:12345"))]
  )
  
  // Build the trace chain
  let trace_chain = TraceChainBuilder::build(builder)
  
  // Verify trace chain structure
  assert_eq(TraceChain::trace_id(trace_chain), TraceChainBuilder::trace_id(builder))
  assert_eq(TraceChain::span_count(trace_chain), 4)
  
  // Verify root span
  let root = TraceChain::get_root_span(trace_chain)
  assert_eq(Span::name(root), "user_request")
  match Span::kind(root) {
    SpanKind::Server => assert_true(true)
    _ => assert_true(false)
  }
  
  // Verify parent-child relationships
  let auth_children = TraceChain::get_child_spans(trace_chain, root_span)
  assert_eq(auth_children.length(), 2)  // auth_span and cache_span
  
  let db_children = TraceChain::get_child_spans(trace_chain, auth_span)
  assert_eq(db_children.length(), 1)  // db_span
}

// Test 3: Cross-Service Trace Propagation
test "cross-service trace propagation" {
  // Service A: Create trace
  let service_a_trace_id = TraceId::generate()
  let service_a_span_id = SpanId::generate()
  let service_a_context = TraceContext::new(service_a_trace_id, service_a_span_id, 1)
  
  let service_a_span = Span::new("service_a_handler", SpanKind::Server, service_a_context)
  Span::add_event(service_a_span, "request_received", None)
  
  // Simulate HTTP call to Service B
  let trace_headers = TraceContext::to_headers(service_a_context)
  
  // Service B: Extract trace context from headers
  let service_b_context = TraceContext::from_headers(trace_headers)
  let service_b_span_id = SpanId::generate()
  let service_b_child_context = TraceContext::create_child(service_b_context, service_b_span_id)
  
  let service_b_span = Span::new("service_b_handler", SpanKind::Server, service_b_child_context)
  Span::add_event(service_b_span, "request_received", None)
  
  // Simulate database call from Service B
  let db_span_id = SpanId::generate()
  let db_context = TraceContext::create_child(service_b_child_context, db_span_id)
  
  let db_span = Span::new("database_query", SpanKind::Client, db_context)
  Span::add_event(db_span, "query_executed", None)
  
  // Complete the spans
  Span::end(service_b_span)
  Span::end(db_span)
  
  // Service A: Complete after receiving response from Service B
  Span::add_event(service_a_span, "response_received", None)
  Span::end(service_a_span)
  
  // Verify trace continuity
  assert_eq(TraceContext::trace_id(service_a_context), TraceContext::trace_id(service_b_context))
  assert_eq(TraceContext::trace_id(service_b_context), TraceContext::trace_id(db_context))
  
  // Verify span hierarchy
  assert_eq(TraceContext::parent_span_id(service_b_child_context), Some(service_a_span_id))
  assert_eq(TraceContext::parent_span_id(db_context), Some(service_b_span_id))
}

// Test 4: Trace Chain Analysis
test "trace chain analysis" {
  // Create a complex trace chain
  let builder = TraceChainBuilder::new("api_request")
  
  // Root span
  let root_span = TraceChainBuilder::add_root_span(
    builder,
    "api_request",
    SpanKind::Server,
    [("http.method", StringValue("POST")), ("http.route", StringValue("/api/process"))]
  )
  
  // Authentication span
  let auth_span = TraceChainBuilder::add_child_span(
    builder,
    root_span,
    "authentication",
    SpanKind::Internal,
    [("auth.method", StringValue("jwt"))]
  )
  
  // Authorization span
  let authz_span = TraceChainBuilder::add_child_span(
    builder,
    auth_span,
    "authorization",
    SpanKind::Internal,
    [("authz.resource", StringValue("data"))]
  )
  
  // Business logic span
  let business_span = TraceChainBuilder::add_child_span(
    builder,
    root_span,
    "business_logic",
    SpanKind::Internal,
    [("operation.type", StringValue("process"))]
  )
  
  // Database spans
  let db_read_span = TraceChainBuilder::add_child_span(
    builder,
    business_span,
    "database_read",
    SpanKind::Client,
    [("db.operation", StringValue("SELECT"))]
  )
  
  let db_write_span = TraceChainBuilder::add_child_span(
    builder,
    business_span,
    "database_write",
    SpanKind::Client,
    [("db.operation", StringValue("INSERT"))]
  )
  
  // External service call
  let external_span = TraceChainBuilder::add_child_span(
    builder,
    business_span,
    "external_service_call",
    SpanKind::Client,
    [("service.name", StringValue("payment")), ("service.version", StringValue("v2"))]
  )
  
  // Build the trace chain
  let trace_chain = TraceChainBuilder::build(builder)
  
  // Analyze the trace chain
  let analysis = TraceChainAnalyzer::analyze(trace_chain)
  
  // Verify analysis results
  assert_eq(analysis.total_spans, 7)
  assert_eq(analysis.max_depth, 3)  // root -> auth -> authz
  assert_eq(analysis.service_count, 1)  // All spans in same service
  
  // Verify span types
  assert_eq(analysis.span_types.get("server"), Some(1))  // root span
  assert_eq(analysis.span_types.get("internal"), Some(3))  // auth, authz, business
  assert_eq(analysis.span_types.get("client"), Some(3))  // db_read, db_write, external
  
  // Verify critical path
  let critical_path = TraceChainAnalyzer::find_critical_path(trace_chain)
  assert_eq(critical_path.length(), 3)  // root -> business -> external (longest path)
}

// Test 5: Trace Chain Performance Metrics
test "trace chain performance metrics" {
  // Create a trace chain with timing information
  let builder = TraceChainBuilder::new("performance_test")
  
  let start_time = 1640995200000000L  // High precision timestamp
  
  // Root span
  let root_span = TraceChainBuilder::add_root_span_with_timing(
    builder,
    "root_operation",
    SpanKind::Server,
    [],
    start_time,
    start_time + 1000000L  // 1ms
  )
  
  // Child spans with different durations
  let fast_span = TraceChainBuilder::add_child_span_with_timing(
    builder,
    root_span,
    "fast_operation",
    SpanKind::Internal,
    [],
    start_time + 100000L,
    start_time + 200000L  // 0.1ms
  )
  
  let slow_span = TraceChainBuilder::add_child_span_with_timing(
    builder,
    root_span,
    "slow_operation",
    SpanKind::Internal,
    [],
    start_time + 200000L,
    start_time + 800000L  // 0.6ms
  )
  
  let parallel_span = TraceChainBuilder::add_child_span_with_timing(
    builder,
    root_span,
    "parallel_operation",
    SpanKind::Internal,
    [],
    start_time + 100000L,
    start_time + 900000L  // 0.8ms, overlaps with other spans
  )
  
  // Build the trace chain
  let trace_chain = TraceChainBuilder::build(builder)
  
  // Calculate performance metrics
  let metrics = TraceChainAnalyzer::calculate_performance_metrics(trace_chain)
  
  // Verify metrics
  assert_eq(metrics.total_duration, 1000000L)  // 1ms (root span duration)
  assert_eq(metrics.span_count, 4)
  
  // Verify slowest span
  match metrics.slowest_span {
    Some((span_name, duration)) => {
      assert_eq(span_name, "slow_operation")
      assert_eq(duration, 600000L)  // 0.6ms
    }
    None => assert_true(false)
  }
  
  // Verify fastest span
  match metrics.fastest_span {
    Some((span_name, duration)) => {
      assert_eq(span_name, "fast_operation")
      assert_eq(duration, 100000L)  // 0.1ms
    }
    None => assert_true(false)
  }
  
  // Verify parallelism calculation
  assert_true(metrics.max_parallelism >= 2)  // At least 2 spans overlap
}

// Test 6: Trace Chain Error Handling
test "trace chain error handling" {
  // Create a trace chain with errors
  let builder = TraceChainBuilder::new("error_test")
  
  // Root span
  let root_span = TraceChainBuilder::add_root_span(
    builder,
    "request_handler",
    SpanKind::Server,
    [("http.method", StringValue("GET"))]
  )
  
  // Span with error
  let error_span = TraceChainBuilder::add_child_span(
    builder,
    root_span,
    "database_operation",
    SpanKind::Client,
    [("db.operation", StringValue("SELECT"))]
  )
  
  // Simulate error in the span
  Span::set_status(error_span, StatusCode::Error, Some("Connection timeout"))
  Span::add_event(error_span, "exception", Some([
    ("exception.type", StringValue("TimeoutException")),
    ("exception.message", StringValue("Database connection timeout after 30s"))
  ]))
  
  // Span that handles the error
  let recovery_span = TraceChainBuilder::add_child_span(
    builder,
    root_span,
    "error_recovery",
    SpanKind::Internal,
    [("recovery.strategy", StringValue("fallback_cache"))]
  )
  
  Span::add_event(recovery_span, "fallback_used", Some([
    ("cache.key", StringValue("user_data"))
  ]))
  
  // Build the trace chain
  let trace_chain = TraceChainBuilder::build(builder)
  
  // Analyze errors in the trace chain
  let error_analysis = TraceChainAnalyzer::analyze_errors(trace_chain)
  
  // Verify error analysis
  assert_eq(error_analysis.error_count, 1)
  assert_eq(error_analysis.total_spans, 3)
  
  // Verify error details
  match error_analysis.errors.get(0) {
    Some(error) => {
      assert_eq(error.span_name, "database_operation")
      assert_eq(error.status_code, StatusCode::Error)
      match error.error_message {
        Some(msg) => assert_eq(msg, "Connection timeout")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // Verify error recovery detection
  assert_true(error_analysis.has_recovery)
  match error_analysis.recovery_strategy {
    Some(strategy) => assert_eq(strategy, "fallback_cache")
    None => assert_true(false)
  }
}

// Test 7: Trace Chain Sampling
test "trace chain sampling" {
  // Test different sampling strategies
  let always_on_sampler = AlwaysOnSampler::new()
  let always_off_sampler = AlwaysOffSampler::new()
  let rate_limiting_sampler = RateLimitingSampler::new(10)  // 10 traces per second
  let probability_sampler = ProbabilitySampler::new(0.1)  // 10% probability
  
  // Test always on sampler
  let trace_id_1 = TraceId::generate()
  let decision_1 = Sampler::should_sample(always_on_sampler, trace_id_1, "test_operation")
  match decision_1 {
    SamplingDecision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test always off sampler
  let trace_id_2 = TraceId::generate()
  let decision_2 = Sampler::should_sample(always_off_sampler, trace_id_2, "test_operation")
  match decision_2 {
    SamplingDecision::Drop => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test probability sampler with known trace ID for deterministic result
  let trace_id_3 = "40000000000000000000000000000000"  // Known to be sampled at 10%
  let decision_3 = Sampler::should_sample(probability_sampler, trace_id_3, "test_operation")
  // This specific trace ID should be sampled with probability sampler at 10%
  match decision_3 {
    SamplingDecision::RecordAndSample => assert_true(true)
    _ => assert_true(false)
  }
  
  // Test rate limiting sampler
  let trace_id_4 = TraceId::generate()
  let decision_4 = Sampler::should_sample(rate_limiting_sampler, trace_id_4, "test_operation")
  // First few traces should be sampled
  match decision_4 {
    SamplingDecision::RecordAndSample => assert_true(true)
    SamplingDecision::Drop => assert_true(true)  // Also valid if rate limit exceeded
    _ => assert_true(false)
  }
}

// Test 8: Trace Chain Context Baggage
test "trace chain context baggage" {
  // Create trace context with baggage
  let trace_id = TraceId::generate()
  let span_id = SpanId::generate()
  let trace_context = TraceContext::new(trace_id, span_id, 1)
  
  // Add baggage items
  let baggage = Baggage::new()
  let updated_baggage = Baggage::set_entry(baggage, "user.id", "12345")
  let updated_baggage = Baggage::set_entry(updated_baggage, "request.id", "req-67890")
  let updated_baggage = Baggage::set_entry(updated_baggage, "tenant.id", "tenant-001")
  
  // Create context with baggage
  let context_with_baggage = TraceContext::with_baggage(trace_context, updated_baggage)
  
  // Verify baggage propagation
  let retrieved_baggage = TraceContext::baggage(context_with_baggage)
  let user_id = Baggage::get_entry(retrieved_baggage, "user.id")
  match user_id {
    Some(value) => assert_eq(value, "12345")
    None => assert_true(false)
  }
  
  // Test baggage serialization
  let baggage_headers = Baggage::to_headers(retrieved_baggage)
  
  // Test baggage deserialization
  let deserialized_baggage = Baggage::from_headers(baggage_headers)
  let request_id = Baggage::get_entry(deserialized_baggage, "request.id")
  match request_id {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false)
  }
  
  // Test baggage removal
  let filtered_baggage = Baggage::remove_entry(deserialized_baggage, "tenant.id")
  let tenant_id = Baggage::get_entry(filtered_baggage, "tenant.id")
  match tenant_id {
    Some(_) => assert_true(false)  // Should be removed
    None => assert_true(true)
  }
}

// Test 9: Trace Chain Export
test "trace chain export" {
  // Create a trace chain
  let builder = TraceChainBuilder::new("export_test")
  
  let root_span = TraceChainBuilder::add_root_span(
    builder,
    "root_operation",
    SpanKind::Server,
    [("service.name", StringValue("test_service"))]
  )
  
  let child_span = TraceChainBuilder::add_child_span(
    builder,
    root_span,
    "child_operation",
    SpanKind::Internal,
    [("operation.type", StringValue("test"))]
  )
  
  // Build the trace chain
  let trace_chain = TraceChainBuilder::build(builder)
  
  // Export to JSON format
  let json_exporter = JsonTraceExporter::new()
  let json_data = TraceExporter::export(json_exporter, trace_chain)
  
  // Verify JSON contains expected data
  assert_true(json_data.contains("root_operation"))
  assert_true(json_data.contains("child_operation"))
  assert_true(json_data.contains("test_service"))
  
  // Export to Protocol Buffers format
  let protobuf_exporter = ProtobufTraceExporter::new()
  let protobuf_data = TraceExporter::export(protobuf_exporter, trace_chain)
  
  // Verify protobuf data is not empty
  assert_true(protobuf_data.length() > 0)
  
  // Export to Jaeger format
  let jaeger_exporter = JaegerTraceExporter::new("http://localhost:14268/api/traces")
  let jaeger_data = TraceExporter::export(jaeger_exporter, trace_chain)
  
  // Verify Jaeger format
  assert_true(jaeger_data.contains("spans"))
  assert_true(jaeger_data.contains("traceID"))
}

// Test 10: Trace Chain Filtering and Search
test "trace chain filtering and search" {
  // Create multiple trace chains
  let chains = []
  
  // Chain 1: Successful request
  let builder1 = TraceChainBuilder::new("success_request")
  let root1 = TraceChainBuilder::add_root_span(
    builder1,
    "api_request",
    SpanKind::Server,
    [("http.status_code", StringValue("200"))]
  )
  let chain1 = TraceChainBuilder::build(builder1)
  chains.push(chain1)
  
  // Chain 2: Failed request
  let builder2 = TraceChainBuilder::new("error_request")
  let root2 = TraceChainBuilder::add_root_span(
    builder2,
    "api_request",
    SpanKind::Server,
    [("http.status_code", StringValue("500"))]
  )
  let error_span = TraceChainBuilder::add_child_span(
    builder2,
    root2,
    "database_error",
    SpanKind::Client,
    [("error.type", StringValue("ConnectionError"))]
  )
  Span::set_status(error_span, StatusCode::Error, Some("Database connection failed"))
  let chain2 = TraceChainBuilder::build(builder2)
  chains.push(chain2)
  
  // Chain 3: Slow request
  let builder3 = TraceChainBuilder::new("slow_request")
  let start_time = 1640995200000000L
  let root3 = TraceChainBuilder::add_root_span_with_timing(
    builder3,
    "api_request",
    SpanKind::Server,
    [("http.status_code", StringValue("200"))],
    start_time,
    start_time + 5000000L  // 5ms
  )
  let chain3 = TraceChainBuilder::build(builder3)
  chains.push(chain3)
  
  // Filter by status code
  let success_chains = TraceChainFilter::by_attribute(chains, "http.status_code", "200")
  assert_eq(success_chains.length(), 2)
  
  let error_chains = TraceChainFilter::by_attribute(chains, "http.status_code", "500")
  assert_eq(error_chains.length(), 1)
  
  // Filter by error status
  let chains_with_errors = TraceChainFilter::by_error_status(chains, StatusCode::Error)
  assert_eq(chains_with_errors.length(), 1)
  
  // Filter by duration (> 1ms)
  let slow_chains = TraceChainFilter::by_min_duration(chains, 1000000L)
  assert_eq(slow_chains.length(), 1)
  
  // Search by span name
  let chains_with_db = TraceChainFilter::by_span_name(chains, "database_error")
  assert_eq(chains_with_db.length(), 1)
  
  // Complex filter: successful requests with duration > 1ms
  let complex_filter = TraceChainFilter::and([
    TraceChainFilter::by_attribute(chains, "http.status_code", "200"),
    TraceChainFilter::by_min_duration(chains, 1000000L)
  ])
  assert_eq(complex_filter.length(), 1)
}