// Enhanced Feature Tests for Azimuth Telemetry System
// This file contains test cases for enhanced telemetry features

// Test 1: Dynamic Attribute Filtering
test "dynamic attribute filtering" {
  let attrs = Attributes::new()
  Attributes::set(attrs, "service.name", StringValue("azimuth"))
  Attributes::set(attrs, "service.version", StringValue("1.0.0"))
  Attributes::set(attrs, "environment", StringValue("production"))
  Attributes::set(attrs, "trace.id", StringValue("12345"))
  Attributes::set(attrs, "debug.mode", StringValue("false"))
  
  // Test filtering by prefix
  let filtered_attrs = Attributes::filter_by_prefix(attrs, "service.")
  assert_eq(Attributes::size(filtered_attrs), 2)
  
  let service_name = Attributes::get(filtered_attrs, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth")
    _ => assert_true(false)
  }
  
  // Test filtering by value pattern
  let env_filtered = Attributes::filter_by_value_pattern(attrs, "prod")
  assert_eq(Attributes::size(env_filtered), 1)
  
  let env = Attributes::get(env_filtered, "environment")
  match env {
    Some(StringValue(value)) => assert_eq(value, "production")
    _ => assert_true(false)
  }
}

// Test 2: Advanced Span Linking
test "advanced span linking" {
  let parent_ctx = SpanContext::new("trace_001", "span_001", true, "parent_state")
  let child_ctx = SpanContext::new("trace_001", "span_002", true, "child_state")
  let linked_ctx = SpanContext::new("trace_002", "span_003", true, "linked_state")
  
  let span = Span::new("test_operation", Client, parent_ctx)
  
  // Test adding span links
  let link_attrs = Attributes::new()
  Attributes::set(link_attrs, "link.type", StringValue("causality"))
  
  Span::add_link(span, child_ctx, link_attrs)
  Span::add_link(span, linked_ctx, Attributes::new())
  
  let links = Span::links(span)
  assert_eq(links.length(), 2)
  
  let first_link = links[0]
  assert_eq(SpanContext::span_id(first_link.context), "span_002")
  assert_eq(Attributes::size(first_link.attributes), 1)
  
  // Test link retrieval by trace ID
  let trace_links = Span::get_links_by_trace_id(span, "trace_002")
  assert_eq(trace_links.length(), 1)
  assert_eq(SpanContext::span_id(trace_links[0].context), "span_003")
}

// Test 3: Metric Aggregation Strategies
test "metric aggregation strategies" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "aggregation_test")
  
  // Create counter with different aggregation strategies
  let counter = Meter::create_counter_with_aggregation(
    meter, 
    "request_count", 
    Some("Total request count"),
    Some("requests"),
    Sum
  )
  
  // Add measurements
  Counter::add(counter, 10.0)
  Counter::add(counter, 20.0)
  Counter::add(counter, 15.0)
  
  // Test histogram with explicit bucket boundaries
  let histogram = Meter::create_histogram_with_buckets(
    meter,
    "response_time",
    Some("Response time distribution"),
    Some("ms"),
    [10.0, 50.0, 100.0, 500.0, 1000.0]
  )
  
  Histogram::record(histogram, 5.0)   // Falls in first bucket
  Histogram::record(histogram, 25.0)  // Falls in second bucket
  Histogram::record(histogram, 75.0)  // Falls in third bucket
  Histogram::record(histogram, 200.0) // Falls in fourth bucket
  Histogram::record(histogram, 750.0) // Falls in fifth bucket
  Histogram::record(histogram, 1500.0)// Falls in overflow bucket
  
  // Test exponential histogram
  let exp_histogram = Meter::create_exponential_histogram(
    meter,
    "exp_response_time",
    Some("Exponential histogram for response times"),
    Some("ms")
  )
  
  for i in 1..=100 {
    Histogram::record(exp_histogram, (i * 10.0))
  }
}

// Test 4: Context Propagation with Custom Carriers
test "context propagation with custom carriers" {
  let ctx = Context::root()
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "test_state")
  let ctx_with_span = Context::with_value(ctx, ContextKey::new("span_context"), span_ctx)
  
  // Test custom text carrier
  let text_carrier = TextMapCarrier::new()
  let text_propagator = TraceContextPropagator::new()
  
  Propagator::inject(text_propagator, ctx_with_span, text_carrier)
  
  let extracted_ctx = Propagator::extract(text_propagator, text_carrier)
  let extracted_span_ctx = Context::get(extracted_ctx, ContextKey::new("span_context"))
  
  match extracted_span_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), "trace_123")
      assert_eq(SpanContext::span_id(ctx), "span_456")
    }
    None => assert_true(false)
  }
  
  // Test custom binary carrier
  let binary_carrier = BinaryCarrier::new()
  let binary_propagator = BinaryTraceContextPropagator::new()
  
  Propagator::inject(binary_propagator, ctx_with_span, binary_carrier)
  
  let binary_extracted_ctx = Propagator::extract(binary_propagator, binary_carrier)
  let binary_extracted_span_ctx = Context::get(binary_extracted_ctx, ContextKey::new("span_context"))
  
  match binary_extracted_span_ctx {
    Some(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), "trace_123")
      assert_eq(SpanContext::span_id(ctx), "span_456")
    }
    None => assert_true(false)
  }
}

// Test 5: Advanced Sampling Strategies
test "advanced sampling strategies" {
  // Test probability-based sampler
  let prob_sampler = ProbabilitySampler::new(0.5) // 50% sampling rate
  
  let trace_id_1 = "00000000000000000000000000000001" // Should be sampled
  let trace_id_2 = "ffffffffffffffffffffffffffffffff" // Should not be sampled
  
  let decision_1 = Sampler::should_sample(
    prob_sampler,
    Context::root(),
    trace_id_1,
    "test_span",
    Server,
    Attributes::new()
  )
  
  match decision_1 {
    RecordAndSample => assert_true(true)
    Drop => assert_true(false)
  }
  
  // Test rate-limiting sampler
  let rate_limit_sampler = RateLimitingSampler::new(10) // 10 traces per second
  
  for i in 1..=15 {
    let decision = Sampler::should_sample(
      rate_limit_sampler,
      Context::root(),
      trace_id_1,
      "test_span",
      Server,
      Attributes::new()
    )
    
    // First 10 should be sampled, rest should be dropped
    if i <= 10 {
      match decision {
        RecordAndSample => assert_true(true)
        Drop => assert_true(false)
      }
    } else {
      match decision {
        RecordAndSample => assert_true(false)
        Drop => assert_true(true)
      }
    }
  }
  
  // Test parent-based sampler
  let parent_sampler = ParentBasedSampler::new(RootSampler::new(AlwaysOn))
  
  let parent_ctx = Context::with_value(
    Context::root(),
    ContextKey::new("span_context"),
    SpanContext::new(trace_id_1, "parent_span", false, "parent_state")
  )
  
  let decision = Sampler::should_sample(
    parent_sampler,
    parent_ctx,
    trace_id_1,
    "child_span",
    Internal,
    Attributes::new()
  )
  
  // Should respect parent's sampling decision
  match decision {
    Drop => assert_true(true)
    RecordAndSample => assert_true(false)
  }
}

// Test 6: Enhanced Log Correlation
test "enhanced log correlation" {
  let provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(provider, "correlation_test")
  
  // Create span context for correlation
  let span_ctx = SpanContext::new("trace_789", "span_101", true, "correlation_state")
  let ctx_with_span = Context::with_value(
    Context::root(),
    ContextKey::new("span_context"),
    span_ctx
  )
  
  // Create log record with automatic correlation
  let log_record = LogRecord::new_with_context(
    Warn,
    Some("Operation completed with warnings"),
    Some(Attributes::new()),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_789"),
    Some("span_101"),
    Some(ctx_with_span)
  )
  
  // Add correlation attributes
  let correlation_attrs = [
    ("user.id", StringValue("user_123")),
    ("session.id", StringValue("session_456")),
    ("request.id", StringValue("req_789"))
  ]
  
  let enhanced_log = LogRecord::with_correlation_attributes(
    log_record,
    correlation_attrs
  )
  
  assert_eq(LogRecord::trace_id(enhanced_log), Some("trace_789"))
  assert_eq(LogRecord::span_id(enhanced_log), Some("span_101"))
  
  // Test log batch with correlation
  let log_batch = [
    LogRecord::new_with_context(
      Info,
      Some("Starting operation"),
      None,
      None,
      None,
      Some("trace_789"),
      Some("span_101"),
      Some(ctx_with_span)
    ),
    LogRecord::new_with_context(
      Debug,
      Some("Processing step 1"),
      None,
      None,
      None,
      Some("trace_789"),
      Some("span_101"),
      Some(ctx_with_span)
    ),
    enhanced_log
  ]
  
  Logger::emit_batch(logger, log_batch)
}

// Test 7: Resource Detection and Enrichment
test "resource detection and enrichment" {
  // Test automatic resource detection
  let detected_resource = ResourceDetector::detect()
  
  // Test manual resource enrichment
  let base_resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("azimuth-service")),
    ("service.version", StringValue("1.0.0"))
  ])
  
  let enriched_resource = Resource::enrich(base_resource, [
    ("host.name", StringValue("production-server-01")),
    ("host.ip", StringValue("192.168.1.100")),
    ("process.id", StringValue("12345")),
    ("process.executable.name", StringValue("azimuth")),
    ("process.executable.path", StringValue("/usr/bin/azimuth"))
  ])
  
  // Test merging with detected resources
  let final_resource = Resource::merge(detected_resource, enriched_resource)
  
  // Verify all attributes exist
  let service_name = Resource::get_attribute(final_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let host_name = Resource::get_attribute(final_resource, "host.name")
  match host_name {
    Some(StringValue(name)) => assert_eq(name, "production-server-01")
    _ => assert_true(false)
  }
  
  let process_id = Resource::get_attribute(final_resource, "process.id")
  match process_id {
    Some(StringValue(id)) => assert_eq(id, "12345")
    _ => assert_true(false)
  }
}

// Test 8: Metrics with Multi-dimensional Attributes
test "metrics with multi-dimensional attributes" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "multi_dimensional_test")
  
  // Create counter with multi-dimensional attributes
  let counter = Meter::create_counter(meter, "api_requests", Some("API request count"), Some("requests"))
  
  // Add measurements with different attribute combinations
  let attrs1 = Attributes::new()
  Attributes::set(attrs1, "endpoint", StringValue("/api/users"))
  Attributes::set(attrs1, "method", StringValue("GET"))
  Attributes::set(attrs1, "status_code", StringValue("200"))
  Attributes::set(attrs1, "user_type", StringValue("premium"))
  
  Counter::add(counter, 10.0, Some(attrs1))
  
  let attrs2 = Attributes::new()
  Attributes::set(attrs2, "endpoint", StringValue("/api/users"))
  Attributes::set(attrs2, "method", StringValue("POST"))
  Attributes::set(attrs2, "status_code", StringValue("201"))
  Attributes::set(attrs2, "user_type", StringValue("premium"))
  
  Counter::add(counter, 5.0, Some(attrs2))
  
  let attrs3 = Attributes::new()
  Attributes::set(attrs3, "endpoint", StringValue("/api/users"))
  Attributes::set(attrs3, "method", StringValue("GET"))
  Attributes::set(attrs3, "status_code", StringValue("404"))
  Attributes::set(attrs3, "user_type", StringValue("free"))
  
  Counter::add(counter, 3.0, Some(attrs3))
  
  // Test attribute filtering for metrics
  let filtered_metrics = Meter::get_measurements_by_attributes(
    meter,
    counter,
    [("endpoint", "/api/users"), ("method", "GET")]
  )
  
  assert_eq(filtered_metrics.length(), 2)
  
  // Test time series generation
  let time_series = Meter::generate_time_series(counter)
  assert_eq(time_series.length(), 3) // Three unique attribute combinations
}