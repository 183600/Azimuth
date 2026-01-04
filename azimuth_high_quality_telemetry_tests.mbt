// Azimuth High-Quality Telemetry Test Suite
// This file contains high-quality test cases focusing on advanced telemetry features

// Test 1: Distributed Tracing with Complex Hierarchies
test "distributed tracing with complex hierarchies" {
  // 1. Create root span
  let root_context = SpanContext({
    trace_id: "trace-abc123def456",
    span_id: "span-root001",
    sampled: true,
    trace_state: "service=api,env=production"
  })
  
  let root_span = Span({
    name: "api.request.processing",
    kind: Server,
    recording: true,
    span_context: root_context
  })
  
  // 2. Create child spans with different operations
  let auth_context = SpanContext({
    trace_id: "trace-abc123def456",
    span_id: "span-auth002",
    sampled: true,
    trace_state: "service=auth,env=production"
  })
  
  let auth_span = Span({
    name: "authentication.verify",
    kind: Internal,
    recording: true,
    span_context: auth_context
  })
  
  let db_context = SpanContext({
    trace_id: "trace-abc123def456",
    span_id: "span-db003",
    sampled: true,
    trace_state: "service=database,env=production"
  })
  
  let db_span = Span({
    name: "database.query.user_profile",
    kind: Client,
    recording: true,
    span_context: db_context
  })
  
  // 3. Create nested child spans
  let cache_context = SpanContext({
    trace_id: "trace-abc123def456",
    span_id: "span-cache004",
    sampled: true,
    trace_state: "service=cache,env=production"
  })
  
  let cache_span = Span({
    name: "cache.get.user_session",
    kind: Internal,
    recording: true,
    span_context: cache_context
  })
  
  // 4. Verify trace hierarchy
  let spans = [root_span, auth_span, db_span, cache_span]
  assert_eq(spans.length(), 4)
  
  // All spans should have the same trace ID
  for span in spans {
    assert_eq(span.span_context.trace_id, "trace-abc123def456")
    assert_true(span.span_context.sampled)
  }
  
  // Verify span relationships through trace state
  assert_true(root_span.span_context.trace_state.contains("service=api"))
  assert_true(auth_span.span_context.trace_state.contains("service=auth"))
  assert_true(db_span.span_context.trace_state.contains("service=database"))
  assert_true(cache_span.span_context.trace_state.contains("service=cache"))
  
  // Verify span names are unique and descriptive
  let span_names = spans.map(fn(s) { s.name })
  assert_true(span_names.contains("api.request.processing"))
  assert_true(span_names.contains("authentication.verify"))
  assert_true(span_names.contains("database.query.user_profile"))
  assert_true(span_names.contains("cache.get.user_session"))
}

// Test 2: Advanced Metrics with Multi-Dimensional Analysis
test "advanced metrics with multi-dimensional analysis" {
  // 1. Create dimensional metrics
  let request_metrics = [
    {
      name: "http.requests.total",
      value: 1250,
      attributes: [
        ("method", "GET"),
        ("status", "200"),
        ("service", "api-gateway"),
        ("region", "us-west-2")
      ]
    },
    {
      name: "http.requests.total",
      value: 45,
      attributes: [
        ("method", "GET"),
        ("status", "404"),
        ("service", "api-gateway"),
        ("region", "us-west-2")
      ]
    },
    {
      name: "http.requests.total",
      value: 23,
      attributes: [
        ("method", "POST"),
        ("status", "500"),
        ("service", "api-gateway"),
        ("region", "us-west-2")
      ]
    },
    {
      name: "http.request.duration",
      value: 125.5,
      attributes: [
        ("method", "GET"),
        ("endpoint", "/api/users"),
        ("service", "user-service"),
        ("region", "us-west-2")
      ]
    }
  ]
  
  // 2. Verify metrics structure
  assert_eq(request_metrics.length(), 4)
  
  // 3. Analyze metrics by dimensions
  let get_requests = request_metrics.filter(fn(m) {
    let mut found = false
    for (key, value) in m.attributes {
      if key == "method" and value == "GET" {
        found = true
      }
    }
    found
  })
  
  assert_eq(get_requests.length(), 2)
  
  // 4. Calculate success rate
  let success_requests = request_metrics.filter(fn(m) {
    let mut found = false
    for (key, value) in m.attributes {
      if key == "status" and value == "200" {
        found = true
      }
    }
    found
  })
  
  let total_requests = request_metrics.filter(fn(m) { m.name == "http.requests.total" })
  let success_count = success_requests.reduce(fn(acc, m) { acc + m.value }, 0)
  let total_count = total_requests.reduce(fn(acc, m) { acc + m.value }, 0)
  
  assert_eq(success_count, 1250)
  assert_eq(total_count, 1318)
  
  let success_rate = (success_count.to_float() / total_count.to_float()) * 100.0
  assert_true(success_rate > 94.0 and success_rate < 95.0)
  
  // 5. Verify metric attributes
  for metric in request_metrics {
    assert_true(metric.attributes.length() >= 3)
    
    // Verify all attributes have valid keys and values
    for (key, value) in metric.attributes {
      assert_true(key.length() > 0)
      assert_true(value.length() > 0)
    }
  }
}

// Test 3: Context Propagation Across Service Boundaries
test "context propagation across service boundaries" {
  // 1. Create initial context with baggage
  let initial_baggage = Baggage({ entries = [
    ("user.id", "user-12345"),
    ("request.id", "req-abc123"),
    ("session.id", "sess-def456"),
    ("correlation.id", "corr-ghi789")
  ] })
  
  let initial_context = Context({
    baggage: Some(initial_baggage),
    span_context: Some(SpanContext({
      trace_id: "trace-propagation-001",
      span_id: "span-initial-001",
      sampled: true,
      trace_state: "origin=frontend"
    }))
  })
  
  // 2. Simulate context propagation to API Gateway
  let api_gateway_context = Context({
    baggage: initial_context.baggage,
    span_context: Some(SpanContext({
      trace_id: "trace-propagation-001",
      span_id: "span-api-gateway-002",
      sampled: true,
      trace_state: "origin=frontend,service=api-gateway"
    }))
  })
  
  // 3. Simulate context propagation to Auth Service
  let auth_service_context = Context({
    baggage: api_gateway_context.baggage,
    span_context: Some(SpanContext({
      trace_id: "trace-propagation-001",
      span_id: "span-auth-service-003",
      sampled: true,
      trace_state: "origin=frontend,service=api-gateway,service=auth"
    }))
  })
  
  // 4. Add service-specific baggage
  let auth_baggage = match auth_service_context.baggage {
    None => Baggage({ entries = [] })
    Some(b) => b
  }
  
  let updated_auth_baggage = Baggage({ entries = auth_baggage.entries + [
    ("auth.method", "jwt"),
    ("auth.provider", "keycloak")
  ] })
  
  let final_auth_context = Context({
    baggage: Some(updated_auth_baggage),
    span_context: auth_service_context.span_context
  })
  
  // 5. Verify context propagation
  // All contexts should have the same trace ID
  match initial_context.span_context {
    None => assert_true(false)
    Some(sc) => {
      match api_gateway_context.span_context {
        None => assert_true(false)
        Some(api_sc) => assert_eq(sc.trace_id, api_sc.trace_id)
      }
      match auth_service_context.span_context {
        None => assert_true(false)
        Some(auth_sc) => assert_eq(sc.trace_id, auth_sc.trace_id)
      }
    }
  }
  
  // Verify baggage propagation and enrichment
  match final_auth_context.baggage {
    None => assert_true(false)
    Some(b) => {
      assert_true(b.entries.length() >= 6)
      
      // Verify original baggage entries are preserved
      let mut found_user_id = false
      let mut found_request_id = false
      let mut found_session_id = false
      let mut found_correlation_id = false
      let mut found_auth_method = false
      let mut found_auth_provider = false
      
      for (key, value) in b.entries {
        match key {
          "user.id" => {
            assert_eq(value, "user-12345")
            found_user_id = true
          }
          "request.id" => {
            assert_eq(value, "req-abc123")
            found_request_id = true
          }
          "session.id" => {
            assert_eq(value, "sess-def456")
            found_session_id = true
          }
          "correlation.id" => {
            assert_eq(value, "corr-ghi789")
            found_correlation_id = true
          }
          "auth.method" => {
            assert_eq(value, "jwt")
            found_auth_method = true
          }
          "auth.provider" => {
            assert_eq(value, "keycloak")
            found_auth_provider = true
          }
          _ => assert_true(false)
        }
      }
      
      assert_true(found_user_id)
      assert_true(found_request_id)
      assert_true(found_session_id)
      assert_true(found_correlation_id)
      assert_true(found_auth_method)
      assert_true(found_auth_provider)
    }
  }
}

// Test 4: Telemetry Data Serialization and Deserialization
test "telemetry data serialization and deserialization" {
  // 1. Create complex telemetry data structure
  let telemetry_data = {
    spans: [
      {
        name: "http.request",
        trace_id: "trace-serialization-001",
        span_id: "span-001",
        parent_span_id: None,
        kind: "Server",
        status: "Ok",
        start_time: 1640995200000,
        end_time: 1640995200250,
        attributes: [
          ("http.method", "GET"),
          ("http.url", "/api/users"),
          ("http.status_code", "200"),
          ("user.id", "user-123")
        ]
      },
      {
        name: "database.query",
        trace_id: "trace-serialization-001",
        span_id: "span-002",
        parent_span_id: Some("span-001"),
        kind: "Client",
        status: "Ok",
        start_time: 1640995200100,
        end_time: 1640995200200,
        attributes: [
          ("db.system", "postgresql"),
          ("db.statement", "SELECT * FROM users WHERE id = $1"),
          ("db.user", "app_user")
        ]
      }
    ],
    metrics: [
      {
        name: "http.requests.total",
        value: 1250,
        unit: "requests",
        attributes: [
          ("method", "GET"),
          ("status", "200"),
          ("service", "api-gateway")
        ]
      },
      {
        name: "http.request.duration",
        value: 125.5,
        unit: "ms",
        attributes: [
          ("method", "GET"),
          ("endpoint", "/api/users")
        ]
      }
    ],
    resources: [
      {
        attributes: [
          ("service.name", "user-service"),
          ("service.version", "1.2.3"),
          ("service.instance.id", "instance-001"),
          ("deployment.environment", "production"),
          ("cloud.provider", "aws"),
          ("cloud.region", "us-west-2")
        ]
      }
    ]
  }
  
  // 2. Serialize to JSON-like string representation
  let serialize_telemetry = fn(data) {
    let spans_json = data.spans.map(fn(span) {
      "{\n" +
      "  \"name\": \"" + span.name + "\",\n" +
      "  \"trace_id\": \"" + span.trace_id + "\",\n" +
      "  \"span_id\": \"" + span.span_id + "\",\n" +
      "  \"parent_span_id\": " + match span.parent_span_id {
        None => "null"
        Some(id) => "\"" + id + "\""
      } + ",\n" +
      "  \"kind\": \"" + span.kind + "\",\n" +
      "  \"status\": \"" + span.status + "\",\n" +
      "  \"start_time\": " + span.start_time.to_string() + ",\n" +
      "  \"end_time\": " + span.end_time.to_string() + ",\n" +
      "  \"attributes\": [" + span.attributes.map(fn(attr) {
        "\"" + attr.0 + "\": \"" + attr.1 + "\""
      }).join(", ") + "]\n" +
      "}"
    }).join(",\n")
    
    let metrics_json = data.metrics.map(fn(metric) {
      "{\n" +
      "  \"name\": \"" + metric.name + "\",\n" +
      "  \"value\": " + metric.value.to_string() + ",\n" +
      "  \"unit\": \"" + metric.unit + "\",\n" +
      "  \"attributes\": [" + metric.attributes.map(fn(attr) {
        "\"" + attr.0 + "\": \"" + attr.1 + "\""
      }).join(", ") + "]\n" +
      "}"
    }).join(",\n")
    
    let resources_json = data.resources.map(fn(resource) {
      "{\n" +
      "  \"attributes\": [" + resource.attributes.map(fn(attr) {
        "\"" + attr.0 + "\": \"" + attr.1 + "\""
      }).join(", ") + "]\n" +
      "}"
    }).join(",\n")
    
    "{\n" +
    "  \"spans\": [" + spans_json + "],\n" +
    "  \"metrics\": [" + metrics_json + "],\n" +
    "  \"resources\": [" + resources_json + "]\n" +
    "}"
  }
  
  // 3. Serialize the data
  let serialized_data = serialize_telemetry(telemetry_data)
  
  // 4. Verify serialization contains expected data
  assert_true(serialized_data.contains("http.request"))
  assert_true(serialized_data.contains("database.query"))
  assert_true(serialized_data.contains("http.requests.total"))
  assert_true(serialized_data.contains("http.request.duration"))
  assert_true(serialized_data.contains("service.name"))
  assert_true(serialized_data.contains("user-service"))
  assert_true(serialized_data.contains("trace-serialization-001"))
  
  // 5. Parse key information from serialized data
  let extract_trace_ids = fn(serialized: String) {
    let mut trace_ids = []
    let lines = serialized.split("\n")
    for line in lines {
      if line.contains("\"trace_id\":") {
        let start = line.find("\"trace_id\": \"").unwrap() + 14
        let end = line.find("\",").unwrap()
        let trace_id = line.substring(start, end - start)
        trace_ids = trace_ids.push(trace_id)
      }
    }
    trace_ids
  }
  
  let extracted_trace_ids = extract_trace_ids(serialized_data)
  assert_eq(extracted_trace_ids.length(), 2)
  assert_true(extracted_trace_ids.contains("trace-serialization-001"))
  
  // 6. Verify span relationships are preserved
  assert_true(serialized_data.contains("\"parent_span_id\": null"))
  assert_true(serialized_data.contains("\"parent_span_id\": \"span-001\""))
  
  // 7. Verify attribute serialization
  assert_true(serialized_data.contains("\"http.method\": \"GET\""))
  assert_true(serialized_data.contains("\"db.system\": \"postgresql\""))
  assert_true(serialized_data.contains("\"service.name\": \"user-service\""))
}

// Test 5: Telemetry Sampling Strategies
test "telemetry sampling strategies" {
  // 1. Define sampling strategies
  enum SamplingStrategy {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)  // Sample ratio between 0.0 and 1.0
    ParentBased(Box[SamplingStrategy])
    AttributeBased(String, String)  // Sample if attribute key has specific value
  }
  
  // 2. Create sampling decisions
  let always_on_strategy = SamplingStrategy::AlwaysOn
  let always_off_strategy = SamplingStrategy::AlwaysOff
  let ratio_strategy = SamplingStrategy::TraceIdRatio(0.1)  // 10% sampling
  let parent_based_strategy = SamplingStrategy::ParentBased(Box::new(SamplingStrategy::AlwaysOn))
  let attribute_based_strategy = SamplingStrategy::AttributeBased("service.name", "critical-service")
  
  // 3. Create sampling decision function
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String, attributes: Array[(String, String)], parent_sampled: Option[Bool]) {
    match strategy {
      SamplingStrategy::AlwaysOn => true,
      SamplingStrategy::AlwaysOff => false,
      SamplingStrategy::TraceIdRatio(ratio) => {
        // Simple hash-based sampling using trace ID
        let hash = trace_id.length() % 100
        hash.to_float() < (ratio * 100.0)
      },
      SamplingStrategy::ParentBased(base_strategy) => {
        match parent_sampled {
          None => should_sample(base_strategy, trace_id, attributes, None),
          Some(sampled) => sampled
        }
      },
      SamplingStrategy::AttributeBased(key, expected_value) => {
        let mut found = false
        for (attr_key, attr_value) in attributes {
          if attr_key == key and attr_value == expected_value {
            found = true
          }
        }
        found
      }
    }
  }
  
  // 4. Test AlwaysOn strategy
  assert_true(should_sample(always_on_strategy, "trace-001", [], None))
  assert_true(should_sample(always_on_strategy, "trace-002", [("service", "test")], None))
  
  // 5. Test AlwaysOff strategy
  assert_false(should_sample(always_off_strategy, "trace-003", [], None))
  assert_false(should_sample(always_off_strategy, "trace-004", [("service", "test")], None))
  
  // 6. Test TraceIdRatio strategy
  let short_trace_id = "trace-5"  // Length 10, 10 % 100 = 10
  let long_trace_id = "trace-very-long-trace-id-12345"  // Length 32, 32 % 100 = 32
  
  // With 10% ratio, trace IDs with hash < 10 should be sampled
  assert_true(should_sample(ratio_strategy, short_trace_id, [], None))  // 10 < 10 (edge case)
  assert_false(should_sample(ratio_strategy, long_trace_id, [], None))  // 32 >= 10
  
  // 7. Test ParentBased strategy
  // With parent sampled
  assert_true(should_sample(parent_based_strategy, "trace-005", [], Some(true)))
  assert_false(should_sample(parent_based_strategy, "trace-006", [], Some(false)))
  
  // Without parent (fallback to base strategy)
  assert_true(should_sample(parent_based_strategy, "trace-007", [], None))
  
  // 8. Test AttributeBased strategy
  let critical_service_attrs = [
    ("service.name", "critical-service"),
    ("service.version", "1.0.0")
  ]
  
  let normal_service_attrs = [
    ("service.name", "normal-service"),
    ("service.version", "1.0.0")
  ]
  
  assert_true(should_sample(attribute_based_strategy, "trace-008", critical_service_attrs, None))
  assert_false(should_sample(attribute_based_strategy, "trace-009", normal_service_attrs, None))
  
  // 9. Test complex sampling scenarios
  let complex_strategy = SamplingStrategy::ParentBased(
    Box::new(SamplingStrategy::AttributeBased("environment", "production"))
  )
  
  let production_attrs = [("environment", "production")]
  let staging_attrs = [("environment", "staging")]
  
  // With parent sampled, should respect parent decision
  assert_true(should_sample(complex_strategy, "trace-010", production_attrs, Some(true)))
  assert_false(should_sample(complex_strategy, "trace-011", production_attrs, Some(false)))
  
  // Without parent, should use attribute-based sampling
  assert_true(should_sample(complex_strategy, "trace-012", production_attrs, None))
  assert_false(should_sample(complex_strategy, "trace-013", staging_attrs, None))
}

// Test 6: Telemetry Performance Optimization
test "telemetry performance optimization" {
  // 1. Create performance monitoring utilities
  let create_performance_monitor = fn() {
    {
      start_time: 0,
      end_time: 0,
      memory_usage: 0,
      cpu_usage: 0.0,
      operation_count: 0
    }
  }
  
  let start_monitoring = fn(monitor) {
    { monitor | start_time: 1640995200000, operation_count: 0 }
  }
  
  let stop_monitoring = fn(monitor) {
    { monitor | end_time: 1640995201000 }
  }
  
  let record_operation = fn(monitor) {
    { monitor | operation_count: monitor.operation_count + 1 }
  }
  
  // 2. Test batch processing optimization
  let process_spans_batch = fn(spans: Array[String], batch_size: Int) {
    let mut batches = []
    let mut current_batch = []
    
    for span in spans {
      current_batch = current_batch.push(span)
      
      if current_batch.length() >= batch_size {
        batches = batches.push(current_batch)
        current_batch = []
      }
    }
    
    if current_batch.length() > 0 {
      batches = batches.push(current_batch)
    }
    
    batches
  }
  
  // 3. Create test spans
  let test_spans = []
  for i in 0..=100 {
    test_spans = test_spans.push("span-" + i.to_string())
  }
  
  // 4. Test batch processing with different batch sizes
  let small_batches = process_spans_batch(test_spans, 10)
  let medium_batches = process_spans_batch(test_spans, 25)
  let large_batches = process_spans_batch(test_spans, 50)
  
  assert_eq(small_batches.length(), 11)  // 10 batches of 10 + 1 batch of 1
  assert_eq(medium_batches.length(), 5)  // 4 batches of 25 + 1 batch of 1
  assert_eq(large_batches.length(), 3)   // 2 batches of 50 + 1 batch of 1
  
  // Verify total spans are preserved
  let small_total = small_batches.reduce(fn(acc, batch) { acc + batch.length() }, 0)
  let medium_total = medium_batches.reduce(fn(acc, batch) { acc + batch.length() }, 0)
  let large_total = large_batches.reduce(fn(acc, batch) { acc + batch.length() }, 0)
  
  assert_eq(small_total, 101)
  assert_eq(medium_total, 101)
  assert_eq(large_total, 101)
  
  // 5. Test memory-efficient attribute processing
  let process_attributes_efficiently = fn(attributes: Array[(String, String)]) {
    let mut result = []
    let mut seen_keys = []
    
    for (key, value) in attributes {
      if not(seen_keys.contains(key)) {
        result = result.push((key, value))
        seen_keys = seen_keys.push(key)
      }
    }
    
    result
  }
  
  let duplicate_attributes = [
    ("service.name", "api-service"),
    ("service.version", "1.0.0"),
    ("service.name", "api-service"),  // Duplicate
    ("environment", "production"),
    ("service.version", "1.0.0"),     // Duplicate
    ("region", "us-west-2")
  ]
  
  let unique_attributes = process_attributes_efficiently(duplicate_attributes)
  assert_eq(unique_attributes.length(), 4)  // Only unique keys
  
  // Verify uniqueness
  let unique_keys = unique_attributes.map(fn(attr) { attr.0 })
  for key in unique_keys {
    let occurrences = unique_attributes.filter(fn(attr) { attr.0 == key }).length()
    assert_eq(occurrences, 1)
  }
  
  // 6. Test compression simulation for telemetry data
  let compress_telemetry_data = fn(data: String) {
    // Simple compression simulation: remove repeated patterns
    let compressed = data
      .replace("\"trace_id\": \"", "\"t\":\"")
      .replace("\"span_id\": \"", "\"s\":\"")
      .replace("\"name\": \"", "\"n\":\"")
      .replace("\"attributes\": [", "\"a\":[")
      .replace("\"value\": ", "\"v\":")
    
    compressed
  }
  
  let original_data = "{ \"trace_id\": \"trace-123\", \"span_id\": \"span-456\", \"name\": \"test\", \"attributes\": [], \"value\": 42 }"
  let compressed_data = compress_telemetry_data(original_data)
  
  assert_true(compressed_data.length() < original_data.length())
  assert_true(compressed_data.contains("\"t\":\"trace-123\""))
  assert_true(compressed_data.contains("\"s\":\"span-456\""))
  assert_true(compressed_data.contains("\"n\":\"test\""))
  assert_true(compressed_data.contains("\"a\":[]"))
  assert_true(compressed_data.contains("\"v\":42"))
  
  // 7. Test async processing simulation
  let process_async_operations = fn(operations: Array[String]) {
    let mut results = []
    
    // Simulate async processing by processing in parallel (simplified)
    for op in operations {
      let result = op + "_processed"
      results = results.push(result)
    }
    
    results
  }
  
  let async_operations = ["op1", "op2", "op3", "op4", "op5"]
  let async_results = process_async_operations(async_operations)
  
  assert_eq(async_results.length(), 5)
  assert_true(async_results.contains("op1_processed"))
  assert_true(async_results.contains("op2_processed"))
  assert_true(async_results.contains("op3_processed"))
  assert_true(async_results.contains("op4_processed"))
  assert_true(async_results.contains("op5_processed"))
}

// Test 7: Telemetry Error Handling and Recovery
test "telemetry error handling and recovery" {
  // 1. Define telemetry error types
  enum TelemetryError {
    SpanCreationFailed(String)
    MetricCollectionFailed(String)
    ContextPropagationError(String)
    SerializationError(String)
    NetworkError(String)
    ConfigurationError(String)
  }
  
  // 2. Create error handling utilities
  let create_safe_span = fn(name: String) {
    if name.length() == 0 {
      Err(TelemetryError::SpanCreationFailed("Empty span name"))
    } else if name.length() > 50 {
      Err(TelemetryError::SpanCreationFailed("Span name too long"))
    } else {
      let span_context = SpanContext({
        trace_id: "trace-safe-001",
        span_id: "span-safe-001",
        sampled: true,
        trace_state: ""
      })
      
      let span = Span({
        name: name,
        kind: Internal,
        recording: true,
        span_context: span_context
      })
      
      Ok(span)
    }
  }
  
  let create_safe_metric = fn(name: String, value: Float) {
    if name.length() == 0 {
      Err(TelemetryError::MetricCollectionFailed("Empty metric name"))
    } else if value < 0.0 {
      Err(TelemetryError::MetricCollectionFailed("Negative metric value"))
    } else {
      let metric = {
        name: name,
        value: value,
        unit: "unit",
        attributes: []
      }
      Ok(metric)
    }
  }
  
  // 3. Test successful operations
  let valid_span_result = create_safe_span("valid.span.name")
  match valid_span_result {
    Ok(span) => {
      assert_eq(span.name, "valid.span.name")
      assert_eq(span.span_context.trace_id, "trace-safe-001")
    }
    Err(_) => assert_true(false)
  }
  
  let valid_metric_result = create_safe_metric("valid.metric", 42.5)
  match valid_metric_result {
    Ok(metric) => {
      assert_eq(metric.name, "valid.metric")
      assert_eq(metric.value, 42.5)
    }
    Err(_) => assert_true(false)
  }
  
  // 4. Test error conditions
  let empty_span_result = create_safe_span("")
  match empty_span_result {
    Ok(_) => assert_true(false)
    Err(TelemetryError::SpanCreationFailed(msg)) => {
      assert_eq(msg, "Empty span name")
    }
    Err(_) => assert_true(false)
  }
  
  let long_span_result = create_safe_span("a".repeat(51))
  match long_span_result {
    Ok(_) => assert_true(false)
    Err(TelemetryError::SpanCreationFailed(msg)) => {
      assert_eq(msg, "Span name too long")
    }
    Err(_) => assert_true(false)
  }
  
  let empty_metric_result = create_safe_metric("", 10.0)
  match empty_metric_result {
    Ok(_) => assert_true(false)
    Err(TelemetryError::MetricCollectionFailed(msg)) => {
      assert_eq(msg, "Empty metric name")
    }
    Err(_) => assert_true(false)
  }
  
  let negative_metric_result = create_safe_metric("negative.metric", -5.0)
  match negative_metric_result {
    Ok(_) => assert_true(false)
    Err(TelemetryError::MetricCollectionFailed(msg)) => {
      assert_eq(msg, "Negative metric value")
    }
    Err(_) => assert_true(false)
  }
  
  // 5. Test error recovery with fallback values
  let create_span_with_fallback = fn(name: String, fallback_name: String) {
    match create_safe_span(name) {
      Ok(span) => Ok(span),
      Err(_) => create_safe_span(fallback_name)
    }
  }
  
  let fallback_span_result = create_span_with_fallback("", "fallback.span")
  match fallback_span_result {
    Ok(span) => {
      assert_eq(span.name, "fallback.span")
    }
    Err(_) => assert_true(false)
  }
  
  // 6. Test error aggregation
  let process_multiple_operations = fn(operations: Array[String]) {
    let mut results = []
    let mut errors = []
    
    for op in operations {
      match create_safe_span(op) {
        Ok(span) => results = results.push(span),
        Err(error) => errors = errors.push(error)
      }
    }
    
    { results, errors }
  }
  
  let test_operations = ["valid1", "", "valid2", "a".repeat(51), "valid3"]
  let operation_results = process_multiple_operations(test_operations)
  
  assert_eq(operation_results.results.length(), 3)
  assert_eq(operation_results.errors.length(), 2)
  
  // Verify successful operations
  let result_names = operation_results.results.map(fn(span) { span.name })
  assert_true(result_names.contains("valid1"))
  assert_true(result_names.contains("valid2"))
  assert_true(result_names.contains("valid3"))
  
  // Verify error types
  for error in operation_results.errors {
    match error {
      TelemetryError::SpanCreationFailed(msg) => {
        assert_true(msg == "Empty span name" or msg == "Span name too long")
      }
      _ => assert_true(false)
    }
  }
  
  // 7. Test circuit breaker pattern
  let create_circuit_breaker = fn(failure_threshold: Int) {
    {
      failure_count: 0,
      failure_threshold: failure_threshold,
      state: "closed"  // closed, open, half-open
    }
  }
  
  let call_with_circuit_breaker = fn(circuit_breaker, operation: () -> Result[Span]) {
    if circuit_breaker.state == "open" {
      Err(TelemetryError::NetworkError("Circuit breaker is open"))
    } else {
      match operation() {
        Ok(result) => {
          // Reset failure count on success
          let updated_breaker = { circuit_breaker | failure_count: 0, state: "closed" }
          Ok((result, updated_breaker))
        }
        Err(error) => {
          let new_failure_count = circuit_breaker.failure_count + 1
          let new_state = if new_failure_count >= circuit_breaker.failure_threshold {
            "open"
          } else {
            "closed"
          }
          
          let updated_breaker = { circuit_breaker | failure_count: new_failure_count, state: new_state }
          Err((error, updated_breaker))
        }
      }
    }
  }
  
  let circuit_breaker = create_circuit_breaker(3)
  
  // Test successful call
  let success_result = call_with_circuit_breaker(circuit_breaker, fn() { create_safe_span("success") })
  match success_result {
    Ok((span, updated_breaker)) => {
      assert_eq(span.name, "success")
      assert_eq(updated_breaker.failure_count, 0)
      assert_eq(updated_breaker.state, "closed")
    }
    Err(_) => assert_true(false)
  }
  
  // Test multiple failures leading to circuit breaker opening
  let mut current_breaker = circuit_breaker
  for i in 0..=3 {
    let failure_result = call_with_circuit_breaker(current_breaker, fn() { create_safe_span("") })
    match failure_result {
      Ok(_) => assert_true(false),
      Err((error, updated_breaker)) => {
        current_breaker = updated_breaker
        if i < 2 {
          assert_eq(updated_breaker.state, "closed")
        } else {
          assert_eq(updated_breaker.state, "open")
        }
      }
    }
  }
  
  // Test that circuit breaker rejects calls when open
  let open_circuit_result = call_with_circuit_breaker(current_breaker, fn() { create_safe_span("should.fail") })
  match open_circuit_result {
    Ok(_) => assert_true(false),
    Err((TelemetryError::NetworkError(msg), _)) => {
      assert_eq(msg, "Circuit breaker is open")
    }
    Err(_) => assert_true(false)
  }
}

// Test 8: Telemetry Data Analysis and Insights
test "telemetry data analysis and insights" {
  // 1. Create sample telemetry data for analysis
  let telemetry_dataset = {
    requests: [
      {
        timestamp: 1640995200,
        service: "api-gateway",
        method: "GET",
        endpoint: "/api/users",
        status: 200,
        duration: 125,
        user_id: "user-001"
      },
      {
        timestamp: 1640995205,
        service: "api-gateway",
        method: "GET",
        endpoint: "/api/users",
        status: 200,
        duration: 98,
        user_id: "user-002"
      },
      {
        timestamp: 1640995210,
        service: "api-gateway",
        method: "POST",
        endpoint: "/api/orders",
        status: 201,
        duration: 250,
        user_id: "user-001"
      },
      {
        timestamp: 1640995215,
        service: "api-gateway",
        method: "GET",
        endpoint: "/api/users",
        status: 404,
        duration: 45,
        user_id: "user-003"
      },
      {
        timestamp: 1640995220,
        service: "api-gateway",
        method: "POST",
        endpoint: "/api/orders",
        status: 500,
        duration: 1200,
        user_id: "user-002"
      }
    ],
    spans: [
      {
        name: "http.request",
        service: "api-gateway",
        start_time: 1640995200,
        end_time: 1640995325,
        status: "ok"
      },
      {
        name: "database.query",
        service: "user-service",
        start_time: 1640995205,
        end_time: 1640995255,
        status: "ok"
      },
      {
        name: "cache.get",
        service: "user-service",
        start_time: 1640995210,
        end_time: 1640995215,
        status: "ok"
      },
      {
        name: "external.api.call",
        service: "order-service",
        start_time: 1640995220,
        end_time: 1640995420,
        status: "error"
      }
    ]
  }
  
  // 2. Analyze request patterns
  let analyze_request_patterns = fn(requests) {
    let mut total_requests = requests.length()
    let mut successful_requests = 0
    let mut error_requests = 0
    let mut total_duration = 0
    
    let mut method_counts = {}
    let mut endpoint_counts = {}
    let mut status_counts = {}
    let mut user_requests = {}
    
    for request in requests {
      // Count status codes
      if request.status >= 200 and request.status < 300 {
        successful_requests = successful_requests + 1
      } else {
        error_requests = error_requests + 1
      }
      
      total_duration = total_duration + request.duration
      
      // Count methods
      let method_count = match method_counts.get(request.method) {
        None => 0
        Some(count) => count
      }
      method_counts = method_counts.set(request.method, method_count + 1)
      
      // Count endpoints
      let endpoint_count = match endpoint_counts.get(request.endpoint) {
        None => 0
        Some(count) => count
      }
      endpoint_counts = endpoint_counts.set(request.endpoint, endpoint_count + 1)
      
      // Count status codes
      let status_count = match status_counts.get(request.status.to_string()) {
        None => 0
        Some(count) => count
      }
      status_counts = status_counts.set(request.status.to_string(), status_count + 1)
      
      // Count user requests
      let user_count = match user_requests.get(request.user_id) {
        None => 0
        Some(count) => count
      }
      user_requests = user_requests.set(request.user_id, user_count + 1)
    }
    
    {
      total_requests,
      successful_requests,
      error_requests,
      success_rate: (successful_requests.to_float() / total_requests.to_float()) * 100.0,
      average_duration: total_duration / total_requests,
      method_counts,
      endpoint_counts,
      status_counts,
      user_requests
    }
  }
  
  // 3. Analyze span performance
  let analyze_span_performance = fn(spans) {
    let mut span_durations = []
    let mut error_spans = 0
    
    for span in spans {
      let duration = span.end_time - span.start_time
      span_durations = span_durations.push({
        name: span.name,
        service: span.service,
        duration: duration,
        status: span.status
      })
      
      if span.status == "error" {
        error_spans = error_spans + 1
      }
    }
    
    // Calculate statistics
    let total_duration = span_durations.reduce(fn(acc, span) { acc + span.duration }, 0)
    let average_duration = total_duration / span_durations.length()
    
    let mut min_duration = span_durations[0].duration
    let mut max_duration = span_durations[0].duration
    
    for span in span_durations {
      if span.duration < min_duration {
        min_duration = span.duration
      }
      if span.duration > max_duration {
        max_duration = span.duration
      }
    }
    
    {
      total_spans: span_durations.length(),
      error_spans,
      error_rate: (error_spans.to_float() / span_durations.length().to_float()) * 100.0,
      average_duration,
      min_duration,
      max_duration,
      span_details: span_durations
    }
  }
  
  // 4. Perform analysis
  let request_analysis = analyze_request_patterns(telemetry_dataset.requests)
  let span_analysis = analyze_span_performance(telemetry_dataset.spans)
  
  // 5. Verify request analysis results
  assert_eq(request_analysis.total_requests, 5)
  assert_eq(request_analysis.successful_requests, 3)
  assert_eq(request_analysis.error_requests, 2)
  assert_eq(request_analysis.success_rate, 60.0)
  assert_eq(request_analysis.average_duration, 343.6)  // (125 + 98 + 250 + 45 + 1200) / 5
  
  // Verify method counts
  assert_eq(request_analysis.method_counts.get("GET").unwrap(), 3)
  assert_eq(request_analysis.method_counts.get("POST").unwrap(), 2)
  
  // Verify endpoint counts
  assert_eq(request_analysis.endpoint_counts.get("/api/users").unwrap(), 3)
  assert_eq(request_analysis.endpoint_counts.get("/api/orders").unwrap(), 2)
  
  // Verify status counts
  assert_eq(request_analysis.status_counts.get("200").unwrap(), 2)
  assert_eq(request_analysis.status_counts.get("201").unwrap(), 1)
  assert_eq(request_analysis.status_counts.get("404").unwrap(), 1)
  assert_eq(request_analysis.status_counts.get("500").unwrap(), 1)
  
  // Verify user request counts
  assert_eq(request_analysis.user_requests.get("user-001").unwrap(), 2)
  assert_eq(request_analysis.user_requests.get("user-002").unwrap(), 2)
  assert_eq(request_analysis.user_requests.get("user-003").unwrap(), 1)
  
  // 6. Verify span analysis results
  assert_eq(span_analysis.total_spans, 4)
  assert_eq(span_analysis.error_spans, 1)
  assert_eq(span_analysis.error_rate, 25.0)
  assert_eq(span_analysis.average_duration, 75)  // (125 + 50 + 5 + 200) / 4
  assert_eq(span_analysis.min_duration, 5)
  assert_eq(span_analysis.max_duration, 200)
  
  // 7. Identify performance insights
  let identify_slow_operations = fn(span_details) {
    let average_duration = span_details.reduce(fn(acc, span) { acc + span.duration }, 0) / span_details.length()
    
    span_details.filter(fn(span) { span.duration > average_duration * 1.5 })
  }
  
  let slow_operations = identify_slow_operations(span_analysis.span_details)
  assert_eq(slow_operations.length(), 2)  // external.api.call (200) and http.request (125) > 75 * 1.5 = 112.5
  
  // 8. Identify error patterns
  let identify_error_patterns = fn(requests) {
    let mut error_by_endpoint = {}
    let mut error_by_user = {}
    
    for request in requests {
      if request.status >= 400 {
        let endpoint_errors = match error_by_endpoint.get(request.endpoint) {
          None => 0
          Some(count) => count
        }
        error_by_endpoint = error_by_endpoint.set(request.endpoint, endpoint_errors + 1)
        
        let user_errors = match error_by_user.get(request.user_id) {
          None => 0
          Some(count) => count
        }
        error_by_user = error_by_user.set(request.user_id, user_errors + 1)
      }
    }
    
    { error_by_endpoint, error_by_user }
  }
  
  let error_patterns = identify_error_patterns(telemetry_dataset.requests)
  assert_eq(error_patterns.error_by_endpoint.get("/api/users").unwrap(), 1)
  assert_eq(error_patterns.error_by_endpoint.get("/api/orders").unwrap(), 1)
  assert_eq(error_patterns.error_by_user.get("user-002").unwrap(), 1)
  assert_eq(error_patterns.error_by_user.get("user-003").unwrap(), 1)
}