// Azimuth Telemetry System - MoonBit Integration Tests
// This file contains test cases for end-to-end integration scenarios

// Test 1: End-to-end trace flow
test "end-to-end trace flow" {
  // Create a complete trace flow with multiple spans
  
  // 1. Create root span
  let root_context = SpanContext({
    trace_id: "trace-123456789",
    span_id: "span-root-111",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  let root_span = Span({
    name: "http.request",
    context: root_context,
    kind: Server,
    parent_span_id: None,
    start_time: 1640995200000,
    end_time: None,
    status: Unset,
    attributes: Attributes({ values = [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("https://api.example.com/data")),
      ("http.host", StringValue("api.example.com"))
    ] }),
    events = [],
    links = []
  })
  
  // 2. Create child span for database operation
  let db_context = SpanContext({
    trace_id: "trace-123456789",  // Same trace
    span_id: "span-db-222",       // Different span
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  let db_span = Span({
    name: "db.query",
    context: db_context,
    kind: Client,
    parent_span_id: Some("span-root-111"),  // Parent reference
    start_time: 1640995200100,  // Starts after parent
    end_time: None,
    status: Unset,
    attributes: Attributes({ values = [
      ("db.system", StringValue("postgresql")),
      ("db.statement", StringValue("SELECT * FROM users WHERE id = $1")),
      ("db.user", StringValue("app_user"))
    ] }),
    events = [],
    links = []
  })
  
  // 3. Create grandchild span for cache operation
  let cache_context = SpanContext({
    trace_id: "trace-123456789",  // Same trace
    span_id: "span-cache-333",    // Different span
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  let cache_span = Span({
    name: "cache.get",
    context: cache_context,
    kind: Internal,
    parent_span_id: Some("span-db-222"),  // Parent reference
    start_time: 1640995200200,  // Starts after parent
    end_time: None,
    status: Unset,
    attributes: Attributes({ values = [
      ("cache.system", StringValue("redis")),
      ("cache.key", StringValue("user:12345"))
    ] }),
    events = [],
    links = []
  })
  
  // 4. Complete all spans
  let completed_cache = { cache_span with 
    end_time = Some(1640995200250),
    status = Ok
  }
  
  let completed_db = { db_span with 
    end_time = Some(1640995200300),
    status = Ok,
    events = [
      SpanEvent({
        name: "db.query.start",
        timestamp: 1640995200100,
        attributes: Attributes({ values = [] })
      }),
      SpanEvent({
        name: "db.query.end",
        timestamp: 1640995200300,
        attributes: Attributes({ values = [
          ("db.rows_affected", IntValue(1))
        ] })
      })
    ]
  }
  
  let completed_root = { root_span with 
    end_time = Some(1640995200400),
    status = Ok,
    events = [
      SpanEvent({
        name: "request.received",
        timestamp: 1640995200000,
        attributes: Attributes({ values = [] })
      }),
      SpanEvent({
        name: "request.completed",
        timestamp: 1640995200400,
        attributes: Attributes({ values = [
          ("http.status_code", IntValue(200))
        ] })
      })
    ]
  }
  
  // Verify trace flow
  // All spans should have same trace ID
  assert_eq(completed_root.context.trace_id, completed_db.context.trace_id)
  assert_eq(completed_db.context.trace_id, completed_cache.context.trace_id)
  
  // Verify parent-child relationships
  match completed_db.parent_span_id {
    None => assert_true(false)
    Some(parent_id) => assert_eq(parent_id, completed_root.context.span_id)
  }
  
  match completed_cache.parent_span_id {
    None => assert_true(false)
    Some(parent_id) => assert_eq(parent_id, completed_db.context.span_id)
  }
  
  // Verify timing
  assert_true(completed_db.start_time >= completed_root.start_time)
  assert_true(completed_cache.start_time >= completed_db.start_time)
  
  match completed_cache.end_time {
    None => assert_true(false)
    Some(end_time) => assert_true(end_time >= completed_cache.start_time)
  }
  
  match completed_db.end_time {
    None => assert_true(false)
    Some(end_time) => assert_true(end_time >= completed_db.start_time)
  }
  
  match completed_root.end_time {
    None => assert_true(false)
    Some(end_time) => assert_true(end_time >= completed_root.start_time)
  }
  
  // Verify child completes within parent's lifetime
  match completed_root.end_time {
    None => assert_true(false)
    Some(root_end) => {
      match completed_db.end_time {
        None => assert_true(false)
        Some(db_end) => assert_true(db_end <= root_end)
      }
    }
  }
}

// Test 2: Context propagation with baggage
test "context propagation with baggage" {
  // 1. Create initial baggage
  let initial_baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("request.id", "req-67890"),
    ("trace.origin", "frontend")
  ] })
  
  // 2. Create context with baggage
  let context = Context({ data = Some("correlation.id", "corr-12345") })
  
  // 3. Simulate service boundary crossing
  let service1_baggage = Baggage({ entries = initial_baggage.entries @ [
    ("service.name", "auth-service"),
    ("service.version", "1.2.3")
  ] })
  
  let service1_context = Context({ data = Some("service.start.time", "2023-01-01T00:00:00Z") })
  
  // 4. Simulate another service boundary crossing
  let service2_baggage = Baggage({ entries = service1_baggage.entries @ [
    ("service.name", "user-service"),
    ("service.version", "2.1.0")
  ] })
  
  let service2_context = Context({ data = Some("db.connection.id", "conn-54321") })
  
  // 5. Verify baggage propagation
  let mut found_user_id = false
  let mut found_request_id = false
  let mut found_trace_origin = false
  let mut found_service1_name = false
  let mut found_service1_version = false
  let mut found_service2_name = false
  let mut found_service2_version = false
  
  for (key, value) in service2_baggage.entries {
    match key {
      "user.id" => {
        assert_eq(value, "12345")
        found_user_id = true
      }
      "request.id" => {
        assert_eq(value, "req-67890")
        found_request_id = true
      }
      "trace.origin" => {
        assert_eq(value, "frontend")
        found_trace_origin = true
      }
      "service.name" => {
        // Should be the last service name (user-service)
        assert_eq(value, "user-service")
        found_service2_name = true
      }
      "service.version" => {
        // Should be the last service version (2.1.0)
        assert_eq(value, "2.1.0")
        found_service2_version = true
      }
      _ => ()  // Other keys
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_request_id)
  assert_true(found_trace_origin)
  assert_true(found_service2_name)
  assert_true(found_service2_version)
  
  // Verify context propagation
  match service2_context.data {
    Some((key, value)) => {
      assert_eq(key, "db.connection.id")
      assert_eq(value, "conn-54321")
    }
    None => assert_true(false)
  }
}

// Test 3: Resource merging in distributed system
test "resource merging in distributed system" {
  // 1. Create resources from different services
  let frontend_resource = Resource({ attributes = [
    ("service.name", StringValue("frontend")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("frontend-001")),
    ("service.namespace", StringValue("production")),
    ("deployment.environment", StringValue("prod"))
  ] })
  
  let backend_resource = Resource({ attributes = [
    ("service.name", StringValue("backend")),
    ("service.version", StringValue("2.0.0")),
    ("service.instance.id", StringValue("backend-001")),
    ("service.namespace", StringValue("production")),
    ("host.name", StringValue("backend-host-01"))
  ] })
  
  let database_resource = Resource({ attributes = [
    ("service.name", StringValue("database")),
    ("service.version", StringValue("3.0.0")),
    ("service.instance.id", StringValue("db-001")),
    ("service.namespace", StringValue("production")),
    ("db.system", StringValue("postgresql"))
  ] })
  
  // 2. Merge resources with conflict resolution (backend takes precedence)
  let mut merged_attributes = frontend_resource.attributes
  
  // Add backend attributes (override if exists)
  for (key, value) in backend_resource.attributes {
    let mut found = false
    let mut index = 0
    
    for i in 0..merged_attributes.length() {
      let (existing_key, _) = merged_attributes[i]
      if existing_key == key {
        found = true
        index = i
        break
      }
    }
    
    if found {
      merged_attributes[index] = (key, value)
    } else {
      merged_attributes = merged_attributes @ [(key, value)]
    }
  }
  
  // Add database attributes (override if exists)
  for (key, value) in database_resource.attributes {
    let mut found = false
    let mut index = 0
    
    for i in 0..merged_attributes.length() {
      let (existing_key, _) = merged_attributes[i]
      if existing_key == key {
        found = true
        index = i
        break
      }
    }
    
    if found {
      merged_attributes[index] = (key, value)
    } else {
      merged_attributes = merged_attributes @ [(key, value)]
    }
  }
  
  let merged_resource = Resource({ attributes = merged_attributes })
  
  // 3. Verify merged resource
  assert_eq(merged_resource.attributes.length(), 8)
  
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_service_instance = false
  let mut found_service_namespace = false
  let mut found_deployment_env = false
  let mut found_host_name = false
  let mut found_db_system = false
  
  for (key, value) in merged_resource.attributes {
    match key {
      "service.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "database")  // Should be from database (last)
            found_service_name = true
          }
          _ => assert_true(false)
        }
      }
      "service.version" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "3.0.0")  // Should be from database (last)
            found_service_version = true
          }
          _ => assert_true(false)
        }
      }
      "service.instance.id" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "db-001")  // Should be from database (last)
            found_service_instance = true
          }
          _ => assert_true(false)
        }
      }
      "service.namespace" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "production")  // Same in all
            found_service_namespace = true
          }
          _ => assert_true(false)
        }
      }
      "deployment.environment" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "prod")  // From frontend
            found_deployment_env = true
          }
          _ => assert_true(false)
        }
      }
      "host.name" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "backend-host-01")  // From backend
            found_host_name = true
          }
          _ => assert_true(false)
        }
      }
      "db.system" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "postgresql")  // From database
            found_db_system = true
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_service_name)
  assert_true(found_service_version)
  assert_true(found_service_instance)
  assert_true(found_service_namespace)
  assert_true(found_deployment_env)
  assert_true(found_host_name)
  assert_true(found_db_system)
}

// Test 4: Serialization and propagation in distributed tracing
test "serialization and propagation in distributed tracing" {
  // 1. Create span context
  let span_context = SpanContext({
    trace_id: "trace-123456789",
    span_id: "span-111111111",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  // 2. Create baggage
  let baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("request.id", "req-67890")
  ] })
  
  // 3. Serialize to W3C format for HTTP headers
  let traceparent = "00-" + span_context.trace_id + "-" + span_context.span_id + "-01"
  let tracestate = span_context.trace_state
  
  let mut serialized_baggage = ""
  let mut first = true
  for (key, value) in baggage.entries {
    if first {
      first = false
    } else {
      serialized_baggage = serialized_baggage + ","
    }
    serialized_baggage = serialized_baggage + key + "=" + value
  }
  
  // 4. Create carrier with headers
  let carrier = TextMapCarrier({ headers = [
    ("traceparent", traceparent),
    ("tracestate", tracestate),
    ("baggage", serialized_baggage),
    ("x-correlation-id", "corr-12345")
  ] })
  
  // 5. Simulate HTTP request with headers
  let mut received_traceparent = ""
  let mut received_tracestate = ""
  let mut received_baggage = ""
  let mut received_correlation_id = ""
  
  for (key, value) in carrier.headers {
    match key {
      "traceparent" => received_traceparent = value
      "tracestate" => received_tracestate = value
      "baggage" => received_baggage = value
      "x-correlation-id" => received_correlation_id = value
      _ => ()  // Ignore other headers
    }
  }
  
  // 6. Deserialize and reconstruct context
  let traceparent_parts = received_traceparent.split("-")
  let received_span_context = SpanContext({
    trace_id: traceparent_parts[1],
    span_id: traceparent_parts[2],
    sampled: traceparent_parts[3] == "01",
    trace_state: received_tracestate
  })
  
  let baggage_entries = received_baggage.split(",")
  let mut reconstructed_entries = []
  
  for entry in baggage_entries {
    let key_value = entry.split("=")
    if key_value.length() == 2 {
      let key = key_value[0]
      let value = key_value[1]
      reconstructed_entries = reconstructed_entries @ [(key, value)]
    }
  }
  
  let reconstructed_baggage = Baggage({ entries = reconstructed_entries })
  
  // 7. Verify round-trip integrity
  assert_eq(received_span_context.trace_id, span_context.trace_id)
  assert_eq(received_span_context.span_id, span_context.span_id)
  assert_eq(received_span_context.sampled, span_context.sampled)
  assert_eq(received_span_context.trace_state, span_context.trace_state)
  
  assert_eq(reconstructed_baggage.entries.length(), baggage.entries.length())
  
  for (key, value) in reconstructed_baggage.entries {
    match key {
      "user.id" => assert_eq(value, "12345")
      "request.id" => assert_eq(value, "req-67890")
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_eq(received_correlation_id, "corr-12345")
}

// Test 5: Error handling across service boundaries
test "error handling across service boundaries" {
  // 1. Create span with error
  let span_context = SpanContext({
    trace_id: "trace-123456789",
    span_id: "span-111111111",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  })
  
  let error_span = Span({
    name: "failing.operation",
    context: span_context,
    kind: Client,
    parent_span_id: None,
    start_time: 1640995200000,
    end_time: Some(1640995200500),
    status: Error,
    attributes: Attributes({ values = [
      ("error.type", StringValue("ConnectionError")),
      ("error.message", StringValue("Failed to connect to database")),
      ("error.stack", StringValue("at Connection.connect (connection.js:123)"))
    ] }),
    events = [
      SpanEvent({
        name: "exception",
        timestamp: 1640995200300,
        attributes: Attributes({ values = [
          ("exception.type", StringValue("ConnectionError")),
          ("exception.message", StringValue("Failed to connect to database"))
        ] })
      })
    ],
    links = []
  })
  
  // 2. Create error context for propagation
  let error_context = Context({ data = Some("error.id", "err-12345") })
  
  // 3. Serialize error information
  let error_info = {
    "error.type": "ConnectionError",
    "error.message": "Failed to connect to database",
    "error.id": "err-12345",
    "error.timestamp": "2023-01-01T00:00:00Z"
  }
  
  // 4. Simulate error propagation to calling service
  let calling_service_span = Span({
    name: "api.request",
    context: SpanContext({
      trace_id: "trace-123456789",  // Same trace
      span_id: "span-222222222",    // Different span
      sampled: true,
      trace_state: "key1=value1,key2=value2"
    }),
    kind: Server,
    parent_span_id: None,
    start_time: 1640995199500,  // Starts before failing operation
    end_time: Some(1640995200600),
    status: Error,
    attributes: Attributes({ values = [
      ("http.method", StringValue("POST")),
      ("http.url", StringValue("/api/process")),
      ("http.status_code", IntValue(500)),
      ("error.type", StringValue("DownstreamError")),
      ("error.cause", StringValue("ConnectionError"))
    ] }),
    events = [
      SpanEvent({
        name: "error.propagated",
        timestamp: 1640995200550,
        attributes: Attributes({ values = [
          ("error.source", StringValue("failing.operation")),
          ("error.id", StringValue("err-12345"))
        ] })
      })
    ],
    links = [
      SpanLink({
        context: error_span.context,
        attributes: Attributes({ values = [
          ("link.type", StringValue("error_caused_by"))
        ] })
      })
    ]
  })
  
  // 5. Verify error propagation
  match calling_service_span.status {
    Error => assert_true(true)
    _ => assert_true(false)
  }
  
  // Verify error attributes
  let mut found_error_type = false
  let mut found_error_cause = false
  let mut found_http_status = false
  
  for (key, value) in calling_service_span.attributes.values {
    match key {
      "error.type" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "DownstreamError")
            found_error_type = true
          }
          _ => assert_true(false)
        }
      }
      "error.cause" => {
        match value {
          StringValue(v) => {
            assert_eq(v, "ConnectionError")
            found_error_cause = true
          }
          _ => assert_true(false)
        }
      }
      "http.status_code" => {
        match value {
          IntValue(v) => {
            assert_eq(v, 500)
            found_http_status = true
          }
          _ => assert_true(false)
        }
      }
      _ => ()  // Ignore other keys
    }
  }
  
  assert_true(found_error_type)
  assert_true(found_error_cause)
  assert_true(found_http_status)
  
  // Verify error event
  assert_eq(calling_service_span.events.length(), 1)
  let error_event = calling_service_span.events[0]
  assert_eq(error_event.name, "error.propagated")
  
  // Verify error link
  assert_eq(calling_service_span.links.length(), 1)
  let error_link = calling_service_span.links[0]
  assert_eq(error_link.context.trace_id, error_span.context.trace_id)
  assert_eq(error_link.context.span_id, error_span.context.span_id)
}

// Test 6: Complex telemetry scenario with multiple components
test "complex telemetry scenario with multiple components" {
  // 1. Create resources for different components
  let api_resource = Resource({ attributes = [
    ("service.name", StringValue("api-service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("api-001")),
    ("deployment.environment", StringValue("production"))
  ] })
  
  let db_resource = Resource({ attributes = [
    ("service.name", StringValue("database")),
    ("service.version", StringValue("14.1")),
    ("service.instance.id", StringValue("db-001")),
    ("db.system", StringValue("postgresql"))
  ] })
  
  let cache_resource = Resource({ attributes = [
    ("service.name", StringValue("cache")),
    ("service.version", StringValue("7.0")),
    ("service.instance.id", StringValue("cache-001")),
    ("cache.system", StringValue("redis"))
  ] })
  
  // 2. Create trace with multiple spans
  let trace_id = "trace-complex-123456789"
  
  // Root span (API request)
  let api_span = Span({
    name: "api.request",
    context: SpanContext({
      trace_id: trace_id,
      span_id: "span-api-111",
      sampled: true,
      trace_state: "key1=value1,key2=value2"
    }),
    kind: Server,
    parent_span_id: None,
    start_time: 1640995200000,
    end_time: Some(1640995201500),
    status: Ok,
    attributes: Attributes({ values = [
      ("http.method", StringValue("POST")),
      ("http.url", StringValue("/api/user/profile")),
      ("http.status_code", IntValue(200))
    ] }),
    events = [
      SpanEvent({
        name: "request.received",
        timestamp: 1640995200000,
        attributes: Attributes({ values = [] })
      }),
      SpanEvent({
        name: "request.completed",
        timestamp: 1640995201500,
        attributes: Attributes({ values = [
          ("response.size", IntValue(1024))
        ] })
      })
    ],
    links = []
  })
  
  // Database span
  let db_span = Span({
    name: "db.query",
    context: SpanContext({
      trace_id: trace_id,
      span_id: "span-db-222",
      sampled: true,
      trace_state: "key1=value1,key2=value2"
    }),
    kind: Client,
    parent_span_id: Some("span-api-111"),
    start_time: 1640995200200,
    end_time: Some(1640995200800),
    status: Ok,
    attributes: Attributes({ values = [
      ("db.system", StringValue("postgresql")),
      ("db.statement", StringValue("SELECT * FROM users WHERE id = $1")),
      ("db.user", StringValue("app_user"))
    ] }),
    events = [
      SpanEvent({
        name: "db.query.start",
        timestamp: 1640995200200,
        attributes: Attributes({ values = [] })
      }),
      SpanEvent({
        name: "db.query.end",
        timestamp: 1640995200800,
        attributes: Attributes({ values = [
          ("db.rows_affected", IntValue(1))
        ] })
      })
    ],
    links = []
  })
  
  // Cache span
  let cache_span = Span({
    name: "cache.get",
    context: SpanContext({
      trace_id: trace_id,
      span_id: "span-cache-333",
      sampled: true,
      trace_state: "key1=value1,key2=value2"
    }),
    kind: Client,
    parent_span_id: Some("span-api-111"),
    start_time: 1640995200100,
    end_time: Some(1640995200150),
    status: Ok,
    attributes: Attributes({ values = [
      ("cache.system", StringValue("redis")),
      ("cache.key", StringValue("user:profile:12345")),
      ("cache.hit", BoolValue(true))
    ] }),
    events = [],
    links = []
  })
  
  // 3. Create baggage for context propagation
  let baggage = Baggage({ entries = [
    ("user.id", "12345"),
    ("request.id", "req-abcdef"),
    ("session.id", "sess-12345"),
    ("client.version", StringValue("2.0.0"))
  ] })
  
  // 4. Create instrumentation scope
  let instrumentation_scope = InstrumentationScope({
    name: "azimuth.instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 5. Verify complete scenario
  // Verify trace consistency
  assert_eq(api_span.context.trace_id, db_span.context.trace_id)
  assert_eq(db_span.context.trace_id, cache_span.context.trace_id)
  
  // Verify parent-child relationships
  match db_span.parent_span_id {
    None => assert_true(false)
    Some(parent_id) => assert_eq(parent_id, api_span.context.span_id)
  }
  
  match cache_span.parent_span_id {
    None => assert_true(false)
    Some(parent_id) => assert_eq(parent_id, api_span.context.span_id)
  }
  
  // Verify timing
  assert_true(cache_span.start_time >= api_span.start_time)
  assert_true(db_span.start_time >= api_span.start_time)
  
  match cache_span.end_time {
    None => assert_true(false)
    Some(end_time) => assert_true(end_time >= cache_span.start_time)
  }
  
  match db_span.end_time {
    None => assert_true(false)
    Some(end_time) => assert_true(end_time >= db_span.start_time)
  }
  
  // Verify all operations complete within API request
  match api_span.end_time {
    None => assert_true(false)
    Some(api_end) => {
      match cache_span.end_time {
        None => assert_true(false)
        Some(cache_end) => assert_true(cache_end <= api_end)
      }
      
      match db_span.end_time {
        None => assert_true(false)
        Some(db_end) => assert_true(db_end <= api_end)
      }
    }
  }
  
  // Verify baggage integrity
  assert_eq(baggage.entries.length(), 4)
  
  let mut found_user_id = false
  let mut found_request_id = false
  let mut found_session_id = false
  let mut found_client_version = false
  
  for (key, value) in baggage.entries {
    match key {
      "user.id" => {
        assert_eq(value, "12345")
        found_user_id = true
      }
      "request.id" => {
        assert_eq(value, "req-abcdef")
        found_request_id = true
      }
      "session.id" => {
        assert_eq(value, "sess-12345")
        found_session_id = true
      }
      "client.version" => {
        assert_eq(value, "2.0.0")
        found_client_version = true
      }
      _ => assert_true(false)  // Unexpected key
    }
  }
  
  assert_true(found_user_id)
  assert_true(found_request_id)
  assert_true(found_session_id)
  assert_true(found_client_version)
  
  // Verify instrumentation scope
  assert_eq(instrumentation_scope.name, "azimuth.instrumentation")
  match instrumentation_scope.version {
    None => assert_true(false)
    Some(v) => assert_eq(v, "1.0.0")
  }
  match instrumentation_scope.schema_url {
    None => assert_true(false)
    Some(url) => assert_eq(url, "https://opentelemetry.io/schemas/1.20.0")
  }
  
  // Verify resource attributes
  assert_eq(api_resource.attributes.length(), 4)
  assert_eq(db_resource.attributes.length(), 4)
  assert_eq(cache_resource.attributes.length(), 4)
}