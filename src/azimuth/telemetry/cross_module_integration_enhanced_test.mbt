test "Cross-module integration comprehensive test" {
  // 测试Common模块与其他模块的集成
  
  // 创建共享的Resource和InstrumentationScope
  let shared_resource = common::Resource::default("integrated-service")
  shared_resource.service_version = Some("2.1.0")
  shared_resource.attributes = [
    ("env", common::AttributeValue::string("production")),
    ("region", common::AttributeValue::string("us-east")),
    ("instance_id", common::AttributeValue::int(67890L))
  ]
  
  let shared_scope = common::InstrumentationScope{
    name: "integrated-scope",
    version: Some("1.5.2"),
    schema_url: Some("https://example.com/telemetry/schema/v1")
  }
  
  // 测试Logs模块与Common模块的集成
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
    .severity(logs::Info)
    .body("Application started successfully")
    .with_attribute("service_name", common::AttributeValue::string(shared_resource.service_name))
    .with_attribute("service_version", common::AttributeValue::string(shared_resource.service_version.unwrap_or("unknown")))
    .with_attribute("scope_name", common::AttributeValue::string(shared_scope.name))
    .with_attribute("scope_version", common::AttributeValue::string(shared_scope.version.unwrap_or("unknown")))
    .build()
  
  // 手动设置resource和instrumentation_scope
  let mut integrated_log = log_record
  integrated_log.resource = shared_resource
  integrated_log.instrumentation_scope = shared_scope
  
  // 验证日志记录的集成
  match integrated_log.resource {
    Some(resource) => {
      assert_eq(resource.service_name, "integrated-service")
      match resource.service_version {
        Some(version) => assert_eq(version, "2.1.0")
        None => @test.fail("Expected service version to be Some")
      }
      assert_eq(resource.attributes.length(), 3)
    }
    None => @test.fail("Expected resource to be Some")
  }
  
  match integrated_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "integrated-scope")
      match scope.version {
        Some(version) => assert_eq(version, "1.5.2")
        None => @test.fail("Expected scope version to be Some")
      }
    }
    None => @test.fail("Expected instrumentation_scope to be Some")
  }
  
  // 测试Trace模块与Common模块的集成
  let trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte,
                  9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  let span_context = trace::SpanContext{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "azimuth=1"
  }
  
  let span = trace::Span{
    name: "integrated-operation",
    context: span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200500000000L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("service_name", common::AttributeValue::string(shared_resource.service_name)),
      ("service_version", common::AttributeValue::string(shared_resource.service_version.unwrap_or("unknown"))),
      ("scope_name", common::AttributeValue::string(shared_scope.name)),
      ("env", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-east")),
      ("duration_ms", common::AttributeValue::float(50.0))
    ],
    events: [],
    links: []
  }
  
  // 验证span的集成
  assert_eq(span.attributes.length(), 6)
  let service_name_attr = span.attributes[0]
  match service_name_attr.1 {
    common::StringValue(name) => assert_eq(name, "integrated-service")
    _ => @test.fail("Expected StringValue for service_name")
  }
  
  // 测试Metrics模块与Common模块的集成
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter(shared_scope.name, shared_scope.version, shared_scope.schema_url)
  
  let request_counter = meter.create_counter(
    "http_requests_total", 
    Some("requests"), 
    Some("Total number of HTTP requests")
  )
  
  let response_histogram = meter.create_histogram(
    "http_response_duration_seconds",
    Some("seconds"),
    Some("HTTP request response duration in seconds")
  )
  
  let active_connections_gauge = meter.create_gauge(
    "http_active_connections",
    Some("connections"),
    Some("Current number of active HTTP connections")
  )
  
  // 使用带有共享属性的指标
  request_counter.add(1L, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("200")),
    ("env", common::AttributeValue::string("production"))
  ])
  
  response_histogram.record(0.045, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("endpoint", common::AttributeValue::string("/api/users")),
    ("method", common::AttributeValue::string("GET"))
  ])
  
  active_connections_gauge.record(25.0, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("instance_id", common::AttributeValue::int(67890L))
  ])
  
  // 测试跨模块的复杂场景：一个完整的请求处理流程
  
  // 1. 创建请求span
  let request_trace_id = [10_byte, 20_byte, 30_byte, 40_byte, 50_byte, 60_byte, 70_byte, 80_byte,
                         90_byte, 100_byte, 110_byte, 120_byte, 130_byte, 140_byte, 150_byte, 160_byte]
  let request_span_id = [10_byte, 20_byte, 30_byte, 40_byte, 50_byte, 60_byte, 70_byte, 80_byte]
  let request_context = trace::SpanContext{
    trace_id: request_trace_id,
    span_id: request_span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  let request_span = trace::Span{
    name: "HTTP GET /api/data",
    context: request_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,  // 还在进行中
    status: trace::Unset,
    status_description: None,
    attributes: [
      ("http.method", common::AttributeValue::string("GET")),
      ("http.url", common::AttributeValue::string("/api/data")),
      ("http.user_agent", common::AttributeValue::string("Azimuth-Client/1.0")),
      ("net.host.name", common::AttributeValue::string("api.example.com")),
      ("service_name", common::AttributeValue::string(shared_resource.service_name))
    ],
    events: [],
    links: []
  }
  
  // 2. 记录请求开始日志
  let request_start_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Request started")
    .with_attribute("trace_id", common::AttributeValue::array_string(request_trace_id.map(fn(b) { b.to_string() })))
    .with_attribute("span_id", common::AttributeValue::array_string(request_span_id.map(fn(b) { b.to_string() })))
    .with_attribute("http.method", common::AttributeValue::string("GET"))
    .with_attribute("http.url", common::AttributeValue::string("/api/data"))
    .build()
  
  let mut request_start_log_integrated = request_start_log
  request_start_log_integrated.resource = shared_resource
  request_start_log_integrated.instrumentation_scope = shared_scope
  
  // 3. 记录请求指标
  let api_requests_counter = meter.create_counter("api_requests_total", Some("requests"), Some("Total API requests"))
  api_requests_counter.add(1L, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("http.method", common::AttributeValue::string("GET")),
    ("http.route", common::AttributeValue::string("/api/data")),
    ("env", common::AttributeValue::string("production"))
  ])
  
  // 4. 模拟处理过程中的事件
  let db_query_event = trace::SpanEvent{
    name: "database.query",
    timestamp_unix_nanos: 1640995200100000000L,
    attributes: [
      ("db.statement", common::AttributeValue::string("SELECT * FROM data WHERE active = true")),
      ("db.type", common::AttributeValue::string("postgresql")),
      ("db.duration_ms", common::AttributeValue::float(25.5))
    ]
  }
  
  // 5. 记录处理过程中的日志
  let processing_log = logs::LogRecord::builder()
    .timestamp(1640995200150000000L)
    .severity(logs::Debug)
    .body("Processing database query results")
    .with_attribute("query_duration_ms", common::AttributeValue::float(25.5))
    .with_attribute("result_count", common::AttributeValue::int(150L))
    .build()
  
  let mut processing_log_integrated = processing_log
  processing_log_integrated.resource = shared_resource
  processing_log_integrated.instrumentation_scope = shared_scope
  
  // 6. 记录处理指标
  let db_query_histogram = meter.create_histogram("db_query_duration_seconds", Some("seconds"), Some("Database query duration"))
  db_query_histogram.record(0.0255, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("db.type", common::AttributeValue::string("postgresql")),
    ("operation", common::AttributeValue::string("SELECT"))
  ])
  
  // 7. 完成请求
  let mut completed_request_span = request_span
  completed_request_span.end_time_unix_nanos = Some(1640995200450000000L)
  completed_request_span.status = trace::Ok
  completed_request_span.status_description = Some("Request completed successfully")
  completed_request_span.events = [db_query_event]
  completed_request_span.attributes = completed_request_span.attributes + [
    ("http.status_code", common::AttributeValue::int(200L)),
    ("http.response_content_length", common::AttributeValue::int(2048L))
  ]
  
  // 8. 记录完成日志
  let completion_log = logs::LogRecord::builder()
    .timestamp(1640995200450000000L)
    .severity(logs::Info)
    .body("Request completed successfully")
    .with_attribute("http.status_code", common::AttributeValue::int(200L))
    .with_attribute("duration_ms", common::AttributeValue::float(45.0))
    .with_attribute("response_size", common::AttributeValue::int(2048L))
    .build()
  
  let mut completion_log_integrated = completion_log
  completion_log_integrated.resource = shared_resource
  completion_log_integrated.instrumentation_scope = shared_scope
  
  // 9. 记录最终指标
  let request_duration_histogram = meter.create_histogram("http_request_duration_seconds", Some("seconds"), Some("HTTP request duration"))
  request_duration_histogram.record(0.045, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("http.method", common::AttributeValue::string("GET")),
    ("http.route", common::AttributeValue::string("/api/data")),
    ("http.status_code", common::AttributeValue::int(200L))
  ])
  
  let response_size_histogram = meter.create_histogram("http_response_size_bytes", Some("bytes"), Some("HTTP response size"))
  response_size_histogram.record(2048.0, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("http.method", common::AttributeValue::string("GET")),
    ("http.route", common::AttributeValue::string("/api/data"))
  ])
  
  // 验证完整的集成场景
  assert_eq(completed_request_span.end_time_unix_nanos, Some(1640995200450000000L))
  assert_eq(completed_request_span.status, trace::Ok)
  assert_eq(completed_request_span.events.length(), 1)
  assert_eq(completed_request_span.attributes.length(), 8)  // 原始5个 + 3个新增
  
  match completion_log_integrated.resource {
    Some(resource) => assert_eq(resource.service_name, "integrated-service")
    None => @test.fail("Expected resource to be Some")
  }
  
  // 测试错误场景的集成
  let error_span = trace::Span{
    name: "HTTP POST /api/error",
    context: trace::SpanContext{
      trace_id: [255_byte, 254_byte, 253_byte, 252_byte, 251_byte, 250_byte, 249_byte, 248_byte,
                 247_byte, 246_byte, 245_byte, 244_byte, 243_byte, 242_byte, 241_byte, 240_byte],
      span_id: [255_byte, 254_byte, 253_byte, 252_byte, 251_byte, 250_byte, 249_byte, 248_byte],
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995300000000000L,
    end_time_unix_nanos: Some(1640995300050000000L),
    status: trace::Error,
    status_description: Some("Internal server error"),
    attributes: [
      ("http.method", common::AttributeValue::string("POST")),
      ("http.url", common::AttributeValue::string("/api/error")),
      ("http.status_code", common::AttributeValue::int(500L)),
      ("error.type", common::AttributeValue::string("DatabaseError")),
      ("service_name", common::AttributeValue::string(shared_resource.service_name))
    ],
    events: [
      trace::SpanEvent{
        name: "exception",
        timestamp_unix_nanos: 1640995300030000000L,
        attributes: [
          ("exception.type", common::AttributeValue::string("DatabaseError")),
          ("exception.message", common::AttributeValue::string("Connection timeout")),
          ("exception.stacktrace", common::AttributeValue::string("at Database.query (db.js:123)"))
        ]
      }
    ],
    links: []
  }
  
  let error_log = logs::LogRecord::builder()
    .timestamp(1640995300030000000L)
    .severity(logs::Error)
    .body("Database connection timeout")
    .with_attribute("error.type", common::AttributeValue::string("DatabaseError"))
    .with_attribute("error.message", common::AttributeValue::string("Connection timeout"))
    .with_attribute("http.status_code", common::AttributeValue::int(500L))
    .build()
  
  let mut error_log_integrated = error_log
  error_log_integrated.resource = shared_resource
  error_log_integrated.instrumentation_scope = shared_scope
  
  let error_counter = meter.create_counter("http_errors_total", Some("errors"), Some("Total HTTP errors"))
  error_counter.add(1L, [
    ("service_name", common::AttributeValue::string(shared_resource.service_name)),
    ("http.method", common::AttributeValue::string("POST")),
    ("http.status_code", common::AttributeValue::int(500L)),
    ("error.type", common::AttributeValue::string("DatabaseError"))
  ])
  
  // 验证错误场景
  assert_eq(error_span.status, trace::Error)
  assert_eq(error_span.events.length(), 1)
  match error_log_integrated.resource {
    Some(resource) => assert_eq(resource.service_name, "integrated-service")
    None => @test.fail("Expected resource to be Some")
  }
  
  // 测试跨模块的边界条件
  let empty_resource = common::Resource::default("")
  let empty_scope = common::InstrumentationScope{ name: "", version: None, schema_url: None }
  
  let boundary_log = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Boundary test")
    .build()
  
  let mut boundary_log_integrated = boundary_log
  boundary_log_integrated.resource = empty_resource
  boundary_log_integrated.instrumentation_scope = empty_scope
  
  match boundary_log_integrated.resource {
    Some(resource) => assert_eq(resource.service_name, "")
    None => @test.fail("Expected resource to be Some")
  }
  
  match boundary_log_integrated.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "")
    None => @test.fail("Expected instrumentation_scope to be Some")
  }
}