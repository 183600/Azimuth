// 跨API集成高级测试
test "trace_logs_integration" {
  // 测试Trace和Logs API的集成
  
  let trace_id = Array::make(16, 0x01_byte)
  let span_id = Array::make(8, 0x02_byte)
  
  // 创建Span
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01_byte,
    trace_state: "test=value"
  }
  
  let span = Span::{
    name: "api-request",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: Some(1234567990L),
    status: Ok,
    status_description: Some("Request completed successfully"),
    attributes: [
      ("http.method", AttributeValue::string("GET")),
      ("http.url", AttributeValue::string("/api/users")),
      ("http.status_code", AttributeValue::int(200L))
    ],
    events: [],
    links: []
  }
  
  // 创建与Span关联的LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1234567950L)
    .severity(Info)
    .body("Processing user request")
    .with_attribute("span.name", AttributeValue::string(span.name))
    .with_attribute("span.kind", AttributeValue::string("Server"))
    .with_attribute("http.method", AttributeValue::string("GET"))
    .with_attribute("http.url", AttributeValue::string("/api/users"))
    .build()
  
  // 验证Span和LogRecord的关联
  assert_eq(span.name, "api-request")
  match log_record.attributes[0].1 {
    StringValue(span_name) => assert_eq(span_name, "api-request")
    _ => @test.fail("Expected StringValue for span.name")
  }
  
  // 验证时间戳顺序
  assert_eq(span.start_time_unix_nanos, 1234567890L)
  assert_eq(log_record.timestamp_unix_nanos, 1234567950L)
  assert_eq(log_record.timestamp_unix_nanos > span.start_time_unix_nanos, true)
  
  // 验证共同属性
  let mut found_method = false
  let mut found_url = false
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    if key == "http.method" {
      match value {
        StringValue(method) => {
          assert_eq(method, "GET")
          found_method = true
        }
        _ => @test.fail("Expected StringValue for http.method")
      }
    }
    if key == "http.url" {
      match value {
        StringValue(url) => {
          assert_eq(url, "/api/users")
          found_url = true
        }
        _ => @test.fail("Expected StringValue for http.url")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_method, true)
  assert_eq(found_url, true)
}

test "trace_metrics_integration" {
  // 测试Trace和Metrics API的集成
  
  let meter = NoopMeter::{}
  let tracer_provider = NoopTracerProvider::{}
  
  // 创建Counter指标
  let request_counter = meter.create_counter(
    "http_requests_total",
    Some("count"),
    Some("Total number of HTTP requests")
  )
  
  // 创建Histogram指标
  let duration_histogram = meter.create_histogram(
    "http_request_duration",
    Some("ms"),
    Some("HTTP request duration in milliseconds")
  )
  
  // 创建Tracer和Span
  let tracer = tracer_provider.get_tracer("test-tracer")
  let (ctx, span) = tracer.start_span(
    context::Context::current(),
    "http-request",
    Some(Server),
    Some([
      ("http.method", AttributeValue::string("POST")),
      ("http.url", AttributeValue::string("/api/orders"))
    ]),
    Some(1234567890L)
  )
  
  // 在Span上下文中记录指标
  request_counter.add(1L, Some([
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("/api/orders")),
    ("http.status_code", AttributeValue::int(201L))
  ]))
  
  duration_histogram.record(150.5, Some([
    ("http.method", AttributeValue::string("POST")),
    ("http.url", AttributeValue::string("/api/orders")),
    ("http.status_code", AttributeValue::int(201L))
  ]))
  
  // 验证Span和指标的属性一致性
  let mut found_method = false
  let mut found_url = false
  let mut found_status = false
  let mut i = 0
  while i < span.attributes.length() {
    let (key, _) = span.attributes[i]
    if key == "http.method" {
      found_method = true
    }
    if key == "http.url" {
      found_url = true
    }
    i = i + 1
  }
  
  assert_eq(found_method, true)
  assert_eq(found_url, true)
  assert_eq(span.name, "http-request")
  assert_eq(true, true) // 验证指标记录不会出错
}

test "logs_metrics_integration" {
  // 测试Logs和Metrics API的集成
  
  let meter = NoopMeter::{}
  let logger_provider = NoopLoggerProvider::{}
  
  // 创建指标
  let log_counter = meter.create_counter(
    "logs_total",
    Some("count"),
    Some("Total number of log records")
  )
  
  let error_counter = meter.create_counter(
    "log_errors_total",
    Some("count"),
    Some("Total number of error log records"
  )
  
  // 创建Logger
  let logger = logger_provider.get_logger("test-logger")
  
  // 记录不同级别的日志
  logger.debug("Debug message", Some([
    ("component", AttributeValue::string("auth")),
    ("user.id", AttributeValue::int(12345L))
  ]))
  
  log_counter.add(1L, Some([
    ("severity", AttributeValue::string("DEBUG")),
    ("component", AttributeValue::string("auth"))
  ]))
  
  logger.info("User authenticated", Some([
    ("component", AttributeValue::string("auth")),
    ("user.id", AttributeValue::int(12345L)),
    ("session.id", AttributeValue::string("session-abc123"))
  ]))
  
  log_counter.add(1L, Some([
    ("severity", AttributeValue::string("INFO")),
    ("component", AttributeValue::string("auth"))
  ]))
  
  logger.error("Authentication failed", Some([
    ("component", AttributeValue::string("auth")),
    ("error.code", AttributeValue::string("INVALID_CREDENTIALS")),
    ("user.id", AttributeValue::int(67890L))
  ]))
  
  log_counter.add(1L, Some([
    ("severity", AttributeValue::string("ERROR")),
    ("component", AttributeValue::string("auth"))
  ]))
  
  error_counter.add(1L, Some([
    ("component", AttributeValue::string("auth")),
    ("error.code", AttributeValue::string("INVALID_CREDENTIALS"))
  ]))
  
  assert_eq(true, true) // 验证所有操作不会出错
}

test "context_propagation_integration" {
  // 测试Context、Propagation和Trace的集成
  
  let ctx = context::Context::empty()
  let user_key = create_key("user-id")
  let session_key = create_key("session-id")
  
  // 在Context中添加值
  let ctx_with_values = ctx
    .with_value(user_key, "user-12345")
    .with_value(session_key, "session-abcdef")
  
  // 创建Baggage
  let baggage = Baggage::empty()
    .with_entry("request-id", "req-123456")
    .with_entry("trace-origin", "frontend")
    .with_entry("environment", "production")
  
  // 创建传播器
  let carrier = MapCarrier::new()
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 注入Context到Carrier
  composite.inject(ctx_with_values, carrier)
  
  // 创建Span（在实际实现中会从Context中提取信息）
  let tracer = NoopTracer::{}
  let (new_ctx, span) = tracer.start_span(
    ctx_with_values,
    "service-operation",
    Some(Server),
    Some([
      ("service.name", AttributeValue::string("payment-service")),
      ("service.version", AttributeValue::string("1.2.3"))
    ]),
    Some(1234567890L)
  )
  
  // 从Carrier中提取Context
  let extracted_ctx = composite.extract(ctx, carrier)
  
  // 验证Context值仍然存在
  match extracted_ctx.get(user_key) {
    Some(value) => assert_eq(value, "user-12345")
    None => @test.fail("Expected to find user-id in extracted context")
  }
  
  match extracted_ctx.get(session_key) {
    Some(value) => assert_eq(value, "session-abcdef")
    None => @test.fail("Expected to find session-id in extracted context")
  }
  
  // 验证Span属性
  assert_eq(span.name, "service-operation")
  match span.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  
  // 验证传播的头部
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
}

test "resource_metadata_integration" {
  // 测试Resource元数据在各个API中的集成
  
  let resource = Resource::default("user-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "user-auth",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 创建带有Resource信息的LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("User login attempt")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .with_attribute("service.version", AttributeValue::string("2.1.0"))
    .with_attribute("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name))
    .with_attribute("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version))
    .with_attribute("instrumentation.name", AttributeValue::string(instrumentation_scope.name))
    .with_attribute("instrumentation.version", AttributeValue::string("1.0.0"))
    .build()
  
  // 创建带有Resource信息的Span
  let span = Span::{
    name: "user-authentication",
    context: SpanContext::{
      trace_id: Array::make(16, 0x01_byte),
      span_id: Array::make(8, 0x02_byte),
      trace_flags: 0x01_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1234567890L,
    end_time_unix_nanos: Some(1234567990L),
    status: Ok,
    status_description: None,
    attributes: [
      ("service.name", AttributeValue::string(resource.service_name)),
      ("service.version", AttributeValue::string("2.1.0")),
      ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
      ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version))
    ],
    events: [],
    links: []
  }
  
  // 创建带有Resource信息的指标
  let meter = NoopMeter::{}
  let auth_counter = meter.create_counter(
    "authentication_attempts_total",
    Some("count"),
    Some("Total authentication attempts"
  )
  
  auth_counter.add(1L, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("service.version", AttributeValue::string("2.1.0")),
    ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
    ("instrumentation.name", AttributeValue::string(instrumentation_scope.name))
  ]))
  
  // 验证所有API中的Resource元数据一致性
  let mut log_service_name = ""
  let mut span_service_name = ""
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    if key == "service.name" {
      match value {
        StringValue(name) => log_service_name = name
        _ => @test.fail("Expected StringValue for service.name")
      }
    }
    i = i + 1
  }
  
  let mut j = 0
  while j < span.attributes.length() {
    let (key, value) = span.attributes[j]
    if key == "service.name" {
      match value {
        StringValue(name) => span_service_name = name
        _ => @test.fail("Expected StringValue for service.name")
      }
    }
    j = j + 1
  }
  
  assert_eq(log_service_name, "user-service")
  assert_eq(span_service_name, "user-service")
  assert_eq(log_service_name, span_service_name)
  assert_eq(resource.service_name, "user-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(instrumentation_scope.name, "user-auth")
}

test "end_to_end_telemetry_flow" {
  // 测试端到端的遥测数据流
  
  // 1. 创建Resource和InstrumentationScope
  let resource = Resource::default("order-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "order-processing",
    version: Some("1.5.0"),
    schema_url: None
  }
  
  // 2. 创建Context并添加Baggage
  let ctx = context::Context::empty()
  let baggage = Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("request.id", "req-67890")
    .with_entry("session.id", "session-abc")
  
  // 3. 创建传播器并注入Context
  let carrier = MapCarrier::new()
  let composite = CompositePropagator::new([
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ])
  composite.inject(ctx, carrier)
  
  // 4. 创建Span并记录事件
  let tracer = NoopTracer::{}
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "process-order",
    Some(Server),
    Some([
      ("service.name", AttributeValue::string(resource.service_name)),
      ("operation.type", AttributeValue::string("order_processing")),
      ("user.id", AttributeValue::string("12345"))
    ]),
    Some(1234567890L)
  )
  
  // 5. 记录指标
  let meter = NoopMeter::{}
  let order_counter = meter.create_counter(
    "orders_processed_total",
    Some("count"),
    Some("Total number of orders processed"
  )
  
  let processing_time = meter.create_histogram(
    "order_processing_duration",
    Some("ms"),
    Some("Order processing duration"
  )
  
  order_counter.add(1L, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.type", AttributeValue::string("order_processing")),
    ("user.id", AttributeValue::string("12345")),
    ("order.status", AttributeValue::string("completed"))
  ]))
  
  processing_time.record(250.5, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.type", AttributeValue::string("order_processing")),
    ("user.id", AttributeValue::string("12345"))
  ]))
  
  // 6. 记录日志
  let logger = NoopLogger::{}
  logger.info("Order processed successfully", Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.type", AttributeValue::string("order_processing")),
    ("user.id", AttributeValue::string("12345")),
    ("order.id", AttributeValue::string("order-12345")),
    ("order.amount", AttributeValue::float(99.99))
  ]))
  
  // 7. 创建详细的LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1234567950L)
    .severity(Info)
    .body("Order processing completed")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .with_attribute("instrumentation.name", AttributeValue::string(instrumentation_scope.name))
    .with_attribute("span.name", AttributeValue::string(span.name))
    .with_attribute("user.id", AttributeValue::string("12345"))
    .with_attribute("order.id", AttributeValue::string("order-12345"))
    .with_attribute("order.amount", AttributeValue::float(99.99))
    .with_attribute("processing.time.ms", AttributeValue::float(250.5))
    .build()
  
  // 8. 验证端到端数据一致性
  assert_eq(span.name, "process-order")
  assert_eq(resource.service_name, "order-service")
  assert_eq(instrumentation_scope.name, "order-processing")
  
  // 验证传播的头部
  match carrier.get(TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get(BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
  
  // 验证LogRecord中的关联信息
  let mut found_span_name = false
  let mut found_user_id = false
  let mut found_order_id = false
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    if key == "span.name" {
      match value {
        StringValue(name) => {
          assert_eq(name, "process-order")
          found_span_name = true
        }
        _ => @test.fail("Expected StringValue for span.name")
      }
    }
    if key == "user.id" {
      match value {
        StringValue(user_id) => {
          assert_eq(user_id, "12345")
          found_user_id = true
        }
        _ => @test.fail("Expected StringValue for user.id")
      }
    }
    if key == "order.id" {
      match value {
        StringValue(order_id) => {
          assert_eq(order_id, "order-12345")
          found_order_id = true
        }
        _ => @test.fail("Expected StringValue for order.id")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_span_name, true)
  assert_eq(found_user_id, true)
  assert_eq(found_order_id, true)
}