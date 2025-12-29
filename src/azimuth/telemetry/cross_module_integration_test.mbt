// 跨模块集成测试用例
test "trace_context_integration" {
  // 测试Trace与Context的集成
  let ctx = Context::empty()
  
  // 创建ContextKey用于存储trace相关信息
  let trace_id_key = create_key("trace.id")
  let span_id_key = create_key("span.id")
  let sampling_decision_key = create_key("sampling.decision")
  
  // 模拟从Span中提取的信息
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let sampling_decision = "true"
  
  // 将trace信息存储到Context中
  let enriched_ctx = ctx
    .with_value(trace_id_key, trace_id)
    .with_value(span_id_key, span_id)
    .with_value(sampling_decision_key, sampling_decision)
  
  // 创建带有trace上下文的LogRecord
  let trace_id_bytes = Array::make(16, 0x0a_byte)
  let span_id_bytes = Array::make(8, 0x0b_byte)
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Processing payment request"),
    attributes: [
      ("operation.name", AttributeValue::string("process_payment")),
      ("service.name", AttributeValue::string("payment-service")),
      ("user.id", AttributeValue::string("user-12345")),
      ("payment.amount", AttributeValue::float(99.99)),
      ("payment.currency", AttributeValue::string("USD"))
    ],
    trace_id: Some(trace_id_bytes),
    span_id: Some(span_id_bytes),
    trace_flags: Some(1_byte),
    resource: Some(Resource::default("payment-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "payment-processor",
      version: Some("2.1.0"),
      schema_url: None
    })
  }
  
  // 验证Context中的trace信息
  match enriched_ctx.get(trace_id_key) {
    Some(stored_trace_id) => assert_eq(stored_trace_id, trace_id)
    None => @test.fail("Expected trace_id in context")
  }
  
  match enriched_ctx.get(span_id_key) {
    Some(stored_span_id) => assert_eq(stored_span_id, span_id)
    None => @test.fail("Expected span_id in context")
  }
  
  match enriched_ctx.get(sampling_decision_key) {
    Some(decision) => assert_eq(decision, sampling_decision)
    None => @test.fail("Expected sampling decision in context")
  }
  
  // 验证LogRecord的trace上下文
  match log_record.trace_id {
    Some(id) => assert_eq(id.length(), 16)
    None => @test.fail("Expected trace_id in log record")
  }
  
  match log_record.span_id {
    Some(id) => assert_eq(id.length(), 8)
    None => @test.fail("Expected span_id in log record")
  }
  
  match log_record.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Expected trace_flags in log record")
  }
}

test "metrics_context_baggage_integration" {
  // 测试Metrics与Context和Baggage的集成
  let ctx = Context::empty()
  let baggage = Baggage::empty()
  
  // 在Context中存储操作相关元数据
  let operation_key = create_key("operation.name")
  let service_key = create_key("service.name")
  let version_key = create_key("service.version")
  
  let enriched_ctx = ctx
    .with_value(operation_key, "http_request")
    .with_value(service_key, "api-gateway")
    .with_value(version_key, "3.2.1")
  
  // 在Baggage中存储请求相关元数据
  let enriched_baggage = baggage
    .with_entry("user.id", "user-67890")
    .with_entry("request.id", "req-xyz789")
    .with_entry("client.ip", "192.168.1.100")
    .with_entry("user.agent", "Mozilla/5.0")
    .with_entry("request.path", "/api/v2/users")
    .with_entry("http.method", "GET")
  
  // 创建带有上下文信息的Measurement
  let complex_attributes = [
    // Context中的信息
    ("operation.name", AttributeValue::string("http_request")),
    ("service.name", AttributeValue::string("api-gateway")),
    ("service.version", AttributeValue::string("3.2.1")),
    
    // Baggage中的信息
    ("user.id", AttributeValue::string("user-67890")),
    ("request.id", AttributeValue::string("req-xyz789")),
    ("client.ip", AttributeValue::string("192.168.1.100")),
    ("user.agent", AttributeValue::string("Mozilla/5.0")),
    ("request.path", AttributeValue::string("/api/v2/users")),
    ("http.method", AttributeValue::string("GET")),
    
    // 指标特定信息
    ("http.status_code", AttributeValue::int(200L)),
    ("response.size", AttributeValue::int(2048L)),
    ("duration.ms", AttributeValue::float(156.7)),
    ("cache.hit", AttributeValue::bool(false)),
    ("db.queries", AttributeValue::int(3L)),
    
    // 数组属性
    ("tags", AttributeValue::array_string(["api", "users", "auth"])),
    ("status.codes", AttributeValue::array_int([200L, 201L, 400L, 404L])),
    ("latency.buckets", AttributeValue::array_float([10.0, 50.0, 100.0, 500.0])),
    ("feature.flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  let measurement = Measurement::{
    value: 156.7,
    attributes: complex_attributes
  }
  
  // 验证Context信息
  match enriched_ctx.get(operation_key) {
    Some(operation) => assert_eq(operation, "http_request")
    None => @test.fail("Expected operation name in context")
  }
  
  match enriched_ctx.get(service_key) {
    Some(service) => assert_eq(service, "api-gateway")
    None => @test.fail("Expected service name in context")
  }
  
  match enriched_ctx.get(version_key) {
    Some(version) => assert_eq(version, "3.2.1")
    None => @test.fail("Expected service version in context")
  }
  
  // 验证Baggage信息
  match enriched_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-67890")
    None => @test.fail("Expected user.id in baggage")
  }
  
  match enriched_baggage.get("request.path") {
    Some(path) => assert_eq(path, "/api/v2/users")
    None => @test.fail("Expected request.path in baggage")
  }
  
  match enriched_baggage.get("http.method") {
    Some(method) => assert_eq(method, "GET")
    None => @test.fail("Expected http.method in baggage")
  }
  
  // 验证Measurement的属性
  assert_eq(measurement.value, 156.7)
  assert_eq(measurement.attributes.length(), 20)
  
  // 验证Context和Baggage信息都在Measurement中
  let mut found_operation = false
  let mut found_user_id = false
  let mut found_request_path = false
  
  let mut i = 0
  while i < measurement.attributes.length() {
    let (key, value) = measurement.attributes[i]
    
    if key == "operation.name" {
      match value {
        StringValue(operation) => {
          assert_eq(operation, "http_request")
          found_operation = true
        }
        _ => @test.fail("Expected StringValue for operation.name")
      }
    }
    
    if key == "user.id" {
      match value {
        StringValue(user_id) => {
          assert_eq(user_id, "user-67890")
          found_user_id = true
        }
        _ => @test.fail("Expected StringValue for user.id")
      }
    }
    
    if key == "request.path" {
      match value {
        StringValue(path) => {
          assert_eq(path, "/api/v2/users")
          found_request_path = true
        }
        _ => @test.fail("Expected StringValue for request.path")
      }
    }
    
    i = i + 1
  }
  
  assert_eq(found_operation, true, "operation.name should be found")
  assert_eq(found_user_id, true, "user.id should be found")
  assert_eq(found_request_path, true, "request.path should be found")
}