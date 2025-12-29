// 跨模块集成测试用例
// 测试不同telemetry API模块之间的交互和集成

test "trace_metrics_integration" {
  // 测试Trace和Metrics模块的集成
  
  // 创建一个Span并记录相关指标
  let trace_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                  0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                  0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                  0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  
  let span_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                 0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01_byte,
    trace_state: ""
  }
  
  let test_span = Span::{
    name: "http-request",
    context: span_context,
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200001000000L),
    status: Ok,
    status_description: None,
    attributes: [
      ("http.method", AttributeValue::string("GET")),
      ("http.status_code", AttributeValue::int(200L)),
      ("http.url", AttributeValue::string("/api/users"))
    ],
    events: [],
    links: []
  }
  
  // 创建与Span相关的指标
  let request_counter_attrs = [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L)),
    ("span.name", AttributeValue::string("http-request"))
  ]
  
  let request_duration_attrs = [
    ("http.method", AttributeValue::string("GET")),
    ("http.url", AttributeValue::string("/api/users")),
    ("trace.id", AttributeValue::string("0123456789abcdef0123456789abcdef"))
  ]
  
  // 验证Span和指标的关联性
  assert_eq(test_span.name, "http-request")
  assert_eq(request_counter_attrs.length(), 3)
  assert_eq(request_duration_attrs.length(), 3)
  
  // 验证Span属性在指标中正确反映
  let mut found_method = false
  let mut found_status = false
  let mut found_span_name = false
  
  let mut i = 0
  while i < request_counter_attrs.length() {
    match request_counter_attrs[i] {
      ("http.method", AttributeValue::string(method)) => {
        assert_eq(method, "GET")
        found_method = true
      }
      ("http.status_code", AttributeValue::int(status)) => {
        assert_eq(status, 200L)
        found_status = true
      }
      ("span.name", AttributeValue::string(name)) => {
        assert_eq(name, "http-request")
        found_span_name = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(found_method, true)
  assert_eq(found_status, true)
  assert_eq(found_span_name, true)
}

test "trace_logs_integration" {
  // 测试Trace和Logs模块的集成
  
  // 创建一个带有日志事件的Span
  let trace_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                  0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte,
                  0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                  0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  
  let span_id = [0x01_byte, 0x23_byte, 0x45_byte, 0x67_byte,
                 0x89_byte, 0xAB_byte, 0xCD_byte, 0xEF_byte]
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 0x01_byte,
    trace_state: ""
  }
  
  // 创建与Span关联的日志记录
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000500000L,
    observed_timestamp_unix_nanos: Some(1640995200000500000L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Processing user request"),
    attributes: [
      ("user.id", AttributeValue::string("user-123")),
      ("operation", AttributeValue::string("get_profile"))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(0x01_byte),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证日志和Span的关联
  match log_record.trace_id {
    Some(log_trace_id) => {
      assert_eq(log_trace_id.length(), 16)
      let mut i = 0
      while i < log_trace_id.length() {
        assert_eq(log_trace_id[i], trace_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Expected trace_id in log record")
  }
  
  match log_record.span_id {
    Some(log_span_id) => {
      assert_eq(log_span_id.length(), 8)
      let mut i = 0
      while i < log_span_id.length() {
        assert_eq(log_span_id[i], span_id[i])
        i = i + 1
      }
    }
    None => @test.fail("Expected span_id in log record")
  }
  
  match log_record.trace_flags {
    Some(flags) => assert_eq(flags, 0x01_byte)
    None => @test.fail("Expected trace_flags in log record")
  }
  
  // 验证日志内容
  match log_record.body {
    Some(body) => assert_eq(body, "Processing user request")
    None => @test.fail("Expected body in log record")
  }
  
  match log_record.severity_text {
    Some(severity) => assert_eq(severity, "INFO")
    None => @test.fail("Expected severity_text in log record")
  }
}

test "context_propagation_integration" {
  // 测试Context、Propagation和Trace的集成
  
  // 创建初始Context
  let initial_context = Context::empty()
  let trace_key = create_key("trace.context")
  let baggage_key = create_key("baggage.items")
  
  // 添加trace信息到Context
  let trace_context = initial_context.with_value(trace_key, "trace-id:12345,span-id:67890")
  
  // 添加baggage信息到Context
  let baggage_context = trace_context.with_value(baggage_key, "user-id=abc,session=xyz")
  
  // 创建MapCarrier用于传播
  let carrier_data = [
    ("traceparent", "00-0123456789abcdef0123456789abcdef-67890abcdef12-01"),
    ("baggage", "user-id=abc,session=xyz")
  ]
  let carrier = MapCarrier::from_map(carrier_data)
  
  // 验证Context中的值
  match baggage_context.get(trace_key) {
    Some(trace_value) => {
      assert_eq(trace_value.contains("trace-id:12345"), true)
      assert_eq(trace_value.contains("span-id:67890"), true)
    }
    None => @test.fail("Expected trace context value")
  }
  
  match baggage_context.get(baggage_key) {
    Some(baggage_value) => {
      assert_eq(baggage_value.contains("user-id=abc"), true)
      assert_eq(baggage_value.contains("session=xyz"), true)
    }
    None => @test.fail("Expected baggage context value")
  }
  
  // 验证Carrier中的数据
  match carrier.get("traceparent") {
    Some(trace_parent) => {
      assert_eq(trace_parent.contains("0123456789abcdef"), true)
      assert_eq(trace_parent.contains("67890abcdef12"), true)
    }
    None => @test.fail("Expected traceparent in carrier")
  }
  
  match carrier.get("baggage") {
    Some(baggage) => {
      assert_eq(baggage.contains("user-id=abc"), true)
      assert_eq(baggage.contains("session=xyz"), true)
    }
    None => @test.fail("Expected baggage in carrier")
  }
  
  // 验证Carrier的keys
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)
  assert_eq(keys.contains("traceparent"), true)
  assert_eq(keys.contains("baggage"), true)
}

test "metrics_logs_resource_integration" {
  // 测试Metrics、Logs和Resource的集成
  
  // 创建共享的Resource
  let resource = Resource::default("payment-service")
  let resource_with_attrs = Resource::{
    service_name: "payment-service",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("service.instance.id", AttributeValue::string("instance-123")),
      ("host.name", AttributeValue::string("payment-server-01"))
    ]
  }
  
  // 创建带有Resource信息的指标
  let metric_attrs = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.version", AttributeValue::string("1.2.3")),
    ("deployment.environment", AttributeValue::string("production"))
  ]
  
  // 创建带有Resource信息的日志
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Payment processed successfully"),
    attributes: [
      ("payment.id", AttributeValue::string("pay-12345")),
      ("payment.amount", AttributeValue::float(99.99)),
      ("service.name", AttributeValue::string("payment-service")),
      ("deployment.environment", AttributeValue::string("production"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource_with_attrs),
    instrumentation_scope: None
  }
  
  // 验证Resource信息在指标和日志中的一致性
  assert_eq(resource_with_attrs.service_name, "payment-service")
  match resource_with_attrs.service_version {
    Some(version) => assert_eq(version, "1.2.3")
    None => @test.fail("Expected service version")
  }
  
  // 验证指标属性
  let mut found_service_name = false
  let mut found_service_version = false
  let mut found_environment = false
  
  let mut i = 0
  while i < metric_attrs.length() {
    match metric_attrs[i] {
      ("service.name", AttributeValue::string(name)) => {
        assert_eq(name, "payment-service")
        found_service_name = true
      }
      ("service.version", AttributeValue::string(version)) => {
        assert_eq(version, "1.2.3")
        found_service_version = true
      }
      ("deployment.environment", AttributeValue::string(env)) => {
        assert_eq(env, "production")
        found_environment = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_service_version, true)
  assert_eq(found_environment, true)
  
  // 验证日志记录中的Resource信息
  match log_record.resource {
    Some(log_resource) => {
      assert_eq(log_resource.service_name, "payment-service")
      match log_resource.service_version {
        Some(version) => assert_eq(version, "1.2.3")
        None => @test.fail("Expected service version in log resource")
      }
    }
    None => @test.fail("Expected resource in log record")
  }
  
  // 验证日志属性中的Resource信息
  let mut log_found_service_name = false
  let mut log_found_environment = false
  let mut log_found_payment_id = false
  let mut log_found_payment_amount = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    match log_record.attributes[i] {
      ("service.name", AttributeValue::string(name)) => {
        assert_eq(name, "payment-service")
        log_found_service_name = true
      }
      ("deployment.environment", AttributeValue::string(env)) => {
        assert_eq(env, "production")
        log_found_environment = true
      }
      ("payment.id", AttributeValue::string(id)) => {
        assert_eq(id, "pay-12345")
        log_found_payment_id = true
      }
      ("payment.amount", AttributeValue::float(amount)) => {
        assert_eq(amount, 99.99)
        log_found_payment_amount = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(log_found_service_name, true)
  assert_eq(log_found_environment, true)
  assert_eq(log_found_payment_id, true)
  assert_eq(log_found_payment_amount, true)
}