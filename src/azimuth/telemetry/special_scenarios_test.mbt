// Azimuth Telemetry - Special Scenarios Test
// 测试特殊使用场景和复杂的数据流

test "chained_span_operations" {
  // 测试链式Span操作
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("chained_tracer")
  let ctx = context::Context::empty()
  
  // 创建父子Span链
  let (ctx1, parent_span) = tracer.start_span(
    ctx,
    "parent_operation",
    trace::Server,
    [("service", AttributeValue::string("web_server"))]
  )
  
  let (ctx2, child_span1) = tracer.start_span(
    ctx1,
    "child_operation_1",
    trace::Internal,
    [("component", AttributeValue::string("database"))]
  )
  
  let (ctx3, child_span2) = tracer.start_span(
    ctx2,
    "child_operation_2",
    trace::Client,
    [("target", AttributeValue::string("external_api"))]
  )
  
  // 验证Span链关系
  assert_eq(parent_span.name, "parent_operation")
  assert_eq(parent_span.kind, trace::Server)
  assert_eq(child_span1.name, "child_operation_1")
  assert_eq(child_span1.kind, trace::Internal)
  assert_eq(child_span2.name, "child_operation_2")
  assert_eq(child_span2.kind, trace::Client)
  
  // 验证属性传播
  assert_eq(parent_span.attributes.length(), 1)
  assert_eq(child_span1.attributes.length(), 1)
  assert_eq(child_span2.attributes.length(), 1)
}

test "complex_attribute_scenarios" {
  // 测试复杂属性场景
  
  // 嵌套属性结构模拟
  let nested_attrs = [
    ("user.id", AttributeValue::int(12345)),
    ("user.name", AttributeValue::string("张三")),
    ("user.email", AttributeValue::string("zhangsan@example.com")),
    ("user.roles", AttributeValue::array_string(["admin", "user"])),
    ("request.method", AttributeValue::string("POST")),
    ("request.path", AttributeValue::string("/api/v1/users")),
    ("request.size", AttributeValue::int(1024)),
    ("response.status", AttributeValue::int(200)),
    ("response.time_ms", AttributeValue::float(150.5)),
    ("response.size", AttributeValue::int(2048))
  ]
  
  // 验证嵌套属性
  assert_eq(nested_attrs.length(), 10)
  assert_eq(nested_attrs[0], ("user.id", AttributeValue::int(12345)))
  assert_eq(nested_attrs[3], ("user.roles", AttributeValue::array_string(["admin", "user"])))
  
  // 测试属性值的复杂类型
  let complex_attrs = [
    ("unicode_text", AttributeValue::string("测试Unicode文本🚀")),
    ("json_string", AttributeValue::string("{\"key\": \"value\", \"number\": 42}")),
    ("url_encoded", AttributeValue::string("https%3A%2F%2Fexample.com%2Fpath%3Fparam%3Dvalue")),
    ("base64_data", AttributeValue::string("SGVsbG8gV29ybGQ=")),
    ("version_info", AttributeValue::array_string(["1.0.0", "2.0.0", "3.0.0"])),
    ("status_codes", AttributeValue::array_int([200, 201, 400, 404, 500])),
    ("latency_values", AttributeValue::array_float([10.5, 25.3, 50.7, 100.2])),
    ("feature_flags", AttributeValue::array_bool([true, false, true]))
  ]
  
  assert_eq(complex_attrs.length(), 8)
}

test "context_propagation_complex_flow" {
  // 测试复杂的上下文传播流程
  
  // 初始化Context
  let ctx = context::Context::empty()
  
  // 添加多个层次的上下文信息
  let ctx_with_user = ctx.with_value(
    context::create_key("user_id"),
    "user_12345"
  ).with_value(
    context::create_key("user_role"),
    "admin"
  )
  
  let ctx_with_request = ctx_with_user.with_value(
    context::create_key("request_id"),
    "req_abcdef"
  ).with_value(
    context::create_key("trace_id"),
    "trace_123456"
  )
  
  let ctx_with_metadata = ctx_with_request.with_value(
    context::create_key("service_name"),
    "auth_service"
  ).with_value(
    context::create_key("version"),
    "v2.1.0"
  )
  
  // 验证所有上下文值都能正确获取
  assert_eq(ctx_with_metadata.get(context::create_key("user_id")), Some("user_12345"))
  assert_eq(ctx_with_metadata.get(context::create_key("user_role")), Some("admin"))
  assert_eq(ctx_with_metadata.get(context::create_key("request_id")), Some("req_abcdef"))
  assert_eq(ctx_with_metadata.get(context::create_key("trace_id")), Some("trace_123456"))
  assert_eq(ctx_with_metadata.get(context::create_key("service_name")), Some("auth_service"))
  assert_eq(ctx_with_metadata.get(context::create_key("version")), Some("v2.1.0"))
  
  // 验证不存在的键返回None
  assert_eq(ctx_with_metadata.get(context::create_key("non_existent")), None)
}

test "baggage_complex_scenarios" {
  // 测试复杂的Baggage场景
  
  // 创建多层级的baggage
  let baggage = context::Baggage::empty()
    .with_entry("user.id", "12345")
    .with_entry("user.tier", "premium")
    .with_entry("request.source", "mobile_app")
    .with_entry("request.version", "3.2.1")
    .with_entry("experiment.id", "exp_ab_test_001")
    .with_entry("experiment.variant", "control")
    .with_entry("geo.country", "CN")
    .with_entry("geo.city", "Beijing")
  
  // 验证所有baggage条目
  assert_eq(baggage.get("user.id"), Some("12345"))
  assert_eq(baggage.get("user.tier"), Some("premium"))
  assert_eq(baggage.get("request.source"), Some("mobile_app"))
  assert_eq(baggage.get("experiment.id"), Some("exp_ab_test_001"))
  assert_eq(baggage.get("geo.country"), Some("CN"))
  
  // 测试baggage值的覆盖
  let updated_baggage = baggage
    .with_entry("user.tier", "enterprise")  // 覆盖现有值
    .with_entry("request.version", "3.3.0")  // 更新版本
  
  assert_eq(updated_baggage.get("user.tier"), Some("enterprise"))
  assert_eq(updated_baggage.get("request.version"), Some("3.3.0"))
  
  // 验证原始baggage不受影响
  assert_eq(baggage.get("user.tier"), Some("premium"))
  assert_eq(baggage.get("request.version"), Some("3.2.1"))
}

test "multi_propagator_complex_scenario" {
  // 测试多传播器的复杂场景
  
  let ctx = context::Context::empty()
  
  // 创建包含多种头信息的carrier
  let carrier_data = [
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("tracestate", "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"),
    ("baggage", "userId=user123,sessionId=session456,expId=exp789"),
    ("x-correlation-id", "corr-12345"),
    ("x-request-id", "req-abcdef"),
    ("custom-header-1", "value1"),
    ("custom-header-2", "value2")
  ]
  
  let carrier = propagation::MapCarrier::from_map(carrier_data)
  
  // 创建多个传播器
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  // 创建复合传播器
  let composite_propagator = propagation::CompositePropagator::new([
    trace_propagator,
    baggage_propagator
  ])
  
  // 执行提取操作
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 在提取的上下文中添加信息
  let enriched_ctx = extracted_ctx
    .with_value(context::create_key("processing_time"), "150ms")
    .with_value(context::create_key("service_name"), "api_gateway")
  
  // 执行注入操作到新的carrier
  let new_carrier = propagation::MapCarrier::new()
  composite_propagator.inject(enriched_ctx, new_carrier)
  
  // 验证carrier包含预期的头信息
  let keys = carrier.keys()
  assert_eq(keys.contains("traceparent"), true)
  assert_eq(keys.contains("baggage"), true)
  assert_eq(keys.contains("x-correlation-id"), true)
}

test "log_record_complex_building" {
  // 测试复杂的LogRecord构建场景
  
  // 创建带有丰富属性的LogRecord
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Error)
    .body("Database connection failed")
    .with_attribute("error.code", AttributeValue::int(500))
    .with_attribute("error.type", AttributeValue::string("ConnectionError"))
    .with_attribute("database.host", AttributeValue::string("db.example.com"))
    .with_attribute("database.port", AttributeValue::int(5432))
    .with_attribute("retry.count", AttributeValue::int(3))
    .with_attribute("retry.delay_ms", AttributeValue::array_float([100, 200, 400]))
    .with_attribute("affected.tables", AttributeValue::array_string(["users", "orders", "products"]))
    .with_attribute("transaction.id", AttributeValue::string("txn_12345"))
    |> build
  
  // 验证LogRecord结构
  assert_eq(log_record.severity_number, logs::Error)
  assert_eq(log_record.body, Some("Database connection failed"))
  assert_eq(log_record.attributes.length(), 8)
  
  // 验证特定属性
  let mut found_error_code = false
  let mut found_retry_delays = false
  let mut found_affected_tables = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match key {
      "error.code" => {
        assert_eq(value, AttributeValue::int(500))
        found_error_code = true
      }
      "retry.delay_ms" => {
        assert_eq(value, AttributeValue::array_float([100, 200, 400]))
        found_retry_delays = true
      }
      "affected.tables" => {
        assert_eq(value, AttributeValue::array_string(["users", "orders", "products"]))
        found_affected_tables = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_error_code, true)
  assert_eq(found_retry_delays, true)
  assert_eq(found_affected_tables, true)
}

test "metrics_complex_scenarios" {
  // 测试复杂的Metrics场景
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("complex_metrics")
  
  // 创建多个不同类型的指标
  let request_counter = meter.create_counter(
    "http_requests_total",
    Some("requests"),
    Some("Total HTTP requests")
  )
  
  let response_histogram = meter.create_histogram(
    "http_response_time_seconds",
    Some("seconds"),
    Some("HTTP response time in seconds")
  )
  
  let active_connections = meter.create_gauge(
    "active_connections",
    Some("connections"),
    Some("Active database connections")
  )
  
  let queue_size = meter.create_up_down_counter(
    "queue_size",
    Some("items"),
    Some("Current queue size")
  )
  
  // 模拟复杂的指标操作序列
  
  // 模拟请求处理
  let mut i = 0
  while i < 100 {
    // 记录请求
    request_counter.add(1, [
      ("method", AttributeValue::string(if i % 2 == 0 { "GET" } else { "POST" })),
      ("status", AttributeValue::int(if i % 10 == 0 { 500 } else { 200 })),
      ("endpoint", AttributeValue::string("/api/v1/resource"))
    ])
    
    // 记录响应时间
    response_histogram.record(
      0.1 + (i.to_double() * 0.001),
      [
        ("method", AttributeValue::string(if i % 2 == 0 { "GET" } else { "POST" })),
        ("endpoint", AttributeValue::string("/api/v1/resource"))
      ]
    )
    
    i = i + 1
  }
  
  // 模拟连接数变化
  i = 0
  while i < 50 {
    active_connections.record(10 + (i % 20).to_double(), [
      ("database", AttributeValue::string("primary")),
      ("pool", AttributeValue::string("write"))
    ])
    i = i + 1
  }
  
  // 模拟队列大小变化
  i = 0
  while i < 30 {
    if i % 3 == 0 {
      queue_size.add(5, [
        ("queue_type", AttributeValue::string("priority")),
        ("worker", AttributeValue::string("worker_" + (i % 5).to_string()))
      ])
    } else {
      queue_size.add(-3, [
        ("queue_type", AttributeValue::string("priority")),
        ("worker", AttributeValue::string("worker_" + (i % 5).to_string()))
      ])
    }
    i = i + 1
  }
}