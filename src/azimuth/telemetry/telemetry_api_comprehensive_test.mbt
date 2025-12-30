// 遥测API综合测试用例
// 测试OpenTelemetry风格遥测套件的核心功能

test "attribute_value_types_and_conversions" {
  // 测试属性值类型和转换
  
  // 测试字符串属性值
  let string_attr = common::AttributeValue::string("test-service")
  assert_eq(string_attr isa common::AttributeValue::StringValue, true)
  
  // 测试整数属性值
  let int_attr = common::AttributeValue::int(42L)
  assert_eq(int_attr isa common::AttributeValue::IntValue, true)
  
  // 测试浮点属性值
  let float_attr = common::AttributeValue::float(3.14159)
  assert_eq(float_attr isa common::AttributeValue::FloatValue, true)
  
  // 测试布尔属性值
  let bool_attr = common::AttributeValue::bool(true)
  assert_eq(bool_attr isa common::AttributeValue::BoolValue, true)
  
  // 测试数组属性值
  let string_array = ["item1", "item2", "item3"]
  let array_attr = common::AttributeValue::array_string(string_array)
  assert_eq(array_attr isa common::AttributeValue::ArrayStringValue, true)
  
  // 创建属性集合
  let attributes = [
    ("service.name", string_attr),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("request.count", int_attr),
    ("response.time", float_attr),
    ("service.enabled", bool_attr),
    ("service.tags", array_attr)
  ]
  
  // 验证属性数量
  assert_eq(attributes.length(), 6)
  
  // 验证特定属性
  assert_eq(attributes[0].0, "service.name")
  assert_eq(attributes[2].0, "request.count")
  assert_eq(attributes[4].0, "service.enabled")
}

test "resource_creation_and_defaults" {
  // 测试资源创建和默认值
  
  // 使用默认构造函数创建资源
  let resource = common::Resource::default("payment-service")
  
  // 验证基本属性
  assert_eq(resource.service_name, "payment-service")
  assert_eq(resource.service_version, None)
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  assert_eq(resource.attributes.length(), 0)
  
  // 创建带有属性的完整资源
  let full_resource = common::Resource{
    service_name: "order-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("instance.id", common::AttributeValue::string("i-1234567890abcdef0"))
    ]
  }
  
  // 验证完整资源属性
  assert_eq(full_resource.service_name, "order-service")
  assert_eq(full_resource.service_version, Some("2.1.0"))
  assert_eq(full_resource.attributes.length(), 3)
  assert_eq(full_resource.attributes[0].0, "environment")
  assert_eq(full_resource.attributes[1].0, "region")
  assert_eq(full_resource.attributes[2].0, "instance.id")
}

test "context_operations_and_key_management" {
  // 测试上下文操作和键管理
  
  // 创建空上下文
  let ctx = context::Context::empty()
  assert_eq(ctx.values.length(), 0)
  
  // 创建上下文键
  let trace_key = context::create_key("trace.id")
  let user_key = context::create_key("user.id")
  let request_key = context::create_key("request.id")
  
  // 验证键创建
  assert_eq(trace_key.name, "trace.id")
  assert_eq(user_key.name, "user.id")
  assert_eq(request_key.name, "request.id")
  
  // 添加值到上下文
  let ctx_with_trace = ctx.with_value(trace_key, "0af7651916cd43dd8448eb211c80319c")
  let ctx_with_user = ctx_with_trace.with_value(user_key, "user-12345")
  let ctx_with_request = ctx_with_user.with_value(request_key, "req-67890")
  
  // 验证上下文值数量
  assert_eq(ctx_with_request.values.length(), 3)
  
  // 验证从上下文获取值
  assert_eq(ctx_with_request.get(trace_key), Some("0af7651916cd43dd8448eb211c80319c"))
  assert_eq(ctx_with_request.get(user_key), Some("user-12345"))
  assert_eq(ctx_with_request.get(request_key), Some("req-67890"))
  
  // 测试不存在的键
  let missing_key = context::create_key("missing.key")
  assert_eq(ctx_with_request.get(missing_key), None)
}

test "baggage_operations_and_entry_management" {
  // 测试行李操作和条目管理
  
  // 创建空行李
  let baggage = context::Baggage::empty()
  assert_eq(baggage.entries.length(), 0)
  
  // 添加条目到行李
  let baggage_with_session = baggage.with_entry("session.id", "sess-abcdef123456")
  let baggage_with_user = baggage_with_session.with_entry("user.role", "admin")
  let baggage_with_region = baggage_with_user.with_entry("user.region", "eu-west-1")
  
  // 验证行李条目数量
  assert_eq(baggage_with_region.entries.length(), 3)
  
  // 验证从行李获取条目
  assert_eq(baggage_with_region.get("session.id"), Some("sess-abcdef123456"))
  assert_eq(baggage_with_region.get("user.role"), Some("admin"))
  assert_eq(baggage_with_region.get("user.region"), Some("eu-west-1"))
  
  // 测试不存在的条目
  assert_eq(baggage_with_region.get("nonexistent.entry"), None)
  
  // 验证条目内容
  let mut i = 0
  while i < baggage_with_region.entries.length() {
    let (key, value) = baggage_with_region.entries[i]
    match key {
      "session.id" => assert_eq(value, "sess-abcdef123456")
      "user.role" => assert_eq(value, "admin")
      "user.region" => assert_eq(value, "eu-west-1")
      _ => assert_eq(false, true)  // 不应该有其他键
    }
    i = i + 1
  }
}

test "span_context_and_trace_identification" {
  // 测试跨度上下文和追踪标识
  
  // 创建跨度上下文
  let span_context = trace::SpanContext{
    trace_id: [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c],
    span_id: [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31],
    trace_flags: 0x01,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  // 验证trace_id长度（16字节）
  assert_eq(span_context.trace_id.length(), 16)
  
  // 验证span_id长度（8字节）
  assert_eq(span_context.span_id.length(), 8)
  
  // 验证trace_flags
  assert_eq(span_context.trace_flags, 0x01)
  
  // 验证trace_state
  assert_eq(span_context.trace_state.contains("rojo=00f067aa0ba902b7"), true)
  assert_eq(span_context.trace_state.contains("congo=t61rcWkgMzE"), true)
  
  // 创建完整的跨度
  let span = trace::Span{
    name: "http.request",
    context: span_context,
    kind: trace::Server,
    parent_span_id: Some([0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200500000000L),
    status: trace::Ok,
    status_description: Some("Request completed successfully"),
    attributes: [
      ("http.method", common::AttributeValue::string("GET")),
      ("http.url", common::AttributeValue::string("/api/users")),
      ("http.status_code", common::AttributeValue::int(200L))
    ],
    events: [],
    links: []
  }
  
  // 验证跨度属性
  assert_eq(span.name, "http.request")
  assert_eq(span.kind isa trace::SpanKind::Server, true)
  assert_eq(span.status isa trace::StatusCode::Ok, true)
  assert_eq(span.attributes.length(), 3)
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  assert_eq(span.end_time_unix_nanos, Some(1640995200500000000L))
}

test "metrics_instrument_creation_and_operations" {
  // 测试指标仪器创建和操作
  
  // 创建No-op计量提供者
  let meter_provider = metrics::NoopMeterProvider{}
  
  // 获取计量器
  let meter = meter_provider.get_meter("test-service", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 创建各种指标仪器
  let counter = meter.create_counter("http.requests.total", Some("requests"), Some("Total HTTP requests"))
  let histogram = meter.create_histogram("http.request.duration", Some("ms"), Some("HTTP request duration"))
  let up_down_counter = meter.create_up_down_counter("active.connections", Some("connections"), Some("Active connections"))
  let gauge = meter.create_gauge("cpu.utilization", Some("%"), Some("CPU utilization percentage"))
  
  // 验证仪器创建（所有应该是No-op实现）
  assert_eq(counter isa metrics::NoopCounter, true)
  assert_eq(histogram isa metrics::NoopHistogram, true)
  assert_eq(up_down_counter isa metrics::NoopUpDownCounter, true)
  assert_eq(gauge isa metrics::NoopGauge, true)
  
  // 测试计数器操作
  counter.add(1L, [
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("200"))
  ])
  
  counter.add(1L, [
    ("method", common::AttributeValue::string("POST")),
    ("status", common::AttributeValue::string("201"))
  ])
  
  // 测试直方图操作
  histogram.record(125.5, [
    ("endpoint", common::AttributeValue::string("/api/users"))
  ])
  
  histogram.record(89.2, [
    ("endpoint", common::AttributeValue::string("/api/orders"))
  ])
  
  // 测试上下计数器操作
  up_down_counter.add(5L, [
    ("connection.type", common::AttributeValue::string("websocket"))
  ])
  
  up_down_counter.add(-2L, [
    ("connection.type", common::AttributeValue::string("websocket"))
  ])
  
  // 测试仪表操作
  gauge.record(75.8, [
    ("core", common::AttributeValue::int(0L))
  ])
  
  gauge.record(82.1, [
    ("core", common::AttributeValue::int(1L))
  ])
}

test "log_record_builder_and_severity_levels" {
  // 测试日志记录构建器和严重性级别
  
  // 使用构建器创建日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("User login successful")
    .with_attribute("user.id", common::AttributeValue::string("user-12345"))
    .with_attribute("ip.address", common::AttributeValue::string("192.168.1.100"))
    .with_attribute("user.agent", common::AttributeValue::string("Mozilla/5.0"))
    .build()
  
  // 验证日志记录属性
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.severity_number isa logs::SeverityNumber::Info, true)
  assert_eq(log_record.body, Some("User login successful"))
  assert_eq(log_record.attributes.length(), 3)
  
  // 验证属性内容
  assert_eq(log_record.attributes[0].0, "user.id")
  assert_eq(log_record.attributes[1].0, "ip.address")
  assert_eq(log_record.attributes[2].0, "user.agent")
  
  // 创建不同严重性级别的日志记录
  let debug_log = logs::LogRecord::builder()
    .severity(logs::Debug)
    .body("Debugging user session")
    .build()
  
  let warn_log = logs::LogRecord::builder()
    .severity(logs::Warn)
    .body("High memory usage detected")
    .build()
  
  let error_log = logs::LogRecord::builder()
    .severity(logs::Error)
    .body("Database connection failed")
    .build()
  
  let fatal_log = logs::LogRecord::builder()
    .severity(logs::Fatal)
    .body("System crash detected")
    .build()
  
  // 验证严重性级别
  assert_eq(debug_log.severity_number isa logs::SeverityNumber::Debug, true)
  assert_eq(warn_log.severity_number isa logs::SeverityNumber::Warn, true)
  assert_eq(error_log.severity_number isa logs::SeverityNumber::Error, true)
  assert_eq(fatal_log.severity_number isa logs::SeverityNumber::Fatal, true)
  
  // 创建带有追踪信息的日志记录
  let trace_log = logs::LogRecord::builder()
    .severity(logs::Error)
    .body("Payment processing failed")
    .with_attribute("error.code", common::AttributeValue::string("PAYMENT_DECLINED"))
    .with_attribute("amount", common::AttributeValue::float(99.99))
    .build()
  
  // 设置追踪上下文（在真实实现中会从上下文提取）
  let trace_log_with_context = logs::LogRecord{
    ..trace_log,
    trace_id: Some([0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]),
    span_id: Some([0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]),
    trace_flags: Some(0x01)
  }
  
  // 验证追踪上下文
  assert_eq(trace_log_with_context.trace_id, Some([0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]))
  assert_eq(trace_log_with_context.span_id, Some([0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]))
  assert_eq(trace_log_with_context.trace_flags, Some(0x01))
}

test "w3c_propagation_and_context_injection" {
  // 测试W3C传播和上下文注入
  
  // 创建传播器
  let trace_propagator = propagation::W3CTraceContextPropagator{}
  let baggage_propagator = propagation::W3CBaggagePropagator{}
  let composite_propagator = propagation::CompositePropagator::new([
    trace_propagator,
    baggage_propagator
  ])
  
  // 创建载体
  let carrier_data = [
    ("existing-header", "existing-value")
  ]
  let carrier = propagation::MapCarrier::from_map(carrier_data)
  
  // 创建上下文
  let ctx = context::Context::empty()
  
  // 注入上下文到载体
  composite_propagator.inject(ctx, carrier)
  
  // 验证载体键
  let keys = carrier.keys()
  assert_eq(keys.length(), 1)  // 原始existing-header
  assert_eq(keys.contains("existing-header"), true)
  
  // 验证获取值
  assert_eq(carrier.get("existing-header"), Some("existing-value"))
  
  // 从载体提取上下文
  let extracted_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证提取的上下文（简化实现中应该与原上下文相同）
  assert_eq(extracted_ctx.values.length(), ctx.values.length())
  
  // 测试单独的trace传播器
  let trace_carrier = propagation::MapCarrier::new()
  trace_propagator.inject(ctx, trace_carrier)
  
  // 测试单独的baggage传播器
  let baggage_carrier = propagation::MapCarrier::new()
  baggage_propagator.inject(ctx, baggage_carrier)
  
  // 验证常量
  assert_eq(propagation::TRACE_PARENT_HEADER, "traceparent")
  assert_eq(propagation::TRACE_STATE_HEADER, "tracestate")
  assert_eq(propagation::BAGGAGE_HEADER, "baggage")
}

test "telemetry_integration_workflow" {
  // 测试遥测集成工作流
  
  // 创建资源
  let resource = common::Resource::default("integration-test-service")
  assert_eq(resource.service_name, "integration-test-service")
  
  // 创建上下文
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace.id")
  let ctx_with_trace = ctx.with_value(trace_key, "integration-trace-12345")
  
  // 创建行李
  let baggage = context::Baggage::empty()
  let baggage_with_session = baggage.with_entry("session.id", "integration-session-67890")
  
  // 创建跨度上下文
  let span_context = trace::SpanContext{
    trace_id: [0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef],
    span_id: [0xfe, 0xdc, 0xba, 0x98, 0x76, 0x54, 0x32, 0x10],
    trace_flags: 0x01,
    trace_state: "test=value1"
  }
  
  // 创建跨度
  let span = trace::Span{
    name: "integration.workflow",
    context: span_context,
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: trace::Ok,
    status_description: Some("Integration workflow completed"),
    attributes: [
      ("workflow.name", common::AttributeValue::string("test-workflow")),
      ("workflow.version", common::AttributeValue::string("1.0.0")),
      ("workflow.duration", common::AttributeValue::float(100.0))
    ],
    events: [],
    links: []
  }
  
  // 创建计量器
  let meter_provider = metrics::NoopMeterProvider{}
  let meter = meter_provider.get_meter("integration-test")
  let counter = meter.create_counter("workflow.executions", Some("executions"), Some("Workflow executions"))
  let histogram = meter.create_histogram("workflow.duration", Some("ms"), Some("Workflow duration"))
  
  // 执行指标操作
  counter.add(1L, [
    ("workflow.name", common::AttributeValue::string("test-workflow")),
    ("workflow.status", common::AttributeValue::string("success"))
  ])
  
  histogram.record(100.0, [
    ("workflow.name", common::AttributeValue::string("test-workflow"))
  ])
  
  // 创建日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200050000000L)
    .severity(logs::Info)
    .body("Integration workflow step completed")
    .with_attribute("workflow.step", common::AttributeValue::string("data-processing"))
    .with_attribute("step.status", common::AttributeValue::string("success"))
    .build()
  
  // 创建传播器
  let propagator = propagation::CompositePropagator::new([
    propagation::W3CTraceContextPropagator{},
    propagation::W3CBaggagePropagator{}
  ])
  
  // 创建载体
  let carrier = propagation::MapCarrier::new()
  
  // 注入上下文
  propagator.inject(ctx_with_trace, carrier)
  
  // 验证集成工作流组件
  assert_eq(resource.service_name, "integration-test-service")
  assert_eq(ctx_with_trace.get(trace_key), Some("integration-trace-12345"))
  assert_eq(baggage_with_session.get("session.id"), Some("integration-session-67890"))
  assert_eq(span.name, "integration.workflow")
  assert_eq(span.status isa trace::StatusCode::Ok, true)
  assert_eq(counter isa metrics::NoopCounter, true)
  assert_eq(histogram isa metrics::NoopHistogram, true)
  assert_eq(log_record.severity_number isa logs::SeverityNumber::Info, true)
  assert_eq(log_record.body, Some("Integration workflow step completed"))
  assert_eq(propagator isa propagation::CompositePropagator, true)
}