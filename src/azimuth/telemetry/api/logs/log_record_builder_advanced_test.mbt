// LogRecordBuilder 链式调用高级测试用例
// 测试LogRecordBuilder的链式调用模式和复杂场景

test "log_record_builder_basic_chaining" {
  // 测试LogRecordBuilder的基本链式调用
  
  let builder = logs::LogRecord::builder()
  
  // 链式设置基本属性
  let log_record = builder
    .timestamp(1640995200000000000L)
    .severity(logs::Error)
    .body("Database connection failed")
    .build()
  
  // 验证基本属性
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  match log_record.severity_number {
    logs::Error => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  match log_record.body {
    Some(body) => assert_eq(body, "Database connection failed")
    None => assert_eq(false, true)
  }
  
  // 验证其他默认值
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  match log_record.severity_text {
    None => assert_eq(true, true)
    Some(_) => assert_eq(false, true)
  }
  assert_eq(log_record.attributes.length(), 0)
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
}

test "log_record_builder_attribute_chaining" {
  // 测试LogRecordBuilder的属性链式添加
  
  let builder = logs::LogRecord::builder()
  
  // 链式添加多个属性
  let log_record = builder
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("HTTP request processed")
    .with_attribute("http.method", AttributeValue::string("GET"))
    .with_attribute("http.status_code", AttributeValue::int(200L))
    .with_attribute("http.duration_ms", AttributeValue::float(123.456))
    .with_attribute("http.success", AttributeValue::bool(true))
    .with_attribute("user.roles", AttributeValue::array_string(["admin", "user"]))
    .build()
  
  // 验证属性数量和内容
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证每个属性
  assert_eq(log_record.attributes[0].0, "http.method")
  match log_record.attributes[0].1 {
    StringValue(method) => assert_eq(method, "GET")
    _ => assert_eq(false, true)
  }
  
  assert_eq(log_record.attributes[1].0, "http.status_code")
  match log_record.attributes[1].1 {
    IntValue(status) => assert_eq(status, 200L)
    _ => assert_eq(false, true)
  }
  
  assert_eq(log_record.attributes[2].0, "http.duration_ms")
  match log_record.attributes[2].1 {
    FloatValue(duration) => {
      assert_eq(duration > 120.0 && duration < 130.0, true)
    }
    _ => assert_eq(false, true)
  }
  
  assert_eq(log_record.attributes[3].0, "http.success")
  match log_record.attributes[3].1 {
    BoolValue(success) => assert_eq(success, true)
    _ => assert_eq(false, true)
  }
  
  assert_eq(log_record.attributes[4].0, "user.roles")
  match log_record.attributes[4].1 {
    ArrayStringValue(roles) => {
      assert_eq(roles.length(), 2)
      assert_eq(roles[0], "admin")
      assert_eq(roles[1], "user")
    }
    _ => assert_eq(false, true)
  }
}

test "log_record_builder_complex_severity_scenarios" {
  // 测试LogRecordBuilder的复杂严重性场景
  
  let builder = logs::LogRecord::builder()
  
  // 测试不同严重性级别
  let trace_log = builder
    .severity(logs::Trace)
    .body("Detailed trace information")
    .build()
  match trace_log.severity_number {
    logs::Trace => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let debug_log = builder
    .severity(logs::Debug)
    .body("Debug information")
    .build()
  match debug_log.severity_number {
    logs::Debug => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let info_log = builder
    .severity(logs::Info)
    .body("General information")
    .build()
  match info_log.severity_number {
    logs::Info => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let warn_log = builder
    .severity(logs::Warn)
    .body("Warning condition")
    .build()
  match warn_log.severity_number {
    logs::Warn => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let error_log = builder
    .severity(logs::Error)
    .body("Error condition")
    .build()
  match error_log.severity_number {
    logs::Error => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  let fatal_log = builder
    .severity(logs::Fatal)
    .body("Fatal error condition")
    .build()
  match fatal_log.severity_number {
    logs::Fatal => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
}

test "log_record_builder_trace_context_integration" {
  // 测试LogRecordBuilder与追踪上下文的集成
  
  let builder = logs::LogRecord::builder()
  
  // 创建追踪ID和Span ID
  let trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 2_byte })
  let trace_flags = Some(1_byte)
  
  // 创建带有追踪上下文的日志记录
  let log_with_trace = builder
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Operation completed with trace context")
    .with_attribute("operation.name", AttributeValue::string("user-login"))
    .build()
  
  // 手动设置追踪上下文（因为简化实现中没有直接的方法）
  let log_record_with_trace = logs::LogRecord::{
    ..log_with_trace,
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: trace_flags
  }
  
  // 验证追踪上下文
  match log_record_with_trace.trace_id {
    Some(id) => {
      assert_eq(id.length(), 16)
      assert_eq(id[0], 1_byte)
    }
    None => assert_eq(false, true)
  }
  
  match log_record_with_trace.span_id {
    Some(id) => {
      assert_eq(id.length(), 8)
      assert_eq(id[0], 2_byte)
    }
    None => assert_eq(false, true)
  }
  
  match log_record_with_trace.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => assert_eq(false, true)
  }
  
  // 验证其他属性
  assert_eq(log_record_with_trace.timestamp_unix_nanos, 1640995200000000000L)
  match log_record_with_trace.severity_number {
    logs::Info => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  match log_record_with_trace.body {
    Some(body) => assert_eq(body, "Operation completed with trace context")
    None => assert_eq(false, true)
  }
  assert_eq(log_record_with_trace.attributes.length(), 1)
}

test "log_record_builder_resource_and_instrumentation_scope" {
  // 测试LogRecordBuilder与资源和InstrumentationScope的集成
  
  let builder = logs::LogRecord::builder()
  
  // 创建资源
  let resource = common::Resource::default("auth-service")
  let resource_with_attrs = common::Resource::{
    ..resource,
    service_version: Some("2.1.0"),
    attributes: [
      ("deployment.environment", AttributeValue::string("production")),
      ("service.instance.id", AttributeValue::string("instance-123"))
    ]
  }
  
  // 创建InstrumentationScope
  let instrumentation_scope = common::InstrumentationScope::{
    name: "auth-middleware",
    version: Some("1.5.2"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  
  // 创建带有资源和InstrumentationScope的日志记录
  let log_record = builder
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("User authentication successful")
    .with_attribute("user.id", AttributeValue::string("user-456"))
    .with_attribute("auth.method", AttributeValue::string("oauth2"))
    .build()
  
  // 手动设置资源和InstrumentationScope
  let complete_log_record = logs::LogRecord::{
    ..log_record,
    resource: Some(resource_with_attrs),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证资源
  match complete_log_record.resource {
    Some(res) => {
      assert_eq(res.service_name, "auth-service")
      match res.service_version {
        Some(version) => assert_eq(version, "2.1.0")
        None => assert_eq(false, true)
      }
      assert_eq(res.telemetry_sdk_name, "azimuth")
      assert_eq(res.telemetry_sdk_version, "0.1.0")
      assert_eq(res.attributes.length(), 2)
      assert_eq(res.attributes[0].0, "deployment.environment")
      match res.attributes[0].1 {
        StringValue(env) => assert_eq(env, "production")
        _ => assert_eq(false, true)
      }
    }
    None => assert_eq(false, true)
  }
  
  // 验证InstrumentationScope
  match complete_log_record.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "auth-middleware")
      match scope.version {
        Some(version) => assert_eq(version, "1.5.2")
        None => assert_eq(false, true)
      }
      match scope.schema_url {
        Some(schema_url) => assert_eq(schema_url, "https://opentelemetry.io/schemas/1.20.0")
        None => assert_eq(false, true)
      }
    }
    None => assert_eq(false, true)
  }
  
  // 验证其他属性
  match complete_log_record.body {
    Some(body) => assert_eq(body, "User authentication successful")
    None => assert_eq(false, true)
  }
  assert_eq(complete_log_record.attributes.length(), 2)
  assert_eq(complete_log_record.attributes[0].0, "user.id")
  assert_eq(complete_log_record.attributes[1].0, "auth.method")
}

test "log_record_builder_observed_timestamp_and_severity_text" {
  // 测试LogRecordBuilder的观察时间戳和严重性文本
  
  let builder = logs::LogRecord::builder()
  
  // 创建带有观察时间戳和严重性文本的日志记录
  let log_record = builder
    .timestamp(1640995200000000000L)  // 事件时间戳
    .severity(logs::Warn)
    .body("Resource usage warning")
    .with_attribute("cpu.usage", AttributeValue::float(85.5))
    .with_attribute("memory.usage", AttributeValue::float(78.2))
    .build()
  
  // 手动设置观察时间戳和严重性文本
  let complete_log_record = logs::LogRecord::{
    ..log_record,
    observed_timestamp_unix_nanos: Some(1640995200000001000L),  // 稍晚观察
    severity_text: Some("HIGH_CPU_USAGE")
  }
  
  // 验证观察时间戳
  match complete_log_record.observed_timestamp_unix_nanos {
    Some(observed) => {
      assert_eq(observed, 1640995200000001000L)
      let delay = observed - complete_log_record.timestamp_unix_nanos
      assert_eq(delay, 1000L)  // 1000纳秒延迟
    }
    None => assert_eq(false, true)
  }
  
  // 验证严重性文本
  match complete_log_record.severity_text {
    Some(text) => assert_eq(text, "HIGH_CPU_USAGE")
    None => assert_eq(false, true)
  }
  
  // 验证其他属性
  assert_eq(complete_log_record.timestamp_unix_nanos, 1640995200000000000L)
  match complete_log_record.severity_number {
    logs::Warn => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  match complete_log_record.body {
    Some(body) => assert_eq(body, "Resource usage warning")
    None => assert_eq(false, true)
  }
  assert_eq(complete_log_record.attributes.length(), 2)
  
  // 验证属性值
  assert_eq(complete_log_record.attributes[0].0, "cpu.usage")
  match complete_log_record.attributes[0].1 {
    FloatValue(cpu) => assert_eq(cpu > 85.0 && cpu < 86.0, true)
    _ => assert_eq(false, true)
  }
  
  assert_eq(complete_log_record.attributes[1].0, "memory.usage")
  match complete_log_record.attributes[1].1 {
    FloatValue(memory) => assert_eq(memory > 78.0 && memory < 79.0, true)
    _ => assert_eq(false, true)
  }
}