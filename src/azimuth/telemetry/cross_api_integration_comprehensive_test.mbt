// 跨API模块集成测试
// 测试Trace、Logs、Metrics三个API模块的协同工作

test "cross_api_integration_trace_logs_metrics" {
  // 创建基础资源
  let resource = common::Resource::default("test-service")
  
  // 创建Span相关数据
  let span_context = trace::SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { (i % 256) |> byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { (i % 256) |> byte }),
    trace_flags: 1_byte,
    trace_state: "test-state"
  }
  
  let span = trace::Span::{
    name: "test-operation",
    context: span_context,
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995201000000000L),
    status: trace::Ok,
    status_description: Some("Success"),
    attributes: [("http.method", common::AttributeValue::string("GET"))],
    events: [],
    links: []
  }
  
  // 创建LogRecord，关联到Span
  let log_record = logs::LogRecord::{
    timestamp_unix_nanos: 1640995200500000000L,
    observed_timestamp_unix_nanos: Some(1640995200500000000L),
    severity_number: logs::Info,
    severity_text: Some("INFO"),
    body: Some("Operation completed successfully"),
    attributes: [("user.id", common::AttributeValue::int(12345L))],
    trace_id: Some(span_context.trace_id),
    span_id: Some(span_context.span_id),
    trace_flags: Some(span_context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(common::InstrumentationScope::{
      name: "test-instrumentation",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 创建Metrics数据
  let metric_attributes = [("operation.name", common::AttributeValue::string("test-operation"))]
  let counter_value = 1L
  let histogram_value = 123.45
  
  // 验证数据关联性
  assert_eq(log_record.trace_id?, span_context.trace_id)
  assert_eq(log_record.span_id?, span_context.span_id)
  assert_eq(log_record.trace_flags?, span_context.trace_flags)
  
  // 验证时间戳顺序
  assert_eq(span.start_time_unix_nanos < log_record.timestamp_unix_nanos, true)
  assert_eq(log_record.timestamp_unix_nanos < span.end_time_unix_nanos?, true)
  
  // 验证资源一致性
  match log_record.resource {
    Some(log_resource) => assert_eq(log_resource.service_name, resource.service_name)
    None => @test.fail("Log record should have resource")
  }
  
  // 验证属性值类型
  match span.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "GET")
    _ => @test.fail("Attribute should be string value")
  }
  
  match log_record.attributes[0].1 {
    common::IntValue(i) => assert_eq(i, 12345L)
    _ => @test.fail("Attribute should be int value")
  }
  
  // 验证指标数据类型
  assert_eq(counter_value > 0L, true)
  assert_eq(histogram_value > 0.0, true)
}

test "cross_api_context_propagation_integration" {
  // 测试Context在Trace、Logs、Metrics间的传播
  
  // 创建基础Context
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace_id")
  let user_key = context::create_key("user_id")
  
  // 添加追踪相关信息
  let ctx_with_trace = ctx.with_value(trace_key, "0af7651916cd43dd8448eb211c80319c")
  let ctx_with_user = ctx_with_trace.with_value(user_key, "user123")
  
  // 验证Context数据
  match ctx_with_user.get(trace_key) {
    Some(trace_id) => assert_eq(trace_id, "0af7651916cd43dd8448eb211c80319c")
    None => @test.fail("Trace ID should be found in context")
  }
  
  match ctx_with_user.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user123")
    None => @test.fail("User ID should be found in context")
  }
  
  // 创建Baggage并测试集成
  let baggage = context::Baggage::empty()
  let baggage_with_data = baggage.with_entry("request.id", "req-456")
  let baggage_with_user = baggage_with_data.with_entry("user.role", "admin")
  
  match baggage_with_user.get("request.id") {
    Some(request_id) => assert_eq(request_id, "req-456")
    None => @test.fail("Request ID should be found in baggage")
  }
  
  match baggage_with_user.get("user.role") {
    Some(user_role) => assert_eq(user_role, "admin")
    None => @test.fail("User role should be found in baggage")
  }
  
  // 验证数据长度和格式
  match ctx_with_user.get(trace_key) {
    Some(trace_id) => {
      assert_eq(trace_id.length(), 32)
      assert_eq(trace_id.has_prefix("0af7"), true)
    }
    None => @test.fail("Trace ID should be found")
  }
}

test "cross_api_error_handling_integration" {
  // 测试跨API模块的错误处理
  
  // 创建错误状态的Span
  let error_span = trace::Span::{
    name: "error-operation",
    context: trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 255_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 255_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995202000000000L),
    status: trace::Error,
    status_description: Some("Database connection failed"),
    attributes: [("error.type", common::AttributeValue::string("connection_error"))],
    events: [],
    links: []
  }
  
  // 创建错误日志
  let error_log = logs::LogRecord::{
    timestamp_unix_nanos: 1640995201500000000L,
    observed_timestamp_unix_nanos: Some(1640995201500000000L),
    severity_number: logs::Error,
    severity_text: Some("ERROR"),
    body: Some("Failed to execute query: Connection timeout"),
    attributes: [
      ("error.code", common::AttributeValue::int(5001L)),
      ("error.message", common::AttributeValue::string("Connection timeout"))
    ],
    trace_id: Some(error_span.context.trace_id),
    span_id: Some(error_span.context.span_id),
    trace_flags: Some(error_span.context.trace_flags),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证错误状态传播
  assert_eq(error_span.status, trace::Error)
  assert_eq(error_log.severity_number, logs::Error)
  assert_eq(error_log.severity_text?, "ERROR")
  
  // 验证错误信息关联
  assert_eq(error_span.context.trace_id, error_log.trace_id?)
  assert_eq(error_span.context.span_id, error_log.span_id?)
  
  // 验证错误属性
  match error_span.attributes[0].1 {
    common::StringValue(error_type) => assert_eq(error_type, "connection_error")
    _ => @test.fail("Error type should be string")
  }
  
  match error_log.attributes[0].1 {
    common::IntValue(error_code) => assert_eq(error_code, 5001L)
    _ => @test.fail("Error code should be int")
  }
  
  match error_log.attributes[1].1 {
    common::StringValue(error_msg) => assert_eq(error_msg, "Connection timeout")
    _ => @test.fail("Error message should be string")
  }
}