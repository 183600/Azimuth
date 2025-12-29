test "telemetry_integration_tracing_and_metrics" {
  // 测试tracing和metrics的集成
  
  // 模拟一个完整的请求处理流程
  let service_name = "integration-test-service"
  let operation_name = "process-request"
  
  // 创建资源
  let resource = Resource::default(service_name)
  assert_eq(resource.service_name, service_name)
  
  // 模拟tracing操作
  let trace_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte,
                  9_byte, 10_byte, 11_byte, 12_byte, 13_byte, 14_byte, 15_byte, 16_byte]
  let span_id = [1_byte, 2_byte, 3_byte, 4_byte, 5_byte, 6_byte, 7_byte, 8_byte]
  
  let span_context = SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 模拟metrics操作
  let request_count_attrs : Attributes = [
    ("service.name", AttributeValue::string(service_name)),
    ("operation.name", AttributeValue::string(operation_name)),
    ("status", AttributeValue::string("success"))
  ]
  
  let request_count_measurement = Measurement::{
    value: 1.0,
    attributes: request_count_attrs
  }
  
  // 验证集成数据的一致性
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(request_count_attrs.length(), 3)
  
  // 验证service名称在所有组件中一致
  match request_count_attrs[0].1 {
    AttributeValue::StringValue(name) => assert_eq(name, service_name)
    _ => @test.fail("Expected StringValue")
  }
}

test "telemetry_integration_context_and_propagation" {
  // 测试context和propagation的集成
  
  // 创建基础context
  let base_ctx = Context::empty()
  let user_key = create_key("user.id")
  let request_key = create_key("request.id")
  
  // 添加context值
  let ctx_with_user = base_ctx.with_value(user_key, "user-123")
  let ctx_with_request = ctx_with_user.with_value(request_key, "req-456")
  
  // 创建baggage
  let baggage = Baggage::empty()
  let baggage_with_session = baggage.with_entry("session.id", "session-789")
  let baggage_with_locale = baggage_with_session.with_entry("user.locale", "zh-CN")
  
  // 创建carrier用于传播
  let carrier = MapCarrier::new()
  
  // 验证context和baggage的数据
  match ctx_with_request.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user-123")
    None => @test.fail("Expected user.id")
  }
  
  match ctx_with_request.get(request_key) {
    Some(request_id) => assert_eq(request_id, "req-456")
    None => @test.fail("Expected request.id")
  }
  
  match baggage_with_locale.get("session.id") {
    Some(session_id) => assert_eq(session_id, "session-789")
    None => @test.fail("Expected session.id")
  }
  
  match baggage_with_locale.get("user.locale") {
    Some(locale) => assert_eq(locale, "zh-CN")
    None => @test.fail("Expected user.locale")
  }
  
  // 验证carrier初始状态
  assert_eq(carrier.keys().length(), 0)
}

test "telemetry_integration_logs_and_tracing" {
  // 测试logs和tracing的集成
  
  // 模拟span和log的关联
  let span_context = SpanContext::{
    trace_id: [1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte, 1_byte,
               2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte, 2_byte],
    span_id: [3_byte, 3_byte, 3_byte, 3_byte, 3_byte, 3_byte, 3_byte, 3_byte],
    trace_flags: 1_byte,
    trace_state: ""
  }
  
  // 创建与span关联的log记录
  let log_with_span = LogRecord::builder()
    .severity(Info)
    .body("Operation completed successfully")
    .with_attribute("trace_id", AttributeValue::string("01010101010101010202020202020202"))
    .with_attribute("span_id", AttributeValue::string("0303030303030303"))
    .with_attribute("service.name", AttributeValue::string("integrated-service"))
    .build()
  
  // 验证log和span的关联
  match log_with_span.body {
    Some(body) => assert_eq(body, "Operation completed successfully")
    None => @test.fail("Expected log body")
  }
  
  assert_eq(log_with_span.attributes.length(), 3)
  
  // 验证trace_id属性
  match log_with_span.attributes[0].1 {
    AttributeValue::StringValue(trace_id) => {
      assert_eq(trace_id.length(), 32)  // 16 bytes = 32 hex chars
      assert_eq(trace_id.contains("01"), true)
      assert_eq(trace_id.contains("02"), true)
    }
    _ => @test.fail("Expected StringValue for trace_id")
  }
  
  // 验证span_id属性
  match log_with_span.attributes[1].1 {
    AttributeValue::StringValue(span_id) => {
      assert_eq(span_id.length(), 16)  // 8 bytes = 16 hex chars
      assert_eq(span_id.contains("03"), true)
    }
    _ => @test.fail("Expected StringValue for span_id")
  }
}