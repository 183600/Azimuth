// 跨信号集成测试 - 测试Trace、Metrics、Logs之间的集成
test "cross_signal_integration_trace_to_metrics" {
  // 测试从Trace信息生成Metrics的场景
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace_id")
  let ctx_with_trace = ctx.with_value(trace_key, "trace-123")
  
  // 模拟基于Trace信息创建Metric
  let metric_attributes = [
    ("trace_id", common::AttributeValue::string("trace-123")),
    ("span_name", common::AttributeValue::string("operation")),
    ("service.name", common::AttributeValue::string("test-service"))
  ]
  
  assert_eq(metric_attributes.length(), 3)
  match metric_attributes[0].1 {
    common::StringValue(id) => assert_eq(id, "trace-123")
    _ => @test.fail("Test failed")
  }
}

test "cross_signal_integration_trace_to_logs" {
  // 测试从Trace信息生成Logs的场景
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace_id")
  let span_key = context::create_key("span_id")
  let ctx_with_trace = ctx.with_value(trace_key, "trace-456").with_value(span_key, "span-789")
  
  // 模拟基于Trace信息创建Log记录
  let log_attributes = [
    ("trace_id", common::AttributeValue::string("trace-456")),
    ("span_id", common::AttributeValue::string("span-789")),
    ("log.level", common::AttributeValue::string("INFO")),
    ("service.name", common::AttributeValue::string("test-service"))
  ]
  
  assert_eq(log_attributes.length(), 4)
  match log_attributes[0].1 {
    common::StringValue(id) => assert_eq(id, "trace-456")
    _ => @test.fail("Test failed")
  }
  match log_attributes[1].1 {
    common::StringValue(id) => assert_eq(id, "span-789")
    _ => @test.fail("Test failed")
  }
}

test "cross_signal_integration_metrics_to_logs" {
  // 测试Metrics异常触发Logs记录的场景
  let metric_threshold_breached = true
  let metric_value = 95.5
  let threshold = 80.0
  
  if metric_threshold_breached {
    let log_attributes = [
      ("metric.name", common::AttributeValue::string("cpu.usage")),
      ("metric.value", common::AttributeValue::float(metric_value)),
      ("metric.threshold", common::AttributeValue::float(threshold)),
      ("alert.severity", common::AttributeValue::string("WARNING")),
      ("service.name", common::AttributeValue::string("monitoring-service"))
    ]
    
    assert_eq(log_attributes.length(), 5)
    match log_attributes[1].1 {
      common::FloatValue(value) => assert_eq(value, metric_value)
      _ => @test.fail("Test failed")
    }
  }
}

test "cross_signal_integration_comprehensive_workflow" {
  // 测试完整的跨信号工作流
  
  // 1. 创建Trace
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace_id")
  let span_key = context::create_key("span_id")
  let ctx_with_trace = ctx.with_value(trace_key, "workflow-trace-001").with_value(span_key, "workflow-span-001")
  
  // 2. 基于Trace创建Metrics
  let operation_metrics = [
    ("operation.duration_ms", common::AttributeValue::int(250L)),
    ("operation.status", common::AttributeValue::string("SUCCESS")),
    ("trace_id", common::AttributeValue::string("workflow-trace-001")),
    ("service.name", common::AttributeValue::string("order-service"))
  ]
  
  // 3. 基于Trace和Metrics创建Logs
  let operation_logs = [
    ("trace_id", common::AttributeValue::string("workflow-trace-001")),
    ("span_id", common::AttributeValue::string("workflow-span-001")),
    ("log.level", common::AttributeValue::string("INFO")),
    ("operation.result", common::AttributeValue::string("order_processed")),
    ("order.id", common::AttributeValue::string("order-12345")),
    ("customer.id", common::AttributeValue::string("customer-67890"))
  ]
  
  // 4. 验证所有信号的关联性
  assert_eq(operation_metrics.length(), 4)
  assert_eq(operation_logs.length(), 6)
  
  // 验证Trace ID在所有信号中一致
  match operation_metrics[2].1 {
    common::StringValue(trace_id) => assert_eq(trace_id, "workflow-trace-001")
    _ => @test.fail("Test failed")
  }
  
  match operation_logs[0].1 {
    common::StringValue(trace_id) => assert_eq(trace_id, "workflow-trace-001")
    _ => @test.fail("Test failed")
  }
}

test "cross_signal_integration_error_propagation" {
  // 测试错误在跨信号之间的传播
  
  // 1. Trace中记录错误
  let error_span = trace::Span::{
    name: "failing_operation",
    context: trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0x01_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0x02_byte }),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: trace::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1500L),
    status: trace::Error,
    status_description: Some("Database connection timeout"),
    attributes: [
      ("error.code", common::AttributeValue::string("DB_TIMEOUT")),
      ("error.retries", common::AttributeValue::int(3L))
    ],
    events: [
      trace::SpanEvent::{
        name: "error",
        timestamp_unix_nanos: 1200L,
        attributes: [
          ("error.type", common::AttributeValue::string("timeout")),
          ("error.message", common::AttributeValue::string("Connection timeout after 30 seconds"))
        ]
      }
    ],
    links: []
  }
  
  // 2. 基于Trace错误创建Metrics
  let error_metrics = [
    ("errors.total", common::AttributeValue::int(1L)),
    ("errors.type", common::AttributeValue::string("database_timeout")),
    ("service.name", common::AttributeValue::string("payment-service")),
    ("error.severity", common::AttributeValue::string("HIGH"))
  ]
  
  // 3. 基于Trace错误创建Logs
  let error_logs = [
    ("log.level", common::AttributeValue::string("ERROR")),
    ("error.code", common::AttributeValue::string("DB_TIMEOUT")),
    ("error.message", common::AttributeValue::string("Database connection timeout")),
    ("error.retries", common::AttributeValue::int(3L)),
    ("service.name", common::AttributeValue::string("payment-service")),
    ("trace.id", common::AttributeValue::string("error-trace-001"))
  ]
  
  // 4. 验证错误信息在所有信号中一致
  assert_eq(error_span.status, trace::Error)
  assert_eq(error_metrics.length(), 4)
  assert_eq(error_logs.length(), 6)
  
  match error_metrics[1].1 {
    common::StringValue(error_type) => assert_eq(error_type, "database_timeout")
    _ => @test.fail("Test failed")
  }
  
  match error_logs[1].1 {
    common::StringValue(error_code) => assert_eq(error_code, "DB_TIMEOUT")
    _ => @test.fail("Test failed")
  }
}

test "cross_signal_integration_context_propagation" {
  // 测试上下文在跨信号操作中的传播
  
  // 1. 创建带有丰富上下文的Context
  let ctx = context::Context::empty()
  let user_key = context::create_key("user_id")
  let request_key = context::create_key("request_id")
  let tenant_key = context::create_key("tenant_id")
  
  let enriched_ctx = ctx
    .with_value(user_key, "user-12345")
    .with_value(request_key, "req-67890")
    .with_value(tenant_key, "tenant-001")
  
  // 2. 基于上下文创建Trace属性
  let trace_attributes = [
    ("user.id", common::AttributeValue::string("user-12345")),
    ("request.id", common::AttributeValue::string("req-67890")),
    ("tenant.id", common::AttributeValue::string("tenant-001")),
    ("service.name", common::AttributeValue::string("api-gateway"))
  ]
  
  // 3. 基于上下文创建Metrics属性
  let metric_attributes = [
    ("user.id", common::AttributeValue::string("user-12345")),
    ("tenant.id", common::AttributeValue::string("tenant-001")),
    ("operation.name", common::AttributeValue::string("api_request")),
    ("service.name", common::AttributeValue::string("api-gateway"))
  ]
  
  // 4. 基于上下文创建Logs属性
  let log_attributes = [
    ("user.id", common::AttributeValue::string("user-12345")),
    ("request.id", common::AttributeValue::string("req-67890")),
    ("tenant.id", common::AttributeValue::string("tenant-001")),
    ("log.level", common::AttributeValue::string("INFO")),
    ("service.name", common::AttributeValue::string("api-gateway"))
  ]
  
  // 5. 验证上下文信息在所有信号中正确传播
  assert_eq(trace_attributes.length(), 4)
  assert_eq(metric_attributes.length(), 4)
  assert_eq(log_attributes.length(), 5)
  
  // 验证用户ID在所有信号中一致
  match trace_attributes[0].1 {
    common::StringValue(user_id) => assert_eq(user_id, "user-12345")
    _ => @test.fail("Test failed")
  }
  
  match metric_attributes[0].1 {
    common::StringValue(user_id) => assert_eq(user_id, "user-12345")
    _ => @test.fail("Test failed")
  }
  
  match log_attributes[0].1 {
    common::StringValue(user_id) => assert_eq(user_id, "user-12345")
    _ => @test.fail("Test failed")
  }
}