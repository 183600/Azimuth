// 资源生命周期管理测试
// 测试遥测资源的创建、使用和销毁过程

test "resource_lifecycle_management" {
  // 测试Resource的完整生命周期
  
  // 1. 创建资源
  let resource = common::Resource::default("lifecycle-test-service")
  assert_eq(resource.service_name, "lifecycle-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 2. 测试资源的属性管理
  let initial_attrs = resource.attributes
  assert_eq(initial_attrs.length(), 0)
  
  // 3. 模拟资源使用过程
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("lifecycle-tracer")
  let meter = meter_provider.get_meter("lifecycle-meter")
  let logger = logger_provider.get_logger("lifecycle-logger")
  
  // 4. 在资源生命周期内创建和使用遥测组件
  let ctx = context::Context::empty()
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "lifecycle-test-span",
    trace::Internal,
    [("resource.phase", common::AttributeValue::string("active"))]
  )
  
  let counter = meter.create_counter("lifecycle_operations", "count", "Lifecycle operations")
  counter.add(1L, [("resource.phase", common::AttributeValue::string("active"))])
  
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Resource is active")
    .with_attribute("resource.phase", common::AttributeValue::string("active"))
    .build()
  
  // 5. 验证资源在活跃状态下的行为
  assert_eq(span.name, "lifecycle-test-span")
  assert_eq(log_record.body, Some("Resource is active"))
  
  // 6. 模拟资源清理阶段
  let (cleanup_ctx, cleanup_span) = tracer.start_span(
    ctx,
    "resource-cleanup",
    trace::Internal,
    [("resource.phase", common::AttributeValue::string("cleanup"))]
  )
  
  let cleanup_log = logs::LogRecord::builder()
    .timestamp(1640995200001000000L)
    .severity(logs::Info)
    .body("Resource cleanup started")
    .with_attribute("resource.phase", common::AttributeValue::string("cleanup"))
    .build()
  
  // 7. 验证清理阶段的行为
  assert_eq(cleanup_span.name, "resource-cleanup")
  assert_eq(cleanup_log.body, Some("Resource cleanup started"))
}

test "context_lifecycle_with_nested_operations" {
  // 测试Context在嵌套操作中的生命周期管理
  
  let root_ctx = context::Context::empty()
  
  // 1. 创建根级Context
  let operation_key = context::create_key("operation.id")
  let user_key = context::create_key("user.id")
  
  let root_ctx_with_values = root_ctx
    .with_value(operation_key, "root-operation-123")
    .with_value(user_key, "user-456")
  
  // 2. 创建嵌套Context
  let sub_operation_key = context::create_key("sub.operation.id")
  let nested_ctx = root_ctx_with_values
    .with_value(sub_operation_key, "sub-operation-789")
  
  // 3. 验证Context层级关系
  match root_ctx_with_values.get(operation_key) {
    Some(value) => assert_eq(value, "root-operation-123")
    None => @test.fail("Expected root operation ID")
  }
  
  match nested_ctx.get(operation_key) {
    Some(value) => assert_eq(value, "root-operation-123")  // 应该继承父Context的值
    None => @test.fail("Expected inherited root operation ID")
  }
  
  match nested_ctx.get(sub_operation_key) {
    Some(value) => assert_eq(value, "sub-operation-789")
    None => @test.fail("Expected sub operation ID")
  }
  
  // 4. 验证根Context不受嵌套Context影响
  match root_ctx_with_values.get(sub_operation_key) {
    Some(_) => @test.fail("Root context should not have sub operation ID")
    None => assert_eq(true, true)  // 预期行为
  }
  
  // 5. 测试深层嵌套Context
  let deep_nested_key = context::create_key("deep.operation.id")
  let deep_nested_ctx = nested_ctx
    .with_value(deep_nested_key, "deep-operation-999")
  
  // 验证所有层级的值都正确传播
  match deep_nested_ctx.get(operation_key) {
    Some(value) => assert_eq(value, "root-operation-123")
    None => @test.fail("Expected inherited root operation ID in deep context")
  }
  
  match deep_nested_ctx.get(sub_operation_key) {
    Some(value) => assert_eq(value, "sub-operation-789")
    None => @test.fail("Expected inherited sub operation ID in deep context")
  }
  
  match deep_nested_ctx.get(deep_nested_key) {
    Some(value) => assert_eq(value, "deep-operation-999")
    None => @test.fail("Expected deep operation ID")
  }
}

test "span_lifecycle_with_proper_termination" {
  // 测试Span的完整生命周期，包括创建、使用和正确的终止
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("lifecycle-test-tracer")
  let ctx = context::Context::empty()
  
  // 1. 创建Span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "lifecycle-test-span",
    trace::Server,
    [
      ("service.name", common::AttributeValue::string("lifecycle-service")),
      ("operation.type", common::AttributeValue::string("test"))
    ],
    Some(1640995200000000000L)  // 开始时间
  )
  
  // 2. 验证Span初始状态
  assert_eq(span.name, "lifecycle-test-span")
  match span.kind {
    trace::Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  match span.end_time_unix_nanos {
    None => assert_eq(true, true)  // 初始状态下结束时间应该为None
    Some(_) => @test.fail("Expected None for end_time_unix_nanos initially")
  }
  
  // 3. 在Span生命周期内添加事件
  let event = trace::SpanEvent::{
    name: "operation.completed",
    timestamp_unix_nanos: 1640995200000500000L,
    attributes: [
      ("operation.result", common::AttributeValue::string("success")),
      ("operation.duration_ms", common::AttributeValue::float(50.0))
    ]
  }
  
  // 4. 在Span生命周期内创建子Span
  let (child_ctx, child_span) = tracer.start_span(
    span_ctx,
    "child-operation",
    trace::Internal,
    [
      ("parent.span", common::AttributeValue::string("lifecycle-test-span")),
      ("child.operation.type", common::AttributeValue::string("validation"))
    ],
    Some(1640995200000100000L)
  )
  
  // 5. 验证子Span属性
  assert_eq(child_span.name, "child-operation")
  match child_span.kind {
    trace::Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal span kind")
  }
  
  // 6. 模拟Span结束（在实际实现中会设置end_time_unix_nanos）
  // 这里我们验证Span的结构完整性
  assert_eq(span.attributes.length(), 2)
  assert_eq(child_span.attributes.length(), 2)
  
  // 7. 验证Span生命周期结束后的状态
  match span.status {
    trace::Unset => assert_eq(true, true)  // 初始状态
    _ => @test.fail("Expected Unset status initially")
  }
}

test "baggage_lifecycle_across_operations" {
  // 测试Baggage在跨操作中的生命周期管理
  
  // 1. 创建初始Baggage
  let initial_baggage = context::Baggage::empty()
  
  // 2. 在操作过程中添加Baggage条目
  let baggage_with_user = initial_baggage
    .with_entry("user.id", "user-123")
    .with_entry("user.role", "admin")
  
  // 3. 在子操作中添加更多Baggage条目
  let baggage_with_session = baggage_with_user
    .with_entry("session.id", "session-456")
    .with_entry("session.timeout", "3600")
  
  // 4. 验证Baggage条目的累积
  match baggage_with_session.get("user.id") {
    Some(value) => assert_eq(value, "user-123")
    None => @test.fail("Expected user.id in baggage")
  }
  
  match baggage_with_session.get("user.role") {
    Some(value) => assert_eq(value, "admin")
    None => @test.fail("Expected user.role in baggage")
  }
  
  match baggage_with_session.get("session.id") {
    Some(value) => assert_eq(value, "session-456")
    None => @test.fail("Expected session.id in baggage")
  }
  
  match baggage_with_session.get("session.timeout") {
    Some(value) => assert_eq(value, "3600")
    None => @test.fail("Expected session.timeout in baggage")
  }
  
  // 5. 验证原始Baggage不受影响
  match baggage_with_user.get("session.id") {
    Some(_) => @test.fail("Original baggage should not have session.id")
    None => assert_eq(true, true)
  }
  
  // 6. 测试Baggage条目覆盖
  let baggage_with_updated_user = baggage_with_session
    .with_entry("user.id", "user-789")  // 覆盖现有的user.id
  
  match baggage_with_updated_user.get("user.id") {
    Some(value) => assert_eq(value, "user-789")
    None => @test.fail("Expected updated user.id")
  }
  
  // 7. 验证其他条目不受影响
  match baggage_with_updated_user.get("user.role") {
    Some(value) => assert_eq(value, "admin")
    None => @test.fail("Expected user.role to remain unchanged")
  }
}

test "resource_cleanup_with_error_handling" {
  // 测试资源清理过程中的错误处理
  
  let resource = common::Resource::default("cleanup-test-service")
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("cleanup-test-tracer")
  let ctx = context::Context::empty()
  
  // 1. 模拟正常操作
  let (normal_ctx, normal_span) = tracer.start_span(
    ctx,
    "normal-operation",
    trace::Internal,
    [("operation.phase", common::AttributeValue::string("normal"))]
  )
  
  // 2. 模拟错误情况
  let (error_ctx, error_span) = tracer.start_span(
    ctx,
    "error-operation",
    trace::Internal,
    [
      ("operation.phase", common::AttributeValue::string("error")),
      ("error.type", common::AttributeValue::string("ResourceExhausted"))
    ]
  )
  
  // 3. 模拟清理操作
  let (cleanup_ctx, cleanup_span) = tracer.start_span(
    error_ctx,  // 从错误上下文开始清理
    "cleanup-operation",
    trace::Internal,
    [
      ("operation.phase", common::AttributeValue::string("cleanup")),
      ("cleanup.triggered_by", common::AttributeValue::string("error"))
    ]
  )
  
  // 4. 验证清理操作可以访问错误上下文
  assert_eq(cleanup_span.name, "cleanup-operation")
  assert_eq(cleanup_span.attributes.length(), 2)
  
  // 5. 创建错误日志
  let error_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Error)
    .body("Operation failed, starting cleanup")
    .with_attribute("operation.phase", common::AttributeValue::string("error"))
    .with_attribute("error.type", common::AttributeValue::string("ResourceExhausted"))
    .build()
  
  // 6. 创建清理日志
  let cleanup_log = logs::LogRecord::builder()
    .timestamp(1640995200001000000L)
    .severity(logs::Info)
    .body("Cleanup completed successfully")
    .with_attribute("operation.phase", common::AttributeValue::string("cleanup"))
    .with_attribute("cleanup.result", common::AttributeValue::string("success"))
    .build()
  
  // 7. 验证日志记录
  match error_log.severity_number {
    logs::Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity for error log")
  }
  
  match cleanup_log.severity_number {
    logs::Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity for cleanup log")
  }
  
  // 8. 验证资源状态一致性
  assert_eq(resource.service_name, "cleanup-test-service")
  assert_eq(normal_span.name, "normal-operation")
  assert_eq(error_span.name, "error-operation")
  assert_eq(cleanup_span.name, "cleanup-operation")
}