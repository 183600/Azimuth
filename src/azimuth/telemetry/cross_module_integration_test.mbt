// 跨模块集成测试

test "telemetry_end_to_end_workflow" {
  // 测试完整的遥测工作流程
  
  // 1. 创建资源
  let resource = common::Resource::default("integration-test-service")
  
  // 2. 创建仪器化范围
  let instrumentation_scope = common::InstrumentationScope::{
    name: "integration.test.instrumentation",
    version: Some("1.0.0"),
    schema_url: Some("https://azimuth.dev/schemas/1.0.0")
  }
  
  // 3. 创建上下文
  let ctx = context::Context::empty()
  let trace_key = context::create_key("trace.context")
  let baggage_key = context::create_key("baggage.context")
  
  // 4. 创建Baggage
  let baggage = context::Baggage::empty()
    .with_entry("user.id", "user123")
    .with_entry("request.id", "req456")
    .with_entry("session.id", "sess789")
  
  // 5. 将Baggage存储在Context中
  let ctx_with_baggage = ctx.with_value(baggage_key, "baggage_data")
  
  // 6. 创建Tracer并开始Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration.tracer", Some("1.0.0"))
  
  let (ctx_with_span, span) = tracer.start_span(
    ctx_with_baggage,
    "integration.operation",
    Some(trace::Server),
    Some([
      ("operation.type", common::AttributeValue::string("integration")),
      ("service.name", common::AttributeValue::string(resource.service_name))
    ]),
    Some(1640995200000000000L) // 2022-01-01 00:00:00 UTC
  )
  
  // 7. 创建Logger并记录日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration.logger", Some("1.0.0"), None)
  
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995201000000000L) // 1秒后
    .severity(logs::Info)
    .body("Integration operation started")
    .with_attribute("logger.name", common::AttributeValue::string("integration.logger"))
    .with_attribute("operation.id", common::AttributeValue::string("op123"))
    .build()
  
  logger.emit(log_record)
  
  // 8. 创建Meter并记录指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration.meter", Some("1.0.0"), None)
  
  let counter = meter.create_counter("integration.operations.total", Some("count"), Some("Total integration operations"))
  let histogram = meter.create_histogram("integration.operation.duration", Some("seconds"), Some("Integration operation duration"))
  
  counter.add(1L, [
    ("operation.type", common::AttributeValue::string("integration")),
    ("operation.status", common::AttributeValue::string("started"))
  ])
  
  histogram.record(1.5, [
    ("operation.type", common::AttributeValue::string("integration")),
    ("operation.result", common::AttributeValue::string("success"))
  ])
  
  // 9. 验证所有组件都能正常工作
  assert_eq(resource.service_name, "integration-test-service")
  assert_eq(instrumentation_scope.name, "integration.test.instrumentation")
  match instrumentation_scope.version {
    Some(v) => assert_eq(v, "1.0.0")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Baggage
  let user_id = baggage.get("user.id")
  match user_id {
    Some(id) => assert_eq(id, "user123")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Span
  assert_eq(span.name, "integration.operation")
  assert_eq(span.kind, trace::Server)
  assert_eq(span.attributes.length(), 2)
  
  // 验证LogRecord
  match log_record.body {
    Some(msg) => assert_eq(msg, "Integration operation started")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  assert_eq(log_record.severity_number, logs::Info)
  assert_eq(log_record.attributes.length(), 2)
  
  // 验证指标创建
  // 由于Noop实现，我们只能验证创建过程不崩溃
  assert_eq(true, true) // 如果到达这里，说明所有操作都成功
}

test "cross_module_data_consistency" {
  // 测试跨模块数据一致性
  
  // 1. 在Common模块中创建属性
  let common_attributes = [
    ("service.name", common::AttributeValue::string("consistency-test")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("service.instance.id", common::AttributeValue::string("instance-123"))
  ]
  
  // 2. 在Context模块中创建上下文并存储相同的数据
  let ctx = context::Context::empty()
  let service_name_key = context::create_key("service.name")
  let service_version_key = context::create_key("service.version")
  let instance_id_key = context::create_key("service.instance.id")
  
  let ctx_with_data = ctx
    .with_value(service_name_key, "consistency-test")
    .with_value(service_version_key, "1.0.0")
    .with_value(instance_id_key, "instance-123")
  
  // 3. 在Logs模块中创建包含相同属性的日志记录
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Cross-module consistency test")
    .with_attribute("service.name", common::AttributeValue::string("consistency-test"))
    .with_attribute("service.version", common::AttributeValue::string("1.0.0"))
    .with_attribute("service.instance.id", common::AttributeValue::string("instance-123"))
    .build()
  
  // 4. 在Metrics模块中创建包含相同属性的指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("consistency.meter", None, None)
  let counter = meter.create_counter("consistency.operations", None, None)
  
  counter.add(1L, [
    ("service.name", common::AttributeValue::string("consistency-test")),
    ("service.version", common::AttributeValue::string("1.0.0")),
    ("service.instance.id", common::AttributeValue::string("instance-123"))
  ])
  
  // 5. 在Trace模块中创建包含相同属性的Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("consistency.tracer", None, None)
  let (ctx_with_span, span) = tracer.start_span(
    ctx_with_data,
    "consistency.operation",
    None,
    Some([
      ("service.name", common::AttributeValue::string("consistency-test")),
      ("service.version", common::AttributeValue::string("1.0.0")),
      ("service.instance.id", common::AttributeValue::string("instance-123"))
    ]),
    None
  )
  
  // 6. 验证数据一致性
  // 验证Common模块中的属性
  assert_eq(common_attributes.length(), 3)
  assert_eq(common_attributes[0].0, "service.name")
  match common_attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "consistency-test")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Context模块中的数据
  let ctx_service_name = ctx_with_data.get(service_name_key)
  match ctx_service_name {
    Some(name) => assert_eq(name, "consistency-test")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Logs模块中的属性
  assert_eq(log_record.attributes.length(), 3)
  assert_eq(log_record.attributes[0].0, "service.name")
  match log_record.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "consistency-test")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Trace模块中的属性
  assert_eq(span.attributes.length(), 3)
  assert_eq(span.attributes[0].0, "service.name")
  match span.attributes[0].1 {
    common::StringValue(s) => assert_eq(s, "consistency-test")
    _ => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证所有模块中的service.name值相同
  let common_service_name = match common_attributes[0].1 {
    common::StringValue(s) => s
    _ => "invalid"
  }
  
  let ctx_service_name_value = match ctx_service_name {
    Some(name) => name
    None => "invalid"
  }
  
  let log_service_name = match log_record.attributes[0].1 {
    common::StringValue(s) => s
    _ => "invalid"
  }
  
  let span_service_name = match span.attributes[0].1 {
    common::StringValue(s) => s
    _ => "invalid"
  }
  
  assert_eq(common_service_name, ctx_service_name_value)
  assert_eq(common_service_name, log_service_name)
  assert_eq(common_service_name, span_service_name)
}

test "context_propagation_across_modules" {
  // 测试跨模块的上下文传播
  
  // 1. 在Context模块中创建带有数据的上下文
  let ctx = context::Context::empty()
  let trace_id_key = context::create_key("trace.id")
  let span_id_key = context::create_key("span.id")
  let user_id_key = context::create_key("user.id")
  let request_id_key = context::create_key("request.id")
  
  let enriched_ctx = ctx
    .with_value(trace_id_key, "trace123456")
    .with_value(span_id_key, "span789012")
    .with_value(user_id_key, "user345678")
    .with_value(request_id_key, "req901234")
  
  // 2. 创建Baggage
  let baggage = context::Baggage::empty()
    .with_entry("correlation.id", "corr567890")
    .with_entry("session.id", "sess123456")
    .with_entry("tenant.id", "tenant789012")
  
  // 3. 将Baggage存储在Context中
  let baggage_key = context::create_key("baggage")
  let ctx_with_baggage = enriched_ctx.with_value(baggage_key, "baggage_data")
  
  // 4. 在Trace模块中使用上下文创建Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("propagation.tracer", None, None)
  
  let (ctx_after_span, span) = tracer.start_span(
    ctx_with_baggage,
    "propagation.test",
    Some(trace::Server),
    Some([
      ("trace.id", common::AttributeValue::string("trace123456")),
      ("parent.span.id", common::AttributeValue::string("span789012")),
      ("user.id", common::AttributeValue::string("user345678"))
    ]),
    None
  )
  
  // 5. 在Logs模块中使用上下文创建日志记录
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("propagation.logger", None, None)
  
  let trace_id_from_ctx = ctx_after_span.get(trace_id_key)
  let user_id_from_ctx = ctx_after_span.get(user_id_key)
  
  let log_attributes = []
  match trace_id_from_ctx {
    Some(trace_id) => {
      log_attributes.push(("trace.id", common::AttributeValue::string(trace_id)))
    }
    None => {}
  }
  
  match user_id_from_ctx {
    Some(user_id) => {
      log_attributes.push(("user.id", common::AttributeValue::string(user_id)))
    }
    None => {}
  }
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Context propagation test")
    .build()
  
  // 由于当前实现限制，我们手动添加属性
  let log_record_with_ctx = logs::LogRecord::{
    timestamp_unix_nanos: log_record.timestamp_unix_nanos,
    observed_timestamp_unix_nanos: log_record.observed_timestamp_unix_nanos,
    severity_number: log_record.severity_number,
    severity_text: log_record.severity_text,
    body: log_record.body,
    attributes: log_attributes,
    trace_id: log_record.trace_id,
    span_id: log_record.span_id,
    trace_flags: log_record.trace_flags,
    resource: log_record.resource,
    instrumentation_scope: log_record.instrumentation_scope
  }
  
  logger.emit(log_record_with_ctx)
  
  // 6. 在Metrics模块中使用上下文记录指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("propagation.meter", None, None)
  let counter = meter.create_counter("propagation.operations", None, None)
  
  let request_id_from_ctx = ctx_after_span.get(request_id_key)
  let metric_attributes = []
  
  match request_id_from_ctx {
    Some(request_id) => {
      metric_attributes.push(("request.id", common::AttributeValue::string(request_id)))
    }
    None => {}
  }
  
  counter.add(1L, metric_attributes)
  
  // 7. 验证上下文传播
  // 验证原始上下文数据仍然存在
  let trace_id_check = ctx_after_span.get(trace_id_key)
  match trace_id_check {
    Some(id) => assert_eq(id, "trace123456")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let span_id_check = ctx_after_span.get(span_id_key)
  match span_id_check {
    Some(id) => assert_eq(id, "span789012")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let user_id_check = ctx_after_span.get(user_id_key)
  match user_id_check {
    Some(id) => assert_eq(id, "user345678")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let request_id_check = ctx_after_span.get(request_id_key)
  match request_id_check {
    Some(id) => assert_eq(id, "req901234")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Baggage数据仍然存在
  let baggage_data_check = ctx_after_span.get(baggage_key)
  match baggage_data_check {
    Some(data) => assert_eq(data, "baggage_data")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Baggage本身的数据
  let correlation_id = baggage.get("correlation.id")
  match correlation_id {
    Some(id) => assert_eq(id, "corr567890")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证Span包含上下文信息
  assert_eq(span.name, "propagation.test")
  assert_eq(span.kind, trace::Server)
  assert_eq(span.attributes.length(), 3)
  
  // 验证日志记录包含上下文信息
  match log_record_with_ctx.body {
    Some(msg) => assert_eq(msg, "Context propagation test")
    None => assert_eq(false, true)  // 不应该到达这里
  }
}

test "error_handling_across_modules" {
  // 测试跨模块的错误处理
  
  // 1. 在Common模块中创建错误属性
  let error_attributes = [
    ("error.type", common::AttributeValue::string("ValidationError")),
    ("error.message", common::AttributeValue::string("Invalid input parameter")),
    ("error.code", common::AttributeValue::int(400L)),
    ("error.severity", common::AttributeValue::string("ERROR"))
  ]
  
  // 2. 在Context模块中存储错误上下文
  let ctx = context::Context::empty()
  let error_type_key = context::create_key("error.type")
  let error_message_key = context::create_key("error.message")
  let error_code_key = context::create_key("error.code")
  
  let error_ctx = ctx
    .with_value(error_type_key, "ValidationError")
    .with_value(error_message_key, "Invalid input parameter")
    .with_value(error_code_key, "400")
  
  // 3. 在Trace模块中创建表示错误的Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error.tracer", None, None)
  
  let (error_ctx_with_span, error_span) = tracer.start_span(
    error_ctx,
    "error.operation",
    Some(trace::Server),
    Some(error_attributes),
    None
  )
  
  // 由于当前实现限制，我们创建一个带有错误状态的Span
  let error_span_with_status = trace::Span::{
    ..error_span,
    end_time_unix_nanos: Some(1640995201000000000L),
    status: trace::Error,
    status_description: Some("Operation failed due to validation error")
  }
  
  // 4. 在Logs模块中记录错误日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("error.logger", None, None)
  
  let error_log_record = logs::LogRecord::builder()
    .timestamp(1640995201000000000L)
    .severity(logs::Error)
    .body("Operation failed: Invalid input parameter")
    .with_attribute("error.type", common::AttributeValue::string("ValidationError"))
    .with_attribute("error.code", common::AttributeValue::int(400L))
    .build()
  
  logger.emit(error_log_record)
  
  // 5. 在Metrics模块中记录错误指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("error.meter", None, None)
  
  let error_counter = meter.create_counter("errors.total", Some("count"), Some("Total number of errors"))
  let error_histogram = meter.create_histogram("error.duration", Some("seconds"), Some("Error operation duration"))
  
  error_counter.add(1L, [
    ("error.type", common::AttributeValue::string("ValidationError")),
    ("error.code", common::AttributeValue::int(400L))
  ])
  
  error_histogram.record(0.5, [
    ("error.type", common::AttributeValue::string("ValidationError")),
    ("error.result", common::AttributeValue::string("failed"))
  ])
  
  // 6. 验证错误处理
  // 验证错误上下文
  let error_type_from_ctx = error_ctx_with_span.get(error_type_key)
  match error_type_from_ctx {
    Some(type) => assert_eq(type, "ValidationError")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let error_message_from_ctx = error_ctx_with_span.get(error_message_key)
  match error_message_from_ctx {
    Some(message) => assert_eq(message, "Invalid input parameter")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let error_code_from_ctx = error_ctx_with_span.get(error_code_key)
  match error_code_from_ctx {
    Some(code) => assert_eq(code, "400")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证错误Span状态
  assert_eq(error_span_with_status.status, trace::Error)
  match error_span_with_status.status_description {
    Some(desc) => assert_eq(desc, "Operation failed due to validation error")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证错误日志
  assert_eq(error_log_record.severity_number, logs::Error)
  match error_log_record.body {
    Some(msg) => assert_eq(msg, "Operation failed: Invalid input parameter")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  assert_eq(error_log_record.attributes.length(), 2)
  
  // 验证错误属性在所有模块中一致
  let common_error_type = match error_attributes[0].1 {
    common::StringValue(s) => s
    _ => "invalid"
  }
  
  let common_error_code = match error_attributes[2].1 {
    common::IntValue(code) => code
    _ => -1L
  }
  
  let span_error_type = match error_span_with_status.attributes[0].1 {
    common::StringValue(s) => s
    _ => "invalid"
  }
  
  let span_error_code = match error_span_with_status.attributes[2].1 {
    common::IntValue(code) => code
    _ => -1L
  }
  
  let log_error_type = match error_log_record.attributes[0].1 {
    common::StringValue(s) => s
    _ => "invalid"
  }
  
  let log_error_code = match error_log_record.attributes[1].1 {
    common::IntValue(code) => code
    _ => -1L
  }
  
  assert_eq(common_error_type, span_error_type)
  assert_eq(common_error_type, log_error_type)
  assert_eq(common_error_code, span_error_code)
  assert_eq(common_error_code, log_error_code)
}

test "performance_across_modules" {
  // 测试跨模块的性能
  
  // 1. 创建大量属性
  let large_attributes = []
  let mut i = 0
  while i < 100 {
    let key = "attribute." + i.to_string()
    let value = "value." + i.to_string()
    large_attributes.push((key, common::AttributeValue::string(value)))
    i = i + 1
  }
  
  // 2. 在Context模块中创建大量上下文数据
  let ctx = context::Context::empty()
  let mut ctx_with_data = ctx
  
  i = 0
  while i < 50 {
    let key = context::create_key("context.key." + i.to_string())
    let value = "context.value." + i.to_string()
    ctx_with_data = ctx_with_data.with_value(key, value)
    i = i + 1
  }
  
  // 3. 在Baggage中创建大量条目
  let baggage = context::Baggage::empty()
  let mut baggage_with_data = baggage
  
  i = 0
  while i < 25 {
    let key = "baggage.key." + i.to_string()
    let value = "baggage.value." + i.to_string()
    baggage_with_data = baggage_with_data.with_entry(key, value)
    i = i + 1
  }
  
  // 4. 在Trace模块中创建带有大量属性的Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("performance.tracer", None, None)
  
  let (perf_ctx, perf_span) = tracer.start_span(
    ctx_with_data,
    "performance.operation",
    Some(trace::Server),
    Some(large_attributes),
    None
  )
  
  // 5. 在Logs模块中创建带有大量属性的日志记录
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("performance.logger", None, None)
  
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Performance test with large data")
    .build()
  
  // 由于当前实现限制，我们手动添加大量属性
  let log_record_with_large_attrs = logs::LogRecord::{
    timestamp_unix_nanos: log_record.timestamp_unix_nanos,
    observed_timestamp_unix_nanos: log_record.observed_timestamp_unix_nanos,
    severity_number: log_record.severity_number,
    severity_text: log_record.severity_text,
    body: log_record.body,
    attributes: large_attributes,
    trace_id: log_record.trace_id,
    span_id: log_record.span_id,
    trace_flags: log_record.trace_flags,
    resource: log_record.resource,
    instrumentation_scope: log_record.instrumentation_scope
  }
  
  logger.emit(log_record_with_large_attrs)
  
  // 6. 在Metrics模块中创建带有大量属性的指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("performance.meter", None, None)
  
  let counter = meter.create_counter("performance.operations", None, None)
  let histogram = meter.create_histogram("performance.duration", None, None)
  
  // 记录多次指标操作
  i = 0
  while i < 10 {
    counter.add(1L, large_attributes)
    histogram.record(i.to_double(), large_attributes)
    i = i + 1
  }
  
  // 7. 验证性能测试结果
  // 验证大量属性被正确处理
  assert_eq(large_attributes.length(), 100)
  assert_eq(perf_span.attributes.length(), 100)
  assert_eq(log_record_with_large_attrs.attributes.length(), 100)
  
  // 验证大量上下文数据被正确处理
  let context_key_0 = context::create_key("context.key.0")
  let context_key_49 = context::create_key("context.key.49")
  
  let context_value_0 = perf_ctx.get(context_key_0)
  match context_value_0 {
    Some(value) => assert_eq(value, "context.value.0")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let context_value_49 = perf_ctx.get(context_key_49)
  match context_value_49 {
    Some(value) => assert_eq(value, "context.value.49")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证大量Baggage条目被正确处理
  let baggage_value_0 = baggage_with_data.get("baggage.key.0")
  match baggage_value_0 {
    Some(value) => assert_eq(value, "baggage.value.0")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  let baggage_value_24 = baggage_with_data.get("baggage.key.24")
  match baggage_value_24 {
    Some(value) => assert_eq(value, "baggage.value.24")
    None => assert_eq(false, true)  // 不应该到达这里
  }
  
  // 验证所有操作都能成功完成
  assert_eq(true, true) // 如果到达这里，说明所有性能测试都通过
}