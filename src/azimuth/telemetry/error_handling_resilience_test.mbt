// 错误处理和恢复能力测试 - 测试错误情况下的恢复能力

test "context_error_resilience" {
  // 测试Context在错误情况下的恢复能力
  let ctx = context::Context::empty()
  let key = context::create_key("test-key")
  
  // 测试使用null或undefined值（在MoonBit中等价于None或其他边界情况）
  let ctx_with_none = ctx.with_value(key, "")
  let retrieved_none = ctx_with_none.get(key)
  assert_eq(retrieved_none, Some(""))
  
  // 测试连续操作后的状态一致性
  let mut current_ctx = ctx
  let mut i = 0
  while i < 100 {
    current_ctx = current_ctx.with_value(
      context::create_key("key-${i}"), 
      "value-${i}"
    )
    i = i + 1
  }
  
  // 验证状态保持一致
  let test_retrieval = current_ctx.get(context::create_key("key-99"))
  assert_eq(test_retrieval, Some("value-99"))
  
  // 测试不存在的键的检索
  let non_existent_key = context::create_key("non-existent-key-12345")
  let non_existent_value = current_ctx.get(non_existent_key)
  assert_eq(non_existent_value, None)
}

test "span_error_handling" {
  // 测试Span创建和操作中的错误处理
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error-test")
  
  // 测试在异常情况下创建Span（如无效参数）
  let (_, error_span) = tracer.start_span(
    ctx, 
    "error-span",
    Some(trace::SpanKind::Internal),
    Some([("error.test", common::AttributeValue::bool(true))])
  )
  
  // 验证Span创建成功，即使在错误条件下
  assert_eq(error_span.name, "error-span")
  assert_eq(error_span.kind, trace::SpanKind::Internal)
  assert_eq(error_span.status, trace::StatusCode::Unset)
  
  // 测试Span状态变更
  // 在实际实现中，这里会有状态变更逻辑
  // 当前简化版本中，我们验证基本属性
  assert_eq(error_span.attributes.length(), 1)
  
  // 测试创建带有错误属性的Span
  let error_attrs = [
    ("error.type", common::AttributeValue::string("timeout")),
    ("error.message", common::AttributeValue::string("Operation timed out")),
    ("error.stack", common::AttributeValue::string("at function (line 123)")),
    ("error.code", common::AttributeValue::int(500))
  ]
  
  let (_, span_with_error) = tracer.start_span(
    ctx,
    "operation-with-error",
    Some(trace::SpanKind::Server),
    Some(error_attrs)
  )
  
  assert_eq(span_with_error.name, "operation-with-error")
  assert_eq(span_with_error.attributes.length(), 4)
}

test "log_error_handling" {
  // 测试日志记录中的错误处理
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("error-test")
  
  // 测试记录错误日志
  let error_log = logs::LogRecord::builder()
    .body("Error occurred during operation")
    .severity(logs::SeverityNumber::Error)
    .with_attribute("error.type", common::AttributeValue::string("validation_error"))
    .with_attribute("error.code", common::AttributeValue::int(400))
    .with_attribute("error.message", common::AttributeValue::string("Invalid input parameter"))
    .with_attribute("error.retryable", common::AttributeValue::bool(false))
    .build()
  
  // 验证错误日志创建成功
  assert_eq(error_log.body, Some("Error occurred during operation"))
  assert_eq(error_log.severity_number, logs::SeverityNumber::Error)
  assert_eq(error_log.attributes.length(), 4)
  
  // 测试致命错误日志
  let fatal_log = logs::LogRecord::builder()
    .body("Critical system failure")
    .severity(logs::SeverityNumber::Fatal)
    .with_attribute("system.component", common::AttributeValue::string("database"))
    .with_attribute("error.code", common::AttributeValue::int(500))
    .with_attribute("error.impact", common::AttributeValue::string("service_unavailable"))
    .build()
  
  assert_eq(fatal_log.severity_number, logs::SeverityNumber::Fatal)
  
  // 测试使用Logger的便捷方法记录错误
  logger.error("Database connection failed", Some([
    ("error.type", common::AttributeValue::string("connection_error")),
    ("error.retry_count", common::AttributeValue::int(3))
  ]))
  
  logger.fatal("System shutdown initiated", Some([
    ("shutdown.reason", common::AttributeValue::string("critical_error")),
    ("shutdown.graceful", common::AttributeValue::bool(true))
  ]))
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "metrics_error_handling" {
  // 测试指标记录中的错误处理
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("error-test")
  
  // 创建各种指标
  let error_counter = meter.create_counter("errors.total", Some("count"), Some("Total number of errors"))
  let error_histogram = meter.create_histogram("error.duration", Some("ms"), Some("Error processing duration"))
  let error_gauge = meter.create_gauge("error.rate", Some("errors/sec"), Some("Current error rate"))
  
  // 记录各种错误指标
  error_counter.add(1L, Some([
    ("error.type", common::AttributeValue::string("validation")),
    ("error.severity", common::AttributeValue::string("medium"))
  ]))
  
  error_counter.add(1L, Some([
    ("error.type", common::AttributeValue::string("timeout")),
    ("error.severity", common::AttributeValue::string("high"))
  ]))
  
  error_histogram.record(1500.0, Some([
    ("operation.type", common::AttributeValue::string("database_query")),
    ("error.type", common::AttributeValue::string("connection_timeout"))
  ]))
  
  error_gauge.record(0.05, Some([
    ("service.name", common::AttributeValue::string("api-service")),
    ("error.category", common::AttributeValue::string("network"))
  ]))
  
  // 测试极值情况
  error_counter.add(-1L, Some([("error.type", common::AttributeValue::string("correction"))]))
  error_histogram.record(Double::nan, Some([("error.type", common::AttributeValue::string("invalid"))]))
  error_histogram.record(Double::infinity, Some([("error.type", common::AttributeValue::string("overflow"))]))
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "propagation_error_handling" {
  // 测试传播过程中的错误处理
  let ctx = context::Context::empty()
  
  // 测试损坏的header数据
  let corrupted_data = [
    ("traceparent", "invalid-trace-data"),
    ("baggage", "malformed-baggage-string"),
    ("custom-header", "some-value")
  ]
  
  let corrupted_carrier = propagation::MapCarrier::from_map(corrupted_data)
  
  // 测试从损坏的数据中提取上下文
  let trace_propagator = trace::NoopTracerProvider::{}
  let w3c_propagator = propagation::W3CTraceContextPropagator::{}
  let extracted_ctx = w3c_propagator.extract(ctx, corrupted_carrier)
  
  // 验证即使数据损坏，操作也不会崩溃
  assert_eq(true, true)
  
  // 测试注入到无效carrier
  let invalid_carrier = propagation::MapCarrier::new()
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  baggage_propagator.inject(ctx, invalid_carrier)
  
  // 测试复合传播器的错误处理
  let propagators = [
    w3c_propagator as propagation::TextMapPropagator,
    baggage_propagator as propagation::TextMapPropagator
  ]
  let composite_propagator = propagation::CompositePropagator::new(propagators)
  
  // 测试在错误条件下的复合操作
  composite_propagator.inject(ctx, corrupted_carrier)
  let extracted_composite_ctx = composite_propagator.extract(ctx, corrupted_carrier)
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "resource_error_handling" {
  // 测试Resource创建和操作中的错误处理
  
  // 测试创建具有无效属性的Resource
  let resource_with_invalid_attrs = common::Resource::{
    service_name: "test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("invalid.key", common::AttributeValue::string("value")),
      ("", common::AttributeValue::string("empty-key")),
      ("valid.key", common::AttributeValue::string(""))
    ]
  }
  
  // 验证Resource创建成功，即使有无效属性
  assert_eq(resource_with_invalid_attrs.service_name, "test-service")
  assert_eq(resource_with_invalid_attrs.attributes.length(), 3)
  
  // 测试空服务名称的Resource
  let empty_service_resource = common::Resource::default("")
  assert_eq(empty_service_resource.service_name, "")
  
  // 测试超长服务名称的Resource
  let long_service_name = "a" * 10000
  let long_service_resource = common::Resource::default(long_service_name)
  assert_eq(long_service_resource.service_name, long_service_name)
}

test "instrumentation_scope_error_handling" {
  // 测试InstrumentationScope的错误处理
  
  // 测试创建具有无效参数的InstrumentationScope
  let invalid_scope = common::InstrumentationScope::{
    name: "",
    version: Some("invalid.version.format"),
    schema_url: Some("invalid-url")
  }
  
  // 验证创建成功
  assert_eq(invalid_scope.name, "")
  assert_eq(invalid_scope.version, Some("invalid.version.format"))
  assert_eq(invalid_scope.schema_url, Some("invalid-url"))
  
  // 测试超长名称的InstrumentationScope
  let long_scope_name = "a" * 10000
  let long_scope = common::InstrumentationScope::{
    name: long_scope_name,
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  assert_eq(long_scope.name, long_scope_name)
  
  // 测试包含特殊字符的InstrumentationScope
  let special_scope = common::InstrumentationScope::{
    name: "scope-with-special-chars-!@#$%^&*()",
    version: Some("1.0.0-beta+build.123"),
    schema_url: Some("https://example.com/path?query=value&other=123")
  }
  
  assert_eq(special_scope.name, "scope-with-special-chars-!@#$%^&*()")
}

test "cross_module_error_propagation" {
  // 测试跨模块错误传播
  
  // 在Context中设置错误信息
  let ctx = context::Context::empty()
  let error_key = context::create_key("error.context")
  let ctx_with_error = ctx.with_value(error_key, "database_connection_failed")
  
  // 在Trace中记录错误
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error-propagation-test")
  let (_, error_span) = tracer.start_span(
    ctx_with_error,
    "failing-operation",
    Some(trace::SpanKind::Client),
    Some([
      ("error.propagated", common::AttributeValue::bool(true)),
      ("error.source", common::AttributeValue::string("context"))
    ])
  )
  
  // 在Log中记录相关错误
  let error_log = logs::LogRecord::builder()
    .body("Operation failed with propagated error")
    .severity(logs::SeverityNumber::Error)
    .with_attribute("error.context", common::AttributeValue::string("database_connection_failed"))
    .with_attribute("error.span", common::AttributeValue::string("failing-operation"))
    .build()
  
  // 在Metrics中记录错误指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("error-propagation")
  let error_counter = meter.create_counter("errors.propagated", None, None)
  error_counter.add(1L, Some([
    ("error.context", common::AttributeValue::string("database_connection_failed")),
    ("error.span", common::AttributeValue::string("failing-operation"))
  ]))
  
  // 验证所有操作都能正常完成
  assert_eq(error_span.name, "failing-operation")
  assert_eq(error_log.body, Some("Operation failed with propagated error"))
  assert_eq(true, true)
}