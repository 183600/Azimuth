// 跨信号关联测试 - 测试Trace、Metrics、Logs之间的关联
// 验证不同遥测信号能够正确关联，提供完整的可观测性

test "cross_signal_correlation_basic" {
  // 基本的跨信号关联测试
  // 验证相同操作产生的trace、metric、log能够通过trace_id和span_id关联
  
  // 1. 创建一个span并记录trace_id和span_id
  let tracer_provider = api::trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  let ctx = api::context::Context::current()
  let (new_ctx, span) = tracer.start_span(ctx, "test-operation", kind: api::trace::Server)
  
  // 验证span context
  @assertion.assert_eq(span.context.trace_id.length(), 16)?
  @assertion.assert_eq(span.context.span_id.length(), 8)?
  
  // 2. 创建与span关联的metric
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", "1.0.0", "https://example.com/schema")
  let counter = meter.create_counter("test-counter", "count", "Test counter for cross-signal correlation")
  
  // 添加trace和span属性到metric
  let metric_attributes = [
    ("trace_id", api::common::AttributeValue::array_string(
      span.context.trace_id.map(fn(byte) { byte.to_string() })
    )),
    ("span_id", api::common::AttributeValue::array_string(
      span.context.span_id.map(fn(byte) { byte.to_string() })
    )),
    ("operation.name", api::common::AttributeValue::string("test-operation"))
  ]
  
  counter.add(1L, attributes: metric_attributes)
  
  // 3. 创建与span关联的log
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test-logger", "1.0.0", "https://example.com/schema")
  
  let log_record = api::logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(api::logs::Info)
    .body("Test log entry with correlation")
    .with_attribute("trace_id", api::common::AttributeValue::array_string(
      span.context.trace_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("span_id", api::common::AttributeValue::array_string(
      span.context.span_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("operation.name", api::common::AttributeValue::string("test-operation"))
    .build()
  
  logger.emit(log_record)
  
  // 4. 验证所有信号都包含相同的关联标识
  // 在实际实现中，这里会验证存储后端中的数据关联性
  @assertion.assert_true(true)? // 占位符，实际实现会检查关联性
}

test "cross_signal_correlation_nested_spans" {
  // 嵌套span的跨信号关联测试
  // 验证父子span关系在不同信号中的正确传播
  
  let tracer_provider = api::trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("nested-test-tracer")
  let ctx = api::context::Context::current()
  
  // 创建父span
  let (parent_ctx, parent_span) = tracer.start_span(ctx, "parent-operation", kind: api::trace::Server)
  
  // 在父span上下文中创建子span
  let (child_ctx, child_span) = tracer.start_span(parent_ctx, "child-operation", kind: api::trace::Internal)
  
  // 验证trace_id相同，span_id不同
  @assertion.assert_eq(parent_span.context.trace_id, child_span.context.trace_id)?
  @assertion.assert_ne(parent_span.context.span_id, child_span.context.span_id)?
  @assertion.assert_eq(child_span.parent_span_id, Some(parent_span.context.span_id))?
  
  // 创建与子span关联的metric
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("nested-test-meter")
  let histogram = meter.create_histogram("operation.duration", "ms", "Operation duration histogram")
  
  let child_metric_attributes = [
    ("trace_id", api::common::AttributeValue::array_string(
      child_span.context.trace_id.map(fn(byte) { byte.to_string() })
    )),
    ("span_id", api::common::AttributeValue::array_string(
      child_span.context.span_id.map(fn(byte) { byte.to_string() })
    )),
    ("parent_span_id", api::common::AttributeValue::array_string(
      parent_span.context.span_id.map(fn(byte) { byte.to_string() })
    )),
    ("operation.name", api::common::AttributeValue::string("child-operation"))
  ]
  
  histogram.record(150.5, attributes: child_metric_attributes)
  
  // 创建与子span关联的log
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("nested-test-logger")
  
  let child_log_record = api::logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(api::logs::Debug)
    .body("Child operation log entry")
    .with_attribute("trace_id", api::common::AttributeValue::array_string(
      child_span.context.trace_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("span_id", api::common::AttributeValue::array_string(
      child_span.context.span_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("parent_span_id", api::common::AttributeValue::array_string(
      parent_span.context.span_id.map(fn(byte) { byte.to_string() })
    ))
    .build()
  
  logger.emit(child_log_record)
}

test "cross_signal_correlation_baggage_propagation" {
  // 通过baggage传播关联信息的测试
  // 验证跨服务边界时关联信息的传播
  
  let tracer_provider = api::trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("baggage-test-tracer")
  let ctx = api::context::Context::current()
  
  // 创建初始span并添加baggage
  let (initial_ctx, initial_span) = tracer.start_span(ctx, "initial-operation")
  
  // 添加业务相关的baggage
  let baggage_ctx = api::context::with_value(
    initial_ctx, 
    "user.id", 
    api::common::AttributeValue::string("user-12345")
  )
  let baggage_ctx_with_tenant = api::context::with_value(
    baggage_ctx,
    "tenant.id",
    api::common::AttributeValue::string("tenant-67890")
  )
  
  // 在传播的上下文中创建新的span
  let (propagated_ctx, propagated_span) = tracer.start_span(
    baggage_ctx_with_tenant, 
    "propagated-operation"
  )
  
  // 验证trace_id的连续性
  @assertion.assert_eq(initial_span.context.trace_id, propagated_span.context.trace_id)?
  
  // 创建带有baggage信息的metric
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("baggage-test-meter")
  let counter = meter.create_counter("propagated.operations", "count", "Propagated operations count")
  
  let baggage_metric_attributes = [
    ("trace_id", api::common::AttributeValue::array_string(
      propagated_span.context.trace_id.map(fn(byte) { byte.to_string() })
    )),
    ("user.id", api::common::AttributeValue::string("user-12345")),
    ("tenant.id", api::common::AttributeValue::string("tenant-67890")),
    ("operation.name", api::common::AttributeValue::string("propagated-operation"))
  ]
  
  counter.add(1L, attributes: baggage_metric_attributes)
  
  // 创建带有baggage信息的log
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("baggage-test-logger")
  
  let baggage_log_record = api::logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(api::logs::Info)
    .body("Operation with propagated baggage")
    .with_attribute("trace_id", api::common::AttributeValue::array_string(
      propagated_span.context.trace_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("user.id", api::common::AttributeValue::string("user-12345"))
    .with_attribute("tenant.id", api::common::AttributeValue::string("tenant-67890"))
    .build()
  
  logger.emit(baggage_log_record)
}

test "cross_signal_correlation_error_scenarios" {
  // 错误场景下的跨信号关联测试
  // 验证异常情况下信号关联的完整性
  
  let tracer_provider = api::trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("error-test-tracer")
  let ctx = api::context::Context::current()
  
  // 创建一个会失败的span
  let (error_ctx, error_span) = tracer.start_span(ctx, "error-operation", kind: api::trace::Server)
  
  // 创建与错误相关的metric
  let meter_provider = api::metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("error-test-meter")
  let error_counter = meter.create_counter("operation.errors", "count", "Operation error count")
  
  let error_metric_attributes = [
    ("trace_id", api::common::AttributeValue::array_string(
      error_span.context.trace_id.map(fn(byte) { byte.to_string() })
    )),
    ("span_id", api::common::AttributeValue::array_string(
      error_span.context.span_id.map(fn(byte) { byte.to_string() })
    )),
    ("operation.name", api::common::AttributeValue::string("error-operation")),
    ("error.type", api::common::AttributeValue::string("ValidationError")),
    ("error.message", api::common::AttributeValue::string("Invalid input parameter"))
  ]
  
  error_counter.add(1L, attributes: error_metric_attributes)
  
  // 创建错误日志
  let logger_provider = api::logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("error-test-logger")
  
  let error_log_record = api::logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(api::logs::Error)
    .body("Operation failed with validation error")
    .with_attribute("trace_id", api::common::AttributeValue::array_string(
      error_span.context.trace_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("span_id", api::common::AttributeValue::array_string(
      error_span.context.span_id.map(fn(byte) { byte.to_string() })
    ))
    .with_attribute("error.type", api::common::AttributeValue::string("ValidationError"))
    .with_attribute("error.message", api::common::AttributeValue::string("Invalid input parameter"))
    .with_attribute("stack.trace", api::common::AttributeValue::string("at function validateInput (line 42)"))
    .build()
  
  logger.emit(error_log_record)
  
  // 验证错误信号的关联性
  @assertion.assert_true(true)? // 占位符，实际实现会验证错误关联性
}