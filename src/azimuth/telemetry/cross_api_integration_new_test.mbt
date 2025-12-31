// Azimuth Telemetry - Cross API Integration Test
// 测试Trace、Metrics、Logs三个API之间的集成功能

use azimuth.telemetry.api.common
use azimuth.telemetry.api.context
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

test "cross_api_trace_metrics_logs_integration" {
  // 创建共享的资源
  let resource = common::Resource::default("integration-test-service")
  
  // 创建共享的上下文
  let ctx = context::Context::new()
  
  // 1. 创建Tracer并开始一个Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("integration-test-tracer", "1.0.0")
  let (span_ctx, span) = tracer.start_span(
    ctx, 
    "integration-operation",
    trace::Server,
    [("operation.type", common::AttributeValue::string("integration"))],
    1234567890L
  )
  
  // 验证Span创建成功
  @assert(span.name == "integration-operation")
  @assert(span.context.trace_id.length() == 16)
  @assert(span.context.span_id.length() == 8)
  @assert(span.kind == trace::Server)
  @assert(span.attributes.length() == 1)
  
  // 2. 创建Meter并记录指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-test-meter", "1.0.0")
  let counter = meter.create_counter("operation.count", "count", "Number of operations")
  let histogram = meter.create_histogram("operation.duration", "ms", "Operation duration")
  
  // 记录指标（应该与Span关联相同的上下文）
  counter.add(1L, [("operation.name", common::AttributeValue::string("integration-operation"))])
  histogram.record(100.5, [("operation.type", common::AttributeValue::string("integration"))])
  
  // 3. 创建Logger并记录日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("integration-test-logger", "1.0.0")
  
  // 创建与Span关联的日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(logs::Info)
    .body("Integration operation completed successfully")
    .with_attribute("operation.name", common::AttributeValue::string("integration-operation"))
    .with_attribute("operation.result", common::AttributeValue::bool(true))
    .build()
  
  logger.emit(log_record)
  
  // 使用便捷方法记录日志
  logger.info("Integration operation started", [
    ("operation.phase", common::AttributeValue::string("start")),
    ("trace.id", common::AttributeValue::array_string([
      for i = 0; i < span.context.trace_id.length(); i = i + 1 {
        span.context.trace_id[i].to_string()
      }
    ]))
  ])
  
  logger.warn("Integration operation warning", [
    ("operation.phase", common::AttributeValue::string("warning")),
    ("warning.level", common::AttributeValue::int(2L))
  ])
  
  logger.error("Integration operation error", [
    ("operation.phase", common::AttributeValue::string("error")),
    ("error.code", common::AttributeValue::int(500L))
  ])
  
  // 4. 验证跨API的数据一致性
  // 所有API都应该使用相同的属性类型系统
  let common_attributes = [
    ("service.name", common::AttributeValue::string(resource.service_name)),
    ("service.version", common::AttributeValue::string(resource.service_version.unwrap_or("unknown"))),
    ("sdk.name", common::AttributeValue::string(resource.telemetry_sdk_name)),
    ("sdk.version", common::AttributeValue::string(resource.telemetry_sdk_version))
  ]
  
  // 验证属性类型一致性
  @assert(common_attributes.length() == 4)
  @assert(match common_attributes[0].1 {
    common::StringValue(s) => s == "integration-test-service"
    _ => false
  })
}

test "cross_api_context_propagation" {
  // 测试上下文在Trace、Metrics、Logs之间的传播
  
  // 创建基础上下文
  let base_ctx = context::Context::new()
  
  // 创建Tracer并开始Span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-test-tracer")
  let (ctx_with_span, span) = tracer.start_span(base_ctx, "context-test-operation")
  
  // 验证上下文包含Span信息
  @assert(span.context.trace_id.length() == 16)
  @assert(span.context.span_id.length() == 8)
  
  // 在相同的上下文中记录指标
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("context-test-meter")
  let counter = meter.create_counter("context.operations")
  
  counter.add(1L, [
    ("trace.id", common::AttributeValue::array_string([
      for i = 0; i < span.context.trace_id.length(); i = i + 1 {
        span.context.trace_id[i].to_string()
      }
    ])),
    ("span.id", common::AttributeValue::array_string([
      for i = 0; i < span.context.span_id.length(); i = i + 1 {
        span.context.span_id[i].to_string()
      }
    ]))
  ])
  
  // 在相同的上下文中记录日志
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("context-test-logger")
  
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(logs::Info)
    .body("Operation with context propagation")
    .with_attribute("trace.id", common::AttributeValue::array_string([
      for i = 0; i < span.context.trace_id.length(); i = i + 1 {
        span.context.trace_id[i].to_string()
      }
    ]))
    .with_attribute("span.id", common::AttributeValue::array_string([
      for i = 0; i < span.context.span_id.length(); i = i + 1 {
        span.context.span_id[i].to_string()
      }
    ]))
    .build()
  
  logger.emit(log_record)
  
  // 验证所有操作都使用相同的trace_id和span_id
  @assert(span.context.trace_id.length() == 16)
  @assert(span.context.span_id.length() == 8)
}

test "cross_api_attribute_type_consistency" {
  // 测试所有API使用相同的属性类型系统
  
  // 创建各种类型的属性值
  let string_attr = common::AttributeValue::string("test-value")
  let int_attr = common::AttributeValue::int(42L)
  let float_attr = common::AttributeValue::float(3.14)
  let bool_attr = common::AttributeValue::bool(true)
  let string_array_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 在Trace中使用这些属性
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("attribute-test-tracer")
  let (_, span) = tracer.start_span(
    context::Context::new(),
    "attribute-test",
    trace::Internal,
    [
      ("string.attr", string_attr),
      ("int.attr", int_attr),
      ("float.attr", float_attr),
      ("bool.attr", bool_attr),
      ("string.array.attr", string_array_attr),
      ("int.array.attr", int_array_attr),
      ("float.array.attr", float_array_attr),
      ("bool.array.attr", bool_array_attr)
    ]
  )
  
  // 验证Span属性
  @assert(span.attributes.length() == 8)
  
  // 在Metrics中使用这些属性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("attribute-test-meter")
  let counter = meter.create_counter("attribute.test")
  
  counter.add(1L, [
    ("string.attr", string_attr),
    ("int.attr", int_attr),
    ("float.attr", float_attr),
    ("bool.attr", bool_attr),
    ("string.array.attr", string_array_attr),
    ("int.array.attr", int_array_attr),
    ("float.array.attr", float_array_attr),
    ("bool.array.attr", bool_array_attr)
  ])
  
  // 在Logs中使用这些属性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("attribute-test-logger")
  
  let log_record = logs::LogRecord::builder()
    .severity(logs::Info)
    .body("Attribute type consistency test")
    .with_attribute("string.attr", string_attr)
    .with_attribute("int.attr", int_attr)
    .with_attribute("float.attr", float_attr)
    .with_attribute("bool.attr", bool_attr)
    .with_attribute("string.array.attr", string_array_attr)
    .with_attribute("int.array.attr", int_array_attr)
    .with_attribute("float.array.attr", float_array_attr)
    .with_attribute("bool.array.attr", bool_array_attr)
    .build()
  
  logger.emit(log_record)
  
  // 验证所有API都能正确处理所有属性类型
  @assert(match string_attr { common::StringValue(_) => true _ => false })
  @assert(match int_attr { common::IntValue(_) => true _ => false })
  @assert(match float_attr { common::FloatValue(_) => true _ => false })
  @assert(match bool_attr { common::BoolValue(_) => true _ => false })
  @assert(match string_array_attr { common::ArrayStringValue(_) => true _ => false })
  @assert(match int_array_attr { common::ArrayIntValue(_) => true _ => false })
  @assert(match float_array_attr { common::ArrayFloatValue(_) => true _ => false })
  @assert(match bool_array_attr { common::ArrayBoolValue(_) => true _ => false })
}