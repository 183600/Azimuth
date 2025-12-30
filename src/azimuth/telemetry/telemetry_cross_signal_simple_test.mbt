// 简化的跨模块遥测数据集成测试
// 测试Trace、Metrics、Logs和Context模块之间的数据流动和一致性

test "basic_cross_module_integration" {
  // 创建资源和上下文
  let resource = @azimuth.telemetry.api.common.Resource::default("test_service")
  let context = @azimuth.telemetry.api.context.Context::empty()
  let trace_key = @azimuth.telemetry.api.context.create_key("trace_id")
  
  // 模拟生成trace信息
  let trace_id = "1234567890abcdef1234567890abcdef"
  let context_with_trace = context.with_value(trace_key, trace_id)
  
  // 创建Tracer和Span
  let tracer_provider = @azimuth.telemetry.api.trace.NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test_tracer", "1.0.0")
  let (ctx, span) = tracer.start_span(
    context_with_trace,
    "test_operation",
    @azimuth.telemetry.api.trace.Server,
    [("operation_type", @azimuth.telemetry.api.common.AttributeValue::string("test"))]
  )
  
  // 创建Meter和指标
  let meter_provider = @azimuth.telemetry.api.metrics.NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", "1.0.0")
  let counter = meter.create_counter("operations_total", "count", "Total number of operations")
  
  // 记录指标数据
  counter.add(1L, [("trace_id", @azimuth.telemetry.api.common.AttributeValue::string(trace_id))])
  
  // 创建Logger和日志
  let logger_provider = @azimuth.telemetry.api.logs.NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("test_logger", "1.0.0")
  
  // 记录日志
  logger.info(
    "Operation completed successfully",
    [("trace_id", @azimuth.telemetry.api.common.AttributeValue::string(trace_id))]
  )
  
  // 验证数据一致性
  let retrieved_trace_id = ctx.get(trace_key)
  assert_eq(retrieved_trace_id.unwrap(), trace_id)
  assert_eq(span.name, "test_operation")
  assert_eq(span.kind, @azimuth.telemetry.api.trace.Server)
}

test "context_basic_operations" {
  // 测试基本上下文操作
  let context = @azimuth.telemetry.api.context.Context::empty()
  let key1 = @azimuth.telemetry.api.context.create_key("test_key1")
  let key2 = @azimuth.telemetry.api.context.create_key("test_key2")
  
  let context_with_values = context
    .with_value(key1, "value1")
    .with_value(key2, "value2")
  
  assert_eq(context_with_values.get(key1).unwrap(), "value1")
  assert_eq(context_with_values.get(key2).unwrap(), "value2")
}

test "baggage_basic_operations" {
  // 测试基本baggage操作
  let baggage = @azimuth.telemetry.api.context.Baggage::empty()
    .with_entry("user_id", "12345")
    .with_entry("session_id", "abcdef")
  
  assert_eq(baggage.get("user_id").unwrap(), "12345")
  assert_eq(baggage.get("session_id").unwrap(), "abcdef")
  assert_eq(baggage.entries.length(), 2)
}

test "attribute_value_creation" {
  // 测试属性值创建
  let string_attr = @azimuth.telemetry.api.common.AttributeValue::string("test")
  let int_attr = @azimuth.telemetry.api.common.AttributeValue::int(42L)
  let float_attr = @azimuth.telemetry.api.common.AttributeValue::float(3.14)
  let bool_attr = @azimuth.telemetry.api.common.AttributeValue::bool(true)
  
  match string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(value) => assert_eq(value, "test")
    _ => @test.fail("Expected StringValue")
  }
  
  match int_attr {
    @azimuth.telemetry.api.common.AttributeValue::IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  match float_attr {
    @azimuth.telemetry.api.common.AttributeValue::FloatValue(value) => assert_eq(value, 3.14)
    _ => @test.fail("Expected FloatValue")
  }
  
  match bool_attr {
    @azimuth.telemetry.api.common.AttributeValue::BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Expected BoolValue")
  }
}