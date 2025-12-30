// 错误处理边界条件测试
// 测试遥测系统在各种错误和边界条件下的行为

test "metrics_with_invalid_inputs" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("error_test_meter")
  
  let counter = meter.create_counter("error_counter")
  let histogram = meter.create_histogram("error_histogram")
  let up_down_counter = meter.create_up_down_counter("error_updown")
  let gauge = meter.create_gauge("error_gauge")
  
  // 测试极值输入
  counter.add(9223372036854775807L)  // Int64最大值
  counter.add(-9223372036854775808L) // Int64最小值（虽然counter通常不应该为负）
  
  histogram.record(1.7976931348623157e308)      // Double最大值
  histogram.record(-1.7976931348623157e308)     // Double最小值
  histogram.record(1.0/0.0)                     // 正无穷
  histogram.record(-1.0/0.0)                    // 负无穷
  histogram.record(0.0/0.0)                     // NaN
  
  up_down_counter.add(9223372036854775807L)    // Int64最大值
  up_down_counter.add(-9223372036854775808L)   // Int64最小值
  
  gauge.record(1.7976931348623157e308)         // Double最大值
  gauge.record(-1.7976931348623157e308)        // Double最小值
  gauge.record(1.0/0.0)                        // 正无穷
  gauge.record(-1.0/0.0)                       // 负无穷
  gauge.record(0.0/0.0)                        // NaN
  
  @test.succeed()
}

test "metrics_with_empty_and_null_inputs" {
  let provider = NoopMeterProvider::{}
  let meter = provider.get_meter("null_test_meter")
  
  // 测试空字符串名称
  let empty_counter = meter.create_counter("")
  let empty_histogram = meter.create_histogram("")
  let empty_updown = meter.create_up_down_counter("")
  let empty_gauge = meter.create_gauge("")
  
  empty_counter.add(1L)
  empty_histogram.record(1.0)
  empty_updown.add(1L)
  empty_gauge.record(1.0)
  
  // 测试极长名称
  let long_name = "a".repeat(10000)
  let long_counter = meter.create_counter(long_name)
  long_counter.add(1L)
  
  // 测试特殊字符名称
  let special_name = "Special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./ 你好🌍"
  let special_counter = meter.create_counter(special_name)
  special_counter.add(1L)
  
  // 测试空属性数组
  empty_counter.add(1L, Some([]))
  
  // 测试包含null值的属性（在MoonBit中用None表示）
  let counter_with_none_attrs = meter.create_counter("test_counter")
  counter_with_none_attrs.add(1L, None)  // 使用None而不是空数组
  
  @test.succeed()
}

test "logs_with_invalid_and_edge_case_inputs" {
  let provider = NoopLoggerProvider::{}
  let logger = provider.get_logger("error_test_logger")
  
  // 测试极长日志消息
  let very_long_message = "x".repeat(1000000)  // 1MB的日志消息
  logger.info(very_long_message)
  
  // 测试包含特殊字符的日志消息
  let special_message = "Special chars: \0\x01\x02\x1F\x7F\x80\xFF\t\n\r\"'\\{}[]()<>@#$%^&*()_+-=|/?.>,<~`"
  logger.info(special_message)
  
  // 测试Unicode字符
  let unicode_message = "Unicode test: 你好 🌍 🚀 📊 ñáéíóú 🎉 🎊 🎈"
  logger.info(unicode_message)
  
  // 测试空字符串日志消息
  logger.info("")
  
  // 测试只有空格的日志消息
  logger.info("   \t\n\r   ")
  
  // 测试包含各种控制字符的消息
  let control_chars_message = "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x0D\x0E\x0F"
  logger.info(control_chars_message)
  
  // 测试复杂属性集合
  let complex_attributes = [
    ("", @azimuth.telemetry.api.common.AttributeValue::string("empty_key")),
    ("empty_value", @azimuth.telemetry.api.common.AttributeValue::string("")),
    ("very_long_key", @azimuth.telemetry.api.common.AttributeValue::string("x".repeat(1000))),
    ("very_long_value", @azimuth.telemetry.api.common.AttributeValue::string("y".repeat(1000))),
    ("special_key", @azimuth.telemetry.api.common.AttributeValue::string("!@#$%^&*()")),
    ("unicode_key", @azimuth.telemetry.api.common.AttributeValue::string("键值")),
    ("max_int", @azimuth.telemetry.api.common.AttributeValue::int(9223372036854775807L)),
    ("min_int", @azimuth.telemetry.api.common.AttributeValue::int(-9223372036854775808L)),
    ("max_float", @azimuth.telemetry.api.common.AttributeValue::float(1.7976931348623157e308)),
    ("min_float", @azimuth.telemetry.api.common.AttributeValue::float(-1.7976931348623157e308)),
    ("infinity", @azimuth.telemetry.api.common.AttributeValue::float(1.0/0.0)),
    ("neg_infinity", @azimuth.telemetry.api.common.AttributeValue::float(-1.0/0.0)),
    ("nan", @azimuth.telemetry.api.common.AttributeValue::float(0.0/0.0)),
    ("empty_string_array", @azimuth.telemetry.api.common.AttributeValue::array_string([])),
    ("large_string_array", @azimuth.telemetry.api.common.AttributeValue::array_string([
      "x".repeat(1000), "y".repeat(1000), "z".repeat(1000)
    ])),
    ("empty_int_array", @azimuth.telemetry.api.common.AttributeValue::array_int([])),
    ("large_int_array", @azimuth.telemetry.api.common.AttributeValue::array_int([
      9223372036854775807L, -9223372036854775808L, 0L
    ]))
  ]
  
  logger.error("Complex attributes test", Some(complex_attributes))
  
  // 测试LogRecordBuilder的边界情况
  let edge_case_log = LogRecord::builder()
    .timestamp(-9223372036854775808L)  // Int64最小值
    .severity(Fatal)
    .body("")
    .with_attribute("", @azimuth.telemetry.api.common.AttributeValue::string(""))
    .with_attribute("normal", @azimuth.telemetry.api.common.AttributeValue::string("value"))
    .build()
  
  assert_eq(edge_case_log.timestamp_unix_nanos, -9223372036854775808L)
  match edge_case_log.severity_number { Fatal => @test.succeed() _ => @test.fail("Expected Fatal") }
  match edge_case_log.body { Some(body) => assert_eq(body, "") None => @test.fail("Expected empty body") }
  assert_eq(edge_case_log.attributes.length(), 2)
  
  @test.succeed()
}

test "trace_with_invalid_and_edge_case_inputs" {
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("error_test_tracer")
  
  // 测试空span名称
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let (_, empty_span) = tracer.start_span(ctx, "")
  assert_eq(empty_span.name, "")
  
  // 测试极长span名称
  let very_long_name = "x".repeat(10000)
  let (_, long_span) = tracer.start_span(ctx, very_long_name)
  assert_eq(long_span.name, very_long_name)
  
  // 测试特殊字符span名称
  let special_name = "Special!@#$%^&*()_+-={}[]|\\:;\"'<>?,./ 你好🌍"
  let (_, special_span) = tracer.start_span(ctx, special_name)
  assert_eq(special_span.name, special_name)
  
  // 测试极值时间戳
  let min_time = -9223372036854775808L
  let max_time = 9223372036854775807L
  let (_, min_time_span) = tracer.start_span(ctx, "min_time", start_time_unix_nanos: Some(min_time))
  let (_, max_time_span) = tracer.start_span(ctx, "max_time", start_time_unix_nanos: Some(max_time))
  
  assert_eq(min_time_span.start_time_unix_nanos, min_time)
  assert_eq(max_time_span.start_time_unix_nanos, max_time)
  
  // 测试复杂属性集合
  let complex_attributes = [
    ("", @azimuth.telemetry.api.common.AttributeValue::string("empty_key")),
    ("span.type", @azimuth.telemetry.api.common.AttributeValue::string("error_test")),
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(500)),
    ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Internal server error")),
    ("stack.trace", @azimuth.telemetry.api.common.AttributeValue::string("at com.example.Test.method(Test.java:123)")),
    ("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user_12345")),
    ("session.id", @azimuth.telemetry.api.common.AttributeValue::string("sess_67890")),
    ("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req_abcdef")),
    ("correlation.id", @azimuth.telemetry.api.common.AttributeValue::string("corr_123456")),
    ("retry.count", @azimuth.telemetry.api.common.AttributeValue::int(3)),
    ("timeout.ms", @azimuth.telemetry.api.common.AttributeValue::int(30000)),
    ("payload.size", @azimuth.telemetry.api.common.AttributeValue::int(1024000)),
    ("compression.ratio", @azimuth.telemetry.api.common.AttributeValue::float(0.75)),
    ("cache.hit", @azimuth.telemetry.api.common.AttributeValue::bool(false)),
    ("circuit.breaker.open", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("rate.limited", @azimuth.telemetry.api.common.AttributeValue::bool(false))
  ]
  
  let (_, complex_span) = tracer.start_span(ctx, "complex_error_span", 
    kind: Some(Server), 
    attributes: Some(complex_attributes))
  
  assert_eq(complex_span.attributes.length(), 16)
  
  // 测试深层嵌套的span层次结构
  let mut current_ctx = ctx
  let mut spans = []
  let max_depth = 1000
  
  let mut i = 0
  while i < max_depth {
    let span_name = "deep_span_" + i.to_string()
    let (new_ctx, span) = tracer.start_span(current_ctx, span_name)
    spans.push(span)
    current_ctx = new_ctx
    i = i + 1
  }
  
  assert_eq(spans.length(), max_depth)
  
  // 验证最深层的span
  let deepest_span = spans[max_depth - 1]
  assert_eq(deepest_span.name, "deep_span_" + (max_depth - 1).to_string())
  
  @test.succeed()
}

test "common_types_with_edge_cases" {
  // 测试Resource的边界情况
  let empty_resource = @azimuth.telemetry.api.common.Resource::default("")
  assert_eq(empty_resource.service_name, "")
  
  let very_long_service_name = "x".repeat(10000)
  let long_resource = @azimuth.telemetry.api.common.Resource::default(very_long_service_name)
  assert_eq(long_resource.service_name, very_long_service_name)
  
  let special_service_name = "Special!@#$%^&*() 你好🌍"
  let special_resource = @azimuth.telemetry.api.common.Resource::default(special_service_name)
  assert_eq(special_resource.service_name, special_service_name)
  
  // 测试InstrumentationScope的边界情况
  let empty_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "",
    version: None,
    schema_url: None
  }
  assert_eq(empty_scope.name, "")
  
  let very_long_scope_name = "y".repeat(10000)
  let long_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: very_long_scope_name,
    version: Some("z".repeat(1000)),
    schema_url: Some("http://example.com/" + "a".repeat(1000))
  }
  assert_eq(long_scope.name, very_long_scope_name)
  
  // 测试AttributeValue的边界情况
  let empty_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("")
  match empty_string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Expected empty string")
  }
  
  let very_long_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("x".repeat(1000000))
  match very_long_string_attr {
    @azimuth.telemetry.api.common.StringValue(value) => assert_eq(value.length(), 1000000)
    _ => @test.fail("Expected very long string")
  }
  
  // 测试数组的边界情况
  let empty_string_array = @azimuth.telemetry.api.common.AttributeValue::array_string([])
  match empty_string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => assert_eq(values.length(), 0)
    _ => @test.fail("Expected empty string array")
  }
  
  let large_string_array = @azimuth.telemetry.api.common.AttributeValue::array_string(
    [for i = 0; i < 10000; i = i + 1].map(fn(i) { "item_" + i.to_string() })
  )
  match large_string_array {
    @azimuth.telemetry.api.common.ArrayStringValue(values) => {
      assert_eq(values.length(), 10000)
      assert_eq(values[0], "item_0")
      assert_eq(values[9999], "item_9999")
    }
    _ => @test.fail("Expected large string array")
  }
  
  let large_int_array = @azimuth.telemetry.api.common.AttributeValue::array_int(
    [for i = 0; i < 10000; i = i + 1].map(fn(i) { i.to_int64() })
  )
  match large_int_array {
    @azimuth.telemetry.api.common.ArrayIntValue(values) => {
      assert_eq(values.length(), 10000)
      assert_eq(values[0], 0L)
      assert_eq(values[9999], 9999L)
    }
    _ => @test.fail("Expected large int array")
  }
  
  @test.succeed()
}

test "cross_api_error_propagation" {
  // 测试跨API的错误传播
  
  // 创建包含错误信息的共享属性
  let error_attributes = [
    ("error.type", @azimuth.telemetry.api.common.AttributeValue::string("validation_error")),
    ("error.code", @azimuth.telemetry.api.common.AttributeValue::int(400)),
    ("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Invalid input parameter")),
    ("error.stack", @azimuth.telemetry.api.common.AttributeValue::string("at com.example.Service.process(Service.java:123)")),
    ("error.retryable", @azimuth.telemetry.api.common.AttributeValue::bool(false)),
    ("error.category", @azimuth.telemetry.api.common.AttributeValue::string("client_error"))
  ]
  
  // Metrics API - 记录错误指标
  let metrics_provider = NoopMeterProvider::{}
  let metrics_meter = metrics_provider.get_meter("error_meter")
  let error_counter = metrics_meter.create_counter("errors_total")
  let error_latency = metrics_meter.create_histogram("error_processing_time_ms")
  
  error_counter.add(1L, Some(error_attributes))
  error_latency.record(150.0, Some(error_attributes))
  
  // Logs API - 记录错误日志
  let logs_provider = NoopLoggerProvider::{}
  let logger = logs_provider.get_logger("error_logger")
  
  logger.error("Processing error occurred", Some(error_attributes))
  
  // 创建详细的错误日志记录
  let detailed_error_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Detailed error information")
    .with_attribute("error.type", @azimuth.telemetry.api.common.AttributeValue::string("validation_error"))
    .with_attribute("error.code", @azimuth.telemetry.api.common.AttributeValue::int(400))
    .with_attribute("error.message", @azimuth.telemetry.api.common.AttributeValue::string("Invalid input parameter"))
    .with_attribute("error.field", @azimuth.telemetry.api.common.AttributeValue::string("user_id"))
    .with_attribute("error.value", @azimuth.telemetry.api.common.AttributeValue::string("invalid_id"))
    .with_attribute("error.expected", @azimuth.telemetry.api.common.AttributeValue::string("numeric_id"))
    .with_attribute("error.constraint", @azimuth.telemetry.api.common.AttributeValue::string("must_be_positive_integer"))
    .with_attribute("request.id", @azimuth.telemetry.api.common.AttributeValue::string("req_12345"))
    .with_attribute("user.id", @azimuth.telemetry.api.common.AttributeValue::string("user_67890"))
    .with_attribute("session.id", @azimuth.telemetry.api.common.AttributeValue::string("sess_abcdef"))
    .with_attribute("client.ip", @azimuth.telemetry.api.common.AttributeValue::string("192.168.1.100"))
    .with_attribute("user.agent", @azimuth.telemetry.api.common.AttributeValue::string("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
    .with_attribute("endpoint", @azimuth.telemetry.api.common.AttributeValue::string("/api/users"))
    .with_attribute("method", @azimuth.telemetry.api.common.AttributeValue::string("POST"))
    .with_attribute("payload.size", @azimuth.telemetry.api.common.AttributeValue::int(1024))
    .with_attribute("processing.time.ms", @azimuth.telemetry.api.common.AttributeValue::float(150.0))
    .with_attribute("database.queries", @azimuth.telemetry.api.common.AttributeValue::int(3))
    .with_attribute("cache.hits", @azimuth.telemetry.api.common.AttributeValue::int(1))
    .with_attribute("cache.misses", @azimuth.telemetry.api.common.AttributeValue::int(2))
    .build()
  
  // Trace API - 记录错误span
  let trace_provider = NoopTracerProvider::{}
  let tracer = trace_provider.get_tracer("error_tracer")
  
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  let (error_ctx, error_span) = tracer.start_span(ctx, "error_processing", 
    kind: Some(Server), 
    attributes: Some(error_attributes))
  
  // 验证错误span的属性
  assert_eq(error_span.attributes.length(), 6)
  match error_span.attributes[0] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "error.type")
      assert_eq(value, "validation_error")
    }
    _ => @test.fail("Expected error.type attribute")
  }
  
  @test.succeed()
}

test "resource_exhaustion_scenarios" {
  // 测试资源耗尽场景
  
  // 创建大量不同的meter
  let metrics_provider = NoopMeterProvider::{}
  let mut meters = []
  
  let mut i = 0
  while i < 1000 {
    let meter = metrics_provider.get_meter("meter_" + i.to_string())
    meters.push(meter)
    i = i + 1
  }
  
  // 为每个meter创建多个仪器
  i = 0
  while i < meters.length() {
    let meter = meters[i]
    let counter = meter.create_counter("counter_" + i.to_string())
    let histogram = meter.create_histogram("histogram_" + i.to_string())
    
    counter.add(1L)
    histogram.record(i.to_float())
    
    i = i + 1
  }
  
  // 创建大量不同的logger
  let logs_provider = NoopLoggerProvider::{}
  let mut loggers = []
  
  i = 0
  while i < 1000 {
    let logger = logs_provider.get_logger("logger_" + i.to_string())
    loggers.push(logger)
    i = i + 1
  }
  
  // 使用每个logger记录日志
  i = 0
  while i < loggers.length() {
    let logger = loggers[i]
    logger.info("Log message from logger " + i.to_string())
    i = i + 1
  }
  
  // 创建大量不同的tracer
  let trace_provider = NoopTracerProvider::{}
  let mut tracers = []
  
  i = 0
  while i < 1000 {
    let tracer = trace_provider.get_tracer("tracer_" + i.to_string())
    tracers.push(tracer)
    i = i + 1
  }
  
  // 使用每个tracer创建span
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  i = 0
  while i < tracers.length() {
    let tracer = tracers[i]
    let (_, span) = tracer.start_span(ctx, "span_" + i.to_string())
    i = i + 1
  }
  
  @test.succeed()
}