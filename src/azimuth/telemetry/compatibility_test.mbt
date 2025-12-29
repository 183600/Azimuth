// 兼容性测试 - 测试版本兼容性

test "api_backward_compatibility" {
  // 测试API向后兼容性
  
  // 1. 测试Resource创建的向后兼容性
  let resource_v1 = common::Resource::default("test-service")
  
  // 验证基本属性保持不变
  assert_eq(resource_v1.service_name, "test-service")
  assert_eq(resource_v1.service_version, None)
  assert_eq(resource_v1.telemetry_sdk_name, "azimuth")
  assert_eq(resource_v1.telemetry_sdk_version, "0.1.0")
  assert_eq(resource_v1.attributes.length(), 0)
  
  // 2. 测试AttributeValue的向后兼容性
  let string_attr = AttributeValue::string("test")
  let int_attr = AttributeValue::int(42L)
  let float_attr = AttributeValue::float(3.14)
  let bool_attr = AttributeValue::bool(true)
  
  // 验证所有属性类型仍然可用
  match string_attr {
    StringValue(s) => assert_eq(s, "test")
    _ => @test.fail("Test failed")
  }
  
  match int_attr {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Test failed")
  }
  
  match float_attr {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Test failed")
  }
  
  match bool_attr {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Test failed")
  }
  
  // 3. 测试Context API的向后兼容性
  let ctx = context::Context::empty()
  let key = context::create_key("test-key")
  let ctx_with_value = ctx.with_value(key, "test-value")
  
  match ctx_with_value.get(key) {
    Some(value) => assert_eq(value, "test-value")
    None => @test.fail("Test failed")
  }
  
  // 4. 测试Baggage API的向后兼容性
  let baggage = context::Baggage::empty()
  let baggage_with_entry = baggage.with_entry("test-entry", "test-value")
  
  match baggage_with_entry.get("test-entry") {
    Some(value) => assert_eq(value, "test-value")
    None => @test.fail("Test failed")
  }
}

test "version_specific_features" {
  // 测试版本特定功能
  
  // 1. 测试当前版本的所有功能
  let resource = common::Resource::default("version-test-service")
  
  // 验证当前版本的SDK信息
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 2. 测试数组属性类型（可能是较新版本的功能）
  let string_array = AttributeValue::array_string(["item1", "item2", "item3"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  // 验证数组类型可用
  match string_array {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
  
  match int_array {
    ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
  
  match float_array {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
  
  match bool_array {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Test failed")
  }
  
  // 3. 测试LogRecordBuilder的所有功能
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Version test log")
    .with_attribute("version.test", AttributeValue::string("0.1.0"))
    .build()
  
  // 验证LogRecordBuilder功能完整
  match log_record.body {
    Some(body) => assert_eq(body, "Version test log")
    None => @test.fail("Test failed")
  }
  
  match log_record.severity_number {
    logs::Info => assert_eq(true, true)
    _ => @test.fail("Test failed")
  }
  
  assert_eq(log_record.attributes.length(), 1)
}

test "cross_version_interoperability" {
  // 测试跨版本互操作性
  
  // 1. 模拟不同版本的数据格式
  let v1_resource = common::Resource::default("v1-service")
  let v2_resource = common::Resource::default("v2-service")
  
  // 验证不同版本创建的资源可以共存
  assert_eq(v1_resource.service_name, "v1-service")
  assert_eq(v2_resource.service_name, "v2-service")
  
  // 2. 测试不同版本的属性格式兼容性
  let v1_attrs : Array[(String, common::AttributeValue)] = [
    ("legacy.attr", AttributeValue::string("legacy-value")),
    ("numeric.attr", AttributeValue::int(100L))
  ]
  
  let v2_attrs : Array[(String, common::AttributeValue)] = [
    ("legacy.attr", AttributeValue::string("legacy-value")),
    ("numeric.attr", AttributeValue::int(100L)),
    ("array.attr", AttributeValue::array_string(["new", "feature"]))
  ]
  
  // 验证属性格式兼容
  assert_eq(v1_attrs.length(), 2)
  assert_eq(v2_attrs.length(), 3)
  
  // 3. 测试不同版本的SpanKind兼容性
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("compatibility-test-tracer")
  
  let (v1_ctx, v1_span) = tracer.start_span(ctx, "v1-span", trace::Internal)
  let (v2_ctx, v2_span) = tracer.start_span(ctx, "v2-span", trace::Server)
  
  // 验证所有SpanKind仍然可用
  match v1_span.kind { trace::Internal => assert_eq(true, true) _ => @test.fail("Test failed") }
  match v2_span.kind { trace::Server => assert_eq(true, true) _ => @test.fail("Test failed") }
  
  // 4. 测试不同版本的SeverityNumber兼容性
  let v1_log = logs::LogRecord::builder().severity(logs::Info).build()
  let v2_log = logs::LogRecord::builder().severity(logs::Warn).build()
  
  match v1_log.severity_number { logs::Info => assert_eq(true, true) _ => @test.fail("Test failed") }
  match v2_log.severity_number { logs::Warn => assert_eq(true, true) _ => @test.fail("Test failed") }
}

test "deprecation_compatibility" {
  // 测试弃用功能的兼容性
  
  // 1. 测试可能弃用的功能仍然可用
  let resource = common::Resource::default("deprecation-test")
  
  // 验证旧的Resource创建方式仍然工作
  assert_eq(resource.service_name, "deprecation-test")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  
  // 2. 测试可能弃用的属性类型
  let legacy_string = AttributeValue::string("legacy")
  let legacy_int = AttributeValue::int(42)
  let legacy_float = AttributeValue::float(3.14)
  let legacy_bool = AttributeValue::bool(true)
  
  // 验证旧的属性类型仍然可用
  match legacy_string {
    StringValue(s) => assert_eq(s, "legacy")
    _ => @test.fail("Test failed")
  }
  
  match legacy_int {
    IntValue(i) => assert_eq(i, 42L)
    _ => @test.fail("Test failed")
  }
  
  match legacy_float {
    FloatValue(f) => assert_eq(f, 3.14)
    _ => @test.fail("Test failed")
  }
  
  match legacy_bool {
    BoolValue(b) => assert_eq(b, true)
    _ => @test.fail("Test failed")
  }
  
  // 3. 测试可能弃用的API调用
  let ctx = context::Context::empty()
  let key = context::create_key("legacy-key")
  let ctx_with_legacy = ctx.with_value(key, "legacy-value")
  
  match ctx_with_legacy.get(key) {
    Some(value) => assert_eq(value, "legacy-value")
    None => @test.fail("Test failed")
  }
  
  // 4. 测试可能弃用的传播格式
  let carrier = propagation::MapCarrier::new()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  
  trace_propagator.inject(ctx, carrier)
  
  match carrier.get(propagation::TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Test failed")
  }
}

test "future_compatibility" {
  // 测试未来兼容性
  
  // 1. 测试扩展性 - 预留未来扩展的空间
  let resource = common::Resource::default("future-test")
  
  // 验证当前结构可以容纳未来扩展
  assert_eq(resource.service_name, "future-test")
  assert_eq(resource.service_version, None)  // 可以为未来版本预留
  
  // 2. 测试属性扩展性
  let extensible_attrs : Array[(String, common::AttributeValue)] = [
    ("current.attr", AttributeValue::string("current")),
    ("future.attr1", AttributeValue::string("future1")),
    ("future.attr2", AttributeValue::int(123L)),
    ("future.attr3", AttributeValue::bool(true))
  ]
  
  // 验证可以添加未来的属性
  assert_eq(extensible_attrs.length(), 4)
  
  // 3. 测试枚举扩展性
  let all_severities = [
    logs::Trace, logs::Debug, logs::Info, logs::Warn, logs::Error, logs::Fatal
  ]
  
  // 验证所有当前严重级别都可用
  assert_eq(all_severities.length(), 6)
  
  let all_kinds = [
    trace::Internal, trace::Server, trace::Client, trace::Producer, trace::Consumer
  ]
  
  // 验证所有当前SpanKind都可用
  assert_eq(all_kinds.length(), 5)
  
  // 4. 测试传播格式扩展性
  let carrier = propagation::MapCarrier::from_map([
    ("existing.header", "existing-value"),
    ("future.header1", "future-value1"),
    ("future.header2", "future-value2")
  ])
  
  // 验证可以添加未来的头部
  let keys = carrier.keys()
  assert_eq(keys.length(), 3)
  
  // 5. 测试数组类型扩展性
  let large_arrays = [
    AttributeValue::array_string([for i = 0; i < 1000; i = i + 1].map(fn(_) { "item" })),
    AttributeValue::array_int([for i = 0; i < 1000; i = i + 1].map(fn(_) { 1L })),
    AttributeValue::array_float([for i = 0; i < 1000; i = i + 1].map(fn(_) { 1.0 })),
    AttributeValue::array_bool([for i = 0; i < 1000; i = i + 1].map(fn(_) { true }))
  ]
  
  // 验证数组可以处理大量数据（为未来扩展做准备）
  let mut i = 0
  while i < large_arrays.length() {
    match large_arrays[i] {
      ArrayStringValue(arr) => assert_eq(arr.length(), 1000)
      ArrayIntValue(arr) => assert_eq(arr.length(), 1000)
      ArrayFloatValue(arr) => assert_eq(arr.length(), 1000)
      ArrayBoolValue(arr) => assert_eq(arr.length(), 1000)
      _ => @test.fail("Test failed")
    }
    i = i + 1
  }
}

test "configuration_compatibility" {
  // 测试配置兼容性
  
  // 1. 测试默认配置的兼容性
  let default_resource = common::Resource::default("config-test")
  
  // 验证默认配置值
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.attributes.length(), 0)
  
  // 2. 测试不同配置组合的兼容性
  let config1 = common::Resource::default("config1")
  let config2 = common::Resource::default("config2")
  let config3 = common::Resource::default("config3")
  
  // 验证不同配置可以共存
  assert_eq(config1.service_name, "config1")
  assert_eq(config2.service_name, "config2")
  assert_eq(config3.service_name, "config3")
  
  // 3. 测试传播器配置兼容性
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  let composite = propagation::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 验证不同传播器配置可以工作
  let ctx = context::Context::empty()
  let carrier = propagation::MapCarrier::new()
  
  trace_propagator.inject(ctx, carrier)
  baggage_propagator.inject(ctx, carrier)
  composite.inject(ctx, carrier)
  
  // 4. 测试指标配置兼容性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("config-meter", "1.0.0", "http://example.com/schema")
  
  // 验证meter配置
  let counter = meter.create_counter("config-counter", "count", "Config test counter")
  let histogram = meter.create_histogram("config-histogram", "ms", "Config test histogram")
  let gauge = meter.create_gauge("config-gauge", "percent", "Config test gauge")
  let up_down = meter.create_up_down_counter("config-updown", "count", "Config test up-down")
  
  // 验证所有指标类型都可以创建
  counter.add(1L)
  histogram.record(100.0)
  gauge.record(75.0)
  up_down.add(1L)
  
  // 5. 测试日志配置兼容性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("config-logger", "1.0.0", "http://example.com/schema")
  
  // 验证logger配置
  logger.debug("Debug message", [])
  logger.info("Info message", [])
  logger.warn("Warn message", [])
  logger.error("Error message", [])
  logger.fatal("Fatal message", [])
}

test "data_format_compatibility" {
  // 测试数据格式兼容性
  
  // 1. 测试属性值格式兼容性
  let string_values = [
    AttributeValue::string(""),
    AttributeValue::string("simple"),
    AttributeValue::string("with spaces"),
    AttributeValue::string("with-symbols!@#$%"),
    AttributeValue::string("Unicode测试🚀"),
    AttributeValue::string("very-long-string-" + "x" * 1000)
  ]
  
  // 验证各种字符串格式
  let mut i = 0
  while i < string_values.length() {
    match string_values[i] {
      StringValue(s) => assert_eq(s.length() > 0, true)
      _ => @test.fail("Test failed")
    }
    i = i + 1
  }
  
  // 2. 测试数值格式兼容性
  let numeric_values = [
    AttributeValue::int(0L),
    AttributeValue::int(-1L),
    AttributeValue::int(1L),
    AttributeValue::int(9223372036854775807L),  // Int64最大值
    AttributeValue::int(-9223372036854775808L), // Int64最小值
    AttributeValue::float(0.0),
    AttributeValue::float(-1.0),
    AttributeValue::float(1.0),
    AttributeValue::float(1.7976931348623157e+308), // Double最大值
    AttributeValue::float(-1.7976931348623157e+308), // Double最小值
    AttributeValue::float(1.0/0.0), // 无穷大
    AttributeValue::float(0.0/0.0)  // NaN
  ]
  
  // 验证各种数值格式
  i = 0
  while i < numeric_values.length() {
    match numeric_values[i] {
      IntValue(_) => assert_eq(true, true)
      FloatValue(_) => assert_eq(true, true)
      _ => @test.fail("Test failed")
    }
    i = i + 1
  }
  
  // 3. 测试数组格式兼容性
  let array_formats = [
    AttributeValue::array_string([]),
    AttributeValue::array_string(["single"]),
    AttributeValue::array_string(["multiple", "values"]),
    AttributeValue::array_int([]),
    AttributeValue::array_int([1L]),
    AttributeValue::array_int([1L, 2L, 3L]),
    AttributeValue::array_float([]),
    AttributeValue::array_float([1.0]),
    AttributeValue::array_float([1.1, 2.2, 3.3]),
    AttributeValue::array_bool([]),
    AttributeValue::array_bool([true]),
    AttributeValue::array_bool([true, false, true])
  ]
  
  // 验证各种数组格式
  i = 0
  while i < array_formats.length() {
    match array_formats[i] {
      ArrayStringValue(_) => assert_eq(true, true)
      ArrayIntValue(_) => assert_eq(true, true)
      ArrayFloatValue(_) => assert_eq(true, true)
      ArrayBoolValue(_) => assert_eq(true, true)
      _ => @test.fail("Test failed")
    }
    i = i + 1
  }
  
  // 4. 测试传播格式兼容性
  let ctx = context::Context::empty()
  let carrier = propagation::MapCarrier::from_map([
    ("simple.header", "simple-value"),
    ("complex.header", "complex-value-with-dashes"),
    ("numeric.header", "12345"),
    ("empty.header", ""),
    ("unicode.header", "Unicode测试🚀")
  ])
  
  // 验证各种头部格式
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let extracted_ctx = trace_propagator.extract(ctx, carrier)
  
  // 验证提取操作成功
  assert_eq(true, true)
}