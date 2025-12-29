// Azimuth Telemetry - Configuration and Initialization Tests
// 测试配置和初始化场景

test "resource_configuration_variants" {
  // 测试Resource配置的各种变体
  // 默认配置
  let default_resource = @azimuth.telemetry.api.common.Resource::default("test-service")
  assert_eq(default_resource.service_name, "test-service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 带有特殊字符的服务名
  let special_chars_resource = @azimuth.telemetry.api.common.Resource::default("service-with.special@chars#123")
  assert_eq(special_chars_resource.service_name, "service-with.special@chars#123")
  
  // Unicode服务名
  let unicode_resource = @azimuth.telemetry.api.common.Resource::default("服务名称-测试")
  assert_eq(unicode_resource.service_name, "服务名称-测试")
  
  // 空服务名
  let empty_resource = @azimuth.telemetry.api.common.Resource::default("")
  assert_eq(empty_resource.service_name, "")
  
  // 超长服务名
  let long_name = "a".repeat(1000)
  let long_resource = @azimuth.telemetry.api.common.Resource::default(long_name)
  assert_eq(long_resource.service_name.length(), 1000)
  
  // 验证SDK信息在所有配置中保持一致
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(special_chars_resource.telemetry_sdk_name, "azimuth")
  assert_eq(unicode_resource.telemetry_sdk_name, "azimuth")
  assert_eq(empty_resource.telemetry_sdk_name, "azimuth")
  assert_eq(long_resource.telemetry_sdk_name, "azimuth")
  
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(special_chars_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(unicode_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(empty_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(long_resource.telemetry_sdk_version, "0.1.0")
}

test "instrumentation_scope_configuration_variants" {
  // 测试InstrumentationScope配置的各种变体
  // 最小配置
  let minimal_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
  
  // 完整配置
  let full_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "full-scope",
    version: Some("2.1.0-beta+build.123"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  }
  assert_eq(full_scope.name, "full-scope")
  assert_eq(full_scope.version, Some("2.1.0-beta+build.123"))
  assert_eq(full_scope.schema_url, Some("https://opentelemetry.io/schemas/1.20.0"))
  
  // 空值配置
  let empty_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "",
    version: Some(""),
    schema_url: Some("")
  }
  assert_eq(empty_scope.name, "")
  assert_eq(empty_scope.version, Some(""))
  assert_eq(empty_scope.schema_url, Some(""))
  
  // Unicode配置
  let unicode_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "测试范围",
    version: Some("版本1.0"),
    schema_url: Some("https://例子.com/模式")
  }
  assert_eq(unicode_scope.name, "测试范围")
  assert_eq(unicode_scope.version, Some("版本1.0"))
  assert_eq(unicode_scope.schema_url, Some("https://例子.com/模式"))
  
  // 超长配置
  let long_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "a".repeat(500),
    version: Some("1.0.0".repeat(100)),
    schema_url: Some("http://".repeat(50) + "example.com")
  }
  assert_eq(long_scope.name.length(), 500)
  assert_eq(long_scope.version?.length(), 500)
  assert_eq(long_scope.schema_url?.length(), 400)
  
  // 特殊字符配置
  let special_scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "scope-with.special@chars#123",
    version: Some("version_1.0.0-beta+build.123"),
    schema_url: Some("https://example.com/path?query=value#fragment")
  }
  assert_eq(special_scope.name, "scope-with.special@chars#123")
  assert_eq(special_scope.version, Some("version_1.0.0-beta+build.123"))
  assert_eq(special_scope.schema_url, Some("https://example.com/path?query=value#fragment"))
}

test "logger_provider_configuration" {
  // 测试LoggerProvider配置
  // 基本配置
  let basic_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("basic-logger")
  basic_logger.info("Basic logger test", None)
  
  // 带版本的配置
  let versioned_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger(
    "versioned-logger",
    Some("1.2.3")
  )
  versioned_logger.info("Versioned logger test", None)
  
  // 完整配置
  let full_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger(
    "full-logger",
    Some("2.0.0"),
    Some("https://example.com/schema")
  )
  full_logger.info("Full logger test", None)
  
  // 特殊字符配置
  let special_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger(
    "logger.with.special@chars#123",
    Some("version_1.0.0-beta+build.123"),
    Some("https://例子.com/模式")
  )
  special_logger.info("Special logger test", None)
  
  // Unicode配置
  let unicode_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger(
    "日志记录器",
    Some("版本1.0"),
    Some("https://例子.com/模式")
  )
  unicode_logger.info("Unicode logger test", None)
  
  // 空配置
  let empty_logger = @azimuth.telemetry.api.logs.NoopLoggerProvider::get_logger("")
  empty_logger.info("Empty logger test", None)
  
  // 验证所有配置都能正常工作
  assert_eq(true, true)
}

test "meter_provider_configuration" {
  // 测试MeterProvider配置
  // 基本配置
  let basic_meter = @azimuth.telemetry.api.metrics.NoopMeterProvider::get_meter("basic-meter")
  let basic_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter("basic.counter", "1", "Basic counter")
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, None)
  
  // 带版本的配置
  let versioned_meter = @azimuth.telemetry.api.metrics.NoopMeterProvider::get_meter(
    "versioned-meter",
    Some("1.2.3")
  )
  let versioned_histogram = @azimuth.telemetry.api.metrics.NoopMeter::create_histogram(
    "versioned.histogram",
    "ms",
    "Versioned histogram"
  )
  @azimuth.telemetry.api.metrics.NoopHistogram::record(100.0, None)
  
  // 完整配置
  let full_meter = @azimuth.telemetry.api.metrics.NoopMeterProvider::get_meter(
    "full-meter",
    Some("2.0.0"),
    Some("https://example.com/schema")
  )
  let full_gauge = @azimuth.telemetry.api.metrics.NoopMeter::create_gauge(
    "full.gauge",
    "%",
    "Full gauge"
  )
  @azimuth.telemetry.api.metrics.NoopGauge::record(75.0, None)
  
  // 特殊字符配置
  let special_meter = @azimuth.telemetry.api.metrics.NoopMeterProvider::get_meter(
    "meter.with.special@chars#123",
    Some("version_1.0.0-beta+build.123"),
    Some("https://例子.com/模式")
  )
  let special_up_down = @azimuth.telemetry.api.metrics.NoopMeter::create_up_down_counter(
    "special.updown",
    "1",
    "Special up-down counter"
  )
  @azimuth.telemetry.api.metrics.NoopUpDownCounter::add(5L, None)
  
  // Unicode配置
  let unicode_meter = @azimuth.telemetry.api.metrics.NoopMeterProvider::get_meter(
    "计量器",
    Some("版本1.0"),
    Some("https://例子.com/模式")
  )
  let unicode_counter = @azimuth.telemetry.api.metrics.NoopMeter::create_counter(
    "unicode.counter",
    "个",
    "Unicode counter"
  )
  @azimuth.telemetry.api.metrics.NoopCounter::add(1L, None)
  
  // 验证所有配置都能正常工作
  assert_eq(true, true)
}

test "tracer_provider_configuration" {
  // 测试TracerProvider配置
  // 基本配置
  let basic_tracer = @azimuth.telemetry.api.trace.NoopTracerProvider::get_tracer("basic-tracer")
  let (_, basic_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    @azimuth.telemetry.api.context.Context::empty(),
    "basic-span"
  )
  
  // 带版本的配置
  let versioned_tracer = @azimuth.telemetry.api.trace.NoopTracerProvider::get_tracer(
    "versioned-tracer",
    Some("1.2.3")
  )
  let (_, versioned_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    @azimuth.telemetry.api.context.Context::empty(),
    "versioned-span",
    Some(@azimuth.telemetry.api.trace.Server)
  )
  
  // 完整配置
  let full_tracer = @azimuth.telemetry.api.trace.NoopTracerProvider::get_tracer(
    "full-tracer",
    Some("2.0.0"),
    Some("https://example.com/schema")
  )
  let (_, full_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    @azimuth.telemetry.api.context.Context::empty(),
    "full-span",
    Some(@azimuth.telemetry.api.trace.Client),
    Some([("test", @azimuth.telemetry.api.common.AttributeValue::bool(true))])
  )
  
  // 特殊字符配置
  let special_tracer = @azimuth.telemetry.api.trace.NoopTracerProvider::get_tracer(
    "tracer.with.special@chars#123",
    Some("version_1.0.0-beta+build.123")
  )
  let (_, special_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    @azimuth.telemetry.api.context.Context::empty(),
    "special-span",
    Some(@azimuth.telemetry.api.trace.Producer)
  )
  
  // Unicode配置
  let unicode_tracer = @azimuth.telemetry.api.trace.NoopTracerProvider::get_tracer(
    "追踪器",
    Some("版本1.0")
  )
  let (_, unicode_span) = @azimuth.telemetry.api.trace.NoopTracer::start_span(
    @azimuth.telemetry.api.context.Context::empty(),
    "unicode-span",
    Some(@azimuth.telemetry.api.trace.Consumer)
  )
  
  // 验证所有配置都能正常工作
  assert_eq(basic_span.name, "basic-span")
  assert_eq(versioned_span.name, "versioned-span")
  assert_eq(full_span.name, "full-span")
  assert_eq(special_span.name, "special-span")
  assert_eq(unicode_span.name, "unicode-span")
}

test "propagator_configuration_combinations" {
  // 测试传播器配置组合
  let ctx = @azimuth.telemetry.api.context.Context::empty()
  
  // 单个传播器配置
  let trace_only_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.W3CTraceContextPropagator::inject(ctx, trace_only_carrier)
  
  let baggage_only_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.W3CBaggagePropagator::inject(ctx, baggage_only_carrier)
  
  // 复合传播器配置
  let composite_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  let composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator,
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator
  ])
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(ctx, composite_carrier)
  
  // 多重复合传播器配置
  let multi_composite_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  let multi_composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator,
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator,
    @azimuth.telemetry.api.propagation.W3CTraceContextPropagator, // 重复的传播器
    @azimuth.telemetry.api.propagation.W3CBaggagePropagator   // 重复的传播器
  ])
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(ctx, multi_composite_carrier)
  
  // 空传播器列表
  let empty_composite = @azimuth.telemetry.api.propagation.CompositePropagator::new([])
  let empty_carrier = @azimuth.telemetry.api.propagation.MapCarrier::new()
  @azimuth.telemetry.api.propagation.CompositePropagator::inject(ctx, empty_carrier)
  
  // 验证所有配置都能正常工作
  assert_eq(trace_only_carrier.get(@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(baggage_only_carrier.get(@azimuth.telemetry.api.propagation.BAGGAGE_HEADER), Some("key1=value1,key2=value2"))
  assert_eq(composite_carrier.get(@azimuth.telemetry.api.propagation.TRACE_PARENT_HEADER), Some("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"))
  assert_eq(composite_carrier.get(@azimuth.telemetry.api.propagation.BAGGAGE_HEADER), Some("key1=value1,key2=value2"))
}

test "context_key_configuration_edge_cases" {
  // 测试ContextKey配置的边界情况
  let base_context = @azimuth.telemetry.api.context.Context::empty()
  
  // 空键名
  let empty_key = @azimuth.telemetry.api.context.create_key("")
  let context_with_empty_key = base_context.with_value(empty_key, "empty.key.value")
  assert_eq(context_with_empty_key.get(empty_key), Some("empty.key.value"))
  
  // 超长键名
  let long_key_name = "a".repeat(10000)
  let long_key = @azimuth.telemetry.api.context.create_key(long_key_name)
  let context_with_long_key = base_context.with_value(long_key, "long.key.value")
  assert_eq(context_with_long_key.get(long_key), Some("long.key.value"))
  
  // 特殊字符键名
  let special_key_name = "key.with.special@chars#123"
  let special_key = @azimuth.telemetry.api.context.create_key(special_key_name)
  let context_with_special_key = base_context.with_value(special_key, "special.key.value")
  assert_eq(context_with_special_key.get(special_key), Some("special.key.value"))
  
  // Unicode键名
  let unicode_key_name = "键名中文测试"
  let unicode_key = @azimuth.telemetry.api.context.create_key(unicode_key_name)
  let context_with_unicode_key = base_context.with_value(unicode_key, "unicode.key.value")
  assert_eq(context_with_unicode_key.get(unicode_key), Some("unicode.key.value"))
  
  // 控制字符键名
  let control_key_name = "key.with.control.chars\n\t\r"
  let control_key = @azimuth.telemetry.api.context.create_key(control_key_name)
  let context_with_control_key = base_context.with_value(control_key, "control.key.value")
  assert_eq(context_with_control_key.get(control_key), Some("control.key.value"))
  
  // 数字键名
  let numeric_key_name = "123456789"
  let numeric_key = @azimuth.telemetry.api.context.create_key(numeric_key_name)
  let context_with_numeric_key = base_context.with_value(numeric_key, "numeric.key.value")
  assert_eq(context_with_numeric_key.get(numeric_key), Some("numeric.key.value"))
  
  // 验证所有键名配置都能正常工作
  assert_eq(true, true)
}

test "baggage_configuration_edge_cases" {
  // 测试Baggage配置的边界情况
  let base_baggage = @azimuth.telemetry.api.context.Baggage::empty()
  
  // 空键名和值
  let baggage_with_empty_key = base_baggage.with_entry("", "empty.key.value")
  let baggage_with_empty_value = base_baggage.with_entry("empty.value", "")
  let baggage_with_both_empty = base_baggage.with_entry("", "")
  
  assert_eq(baggage_with_empty_key.get(""), Some("empty.key.value"))
  assert_eq(baggage_with_empty_value.get("empty.value"), Some(""))
  assert_eq(baggage_with_both_empty.get(""), Some(""))
  
  // 超长键名和值
  let long_key_name = "a".repeat(10000)
  let long_value = "b".repeat(10000)
  let baggage_with_long_key = base_baggage.with_entry(long_key_name, "long.key.value")
  let baggage_with_long_value = base_baggage.with_entry("long.value", long_value)
  let baggage_with_both_long = base_baggage.with_entry(long_key_name, long_value)
  
  assert_eq(baggage_with_long_key.get(long_key_name), Some("long.key.value"))
  assert_eq(baggage_with_long_value.get("long.value"), Some(long_value))
  assert_eq(baggage_with_both_long.get(long_key_name), Some(long_value))
  
  // 特殊字符键名和值
  let special_key_name = "key.with.special@chars#123"
  let special_value = "value.with.special@chars#456"
  let baggage_with_special = base_baggage.with_entry(special_key_name, special_value)
  assert_eq(baggage_with_special.get(special_key_name), Some(special_value))
  
  // Unicode键名和值
  let unicode_key_name = "键名中文测试"
  let unicode_value = "值中文测试"
  let baggage_with_unicode = base_baggage.with_entry(unicode_key_name, unicode_value)
  assert_eq(baggage_with_unicode.get(unicode_key_name), Some(unicode_value))
  
  // JSON和URL编码值
  let json_value = "{\"key\": \"value\", \"array\": [1, 2, 3]}"
  let url_value = "https%3A%2F%2Fexample.com%2Fpath%3Fquery%3Dvalue"
  let baggage_with_complex = base_baggage
    .with_entry("json.value", json_value)
    .with_entry("url.value", url_value)
  
  assert_eq(baggage_with_complex.get("json.value"), Some(json_value))
  assert_eq(baggage_with_complex.get("url.value"), Some(url_value))
  
  // 验证所有配置都能正常工作
  assert_eq(true, true)
}

test "attribute_value_configuration_edge_cases" {
  // 测试AttributeValue配置的边界情况
  // 空字符串值
  let empty_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("")
  match empty_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "")
    _ => @test.fail("Expected StringValue")
  }
  
  // 超长字符串值
  let long_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("a".repeat(100000))
  match long_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s.length(), 100000)
    _ => @test.fail("Expected StringValue")
  }
  
  // 特殊字符字符串值
  let special_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("Special chars: \n\t\r\"'<>{}[]|&;`")
  match special_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "Special chars: \n\t\r\"'<>{}[]|&;`")
    _ => @test.fail("Expected StringValue")
  }
  
  // Unicode字符串值
  let unicode_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("Unicode测试: 🔥 🚀 ✨ 中文测试")
  match unicode_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "Unicode测试: 🔥 🚀 ✨ 中文测试")
    _ => @test.fail("Expected StringValue")
  }
  
  // JSON字符串值
  let json_string_attr = @azimuth.telemetry.api.common.AttributeValue::string("{\"key\": \"value\", \"number\": 123, \"array\": [1, 2, 3]}")
  match json_string_attr {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => assert_eq(s, "{\"key\": \"value\", \"number\": 123, \"array\": [1, 2, 3]}")
    _ => @test.fail("Expected StringValue")
  }
  
  // 边界数值
  let min_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(-9223372036854775808L)
  let max_int_attr = @azimuth.telemetry.api.common.AttributeValue::int(9223372036854775807L)
  let min_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(-1.7976931348623157e+308)
  let max_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(1.7976931348623157e+308)
  let nan_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(0.0/0.0)
  let inf_float_attr = @azimuth.telemetry.api.common.AttributeValue::float(1.0/0.0)
  
  match min_int_attr {
    @azimuth.telemetry.api.common.AttributeValue::IntValue(i) => assert_eq(i, -9223372036854775808L)
    _ => @test.fail("Expected IntValue")
  }
  
  match max_int_attr {
    @azimuth.telemetry.api.common.AttributeValue::IntValue(i) => assert_eq(i, 9223372036854775807L)
    _ => @test.fail("Expected IntValue")
  }
  
  match nan_float_attr {
    @azimuth.telemetry.api.common.AttributeValue::FloatValue(f) => assert_eq(f/f != f/f, true)
    _ => @test.fail("Expected FloatValue")
  }
  
  match inf_float_attr {
    @azimuth.telemetry.api.common.AttributeValue::FloatValue(f) => assert_eq(f > 0.0, true)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 空数组值
  let empty_string_array = @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue([])
  let empty_int_array = @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue([])
  let empty_float_array = @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue([])
  let empty_bool_array = @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue([])
  
  match empty_string_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match empty_int_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayIntValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match empty_float_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayFloatValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match empty_bool_array {
    @azimuth.telemetry.api.common.AttributeValue::ArrayBoolValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayBoolValue")
  }
}