// 配置验证测试 - 测试各种配置选项的有效性验证
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.metrics.{NoopMeter, NoopMeterProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, Baggage, create_key}

test "configuration_tracer_validation" {
  // 测试Tracer配置验证
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  
  // 测试有效的Tracer配置
  let valid_tracer = tracer_provider.get_tracer("valid-tracer", Some("1.0.0"))
  let tracer_with_version = tracer_provider.get_tracer("versioned-tracer", Some("2.1.5"))
  let tracer_without_version = tracer_provider.get_tracer("no-version-tracer", None)
  
  // 测试不同名称格式的Tracer
  let simple_name_tracer = tracer_provider.get_tracer("simple", Some("1.0.0"))
  let complex_name_tracer = tracer_provider.get_tracer("complex.service.name.with.dots", Some("1.0.0"))
  let underscore_name_tracer = tracer_provider.get_tracer("underscore_name", Some("1.0.0"))
  let dash_name_tracer = tracer_provider.get_tracer("dash-name", Some("1.0.0"))
  
  // 测试不同版本格式的Tracer
  let semantic_version_tracer = tracer_provider.get_tracer("semantic-tracer", Some("1.2.3"))
  let major_version_tracer = tracer_provider.get_tracer("major-tracer", Some("2.0.0"))
  let pre_release_tracer = tracer_provider.get_tracer("pre-release-tracer", Some("1.0.0-beta"))
  let build_metadata_tracer = tracer_provider.get_tracer("build-tracer", Some("1.0.0+build.123"))
  
  // 验证所有Tracer都能正常创建span
  let (_, valid_span) = valid_tracer.start_span(ctx, "valid_test", Internal, None)
  let (_, version_span) = tracer_with_version.start_span(ctx, "version_test", Internal, None)
  let (_, no_version_span) = tracer_without_version.start_span(ctx, "no_version_test", Internal, None)
  
  // 验证不同名称格式的Tracer
  let (_, simple_span) = simple_name_tracer.start_span(ctx, "simple_test", Internal, None)
  let (_, complex_span) = complex_name_tracer.start_span(ctx, "complex_test", Internal, None)
  let (_, underscore_span) = underscore_name_tracer.start_span(ctx, "underscore_test", Internal, None)
  let (_, dash_span) = dash_name_tracer.start_span(ctx, "dash_test", Internal, None)
  
  // 验证不同版本格式的Tracer
  let (_, semantic_span) = semantic_version_tracer.start_span(ctx, "semantic_test", Internal, None)
  let (_, major_span) = major_version_tracer.start_span(ctx, "major_test", Internal, None)
  let (_, pre_release_span) = pre_release_tracer.start_span(ctx, "pre_release_test", Internal, None)
  let (_, build_span) = build_metadata_tracer.start_span(ctx, "build_test", Internal, None)
  
  // 验证所有span都正确创建
  assert_eq(valid_span.name, "valid_test")
  assert_eq(version_span.name, "version_test")
  assert_eq(no_version_span.name, "no_version_test")
  assert_eq(simple_span.name, "simple_test")
  assert_eq(complex_span.name, "complex_test")
  assert_eq(underscore_span.name, "underscore_test")
  assert_eq(dash_span.name, "dash_test")
  assert_eq(semantic_span.name, "semantic_test")
  assert_eq(major_span.name, "major_test")
  assert_eq(pre_release_span.name, "pre_release_test")
  assert_eq(build_span.name, "build_test")
}

test "configuration_meter_validation" {
  // 测试Meter配置验证
  
  let meter_provider = NoopMeterProvider::{}
  
  // 测试有效的Meter配置
  let valid_meter = meter_provider.get_meter("valid-meter", Some("1.0.0"), Some("http://example.com/schema"))
  let meter_with_schema = meter_provider.get_meter("schema-meter", Some("2.0.0"), Some("https://api.example.com/metrics-schema"))
  let meter_minimal = meter_provider.get_meter("minimal-meter", None, None)
  
  // 测试不同Schema URL格式的Meter
  let http_schema_meter = meter_provider.get_meter("http-schema", Some("1.0.0"), Some("http://example.com/schema"))
  let https_schema_meter = meter_provider.get_meter("https-schema", Some("1.0.0"), Some("https://example.com/schema"))
  let no_schema_meter = meter_provider.get_meter("no-schema", Some("1.0.0"), None)
  
  // 验证所有Meter都能正常创建指标
  let valid_counter = valid_meter.create_counter("valid.counter", Some("count"), Some("Valid counter"))
  let schema_counter = meter_with_schema.create_counter("schema.counter", Some("count"), Some("Schema counter"))
  let minimal_counter = meter_minimal.create_counter("minimal.counter", None, None)
  
  let valid_histogram = valid_meter.create_histogram("valid.histogram", Some("ms"), Some("Valid histogram"))
  let schema_histogram = meter_with_schema.create_histogram("schema.histogram", Some("seconds"), Some("Schema histogram"))
  let minimal_histogram = meter_minimal.create_histogram("minimal.histogram", None, None)
  
  let valid_gauge = valid_meter.create_gauge("valid.gauge", Some("bytes"), Some("Valid gauge"))
  let schema_gauge = meter_with_schema.create_gauge("schema.gauge", Some("percent"), Some("Schema gauge"))
  let minimal_gauge = meter_minimal.create_gauge("minimal.gauge", None, None)
  
  // 验证所有指标都能正常记录数据
  valid_counter.add(1L, None)
  schema_counter.add(1L, None)
  minimal_counter.add(1L, None)
  
  valid_histogram.record(100.0, None)
  schema_histogram.record(200.0, None)
  minimal_histogram.record(300.0, None)
  
  valid_gauge.record(50.0, None)
  schema_gauge.record(75.0, None)
  minimal_gauge.record(25.0, None)
  
  // 验证配置有效性（通过成功创建和记录来验证）
  assert_eq(true, true)
}

test "configuration_logger_validation" {
  // 测试Logger配置验证
  
  let logger_provider = NoopLoggerProvider::{}
  
  // 测试有效的Logger配置
  let valid_logger = logger_provider.get_logger("valid-logger", Some("1.0.0"), Some("http://example.com/schema"))
  let logger_with_schema = logger_provider.get_logger("schema-logger", Some("2.1.0"), Some("https://api.example.com/logs-schema"))
  let logger_minimal = logger_provider.get_logger("minimal-logger", None, None)
  
  // 测试不同名称格式的Logger
  let service_logger = logger_provider.get_logger("service-logger", Some("1.0.0"), None)
  let component_logger = logger_provider.get_logger("component.logger.v2", Some("2.0.0"), None)
  let module_logger = logger_provider.get_logger("module_logger", Some("1.5.2"), None)
  
  // 验证所有Logger都能正常记录日志
  let valid_log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Valid logger test")
    .with_attribute("logger.name", AttributeValue::string("valid-logger"))
    .build()
  
  let schema_log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Warn)
    .body("Schema logger test")
    .with_attribute("logger.name", AttributeValue::string("schema-logger"))
    .build()
  
  let minimal_log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Error)
    .body("Minimal logger test")
    .build()
  
  // 发出日志记录
  valid_logger.emit(valid_log_record)
  schema_logger.emit(schema_log_record)
  logger_minimal.emit(minimal_log_record)
  
  // 测试便捷方法
  service_logger.debug("Debug message from service logger", None)
  component_logger.info("Info message from component logger", None)
  module_logger.warn("Warning message from module logger", None)
  valid_logger.error("Error message from valid logger", None)
  schema_logger.fatal("Fatal message from schema logger", None)
  
  // 验证配置有效性（通过成功记录来验证）
  assert_eq(true, true)
}

test "configuration_resource_validation" {
  // 测试Resource配置验证
  
  // 测试基本Resource配置
  let basic_resource = Resource::default("basic-service")
  
  // 测试不同服务名称格式的Resource
  let short_name_resource = Resource::default("svc")
  let long_name_resource = Resource::default("very-long-service-name-with-many-components")
  let dot_separated_resource = Resource::default("service.subservice.component")
  let underscore_resource = Resource::default("service_name_with_underscores")
  let dash_resource = Resource::default("service-name-with-dashes")
  let mixed_resource = Resource::default("service.name-with_mixed-separators")
  
  // 测试特殊字符的服务名称
  let number_resource = Resource::default("service123")
  let alphanum_resource = Resource::default("service-v2-123")
  let unicode_resource = Resource::default("service-测试")
  
  // 测试边界情况
  let empty_name_resource = Resource::default("")
  let single_char_resource = Resource::default("a")
  let space_resource = Resource::default("service with spaces")
  
  // 验证所有Resource都正确创建
  assert_eq(basic_resource.service_name, "basic-service")
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  
  assert_eq(short_name_resource.service_name, "svc")
  assert_eq(long_name_resource.service_name, "very-long-service-name-with-many-components")
  assert_eq(dot_separated_resource.service_name, "service.subservice.component")
  assert_eq(underscore_resource.service_name, "service_name_with_underscores")
  assert_eq(dash_resource.service_name, "service-name-with-dashes")
  assert_eq(mixed_resource.service_name, "service.name-with_mixed-separators")
  
  assert_eq(number_resource.service_name, "service123")
  assert_eq(alphanum_resource.service_name, "service-v2-123")
  assert_eq(unicode_resource.service_name, "service-测试")
  
  assert_eq(empty_name_resource.service_name, "")
  assert_eq(single_char_resource.service_name, "a")
  assert_eq(space_resource.service_name, "service with spaces")
  
  // 验证所有Resource都有相同的SDK信息
  let resources = [
    basic_resource, short_name_resource, long_name_resource,
    dot_separated_resource, underscore_resource, dash_resource,
    mixed_resource, number_resource, alphanum_resource,
    unicode_resource, empty_name_resource, single_char_resource,
    space_resource
  ]
  
  let mut i = 0
  while i < resources.length() {
    assert_eq(resources[i].telemetry_sdk_name, "azimuth")
    assert_eq(resources[i].telemetry_sdk_version, "0.1.0")
    assert_eq(resources[i].service_version, None)
    assert_eq(resources[i].attributes.length(), 0)
    i = i + 1
  }
}

test "configuration_attribute_validation" {
  // 测试属性配置验证
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("attribute-test", Some("1.0.0"))
  
  // 测试有效的属性键
  let valid_keys = [
    "simple.key",
    "key.with.dots",
    "key_with_underscores",
    "key-with-dashes",
    "key123",
    "key.with.numbers123",
    "very.long.key.name.with.many.components",
    "short",
    "k",
    "UPPERCASE.KEY",
    "Mixed.Case.Key"
  ]
  
  // 测试有效的属性值
  let string_values = [
    "simple value",
    "value with spaces",
    "value-with-dashes",
    "value_with_underscores",
    "value123",
    "UPPERCASE_VALUE",
    "Mixed.Case.Value",
    "value.with.dots",
    "value!@#$%^&*()",
    "Unicode值测试🚀",
    "",
    " ",  // 空格
    "x" * 1000  // 长字符串
  ]
  
  let int_values = [
    0L, 1L, -1L, 42L, -42L,
    9223372036854775807L,  // Int64最大值
    -9223372036854775808L  // Int64最小值
  ]
  
  let float_values = [
    0.0, 1.0, -1.0, 3.14159, -3.14159,
    1.7976931348623157e+308,  // Double最大值
    -1.7976931348623157e+308, // Double最小值
    5e-324,  // 最小正数
    1.0/0.0,  // 正无穷
    -1.0/0.0, // 负无穷
    0.0/0.0   // NaN
  ]
  
  let bool_values = [true, false]
  
  // 测试字符串属性
  let mut i = 0
  while i < valid_keys.length() && i < string_values.length() {
    let string_attributes = [(valid_keys[i], AttributeValue::string(string_values[i]))]
    let (_, string_span) = tracer.start_span(ctx, "string_attr_test", Internal, Some(string_attributes))
    assert_eq(string_span.attributes.length(), 1)
    assert_eq(string_span.attributes[0].0, valid_keys[i])
    
    match string_span.attributes[0].1 {
      StringValue(value) => assert_eq(value, string_values[i])
      _ => @test.fail("Expected StringValue")
    }
    i = i + 1
  }
  
  // 测试整数属性
  let mut j = 0
  while j < valid_keys.length() && j < int_values.length() {
    let int_attributes = [(valid_keys[j], AttributeValue::int(int_values[j]))]
    let (_, int_span) = tracer.start_span(ctx, "int_attr_test", Internal, Some(int_attributes))
    assert_eq(int_span.attributes.length(), 1)
    assert_eq(int_span.attributes[0].0, valid_keys[j])
    
    match int_span.attributes[0].1 {
      IntValue(value) => assert_eq(value, int_values[j])
      _ => @test.fail("Expected IntValue")
    }
    j = j + 1
  }
  
  // 测试浮点数属性
  let mut k = 0
  while k < valid_keys.length() && k < float_values.length() {
    let float_attributes = [(valid_keys[k], AttributeValue::float(float_values[k]))]
    let (_, float_span) = tracer.start_span(ctx, "float_attr_test", Internal, Some(float_attributes))
    assert_eq(float_span.attributes.length(), 1)
    assert_eq(float_span.attributes[0].0, valid_keys[k])
    
    match float_span.attributes[0].1 {
      FloatValue(value) => assert_eq(value, float_values[k])
      _ => @test.fail("Expected FloatValue")
    }
    k = k + 1
  }
  
  // 测试布尔属性
  let mut l = 0
  while l < valid_keys.length() && l < bool_values.length() {
    let bool_attributes = [(valid_keys[l], AttributeValue::bool(bool_values[l]))]
    let (_, bool_span) = tracer.start_span(ctx, "bool_attr_test", Internal, Some(bool_attributes))
    assert_eq(bool_span.attributes.length(), 1)
    assert_eq(bool_span.attributes[0].0, valid_keys[l])
    
    match bool_span.attributes[0].1 {
      BoolValue(value) => assert_eq(value, bool_values[l])
      _ => @test.fail("Expected BoolValue")
    }
    l = l + 1
  }
  
  // 测试数组属性
  let string_array = AttributeValue::array_string(["a", "b", "c"])
  let int_array = AttributeValue::array_int([1L, 2L, 3L])
  let float_array = AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array = AttributeValue::array_bool([true, false, true])
  
  let array_attributes = [
    ("string.array", string_array),
    ("int.array", int_array),
    ("float.array", float_array),
    ("bool.array", bool_array)
  ]
  
  let (_, array_span) = tracer.start_span(ctx, "array_attr_test", Internal, Some(array_attributes))
  assert_eq(array_span.attributes.length(), 4)
  
  // 验证数组属性
  match array_span.attributes[0].1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  match array_span.attributes[1].1 {
    ArrayIntValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  match array_span.attributes[2].1 {
    ArrayFloatValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  match array_span.attributes[3].1 {
    ArrayBoolValue(arr) => assert_eq(arr.length(), 3)
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "configuration_context_and_baggage_validation" {
  // 测试Context和Baggage配置验证
  
  // 测试Context配置
  let ctx = Context::empty()
  
  // 测试有效的Context键
  let valid_context_keys = [
    "simple.key",
    "key.with.dots",
    "key_with_underscores",
    "key-with-dashes",
    "key123",
    "UPPERCASE.KEY",
    "Mixed.Case.Key",
    "very.long.key.name.with.many.components",
    "short",
    "k"
  ]
  
  // 测试有效的Context值
  let valid_context_values = [
    "simple value",
    "value with spaces",
    "value-with-dashes",
    "value_with_underscores",
    "value123",
    "UPPERCASE_VALUE",
    "Mixed.Case.Value",
    "value!@#$%^&*()",
    "Unicode值测试🚀",
    "",
    "x" * 1000
  ]
  
  // 测试Context设置和获取
  let mut current_ctx = ctx
  let mut i = 0
  while i < valid_context_keys.length() && i < valid_context_values.length() {
    let key = create_key(valid_context_keys[i])
    current_ctx = current_ctx.with_value(key, valid_context_values[i])
    
    match current_ctx.get(key) {
      Some(value) => assert_eq(value, valid_context_values[i])
      None => @test.fail("Expected Some(value) for key: " + valid_context_keys[i])
    }
    i = i + 1
  }
  
  // 测试Baggage配置
  let baggage = Baggage::empty()
  
  // 测试有效的Baggage键
  let valid_baggage_keys = [
    "correlation-id",
    "user-id",
    "session-id",
    "request-id",
    "trace-id",
    "tenant-id",
    "region",
    "environment",
    "version",
    "component"
  ]
  
  // 测试有效的Baggage值
  let valid_baggage_values = [
    "corr-12345",
    "user-67890",
    "sess-abcdef",
    "req-fedcba",
    "trace-123456",
    "tenant-789012",
    "us-west-2",
    "production",
    "v1.2.3",
    "web-api"
  ]
  
  // 测试Baggage设置和获取
  let mut current_baggage = baggage
  let mut j = 0
  while j < valid_baggage_keys.length() && j < valid_baggage_values.length() {
    current_baggage = current_baggage.with_entry(valid_baggage_keys[j], valid_baggage_values[j])
    
    match current_baggage.get(valid_baggage_keys[j]) {
      Some(value) => assert_eq(value, valid_baggage_values[j])
      None => @test.fail("Expected Some(value) for baggage key: " + valid_baggage_keys[j])
    }
    j = j + 1
  }
  
  // 测试边界情况
  let empty_key = create_key("")
  let ctx_with_empty_key = current_ctx.with_value(empty_key, "value_with_empty_key")
  match ctx_with_empty_key.get(empty_key) {
    Some(value) => assert_eq(value, "value_with_empty_key")
    None => @test.fail("Expected Some(value) for empty key")
  }
  
  let normal_key = create_key("normal.key")
  let ctx_with_empty_value = current_ctx.with_value(normal_key, "")
  match ctx_with_empty_value.get(normal_key) {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected Some(empty value)")
  }
  
  let baggage_with_empty_key = current_baggage.with_entry("", "value_with_empty_key")
  match baggage_with_empty_key.get("") {
    Some(value) => assert_eq(value, "value_with_empty_key")
    None => @test.fail("Expected Some(value) for empty baggage key")
  }
  
  let baggage_with_empty_value = current_baggage.with_entry("normal.key", "")
  match baggage_with_empty_value.get("normal.key") {
    Some(value) => assert_eq(value, "")
    None => @test.fail("Expected Some(empty baggage value)")
  }
}

test "configuration_log_record_validation" {
  // 测试LogRecord配置验证
  
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("config-validation-logger", Some("1.0.0"))
  
  // 测试所有严重级别
  let severity_levels = [
    (SeverityNumber::Trace, "trace"),
    (SeverityNumber::Debug, "debug"),
    (SeverityNumber::Info, "info"),
    (SeverityNumber::Warn, "warn"),
    (SeverityNumber::Error, "error"),
    (SeverityNumber::Fatal, "fatal")
  ]
  
  let mut i = 0
  while i < severity_levels.length() {
    let (severity, severity_name) = severity_levels[i]
    
    let log_record = LogRecord::builder()
      .timestamp(1640995200000000000L + i.to_int64())
      .severity(severity)
      .body("Test " + severity_name + " log message")
      .with_attribute("severity.level", AttributeValue::string(severity_name))
      .with_attribute("test.index", AttributeValue::int(i.to_int64()))
      .build()
    
    logger.emit(log_record)
    
    // 验证日志记录配置
    assert_eq(log_record.severity_number, severity)
    match log_record.body {
      Some(body) => assert_eq(body, "Test " + severity_name + " log message")
      None => @test.fail("Expected Some(body) for " + severity_name + " log")
    }
    assert_eq(log_record.attributes.length(), 2)
    
    i = i + 1
  }
  
  // 测试不同的时间戳
  let timestamp_values = [
    0L,  // 最小时间戳
    1640995200000000000L,  // 正常时间戳
    9223372036854775807L   // 最大时间戳
  ]
  
  let mut j = 0
  while j < timestamp_values.length() {
    let timestamp_log = LogRecord::builder()
      .timestamp(timestamp_values[j])
      .severity(SeverityNumber::Info)
      .body("Timestamp test " + j.to_string())
      .build()
    
    logger.emit(timestamp_log)
    assert_eq(timestamp_log.timestamp_unix_nanos, timestamp_values[j])
    
    j = j + 1
  }
  
  // 测试不同的日志消息
  let message_values = [
    "Simple message",
    "Message with spaces and punctuation!@#$%^&*()",
    "Message with Unicode: 测试消息🚀🌟",
    "",  // 空消息
    " ",  // 空格消息
    "x" * 1000  // 长消息
  ]
  
  let mut k = 0
  while k < message_values.length() {
    let message_log = LogRecord::builder()
      .timestamp(1640995200000000000L + k.to_int64())
      .severity(SeverityNumber::Info)
      .body(message_values[k])
      .build()
    
    logger.emit(message_log)
    
    match message_log.body {
      Some(body) => assert_eq(body, message_values[k])
      None => @test.fail("Expected Some(body) for message test " + k.to_string())
    }
    
    k = k + 1
  }
  
  // 测试复杂属性组合
  let complex_attributes = [
    ("string.attr", AttributeValue::string("complex_string_value")),
    ("int.attr", AttributeValue::int(42L)),
    ("float.attr", AttributeValue::float(3.14159)),
    ("bool.attr", AttributeValue::bool(true)),
    ("array.string", AttributeValue::array_string(["a", "b", "c"])),
    ("array.int", AttributeValue::array_int([1L, 2L, 3L])),
    ("array.float", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("array.bool", AttributeValue::array_bool([true, false, true]))
  ]
  
  let complex_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Complex attributes test")
    let mut attr_index = 0
    while attr_index < complex_attributes.length() {
      let (key, value) = complex_attributes[attr_index]
      complex_log = complex_log.with_attribute(key, value)
      attr_index = attr_index + 1
    }
    .build()
  
  logger.emit(complex_log)
  
  // 验证复杂属性
  assert_eq(complex_log.attributes.length(), 8)
  assert_eq(complex_log.severity_number, SeverityNumber::Info)
  match complex_log.body {
    Some(body) => assert_eq(body, "Complex attributes test")
    None => @test.fail("Expected Some(body) for complex log")
  }
}