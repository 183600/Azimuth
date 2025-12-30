// 遥测系统配置管理测试
// 测试各种配置选项、动态更新和验证机制

test "resource_configuration_management" {
  // 测试资源配置管理
  
  // 测试默认资源配置
  let default_resource = common::Resource::default("test_service")
  assert_eq(default_resource.service_name, "test_service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  
  // 测试服务配置更新
  let configured_resource = common::Resource::{
    service_name: "production_service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("datacenter", common::AttributeValue::string("dc-01"))
    ]
  }
  
  assert_eq(configured_resource.service_name, "production_service")
  assert_eq(configured_resource.service_version.unwrap(), "2.1.0")
  assert_eq(configured_resource.attributes.length(), 3)
  
  // 测试配置继承
  let inherited_resource = common::Resource::{
    ..configured_resource,
    service_name: "derived_service",
    service_version: Some("2.1.1"),
    attributes: configured_resource.attributes + [
      ("instance_id", common::AttributeValue::string("i-1234567890abcdef0"))
    ]
  }
  
  assert_eq(inherited_resource.service_name, "derived_service")
  assert_eq(inherited_resource.service_version.unwrap(), "2.1.1")
  assert_eq(inherited_resource.telemetry_sdk_name, "azimuth")  // 继承的属性
  assert_eq(inherited_resource.attributes.length(), 4)  // 3个继承 + 1个新增
  
  // 测试环境特定配置
  let dev_config = common::Resource::default("dev_service")
  let staging_config = common::Resource::{
    ..dev_config,
    service_name: "staging_service",
    attributes: [
      ("environment", common::AttributeValue::string("staging")),
      ("debug", common::AttributeValue::bool(true))
    ]
  }
  let prod_config = common::Resource::{
    ..dev_config,
    service_name: "prod_service",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("debug", common::AttributeValue::bool(false)),
      ("monitoring", common::AttributeValue::bool(true))
    ]
  }
  
  assert_eq(dev_config.service_name, "dev_service")
  assert_eq(staging_config.service_name, "staging_service")
  assert_eq(prod_config.service_name, "prod_service")
  assert_eq(staging_config.attributes.length(), 2)
  assert_eq(prod_config.attributes.length(), 3)
}

test "instrumentation_scope_configuration" {
  // 测试instrumentation scope配置
  
  // 测试基础scope配置
  let basic_scope = common::InstrumentationScope {
    name: "basic_instrumentation",
    version: None,
    schema_url: None
  }
  
  // 测试完整scope配置
  let full_scope = common::InstrumentationScope {
    name: "full_instrumentation",
    version: Some("1.2.3"),
    schema_url: Some("http://example.com/schema/v1")
  }
  
  // 测试scope配置变体
  let scopes = [
    basic_scope,
    full_scope,
    common::InstrumentationScope {
      name: "version_only",
      version: Some("2.0.0"),
      schema_url: None
    },
    common::InstrumentationScope {
      name: "schema_only",
      version: None,
      schema_url: Some("https://example.org/schema")
    }
  ]
  
  // 验证配置差异
  assert_eq(scopes[0].name, "basic_instrumentation")
  assert_eq(scopes[0].version, None)
  assert_eq(scopes[0].schema_url, None)
  
  assert_eq(scopes[1].name, "full_instrumentation")
  assert_eq(scopes[1].version.unwrap(), "1.2.3")
  assert_eq(scopes[1].schema_url.unwrap(), "http://example.com/schema/v1")
  
  assert_eq(scopes[2].name, "version_only")
  assert_eq(scopes[2].version.unwrap(), "2.0.0")
  assert_eq(scopes[2].schema_url, None)
  
  assert_eq(scopes[3].name, "schema_only")
  assert_eq(scopes[3].version, None)
  assert_eq(scopes[3].schema_url.unwrap(), "https://example.org/schema")
}

test "tracer_provider_configuration" {
  // 测试TracerProvider配置
  
  // 测试noop tracer provider配置
  let noop_provider = trace::NoopTracerProvider::{}
  
  // 测试获取不同配置的tracer
  let default_tracer = noop_provider.get_tracer("default_tracer")
  let versioned_tracer = noop_provider.get_tracer("versioned_tracer", "1.0.0")
  let full_configured_tracer = noop_provider.get_tracer("full_tracer", "2.1.0")
  
  // 验证tracer创建（由于是noop实现，主要验证不会崩溃）
  let base_context = context::Context::empty()
  
  let (_, default_span) = default_tracer.start_span(base_context, "default_operation")
  let (_, versioned_span) = versioned_tracer.start_span(base_context, "versioned_operation")
  let (_, full_span) = full_configured_tracer.start_span(base_context, "full_operation")
  
  assert_eq(default_span.name, "default_operation")
  assert_eq(versioned_span.name, "versioned_operation")
  assert_eq(full_span.name, "full_operation")
  
  // 测试不同span kind配置
  let span_kinds = [
    trace::Internal,
    trace::Server,
    trace::Client,
    trace::Producer,
    trace::Consumer
  ]
  
  let mut i = 0
  while i < span_kinds.length() {
    let kind = span_kinds[i]
    let (_, span) = default_tracer.start_span(
      base_context,
      "kind_test_" + i.to_string(),
      kind
    )
    assert_eq(span.kind, kind)
    i = i + 1
  }
}

test "meter_provider_configuration" {
  // 测试MeterProvider配置
  
  // 测试noop meter provider配置
  let noop_provider = metrics::NoopMeterProvider::{}
  
  // 测试获取不同配置的meter
  let default_meter = noop_provider.get_meter("default_meter")
  let versioned_meter = noop_provider.get_meter("versioned_meter", "1.0.0")
  let full_configured_meter = noop_provider.get_meter(
    "full_meter", 
    "2.1.0", 
    "http://example.com/schema"
  )
  
  // 测试创建不同类型的仪器
  let default_counter = default_meter.create_counter("default_counter", "count", "Default counter")
  let default_histogram = default_meter.create_histogram("default_histogram", "ms", "Default histogram")
  let default_gauge = default_meter.create_gauge("default_gauge", "value", "Default gauge")
  
  let versioned_counter = versioned_meter.create_counter("versioned_counter", "", "")
  let versioned_histogram = versioned_meter.create_histogram("versioned_histogram", "", "")
  let versioned_gauge = versioned_meter.create_gauge("versioned_gauge", "", "")
  
  let full_counter = full_configured_meter.create_counter("full_counter", "operations", "Full counter with description")
  let full_histogram = full_configured_meter.create_histogram("full_histogram", "seconds", "Full histogram with description")
  let full_gauge = full_configured_meter.create_gauge("full_gauge", "connections", "Full gauge with description")
  
  // 验证仪器创建（主要验证不会崩溃）
  default_counter.add(1L)
  default_histogram.record(100.0)
  default_gauge.record(50.0)
  
  versioned_counter.add(1L)
  versioned_histogram.record(100.0)
  versioned_gauge.record(50.0)
  
  full_counter.add(1L)
  full_histogram.record(100.0)
  full_gauge.record(50.0)
}

test "logger_provider_configuration" {
  // 测试LoggerProvider配置
  
  // 测试noop logger provider配置
  let noop_provider = logs::NoopLoggerProvider::{}
  
  // 测试获取不同配置的logger
  let default_logger = noop_provider.get_logger("default_logger")
  let versioned_logger = noop_provider.get_logger("versioned_logger", "1.0.0")
  let full_configured_logger = noop_provider.get_logger(
    "full_logger", 
    "2.1.0", 
    "http://example.com/schema"
  )
  
  // 测试不同日志级别
  let test_messages = [
    ("Debug message", fn(logger: logs::NoopLogger) { logger.debug("Debug message", []) }),
    ("Info message", fn(logger: logs::NoopLogger) { logger.info("Info message", []) }),
    ("Warning message", fn(logger: logs::NoopLogger) { logger.warn("Warning message", []) }),
    ("Error message", fn(logger: logs::NoopLogger) { logger.error("Error message", []) }),
    ("Fatal message", fn(logger: logs::NoopLogger) { logger.fatal("Fatal message", []) })
  ]
  
  // 测试所有logger配置
  let loggers = [default_logger, versioned_logger, full_configured_logger]
  let mut i = 0
  while i < loggers.length() {
    let logger = loggers[i]
    let mut j = 0
    while j < test_messages.length() {
      let (_, log_function) = test_messages[j]
      log_function(logger)
      j = j + 1
    }
    i = i + 1
  }
  
  // 测试带属性的日志记录
  let log_attributes = [
    ("user_id", common::AttributeValue::string("user123")),
    ("session_id", common::AttributeValue::string("session456")),
    ("request_id", common::AttributeValue::string("request789"))
  ]
  
  default_logger.info("Test with attributes", log_attributes)
  versioned_logger.warn("Warning with attributes", log_attributes)
  full_configured_logger.error("Error with attributes", log_attributes)
}

test "context_configuration_options" {
  // 测试上下文配置选项
  
  // 测试空上下文配置
  let empty_context = context::Context::empty()
  assert_eq(empty_context.values.length(), 0)
  
  // 测试预配置键
  let common_keys = [
    context::create_key("trace_id"),
    context::create_key("span_id"),
    context::create_key("user_id"),
    context::create_key("session_id"),
    context::create_key("request_id"),
    context::create_key("correlation_id")
  ]
  
  // 测试上下文预配置
  let preconfigured_context = empty_context
    .with_value(common_keys[0], "trace123456")
    .with_value(common_keys[1], "span789012")
    .with_value(common_keys[2], "user345678")
    .with_value(common_keys[3], "session901234")
    .with_value(common_keys[4], "request567890")
    .with_value(common_keys[5], "correlation123456")
  
  // 验证预配置值
  assert_eq(preconfigured_context.values.length(), 6)
  assert_eq(preconfigured_context.get(common_keys[0]).unwrap(), "trace123456")
  assert_eq(preconfigured_context.get(common_keys[1]).unwrap(), "span789012")
  assert_eq(preconfigured_context.get(common_keys[2]).unwrap(), "user345678")
  assert_eq(preconfigured_context.get(common_keys[3]).unwrap(), "session901234")
  assert_eq(preconfigured_context.get(common_keys[4]).unwrap(), "request567890")
  assert_eq(preconfigured_context.get(common_keys[5]).unwrap(), "correlation123456")
  
  // 测试上下文配置模板
  let create_user_context = fn(user_id: String, session_id: String) {
    empty_context
      .with_value(context::create_key("user_id"), user_id)
      .with_value(context::create_key("session_id"), session_id)
      .with_value(context::create_key("timestamp"), "1234567890")
  }
  
  let user_context1 = create_user_context("user123", "session456")
  let user_context2 = create_user_context("user789", "session012")
  
  assert_eq(user_context1.get(context::create_key("user_id")).unwrap(), "user123")
  assert_eq(user_context1.get(context::create_key("session_id")).unwrap(), "session456")
  assert_eq(user_context2.get(context::create_key("user_id")).unwrap(), "user789")
  assert_eq(user_context2.get(context::create_key("session_id")).unwrap(), "session012")
}

test "baggage_configuration_patterns" {
  // 测试baggage配置模式
  
  // 测试空baggage配置
  let empty_baggage = context::Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 测试标准baggage配置
  let standard_baggage = empty_baggage
    .with_entry("user_id", "12345")
    .with_entry("session_id", "abcdef")
    .with_entry("request_id", "req789")
    .with_entry("correlation_id", "cor123")
  
  assert_eq(standard_baggage.entries.length(), 4)
  
  // 测试分布式追踪baggage配置
  let tracing_baggage = empty_baggage
    .with_entry("trace_id", "0af7651916cd43dd8448eb211c80319c")
    .with_entry("span_id", "b7ad6b7169203331")
    .with_entry("parent_span_id", "a1b2c3d4e5f6g7h8")
    .with_entry("trace_flags", "01")
    .with_entry("trace_state", "rojo=00f067aa0ba902b7")
  
  assert_eq(tracing_baggage.entries.length(), 5)
  
  // 测试业务上下文baggage配置
  let business_baggage = empty_baggage
    .with_entry("tenant_id", "tenant_123")
    .with_entry("organization_id", "org_456")
    .with_entry("project_id", "project_789")
    .with_entry("environment", "production")
    .with_entry("region", "us-west-2")
    .with_entry("version", "1.2.3")
  
  assert_eq(business_baggage.entries.length(), 6)
  
  // 测试baggage配置模板
  let create_tracing_baggage = fn(trace_id: String, span_id: String) {
    empty_baggage
      .with_entry("trace_id", trace_id)
      .with_entry("span_id", span_id)
      .with_entry("trace_flags", "01")
      .with_entry("sampling_decision", "recorded")
  }
  
  let tracing_baggage1 = create_tracing_baggage("trace123", "span456")
  let tracing_baggage2 = create_tracing_baggage("trace789", "span012")
  
  assert_eq(tracing_baggage1.get("trace_id").unwrap(), "trace123")
  assert_eq(tracing_baggage1.get("span_id").unwrap(), "span456")
  assert_eq(tracing_baggage2.get("trace_id").unwrap(), "trace789")
  assert_eq(tracing_baggage2.get("span_id").unwrap(), "span012")
}

test "attribute_configuration_validation" {
  // 测试属性配置验证
  
  // 测试有效属性配置
  let valid_attributes = [
    ("string_key", common::AttributeValue::string("string_value")),
    ("int_key", common::AttributeValue::int(42L)),
    ("float_key", common::AttributeValue::float(3.14)),
    ("bool_key", common::AttributeValue::bool(true)),
    ("string_array_key", common::AttributeValue::array_string(["a", "b", "c"])),
    ("int_array_key", common::AttributeValue::array_int([1L, 2L, 3L])),
    ("float_array_key", common::AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool_array_key", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  // 验证有效属性
  assert_eq(valid_attributes.length(), 8)
  
  // 测试边界属性配置
  let boundary_attributes = [
    ("empty_string", common::AttributeValue::string("")),
    ("max_int", common::AttributeValue::int(9223372036854775807L)),
    ("min_int", common::AttributeValue::int(-9223372036854775808L)),
    ("max_float", common::AttributeValue::float(1.7976931348623157e+308)),
    ("min_float", common::AttributeValue::float(-1.7976931348623157e+308)),
    ("empty_arrays", common::AttributeValue::array_string([]))
  ]
  
  // 验证边界属性
  assert_eq(boundary_attributes.length(), 6)
  
  // 测试特殊字符属性配置
  let special_char_attributes = [
    ("unicode_key", common::AttributeValue::string("测试🚀")),
    ("special_chars", common::AttributeValue::string("!@#$%^&*()")),
    ("newlines", common::AttributeValue::string("line1\nline2")),
    ("tabs", common::AttributeValue::string("col1\tcol2")),
    ("quotes", common::AttributeValue::string("\"quoted\""))
  ]
  
  // 验证特殊字符属性
  assert_eq(special_char_attributes.length(), 5)
  
  // 测试属性配置组合
  let combined_attributes = valid_attributes + boundary_attributes + special_char_attributes
  assert_eq(combined_attributes.length(), 19)
  
  // 验证属性配置在span中的使用
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("attribute_test_tracer")
  let (_, span) = tracer.start_span(
    context::Context::empty(),
    "attribute_test_span",
    trace::Internal,
    combined_attributes
  )
  
  assert_eq(span.attributes.length(), 19)
}