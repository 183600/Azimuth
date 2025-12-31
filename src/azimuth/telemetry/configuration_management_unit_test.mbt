// 配置管理功能测试
// 测试遥测配置、动态配置更新和配置验证

use azimuth.telemetry.api.common
use azimuth.telemetry.api.trace
use azimuth.telemetry.api.metrics
use azimuth.telemetry.api.logs

test "basic_configuration_creation" {
  // 测试基本配置创建
  
  // 创建服务配置
  let service_config = common::Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2")),
      ("availability_zone", common::AttributeValue::string("us-west-2a")),
      ("instance_id", common::AttributeValue::string("i-1234567890abcdef0")),
      ("datacenter", common::AttributeValue::string("dc-west-1"))
    ]
  }
  
  // 验证服务配置
  assert_eq(service_config.service_name, "payment-service")
  assert_eq(service_config.service_version.unwrap(), "2.1.0")
  assert_eq(service_config.telemetry_sdk_name, "azimuth")
  assert_eq(service_config.telemetry_sdk_version, "0.1.0")
  assert_eq(service_config.attributes.length(), 5)
  
  // 验证特定属性
  assert_eq(service_config.attributes[0].0, "environment")
  assert_eq(service_config.attributes[0].1, common::AttributeValue::string("production"))
  
  assert_eq(service_config.attributes[1].0, "region")
  assert_eq(service_config.attributes[1].1, common::AttributeValue::string("us-west-2"))
  
  assert_eq(service_config.attributes[4].0, "datacenter")
  assert_eq(service_config.attributes[4].1, common::AttributeValue::string("dc-west-1"))
  
  // 创建工具范围配置
  let instrumentation_config = common::InstrumentationScope::{
    name: "payment-processor",
    version: Some("2.1.0"),
    schema_url: Some("http://opentelemetry.io/schemas/1.20.0")
  }
  
  // 验证工具范围配置
  assert_eq(instrumentation_config.name, "payment-processor")
  assert_eq(instrumentation_config.version.unwrap(), "2.1.0")
  assert_eq(instrumentation_config.schema_url.unwrap(), "http://opentelemetry.io/schemas/1.20.0")
  
  // 创建最小配置
  let minimal_resource = common::Resource::default("minimal-service")
  let minimal_scope = common::InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  // 验证最小配置
  assert_eq(minimal_resource.service_name, "minimal-service")
  assert_eq(minimal_resource.service_version, None)
  assert_eq(minimal_resource.attributes.length(), 0)
  
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "tracing_configuration" {
  // 测试追踪配置
  
  // 创建追踪提供者配置
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("payment-tracer", "2.1.0")
  
  // 测试采样配置（通过trace_flags模拟）
  let sampled_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 1_byte })
  let sampled_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 1_byte })
  
  let sampled_context = trace::SpanContext::{
    trace_id: sampled_trace_id,
    span_id: sampled_span_id,
    trace_flags: 1_byte, // 采样标志
    trace_state: "vendor1=value1,vendor2=value2"
  }
  
  // 验证采样配置
  assert_eq(sampled_context.trace_flags, 1_byte)
  assert_eq(sampled_context.trace_state.contains("vendor1=value1"), true)
  assert_eq(sampled_context.trace_state.contains("vendor2=value2"), true)
  
  // 创建未采样上下文
  let unsampled_context = trace::SpanContext::{
    trace_id: sampled_trace_id,
    span_id: sampled_span_id,
    trace_flags: 0_byte, // 未采样
    trace_state: ""
  }
  
  // 验证未采样配置
  assert_eq(unsampled_context.trace_flags, 0_byte)
  assert_eq(unsampled_context.trace_state, "")
  
  // 测试复杂trace_state配置
  let complex_trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE,vendor1=complex_value_with_underscores,vendor2=value-with-dashes"
  let complex_context = trace::SpanContext::{
    trace_id: sampled_trace_id,
    span_id: sampled_span_id,
    trace_flags: 1_byte,
    trace_state: complex_trace_state
  }
  
  // 验证复杂trace_state
  assert_eq(complex_context.trace_state, complex_trace_state)
  assert_eq(complex_context.trace_state.contains("rojo=00f067aa0ba902b7"), true)
  assert_eq(complex_context.trace_state.contains("congo=t61rcWkgMzE"), true)
  assert_eq(complex_context.trace_state.contains("vendor1=complex_value_with_underscores"), true)
  assert_eq(complex_context.trace_state.contains("vendor2=value-with-dashes"), true)
}

test "metrics_configuration" {
  // 测试指标配置
  
  let meter_provider = metrics::NoopMeterProvider::{}
  
  // 测试不同Meter配置
  let default_meter = meter_provider.get_meter("default-meter")
  let versioned_meter = meter_provider.get_meter("versioned-meter", "1.2.3")
  let full_config_meter = meter_provider.get_meter("full-config-meter", "2.0.0", "http://example.com/schema")
  
  // 验证Meter配置
  assert_eq(default_meter is metrics::Meter, true)
  assert_eq(versioned_meter is metrics::Meter, true)
  assert_eq(full_config_meter is metrics::Meter, true)
  
  // 创建不同类型的指标配置
  let counter_config = versioned_meter.create_counter("http_requests_total", "count", "Total HTTP requests")
  let histogram_config = versioned_meter.create_histogram("request_duration_seconds", "seconds", "Request duration in seconds")
  let up_down_counter_config = versioned_meter.create_up_down_counter("active_connections", "connections", "Active connections")
  let gauge_config = versioned_meter.create_gauge("cpu_usage_percent", "percent", "CPU usage percentage")
  
  // 验证指标配置
  assert_eq(counter_config is metrics::Counter, true)
  assert_eq(histogram_config is metrics::Histogram, true)
  assert_eq(up_down_counter_config is metrics::UpDownCounter, true)
  assert_eq(gauge_config is metrics::Gauge, true)
  
  // 测试带配置的指标操作
  counter_config.add(10L, [
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("200")),
    ("endpoint", common::AttributeValue::string("/api/payments"))
  ])
  
  histogram_config.record(0.125, [
    ("method", common::AttributeValue::string("POST")),
    ("endpoint", common::AttributeValue::string("/api/process")),
    ("status", common::AttributeValue::string("201"))
  ])
  
  up_down_counter_config.add(5L, [
    ("service", common::AttributeValue::string("payment-gateway")),
    ("region", common::AttributeValue::string("us-east-1"))
  ])
  
  gauge_config.record(75.5, [
    ("instance", common::AttributeValue::string("payment-server-01")),
    ("metric_type", common::AttributeValue::string("cpu"))
  ])
  
  // 验证配置操作不会抛出异常
  assert_eq(true, true)
}

test "logging_configuration" {
  // 测试日志配置
  
  let logger_provider = logs::NoopLoggerProvider::{}
  
  // 测试不同Logger配置
  let default_logger = logger_provider.get_logger("default-logger")
  let versioned_logger = logger_provider.get_logger("versioned-logger", "1.0.0")
  let full_config_logger = logger_provider.get_logger("full-config-logger", "2.1.0", "http://example.com/logs-schema")
  
  // 验证Logger配置
  assert_eq(default_logger is logs::Logger, true)
  assert_eq(versioned_logger is logs::Logger, true)
  assert_eq(full_config_logger is logs::Logger, true)
  
  // 测试不同日志级别配置
  let log_levels = [
    logs::Trace,
    logs::Debug,
    logs::Info,
    logs::Warn,
    logs::Error,
    logs::Fatal
  ]
  
  let log_level_names = ["TRACE", "DEBUG", "INFO", "WARN", "ERROR", "FATAL"]
  
  // 创建不同级别的日志记录
  let mut i = 0
  while i < log_levels.length() {
    let log_record = logs::LogRecord::{
      timestamp_unix_nanos: 1640995200L + i.to_int64(),
      observed_timestamp_unix_nanos: None,
      severity_number: log_levels[i],
      severity_text: Some(log_level_names[i]),
      body: Some("Log message at " + log_level_names[i] + " level"),
      attributes: [
        ("logger.name", common::AttributeValue::string("test-logger")),
        ("logger.version", common::AttributeValue::string("1.0.0")),
        ("level", common::AttributeValue::string(log_level_names[i]))
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: Some(common::Resource::default("test-service")),
      instrumentation_scope: Some(common::InstrumentationScope::{
        name: "test-logger",
        version: Some("1.0.0"),
        schema_url: Some("http://example.com/logs-schema")
      })
    }
    
    // 发送日志记录
    versioned_logger.emit(log_record)
    i = i + 1
  }
  
  // 测试便捷方法配置
  default_logger.debug("Debug message", [("component", common::AttributeValue::string("auth"))])
  versioned_logger.info("Info message", [("component", common::AttributeValue::string("payment"))])
  full_config_logger.warn("Warning message", [("component", common::AttributeValue::string("notification"))])
  default_logger.error("Error message", [("component", common::AttributeValue::string("database"))])
  versioned_logger.fatal("Fatal message", [("component", common::AttributeValue::string("system"))])
  
  // 验证日志配置操作
  assert_eq(true, true)
}

test "environment_based_configuration" {
  // 测试基于环境的配置
  
  // 开发环境配置
  let dev_resource = common::Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0-dev"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("development")),
      ("debug.enabled", common::AttributeValue::bool(true)),
      ("log.level", common::AttributeValue::string("DEBUG")),
      ("sampling.probability", common::AttributeValue::float(1.0)),
      ("metrics.export.interval", common::AttributeValue::int(10L)) // 10秒
    ]
  }
  
  // 生产环境配置
  let prod_resource = common::Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("debug.enabled", common::AttributeValue::bool(false)),
      ("log.level", common::AttributeValue::string("INFO")),
      ("sampling.probability", common::AttributeValue::float(0.1)),
      ("metrics.export.interval", common::AttributeValue::int(60L)) // 60秒
    ]
  }
  
  // 测试环境配置
  let staging_resource = common::Resource::{
    service_name: "payment-service",
    service_version: Some("2.1.0-staging"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", common::AttributeValue::string("staging")),
      ("debug.enabled", common::AttributeValue::bool(false)),
      ("log.level", common::AttributeValue::string("INFO")),
      ("sampling.probability", common::AttributeValue::float(0.5)),
      ("metrics.export.interval", common::AttributeValue::int(30L)) // 30秒
    ]
  }
  
  // 验证开发环境配置
  assert_eq(dev_resource.attributes[0].1, common::AttributeValue::string("development"))
  assert_eq(dev_resource.attributes[1].1, common::AttributeValue::bool(true))
  assert_eq(dev_resource.attributes[2].1, common::AttributeValue::string("DEBUG"))
  assert_eq(dev_resource.attributes[3].1, common::AttributeValue::float(1.0))
  assert_eq(dev_resource.attributes[4].1, common::AttributeValue::int(10L))
  
  // 验证生产环境配置
  assert_eq(prod_resource.attributes[0].1, common::AttributeValue::string("production"))
  assert_eq(prod_resource.attributes[1].1, common::AttributeValue::bool(false))
  assert_eq(prod_resource.attributes[2].1, common::AttributeValue::string("INFO"))
  assert_eq(prod_resource.attributes[3].1, common::AttributeValue::float(0.1))
  assert_eq(prod_resource.attributes[4].1, common::AttributeValue::int(60L))
  
  // 验证测试环境配置
  assert_eq(staging_resource.attributes[0].1, common::AttributeValue::string("staging"))
  assert_eq(staging_resource.attributes[1].1, common::AttributeValue::bool(false))
  assert_eq(staging_resource.attributes[2].1, common::AttributeValue::string("INFO"))
  assert_eq(staging_resource.attributes[3].1, common::AttributeValue::float(0.5))
  assert_eq(staging_resource.attributes[4].1, common::AttributeValue::int(30L))
}

test "configuration_validation" {
  // 测试配置验证
  
  // 测试有效配置
  let valid_configs = [
    // 最小有效配置
    common::Resource::default("valid-service"),
    // 完整有效配置
    common::Resource::{
      service_name: "valid-complete-service",
      service_version: Some("1.0.0"),
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: [
        ("environment", common::AttributeValue::string("production")),
        ("region", common::AttributeValue::string("us-west-2"))
      ]
    },
    // 带特殊字符的配置
    common::Resource::{
      service_name: "service-with-special-chars_123",
      service_version: Some("2.0.0-beta"),
      telemetry_sdk_name: "测试-sdk",
      telemetry_sdk_version: "1.5.0",
      attributes: [
        ("环境", common::AttributeValue::string("生产")),
        ("регион", common::AttributeValue::string("us-west-2"))
      ]
    }
  ]
  
  // 验证有效配置
  let mut i = 0
  while i < valid_configs.length() {
    assert_eq(valid_configs[i].service_name.length() > 0, true)
    assert_eq(valid_configs[i].telemetry_sdk_name.length() > 0, true)
    assert_eq(valid_configs[i].telemetry_sdk_version.length() > 0, true)
    i = i + 1
  }
  
  // 测试边界情况配置
  let boundary_configs = [
    // 空服务名
    common::Resource::{
      service_name: "",
      service_version: None,
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: []
    },
    // 极长服务名
    common::Resource::{
      service_name: "very-long-service-name-" + "x" * 100,
      service_version: None,
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: []
    },
    // 大量属性
    common::Resource::{
      service_name: "many-attrs-service",
      service_version: None,
      telemetry_sdk_name: "azimuth",
      telemetry_sdk_version: "0.1.0",
      attributes: [for i = 0; i < 1000; i = i + 1].map(fn(i) { ("attr" + i.to_string(), common::AttributeValue::string("value" + i.to_string())) })
    }
  ]
  
  // 验证边界情况配置
  assert_eq(boundary_configs[0].service_name, "")
  assert_eq(boundary_configs[1].service_name.length(), 121) // "very-long-service-name-" + 100个"x"
  assert_eq(boundary_configs[2].attributes.length(), 1000)
}

test "dynamic_configuration_simulation" {
  // 模拟动态配置变更
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  // 初始配置
  let initial_meter = meter_provider.get_meter("dynamic-meter", "1.0.0")
  let initial_logger = logger_provider.get_logger("dynamic-logger", "1.0.0")
  
  let initial_counter = initial_meter.create_counter("dynamic_counter", "count", "Dynamic counter")
  
  // 使用初始配置
  initial_counter.add(1L, [("config.version", common::AttributeValue::string("1.0.0"))])
  initial_logger.info("Using initial configuration", [("version", common::AttributeValue::string("1.0.0"))])
  
  // 模拟配置更新到版本2.0.0
  let updated_meter = meter_provider.get_meter("dynamic-meter", "2.0.0")
  let updated_logger = logger_provider.get_logger("dynamic-logger", "2.0.0")
  
  let updated_counter = updated_meter.create_counter("dynamic_counter", "count", "Updated dynamic counter")
  
  // 使用更新后的配置
  updated_counter.add(1L, [("config.version", common::AttributeValue::string("2.0.0"))])
  updated_logger.info("Using updated configuration", [("version", common::AttributeValue::string("2.0.0"))])
  
  // 模拟配置回滚到版本1.0.0
  let rollback_meter = meter_provider.get_meter("dynamic-meter", "1.0.0")
  let rollback_logger = logger_provider.get_logger("dynamic-logger", "1.0.0")
  
  let rollback_counter = rollback_meter.create_counter("dynamic_counter", "count", "Rollback dynamic counter")
  
  // 使用回滚后的配置
  rollback_counter.add(1L, [("config.version", common::AttributeValue::string("1.0.0"))])
  rollback_logger.info("Using rollback configuration", [("version", common::AttributeValue::string("1.0.0"))])
  
  // 验证动态配置变更不会导致错误
  assert_eq(true, true)
  
  // 模拟配置属性变更
  let config_changes = [
    ("log.level", common::AttributeValue::string("DEBUG")),
    ("sampling.probability", common::AttributeValue::float(0.1)),
    ("metrics.export.interval", common::AttributeValue::int(30L)),
    ("trace.batch.size", common::AttributeValue::int(512L)),
    ("feature.flags", common::AttributeValue::array_bool([true, false, true]))
  ]
  
  let mut i = 0
  while i < config_changes.length() {
    initial_logger.info("Configuration change applied", [config_changes[i]])
    i = i + 1
  }
  
  assert_eq(true, true)
}

test "configuration_error_handling" {
  // 测试配置错误处理
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  // 测试无效配置参数
  let invalid_configs = [
    // 空名称
    ("", None, None),
    // 空版本
    ("test-service", Some(""), None),
    // 无效schema URL
    ("test-service", Some("1.0.0"), Some("invalid-url")),
    // 极长名称
    ("test-" + "x" * 10000, Some("1.0.0"), None)
  ]
  
  let mut i = 0
  while i < invalid_configs.length() {
    let meter = meter_provider.get_meter(invalid_configs[i].0, invalid_configs[i].1, invalid_configs[i].2)
    let logger = logger_provider.get_logger(invalid_configs[i].0, invalid_configs[i].1, invalid_configs[i].2)
    
    // 即使配置无效，也应该能创建Meter和Logger
    assert_eq(meter is metrics::Meter, true)
    assert_eq(logger is metrics::Logger, true)
    
    // 测试操作不会崩溃
    let counter = meter.create_counter("test_counter", "count", "Test counter")
    counter.add(1L, [("test", common::AttributeValue::string("invalid_config"))])
    
    logger.info("Test with invalid config", [("config_index", common::AttributeValue::int(i.to_int64()))])
    
    i = i + 1
  }
  
  // 测试无效属性配置
  let invalid_attributes = [
    // 空属性键
    ("", common::AttributeValue::string("empty_key")),
    // 极长属性键
    ("very_long_attribute_key_" + "x" * 1000, common::AttributeValue::string("long_key")),
    // 特殊字符属性键
    ("key/with/slashes", common::AttributeValue::string("slashes")),
    ("key\\with\\backslashes", common::AttributeValue::string("backslashes")),
    ("key with spaces", common::AttributeValue::string("spaces")),
    ("key@with@symbols", common::AttributeValue::string("symbols")),
    ("中文字符键", common::AttributeValue::string("chinese")),
    ("ключ-русский", common::AttributeValue::string("russian"))
  ]
  
  let meter = meter_provider.get_meter("invalid-attrs-test", "1.0.0")
  let counter = meter.create_counter("invalid_attrs_counter", "count", "Invalid attributes test")
  
  let mut i = 0
  while i < invalid_attributes.length() {
    counter.add(1L, [invalid_attributes[i]])
    i = i + 1
  }
  
  assert_eq(true, true)
  
  // 测试配置资源限制
  let resource_with_many_attrs = common::Resource::{
    service_name: "resource-limit-test",
    service_version: None,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [for i = 0; i < 10000; i = i + 1].map(fn(i) { ("attr" + i.to_string(), common::AttributeValue::string("value" + i.to_string())) })
  }
  
  // 验证大量属性不会导致错误
  assert_eq(resource_with_many_attrs.attributes.length(), 10000)
  
  let scope_with_long_name = common::InstrumentationScope::{
    name: "very-long-instrumentation-scope-name-" + "x" * 1000,
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/very/long/schema/url/" + "x" * 1000)
  }
  
  // 验证长名称不会导致错误
  assert_eq(scope_with_long_name.name.length(), 1038)
  assert_eq(scope_with_long_name.schema_url.unwrap().length(), 1055)
}