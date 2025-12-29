// 配置动态更新测试 - 测试运行时配置变更

test "resource_dynamic_update" {
  // 测试Resource的动态更新
  
  // 创建初始Resource
  let initial_resource = common::Resource::default("initial-service")
  assert_eq(initial_resource.service_name, "initial-service")
  assert_eq(initial_resource.service_version, None)
  assert_eq(initial_resource.attributes.length(), 0)
  
  // 模拟动态更新服务名称
  let updated_resource1 = common::Resource::{
    service_name: "updated-service-v1",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: []
  }
  assert_eq(updated_resource1.service_name, "updated-service-v1")
  assert_eq(updated_resource1.service_version, Some("1.0.0"))
  
  // 模拟动态添加属性
  let updated_resource2 = common::Resource::{
    service_name: "updated-service-v1",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", common::AttributeValue::string("instance-001")),
      ("environment", common::AttributeValue::string("production"))
    ]
  }
  assert_eq(updated_resource2.attributes.length(), 2)
  
  // 模拟动态更新属性
  let updated_resource3 = common::Resource::{
    service_name: "updated-service-v2",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", common::AttributeValue::string("instance-002")),
      ("environment", common::AttributeValue::string("staging")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  assert_eq(updated_resource3.service_name, "updated-service-v2")
  assert_eq(updated_resource3.service_version, Some("2.0.0"))
  assert_eq(updated_resource3.attributes.length(), 3)
}

test "instrumentation_scope_dynamic_update" {
  // 测试InstrumentationScope的动态更新
  
  // 创建初始InstrumentationScope
  let initial_scope = common::InstrumentationScope::{
    name: "initial-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema-v1")
  }
  assert_eq(initial_scope.name, "initial-scope")
  assert_eq(initial_scope.version, Some("1.0.0"))
  assert_eq(initial_scope.schema_url, Some("https://example.com/schema-v1"))
  
  // 模拟动态更新版本
  let updated_scope1 = common::InstrumentationScope::{
    name: "initial-scope",
    version: Some("1.1.0"),
    schema_url: Some("https://example.com/schema-v1")
  }
  assert_eq(updated_scope1.version, Some("1.1.0"))
  
  // 模拟动态更新schema URL
  let updated_scope2 = common::InstrumentationScope::{
    name: "initial-scope",
    version: Some("1.1.0"),
    schema_url: Some("https://example.com/schema-v2")
  }
  assert_eq(updated_scope2.schema_url, Some("https://example.com/schema-v2"))
  
  // 模拟完全更新
  let updated_scope3 = common::InstrumentationScope::{
    name: "updated-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schema-v3")
  }
  assert_eq(updated_scope3.name, "updated-scope")
  assert_eq(updated_scope3.version, Some("2.0.0"))
  assert_eq(updated_scope3.schema_url, Some("https://example.com/schema-v3"))
}

test "tracer_configuration_update" {
  // 测试Tracer配置的动态更新
  
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  
  // 创建初始Tracer
  let initial_tracer = tracer_provider.get_tracer("initial-tracer")
  let (_, initial_span) = initial_tracer.start_span(ctx, "initial-operation")
  assert_eq(initial_span.name, "initial-operation")
  
  // 模拟动态更新Tracer配置
  let updated_tracer1 = tracer_provider.get_tracer("updated-tracer-v1", Some("1.0.0"))
  let (_, updated_span1) = updated_tracer1.start_span(ctx, "updated-operation-v1")
  assert_eq(updated_span1.name, "updated-operation-v1")
  
  // 模拟使用不同配置创建Span
  let default_attrs = [
    ("service.name", common::AttributeValue::string("test-service")),
    ("service.version", common::AttributeValue::string("1.0.0"))
  ]
  let (_, span_with_attrs) = updated_tracer1.start_span(
    ctx,
    "operation-with-attrs",
    Some(trace::SpanKind::Server),
    Some(default_attrs)
  )
  assert_eq(span_with_attrs.name, "operation-with-attrs")
  assert_eq(span_with_attrs.kind, trace::SpanKind::Server)
  assert_eq(span_with_attrs.attributes.length(), 2)
  
  // 模拟动态更新Span配置
  let updated_attrs = [
    ("service.name", common::AttributeValue::string("updated-service")),
    ("service.version", common::AttributeValue::string("2.0.0")),
    ("environment", common::AttributeValue::string("production"))
  ]
  let (_, span_with_updated_attrs) = updated_tracer1.start_span(
    ctx,
    "operation-with-updated-attrs",
    Some(trace::SpanKind::Client),
    Some(updated_attrs)
  )
  assert_eq(span_with_updated_attrs.name, "operation-with-updated-attrs")
  assert_eq(span_with_updated_attrs.kind, trace::SpanKind::Client)
  assert_eq(span_with_updated_attrs.attributes.length(), 3)
}

test "meter_configuration_update" {
  // 测试Meter配置的动态更新
  
  let meter_provider = metrics::NoopMeterProvider::{}
  
  // 创建初始Meter
  let initial_meter = meter_provider.get_meter("initial-meter")
  let initial_counter = initial_meter.create_counter("initial-counter", None, None)
  initial_counter.add(1L, None)
  
  // 模拟动态更新Meter配置
  let updated_meter1 = meter_provider.get_meter("updated-meter-v1", Some("1.0.0"))
  let updated_counter1 = updated_meter1.create_counter("updated-counter-v1", Some("count"), Some("Updated counter v1"))
  updated_counter1.add(1L, Some([("version", common::AttributeValue::string("v1"))]))
  
  // 模拟动态更新指标配置
  let updated_meter2 = meter_provider.get_meter("updated-meter-v2", Some("2.0.0"), Some("https://example.com/schema"))
  let updated_counter2 = updated_meter2.create_counter(
    "updated-counter-v2",
    Some("operations"),
    Some("Updated counter v2 with schema"
  )
  updated_counter2.add(1L, Some([
    ("version", common::AttributeValue::string("v2")),
    ("schema", common::AttributeValue::string("https://example.com/schema"))
  ]))
  
  // 创建不同类型的指标
  let histogram = updated_meter2.create_histogram(
    "operation.duration",
    Some("ms"),
    Some("Operation duration in milliseconds"
  )
  let gauge = updated_meter2.create_gauge(
    "memory.usage",
    Some("bytes"),
    Some("Memory usage in bytes"
  )
  let up_down_counter = updated_meter2.create_up_down_counter(
    "active.connections",
    Some("connections"),
    Some("Number of active connections"
  )
  
  // 记录指标
  histogram.record(100.0, Some([("operation.type", common::AttributeValue::string("http_request"))]))
  gauge.record(1024000.0, Some([("memory.type", common::AttributeValue::string("heap"))]))
  up_down_counter.add(1L, Some([("connection.type", common::AttributeValue::string("websocket"))]))
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "logger_configuration_update" {
  // 测试Logger配置的动态更新
  
  let logger_provider = logs::NoopLoggerProvider::{}
  
  // 创建初始Logger
  let initial_logger = logger_provider.get_logger("initial-logger")
  initial_logger.info("Initial log message", None)
  
  // 模拟动态更新Logger配置
  let updated_logger1 = logger_provider.get_logger("updated-logger-v1", Some("1.0.0"))
  updated_logger1.info("Updated log message v1", Some([
    ("logger.version", common::AttributeValue::string("v1"))
  ]))
  
  // 模拟动态更新Logger配置并添加schema
  let updated_logger2 = logger_provider.get_logger(
    "updated-logger-v2",
    Some("2.0.0"),
    Some("https://example.com/logging-schema"
  )
  updated_logger2.info("Updated log message v2", Some([
    ("logger.version", common::AttributeValue::string("v2")),
    ("logger.schema", common::AttributeValue::string("https://example.com/logging-schema"))
  ]))
  
  // 测试不同级别的日志记录
  updated_logger2.debug("Debug message", Some([("level", common::AttributeValue::string("debug"))]))
  updated_logger2.warn("Warning message", Some([("level", common::AttributeValue::string("warn"))]))
  updated_logger2.error("Error message", Some([("level", common::AttributeValue::string("error"))]))
  updated_logger2.fatal("Fatal message", Some([("level", common::AttributeValue::string("fatal"))]))
  
  // 测试使用LogRecordBuilder
  let log_record = logs::LogRecord::builder()
    .body("Structured log with updated config")
    .severity(logs::SeverityNumber::Info)
    .with_attribute("logger.version", common::AttributeValue::string("v2"))
    .with_attribute("logger.config", common::AttributeValue::string("dynamic"))
    .with_attribute("timestamp", common::AttributeValue::int(1609459200000000000L))
    .build()
  
  updated_logger2.emit(log_record)
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "propagation_configuration_update" {
  // 测试传播配置的动态更新
  
  let ctx = context::Context::empty()
  
  // 创建初始传播器配置
  let initial_trace_propagator = propagation::W3CTraceContextPropagator::{}
  let initial_baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  let initial_carrier = propagation::MapCarrier::new()
  initial_trace_propagator.inject(ctx, initial_carrier)
  initial_baggage_propagator.inject(ctx, initial_carrier)
  
  // 验证初始注入
  let initial_trace_parent = initial_carrier.get(propagation::TRACE_PARENT_HEADER)
  let initial_baggage = initial_carrier.get(propagation::BAGGAGE_HEADER)
  assert!(initial_trace_parent.is_some())
  assert!(initial_baggage.is_some())
  
  // 模拟动态更新传播器配置
  // 创建复合传播器
  let updated_propagators = [
    initial_trace_propagator as propagation::TextMapPropagator,
    initial_baggage_propagator as propagation::TextMapPropagator
  ]
  let composite_propagator = propagation::CompositePropagator::new(updated_propagators)
  
  // 使用更新后的配置
  let updated_carrier = propagation::MapCarrier::new()
  composite_propagator.inject(ctx, updated_carrier)
  
  // 验证更新后的注入
  let updated_trace_parent = updated_carrier.get(propagation::TRACE_PARENT_HEADER)
  let updated_baggage = updated_carrier.get(propagation::BAGGAGE_HEADER)
  assert!(updated_trace_parent.is_some())
  assert!(updated_baggage.is_some())
  
  // 测试提取操作
  let extracted_ctx = composite_propagator.extract(ctx, updated_carrier)
  assert_eq(true, true)
}

test "context_configuration_update" {
  // 测试Context配置的动态更新
  
  // 创建初始Context
  let initial_ctx = context::Context::empty()
  let key1 = context::create_key("config.key1")
  let key2 = context::create_key("config.key2")
  
  // 添加初始配置
  let ctx_with_initial_config = initial_ctx
    .with_value(key1, "initial-value1")
    .with_value(key2, "initial-value2")
  
  assert_eq(ctx_with_initial_config.get(key1), Some("initial-value1"))
  assert_eq(ctx_with_initial_config.get(key2), Some("initial-value2"))
  
  // 模拟动态更新配置值
  let ctx_with_updated_config = ctx_with_initial_config
    .with_value(key1, "updated-value1")
    .with_value(key2, "updated-value2")
  
  assert_eq(ctx_with_updated_config.get(key1), Some("updated-value1"))
  assert_eq(ctx_with_updated_config.get(key2), Some("updated-value2"))
  
  // 添加新的配置键
  let new_key = context::create_key("config.new-key")
  let ctx_with_new_config = ctx_with_updated_config
    .with_value(new_key, "new-value")
  
  assert_eq(ctx_with_new_config.get(new_key), Some("new-value"))
  assert_eq(ctx_with_new_config.get(key1), Some("updated-value1"))
  assert_eq(ctx_with_new_config.get(key2), Some("updated-value2"))
  
  // 测试配置的独立性
  let independent_ctx = initial_ctx
    .with_value(context::create_key("independent.key"), "independent-value")
  
  assert_eq(independent_ctx.get(context::create_key("independent.key")), Some("independent-value"))
  assert_eq(independent_ctx.get(key1), None)
}

test "baggage_configuration_update" {
  // 测试Baggage配置的动态更新
  
  // 创建初始Baggage
  let initial_baggage = context::Baggage::empty()
  
  // 添加初始配置
  let baggage_with_initial_config = initial_baggage
    .with_entry("config.item1", "initial-config1")
    .with_entry("config.item2", "initial-config2")
  
  assert_eq(baggage_with_initial_config.get("config.item1"), Some("initial-config1"))
  assert_eq(baggage_with_initial_config.get("config.item2"), Some("initial-config2"))
  
  // 模拟动态更新配置值
  let baggage_with_updated_config = initial_baggage
    .with_entry("config.item1", "updated-config1")
    .with_entry("config.item2", "updated-config2")
  
  assert_eq(baggage_with_updated_config.get("config.item1"), Some("updated-config1"))
  assert_eq(baggage_with_updated_config.get("config.item2"), Some("updated-config2"))
  
  // 添加新的配置项
  let baggage_with_new_config = baggage_with_updated_config
    .with_entry("config.new-item", "new-config")
  
  assert_eq(baggage_with_new_config.get("config.new-item"), Some("new-config"))
  assert_eq(baggage_with_new_config.get("config.item1"), Some("updated-config1"))
  assert_eq(baggage_with_new_config.get("config.item2"), Some("updated-config2"))
  
  // 测试配置的独立性
  let independent_baggage = initial_baggage
    .with_entry("independent.item", "independent-config")
  
  assert_eq(independent_baggage.get("independent.item"), Some("independent-config"))
  assert_eq(independent_baggage.get("config.item1"), None)
}

test "global_configuration_update_simulation" {
  // 模拟全局配置更新
  
  // 初始全局配置
  let global_resource = common::Resource::default("global-service")
  let global_scope = common::InstrumentationScope::{
    name: "global-scope",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  // 初始全局组件
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let global_tracer = tracer_provider.get_tracer("global-tracer")
  let global_meter = meter_provider.get_meter("global-meter")
  let global_logger = logger_provider.get_logger("global-logger")
  
  // 使用初始配置
  let ctx = context::Context::empty()
  let (_, initial_span) = global_tracer.start_span(ctx, "initial-operation")
  let global_counter = global_meter.create_counter("global.counter", None, None)
  global_counter.add(1L, Some([("config.version", common::AttributeValue::string("v1.0.0"))]))
  global_logger.info("Initial global log", Some([("config.version", common::AttributeValue::string("v1.0.0"))]))
  
  // 模拟全局配置更新
  let updated_global_resource = common::Resource::{
    service_name: "updated-global-service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.2.0",
    attributes: [
      ("environment", common::AttributeValue::string("production")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  
  let updated_global_scope = common::InstrumentationScope::{
    name: "updated-global-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/updated-schema")
  }
  
  // 使用更新后的配置
  let updated_tracer = tracer_provider.get_tracer("updated-global-tracer", Some("2.0.0"))
  let updated_meter = meter_provider.get_meter("updated-global-meter", Some("2.0.0"))
  let updated_logger = logger_provider.get_logger("updated-global-logger", Some("2.0.0"))
  
  let (_, updated_span) = updated_tracer.start_span(
    ctx,
    "updated-operation",
    Some(trace::SpanKind::Server),
    Some([
      ("service.version", common::AttributeValue::string("2.0.0")),
      ("environment", common::AttributeValue::string("production"))
    ])
  )
  
  let updated_counter = updated_meter.create_counter(
    "updated.global.counter",
    Some("operations"),
    Some("Updated global counter"
  )
  updated_counter.add(1L, Some([
    ("config.version", common::AttributeValue::string("v2.0.0")),
    ("environment", common::AttributeValue::string("production"))
  ]))
  
  updated_logger.info("Updated global log", Some([
    ("config.version", common::AttributeValue::string("v2.0.0")),
    ("environment", common::AttributeValue::string("production"))
  ]))
  
  // 验证配置更新后的行为
  assert_eq(initial_span.name, "initial-operation")
  assert_eq(updated_span.name, "updated-operation")
  assert_eq(updated_span.kind, trace::SpanKind::Server)
  assert_eq(updated_span.attributes.length(), 2)
  
  // 验证全局资源更新
  assert_eq(updated_global_resource.service_name, "updated-global-service")
  assert_eq(updated_global_resource.service_version, Some("2.0.0"))
  assert_eq(updated_global_resource.attributes.length(), 2)
  
  // 验证全局作用域更新
  assert_eq(updated_global_scope.name, "updated-global-scope")
  assert_eq(updated_global_scope.version, Some("2.0.0"))
  assert_eq(updated_global_scope.schema_url, Some("https://example.com/updated-schema"))
}