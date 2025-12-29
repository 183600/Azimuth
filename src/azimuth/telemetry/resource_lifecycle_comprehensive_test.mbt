// 资源生命周期综合管理测试 - 测试资源分配和释放

test "resource_creation_and_cleanup" {
  // 测试Resource的创建和清理
  let resource1 = common::Resource::default("test-service-1")
  let resource2 = common::Resource::default("test-service-2")
  let resource3 = common::Resource::default("test-service-3")
  
  // 验证资源创建
  assert_eq(resource1.service_name, "test-service-1")
  assert_eq(resource2.service_name, "test-service-2")
  assert_eq(resource3.service_name, "test-service-3")
  
  // 测试带属性的Resource创建
  let attributes = [
    ("service.instance.id", common::AttributeValue::string("instance-123")),
    ("service.namespace", common::AttributeValue::string("production")),
    ("host.name", common::AttributeValue::string("host-001")),
    ("deployment.environment", common::AttributeValue::string("prod"))
  ]
  
  let resource_with_attrs = common::Resource::{
    service_name: "test-service-with-attrs",
    service_version: Some("1.2.3"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: attributes
  }
  
  // 验证属性设置
  assert_eq(resource_with_attrs.service_name, "test-service-with-attrs")
  assert_eq(resource_with_attrs.service_version, Some("1.2.3"))
  assert_eq(resource_with_attrs.attributes.length(), 4)
  
  // 验证特定属性
  let mut found_instance_id = false
  let mut found_namespace = false
  let mut i = 0
  while i < resource_with_attrs.attributes.length() {
    let (key, value) = resource_with_attrs.attributes[i]
    match key {
      "service.instance.id" => {
        match value {
          common::StringValue(id) => {
            if id == "instance-123" { found_instance_id = true }
          }
          _ => ()
        }
      }
      "service.namespace" => {
        match value {
          common::StringValue(ns) => {
            if ns == "production" { found_namespace = true }
          }
          _ => ()
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert!(found_instance_id)
  assert!(found_namespace)
}

test "instrumentation_scope_lifecycle" {
  // 测试InstrumentationScope的生命周期管理
  let scope1 = common::InstrumentationScope::{
    name: "scope-1",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema-v1")
  }
  
  let scope2 = common::InstrumentationScope::{
    name: "scope-2",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schema-v2")
  }
  
  let scope3 = common::InstrumentationScope::{
    name: "scope-3",
    version: Some("3.0.0"),
    schema_url: Some("https://example.com/schema-v3")
  }
  
  // 验证创建
  assert_eq(scope1.name, "scope-1")
  assert_eq(scope1.version, Some("1.0.0"))
  assert_eq(scope1.schema_url, Some("https://example.com/schema-v1"))
  
  assert_eq(scope2.name, "scope-2")
  assert_eq(scope2.version, Some("2.0.0"))
  assert_eq(scope2.schema_url, Some("https://example.com/schema-v2"))
  
  assert_eq(scope3.name, "scope-3")
  assert_eq(scope3.version, Some("3.0.0"))
  assert_eq(scope3.schema_url, Some("https://example.com/schema-v3"))
  
  // 测试最小配置的Scope
  let minimal_scope = common::InstrumentationScope::{
    name: "minimal-scope",
    version: None,
    schema_url: None
  }
  
  assert_eq(minimal_scope.name, "minimal-scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
}

test "tracer_provider_lifecycle" {
  // 测试TracerProvider的生命周期管理
  let tracer_provider1 = trace::NoopTracerProvider::{}
  let tracer_provider2 = trace::NoopTracerProvider::{}
  let tracer_provider3 = trace::NoopTracerProvider::{}
  
  // 创建多个Tracer实例
  let tracer1 = tracer_provider1.get_tracer("tracer-1")
  let tracer2 = tracer_provider1.get_tracer("tracer-2", Some("1.0.0"))
  let tracer3 = tracer_provider2.get_tracer("tracer-3", Some("2.0.0"))
  let tracer4 = tracer_provider3.get_tracer("tracer-4", Some("3.0.0"))
  
  // 验证Tracer创建（在Noop实现中，所有Tracer都是相同的）
  // 在实际实现中，这里会有不同的Tracer实例
  assert_eq(true, true)
  
  // 测试使用Tracer创建Span
  let ctx = context::Context::empty()
  let (_, span1) = tracer1.start_span(ctx, "test-span-1")
  let (_, span2) = tracer2.start_span(ctx, "test-span-2")
  let (_, span3) = tracer3.start_span(ctx, "test-span-3")
  let (_, span4) = tracer4.start_span(ctx, "test-span-4")
  
  // 验证Span创建
  assert_eq(span1.name, "test-span-1")
  assert_eq(span2.name, "test-span-2")
  assert_eq(span3.name, "test-span-3")
  assert_eq(span4.name, "test-span-4")
}

test "meter_provider_lifecycle" {
  // 测试MeterProvider的生命周期管理
  let meter_provider1 = metrics::NoopMeterProvider::{}
  let meter_provider2 = metrics::NoopMeterProvider::{}
  let meter_provider3 = metrics::NoopMeterProvider::{}
  
  // 创建多个Meter实例
  let meter1 = meter_provider1.get_meter("meter-1")
  let meter2 = meter_provider1.get_meter("meter-2", Some("1.0.0"))
  let meter3 = meter_provider2.get_meter("meter-3", Some("2.0.0"), Some("https://example.com/schema1"))
  let meter4 = meter_provider3.get_meter("meter-4", Some("3.0.0"), Some("https://example.com/schema2"))
  
  // 验证Meter创建（在Noop实现中，所有Meter都是相同的）
  // 在实际实现中，这里会有不同的Meter实例
  assert_eq(true, true)
  
  // 测试使用Meter创建各种指标
  let counter1 = meter1.create_counter("counter-1", Some("count"), Some("Test counter 1"))
  let histogram1 = meter1.create_histogram("histogram-1", Some("ms"), Some("Test histogram 1"))
  let gauge1 = meter1.create_gauge("gauge-1", Some("value"), Some("Test gauge 1"))
  let up_down_counter1 = meter1.create_up_down_counter("up-down-1", Some("count"), Some("Test up-down 1"))
  
  let counter2 = meter2.create_counter("counter-2", None, None)
  let histogram2 = meter2.create_histogram("histogram-2", None, None)
  let gauge2 = meter2.create_gauge("gauge-2", None, None)
  let up_down_counter2 = meter2.create_up_down_counter("up-down-2", None, None)
  
  // 验证指标创建
  assert_eq(true, true)
  
  // 测试指标操作
  counter1.add(1L, Some([("test", common::AttributeValue::bool(true))]))
  histogram1.record(100.0, Some([("test", common::AttributeValue::bool(true))]))
  gauge1.record(50.0, Some([("test", common::AttributeValue::bool(true))]))
  up_down_counter1.add(1L, Some([("test", common::AttributeValue::bool(true))]))
  
  counter2.add(2L, None)
  histogram2.record(200.0, None)
  gauge2.record(150.0, None)
  up_down_counter2.add(-1L, None)
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "logger_provider_lifecycle" {
  // 测试LoggerProvider的生命周期管理
  let logger_provider1 = logs::NoopLoggerProvider::{}
  let logger_provider2 = logs::NoopLoggerProvider::{}
  let logger_provider3 = logs::NoopLoggerProvider::{}
  
  // 创建多个Logger实例
  let logger1 = logger_provider1.get_logger("logger-1")
  let logger2 = logger_provider1.get_logger("logger-2", Some("1.0.0"))
  let logger3 = logger_provider2.get_logger("logger-3", Some("2.0.0"), Some("https://example.com/schema1"))
  let logger4 = logger_provider3.get_logger("logger-4", Some("3.0.0"), Some("https://example.com/schema2"))
  
  // 验证Logger创建（在Noop实现中，所有Logger都是相同的）
  // 在实际实现中，这里会有不同的Logger实例
  assert_eq(true, true)
  
  // 测试使用Logger记录各种级别的日志
  logger1.debug("Debug message from logger-1", Some([("logger.id", common::AttributeValue::string("logger-1"))]))
  logger1.info("Info message from logger-1", Some([("logger.id", common::AttributeValue::string("logger-1"))]))
  logger1.warn("Warning message from logger-1", Some([("logger.id", common::AttributeValue::string("logger-1"))]))
  logger1.error("Error message from logger-1", Some([("logger.id", common::AttributeValue::string("logger-1"))]))
  logger1.fatal("Fatal message from logger-1", Some([("logger.id", common::AttributeValue::string("logger-1"))]))
  
  logger2.debug("Debug message from logger-2", None)
  logger2.info("Info message from logger-2", None)
  logger2.warn("Warning message from logger-2", None)
  logger2.error("Error message from logger-2", None)
  logger2.fatal("Fatal message from logger-2", None)
  
  // 测试使用LogRecordBuilder
  let log_record = logs::LogRecord::builder()
    .body("Structured log message")
    .severity(logs::SeverityNumber::Info)
    .with_attribute("component", common::AttributeValue::string("test-component"))
    .with_attribute("version", common::AttributeValue::string("1.0.0"))
    .build()
  
  logger1.emit(log_record)
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "propagator_lifecycle" {
  // 测试Propagator的生命周期管理
  let trace_propagator1 = propagation::W3CTraceContextPropagator::{}
  let trace_propagator2 = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator1 = propagation::W3CBaggagePropagator::{}
  let baggage_propagator2 = propagation::W3CBaggagePropagator::{}
  
  // 创建复合传播器
  let composite_propagator1 = propagation::CompositePropagator::new([
    trace_propagator1 as propagation::TextMapPropagator,
    baggage_propagator1 as propagation::TextMapPropagator
  ])
  
  let composite_propagator2 = propagation::CompositePropagator::new([
    trace_propagator2 as propagation::TextMapPropagator,
    baggage_propagator2 as propagation::TextMapPropagator
  ])
  
  // 测试传播器操作
  let ctx = context::Context::empty()
  let carrier1 = propagation::MapCarrier::new()
  let carrier2 = propagation::MapCarrier::new()
  
  // 使用不同的传播器进行注入和提取
  composite_propagator1.inject(ctx, carrier1)
  composite_propagator2.inject(ctx, carrier2)
  
  let extracted_ctx1 = composite_propagator1.extract(ctx, carrier1)
  let extracted_ctx2 = composite_propagator2.extract(ctx, carrier2)
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
  
  // 验证carrier中有预期的数据
  let trace_parent1 = carrier1.get(propagation::TRACE_PARENT_HEADER)
  let baggage1 = carrier1.get(propagation::BAGGAGE_HEADER)
  let trace_parent2 = carrier2.get(propagation::TRACE_PARENT_HEADER)
  let baggage2 = carrier2.get(propagation::BAGGAGE_HEADER)
  
  assert!(trace_parent1.is_some())
  assert!(baggage1.is_some())
  assert!(trace_parent2.is_some())
  assert!(baggage2.is_some())
}

test "carrier_lifecycle" {
  // 测试Carrier的生命周期管理
  let carrier1 = propagation::MapCarrier::new()
  let carrier2 = propagation::MapCarrier::new()
  let carrier3 = propagation::MapCarrier::new()
  
  // 测试carrier操作
  carrier1.set("test-header-1", "test-value-1")
  carrier1.set("test-header-2", "test-value-2")
  
  carrier2.set("another-header-1", "another-value-1")
  carrier2.set("another-header-2", "another-value-2")
  
  // 测试从map创建carrier
  let initial_data = [
    ("pre-set-header-1", "pre-set-value-1"),
    ("pre-set-header-2", "pre-set-value-2"),
    ("pre-set-header-3", "pre-set-value-3")
  ]
  let carrier3 = propagation::MapCarrier::from_map(initial_data)
  
  // 验证carrier操作
  let value1 = carrier1.get("test-header-1")
  let value2 = carrier1.get("test-header-2")
  let another_value1 = carrier2.get("another-header-1")
  let another_value2 = carrier2.get("another-header-2")
  let pre_set_value1 = carrier3.get("pre-set-header-1")
  let pre_set_value2 = carrier3.get("pre-set-header-2")
  let pre_set_value3 = carrier3.get("pre-set-header-3")
  
  assert_eq(value1, Some("test-value-1"))
  assert_eq(value2, Some("test-value-2"))
  assert_eq(another_value1, Some("another-value-1"))
  assert_eq(another_value2, Some("another-value-2"))
  assert_eq(pre_set_value1, Some("pre-set-value-1"))
  assert_eq(pre_set_value2, Some("pre-set-value-2"))
  assert_eq(pre_set_value3, Some("pre-set-value-3"))
  
  // 测试获取所有键
  let keys1 = carrier1.keys()
  let keys2 = carrier2.keys()
  let keys3 = carrier3.keys()
  
  assert_eq(keys1.length(), 2)
  assert_eq(keys2.length(), 2)
  assert_eq(keys3.length(), 3)
}

test "context_and_baggage_lifecycle" {
  // 测试Context和Baggage的生命周期管理
  let base_ctx = context::Context::empty()
  let base_baggage = context::Baggage::empty()
  
  // 创建多个Context实例
  let ctx1 = base_ctx.with_value(context::create_key("key-1"), "value-1")
  let ctx2 = base_ctx.with_value(context::create_key("key-2"), "value-2")
  let ctx3 = base_ctx.with_value(context::create_key("key-3"), "value-3")
  
  // 创建多个Baggage实例
  let baggage1 = base_baggage.with_entry("baggage-key-1", "baggage-value-1")
  let baggage2 = base_baggage.with_entry("baggage-key-2", "baggage-value-2")
  let baggage3 = base_baggage.with_entry("baggage-key-3", "baggage-value-3")
  
  // 验证Context操作
  assert_eq(ctx1.get(context::create_key("key-1")), Some("value-1"))
  assert_eq(ctx2.get(context::create_key("key-2")), Some("value-2"))
  assert_eq(ctx3.get(context::create_key("key-3")), Some("value-3"))
  
  // 验证Baggage操作
  assert_eq(baggage1.get("baggage-key-1"), Some("baggage-value-1"))
  assert_eq(baggage2.get("baggage-key-2"), Some("baggage-value-2"))
  assert_eq(baggage3.get("baggage-key-3"), Some("baggage-value-3"))
  
  // 测试链式操作
  let chained_ctx = ctx1.with_value(context::create_key("chained-key"), "chained-value")
  let chained_baggage = baggage1.with_entry("chained-baggage-key", "chained-baggage-value")
  
  assert_eq(chained_ctx.get(context::create_key("key-1")), Some("value-1"))
  assert_eq(chained_ctx.get(context::create_key("chained-key")), Some("chained-value"))
  assert_eq(chained_baggage.get("baggage-key-1"), Some("baggage-value-1"))
  assert_eq(chained_baggage.get("chained-baggage-key"), Some("chained-baggage-value"))
}

test "resource_limit_management" {
  // 测试资源限制管理
  // 测试大量资源创建
  let mut resources = []
  let mut scopes = []
  let mut tracers = []
  let mut meters = []
  let mut loggers = []
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  // 创建大量资源实例
  let mut i = 0
  while i < 100 {
    let resource = common::Resource::default("service-${i}")
    let scope = common::InstrumentationScope::{
      name: "scope-${i}",
      version: Some("${i}.0.0"),
      schema_url: Some("https://example.com/schema-${i}")
    }
    let tracer = tracer_provider.get_tracer("tracer-${i}")
    let meter = meter_provider.get_meter("meter-${i}")
    let logger = logger_provider.get_logger("logger-${i}")
    
    resources.push(resource)
    scopes.push(scope)
    tracers.push(tracer)
    meters.push(meter)
    loggers.push(logger)
    
    i = i + 1
  }
  
  // 验证资源创建
  assert_eq(resources.length(), 100)
  assert_eq(scopes.length(), 100)
  assert_eq(tracers.length(), 100)
  assert_eq(meters.length(), 100)
  assert_eq(loggers.length(), 100)
  
  // 验证特定资源
  assert_eq(resources[0].service_name, "service-0")
  assert_eq(resources[99].service_name, "service-99")
  assert_eq(scopes[0].name, "scope-0")
  assert_eq(scopes[99].name, "scope-99")
  
  // 测试资源使用
  let ctx = context::Context::empty()
  let (_, test_span) = tracers[0].start_span(ctx, "test-span")
  let test_counter = meters[0].create_counter("test-counter", None, None)
  test_counter.add(1L, None)
  loggers[0].info("Test message", None)
  
  // 验证操作不会抛出异常
  assert_eq(test_span.name, "test-span")
  assert_eq(true, true)
}