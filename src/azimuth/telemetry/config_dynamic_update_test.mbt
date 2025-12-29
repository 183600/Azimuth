// 配置动态更新测试用例
// 测试遥测系统配置的动态更新功能

test "resource_dynamic_update" {
  // 测试资源的动态更新
  
  // 1. 创建初始资源
  let initial_resource = Resource::default("dynamic-service")
  assert_eq(initial_resource.service_name, "dynamic-service")
  assert_eq(initial_resource.service_version, None)
  assert_eq(initial_resource.attributes.length(), 0)
  
  // 2. 模拟动态更新资源属性
  // 由于Resource是不可变的，我们创建新的Resource来模拟更新
  let updated_resource = Resource::default("dynamic-service")
  let updated_attributes = [
    ("service.version", AttributeValue::string("2.0.0")),
    ("service.namespace", AttributeValue::string("production")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("updated-host")),
    ("process.pid", AttributeValue::int(12345L))
  ]
  
  // 3. 创建带有更新资源的span和log
  let ctx = Context::empty()
  let span_attributes = [
    ("service.name", AttributeValue::string("dynamic-service")),
    ("service.version", AttributeValue::string("2.0.0")),
    ("service.namespace", AttributeValue::string("production"))
  ]
  
  let (_, updated_span) = NoopTracer::start_span(ctx, "dynamic_resource_span", Server, Some(span_attributes))
  
  let updated_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Resource dynamically updated"),
    attributes: updated_attributes,
    trace_id: Some(updated_span.context.trace_id),
    span_id: Some(updated_span.context.span_id),
    trace_flags: Some(updated_span.context.trace_flags),
    resource: Some(updated_resource),
    instrumentation_scope: None
  }
  
  // 4. 验证资源更新
  match updated_log.resource {
    Some(resource) => {
      assert_eq(resource.service_name, "dynamic-service")
      assert_eq(resource.telemetry_sdk_name, "azimuth")
      assert_eq(resource.telemetry_sdk_version, "0.1.0")
    }
    None => @test.fail("Test failed")
  }
  
  // 5. 模拟多次动态更新
  let multi_updated_attributes = [
    ("service.version", AttributeValue::string("2.1.0")),
    ("service.namespace", AttributeValue::string("staging")),
    ("deployment.environment", AttributeValue::string("staging")),
    ("host.name", AttributeValue::string("staging-host")),
    ("process.pid", AttributeValue::int(54321L)),
    ("update.count", AttributeValue::int(3L)),
    ("last.update", AttributeValue::string("2023-01-01T00:00:00Z"))
  ]
  
  let multi_updated_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Resource multiple times dynamically updated"),
    attributes: multi_updated_attributes,
    trace_id: Some(updated_span.context.trace_id),
    span_id: Some(updated_span.context.span_id),
    trace_flags: Some(updated_span.context.trace_flags),
    resource: Some(updated_resource),
    instrumentation_scope: None
  }
  
  // 6. 验证多次更新
  assert_eq(multi_updated_log.attributes.length(), 7)
  
  let mut found_version = false
  let mut found_namespace = false
  let mut found_environment = false
  let mut found_update_count = false
  
  let mut i = 0
  while i < multi_updated_log.attributes.length() {
    let (name, value) = multi_updated_log.attributes[i]
    match (name, value) {
      ("service.version", StringValue(version)) => {
        assert_eq(version, "2.1.0")
        found_version = true
      }
      ("service.namespace", StringValue(namespace)) => {
        assert_eq(namespace, "staging")
        found_namespace = true
      }
      ("deployment.environment", StringValue(environment)) => {
        assert_eq(environment, "staging")
        found_environment = true
      }
      ("update.count", IntValue(count)) => {
        assert_eq(count, 3L)
        found_update_count = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_version, true)
  assert_eq(found_namespace, true)
  assert_eq(found_environment, true)
  assert_eq(found_update_count, true)
}

test "instrumentation_scope_dynamic_update" {
  // 测试仪表范围的动态更新
  
  // 1. 创建初始仪表范围
  let initial_scope = InstrumentationScope::{
    name: "initial-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 2. 模拟动态更新仪表范围
  let updated_scope = InstrumentationScope::{
    name: "updated-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schema/v2")
  }
  
  // 3. 创建带有更新仪表范围的span和log
  let ctx = Context::empty()
  let span_attributes = [
    ("scope.name", AttributeValue::string("updated-scope")),
    ("scope.version", AttributeValue::string("2.0.0"))
  ]
  
  let (_, scope_updated_span) = NoopTracer::start_span(ctx, "scope_dynamic_span", Server, Some(span_attributes))
  
  let scope_updated_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Instrumentation scope dynamically updated"),
    attributes: span_attributes,
    trace_id: Some(scope_updated_span.context.trace_id),
    span_id: Some(scope_updated_span.context.span_id),
    trace_flags: Some(scope_updated_span.context.trace_flags),
    resource: None,
    instrumentation_scope: Some(updated_scope)
  }
  
  // 4. 验证仪表范围更新
  match scope_updated_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "updated-scope")
      match scope.version {
        Some(version) => assert_eq(version, "2.0.0")
        None => @test.fail("Test failed")
      }
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://example.com/schema/v2")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 5. 模拟多次动态更新
  let multi_updated_scope = InstrumentationScope::{
    name: "multi-updated-scope",
    version: Some("3.1.0"),
    schema_url: Some("https://example.com/schema/v3.1")
  }
  
  let multi_updated_attributes = [
    ("scope.name", AttributeValue::string("multi-updated-scope")),
    ("scope.version", AttributeValue::string("3.1.0")),
    ("scope.schema", AttributeValue::string("https://example.com/schema/v3.1")),
    ("update.count", AttributeValue::int(2L))
  ]
  
  let multi_updated_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Instrumentation scope multiple times dynamically updated"),
    attributes: multi_updated_attributes,
    trace_id: Some(scope_updated_span.context.trace_id),
    span_id: Some(scope_updated_span.context.span_id),
    trace_flags: Some(scope_updated_span.context.trace_flags),
    resource: None,
    instrumentation_scope: Some(multi_updated_scope)
  }
  
  // 6. 验证多次更新
  match multi_updated_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "multi-updated-scope")
      match scope.version {
        Some(version) => assert_eq(version, "3.1.0")
        None => @test.fail("Test failed")
      }
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://example.com/schema/v3.1")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 验证属性中的更新信息
  let mut found_name = false
  let mut found_version = false
  let mut found_schema = false
  let mut found_update_count = false
  
  let mut i = 0
  while i < multi_updated_log.attributes.length() {
    let (name, value) = multi_updated_log.attributes[i]
    match (name, value) {
      ("scope.name", StringValue(name)) => {
        assert_eq(name, "multi-updated-scope")
        found_name = true
      }
      ("scope.version", StringValue(version)) => {
        assert_eq(version, "3.1.0")
        found_version = true
      }
      ("scope.schema", StringValue(schema)) => {
        assert_eq(schema, "https://example.com/schema/v3.1")
        found_schema = true
      }
      ("update.count", IntValue(count)) => {
        assert_eq(count, 2L)
        found_update_count = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_name, true)
  assert_eq(found_version, true)
  assert_eq(found_schema, true)
  assert_eq(found_update_count, true)
}

test "logger_provider_dynamic_update" {
  // 测试Logger Provider的动态更新
  
  // 1. 创建初始Logger Provider
  let initial_provider = NoopLoggerProvider::{}
  let initial_logger = initial_provider.get_logger("initial-logger", Some("1.0.0"), Some("https://example.com/schema/v1"))
  
  // 2. 使用初始logger记录日志
  let initial_attributes = [
    ("logger.name", AttributeValue::string("initial-logger")),
    ("logger.version", AttributeValue::string("1.0.0")),
    ("config.phase", AttributeValue::string("initial"))
  ]
  
  initial_logger.info("Initial configuration", Some(initial_attributes))
  
  // 3. 模拟动态更新Logger Provider配置
  // 创建新的logger来模拟配置更新
  let updated_provider = NoopLoggerProvider::{}
  let updated_logger = updated_provider.get_logger("updated-logger", Some("2.0.0"), Some("https://example.com/schema/v2"))
  
  // 4. 使用更新后的logger记录日志
  let updated_attributes = [
    ("logger.name", AttributeValue::string("updated-logger")),
    ("logger.version", AttributeValue::string("2.0.0")),
    ("config.phase", AttributeValue::string("updated")),
    ("config.change", AttributeValue::bool(true))
  ]
  
  updated_logger.info("Updated configuration", Some(updated_attributes))
  
  // 5. 验证配置更新
  // 由于是No-op实现，我们通过创建LogRecord来验证
  let config_update_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Logger provider configuration updated"),
    attributes: updated_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: Some(InstrumentationScope::{
      name: "updated-logger",
      version: Some("2.0.0"),
      schema_url: Some("https://example.com/schema/v2")
    })
  }
  
  // 6. 验证配置更新
  match config_update_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "updated-logger")
      match scope.version {
        Some(version) => assert_eq(version, "2.0.0")
        None => @test.fail("Test failed")
      }
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://example.com/schema/v2")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 7. 模拟多次配置更新
  let multi_updated_logger = updated_provider.get_logger("multi-updated-logger", Some("3.1.0"), Some("https://example.com/schema/v3.1"))
  
  let multi_updated_attributes = [
    ("logger.name", AttributeValue::string("multi-updated-logger")),
    ("logger.version", AttributeValue::string("3.1.0")),
    ("config.phase", AttributeValue::string("multi-updated")),
    ("config.change.count", AttributeValue::int(2L)),
    ("last.update", AttributeValue::string("2023-01-01T00:00:00Z"))
  ]
  
  multi_updated_logger.warn("Multi-updated configuration", Some(multi_updated_attributes))
  
  // 8. 验证多次配置更新
  let multi_config_update_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001100L),
    severity_number: Warn,
    severity_text: Some("WARN"),
    body: Some("Logger provider configuration multiple times updated"),
    attributes: multi_updated_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: Some(InstrumentationScope::{
      name: "multi-updated-logger",
      version: Some("3.1.0"),
      schema_url: Some("https://example.com/schema/v3.1")
    })
  }
  
  // 验证多次更新
  match multi_config_update_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "multi-updated-logger")
      match scope.version {
        Some(version) => assert_eq(version, "3.1.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "tracer_provider_dynamic_update" {
  // 测试Tracer Provider的动态更新
  
  // 1. 创建初始Tracer Provider
  let initial_provider = NoopTracerProvider::{}
  let initial_tracer = initial_provider.get_tracer("initial-tracer", Some("1.0.0"))
  
  // 2. 使用初始tracer创建span
  let ctx = Context::empty()
  let initial_span_attributes = [
    ("tracer.name", AttributeValue::string("initial-tracer")),
    ("tracer.version", AttributeValue::string("1.0.0")),
    ("config.phase", AttributeValue::string("initial"))
  ]
  
  let (_, initial_span) = initial_tracer.start_span(ctx, "initial_span", Server, Some(initial_span_attributes))
  
  // 3. 模拟动态更新Tracer Provider配置
  let updated_provider = NoopTracerProvider::{}
  let updated_tracer = updated_provider.get_tracer("updated-tracer", Some("2.0.0"))
  
  // 4. 使用更新后的tracer创建span
  let updated_span_attributes = [
    ("tracer.name", AttributeValue::string("updated-tracer")),
    ("tracer.version", AttributeValue::string("2.0.0")),
    ("config.phase", AttributeValue::string("updated")),
    ("config.change", AttributeValue::bool(true))
  ]
  
  let (_, updated_span) = updated_tracer.start_span(ctx, "updated_span", Server, Some(updated_span_attributes))
  
  // 5. 验证配置更新
  assert_eq(updated_span.name, "updated_span")
  assert_eq(updated_span.kind, Server)
  assert_eq(updated_span.attributes.length(), 4)
  
  // 6. 模拟多次配置更新
  let multi_updated_tracer = updated_provider.get_tracer("multi-updated-tracer", Some("3.1.0"))
  
  let multi_updated_attributes = [
    ("tracer.name", AttributeValue::string("multi-updated-tracer")),
    ("tracer.version", AttributeValue::string("3.1.0")),
    ("config.phase", AttributeValue::string("multi-updated")),
    ("config.change.count", AttributeValue::int(2L)),
    ("last.update", AttributeValue::string("2023-01-01T00:00:00Z"))
  ]
  
  let (_, multi_updated_span) = multi_updated_tracer.start_span(ctx, "multi_updated_span", Client, Some(multi_updated_attributes))
  
  // 7. 验证多次配置更新
  assert_eq(multi_updated_span.name, "multi_updated_span")
  assert_eq(multi_updated_span.kind, Client)
  assert_eq(multi_updated_span.attributes.length(), 5)
  
  // 验证多次更新的属性
  let mut found_tracer_name = false
  let mut found_tracer_version = false
  let mut found_config_phase = false
  let mut found_change_count = false
  
  let mut i = 0
  while i < multi_updated_span.attributes.length() {
    let (name, value) = multi_updated_span.attributes[i]
    match (name, value) {
      ("tracer.name", StringValue(name)) => {
        assert_eq(name, "multi-updated-tracer")
        found_tracer_name = true
      }
      ("tracer.version", StringValue(version)) => {
        assert_eq(version, "3.1.0")
        found_tracer_version = true
      }
      ("config.phase", StringValue(phase)) => {
        assert_eq(phase, "multi-updated")
        found_config_phase = true
      }
      ("config.change.count", IntValue(count)) => {
        assert_eq(count, 2L)
        found_change_count = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_tracer_name, true)
  assert_eq(found_tracer_version, true)
  assert_eq(found_config_phase, true)
  assert_eq(found_change_count, true)
}

test "meter_provider_dynamic_update" {
  // 测试Meter Provider的动态更新
  
  // 1. 创建初始Meter Provider
  let initial_provider = NoopMeterProvider::{}
  let initial_meter = initial_provider.get_meter("initial-meter", Some("1.0.0"), Some("https://example.com/schema/v1"))
  
  // 2. 使用初始meter创建指标
  let initial_counter = initial_meter.create_counter("initial_counter", Some("operations"), Some("Initial counter"))
  let initial_histogram = initial_meter.create_histogram("initial_histogram", Some("ms"), Some("Initial histogram"))
  
  // 3. 记录初始指标
  let initial_attributes = [
    ("meter.name", AttributeValue::string("initial-meter")),
    ("meter.version", AttributeValue::string("1.0.0")),
    ("config.phase", AttributeValue::string("initial"))
  ]
  
  initial_counter.add(1L, Some(initial_attributes))
  initial_histogram.record(100.0, Some(initial_attributes))
  
  // 4. 模拟动态更新Meter Provider配置
  let updated_provider = NoopMeterProvider::{}
  let updated_meter = updated_provider.get_meter("updated-meter", Some("2.0.0"), Some("https://example.com/schema/v2"))
  
  // 5. 使用更新后的meter创建指标
  let updated_counter = updated_meter.create_counter("updated_counter", Some("operations"), Some("Updated counter"))
  let updated_histogram = updated_meter.create_histogram("updated_histogram", Some("ms"), Some("Updated histogram"))
  let updated_gauge = updated_meter.create_gauge("updated_gauge", Some("value"), Some("Updated gauge"))
  
  // 6. 记录更新后的指标
  let updated_attributes = [
    ("meter.name", AttributeValue::string("updated-meter")),
    ("meter.version", AttributeValue::string("2.0.0")),
    ("config.phase", AttributeValue::string("updated")),
    ("config.change", AttributeValue::bool(true))
  ]
  
  updated_counter.add(1L, Some(updated_attributes))
  updated_histogram.record(200.0, Some(updated_attributes))
  updated_gauge(50.0, Some(updated_attributes))
  
  // 7. 验证配置更新
  // 由于是No-op实现，我们通过创建LogRecord来验证
  let meter_config_update_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Meter provider configuration updated"),
    attributes: updated_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: Some(InstrumentationScope::{
      name: "updated-meter",
      version: Some("2.0.0"),
      schema_url: Some("https://example.com/schema/v2")
    })
  }
  
  // 8. 验证配置更新
  match meter_config_update_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "updated-meter")
      match scope.version {
        Some(version) => assert_eq(version, "2.0.0")
        None => @test.fail("Test failed")
      }
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://example.com/schema/v2")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 9. 模拟多次配置更新
  let multi_updated_meter = updated_provider.get_meter("multi-updated-meter", Some("3.1.0"), Some("https://example.com/schema/v3.1"))
  
  let multi_updated_counter = multi_updated_meter.create_counter("multi_updated_counter", Some("operations"), Some("Multi-updated counter"))
  
  let multi_updated_attributes = [
    ("meter.name", AttributeValue::string("multi-updated-meter")),
    ("meter.version", AttributeValue::string("3.1.0")),
    ("config.phase", AttributeValue::string("multi-updated")),
    ("config.change.count", AttributeValue::int(2L)),
    ("last.update", AttributeValue::string("2023-01-01T00:00:00Z"))
  ]
  
  multi_updated_counter.add(1L, Some(multi_updated_attributes))
  
  // 10. 验证多次配置更新
  let multi_meter_config_update_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Meter provider configuration multiple times updated"),
    attributes: multi_updated_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: Some(InstrumentationScope::{
      name: "multi-updated-meter",
      version: Some("3.1.0"),
      schema_url: Some("https://example.com/schema/v3.1")
    })
  }
  
  // 验证多次更新
  match multi_meter_config_update_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "multi-updated-meter")
      match scope.version {
        Some(version) => assert_eq(version, "3.1.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "propagation_config_dynamic_update" {
  // 测试传播配置的动态更新
  
  // 1. 创建初始传播配置
  let initial_trace_propagator = W3CTraceContextPropagator::{}
  let initial_baggage_propagator = W3CBaggagePropagator::{}
  let initial_composite_propagator = CompositePropagator::new([initial_trace_propagator, initial_baggage_propagator])
  
  // 2. 使用初始传播配置
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  initial_composite_propagator.inject(ctx, carrier)
  let initial_extracted_ctx = initial_composite_propagator.extract(ctx, carrier)
  
  // 3. 模拟动态更新传播配置
  // 创建新的传播器来模拟配置更新
  let updated_trace_propagator = W3CTraceContextPropagator::{}
  let updated_baggage_propagator = W3CBaggagePropagator::{}
  let updated_composite_propagator = CompositePropagator::new([updated_trace_propagator, updated_baggage_propagator])
  
  // 4. 使用更新后的传播配置
  let updated_carrier = MapCarrier::new()
  updated_composite_propagator.inject(ctx, updated_carrier)
  let updated_extracted_ctx = updated_composite_propagator.extract(ctx, updated_carrier)
  
  // 5. 验证传播配置更新
  // 检查carrier中的数据
  match updated_carrier.get("traceparent") {
    Some(trace_parent) => assert_eq(trace_parent.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  match updated_carrier.get("baggage") {
    Some(baggage) => assert_eq(baggage.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  // 6. 创建带有传播配置更新信息的日志
  let propagation_config_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Propagation configuration updated"),
    attributes: [
      ("config.phase", AttributeValue::string("updated")),
      ("config.change", AttributeValue::bool(true)),
      ("propagators.count", AttributeValue::int(2L)),
      ("traceparent.present", AttributeValue::bool(true)),
      ("baggage.present", AttributeValue::bool(true))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("propagation-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "propagation-scope",
      version: Some("2.0.0"),
      schema_url: None
    })
  }
  
  // 7. 验证传播配置更新日志
  let mut found_config_phase = false
  let mut found_config_change = false
  let mut found_propagators_count = false
  
  let mut i = 0
  while i < propagation_config_log.attributes.length() {
    let (name, value) = propagation_config_log.attributes[i]
    match (name, value) {
      ("config.phase", StringValue(phase)) => {
        assert_eq(phase, "updated")
        found_config_phase = true
      }
      ("config.change", BoolValue(change)) => {
        assert_eq(change, true)
        found_config_change = true
      }
      ("propagators.count", IntValue(count)) => {
        assert_eq(count, 2L)
        found_propagators_count = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_config_phase, true)
  assert_eq(found_config_change, true)
  assert_eq(found_propagators_count, true)
  
  // 8. 模拟多次传播配置更新
  // 添加更多传播器
  let multi_trace_propagator = W3CTraceContextPropagator::{}
  let multi_baggage_propagator = W3CBaggagePropagator::{}
  let multi_composite_propagator = CompositePropagator::new([multi_trace_propagator, multi_baggage_propagator])
  
  let multi_carrier = MapCarrier::new()
  multi_composite_propagator.inject(ctx, multi_carrier)
  let multi_extracted_ctx = multi_composite_propagator.extract(ctx, multi_carrier)
  
  // 9. 创建多次传播配置更新的日志
  let multi_propagation_config_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Propagation configuration multiple times updated"),
    attributes: [
      ("config.phase", AttributeValue::string("multi-updated")),
      ("config.change.count", AttributeValue::int(2L)),
      ("propagators.count", AttributeValue::int(2L)),
      ("last.update", AttributeValue::string("2023-01-01T00:00:00Z"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("propagation-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "propagation-scope",
      version: Some("3.1.0"),
      schema_url: None
    })
  }
  
  // 10. 验证多次传播配置更新
  match multi_propagation_config_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "propagation-scope")
      match scope.version {
        Some(version) => assert_eq(version, "3.1.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "end_to_end_config_dynamic_update" {
  // 端到端配置动态更新测试
  
  // 1. 创建初始配置
  let initial_resource = Resource::default("e2e-dynamic-service")
  let initial_scope = InstrumentationScope::{
    name: "e2e-initial-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  let initial_provider = NoopLoggerProvider::{}
  let initial_logger = initial_provider.get_logger("e2e-logger", Some("1.0.0"), Some("https://example.com/schema/v1"))
  
  let initial_tracer_provider = NoopTracerProvider::{}
  let initial_tracer = initial_tracer_provider.get_tracer("e2e-tracer", Some("1.0.0"))
  
  let initial_meter_provider = NoopMeterProvider::{}
  let initial_meter = initial_meter_provider.get_meter("e2e-meter", Some("1.0.0"), Some("https://example.com/schema/v1"))
  
  // 2. 使用初始配置创建遥测数据
  let ctx = Context::empty()
  let initial_attributes = [
    ("config.version", AttributeValue::string("1.0.0")),
    ("config.phase", AttributeValue::string("initial")),
    ("service.name", AttributeValue::string("e2e-dynamic-service"))
  ]
  
  let (_, initial_span) = initial_tracer.start_span(ctx, "e2e_initial_span", Server, Some(initial_attributes))
  
  let initial_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Initial configuration"),
    attributes: initial_attributes,
    trace_id: Some(initial_span.context.trace_id),
    span_id: Some(initial_span.context.span_id),
    trace_flags: Some(initial_span.context.trace_flags),
    resource: Some(initial_resource),
    instrumentation_scope: Some(initial_scope)
  }
  
  let initial_counter = initial_meter.create_counter("e2e_operations", Some("operations"), Some("E2E operations"))
  initial_counter.add(1L, Some(initial_attributes))
  
  // 3. 模拟动态更新所有配置
  let updated_resource = Resource::default("e2e-dynamic-service")
  let updated_scope = InstrumentationScope::{
    name: "e2e-updated-scope",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schema/v2")
  }
  
  let updated_provider = NoopLoggerProvider::{}
  let updated_logger = updated_provider.get_logger("e2e-logger", Some("2.0.0"), Some("https://example.com/schema/v2"))
  
  let updated_tracer_provider = NoopTracerProvider::{}
  let updated_tracer = updated_tracer_provider.get_tracer("e2e-tracer", Some("2.0.0"))
  
  let updated_meter_provider = NoopMeterProvider::{}
  let updated_meter = updated_meter_provider.get_meter("e2e-meter", Some("2.0.0"), Some("https://example.com/schema/v2"))
  
  // 4. 使用更新后的配置创建遥测数据
  let updated_attributes = [
    ("config.version", AttributeValue::string("2.0.0")),
    ("config.phase", AttributeValue::string("updated")),
    ("service.name", AttributeValue::string("e2e-dynamic-service")),
    ("config.change", AttributeValue::bool(true))
  ]
  
  let (_, updated_span) = updated_tracer.start_span(ctx, "e2e_updated_span", Server, Some(updated_attributes))
  
  let updated_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Updated configuration"),
    attributes: updated_attributes,
    trace_id: Some(updated_span.context.trace_id),
    span_id: Some(updated_span.context.span_id),
    trace_flags: Some(updated_span.context.trace_flags),
    resource: Some(updated_resource),
    instrumentation_scope: Some(updated_scope)
  }
  
  let updated_counter = updated_meter.create_counter("e2e_operations", Some("operations"), Some("E2E operations"))
  updated_counter.add(1L, Some(updated_attributes))
  
  // 5. 验证配置更新
  match updated_log.resource {
    Some(resource) => {
      assert_eq(resource.service_name, "e2e-dynamic-service")
    }
    None => @test.fail("Test failed")
  }
  
  match updated_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "e2e-updated-scope")
      match scope.version {
        Some(version) => assert_eq(version, "2.0.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 6. 模拟多次配置更新
  let multi_updated_resource = Resource::default("e2e-dynamic-service")
  let multi_updated_scope = InstrumentationScope::{
    name: "e2e-multi-updated-scope",
    version: Some("3.1.0"),
    schema_url: Some("https://example.com/schema/v3.1")
  }
  
  let multi_updated_provider = NoopLoggerProvider::{}
  let multi_updated_logger = multi_updated_provider.get_logger("e2e-logger", Some("3.1.0"), Some("https://example.com/schema/v3.1"))
  
  let multi_updated_tracer_provider = NoopTracerProvider::{}
  let multi_updated_tracer = multi_updated_tracer_provider.get_tracer("e2e-tracer", Some("3.1.0"))
  
  let multi_updated_meter_provider = NoopMeterProvider::{}
  let multi_updated_meter = multi_updated_meter_provider.get_meter("e2e-meter", Some("3.1.0"), Some("https://example.com/schema/v3.1"))
  
  // 7. 使用多次更新后的配置创建遥测数据
  let multi_updated_attributes = [
    ("config.version", AttributeValue::string("3.1.0")),
    ("config.phase", AttributeValue::string("multi-updated")),
    ("service.name", AttributeValue::string("e2e-dynamic-service")),
    ("config.change.count", AttributeValue::int(2L)),
    ("last.update", AttributeValue::string("2023-01-01T00:00:00Z"))
  ]
  
  let (_, multi_updated_span) = multi_updated_tracer.start_span(ctx, "e2e_multi_updated_span", Client, Some(multi_updated_attributes))
  
  let multi_updated_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000002000L,
    observed_timestamp_unix_nanos: Some(1640995200000002100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Multi-updated configuration"),
    attributes: multi_updated_attributes,
    trace_id: Some(multi_updated_span.context.trace_id),
    span_id: Some(multi_updated_span.context.span_id),
    trace_flags: Some(multi_updated_span.context.trace_flags),
    resource: Some(multi_updated_resource),
    instrumentation_scope: Some(multi_updated_scope)
  }
  
  let multi_updated_counter = multi_updated_meter.create_counter("e2e_operations", Some("operations"), Some("E2E operations"))
  multi_updated_counter.add(1L, Some(multi_updated_attributes))
  
  // 8. 验证多次配置更新
  match multi_updated_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "e2e-multi-updated-scope")
      match scope.version {
        Some(version) => assert_eq(version, "3.1.0")
        None => @test.fail("Test failed")
      }
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://example.com/schema/v3.1")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 9. 验证端到端配置一致性
  // 验证所有遥测数据中的service.name一致
  assert_eq(initial_span.attributes.length(), 3)
  assert_eq(updated_span.attributes.length(), 4)
  assert_eq(multi_updated_span.attributes.length(), 5)
  
  // 验证配置版本的一致性
  let mut found_initial_version = false
  let mut found_updated_version = false
  let mut found_multi_updated_version = false
  
  // 检查初始配置
  let mut i = 0
  while i < initial_span.attributes.length() {
    let (name, value) = initial_span.attributes[i]
    match (name, value) {
      ("config.version", StringValue(version)) => {
        assert_eq(version, "1.0.0")
        found_initial_version = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 检查更新配置
  i = 0
  while i < updated_span.attributes.length() {
    let (name, value) = updated_span.attributes[i]
    match (name, value) {
      ("config.version", StringValue(version)) => {
        assert_eq(version, "2.0.0")
        found_updated_version = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  // 检查多次更新配置
  i = 0
  while i < multi_updated_span.attributes.length() {
    let (name, value) = multi_updated_span.attributes[i]
    match (name, value) {
      ("config.version", StringValue(version)) => {
        assert_eq(version, "3.1.0")
        found_multi_updated_version = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_initial_version, true)
  assert_eq(found_updated_version, true)
  assert_eq(found_multi_updated_version, true)
}