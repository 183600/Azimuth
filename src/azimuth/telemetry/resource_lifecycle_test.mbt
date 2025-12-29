// 资源生命周期管理测试 - 测试资源的创建、使用和销毁
use azimuth.telemetry.api.common.{AttributeValue, Resource}
use azimuth.telemetry.api.trace.{SpanContext, Span, SpanKind, StatusCode, SpanEvent, SpanLink, NoopTracer, NoopTracerProvider}
use azimuth.telemetry.api.metrics.{NoopMeter, NoopMeterProvider}
use azimuth.telemetry.api.logs.{SeverityNumber, LogRecordBuilder, NoopLogger, NoopLoggerProvider}
use azimuth.telemetry.api.context.{Context, ContextKey, Baggage, create_key}

test "resource_lifecycle_creation" {
  // 测试资源的创建阶段
  
  // 创建基本资源
  let basic_resource = Resource::default("basic-service")
  assert_eq(basic_resource.service_name, "basic-service")
  assert_eq(basic_resource.telemetry_sdk_name, "azimuth")
  assert_eq(basic_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(basic_resource.service_version, None)
  assert_eq(basic_resource.attributes.length(), 0)
  
  // 创建带有不同服务名称的资源
  let service_names = [
    "web-service",
    "api-service", 
    "database-service",
    "cache-service",
    "queue-service"
  ]
  
  let resources = []
  let mut i = 0
  while i < service_names.length() {
    let resource = Resource::default(service_names[i])
    resources.push(resource)
    
    // 验证每个资源都正确创建
    assert_eq(resources[i].service_name, service_names[i])
    assert_eq(resources[i].telemetry_sdk_name, "azimuth")
    assert_eq(resources[i].telemetry_sdk_version, "0.1.0")
    
    i = i + 1
  }
  
  // 验证所有资源都成功创建
  assert_eq(resources.length(), 5)
}

test "resource_lifecycle_usage" {
  // 测试资源的使用阶段
  
  let resource = Resource::default("usage-test-service")
  
  // 在span中使用资源
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("usage-test", Some("1.0.0"))
  
  let resource_attributes = [
    ("service.name", AttributeValue::string(resource.service_name)),
    ("sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
    ("sdk.version", AttributeValue::string(resource.telemetry_sdk_version))
  ]
  
  let (_, span) = tracer.start_span(
    ctx, 
    "resource_usage_span", 
    Internal, 
    Some(resource_attributes)
  )
  
  // 验证资源在span中的使用
  assert_eq(span.attributes.length(), 3)
  assert_eq(span.attributes[0].0, "service.name")
  assert_eq(span.attributes[1].0, "sdk.name")
  assert_eq(span.attributes[2].0, "sdk.version")
  
  // 在指标中使用资源
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("usage-metrics", Some("1.0.0"))
  let counter = meter.create_counter("resource.operations", Some("count"), Some("Operations using resource"))
  
  counter.add(1L, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("sdk.name", AttributeValue::string(resource.telemetry_sdk_name))
  ]))
  
  // 在日志中使用资源
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("usage-logger", Some("1.0.0"))
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Resource usage test")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .with_attribute("sdk.version", AttributeValue::string(resource.telemetry_sdk_version))
    .build()
  
  logger.emit(log_record)
  
  // 验证资源在所有遥测信号中的正确使用
  assert_eq(resource.service_name, "usage-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
}

test "resource_lifecycle_sharing" {
  // 测试资源的共享使用
  
  let shared_resource = Resource::default("shared-service")
  
  // 多个组件共享同一个资源
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("sharing-test", Some("1.0.0"))
  
  // 模拟5个不同的组件使用共享资源
  let component_names = ["web", "api", "database", "cache", "queue"]
  let spans = []
  
  let mut i = 0
  while i < component_names.length() {
    let component_attributes = [
      ("service.name", AttributeValue::string(shared_resource.service_name)),
      ("sdk.name", AttributeValue::string(shared_resource.telemetry_sdk_name)),
      ("component.name", AttributeValue::string(component_names[i])),
      ("resource.shared", AttributeValue::bool(true))
    ]
    
    let (_, span) = tracer.start_span(
      ctx, 
      component_names[i] + "_component_span", 
      Internal, 
      Some(component_attributes)
    )
    
    spans.push(span)
    i = i + 1
  }
  
  // 验证所有组件都正确使用了共享资源
  assert_eq(spans.length(), 5)
  
  let mut j = 0
  while j < spans.length() {
    assert_eq(spans[j].attributes.length(), 4)
    assert_eq(spans[j].attributes[0].0, "service.name")
    assert_eq(spans[j].attributes[1].0, "sdk.name")
    assert_eq(spans[j].attributes[2].0, "component.name")
    assert_eq(spans[j].attributes[3].0, "resource.shared")
    
    // 验证共享资源信息一致
    match spans[j].attributes[0].1 {
      StringValue(service_name) => assert_eq(service_name, shared_resource.service_name)
      _ => @test.fail("Test failed")
    }
    
    match spans[j].attributes[1].1 {
      StringValue(sdk_name) => assert_eq(sdk_name, shared_resource.telemetry_sdk_name)
      _ => @test.fail("Test failed")
    }
    
    j = j + 1
  }
}

test "resource_lifecycle_context_propagation" {
  // 测试资源在上下文传播中的生命周期
  
  let resource = Resource::default("context-propagation-service")
  
  // 创建带有资源信息的上下文
  let ctx = Context::empty()
  let resource_key = create_key("resource.service_name")
  let sdk_key = create_key("resource.sdk_name")
  let version_key = create_key("resource.sdk_version")
  
  let ctx_with_resource = ctx
    .with_value(resource_key, resource.service_name)
    .with_value(sdk_key, resource.telemetry_sdk_name)
    .with_value(version_key, resource.telemetry_sdk_version)
  
  // 在上下文传播中使用资源
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("context-propagation-test", Some("1.0.0"))
  
  let (_, span) = tracer.start_span(
    ctx_with_resource, 
    "context_propagation_span", 
    Server, 
    Some([
      ("service.name", AttributeValue::string(resource.service_name)),
      ("context.propagated", AttributeValue::bool(true))
    ])
  )
  
  // 验证资源信息在上下文中的传播
  match ctx_with_resource.get(resource_key) {
    Some(service_name) => assert_eq(service_name, resource.service_name)
    None => @test.fail("Test failed")
  }
  
  match ctx_with_resource.get(sdk_key) {
    Some(sdk_name) => assert_eq(sdk_name, resource.telemetry_sdk_name)
    None => @test.fail("Test failed")
  }
  
  match ctx_with_resource.get(version_key) {
    Some(version) => assert_eq(version, resource.telemetry_sdk_version)
    None => @test.fail("Test failed")
  }
  
  // 验证span正确使用了资源信息
  assert_eq(span.attributes.length(), 2)
  match span.attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, resource.service_name)
    _ => @test.fail("Test failed")
  }
}

test "resource_lifecycle_baggage_integration" {
  // 测试资源在Baggage中的集成
  
  let resource = Resource::default("baggage-integration-service")
  
  // 创建带有资源信息的baggage
  let baggage = Baggage::empty()
    .with_entry("service.name", resource.service_name)
    .with_entry("sdk.name", resource.telemetry_sdk_name)
    .with_entry("sdk.version", resource.telemetry_sdk_version)
    .with_entry("resource.type", "telemetry")
  
  // 在span中使用baggage中的资源信息
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("baggage-integration-test", Some("1.0.0"))
  
  let baggage_attributes = [
    ("service.name", AttributeValue::string(resource.service_name)),
    ("sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
    ("sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),
    ("resource.type", AttributeValue::string("telemetry")),
    ("baggage.integrated", AttributeValue::bool(true))
  ]
  
  let (_, span) = tracer.start_span(
    ctx, 
    "baggage_integration_span", 
    Client, 
    Some(baggage_attributes)
  )
  
  // 验证baggage中的资源信息
  match baggage.get("service.name") {
    Some(service_name) => assert_eq(service_name, resource.service_name)
    None => @test.fail("Test failed")
  }
  
  match baggage.get("sdk.name") {
    Some(sdk_name) => assert_eq(sdk_name, resource.telemetry_sdk_name)
    None => @test.fail("Test failed")
  }
  
  match baggage.get("sdk.version") {
    Some(version) => assert_eq(version, resource.telemetry_sdk_version)
    None => @test.fail("Test failed")
  }
  
  match baggage.get("resource.type") {
    Some(resource_type) => assert_eq(resource_type, "telemetry")
    None => @test.fail("Test failed")
  }
  
  // 验证span正确使用了baggage中的资源信息
  assert_eq(span.attributes.length(), 5)
  assert_eq(span.attributes[4].0, "baggage.integrated")
  match span.attributes[4].1 {
    BoolValue(integrated) => assert_eq(integrated, true)
    _ => @test.fail("Test failed")
  }
}

test "resource_lifecycle_complex_scenarios" {
  // 测试资源在复杂场景中的生命周期
  
  let resource = Resource::default("complex-scenario-service")
  
  // 创建复杂的资源使用场景
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("complex-scenario-test", Some("1.0.0"))
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("complex-scenario-metrics", Some("1.0.0"))
  let logger_provider = NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("complex-scenario-logger", Some("1.0.0"))
  
  // 1. 创建带有复杂属性的span
  let complex_span_attributes = [
    ("service.name", AttributeValue::string(resource.service_name)),
    ("sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
    ("sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),
    ("operation.type", AttributeValue::string("complex_processing")),
    ("complex.scenario", AttributeValue::bool(true)),
    ("nested.data", AttributeValue::array_string(["level1", "level2", "level3"])),
    ("metrics.count", AttributeValue::int(42L)),
    ("processing.time", AttributeValue::float(123.45))
  ]
  
  let (_, complex_span) = tracer.start_span(
    ctx, 
    "complex_scenario_span", 
    Server, 
    Some(complex_span_attributes)
  )
  
  // 2. 记录多种类型的指标
  let counter = meter.create_counter("complex.operations", Some("count"), Some("Complex operations"))
  let histogram = meter.create_histogram("complex.duration", Some("ms"), Some("Complex operation duration"))
  let gauge = meter.create_gauge("complex.memory", Some("MB"), Some("Complex memory usage"))
  
  counter.add(1L, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.type", AttributeValue::string("complex_processing"))
  ]))
  
  histogram.record(250.5, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("operation.phase", AttributeValue::string("processing"))
  ]))
  
  gauge.record(512.0, Some([
    ("service.name", AttributeValue::string(resource.service_name)),
    ("memory.type", AttributeValue::string("heap"))
  ]))
  
  // 3. 记录复杂的日志
  let complex_log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(SeverityNumber::Info)
    .body("Complex scenario operation completed")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .with_attribute("sdk.name", AttributeValue::string(resource.telemetry_sdk_name))
    .with_attribute("operation.type", AttributeValue::string("complex_processing"))
    .with_attribute("span.name", AttributeValue::string(complex_span.name))
    .with_attribute("metrics.recorded", AttributeValue::bool(true))
    .with_attribute("logs.emitted", AttributeValue::bool(true))
    .with_attribute("complex.scenario", AttributeValue::bool(true))
    .build()
  
  logger.emit(complex_log_record)
  
  // 4. 创建上下文和baggage传播
  let context_key = create_key("complex.context.key")
  let ctx_with_complex = ctx.with_value(context_key, "complex.context.value")
  
  let complex_baggage = Baggage::empty()
    .with_entry("service.name", resource.service_name)
    .with_entry("scenario.type", "complex")
    .with_entry("processing.stage", "complete")
  
  // 验证复杂场景中的资源使用
  assert_eq(complex_span.name, "complex_scenario_span")
  assert_eq(complex_span.kind, Server)
  assert_eq(complex_span.attributes.length(), 8)
  
  match ctx_with_complex.get(context_key) {
    Some(value) => assert_eq(value, "complex.context.value")
    None => @test.fail("Test failed")
  }
  
  match complex_baggage.get("scenario.type") {
    Some(scenario_type) => assert_eq(scenario_type, "complex")
    None => @test.fail("Test failed")
  }
  
  // 验证日志记录的完整性
  assert_eq(complex_log_record.severity_number, SeverityNumber::Info)
  match complex_log_record.body {
    Some(body_text) => assert_eq(body_text, "Complex scenario operation completed")
    None => @test.fail("Test failed")
  }
  assert_eq(complex_log_record.attributes.length(), 8)
}

test "resource_lifecycle_edge_cases" {
  // 测试资源生命周期的边界情况
  
  // 测试空服务名称的资源
  let empty_service_resource = Resource::default("")
  assert_eq(empty_service_resource.service_name, "")
  assert_eq(empty_service_resource.telemetry_sdk_name, "azimuth")
  
  // 测试非常长服务名称的资源
  let long_name = "a" * 1000
  let long_name_resource = Resource::default(long_name)
  assert_eq(long_name_resource.service_name.length(), 1000)
  
  // 测试包含特殊字符服务名称的资源
  let special_name = "service!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"
  let special_name_resource = Resource::default(special_name)
  assert_eq(special_name_resource.service_name, special_name)
  
  // 测试Unicode服务名称的资源
  let unicode_name = "Unicode服务测试🚀🌟💫"
  let unicode_name_resource = Resource::default(unicode_name)
  assert_eq(unicode_name_resource.service_name, unicode_name)
  
  // 验证所有边界情况下的资源都能正常使用
  let edge_case_resources = [
    empty_service_resource,
    long_name_resource,
    special_name_resource,
    unicode_name_resource
  ]
  
  let ctx = Context::empty()
  let tracer_provider = NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("edge-case-test", Some("1.0.0"))
  
  let mut i = 0
  while i < edge_case_resources.length() {
    let resource = edge_case_resources[i]
    let resource_attributes = [
      ("service.name", AttributeValue::string(resource.service_name)),
      ("sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
      ("edge.case", AttributeValue::bool(true))
    ]
    
    let (_, span) = tracer.start_span(
      ctx, 
      "edge_case_span_" + i.to_string(), 
      Internal, 
      Some(resource_attributes)
    )
    
    // 验证边界情况下的资源都能正确使用
    assert_eq(span.attributes.length(), 3)
    match span.attributes[0].1 {
      StringValue(service_name) => assert_eq(service_name, resource.service_name)
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
}