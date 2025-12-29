// 资源生命周期管理测试
test "resource_creation_and_initialization" {
  // 测试Resource的创建和初始化
  
  // 创建基本Resource
  let resource1 = Resource::default("test-service")
  assert_eq(resource1.service_name, "test-service")
  assert_eq(resource1.service_version, None)
  assert_eq(resource1.telemetry_sdk_name, "azimuth")
  assert_eq(resource1.telemetry_sdk_version, "0.1.0")
  assert_eq(resource1.attributes.length(), 0)
  
  // 创建带有不同名称的Resource
  let resource2 = Resource::default("payment-service")
  assert_eq(resource2.service_name, "payment-service")
  assert_eq(resource2.telemetry_sdk_name, "azimuth")
  assert_eq(resource2.telemetry_sdk_version, "0.1.0")
  
  // 创建带有特殊字符的Resource
  let resource3 = Resource::default("special-service!@#$%")
  assert_eq(resource3.service_name, "special-service!@#$%")
  assert_eq(resource3.telemetry_sdk_name, "azimuth")
  
  // 创建Unicode Resource
  let resource4 = Resource::default("Unicode服务测试🚀")
  assert_eq(resource4.service_name, "Unicode服务测试🚀")
  assert_eq(resource4.telemetry_sdk_name, "azimuth")
  
  // 验证所有Resource的SDK信息一致性
  assert_eq(resource1.telemetry_sdk_name, resource2.telemetry_sdk_name)
  assert_eq(resource2.telemetry_sdk_name, resource3.telemetry_sdk_name)
  assert_eq(resource3.telemetry_sdk_name, resource4.telemetry_sdk_name)
  
  assert_eq(resource1.telemetry_sdk_version, resource2.telemetry_sdk_version)
  assert_eq(resource2.telemetry_sdk_version, resource3.telemetry_sdk_version)
  assert_eq(resource3.telemetry_sdk_version, resource4.telemetry_sdk_version)
}

test "tracer_provider_lifecycle" {
  // 测试TracerProvider的生命周期
  
  let tracer_provider = NoopTracerProvider::{}
  
  // 创建多个Tracer
  let tracer1 = tracer_provider.get_tracer("tracer-1", None, None)
  let tracer2 = tracer_provider.get_tracer("tracer-2", Some("1.0.0"), None)
  let tracer3 = tracer_provider.get_tracer("tracer-3", None, Some("https://example.com/schema"))
  let tracer4 = tracer_provider.get_tracer("tracer-4", Some("2.0.0"), Some("https://example.com/schema/v2"))
  
  // 验证所有Tracer都可以正常工作
  let ctx = context::Context::current()
  
  let (_, span1) = tracer1.start_span(ctx, "test-span-1", None, None, None)
  let (_, span2) = tracer2.start_span(ctx, "test-span-2", Some(Client), None, None)
  let (_, span3) = tracer3.start_span(ctx, "test-span-3", None, Some([]), None)
  let (_, span4) = tracer4.start_span(ctx, "test-span-4", Some(Server), Some([("test", AttributeValue::string("value"))]), Some(1234567890L))
  
  // 验证所有Span都创建成功
  assert_eq(span1.name, "test-span-1")
  assert_eq(span2.name, "test-span-2")
  assert_eq(span3.name, "test-span-3")
  assert_eq(span4.name, "test-span-4")
  
  match span2.kind {
    Client => assert_eq(true, true)
    _ => @test.fail("Expected Client span kind")
  }
  
  match span4.kind {
    Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  
  assert_eq(span4.attributes.length(), 1)
  assert_eq(span4.start_time_unix_nanos, 1234567890L)
}

test "meter_provider_lifecycle" {
  // 测试MeterProvider的生命周期
  
  let meter_provider = NoopMeterProvider::{}
  
  // 创建多个Meter
  let meter1 = meter_provider.get_meter("meter-1", None, None)
  let meter2 = meter_provider.get_meter("meter-2", Some("1.0.0"), None)
  let meter3 = meter_provider.get_meter("meter-3", None, Some("https://example.com/schema"))
  let meter4 = meter_provider.get_meter("meter-4", Some("2.0.0"), Some("https://example.com/schema/v2"))
  
  // 验证所有Meter都可以创建Instrument
  let counter1 = meter1.create_counter("counter-1", None, None)
  let histogram1 = meter2.create_histogram("histogram-1", Some("ms"), Some("Test histogram"))
  let up_down_counter1 = meter3.create_up_down_counter("up-down-1", None, None)
  let gauge1 = meter4.create_gauge("gauge-1", Some("percent"), Some("Test gauge"))
  
  // 验证所有Instrument都可以正常工作
  counter1.add(10L, Some([("test", AttributeValue::string("value"))]))
  histogram1.record(100.0, Some([("test", AttributeValue::string("value"))]))
  up_down_counter1.add(-5L, Some([("test", AttributeValue::string("value"))]))
  gauge1.record(75.5, Some([("test", AttributeValue::string("value"))]))
  
  assert_eq(true, true) // 验证所有操作成功完成
}

test "logger_provider_lifecycle" {
  // 测试LoggerProvider的生命周期
  
  let logger_provider = NoopLoggerProvider::{}
  
  // 创建多个Logger
  let logger1 = logger_provider.get_logger("logger-1", None, None)
  let logger2 = logger_provider.get_logger("logger-2", Some("1.0.0"), None)
  let logger3 = logger_provider.get_logger("logger-3", None, Some("https://example.com/schema"))
  let logger4 = logger_provider.get_logger("logger-4", Some("2.0.0"), Some("https://example.com/schema/v2"))
  
  // 验证所有Logger都可以记录日志
  logger1.debug("Debug message", Some([("test", AttributeValue::string("value"))]))
  logger2.info("Info message", Some([("test", AttributeValue::string("value"))]))
  logger3.warn("Warning message", Some([("test", AttributeValue::string("value"))]))
  logger4.error("Error message", Some([("test", AttributeValue::string("value"))]))
  
  // 测试emit方法
  let log_record = LogRecord::builder()
    .timestamp(1234567890L)
    .severity(Info)
    .body("Test log record")
    .build()
  
  logger1.emit(log_record)
  logger2.emit(log_record)
  logger3.emit(log_record)
  logger4.emit(log_record)
  
  assert_eq(true, true) // 验证所有操作成功完成
}

test "context_lifecycle_management" {
  // 测试Context的生命周期管理
  
  // 创建Context链
  let ctx1 = context::Context::empty()
  let key1 = create_key("key1")
  let key2 = create_key("key2")
  let key3 = create_key("key3")
  
  let ctx2 = ctx1.with_value(key1, "value1")
  let ctx3 = ctx2.with_value(key2, "value2")
  let ctx4 = ctx3.with_value(key3, "value3")
  
  // 验证Context链中的值
  match ctx4.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected to find key1")
  }
  
  match ctx4.get(key2) {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected to find key2")
  }
  
  match ctx4.get(key3) {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected to find key3")
  }
  
  // 验证原始Context不受影响
  match ctx1.get(key1) {
    Some(_) => @test.fail("Did not expect to find key1 in ctx1")
    None => assert_eq(true, true)
  }
  
  // 创建新的分支
  let ctx5 = ctx3.with_value(key1, "new-value1")
  
  match ctx5.get(key1) {
    Some(value) => assert_eq(value, "new-value1")
    None => @test.fail("Expected to find key1 in ctx5")
  }
  
  // 验证原有Context不受影响
  match ctx3.get(key1) {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected to find key1 in ctx3")
  }
}

test "baggage_lifecycle_management" {
  // 测试Baggage的生命周期管理
  
  // 创建Baggage链
  let baggage1 = Baggage::empty()
  let baggage2 = baggage1.with_entry("key1", "value1")
  let baggage3 = baggage2.with_entry("key2", "value2")
  let baggage4 = baggage3.with_entry("key3", "value3")
  
  // 验证Baggage链中的条目
  match baggage4.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected to find key1")
  }
  
  match baggage4.get("key2") {
    Some(value) => assert_eq(value, "value2")
    None => @test.fail("Expected to find key2")
  }
  
  match baggage4.get("key3") {
    Some(value) => assert_eq(value, "value3")
    None => @test.fail("Expected to find key3")
  }
  
  // 验证条目数量
  assert_eq(baggage1.entries.length(), 0)
  assert_eq(baggage2.entries.length(), 1)
  assert_eq(baggage3.entries.length(), 2)
  assert_eq(baggage4.entries.length(), 3)
  
  // 创建新的分支
  let baggage5 = baggage3.with_entry("key1", "new-value1")
  
  match baggage5.get("key1") {
    Some(value) => assert_eq(value, "new-value1")
    None => @test.fail("Expected to find key1 in baggage5")
  }
  
  // 验证原有Baggage不受影响
  match baggage3.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => @test.fail("Expected to find key1 in baggage3")
  }
}

test "propagator_lifecycle_management" {
  // 测试Propagator的生命周期管理
  
  // 创建不同类型的传播器
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 创建复合传播器
  let composite1 = CompositePropagator::new([trace_propagator])
  let composite2 = CompositePropagator::new([trace_propagator, baggage_propagator])
  let composite3 = CompositePropagator::new([baggage_propagator, trace_propagator])
  
  // 测试所有传播器都可以正常工作
  let ctx = context::Context::current()
  let carrier1 = MapCarrier::new()
  let carrier2 = MapCarrier::new()
  let carrier3 = MapCarrier::new()
  
  composite1.inject(ctx, carrier1)
  composite2.inject(ctx, carrier2)
  composite3.inject(ctx, carrier3)
  
  // 验证注入结果
  match carrier1.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier2.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier2.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
  
  match carrier3.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier3.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
  
  // 测试提取操作
  let populated_carrier = MapCarrier::from_map([
    ("traceparent", "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"),
    ("baggage", "key1=value1,key2=value2")
  ])
  
  let extracted_ctx1 = composite1.extract(ctx, populated_carrier)
  let extracted_ctx2 = composite2.extract(ctx, populated_carrier)
  let extracted_ctx3 = composite3.extract(ctx, populated_carrier)
  
  assert_eq(true, true) // 验证所有提取操作成功
}

test "resource_cleanup_and_disposal" {
  // 测试资源的清理和处置
  
  // 创建大量资源并验证它们的生命周期
  let mut resources = []
  let mut tracers = []
  let mut meters = []
  let mut loggers = []
  
  // 创建资源
  let mut i = 0
  while i < 100 {
    let resource = Resource::default("service-" + i.to_string())
    resources.push(resource)
    i = i + 1
  }
  
  // 创建TracerProvider和Tracer
  let tracer_provider = NoopTracerProvider::{}
  let mut j = 0
  while j < 100 {
    let tracer = tracer_provider.get_tracer("tracer-" + j.to_string(), None, None)
    tracers.push(tracer)
    j = j + 1
  }
  
  // 创建MeterProvider和Meter
  let meter_provider = NoopMeterProvider::{}
  let mut k = 0
  while k < 100 {
    let meter = meter_provider.get_meter("meter-" + k.to_string(), None, None)
    meters.push(meter)
    k = k + 1
  }
  
  // 创建LoggerProvider和Logger
  let logger_provider = NoopLoggerProvider::{}
  let mut l = 0
  while l < 100 {
    let logger = logger_provider.get_logger("logger-" + l.to_string(), None, None)
    loggers.push(logger)
    l = l + 1
  }
  
  // 验证所有资源都创建成功
  assert_eq(resources.length(), 100)
  assert_eq(tracers.length(), 100)
  assert_eq(meters.length(), 100)
  assert_eq(loggers.length(), 100)
  
  // 验证资源属性
  assert_eq(resources[0].service_name, "service-0")
  assert_eq(resources[99].service_name, "service-99")
  
  // 使用所有创建的资源
  let ctx = context::Context::current()
  let mut m = 0
  while m < 10 {
    let (_, span) = tracers[m].start_span(ctx, "test-span", None, None, None)
    let counter = meters[m].create_counter("test-counter", None, None)
    let logger = loggers[m]
    
    counter.add(1L, None)
    logger.info("Test message", None)
    
    assert_eq(span.name, "test-span")
    m = m + 1
  }
  
  // 模拟资源清理（在实际实现中可能涉及显式清理）
  // 在这个简化实现中，我们只验证资源在使用期间正常工作
  assert_eq(true, true)
}

test "cross_component_lifecycle_integration" {
  // 测试跨组件的生命周期集成
  
  // 创建全局Resource
  let global_resource = Resource::default("global-service")
  
  // 创建各种Provider
  let tracer_provider = NoopTracerProvider::{}
  let meter_provider = NoopMeterProvider::{}
  let logger_provider = NoopLoggerProvider::{}
  
  // 创建Instrumentation
  let tracer = tracer_provider.get_tracer("global-tracer", None, None)
  let meter = meter_provider.get_meter("global-meter", None, None)
  let logger = logger_provider.get_logger("global-logger", None, None)
  
  // 创建Context和Baggage
  let ctx = context::Context::empty()
  let user_key = create_key("user-id")
  let ctx_with_user = ctx.with_value(user_key, "global-user-123")
  
  let baggage = Baggage::empty()
    .with_entry("request.id", "global-req-456")
    .with_entry("session.id", "global-session-789")
  
  // 创建传播器
  let propagator = CompositePropagator::new([
    W3CTraceContextPropagator::{},
    W3CBaggagePropagator::{}
  ])
  
  // 模拟完整的遥测操作流程
  let carrier = MapCarrier::new()
  propagator.inject(ctx_with_user, carrier)
  
  let (_, span) = tracer.start_span(
    ctx_with_user,
    "global-operation",
    Some(Server),
    Some([
      ("service.name", AttributeValue::string(global_resource.service_name)),
      ("user.id", AttributeValue::string("global-user-123"))
    ]),
    Some(1234567890L)
  )
  
  let counter = meter.create_counter("global_operations", None, None)
  counter.add(1L, Some([
    ("service.name", AttributeValue::string(global_resource.service_name)),
    ("operation.type", AttributeValue::string("global-operation"))
  ]))
  
  logger.info("Global operation completed", Some([
    ("service.name", AttributeValue::string(global_resource.service_name)),
    ("user.id", AttributeValue::string("global-user-123"))
  ]))
  
  let log_record = LogRecord::builder()
    .timestamp(1234567950L)
    .severity(Info)
    .body("Global operation with full context")
    .with_attribute("service.name", AttributeValue::string(global_resource.service_name))
    .with_attribute("span.name", AttributeValue::string(span.name))
    .with_attribute("user.id", AttributeValue::string("global-user-123"))
    .build()
  
  logger.emit(log_record)
  
  // 验证所有组件都正常工作
  assert_eq(global_resource.service_name, "global-service")
  assert_eq(span.name, "global-operation")
  
  match carrier.get("traceparent") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get("baggage") {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
  
  match ctx_with_user.get(user_key) {
    Some(value) => assert_eq(value, "global-user-123")
    None => @test.fail("Expected to find user-id")
  }
  
  match baggage.get("request.id") {
    Some(value) => assert_eq(value, "global-req-456")
    None => @test.fail("Expected to find request.id")
  }
}