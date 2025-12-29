// 跨API模块集成测试 - 测试trace、logs、metrics之间的交互

test "trace_logs_correlation" {
  // 测试trace和logs的关联
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  
  // 创建span
  let (span_ctx, span) = tracer.start_span(ctx, "test-operation", trace::Internal)
  
  // 验证span的基本属性
  assert_eq(span.name, "test-operation")
  match span.kind {
    trace::Internal => assert_eq(true, true)
    _ => @test.fail("Expected Internal span kind")
  }
  
  // 创建与trace关联的日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(logs::Info)
    .body("Test log message")
    .with_attribute("trace.id", common::AttributeValue::string("test-trace-id"))
    .with_attribute("span.id", common::AttributeValue::string("test-span-id"))
    .build()
  
  // 验证日志记录的属性
  assert_eq(log_record.body, Some("Test log message"))
  match log_record.severity_number {
    logs::Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  assert_eq(log_record.attributes.length(), 2)
}

test "metrics_with_trace_context" {
  // 测试metrics与trace上下文的关联
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test-meter", "1.0.0")
  
  // 创建各种类型的指标仪器
  let counter = meter.create_counter("test-counter", "count", "Test counter")
  let histogram = meter.create_histogram("test-histogram", "ms", "Test histogram")
  let gauge = meter.create_gauge("test-gauge", "percent", "Test gauge")
  let up_down_counter = meter.create_up_down_counter("test-up-down", "count", "Test up-down counter")
  
  // 测试counter操作
  counter.add(1L, [("trace.id", common::AttributeValue::string("test-trace-id"))])
  
  // 测试histogram操作
  histogram.record(100.5, [("span.id", common::AttributeValue::string("test-span-id"))])
  
  // 测试gauge操作
  gauge.record(75.0, [("service.name", common::AttributeValue::string("test-service"))])
  
  // 测试up-down counter操作
  up_down_counter.add(-5L, [("operation.type", common::AttributeValue::string("test-operation"))])
  
  // 验证操作不会抛出异常（no-op实现）
  assert_eq(true, true)
}

test "context_propagation_across_apis" {
  // 测试上下文在不同API之间的传播
  let ctx = context::Context::empty()
  let key = context::create_key("test-key")
  
  // 在context中设置值
  let ctx_with_value = ctx.with_value(key, "test-value")
  
  // 验证值可以正确获取
  match ctx_with_value.get(key) {
    Some(value) => assert_eq(value, "test-value")
    None => @test.fail("Expected value to be found")
  }
  
  // 测试baggage的使用
  let baggage = context::Baggage::empty()
  let baggage_with_entry = baggage.with_entry("baggage-key", "baggage-value")
  
  match baggage_with_entry.get("baggage-key") {
    Some(value) => assert_eq(value, "baggage-value")
    None => @test.fail("Expected baggage value to be found")
  }
}

test "resource_sharing_across_apis" {
  // 测试资源在不同API之间的共享
  let resource = common::Resource::default("shared-test-service")
  
  // 验证资源的基本属性
  assert_eq(resource.service_name, "shared-test-service")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 创建带有资源的日志记录
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(logs::Info)
    .body("Log with shared resource")
    .build()
  
  // 创建带有资源的span
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test-tracer")
  let (span_ctx, span) = tracer.start_span(
    context::Context::empty(),
    "span-with-resource",
    trace::Internal,
    [("resource.name", common::AttributeValue::string(resource.service_name))]
  )
  
  // 验证资源信息正确关联
  assert_eq(log_record.body, Some("Log with shared resource"))
  assert_eq(span.name, "span-with-resource")
  match span.attributes[0].1 {
    common::StringValue(name) => assert_eq(name, "shared-test-service")
    _ => @test.fail("Expected StringValue")
  }
}

test "propagation_with_trace_and_logs" {
  // 测试传播机制与trace和logs的集成
  let carrier = propagation::MapCarrier::new()
  let ctx = context::Context::empty()
  
  // 创建W3C传播器
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  // 注入trace和baggage信息
  trace_propagator.inject(ctx, carrier)
  baggage_propagator.inject(ctx, carrier)
  
  // 验证注入的头部信息
  match carrier.get(propagation::TRACE_PARENT_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected traceparent header")
  }
  
  match carrier.get(propagation::BAGGAGE_HEADER) {
    Some(_) => assert_eq(true, true)
    None => @test.fail("Expected baggage header")
  }
  
  // 提取信息到新的context
  let extracted_ctx = trace_propagator.extract(ctx, carrier)
  let final_ctx = baggage_propagator.extract(extracted_ctx, carrier)
  
  // 验证提取操作不会失败
  assert_eq(true, true)
}

test "composite_propagator_integration" {
  // 测试复合传播器与多个API的集成
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  let composite = propagation::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  let carrier = propagation::MapCarrier::new()
  let ctx = context::Context::empty()
  
  // 使用复合传播器注入
  composite.inject(ctx, carrier)
  
  // 验证所有预期的头部都被注入
  let keys = carrier.keys()
  assert_eq(keys.length(), 2)
  
  // 使用复合传播器提取
  let extracted_ctx = composite.extract(ctx, carrier)
  
  // 验证提取操作成功
  assert_eq(true, true)
}

test "end_to_end_telemetry_flow" {
  // 测试完整的遥测数据流
  let resource = common::Resource::default("e2e-test-service")
  let ctx = context::Context::empty()
  
  // 1. 创建trace
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("e2e-tracer")
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "e2e-operation",
    trace::Server,
    [("operation.type", common::AttributeValue::string("http"))]
  )
  
  // 2. 创建metrics
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("e2e-meter")
  let counter = meter.create_counter("http.requests.total", "count", "Total HTTP requests")
  let histogram = meter.create_histogram("http.request.duration", "ms", "HTTP request duration")
  
  // 记录metrics
  counter.add(1L, [
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("200"))
  ])
  histogram.record(150.5, [
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("200"))
  ])
  
  // 3. 创建logs
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("e2e-logger", "1.0.0")
  
  let log_record = logs::LogRecord::builder()
    .timestamp(1234567890L)
    .severity(logs::Info)
    .body("HTTP request processed successfully")
    .with_attribute("method", common::AttributeValue::string("GET"))
    .with_attribute("status", common::AttributeValue::string("200"))
    .with_attribute("duration", common::AttributeValue::float(150.5))
    .build()
  
  // 验证所有遥测数据都正确创建
  assert_eq(span.name, "e2e-operation")
  match span.kind {
    trace::Server => assert_eq(true, true)
    _ => @test.fail("Expected Server span kind")
  }
  
  assert_eq(log_record.body, Some("HTTP request processed successfully"))
  match log_record.severity_number {
    logs::Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  
  assert_eq(log_record.attributes.length(), 3)
}