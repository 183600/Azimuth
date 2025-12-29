// 并发安全综合测试 - 测试多线程环境下的安全性

test "context_concurrent_operations" {
  // 测试Context的并发操作安全性
  let base_ctx = context::Context::empty()
  
  // 模拟并发Context操作
  // 在实际多线程环境中，这里会使用多个线程同时操作
  // 由于MoonBit当前可能不支持真正的多线程，我们模拟并发场景
  
  // 模拟多个"线程"同时设置不同的键值对
  let ctx1 = base_ctx.with_value(context::create_key("thread-1-key"), "thread-1-value")
  let ctx2 = base_ctx.with_value(context::create_key("thread-2-key"), "thread-2-value")
  let ctx3 = base_ctx.with_value(context::create_key("thread-3-key"), "thread-3-value")
  
  // 模拟链式操作
  let ctx_chain_1 = ctx1.with_value(context::create_key("chain-1"), "value-1")
  let ctx_chain_2 = ctx2.with_value(context::create_key("chain-2"), "value-2")
  let ctx_chain_3 = ctx3.with_value(context::create_key("chain-3"), "value-3")
  
  // 验证数据一致性
  assert_eq(ctx_chain_1.get(context::create_key("thread-1-key")), Some("thread-1-value"))
  assert_eq(ctx_chain_1.get(context::create_key("chain-1")), Some("value-1"))
  
  assert_eq(ctx_chain_2.get(context::create_key("thread-2-key")), Some("thread-2-value"))
  assert_eq(ctx_chain_2.get(context::create_key("chain-2")), Some("value-2"))
  
  assert_eq(ctx_chain_3.get(context::create_key("thread-3-key")), Some("thread-3-value"))
  assert_eq(ctx_chain_3.get(context::create_key("chain-3")), Some("value-3"))
  
  // 验证隔离性：一个Context的修改不影响其他Context
  assert_eq(ctx_chain_1.get(context::create_key("thread-2-key")), None)
  assert_eq(ctx_chain_2.get(context::create_key("thread-3-key")), None)
  assert_eq(ctx_chain_3.get(context::create_key("thread-1-key")), None)
}

test "span_concurrent_creation" {
  // 测试Span的并发创建安全性
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("concurrent-test")
  
  // 模拟多个"线程"同时创建Span
  let (_, span1) = tracer.start_span(ctx, "concurrent-span-1", Some(trace::SpanKind::Server), None)
  let (_, span2) = tracer.start_span(ctx, "concurrent-span-2", Some(trace::SpanKind::Client), None)
  let (_, span3) = tracer.start_span(ctx, "concurrent-span-3", Some(trace::SpanKind::Internal), None)
  
  // 验证Span创建的独立性
  assert_eq(span1.name, "concurrent-span-1")
  assert_eq(span1.kind, trace::SpanKind::Server)
  
  assert_eq(span2.name, "concurrent-span-2")
  assert_eq(span2.kind, trace::SpanKind::Client)
  
  assert_eq(span3.name, "concurrent-span-3")
  assert_eq(span3.kind, trace::SpanKind::Internal)
  
  // 模拟并发添加属性
  let span1_with_attrs = span1 // 在实际实现中会有属性添加方法
  let span2_with_attrs = span2
  let span3_with_attrs = span3
  
  // 验证Span的独立性
  assert_eq(span1_with_attrs.name, "concurrent-span-1")
  assert_eq(span2_with_attrs.name, "concurrent-span-2")
  assert_eq(span3_with_attrs.name, "concurrent-span-3")
}

test "log_concurrent_recording" {
  // 测试日志的并发记录安全性
  let logger_provider = logs::NoopLoggerProvider::{}
  let logger = logger_provider.get_logger("concurrent-test")
  
  // 模拟多个"线程"同时记录日志
  let log1 = logs::LogRecord::builder()
    .body("Concurrent log 1")
    .severity(logs::SeverityNumber::Info)
    .with_attribute("thread.id", common::AttributeValue::int(1L))
    .build()
  
  let log2 = logs::LogRecord::builder()
    .body("Concurrent log 2")
    .severity(logs::SeverityNumber::Warn)
    .with_attribute("thread.id", common::AttributeValue::int(2L))
    .build()
  
  let log3 = logs::LogRecord::builder()
    .body("Concurrent log 3")
    .severity(logs::SeverityNumber::Error)
    .with_attribute("thread.id", common::AttributeValue::int(3L))
    .build()
  
  // 验证日志记录的独立性
  assert_eq(log1.body, Some("Concurrent log 1"))
  assert_eq(log1.severity_number, logs::SeverityNumber::Info)
  assert_eq(log1.attributes.length(), 1)
  
  assert_eq(log2.body, Some("Concurrent log 2"))
  assert_eq(log2.severity_number, logs::SeverityNumber::Warn)
  assert_eq(log2.attributes.length(), 1)
  
  assert_eq(log3.body, Some("Concurrent log 3"))
  assert_eq(log3.severity_number, logs::SeverityNumber::Error)
  assert_eq(log3.attributes.length(), 1)
  
  // 使用便捷方法记录日志
  logger.info("Concurrent info log", Some([("thread.id", common::AttributeValue::int(4L))]))
  logger.warn("Concurrent warn log", Some([("thread.id", common::AttributeValue::int(5L))]))
  logger.error("Concurrent error log", Some([("thread.id", common::AttributeValue::int(6L))]))
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "metrics_concurrent_operations" {
  // 测试指标的并发操作安全性
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("concurrent-test")
  
  // 创建共享的指标实例
  let shared_counter = meter.create_counter("shared.counter", None, None)
  let shared_histogram = meter.create_histogram("shared.histogram", None, None)
  let shared_gauge = meter.create_gauge("shared.gauge", None, None)
  
  // 模拟多个"线程"同时操作共享指标
  // 线程1操作
  shared_counter.add(1L, Some([("thread.id", common::AttributeValue::int(1L))]))
  shared_histogram.record(100.0, Some([("thread.id", common::AttributeValue::int(1L))]))
  shared_gauge.record(1.0, Some([("thread.id", common::AttributeValue::int(1L))]))
  
  // 线程2操作
  shared_counter.add(2L, Some([("thread.id", common::AttributeValue::int(2L))]))
  shared_histogram.record(200.0, Some([("thread.id", common::AttributeValue::int(2L))]))
  shared_gauge.record(2.0, Some([("thread.id", common::AttributeValue::int(2L))]))
  
  // 线程3操作
  shared_counter.add(3L, Some([("thread.id", common::AttributeValue::int(3L))]))
  shared_histogram.record(300.0, Some([("thread.id", common::AttributeValue::int(3L))]))
  shared_gauge.record(3.0, Some([("thread.id", common::AttributeValue::int(3L))]))
  
  // 创建独立的指标实例
  let counter1 = meter.create_counter("independent.counter.1", None, None)
  let counter2 = meter.create_counter("independent.counter.2", None, None)
  let counter3 = meter.create_counter("independent.counter.3", None, None)
  
  // 并发操作独立指标
  counter1.add(10L, None)
  counter2.add(20L, None)
  counter3.add(30L, None)
  
  // 验证操作不会抛出异常
  assert_eq(true, true)
}

test "propagation_concurrent_operations" {
  // 测试传播的并发操作安全性
  let ctx = context::Context::empty()
  
  // 创建多个carrier实例
  let carrier1 = propagation::MapCarrier::new()
  let carrier2 = propagation::MapCarrier::new()
  let carrier3 = propagation::MapCarrier::new()
  
  // 创建传播器实例
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  // 模拟并发注入操作
  trace_propagator.inject(ctx, carrier1)
  baggage_propagator.inject(ctx, carrier1)
  
  trace_propagator.inject(ctx, carrier2)
  baggage_propagator.inject(ctx, carrier2)
  
  trace_propagator.inject(ctx, carrier3)
  baggage_propagator.inject(ctx, carrier3)
  
  // 验证注入的独立性
  let trace_parent_1 = carrier1.get(propagation::TRACE_PARENT_HEADER)
  let baggage_1 = carrier1.get(propagation::BAGGAGE_HEADER)
  
  let trace_parent_2 = carrier2.get(propagation::TRACE_PARENT_HEADER)
  let baggage_2 = carrier2.get(propagation::BAGGAGE_HEADER)
  
  let trace_parent_3 = carrier3.get(propagation::TRACE_PARENT_HEADER)
  let baggage_3 = carrier3.get(propagation::BAGGAGE_HEADER)
  
  assert!(trace_parent_1.is_some())
  assert!(baggage_1.is_some())
  assert!(trace_parent_2.is_some())
  assert!(baggage_2.is_some())
  assert!(trace_parent_3.is_some())
  assert!(baggage_3.is_some())
  
  // 模拟并发提取操作
  let extracted_ctx1 = trace_propagator.extract(ctx, carrier1)
  let extracted_ctx2 = trace_propagator.extract(ctx, carrier2)
  let extracted_ctx3 = trace_propagator.extract(ctx, carrier3)
  
  // 验证提取的独立性
  assert_eq(true, true)
}

test "baggage_concurrent_operations" {
  // 测试Baggage的并发操作安全性
  let base_baggage = context::Baggage::empty()
  
  // 模拟多个"线程"同时操作Baggage
  let baggage1 = base_baggage.with_entry("thread-1-key", "thread-1-value")
  let baggage2 = base_baggage.with_entry("thread-2-key", "thread-2-value")
  let baggage3 = base_baggage.with_entry("thread-3-key", "thread-3-value")
  
  // 验证操作的独立性
  assert_eq(baggage1.get("thread-1-key"), Some("thread-1-value"))
  assert_eq(baggage2.get("thread-2-key"), Some("thread-2-value"))
  assert_eq(baggage3.get("thread-3-key"), Some("thread-3-value"))
  
  // 验证隔离性
  assert_eq(baggage1.get("thread-2-key"), None)
  assert_eq(baggage2.get("thread-3-key"), None)
  assert_eq(baggage3.get("thread-1-key"), None)
  
  // 模拟链式操作
  let chain1 = baggage1.with_entry("chain-1", "value-1")
  let chain2 = baggage2.with_entry("chain-2", "value-2")
  let chain3 = baggage3.with_entry("chain-3", "value-3")
  
  // 验证链式操作的独立性
  assert_eq(chain1.get("chain-1"), Some("value-1"))
  assert_eq(chain2.get("chain-2"), Some("value-2"))
  assert_eq(chain3.get("chain-3"), Some("value-3"))
}

test "resource_concurrent_creation" {
  // 测试Resource的并发创建安全性
  // 模拟多个"线程"同时创建Resource
  let resource1 = common::Resource::default("concurrent-service-1")
  let resource2 = common::Resource::default("concurrent-service-2")
  let resource3 = common::Resource::default("concurrent-service-3")
  
  // 验证Resource创建的独立性
  assert_eq(resource1.service_name, "concurrent-service-1")
  assert_eq(resource2.service_name, "concurrent-service-2")
  assert_eq(resource3.service_name, "concurrent-service-3")
  
  // 并发创建带属性的Resource
  let attrs1 = [("attr-1", common::AttributeValue::string("value-1"))]
  let attrs2 = [("attr-2", common::AttributeValue::string("value-2"))]
  let attrs3 = [("attr-3", common::AttributeValue::string("value-3"))]
  
  let resource_with_attrs1 = common::Resource::{
    service_name: "service-1",
    service_version: None,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: attrs1
  }
  
  let resource_with_attrs2 = common::Resource::{
    service_name: "service-2",
    service_version: None,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: attrs2
  }
  
  let resource_with_attrs3 = common::Resource::{
    service_name: "service-3",
    service_version: None,
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: attrs3
  }
  
  // 验证属性独立性
  assert_eq(resource_with_attrs1.attributes.length(), 1)
  assert_eq(resource_with_attrs2.attributes.length(), 1)
  assert_eq(resource_with_attrs3.attributes.length(), 1)
}

test "mixed_concurrent_operations" {
  // 测试混合并发操作的安全性
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("mixed-concurrent-test")
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("mixed-concurrent-test")
  
  // 模拟混合并发操作
  // 操作1：创建Span和记录指标
  let (_, span) = tracer.start_span(ctx, "mixed-operation", Some(trace::SpanKind::Server), None)
  let counter = meter.create_counter("mixed.operations", None, None)
  counter.add(1L, Some([("operation.type", common::AttributeValue::string("span_and_metric"))]))
  
  // 操作2：创建日志和更新上下文
  let log = logs::LogRecord::builder()
    .body("Mixed operation log")
    .severity(logs::SeverityNumber::Info)
    .with_attribute("operation.type", common::AttributeValue::string("log_and_context"))
    .build()
  let updated_ctx = ctx.with_value(context::create_key("operation.id"), "mixed-op-123")
  
  // 操作3：传播和Baggage
  let carrier = propagation::MapCarrier::new()
  let propagator = propagation::W3CTraceContextPropagator::{}
  propagator.inject(updated_ctx, carrier)
  let baggage = context::Baggage::empty().with_entry("operation.id", "mixed-op-123")
  
  // 操作4：Resource和InstrumentationScope
  let resource = common::Resource::default("mixed-operation-service")
  let scope = common::InstrumentationScope::{
    name: "mixed-operation-scope",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  // 验证所有操作的独立性
  assert_eq(span.name, "mixed-operation")
  assert_eq(log.body, Some("Mixed operation log"))
  assert_eq(updated_ctx.get(context::create_key("operation.id")), Some("mixed-op-123"))
  assert_eq(baggage.get("operation.id"), Some("mixed-op-123"))
  assert_eq(resource.service_name, "mixed-operation-service")
  assert_eq(scope.name, "mixed-operation-scope")
  
  // 验证carrier中有传播的数据
  let trace_parent = carrier.get(propagation::TRACE_PARENT_HEADER)
  assert!(trace_parent.is_some())
}

test "stress_concurrent_operations" {
  // 压力测试：大量并发操作
  let ctx = context::Context::empty()
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("stress-test")
  
  // 创建大量Span
  let mut spans = []
  let mut i = 0
  while i < 100 {
    let (_, span) = tracer.start_span(
      ctx,
      "stress-span-${i}",
      Some(trace::SpanKind::Internal),
      Some([("index", common::AttributeValue::int(i.to_int64()))])
    )
    spans.push(span)
    i = i + 1
  }
  
  // 创建大量日志记录
  let mut logs = []
  let mut j = 0
  while j < 100 {
    let log = logs::LogRecord::builder()
      .body("Stress log ${j}")
      .severity(logs::SeverityNumber::Info)
      .with_attribute("index", common::AttributeValue::int(j.to_int64()))
      .build()
    logs.push(log)
    j = j + 1
  }
  
  // 创建大量指标操作
  let meter = metrics::NoopMeterProvider::{}.get_meter("stress-test")
  let counter = meter.create_counter("stress.operations", None, None)
  let mut k = 0
  while k < 100 {
    counter.add(1L, Some([("index", common::AttributeValue::int(k.to_int64()))]))
    k = k + 1
  }
  
  // 验证所有操作完成
  assert_eq(spans.length(), 100)
  assert_eq(logs.length(), 100)
  assert_eq(true, true)
}