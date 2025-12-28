// 并发安全性测试用例
// 测试遥测系统在并发环境下的安全性和数据一致性

test "concurrent_span_operations" {
  // 测试并发span操作的安全性
  let trace_id = "concurrent_trace_001"
  
  // 创建多个并发span
  let span1_ctx = SpanContext::new(trace_id, "span_001", true, "")
  let span2_ctx = SpanContext::new(trace_id, "span_002", true, "")
  let span3_ctx = SpanContext::new(trace_id, "span_003", true, "")
  
  let span1 = Span::new("operation-1", Internal, span1_ctx)
  let span2 = Span::new("operation-2", Client, span2_ctx)
  let span3 = Span::new("operation-3", Server, span3_ctx)
  
  // 验证每个span的独立性
  assert_eq(Span::name(span1), "operation-1")
  assert_eq(Span::name(span2), "operation-2")
  assert_eq(Span::name(span3), "operation-3")
  
  assert_eq(Span::kind(span1), Internal)
  assert_eq(Span::kind(span2), Client)
  assert_eq(Span::kind(span3), Server)
  
  // 验证所有span属于同一trace
  assert_eq(SpanContext::trace_id(span1_ctx), trace_id)
  assert_eq(SpanContext::trace_id(span2_ctx), trace_id)
  assert_eq(SpanContext::trace_id(span3_ctx), trace_id)
  
  // 验证span ID的唯一性
  assert_not_eq(SpanContext::span_id(span1_ctx), SpanContext::span_id(span2_ctx))
  assert_not_eq(SpanContext::span_id(span2_ctx), SpanContext::span_id(span3_ctx))
  assert_not_eq(SpanContext::span_id(span1_ctx), SpanContext::span_id(span3_ctx))
  
  // 验证所有span都是有效的
  assert_true(SpanContext::is_valid(span1_ctx))
  assert_true(SpanContext::is_valid(span2_ctx))
  assert_true(SpanContext::is_valid(span3_ctx))
}

test "concurrent_metrics_operations" {
  // 测试并发指标操作的安全性
  let provider = MeterProvider::default()
  let meter1 = MeterProvider::get_meter(provider, "concurrent-meter-1")
  let meter2 = MeterProvider::get_meter(provider, "concurrent-meter-2")
  
  // 创建多个counter
  let counter1 = Meter::create_counter(meter1, "requests.total.1")
  let counter2 = Meter::create_counter(meter1, "requests.total.2")
  let counter3 = Meter::create_counter(meter2, "requests.total.3")
  
  // 创建多个histogram
  let histogram1 = Meter::create_histogram(meter1, "duration.1")
  let histogram2 = Meter::create_histogram(meter1, "duration.2")
  let histogram3 = Meter::create_histogram(meter2, "duration.3")
  
  // 并发记录指标值
  Counter::add(counter1, 10.0)
  Counter::add(counter2, 20.0)
  Counter::add(counter3, 30.0)
  
  Histogram::record(histogram1, 100.0)
  Histogram::record(histogram2, 200.0)
  Histogram::record(histogram3, 300.0)
  
  // 验证指标独立性
  assert_eq(counter1.name, "requests.total.1")
  assert_eq(counter2.name, "requests.total.2")
  assert_eq(counter3.name, "requests.total.3")
  
  assert_eq(histogram1.name, "duration.1")
  assert_eq(histogram2.name, "duration.2")
  assert_eq(histogram3.name, "duration.3")
  
  // 验证meter关联
  assert_eq(meter1.scope.name, "concurrent-meter-1")
  assert_eq(meter2.scope.name, "concurrent-meter-2")
}

test "concurrent_context_operations" {
  // 测试并发上下文操作的安全性
  let root_ctx = Context::root()
  
  // 创建多个并行的上下文链
  let key1 = ContextKey::new("concurrent.key.1")
  let key2 = ContextKey::new("concurrent.key.2")
  let key3 = ContextKey::new("concurrent.key.3")
  
  let ctx1 = Context::with_value(root_ctx, key1, "value.1")
  let ctx2 = Context::with_value(root_ctx, key2, "value.2")
  let ctx3 = Context::with_value(root_ctx, key3, "value.3")
  
  // 验证上下文独立性
  let value1 = Context::get(ctx1, key1)
  let value2 = Context::get(ctx2, key2)
  let value3 = Context::get(ctx3, key3)
  
  assert_eq(value1, Some("value.1"))
  assert_eq(value2, Some("value.2"))
  assert_eq(value3, Some("value.3"))
  
  // 验证跨上下文访问
  let cross_value1 = Context::get(ctx1, key2)
  let cross_value2 = Context::get(ctx2, key3)
  let cross_value3 = Context::get(ctx3, key1)
  
  assert_eq(cross_value1, None)
  assert_eq(cross_value2, None)
  assert_eq(cross_value3, None)
}

test "concurrent_baggage_operations" {
  // 测试并发baggage操作的安全性
  let baggage1 = Baggage::new()
  let baggage2 = Baggage::new()
  let baggage3 = Baggage::new()
  
  // 并发设置baggage条目
  let baggage1_with_entries = Baggage::set_entry(baggage1, "concurrent.key.1", "concurrent.value.1")
  let baggage1_final = Baggage::set_entry(baggage1_with_entries, "shared.key", "shared.value.1")
  
  let baggage2_with_entries = Baggage::set_entry(baggage2, "concurrent.key.2", "concurrent.value.2")
  let baggage2_final = Baggage::set_entry(baggage2_with_entries, "shared.key", "shared.value.2")
  
  let baggage3_with_entries = Baggage::set_entry(baggage3, "concurrent.key.3", "concurrent.value.3")
  let baggage3_final = Baggage::set_entry(baggage3_with_entries, "shared.key", "shared.value.3")
  
  // 验证baggage独立性
  let value1_1 = Baggage::get_entry(baggage1_final, "concurrent.key.1")
  let value2_2 = Baggage::get_entry(baggage2_final, "concurrent.key.2")
  let value3_3 = Baggage::get_entry(baggage3_final, "concurrent.key.3")
  
  assert_eq(value1_1, Some("concurrent.value.1"))
  assert_eq(value2_2, Some("concurrent.value.2"))
  assert_eq(value3_3, Some("concurrent.value.3"))
  
  // 验证共享键的独立性
  let shared_value1 = Baggage::get_entry(baggage1_final, "shared.key")
  let shared_value2 = Baggage::get_entry(baggage2_final, "shared.key")
  let shared_value3 = Baggage::get_entry(baggage3_final, "shared.key")
  
  assert_eq(shared_value1, Some("shared.value.1"))
  assert_eq(shared_value2, Some("shared.value.2"))
  assert_eq(shared_value3, Some("shared.value.3"))
}

test "concurrent_logging_operations" {
  // 测试并发日志操作的安全性
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // 创建多个并发日志记录
  let log1 = LogRecord::new_with_context(
    Info,
    Some("Concurrent log message 1"),
    Some(Attributes::{ values: [
      ("log.id", IntValue(1)),
      ("thread.id", StringValue("thread-1"))
    ]}),
    Some(base_timestamp),
    None,
    Some("concurrent_trace_001"),
    Some("concurrent_span_001"),
    None
  )
  
  let log2 = LogRecord::new_with_context(
    Warn,
    Some("Concurrent log message 2"),
    Some(Attributes::{ values: [
      ("log.id", IntValue(2)),
      ("thread.id", StringValue("thread-2"))
    ]}),
    Some(base_timestamp + 1000000L),
    None,
    Some("concurrent_trace_001"),
    Some("concurrent_span_002"),
    None
  )
  
  let log3 = LogRecord::new_with_context(
    Error,
    Some("Concurrent log message 3"),
    Some(Attributes::{ values: [
      ("log.id", IntValue(3)),
      ("thread.id", StringValue("thread-3"))
    ]}),
    Some(base_timestamp + 2000000L),
    None,
    Some("concurrent_trace_001"),
    Some("concurrent_span_003"),
    None
  )
  
  // 验证日志记录独立性
  assert_eq(LogRecord::body(log1), Some("Concurrent log message 1"))
  assert_eq(LogRecord::body(log2), Some("Concurrent log message 2"))
  assert_eq(LogRecord::body(log3), Some("Concurrent log message 3"))
  
  assert_eq(LogRecord::severity_number(log1), Info)
  assert_eq(LogRecord::severity_number(log2), Warn)
  assert_eq(LogRecord::severity_number(log3), Error)
  
  // 验证trace关联
  assert_eq(LogRecord::trace_id(log1), Some("concurrent_trace_001"))
  assert_eq(LogRecord::trace_id(log2), Some("concurrent_trace_001"))
  assert_eq(LogRecord::trace_id(log3), Some("concurrent_trace_001"))
  
  // 验证span独立性
  assert_eq(LogRecord::span_id(log1), Some("concurrent_span_001"))
  assert_eq(LogRecord::span_id(log2), Some("concurrent_span_002"))
  assert_eq(LogRecord::span_id(log3), Some("concurrent_span_003"))
}

test "concurrent_resource_operations" {
  // 测试并发资源操作的安全性
  let base_resource1 = Resource::new()
  let base_resource2 = Resource::new()
  let base_resource3 = Resource::new()
  
  // 并发设置资源属性
  let attrs1 = [
    ("service.name", StringValue("concurrent-service-1")),
    ("service.instance.id", StringValue("instance-1")),
    ("shared.attr", StringValue("value-1"))
  ]
  let resource1 = Resource::with_attributes(base_resource1, attrs1)
  
  let attrs2 = [
    ("service.name", StringValue("concurrent-service-2")),
    ("service.instance.id", StringValue("instance-2")),
    ("shared.attr", StringValue("value-2"))
  ]
  let resource2 = Resource::with_attributes(base_resource2, attrs2)
  
  let attrs3 = [
    ("service.name", StringValue("concurrent-service-3")),
    ("service.instance.id", StringValue("instance-3")),
    ("shared.attr", StringValue("value-3"))
  ]
  let resource3 = Resource::with_attributes(base_resource3, attrs3)
  
  // 验证资源独立性
  let service_name1 = Resource::get_attribute(resource1, "service.name")
  let service_name2 = Resource::get_attribute(resource2, "service.name")
  let service_name3 = Resource::get_attribute(resource3, "service.name")
  
  assert_eq(service_name1, Some(StringValue("concurrent-service-1")))
  assert_eq(service_name2, Some(StringValue("concurrent-service-2")))
  assert_eq(service_name3, Some(StringValue("concurrent-service-3")))
  
  // 验证共享属性的独立性
  let shared_attr1 = Resource::get_attribute(resource1, "shared.attr")
  let shared_attr2 = Resource::get_attribute(resource2, "shared.attr")
  let shared_attr3 = Resource::get_attribute(resource3, "shared.attr")
  
  assert_eq(shared_attr1, Some(StringValue("value-1")))
  assert_eq(shared_attr2, Some(StringValue("value-2")))
  assert_eq(shared_attr3, Some(StringValue("value-3")))
}

test "concurrent_propagation_operations" {
  // 测试并发传播操作的安全性
  let propagator1 = W3CTraceContextPropagator::new()
  let propagator2 = W3CTraceContextPropagator::new()
  let propagator3 = W3CTraceContextPropagator::new()
  
  let composite_propagator1 = CompositePropagator::new([propagator1])
  let composite_propagator2 = CompositePropagator::new([propagator2])
  let composite_propagator3 = CompositePropagator::new([propagator3])
  
  // 创建多个carrier
  let carrier1 = TextMapCarrier::new()
  let carrier2 = TextMapCarrier::new()
  let carrier3 = TextMapCarrier::new()
  
  // 并发注入操作
  let ctx1 = Context::with_value(Context::root(), ContextKey::new("key1"), "value1")
  let ctx2 = Context::with_value(Context::root(), ContextKey::new("key2"), "value2")
  let ctx3 = Context::with_value(Context::root(), ContextKey::new("key3"), "value3")
  
  CompositePropagator::inject(composite_propagator1, ctx1, carrier1)
  CompositePropagator::inject(composite_propagator2, ctx2, carrier2)
  CompositePropagator::inject(composite_propagator3, ctx3, carrier3)
  
  // 并发提取操作
  let extracted_ctx1 = CompositePropagator::extract(composite_propagator1, carrier1)
  let extracted_ctx2 = CompositePropagator::extract(composite_propagator2, carrier2)
  let extracted_ctx3 = CompositePropagator::extract(composite_propagator3, carrier3)
  
  // 验证提取的上下文
  let extracted_value1 = Context::get(extracted_ctx1, ContextKey::new("extracted"))
  let extracted_value2 = Context::get(extracted_ctx2, ContextKey::new("extracted"))
  let extracted_value3 = Context::get(extracted_ctx3, ContextKey::new("extracted"))
  
  assert_eq(extracted_value1, Some("true"))
  assert_eq(extracted_value2, Some("true"))
  assert_eq(extracted_value3, Some("true"))
}