// 高并发遥测一致性测试
import "azimuth/azimuth"

pub test "高并发遥测一致性测试" {
  // 模拟高并发的Span创建和操作
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  let spans = []
  let trace_ids = []
  
  // 创建大量并发Span（模拟并发场景）
  for i in 0..100 {
    let trace_id = "trace-" + i.to_string()
    let span_id = "span-" + i.to_string()
    
    trace_ids.push(trace_id)
    
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("concurrent-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    
    spans.push(span)
  }
  
  // 验证所有Span都被正确创建
  assert_eq(spans.length(), 100)
  assert_eq(trace_ids.length(), 100)
  
  // 并发操作Span属性
  for i in 0..spans.length() {
    let span = spans[i]
    azimuth::Span::add_event(span, "concurrent.event", Some([
      ("thread.id", azimuth::StringValue("thread-" + i.to_string())),
      ("operation.id", azimuth::IntValue(i))
    ]))
    azimuth::Span::set_status(span, azimuth::Ok)
  }
  
  // 验证Trace ID的唯一性
  let unique_trace_ids = []
  for span in spans {
    let span_ctx = azimuth::Span::span_context(span)
    let trace_id = azimuth::SpanContext::trace_id(span_ctx)
    
    if (!unique_trace_ids.contains(trace_id)) {
      unique_trace_ids.push(trace_id)
    }
  }
  
  assert_eq(unique_trace_ids.length(), 100)
  
  // 测试高并发度量操作
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrency-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "concurrent.operations")
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.duration")
  
  // 模拟并发度量操作
  for i in 0..500 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double() * 0.1)
  }
  
  // 测试高并发日志操作
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "concurrency-logger")
  
  // 创建并发日志记录
  for i in 0..200 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Concurrent log " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(trace_ids[i % trace_ids.length()]),  // 复用trace_ids
      Some("concurrent-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 测试高并发Baggage操作
  let baggage_entries = []
  
  // 创建并发Baggage条目
  for i in 0..50 {
    let baggage = azimuth::Baggage::new()
    let updated_baggage = azimuth::Baggage::set_entry(baggage, "concurrent.key." + i.to_string(), "concurrent.value." + i.to_string())
    
    baggage_entries.push(updated_baggage)
  }
  
  // 验证所有Baggage条目都被创建
  assert_eq(baggage_entries.length(), 50)
  
  // 测试高并发Context操作
  let contexts = []
  
  // 创建并发Context
  for i in 0..30 {
    let ctx = azimuth::Context::root()
    let key = azimuth::ContextKey::new("concurrent.ctx.key." + i.to_string())
    let updated_ctx = azimuth::Context::with_value(ctx, key, "concurrent.ctx.value." + i.to_string())
    
    contexts.push(updated_ctx)
  }
  
  // 验证所有Context都被创建
  assert_eq(contexts.length(), 30)
  
  // 测试高并发Resource操作
  let resources = []
  
  // 创建并发Resource
  for i in 0..25 {
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
      ("service.name", azimuth::StringValue("concurrent-service-" + i.to_string())),
      ("service.instance.id", azimuth::StringValue("concurrent-instance-" + i.to_string())),
      ("concurrency.level", azimuth::IntValue(i))
    ])
    
    resources.push(resource)
  }
  
  // 验证所有Resource都被创建
  assert_eq(resources.length(), 25)
  
  // 测试高并发Propagator操作
  let propagators = []
  let carriers = []
  
  // 创建并发Propagator和Carrier
  for i in 0..20 {
    let trace_propagator = azimuth::W3CTraceContextPropagator::new()
    let baggage_propagator = azimuth::W3CBaggagePropagator::new()
    
    let composite_propagator = azimuth::CompositePropagator::new([trace_propagator, baggage_propagator])
    let carrier = azimuth::TextMapCarrier::new()
    
    propagators.push(composite_propagator)
    carriers.push(carrier)
  }
  
  // 验证所有Propagator和Carrier都被创建
  assert_eq(propagators.length(), 20)
  assert_eq(carriers.length(), 20)
  
  // 测试并发注入和提取操作
  for i in 0..propagators.length() {
    let propagator = propagators[i]
    let carrier = carriers[i]
    let ctx = azimuth::Context::root()
    
    azimuth::CompositePropagator::inject(propagator, ctx, carrier)
    
    let extracted_ctx = azimuth::CompositePropagator::extract(propagator, carrier)
    
    // 验证提取操作成功
    assert_true(extracted_ctx <> None)
  }
  
  // 测试数据一致性
  // 验证在高并发操作下，数据结构保持一致性
  let consistency_check_spans = []
  
  for i in 0..spans.length() {
    let span = spans[i]
    let span_ctx = azimuth::Span::span_context(span)
    let trace_id = azimuth::SpanContext::trace_id(span_ctx)
    let span_id = azimuth::SpanContext::span_id(span_ctx)
    
    consistency_check_spans.push((trace_id, span_id))
  }
  
  // 验证数据一致性
  for i in 0..consistency_check_spans.length() {
    let (trace_id, span_id) = consistency_check_spans[i]
    
    assert_true(trace_id.length() > 0)
    assert_true(span_id.length() > 0)
    assert_true(trace_id.starts_with("trace-"))
    assert_true(span_id.starts_with("span-"))
  }
}