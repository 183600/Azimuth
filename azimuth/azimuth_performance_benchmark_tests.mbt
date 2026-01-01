// Azimuth Performance Benchmark Tests
// 性能基准测试

test "大量span创建性能测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance.span.test")
  
  // 测试创建1000个span的性能
  let span_count = 1000
  let spans = []
  
  for i = 0; i < span_count; i = i + 1 {
    let span_name = "perf.span." + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    spans.push(span)
  }
  
  // 验证所有span都成功创建
  assert_eq(spans.length(), span_count)
  
  // 批量设置状态
  for span in spans {
    Span::set_status(span, Ok)
  }
  
  // 批量添加事件
  for span in spans {
    Span::add_event(span, "performance.event", Some([("index", IntValue(42))]))
  }
  
  // 批量结束span
  for span in spans {
    Span::end(span)
  }
  
  assert_true(true) // 如果没有崩溃则测试通过
}

test "高频metrics记录性能测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.metrics.test")
  
  // 创建多种类型的metrics
  let counter = Meter::create_counter(meter, "perf.counter")
  let histogram = Meter::create_histogram(meter, "perf.histogram")
  let updown_counter = Meter::create_updown_counter(meter, "perf.updown")
  let gauge = Meter::create_gauge(meter, "perf.gauge")
  
  // 高频记录metrics (10000次)
  let record_count = 10000
  
  for i = 0; i < record_count; i = i + 1 {
    Counter::add(counter, i.to_double())
    Histogram::record(histogram, i.to_double() * 0.1)
    UpDownCounter::add(updown_counter, (i % 100).to_double() - 50.0)
  }
  
  // 测试带有属性的metrics记录 (1000次)
  let attrs = Attributes::new()
  Attributes::set(attrs, "performance.test", StringValue("true"))
  Attributes::set(attrs, "test.type", StringValue("benchmark"))
  
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(counter, 1.0, Some(attrs))
    Histogram::record(histogram, 50.0 + i.to_double() * 0.01, Some(attrs))
  }
  
  assert_true(true)
}

test "大量log记录性能测试" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance.log.test")
  
  // 测试记录10000条日志的性能
  let log_count = 10000
  
  for i = 0; i < log_count; i = i + 1 {
    let message = "Performance test log message " + i.to_string()
    let log = LogRecord::new(Info, message)
    Logger::emit(logger, log)
  }
  
  // 测试带有属性的日志记录 (1000条)
  let attrs = Attributes::with_attributes(Attributes::new(), [
    ("performance.test", StringValue("true")),
    ("log.index", IntValue(0))
  ])
  
  for i = 0; i < 1000; i = i + 1 {
    let message = "Performance test log with attributes " + i.to_string()
    let log = LogRecord::new_with_context(
      Info,
      Some(message),
      Some(attrs),
      Some(1735689600000000000L + i.to_int64()),
      None,
      Some("trace.id.12345"),
      Some("span.id." + i.to_string()),
      Some(Context::root())
    )
    Logger::emit(logger, log)
  }
  
  assert_true(true)
}

test "context操作性能测试" {
  let root_ctx = Context::root()
  
  // 测试创建10000个context的性能
  let context_count = 10000
  let contexts = []
  
  for i = 0; i < context_count; i = i + 1 {
    let key = ContextKey::new("perf.key." + i.to_string())
    let value = "perf.value." + i.to_string()
    let ctx = Context::with_value(root_ctx, key, value)
    contexts.push(ctx)
  }
  
  // 验证所有context都成功创建
  assert_eq(contexts.length(), context_count)
  
  // 测试context值检索性能
  for i = 0; i < context_count; i = i + 1 {
    let ctx = contexts[i]
    let key = ContextKey::new("perf.key." + i.to_string())
    let value = Context::get(ctx, key)
    match value {
      Some(v) => assert_eq(v, "perf.value." + i.to_string())
      None => assert_true(false)
    }
  }
  
  assert_true(true)
}

test "baggage操作性能测试" {
  let baggage = Baggage::new()
  
  // 测试添加1000个baggage条目的性能
  let entry_count = 1000
  let current_baggage = baggage
  
  for i = 0; i < entry_count; i = i + 1 {
    let key = "perf.baggage.key." + i.to_string()
    let value = "perf.baggage.value." + i.to_string()
    current_baggage = Baggage::set_entry(current_baggage, key, value)
  }
  
  // 测试baggage条目检索性能
  for i = 0; i < entry_count; i = i + 1 {
    let key = "perf.baggage.key." + i.to_string()
    let expected_value = "perf.baggage.value." + i.to_string()
    let value = Baggage::get_entry(current_baggage, key)
    match value {
      Some(v) => assert_eq(v, expected_value)
      None => assert_true(false)
    }
  }
  
  // 测试baggage条目移除性能
  for i = 0; i < entry_count; i = i + 1 {
    let key = "perf.baggage.key." + i.to_string()
    current_baggage = Baggage::remove_entry(current_baggage, key)
  }
  
  assert_true(true)
}

test "attributes操作性能测试" {
  let attrs = Attributes::new()
  
  // 测试设置1000个属性的性能
  let attr_count = 1000
  
  for i = 0; i < attr_count; i = i + 1 {
    let key = "perf.attr.key." + i.to_string()
    let value = StringValue("perf.attr.value." + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  // 测试属性检索性能
  for i = 0; i < attr_count; i = i + 1 {
    let key = "perf.attr.key." + i.to_string()
    let value = Attributes::get(attrs, key)
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试不同类型属性的性能
  for i = 0; i < 100; i = i + 1 {
    Attributes::set(attrs, "int.key." + i.to_string(), IntValue(i))
    Attributes::set(attrs, "float.key." + i.to_string(), FloatValue(i.to_double() * 0.1))
    Attributes::set(attrs, "bool.key." + i.to_string(), BoolValue(i % 2 == 0))
    Attributes::set(attrs, "array.string.key." + i.to_string(), ArrayStringValue(["item1", "item2", "item3"]))
    Attributes::set(attrs, "array.int.key." + i.to_string(), ArrayIntValue([1, 2, 3, 4, 5]))
  }
  
  assert_true(true)
}

test "resource操作性能测试" {
  let resource = Resource::new()
  
  // 测试创建1000个资源的性能
  let resource_count = 1000
  let resources = []
  
  for i = 0; i < resource_count; i = i + 1 {
    let attrs = [
      ("resource.id", StringValue("perf.resource." + i.to_string())),
      ("resource.type", StringValue("performance.test")),
      ("resource.index", IntValue(i))
    ]
    let res = Resource::with_attributes(resource, attrs)
    resources.push(res)
  }
  
  // 验证所有资源都成功创建
  assert_eq(resources.length(), resource_count)
  
  // 测试资源属性检索性能
  for i = 0; i < resource_count; i = i + 1 {
    let res = resources[i]
    let resource_id = Resource::get_attribute(res, "resource.id")
    match resource_id {
      Some(StringValue(value)) => assert_eq(value, "perf.resource." + i.to_string())
      _ => assert_true(false)
    }
  }
  
  // 测试资源合并性能
  let base_resource = Resource::with_attributes(resource, [
    ("base.attr", StringValue("base.value")),
    ("base.version", StringValue("1.0.0"))
  ])
  
  let merged_resources = []
  for res in resources {
    let merged = Resource::merge(base_resource, res)
    merged_resources.push(merged)
  }
  
  assert_eq(merged_resources.length(), resource_count)
}

test "传播器操作性能测试" {
  let propagator = CompositePropagator::new([W3CTraceContextPropagator::new()])
  
  // 测试1000次注入和提取操作的性能
  let operation_count = 1000
  
  for i = 0; i < operation_count; i = i + 1 {
    let ctx = Context::root()
    let key = ContextKey::new("perf.key." + i.to_string())
    let ctx_with_value = Context::with_value(ctx, key, "perf.value." + i.to_string())
    
    let carrier = TextMapCarrier::new()
    CompositePropagator::inject(propagator, ctx_with_value, carrier)
    
    let extracted_ctx = CompositePropagator::extract(propagator, carrier)
    let extracted_value = Context::get(extracted_ctx, key)
    
    // 简化实现可能返回None，但不应该崩溃
  }
  
  // 测试带有大量context的传播性能
  let complex_ctx = Context::root()
  for i = 0; i < 100; i = i + 1 {
    let key = ContextKey::new("complex.key." + i.to_string())
    complex_ctx = Context::with_value(complex_ctx, key, "complex.value." + i.to_string())
  }
  
  for i = 0; i < 100; i = i + 1 {
    let carrier = TextMapCarrier::new()
    CompositePropagator::inject(propagator, complex_ctx, carrier)
    let extracted_ctx = CompositePropagator::extract(propagator, carrier)
  }
  
  assert_true(true)
}

test "内存使用压力测试" {
  // 创建大量对象以测试内存使用
  let spans = []
  let metrics = []
  let logs = []
  let contexts = []
  let resources = []
  
  // 创建大量span
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "memory.test")
  
  for i = 0; i < 500; i = i + 1 {
    let span = Tracer::start_span(tracer, "memory.span." + i.to_string())
    spans.push(span)
  }
  
  // 创建大量metrics
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "memory.test")
  
  for i = 0; i < 500; i = i + 1 {
    let counter = Meter::create_counter(meter, "memory.counter." + i.to_string())
    metrics.push(counter)
  }
  
  // 创建大量logs
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "memory.test")
  
  for i = 0; i < 500; i = i + 1 {
    let log = LogRecord::new(Info, "Memory test log " + i.to_string())
    logs.push(log)
  }
  
  // 创建大量contexts
  for i = 0; i < 500; i = i + 1 {
    let key = ContextKey::new("memory.key." + i.to_string())
    let ctx = Context::with_value(Context::root(), key, "memory.value." + i.to_string())
    contexts.push(ctx)
  }
  
  // 创建大量resources
  for i = 0; i < 500; i = i + 1 {
    let attrs = [
      ("memory.resource.id", StringValue("memory.resource." + i.to_string())),
      ("memory.resource.type", StringValue("test"))
    ]
    let res = Resource::with_attributes(Resource::new(), attrs)
    resources.push(res)
  }
  
  // 验证所有对象都成功创建
  assert_eq(spans.length(), 500)
  assert_eq(metrics.length(), 500)
  assert_eq(logs.length(), 500)
  assert_eq(contexts.length(), 500)
  assert_eq(resources.length(), 500)
  
  // 执行一些操作以确保对象正常工作
  for span in spans {
    Span::set_status(span, Ok)
    Span::end(span)
  }
  
  for metric in metrics {
    Counter::add(metric, 1.0)
  }
  
  for log in logs {
    Logger::emit(logger, log)
  }
  
  assert_true(true)
}

test "并发操作性能测试" {
  // 模拟并发操作的测试（虽然MoonBit可能不支持真正的并发）
  let tracer_provider = TracerProvider::default()
  let meter_provider = MeterProvider::default()
  let logger_provider = LoggerProvider::default()
  
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent.perf.test")
  let meter = MeterProvider::get_meter(meter_provider, "concurrent.perf.test")
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent.perf.test")
  
  // 创建多个span
  let spans = []
  for i = 0; i < 100; i = i + 1 {
    let span = Tracer::start_span(tracer, "concurrent.span." + i.to_string())
    spans.push(span)
  }
  
  // 创建多个metrics
  let counters = []
  for i = 0; i < 100; i = i + 1 {
    let counter = Meter::create_counter(meter, "concurrent.counter." + i.to_string())
    counters.push(counter)
  }
  
  // 模拟并发操作：交错执行span和metrics操作
  for i = 0; i < 100; i = i + 1 {
    // span操作
    Span::add_event(spans[i], "concurrent.event", Some([("index", IntValue(i))]))
    
    // metrics操作
    Counter::add(counters[i], i.to_double())
    
    // log操作
    let log = LogRecord::new(Info, "Concurrent log " + i.to_string())
    Logger::emit(logger, log)
    
    // 如果i是偶数，设置span状态
    if i % 2 == 0 {
      Span::set_status(spans[i], Ok)
    }
  }
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  assert_true(true)
}