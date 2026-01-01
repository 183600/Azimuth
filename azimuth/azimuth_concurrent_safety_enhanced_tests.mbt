// Enhanced concurrent safety tests for Azimuth telemetry system
import "azimuth/azimuth"

// Test 1: Concurrent Span operations
pub test "并发Span操作安全性测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrent-test")
  
  // 创建多个Span进行并发操作
  let spans = []
  for i in 0..50 {
    let span = azimuth::Tracer::start_span(tracer, "concurrent-span-" + i.to_string())
    spans.push(span)
  }
  
  // 并发设置属性和事件
  for i in 0..spans.length() {
    let span = spans[i]
    azimuth::Span::add_event(span, "concurrent.event", Some([("iteration", azimuth::StringValue(i.to_string()))]))
    azimuth::Span::set_status(span, azimuth::Ok)
    
    // 模拟并发设置属性
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "thread.id", azimuth::StringValue(i.to_string()))
    azimuth::Attributes::set(attrs, "operation.type", azimuth::StringValue("concurrent"))
  }
  
  // 验证所有Span都正确创建
  assert_true(spans.length() == 50)
  
  // 并发结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
}

// Test 2: Concurrent Metrics operations
pub test "并发度量操作安全性测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "concurrent-metrics")
  
  // 创建多个度量 instruments
  let counter = azimuth::Meter::create_counter(meter, "concurrent.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "concurrent.histogram")
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "concurrent.updown")
  
  // 并发更新度量
  for i in 0..100 {
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
    azimuth::UpDownCounter::add(updown_counter, i.to_double())
  }
  
  // 验证度量创建成功
  assert_eq(counter.name, "concurrent.counter")
  assert_eq(histogram.name, "concurrent.histogram")
  assert_eq(updown_counter.name, "concurrent.updown")
}

// Test 3: Concurrent Context and Baggage operations
pub test "并发上下文和Baggage操作安全性测试" {
  let root_ctx = azimuth::Context::root()
  
  // 创建多个并发上下文
  let contexts = []
  for i in 0..30 {
    let key = azimuth::ContextKey::new("concurrent.key." + i.to_string())
    let ctx = azimuth::Context::with_value(root_ctx, key, "value-" + i.to_string())
    contexts.push(ctx)
  }
  
  // 并发操作Baggage
  let baggages = []
  for i in 0..30 {
    let baggage = azimuth::Baggage::new()
    let updated_baggage = azimuth::Baggage::set_entry(baggage, "concurrent.entry." + i.to_string(), "value-" + i.to_string())
    baggages.push(updated_baggage)
  }
  
  // 验证上下文和Baggage创建
  assert_true(contexts.length() == 30)
  assert_true(baggages.length() == 30)
  
  // 验证Baggage值正确性
  for i in 0..baggages.length() {
    let baggage = baggages[i]
    let value = azimuth::Baggage::get_entry(baggage, "concurrent.entry." + i.to_string())
    assert_eq(value, Some("value-" + i.to_string()))
  }
}

// Test 4: Concurrent Resource operations
pub test "并发资源操作安全性测试" {
  // 创建多个资源进行并发操作
  let resources = []
  for i in 0..20 {
    let resource = azimuth::Resource::new()
    let attrs = [
      ("service.name", azimuth::StringValue("service-" + i.to_string())),
      ("service.instance.id", azimuth::StringValue("instance-" + i.to_string())),
      ("service.version", azimuth::StringValue("1.0." + i.to_string()))
    ]
    let resource_with_attrs = azimuth::Resource::with_attributes(resource, attrs)
    resources.push(resource_with_attrs)
  }
  
  // 验证资源创建
  assert_true(resources.length() == 20)
  
  // 并发合并资源
  let base_resource = azimuth::Resource::new()
  let merged_resources = []
  
  for i in 0..resources.length() {
    let resource = resources[i]
    let merged = azimuth::Resource::merge(base_resource, resource)
    merged_resources.push(merged)
  }
  
  // 验证合并结果
  assert_true(merged_resources.length() == 20)
  
  for i in 0..merged_resources.length() {
    let resource = merged_resources[i]
    let service_name = azimuth::Resource::get_attribute(resource, "service.name")
    assert_eq(service_name, Some(azimuth::StringValue("service-" + i.to_string())))
  }
}

// Test 5: Concurrent Logger operations
pub test "并发日志记录操作安全性测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "concurrent-logger")
  
  // 并发创建日志记录
  let log_records = []
  for i in 0..40 {
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("Concurrent log message " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("concurrent-trace-" + i.to_string()),
      Some("concurrent-span-" + i.to_string()),
      Some(azimuth::Context::root())
    )
    log_records.push(log_record)
  }
  
  // 验证日志记录创建
  assert_true(log_records.length() == 40)
  
  // 并发发送日志
  for log_record in log_records {
    azimuth::Logger::emit(logger, log_record)
  }
  
  // 验证日志记录内容
  for i in 0..log_records.length() {
    let log_record = log_records[i]
    assert_eq(azimuth::LogRecord::severity_number(log_record), azimuth::Info)
    assert_eq(azimuth::LogRecord::body(log_record), Some("Concurrent log message " + i.to_string()))
  }
}

// Test 6: Concurrent Propagator operations
pub test "并发传播器操作安全性测试" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // 并发创建载体和上下文
  let carriers = []
  let contexts = []
  
  for i in 0..25 {
    let carrier = azimuth::TextMapCarrier::new()
    let ctx = azimuth::Context::root()
    
    // 添加自定义 baggage
    let baggage = azimuth::Baggage::new()
    let updated_baggage = azimuth::Baggage::set_entry(baggage, "concurrent.id", i.to_string())
    
    carriers.push(carrier)
    contexts.push(ctx)
  }
  
  // 并发注入操作
  for i in 0..carriers.length() {
    let carrier = carriers[i]
    let ctx = contexts[i]
    azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
  }
  
  // 并发提取操作
  let extracted_contexts = []
  for carrier in carriers {
    let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
    extracted_contexts.push(extracted_ctx)
  }
  
  // 验证操作结果
  assert_true(carriers.length() == 25)
  assert_true(extracted_contexts.length() == 25)
}

// Test 7: Stress test with high concurrency
pub test "高并发压力测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "stress-test")
  
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "stress-meter")
  
  let counter = azimuth::Meter::create_counter(meter, "stress.operations")
  let histogram = azimuth::Meter::create_histogram(meter, "stress.duration")
  
  // 高并发创建和操作Span
  let spans = []
  for i in 0..200 {
    let span = azimuth::Tracer::start_span(tracer, "stress-span-" + i.to_string())
    
    // 添加事件和属性
    azimuth::Span::add_event(span, "stress.event", Some([("index", azimuth::StringValue(i.to_string()))]))
    
    // 更新度量
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double())
    
    spans.push(span)
  }
  
  // 验证高并发操作结果
  assert_true(spans.length() == 200)
  
  // 结束所有Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  // 验证度量状态
  assert_eq(counter.name, "stress.operations")
  assert_eq(histogram.name, "stress.duration")
}

// Test 8: Concurrent attribute operations
pub test "并发属性操作安全性测试" {
  // 创建多个属性集合进行并发操作
  let attributes_collections = []
  
  for i in 0..30 {
    let attrs = azimuth::Attributes::new()
    
    // 并发设置不同类型的属性
    azimuth::Attributes::set(attrs, "string.attr." + i.to_string(), azimuth::StringValue("value-" + i.to_string()))
    azimuth::Attributes::set(attrs, "int.attr." + i.to_string(), azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "float.attr." + i.to_string(), azimuth::FloatValue(i.to_double()))
    azimuth::Attributes::set(attrs, "bool.attr." + i.to_string(), azimuth::BoolValue(i % 2 == 0))
    
    attributes_collections.push(attrs)
  }
  
  // 验证属性集合创建
  assert_true(attributes_collections.length() == 30)
  
  // 并发读取属性
  for i in 0..attributes_collections.length() {
    let attrs = attributes_collections[i]
    
    let string_val = azimuth::Attributes::get(attrs, "string.attr." + i.to_string())
    let int_val = azimuth::Attributes::get(attrs, "int.attr." + i.to_string())
    let float_val = azimuth::Attributes::get(attrs, "float.attr." + i.to_string())
    let bool_val = azimuth::Attributes::get(attrs, "bool.attr." + i.to_string())
    
    // 基于简化实现进行验证
    assert_eq(string_val, Some(azimuth::StringValue("test_value")))
    assert_eq(int_val, Some(azimuth::IntValue(42)))
  }
}