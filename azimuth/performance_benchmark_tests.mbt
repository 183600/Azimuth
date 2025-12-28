// 性能基准测试用例
// 测试Azimuth遥测系统在各种负载下的性能表现

// 测试1: Span创建和操作性能
pub test "Span创建和操作性能" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "性能测试")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建Span
  let spans = []
  for i in 0..1000 {
    let span = azimuth::Tracer::start_span(tracer, "性能测试Span-" + i.to_string())
    spans.push(span)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量添加事件
  for span in spans {
    azimuth::Span::add_event(span, "性能测试事件", Some([
      ("iteration", azimuth::StringValue("test")),
      ("timestamp", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()).to_int()))
    ]))
    azimuth::Span::set_status(span, azimuth::Ok)
  }
  
  let event_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 计算性能指标
  let creation_duration = creation_time - start_time
  let event_duration = event_time - creation_time
  let end_duration = end_time - event_time
  let total_duration = end_time - start_time
  
  // 验证性能在合理范围内
  assert_true(creation_duration < 5000000000L)  // 创建1000个Span应在5秒内完成
  assert_true(event_duration < 3000000000L)    // 添加事件应在3秒内完成
  assert_true(end_duration < 2000000000L)      // 结束Span应在2秒内完成
  assert_true(total_duration < 10000000000L)   // 总操作应在10秒内完成
  
  // 验证所有Span都被创建
  assert_true(spans.length() == 1000)
  
  // 计算平均每个操作的时间（纳秒）
  let avg_creation_time = creation_duration / 1000L
  let avg_event_time = event_duration / 1000L
  let avg_end_time = end_duration / 1000L
  
  // 验证平均操作时间合理
  assert_true(avg_creation_time < 5000000L)    // 每个Span创建时间小于5毫秒
  assert_true(avg_event_time < 3000000L)       // 每个事件添加时间小于3毫秒
  assert_true(avg_end_time < 2000000L)         // 每个Span结束时间小于2毫秒
}

// 测试2: 度量操作性能
pub test "度量操作性能" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "性能度量测试")
  
  // 创建多种度量仪器
  let counter = azimuth::Meter::create_counter(meter, "性能计数器", Some("性能测试计数器"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "性能直方图", Some("性能测试直方图"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "性能上下计数器", Some("性能测试上下计数器"), Some("items"))
  let gauge = azimuth::Meter::create_gauge(meter, "性能仪表", Some("性能测试仪表"), Some("value"))
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量计数器操作
  for i in 0..5000 {
    azimuth::Counter::add(counter, 1.0, Some([
      ("operation", azimuth::StringValue("test")),
      ("iteration", azimuth::IntValue(i))
    ]))
  }
  
  let counter_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量直方图操作
  for i in 0..3000 {
    let value = 100.0 + (i.to_double() * 0.1)
    azimuth::Histogram::record(histogram, value, Some([
      ("metric.type", azimuth::StringValue("performance")),
      ("index", azimuth::IntValue(i))
    ]))
  }
  
  let histogram_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量上下计数器操作
  for i in 0..2000 {
    if i % 3 == 0 {
      azimuth::UpDownCounter::add(updown_counter, 1.0)
    } else {
      azimuth::UpDownCounter::add(updown_counter, -1.0)
    }
  }
  
  let updown_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = updown_time - start_time
  
  // 验证性能指标
  let counter_duration = counter_time - start_time
  let histogram_duration = histogram_time - counter_time
  let updown_duration = updown_time - histogram_time
  
  assert_true(counter_duration < 3000000000L)    // 5000次计数器操作应在3秒内完成
  assert_true(histogram_duration < 2000000000L) // 3000次直方图操作应在2秒内完成
  assert_true(updown_duration < 1000000000L)    // 2000次上下计数器操作应在1秒内完成
  assert_true(total_duration < 6000000000L)     // 总操作应在6秒内完成
  
  // 计算平均操作时间
  let avg_counter_time = counter_duration / 5000L
  let avg_histogram_time = histogram_duration / 3000L
  let avg_updown_time = updown_duration / 2000L
  
  assert_true(avg_counter_time < 600000L)       // 每次计数器操作小于0.6毫秒
  assert_true(avg_histogram_time < 666666L)    // 每次直方图操作小于0.67毫秒
  assert_true(avg_updown_time < 500000L)       // 每次上下计数器操作小于0.5毫秒
}

// 测试3: 日志记录性能
pub test "日志记录性能" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "性能日志测试")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建简单日志
  for i in 0..2000 {
    let log_record = azimuth::LogRecord::new(
      if i % 4 == 0 { azimuth::Debug }
      else if i % 4 == 1 { azimuth::Info }
      else if i % 4 == 2 { azimuth::Warn }
      else { azimuth::Error },
      "性能测试日志消息 " + i.to_string()
    )
    azimuth::Logger::emit(logger, log_record)
  }
  
  let simple_log_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建复杂日志
  for i in 0..1000 {
    let attributes = azimuth::Attributes::new()
    azimuth::Attributes::set(attributes, "log.id", azimuth::StringValue("log-" + i.to_string()))
    azimuth::Attributes::set(attributes, "component", azimuth::StringValue("performance-test"))
    azimuth::Attributes::set(attributes, "iteration", azimuth::IntValue(i))
    
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("复杂性能测试日志 " + i.to_string()),
      Some(attributes),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000L),
      Some("perf-trace-" + (i % 10).to_string()),
      Some("perf-span-" + (i % 5).to_string()),
      Some(azimuth::Context::root())
    )
    azimuth::Logger::emit(logger, log_record)
  }
  
  let complex_log_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = complex_log_time - start_time
  let simple_duration = simple_log_time - start_time
  let complex_duration = complex_log_time - simple_log_time
  
  // 验证性能指标
  assert_true(simple_duration < 2000000000L)    // 2000条简单日志应在2秒内完成
  assert_true(complex_duration < 3000000000L)  // 1000条复杂日志应在3秒内完成
  assert_true(total_duration < 5000000000L)    // 总操作应在5秒内完成
  
  // 计算平均操作时间
  let avg_simple_time = simple_duration / 2000L
  let avg_complex_time = complex_duration / 1000L
  
  assert_true(avg_simple_time < 1000000L)      // 每条简单日志小于1毫秒
  assert_true(avg_complex_time < 3000000L)     // 每条复杂日志小于3毫秒
}

// 测试4: 属性操作性能
pub test "属性操作性能" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建和操作属性
  let attributes_list = []
  for i in 0..1000 {
    let attrs = azimuth::Attributes::new()
    
    // 为每个属性对象设置多个属性
    azimuth::Attributes::set(attrs, "id", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "name", azimuth::StringValue("item-" + i.to_string()))
    azimuth::Attributes::set(attrs, "value", azimuth::FloatValue(i.to_double() * 1.5))
    azimuth::Attributes::set(attrs, "active", azimuth::BoolValue(i % 2 == 0))
    azimuth::Attributes::set(attrs, "tags", azimuth::ArrayStringValue(["tag1", "tag2", "tag3"]))
    azimuth::Attributes::set(attrs, "numbers", azimuth::ArrayIntValue([i, i*2, i*3]))
    
    attributes_list.push(attrs)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量读取属性
  for attrs in attributes_list {
    let _ = azimuth::Attributes::get(attrs, "id")
    let _ = azimuth::Attributes::get(attrs, "name")
    let _ = azimuth::Attributes::get(attrs, "value")
    let _ = azimuth::Attributes::get(attrs, "active")
    let _ = azimuth::Attributes::get(attrs, "tags")
    let _ = azimuth::Attributes::get(attrs, "numbers")
    let _ = azimuth::Attributes::get(attrs, "nonexistent")  // 测试不存在的属性
  }
  
  let read_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = read_time - start_time
  let creation_duration = creation_time - start_time
  let read_duration = read_time - creation_time
  
  // 验证性能指标
  assert_true(creation_duration < 3000000000L)  // 创建1000个属性对象应在3秒内完成
  assert_true(read_duration < 2000000000L)      // 读取操作应在2秒内完成
  assert_true(total_duration < 5000000000L)     // 总操作应在5秒内完成
  
  // 计算平均操作时间
  let avg_creation_time = creation_duration / 1000L
  let avg_read_time = read_duration / (1000L * 7L)  // 每个属性对象读取7次
  
  assert_true(avg_creation_time < 3000000L)    // 每个属性对象创建小于3毫秒
  assert_true(avg_read_time < 285714L)         // 每次属性读取小于0.29毫秒
}

// 测试5: 上下文操作性能
pub test "上下文操作性能" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建上下文链
  let contexts = []
  let base_ctx = azimuth::Context::root()
  
  for i in 0..500 {
    let ctx = azimuth::Context::root()
    let key1 = azimuth::ContextKey::new("key1-" + i.to_string())
    let key2 = azimuth::ContextKey::new("key2-" + i.to_string())
    let key3 = azimuth::ContextKey::new("key3-" + i.to_string())
    
    let ctx1 = azimuth::Context::with_value(ctx, key1, "value1-" + i.to_string())
    let ctx2 = azimuth::Context::with_value(ctx1, key2, "value2-" + i.to_string())
    let ctx3 = azimuth::Context::with_value(ctx2, key3, "value3-" + i.to_string())
    
    contexts.push(ctx3)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量读取上下文值
  for i in 0..500 {
    let ctx = contexts[i]
    let key1 = azimuth::ContextKey::new("key1-" + i.to_string())
    let key2 = azimuth::ContextKey::new("key2-" + i.to_string())
    let key3 = azimuth::ContextKey::new("key3-" + i.to_string())
    let missing_key = azimuth::ContextKey::new("missing-" + i.to_string())
    
    let _ = azimuth::Context::get(ctx, key1)
    let _ = azimuth::Context::get(ctx, key2)
    let _ = azimuth::Context::get(ctx, key3)
    let _ = azimuth::Context::get(ctx, missing_key)
  }
  
  let read_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = read_time - start_time
  let creation_duration = creation_time - start_time
  let read_duration = read_time - creation_time
  
  // 验证性能指标
  assert_true(creation_duration < 2000000000L)  // 创建500个上下文链应在2秒内完成
  assert_true(read_duration < 1000000000L)      // 读取操作应在1秒内完成
  assert_true(total_duration < 3000000000L)     // 总操作应在3秒内完成
  
  // 计算平均操作时间
  let avg_creation_time = creation_duration / 500L
  let avg_read_time = read_duration / (500L * 4L)  // 每个上下文读取4次
  
  assert_true(avg_creation_time < 4000000L)    // 每个上下文链创建小于4毫秒
  assert_true(avg_read_time < 500000L)         // 每次上下文读取小于0.5毫秒
}

// 测试6: Baggage操作性能
pub test "Baggage操作性能" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建和操作Baggage
  let baggage_list = []
  for i in 0..300 {
    let baggage = azimuth::Baggage::new()
    
    // 为每个Baggage设置多个条目
    let baggage1 = azimuth::Baggage::set_entry(baggage, "user.id", "user-" + i.to_string())
    let baggage2 = azimuth::Baggage::set_entry(baggage1, "session.id", "session-" + i.to_string())
    let baggage3 = azimuth::Baggage::set_entry(baggage2, "request.id", "req-" + i.to_string())
    let baggage4 = azimuth::Baggage::set_entry(baggage3, "trace.id", "trace-" + i.to_string())
    
    baggage_list.push(baggage4)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量读取Baggage条目
  for i in 0..300 {
    let baggage = baggage_list[i]
    let _ = azimuth::Baggage::get_entry(baggage, "user.id")
    let _ = azimuth::Baggage::get_entry(baggage, "session.id")
    let _ = azimuth::Baggage::get_entry(baggage, "request.id")
    let _ = azimuth::Baggage::get_entry(baggage, "trace.id")
    let _ = azimuth::Baggage::get_entry(baggage, "nonexistent")  // 测试不存在的条目
  }
  
  let read_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = read_time - start_time
  let creation_duration = creation_time - start_time
  let read_duration = read_time - creation_time
  
  // 验证性能指标
  assert_true(creation_duration < 2000000000L)  // 创建300个Baggage应在2秒内完成
  assert_true(read_duration < 1000000000L)      // 读取操作应在1秒内完成
  assert_true(total_duration < 3000000000L)     // 总操作应在3秒内完成
  
  // 计算平均操作时间
  let avg_creation_time = creation_duration / 300L
  let avg_read_time = read_duration / (300L * 5L)  // 每个Baggage读取5次
  
  assert_true(avg_creation_time < 6666666L)    // 每个Baggage创建小于6.67毫秒
  assert_true(avg_read_time < 666666L)         // 每次Baggage读取小于0.67毫秒
}

// 测试7: 资源操作性能
pub test "资源操作性能" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量创建资源
  let resources = []
  for i in 0..200 {
    let attrs = [
      ("service.name", azimuth::StringValue("service-" + i.to_string())),
      ("service.version", azimuth::StringValue("1.0." + i.to_string())),
      ("service.instance.id", azimuth::StringValue("instance-" + i.to_string())),
      ("host.name", azimuth::StringValue("host-" + i.to_string())),
      ("deployment.environment", azimuth::StringValue("env-" + (i % 3).to_string()))
    ]
    
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), attrs)
    resources.push(resource)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量读取资源属性
  for i in 0..200 {
    let resource = resources[i]
    let _ = azimuth::Resource::get_attribute(resource, "service.name")
    let _ = azimuth::Resource::get_attribute(resource, "service.version")
    let _ = azimuth::Resource::get_attribute(resource, "service.instance.id")
    let _ = azimuth::Resource::get_attribute(resource, "host.name")
    let _ = azimuth::Resource::get_attribute(resource, "deployment.environment")
    let _ = azimuth::Resource::get_attribute(resource, "nonexistent")  // 测试不存在的属性
  }
  
  let read_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量合并资源
  let merged_resources = []
  for i in 0..100 {
    let merged = azimuth::Resource::merge(resources[i], resources[i + 100])
    merged_resources.push(merged)
  }
  
  let merge_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = merge_time - start_time
  let creation_duration = creation_time - start_time
  let read_duration = read_time - creation_time
  let merge_duration = merge_time - read_time
  
  // 验证性能指标
  assert_true(creation_duration < 1000000000L)  // 创建200个资源应在1秒内完成
  assert_true(read_duration < 1000000000L)      // 读取操作应在1秒内完成
  assert_true(merge_duration < 1000000000L)     // 合并操作应在1秒内完成
  assert_true(total_duration < 3000000000L)     // 总操作应在3秒内完成
  
  // 计算平均操作时间
  let avg_creation_time = creation_duration / 200L
  let avg_read_time = read_duration / (200L * 6L)  // 每个资源读取6次
  let avg_merge_time = merge_duration / 100L
  
  assert_true(avg_creation_time < 5000000L)    // 每个资源创建小于5毫秒
  assert_true(avg_read_time < 833333L)         // 每次属性读取小于0.83毫秒
  assert_true(avg_merge_time < 10000000L)      // 每次资源合并小于10毫秒
}

// 测试8: 传播器操作性能
pub test "传播器操作性能" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let propagators = [trace_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量注入操作
  let carriers = []
  for i in 0..500 {
    let carrier = azimuth::TextMapCarrier::new()
    let ctx = azimuth::Context::root()
    
    azimuth::CompositePropagator::inject(composite_propagator, ctx, carrier)
    carriers.push(carrier)
  }
  
  let inject_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 批量提取操作
  for carrier in carriers {
    let _ = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  }
  
  let extract_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = extract_time - start_time
  let inject_duration = inject_time - start_time
  let extract_duration = extract_time - inject_time
  
  // 验证性能指标
  assert_true(inject_duration < 2000000000L)    // 500次注入操作应在2秒内完成
  assert_true(extract_duration < 2000000000L)   // 500次提取操作应在2秒内完成
  assert_true(total_duration < 4000000000L)     // 总操作应在4秒内完成
  
  // 计算平均操作时间
  let avg_inject_time = inject_duration / 500L
  let avg_extract_time = extract_duration / 500L
  
  assert_true(avg_inject_time < 4000000L)       // 每次注入操作小于4毫秒
  assert_true(avg_extract_time < 4000000L)      // 每次提取操作小于4毫秒
}

// 测试9: 高负载综合性能测试
pub test "高负载综合性能测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "高负载测试")
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "高负载度量")
  let logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "高负载日志")
  
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建度量仪器
  let counter = azimuth::Meter::create_counter(meter, "load.test.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "load.test.histogram")
  
  // 综合操作循环
  for i in 0..200 {
    // 创建Span
    let span = azimuth::Tracer::start_span(tracer, "高负载操作-" + i.to_string())
    
    // 添加事件
    azimuth::Span::add_event(span, "操作开始", Some([
      ("iteration", azimuth::IntValue(i)),
      ("load.test", azimuth::StringValue("true"))
    ]))
    
    // 度量操作
    azimuth::Counter::add(counter, 1.0)
    azimuth::Histogram::record(histogram, i.to_double() * 10.0)
    
    // 日志操作
    let log_record = azimuth::LogRecord::new_with_context(
      azimuth::Info,
      Some("高负载测试日志 " + i.to_string()),
      Some(azimuth::Attributes::new()),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some("load-trace"),
      Some("load-span"),
      Some(azimuth::Context::root())
    )
    azimuth::Logger::emit(logger, log_record)
    
    // 属性操作
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "load.test.id", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "load.test.name", azimuth::StringValue("test-" + i.to_string()))
    let _ = azimuth::Attributes::get(attrs, "load.test.id")
    
    // 上下文操作
    let ctx = azimuth::Context::root()
    let key = azimuth::ContextKey::new("load.test.key")
    let ctx_with_value = azimuth::Context::with_value(ctx, key, "value-" + i.to_string())
    let _ = azimuth::Context::get(ctx_with_value, key)
    
    // 添加结束事件
    azimuth::Span::add_event(span, "操作完成", Some([
      ("iteration", azimuth::IntValue(i)),
      ("status", azimuth::StringValue("completed"))
    ]))
    
    // 结束Span
    azimuth::Span::end(span)
  }
  
  let end_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let total_duration = end_time - start_time
  
  // 验证综合性能
  assert_true(total_duration < 10000000000L)  // 200次综合操作应在10秒内完成
  
  // 计算平均每次综合操作的时间
  let avg_operation_time = total_duration / 200L
  assert_true(avg_operation_time < 50000000L)  // 每次综合操作小于50毫秒
}

// 测试10: 内存使用性能测试
pub test "内存使用性能测试" {
  let start_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 创建大量对象以测试内存使用
  let large_spans = []
  let large_attributes = []
  let large_contexts = []
  let large_baggage = []
  let large_resources = []
  
  // 创建大量Span
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "内存测试")
  
  for i in 0..500 {
    let span = azimuth::Tracer::start_span(tracer, "内存测试Span-" + i.to_string())
    large_spans.push(span)
  }
  
  // 创建大量属性对象
  for i in 0..500 {
    let attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(attrs, "id", azimuth::IntValue(i))
    azimuth::Attributes::set(attrs, "name", azimuth::StringValue("内存测试-" + i.to_string()))
    azimuth::Attributes::set(attrs, "data", azimuth::StringValue("x".repeat(100)))  // 100字符的字符串
    large_attributes.push(attrs)
  }
  
  // 创建大量上下文
  for i in 0..500 {
    let ctx = azimuth::Context::root()
    let key = azimuth::ContextKey::new("内存测试键-" + i.to_string())
    let ctx_with_value = azimuth::Context::with_value(ctx, key, "内存测试值-" + i.to_string())
    large_contexts.push(ctx_with_value)
  }
  
  // 创建大量Baggage
  for i in 0..500 {
    let baggage = azimuth::Baggage::new()
    let baggage_with_entry = azimuth::Baggage::set_entry(baggage, "内存测试", "值-" + i.to_string())
    large_baggage.push(baggage_with_entry)
  }
  
  // 创建大量资源
  for i in 0..500 {
    let attrs = [
      ("service.name", azimuth::StringValue("内存测试服务-" + i.to_string())),
      ("service.instance.id", azimuth::StringValue("实例-" + i.to_string())),
      ("large.data", azimuth::StringValue("x".repeat(200)))  // 200字符的字符串
    ]
    let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), attrs)
    large_resources.push(resource)
  }
  
  let creation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 执行大量读取操作
  for i in 0..500 {
    // 读取属性
    let attrs = large_attributes[i]
    let _ = azimuth::Attributes::get(attrs, "id")
    let _ = azimuth::Attributes::get(attrs, "name")
    let _ = azimuth::Attributes::get(attrs, "data")
    
    // 读取上下文
    let ctx = large_contexts[i]
    let key = azimuth::ContextKey::new("内存测试键-" + i.to_string())
    let _ = azimuth::Context::get(ctx, key)
    
    // 读取Baggage
    let baggage = large_baggage[i]
    let _ = azimuth::Baggage::get_entry(baggage, "内存测试")
    
    // 读取资源
    let resource = large_resources[i]
    let _ = azimuth::Resource::get_attribute(resource, "service.name")
    let _ = azimuth::Resource::get_attribute(resource, "large.data")
  }
  
  let read_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 清理操作（结束所有Span）
  for span in large_spans {
    azimuth::Span::end(span)
  }
  
  let cleanup_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let total_duration = cleanup_time - start_time
  let creation_duration = creation_time - start_time
  let read_duration = read_time - creation_time
  let cleanup_duration = cleanup_time - read_time
  
  // 验证内存性能
  assert_true(creation_duration < 15000000000L)  // 创建2500个对象应在15秒内完成
  assert_true(read_duration < 5000000000L)       // 读取操作应在5秒内完成
  assert_true(cleanup_duration < 3000000000L)    // 清理操作应在3秒内完成
  assert_true(total_duration < 23000000000L)     // 总操作应在23秒内完成
  
  // 验证对象数量
  assert_true(large_spans.length() == 500)
  assert_true(large_attributes.length() == 500)
  assert_true(large_contexts.length() == 500)
  assert_true(large_baggage.length() == 500)
  assert_true(large_resources.length() == 500)
}