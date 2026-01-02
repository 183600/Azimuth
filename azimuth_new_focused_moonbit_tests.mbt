// Azimuth New Focused MoonBit Test Suite
// 专注于核心功能的新测试用例，覆盖遥测系统的关键特性

// 测试1: 属性值类型转换
test "属性值类型转换测试" {
  let string_attr = azimuth::StringValue("test-value")
  let int_attr = azimuth::IntValue(42)
  let float_attr = azimuth::FloatValue(3.14)
  let bool_attr = azimuth::BoolValue(true)
  
  // 验证属性值创建
  match string_attr {
    azimuth::StringValue(v) => assert_eq(v, "test-value")
    _ => assert_true(false)
  }
  
  match int_attr {
    azimuth::IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  match float_attr {
    azimuth::FloatValue(v) => assert_true(abs(v - 3.14) < 0.001)
    _ => assert_true(false)
  }
  
  match bool_attr {
    azimuth::BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
}

// 测试2: 资源属性合并
test "资源属性合并测试" {
  let resource1_attrs = [
    ("service.name", azimuth::StringValue("azimuth-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("host.name", azimuth::StringValue("localhost"))
  ]
  
  let resource2_attrs = [
    ("service.name", azimuth::StringValue("updated-service")), // 应该覆盖
    ("environment", azimuth::StringValue("production")),
    ("region", azimuth::StringValue("us-west"))
  ]
  
  let resource1 = azimuth::Resource::new(resource1_attrs)
  let resource2 = azimuth::Resource::new(resource2_attrs)
  
  // 合并资源属性，后面的应该覆盖前面的
  let merged = azimuth::Resource::merge(resource1, resource2)
  
  let merged_attrs = azimuth::Resource::attributes(merged)
  
  // 验证合并结果
  assert_true(merged_attrs.length() >= 5)
  
  // 验证覆盖行为
  let service_name = azimuth::Resource::get_attribute(merged, "service.name")
  match service_name {
    Some(azimuth::StringValue(v)) => assert_eq(v, "updated-service")
    _ => assert_true(false)
  }
  
  // 验证保留的属性
  let version = azimuth::Resource::get_attribute(merged, "service.version")
  match version {
    Some(azimuth::StringValue(v)) => assert_eq(v, "1.0.0")
    _ => assert_true(false)
  }
}

// 测试3: 上下文传播
test "上下文传播测试" {
  let parent_context = azimuth::Context::new()
  
  // 在父上下文中设置数据
  let key1 = azimuth::ContextKey::new("trace-id")
  let key2 = azimuth::ContextKey::new("user-id")
  
  let context_with_data1 = azimuth::Context::set_value(parent_context, key1, "trace-123")
  let context_with_data2 = azimuth::Context::set_value(context_with_data1, key2, "user-456")
  
  // 验证数据可以正确获取
  let trace_id = azimuth::Context::get_value(context_with_data2, key1)
  match trace_id {
    Some(v) => assert_eq(v, "trace-123")
    None => assert_true(false)
  }
  
  let user_id = azimuth::Context::get_value(context_with_data2, key2)
  match user_id {
    Some(v) => assert_eq(v, "user-456")
    None => assert_true(false)
  }
  
  // 验证不存在的键返回None
  let key3 = azimuth::ContextKey::new("non-existent")
  let non_existent = azimuth::Context::get_value(context_with_data2, key3)
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// 测试4: 行李传递
test "行李传递测试" {
  let baggage_entries = [
    ("user-id", "12345"),
    ("session-id", "abcdef"),
    ("request-id", "req-789")
  ]
  
  let baggage = azimuth::Baggage::new(baggage_entries)
  
  // 验证行李条目数量
  assert_eq(azimuth::Baggage::size(baggage), 3)
  
  // 验证可以获取特定条目
  let user_id = azimuth::Baggage::get_entry(baggage, "user-id")
  match user_id {
    Some(v) => assert_eq(v, "12345")
    None => assert_true(false)
  }
  
  // 验证可以添加新条目
  let updated_baggage = azimuth::Baggage::set_entry(baggage, "new-key", "new-value")
  assert_eq(azimuth::Baggage::size(updated_baggage), 4)
  
  let new_entry = azimuth::Baggage::get_entry(updated_baggage, "new-key")
  match new_entry {
    Some(v) => assert_eq(v, "new-value")
    None => assert_true(false)
  }
  
  // 验证可以移除条目
  let reduced_baggage = azimuth::Baggage::remove_entry(updated_baggage, "session-id")
  assert_eq(azimuth::Baggage::size(reduced_baggage), 3)
  
  let removed_entry = azimuth::Baggage::get_entry(reduced_baggage, "session-id")
  match removed_entry {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// 测试5: Span生命周期
test "Span生命周期测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "lifecycle-test")
  
  // 创建Span
  let span = azimuth::Tracer::start_span(tracer, "test-span")
  
  // 验证Span初始状态
  assert_false(azimuth::Span::is_ended(span))
  assert_eq(azimuth::Span::get_name(span), "test-span")
  
  // 添加属性
  azimuth::Span::set_attribute(span, "test.attr", azimuth::StringValue("test-value"))
  
  // 添加事件
  azimuth::Span::add_event(span, "test-event", Some([("event.key", azimuth::StringValue("event-value"))]))
  
  // 设置状态
  azimuth::Span::set_status(span, azimuth::StatusCode::Ok, "Operation completed")
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证Span已结束
  assert_true(azimuth::Span::is_ended(span))
}

// 测试6: 遥测数据序列化
test "遥测数据序列化测试" {
  let attributes = [
    ("service.name", azimuth::StringValue("test-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("port", azimuth::IntValue(8080))
  ]
  
  let resource = azimuth::Resource::new(attributes)
  
  // 序列化资源
  let serialized = azimuth::Serialization::serialize_resource(resource)
  
  // 验证序列化结果不为空
  assert_true(serialized.length() > 0)
  
  // 反序列化资源
  let deserialized = azimuth::Serialization::deserialize_resource(serialized)
  
  // 验证反序列化后的属性
  let deserialized_attrs = azimuth::Resource::attributes(deserialized)
  assert_eq(deserialized_attrs.length(), 3)
  
  // 验证特定属性值
  let service_name = azimuth::Resource::get_attribute(deserialized, "service.name")
  match service_name {
    Some(azimuth::StringValue(v)) => assert_eq(v, "test-service")
    _ => assert_true(false)
  }
}

// 测试7: 指标聚合
test "指标聚合测试" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-test")
  
  // 创建计数器指标
  let counter = azimuth::Meter::create_counter(meter, "test-counter", "test-unit", "Test counter description")
  
  // 记录多个测量值
  for i in 1..10 {
    azimuth::Counter::add(counter, i.to_int(), Some([("iteration", azimuth::StringValue(i.to_string()))]))
  }
  
  // 创建测量指标
  let gauge = azimuth::Meter::create_gauge(meter, "test-gauge", "test-unit", "Test gauge description")
  
  // 记录多个测量值
  for i in 1..5 {
    azimuth::Gauge::record(gauge, i.to_double() * 2.5, Some([("value", azimuth::StringValue((i * 2.5).to_string()))]))
  }
  
  // 验证指标聚合器可以正确收集数据
  let metrics = azimuth::MeterProvider::collect(meter_provider)
  
  // 验证收集到的指标数量
  assert_true(metrics.length() >= 2)
  
  // 验证计数器的总和应该是55 (1+2+...+10)
  let counter_metric = azimuth::Metrics::find_metric(metrics, "test-counter")
  match counter_metric {
    Some(m) => {
      let data_points = azimuth::Metric::data_points(m)
      assert_true(data_points.length() > 0)
      
      // 验证总和
      let sum = data_points.reduce(fn(acc, dp) { acc + azimuth::DataPoint::value(dp) }, 0.0)
      assert_true(abs(sum - 55.0) < 0.001)
    }
    None => assert_true(false)
  }
}

// 测试8: 日志记录
test "日志记录测试" {
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "log-test")
  
  // 创建日志记录器
  let log_record = azimuth::Logger::create_log_record(logger)
  
  // 设置日志属性
  azimuth::LogRecord::set_severity(log_record, azimuth::Severity::Info)
  azimuth::LogRecord::set_body(log_record, "Test log message")
  azimuth::LogRecord::set_attribute(log_record, "log.key", azimuth::StringValue("log-value"))
  
  // 添加事件
  azimuth::LogRecord::add_event(log_record, "log-event", Some([("event.key", azimuth::StringValue("event-value"))]))
  
  // 发出日志
  azimuth::Logger::emit_log(logger, log_record)
  
  // 验证日志记录器状态
  assert_eq(azimuth::Logger::get_name(logger), "log-test")
  
  // 验证日志处理器可以正确处理日志
  let log_processor = azimuth::LoggerProvider::get_processor(logger_provider)
  assert_true(azimuth::LogProcessor::is_active(log_processor))
}

// 测试9: 错误边界
test "错误边界测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "error-boundary-test")
  
  // 测试在Span处理错误时的边界行为
  let span = azimuth::Tracer::start_span(tracer, "error-boundary-span")
  
  // 设置错误状态
  azimuth::Span::set_status(span, azimuth::StatusCode::Error, "Simulated error")
  
  // 添加错误事件
  azimuth::Span::add_event(span, "exception", Some([
    ("exception.type", azimuth::StringValue("TestException")),
    ("exception.message", azimuth::StringValue("This is a test exception")),
    ("exception.stacktrace", azimuth::StringValue("at test.function:1"))
  ]))
  
  // 验证Span状态
  let status = azimuth::Span::get_status(span)
  match status {
    azimuth::Status { code: azimuth::StatusCode::Error, description: desc } => {
      assert_eq(desc, "Simulated error")
    }
    _ => assert_true(false)
  }
  
  // 结束Span
  azimuth::Span::end(span)
  
  // 验证Span已正确结束，即使在错误状态下
  assert_true(azimuth::Span::is_ended(span))
}

// 测试10: 性能优化
test "性能优化测试" {
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  // 测试批量Span创建和结束的性能
  let spans = []
  
  // 批量创建Span
  let start_time = azimuth::Time::now()
  for i in 0..100 {
    let span = azimuth::Tracer::start_span(tracer, "perf-span-" + i.to_string())
    spans.push(span)
  }
  let creation_time = azimuth::Time::now()
  
  // 批量结束Span
  for span in spans {
    azimuth::Span::end(span)
  }
  let end_time = azimuth::Time::now()
  
  // 验证性能指标
  let creation_duration = azimuth::Time::duration_between(start_time, creation_time)
  let end_duration = azimuth::Time::duration_between(creation_time, end_time)
  
  // 验证操作在合理时间内完成（这里只是示例，实际阈值应根据实际情况调整）
  assert_true(creation_duration < 1000)  // 创建100个Span应在1秒内完成
  assert_true(end_duration < 1000)       // 结束100个Span应在1秒内完成
  
  // 验证所有Span都已正确结束
  for span in spans {
    assert_true(azimuth::Span::is_ended(span))
  }
}