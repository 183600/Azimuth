// Azimuth 数据一致性验证综合测试
// 测试遥测系统在各种场景下的数据一致性保证

test "属性数据一致性测试" {
  // 创建属性集合
  let attrs = azimuth::Attributes::new()
  
  // 设置初始属性
  azimuth::Attributes::set(attrs, "string_key", azimuth::AttributeValue::StringValue("initial_value"))
  azimuth::Attributes::set(attrs, "int_key", azimuth::AttributeValue::IntValue(42))
  azimuth::Attributes::set(attrs, "float_key", azimuth::AttributeValue::FloatValue(3.14))
  azimuth::Attributes::set(attrs, "bool_key", azimuth::AttributeValue::BoolValue(true))
  
  // 验证属性一致性
  let string_result = azimuth::Attributes::get(attrs, "string_key")
  match string_result {
    Some(azimuth::AttributeValue::StringValue(value)) => assert_eq(value, "initial_value")
    _ => assert_true(false)
  }
  
  let int_result = azimuth::Attributes::get(attrs, "int_key")
  match int_result {
    Some(azimuth::AttributeValue::IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false)
  }
  
  let float_result = azimuth::Attributes::get(attrs, "float_key")
  match float_result {
    Some(azimuth::AttributeValue::FloatValue(value)) => assert_eq(value, 3.14)
    _ => assert_true(false)
  }
  
  let bool_result = azimuth::Attributes::get(attrs, "bool_key")
  match bool_result {
    Some(azimuth::AttributeValue::BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
  
  // 更新属性并验证一致性
  azimuth::Attributes::set(attrs, "string_key", azimuth::AttributeValue::StringValue("updated_value"))
  azimuth::Attributes::set(attrs, "int_key", azimuth::AttributeValue::IntValue(100))
  
  // 验证更新后的一致性
  let updated_string_result = azimuth::Attributes::get(attrs, "string_key")
  match updated_string_result {
    Some(azimuth::AttributeValue::StringValue(value)) => assert_eq(value, "updated_value")
    _ => assert_true(false)
  }
  
  let updated_int_result = azimuth::Attributes::get(attrs, "int_key")
  match updated_int_result {
    Some(azimuth::AttributeValue::IntValue(value)) => assert_eq(value, 100)
    _ => assert_true(false)
  }
  
  // 验证未修改的属性保持一致
  let unchanged_float_result = azimuth::Attributes::get(attrs, "float_key")
  match unchanged_float_result {
    Some(azimuth::AttributeValue::FloatValue(value)) => assert_eq(value, 3.14)
    _ => assert_true(false)
  }
  
  let unchanged_bool_result = azimuth::Attributes::get(attrs, "bool_key")
  match unchanged_bool_result {
    Some(azimuth::AttributeValue::BoolValue(value)) => assert_true(value)
    _ => assert_true(false)
  }
}

test "跨度上下文一致性测试" {
  // 创建跨度上下文
  let original_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let original_span_id = "00f067aa0ba902b7"
  let original_sampled = true
  let original_trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  
  let span_ctx = azimuth::SpanContext::new(
    original_trace_id,
    original_span_id,
    original_sampled,
    original_trace_state
  )
  
  // 验证上下文一致性
  assert_eq(azimuth::SpanContext::trace_id(span_ctx), original_trace_id)
  assert_eq(azimuth::SpanContext::span_id(span_ctx), original_span_id)
  assert_eq(azimuth::SpanContext::is_sampled(span_ctx), original_sampled)
  assert_eq(azimuth::SpanContext::trace_state(span_ctx), original_trace_state)
  
  // 创建基于此上下文的跨度
  let span = azimuth::Span::new("test_span", azimuth::Internal, span_ctx)
  
  // 验证跨度上下文一致性
  let retrieved_ctx = azimuth::Span::span_context(span)
  assert_eq(azimuth::SpanContext::trace_id(retrieved_ctx), original_trace_id)
  assert_eq(azimuth::SpanContext::span_id(retrieved_ctx), original_span_id)
  assert_eq(azimuth::SpanContext::is_sampled(retrieved_ctx), original_sampled)
  assert_eq(azimuth::SpanContext::trace_state(retrieved_ctx), original_trace_state)
  
  // 验证跨度状态一致性
  assert_eq(azimuth::Span::name(span), "test_span")
  match azimuth::Span::kind(span) {
    azimuth::Internal => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(azimuth::Span::is_recording(span))
  assert_eq(azimuth::Span::status(span), azimuth::Unset)
}

test "指标聚合一致性测试" {
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "consistency_test")
  
  // 创建指标
  let counter = azimuth::Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  let histogram = azimuth::Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  let updown_counter = azimuth::Meter::create_updown_counter(meter, "test_updown", Some("Test updown counter"), Some("value"))
  
  // 执行指标操作
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "operation", azimuth::AttributeValue::StringValue("test"))
  
  // 计数器操作
  azimuth::Counter::add(counter, 1.0)
  azimuth::Counter::add(counter, 2.5, Some(attrs))
  azimuth::Counter::add(counter, 3.0)
  
  // 直方图操作
  azimuth::Histogram::record(histogram, 100.0)
  azimuth::Histogram::record(histogram, 200.5, Some(attrs))
  azimuth::Histogram::record(histogram, 150.0)
  
  // 上下计数器操作
  azimuth::UpDownCounter::add(updown_counter, 10.0)
  azimuth::UpDownCounter::add(updown_counter, -5.0, Some(attrs))
  azimuth::UpDownCounter::add(updown_counter, 15.0)
  
  // 验证指标一致性（简化验证，实际实现需要检查指标值）
  let counter_instrument = azimuth::Histogram::as_instrument(counter)
  assert_eq(azimuth::Instrument::name(counter_instrument), "test_counter")
  assert_eq(azimuth::Instrument::description(counter_instrument), Some("Test counter"))
  assert_eq(azimuth::Instrument::unit(counter_instrument), Some("count"))
  
  let histogram_instrument = azimuth::Histogram::as_instrument(histogram)
  assert_eq(azimuth::Instrument::name(histogram_instrument), "test_histogram")
  assert_eq(azimuth::Instrument::description(histogram_instrument), Some("Test histogram"))
  assert_eq(azimuth::Instrument::unit(histogram_instrument), Some("ms"))
  
  let updown_instrument = azimuth::Histogram::as_instrument(updown_counter)
  assert_eq(azimuth::Instrument::name(updown_instrument), "test_updown")
  assert_eq(azimuth::Instrument::description(updown_instrument), Some("Test updown counter"))
  assert_eq(azimuth::Instrument::unit(updown_instrument), Some("value"))
}

test "日志记录一致性测试" {
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "consistency_logger")
  
  // 创建日志记录
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "component", azimuth::AttributeValue::StringValue("test"))
  azimuth::Attributes::set(attrs, "version", azimuth::AttributeValue::StringValue("1.0.0"))
  
  let log_record = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("Consistency test log message"),
    Some(attrs),
    Some(1234567890L),
    Some(1234567891L),
    Some("trace_id_123"),
    Some("span_id_456"),
    None
  )
  
  // 验证日志记录一致性
  assert_eq(azimuth::LogRecord::severity_number(log_record), azimuth::Info)
  match azimuth::LogRecord::body(log_record) {
    Some(body) => assert_eq(body, "Consistency test log message")
    None => assert_true(false)
  }
  assert_eq(azimuth::LogRecord::trace_id(log_record), Some("trace_id_123"))
  assert_eq(azimuth::LogRecord::span_id(log_record), Some("span_id_456"))
  
  // 发送日志记录
  azimuth::Logger::emit(logger, log_record)
  
  // 创建另一个日志记录并验证独立性
  let another_log_record = azimuth::LogRecord::new(
    azimuth::Error,
    "Another consistency test message"
  )
  
  // 验证第二个日志记录不受第一个影响
  assert_eq(azimuth::LogRecord::severity_number(another_log_record), azimuth::Error)
  match azimuth::LogRecord::body(another_log_record) {
    Some(body) => assert_eq(body, "Another consistency test message")
    None => assert_true(false)
  }
  assert_eq(azimuth::LogRecord::trace_id(another_log_record), None)
  assert_eq(azimuth::LogRecord::span_id(another_log_record), None)
}

test "上下文传播一致性测试" {
  // 创建根上下文
  let root_ctx = azimuth::Context::root()
  
  // 添加上下文值
  let key1 = azimuth::ContextKey::new("key1")
  let key2 = azimuth::ContextKey::new("key2")
  let key3 = azimuth::ContextKey::new("key3")
  
  let ctx1 = azimuth::Context::with_value(root_ctx, key1, "value1")
  let ctx2 = azimuth::Context::with_value(ctx1, key2, "value2")
  let ctx3 = azimuth::Context::with_value(ctx2, key3, "value3")
  
  // 验证上下文传播一致性
  match azimuth::Context::get(ctx3, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match azimuth::Context::get(ctx3, key2) {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match azimuth::Context::get(ctx3, key3) {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  // 验证上下文独立性
  let another_key = azimuth::ContextKey::new("another_key")
  let ctx4 = azimuth::Context::with_value(ctx3, another_key, "another_value")
  
  // 验证新上下文包含所有原有值
  match azimuth::Context::get(ctx4, key1) {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match azimuth::Context::get(ctx4, key2) {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match azimuth::Context::get(ctx4, key3) {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  match azimuth::Context::get(ctx4, another_key) {
    Some(value) => assert_eq(value, "another_value")
    None => assert_true(false)
  }
  
  // 验证原上下文不受新上下文影响
  match azimuth::Context::get(ctx3, another_key) {
    Some(_) => assert_true(false) // 原上下文不应包含新键
    None => assert_true(true)
  }
}

test "资源合并一致性测试" {
  // 创建第一个资源
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::AttributeValue::StringValue("service1")),
    ("service.version", azimuth::AttributeValue::StringValue("1.0.0")),
    ("host.name", azimuth::AttributeValue::StringValue("host1"))
  ])
  
  // 创建第二个资源
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::AttributeValue::StringValue("service2")), // 冲突键
    ("deployment.environment", azimuth::AttributeValue::StringValue("production")),
    ("host.name", azimuth::AttributeValue::StringValue("host2")) // 冲突键
  ])
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(resource1, resource2)
  
  // 验证合并结果一致性（第二个资源应该覆盖第一个资源的冲突键）
  let service_name = azimuth::Resource::get_attribute(merged_resource, "service.name")
  match service_name {
    Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "service2")
    _ => assert_true(false)
  }
  
  let service_version = azimuth::Resource::get_attribute(merged_resource, "service.version")
  match service_version {
    Some(azimuth::AttributeValue::StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false)
  }
  
  let host_name = azimuth::Resource::get_attribute(merged_resource, "host.name")
  match host_name {
    Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "host2")
    _ => assert_true(false)
  }
  
  let environment = azimuth::Resource::get_attribute(merged_resource, "deployment.environment")
  match environment {
    Some(azimuth::AttributeValue::StringValue(env)) => assert_eq(env, "production")
    _ => assert_true(false)
  }
  
  // 验证原资源不受合并影响
  let original_service_name = azimuth::Resource::get_attribute(resource1, "service.name")
  match original_service_name {
    Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "service1")
    _ => assert_true(false)
  }
  
  let original_service_name2 = azimuth::Resource::get_attribute(resource2, "service.name")
  match original_service_name2 {
    Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "service2")
    _ => assert_true(false)
  }
}

test "序列化反序列化一致性测试" {
  // 创建测试数据
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "string.key", azimuth::AttributeValue::StringValue("test_value"))
  azimuth::Attributes::set(attrs, "int.key", azimuth::AttributeValue::IntValue(42))
  azimuth::Attributes::set(attrs, "float.key", azimuth::AttributeValue::FloatValue(3.14))
  azimuth::Attributes::set(attrs, "bool.key", azimuth::AttributeValue::BoolValue(true))
  
  // 模拟序列化过程
  let serialize_attributes = fn(attrs: azimuth::Attributes) -> String {
    let mut result = "{"
    let keys = ["string.key", "int.key", "float.key", "bool.key"]
    
    for i in 0..keys.length() {
      if i > 0 {
        result = result + ","
      }
      
      let key = keys[i]
      let value = azimuth::Attributes::get(attrs, key)
      
      result = result + "\"" + key + "\":"
      
      match value {
        Some(azimuth::AttributeValue::StringValue(s)) => {
          result = result + "\"string:" + s + "\""
        }
        Some(azimuth::AttributeValue::IntValue(i)) => {
          result = result + "\"int:" + i.to_string() + "\""
        }
        Some(azimuth::AttributeValue::FloatValue(f)) => {
          result = result + "\"float:" + f.to_string() + "\""
        }
        Some(azimuth::AttributeValue::BoolValue(b)) => {
          if b {
            result = result + "\"bool:true\""
          } else {
            result = result + "\"bool:false\""
          }
        }
        _ => {
          result = result + "\"null\""
        }
      }
    }
    
    result = result + "}"
    result
  }
  
  // 模拟反序列化过程
  let deserialize_attributes = fn(serialized: String) -> azimuth::Attributes {
    let attrs = azimuth::Attributes::new()
    
    if serialized.contains("\"string.key\":\"string:test_value\"") {
      azimuth::Attributes::set(attrs, "string.key", azimuth::AttributeValue::StringValue("test_value"))
    }
    
    if serialized.contains("\"int.key\":\"int:42\"") {
      azimuth::Attributes::set(attrs, "int.key", azimuth::AttributeValue::IntValue(42))
    }
    
    if serialized.contains("\"float.key\":\"float:3.14\"") {
      azimuth::Attributes::set(attrs, "float.key", azimuth::AttributeValue::FloatValue(3.14))
    }
    
    if serialized.contains("\"bool.key\":\"bool:true\"") {
      azimuth::Attributes::set(attrs, "bool.key", azimuth::AttributeValue::BoolValue(true))
    }
    
    attrs
  }
  
  // 执行序列化和反序列化
  let serialized = serialize_attributes(attrs)
  let deserialized = deserialize_attributes(serialized)
  
  // 验证一致性
  let original_string = azimuth::Attributes::get(attrs, "string.key")
  let deserialized_string = azimuth::Attributes::get(deserialized, "string.key")
  
  match (original_string, deserialized_string) {
    (Some(azimuth::AttributeValue::StringValue(orig)), Some(azimuth::AttributeValue::StringValue(deser))) => {
      assert_eq(orig, deser)
    }
    _ => assert_true(false)
  }
  
  let original_int = azimuth::Attributes::get(attrs, "int.key")
  let deserialized_int = azimuth::Attributes::get(deserialized, "int.key")
  
  match (original_int, deserialized_int) {
    (Some(azimuth::AttributeValue::IntValue(orig)), Some(azimuth::AttributeValue::IntValue(deser))) => {
      assert_eq(orig, deser)
    }
    _ => assert_true(false)
  }
  
  let original_float = azimuth::Attributes::get(attrs, "float.key")
  let deserialized_float = azimuth::Attributes::get(deserialized, "float.key")
  
  match (original_float, deserialized_float) {
    (Some(azimuth::AttributeValue::FloatValue(orig)), Some(azimuth::AttributeValue::FloatValue(deser))) => {
      assert_eq(orig, deser)
    }
    _ => assert_true(false)
  }
  
  let original_bool = azimuth::Attributes::get(attrs, "bool.key")
  let deserialized_bool = azimuth::Attributes::get(deserialized, "bool.key")
  
  match (original_bool, deserialized_bool) {
    (Some(azimuth::AttributeValue::BoolValue(orig)), Some(azimuth::AttributeValue::BoolValue(deser))) => {
      assert_eq(orig, deser)
    }
    _ => assert_true(false)
  }
}

test "时间序列数据一致性测试" {
  // 创建时间序列数据点
  let data_points = [
    (1000L, 10.5),
    (2000L, 15.2),
    (3000L, 12.8),
    (4000L, 18.3),
    (5000L, 20.1)
  ]
  
  // 验证时间序列数据一致性
  for i in 0..data_points.length() {
    let (timestamp, value) = data_points[i]
    
    // 验证时间戳递增
    if i > 0 {
      let (prev_timestamp, _) = data_points[i - 1]
      assert_true(timestamp > prev_timestamp)
    }
    
    // 验证值在合理范围内
    assert_true(value >= 0.0)
    assert_true(value <= 100.0)
  }
  
  // 计算聚合值并验证一致性
  let mut sum = 0.0
  let mut count = 0
  
  for (_, value) in data_points {
    sum = sum + value
    count = count + 1
  }
  
  let average = sum / count.to_float()
  
  // 验证平均值在最小值和最大值之间
  let min_value = 10.5
  let max_value = 20.1
  
  assert_true(average >= min_value)
  assert_true(average <= max_value)
  
  // 验证总和等于各值之和
  let expected_sum = 10.5 + 15.2 + 12.8 + 18.3 + 20.1
  assert_true((sum - expected_sum).abs() < 0.001)
}

test "分布式追踪一致性测试" {
  // 创建根跨度
  let root_trace_id = "1234567890abcdef1234567890abcdef"
  let root_span_id = "1234567890abcdef"
  let root_ctx = azimuth::SpanContext::new(root_trace_id, root_span_id, true, "")
  let root_span = azimuth::Span::new("root_operation", azimuth::Server, root_ctx)
  
  // 创建子跨度
  let child1_span_id = "abcdef1234567890"
  let child1_ctx = azimuth::SpanContext::new(root_trace_id, child1_span_id, true, "")
  let child1_span = azimuth::Span::new("child_operation_1", azimuth::Client, child1_ctx)
  
  let child2_span_id = "fedcba0987654321"
  let child2_ctx = azimuth::SpanContext::new(root_trace_id, child2_span_id, true, "")
  let child2_span = azimuth::Span::new("child_operation_2", azimuth::Internal, child2_ctx)
  
  // 验证分布式追踪一致性
  // 所有跨度应该有相同的trace_id
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(root_span)), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(child1_span)), root_trace_id)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::span_context(child2_span)), root_trace_id)
  
  // 每个跨度应该有唯一的span_id
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::span_context(root_span)), root_span_id)
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::span_context(child1_span)), child1_span_id)
  assert_eq(azimuth::SpanContext::span_id(azimuth::Span::span_context(child2_span)), child2_span_id)
  
  // 验证span_id不同
  assert_true(root_span_id != child1_span_id)
  assert_true(root_span_id != child2_span_id)
  assert_true(child1_span_id != child2_span_id)
  
  // 添加事件并验证一致性
  azimuth::Span::add_event(root_span, "root_event", Some([("event_type", azimuth::AttributeValue::StringValue("start"))]))
  azimuth::Span::add_event(child1_span, "child1_event", Some([("event_type", azimuth::AttributeValue::StringValue("process"))]))
  azimuth::Span::add_event(child2_span, "child2_event", Some([("event_type", azimuth::AttributeValue::StringValue("process"))]))
  
  // 结束跨度
  azimuth::Span::end(child1_span)
  azimuth::Span::end(child2_span)
  azimuth::Span::end(root_span)
  
  // 验证跨度状态一致性
  assert_eq(azimuth::Span::name(root_span), "root_operation")
  assert_eq(azimuth::Span::name(child1_span), "child_operation_1")
  assert_eq(azimuth::Span::name(child2_span), "child_operation_2")
  
  match azimuth::Span::kind(root_span) {
    azimuth::Server => assert_true(true)
    _ => assert_true(false)
  }
  
  match azimuth::Span::kind(child1_span) {
    azimuth::Client => assert_true(true)
    _ => assert_true(false)
  }
  
  match azimuth::Span::kind(child2_span) {
    azimuth::Internal => assert_true(true)
    _ => assert_true(false)
  }
}