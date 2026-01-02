// Serialization/Deserialization Tests
// 验证遥测数据的序列化和反序列化功能

test "span serialization and deserialization" {
  // 创建测试span
  let span_ctx = azimuth::SpanContext::new("trace-12345", "span-67890", true, "key1=value1,key2=value2")
  let span = azimuth::Span::new("test-operation", azimuth::Internal, span_ctx)
  
  // 设置span属性
  azimuth::Span::set_attribute(span, "user.id", azimuth::IntValue(42))
  azimuth::Span::set_attribute(span, "request.size", azimuth::IntValue(1024))
  azimuth::Span::set_attribute(span, "response.time", azimuth::FloatValue(123.45))
  azimuth::Span::set_attribute(span, "cache.hit", azimuth::BoolValue(true))
  azimuth::Span::set_attribute(span, "tags", azimuth::ArrayStringValue(["tag1", "tag2", "tag3"]))
  
  // 添加span事件
  azimuth::Span::add_event(span, "event1", [{"event.data", azimuth::StringValue("event data 1")}])
  azimuth::Span::add_event(span, "event2", [{"event.data", azimuth::StringValue("event data 2")}])
  
  // 序列化span
  let serialized = azimuth::Span::serialize(span)
  assert_true(serialized.length > 0)
  
  // 反序列化span
  let deserialized_span = azimuth::Span::deserialize(serialized)
  assert_not_eq(deserialized_span, None)
  
  // 验证反序列化的数据正确性
  let original_name = azimuth::Span::name(span)
  let deserialized_name = azimuth::Span::name(deserialized_span)
  assert_eq(original_name, deserialized_name)
  
  let original_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::context(span))
  let deserialized_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::context(deserialized_span))
  assert_eq(original_trace_id, deserialized_trace_id)
  
  // 验证属性
  let original_user_id = azimuth::Span::get_attribute(span, "user.id")
  let deserialized_user_id = azimuth::Span::get_attribute(deserialized_span, "user.id")
  assert_eq(original_user_id, deserialized_user_id)
}

test "metrics serialization and deserialization" {
  // 创建测试指标
  let provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(provider, "serialization-test-meter")
  let counter = azimuth::Meter::create_counter(meter, "test.counter")
  let histogram = azimuth::Meter::create_histogram(meter, "test.histogram")
  
  // 记录指标数据
  azimuth::Counter::add(counter, 10, [{"method", azimuth::StringValue("GET")}])
  azimuth::Counter::add(counter, 5, [{"method", azimuth::StringValue("POST")}])
  
  azimuth::Histogram::record(histogram, 100.0, [{"status", azimuth::StringValue("200")}])
  azimuth::Histogram::record(histogram, 200.0, [{"status", azimuth::StringValue("500")}])
  azimuth::Histogram::record(histogram, 150.0, [{"status", azimuth::StringValue("200")}])
  
  // 收集指标数据
  let counter_metrics = azimuth::Counter::collect(counter)
  let histogram_metrics = azimuth::Histogram::collect(histogram)
  
  // 序列化指标数据
  let serialized_counter = azimuth::Metrics::serialize(counter_metrics)
  let serialized_histogram = azimuth::Metrics::serialize(histogram_metrics)
  
  assert_true(serialized_counter.length > 0)
  assert_true(serialized_histogram.length > 0)
  
  // 反序列化指标数据
  let deserialized_counter = azimuth::Metrics::deserialize_counter(serialized_counter)
  let deserialized_histogram = azimuth::Metrics::deserialize_histogram(serialized_histogram)
  
  // 验证反序列化的数据正确性
  assert_eq(deserialized_counter.length, counter_metrics.length)
  assert_eq(deserialized_histogram.length, histogram_metrics.length)
  
  // 验证具体的指标值
  for i in 0..counter_metrics.length {
    let original = counter_metrics[i]
    let deserialized = deserialized_counter[i]
    
    let original_value = azimuth::CounterMetric::value(original)
    let deserialized_value = azimuth::CounterMetric::value(deserialized)
    assert_eq(original_value, deserialized_value)
  }
}

test "log record serialization and deserialization" {
  // 创建测试日志记录
  let provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(provider, "serialization-test-logger")
  
  // 创建日志记录
  let log_record = azimuth::Logger::create_log_record(logger)
  azimuth::LogRecord::set_body(log_record, "Test log message with serialization")
  azimuth::LogRecord::set_severity(log_record, azimuth::INFO)
  azimuth::LogRecord::set_trace_id(log_record, "trace-serialization-test")
  azimuth::LogRecord::set_span_id(log_record, "span-serialization-test")
  azimuth::LogRecord::set_attribute(log_record, "user.id", azimuth::IntValue(123))
  azimuth::LogRecord::set_attribute(log_record, "request.path", azimuth::StringValue("/api/test"))
  
  // 序列化日志记录
  let serialized = azimuth::LogRecord::serialize(log_record)
  assert_true(serialized.length > 0)
  
  // 反序列化日志记录
  let deserialized_log = azimuth::LogRecord::deserialize(serialized)
  assert_not_eq(deserialized_log, None)
  
  // 验证反序列化的数据正确性
  let original_body = azimuth::LogRecord::body(log_record)
  let deserialized_body = azimuth::LogRecord::body(deserialized_log)
  assert_eq(original_body, deserialized_body)
  
  let original_severity = azimuth::LogRecord::severity(log_record)
  let deserialized_severity = azimuth::LogRecord::severity(deserialized_log)
  assert_eq(original_severity, deserialized_severity)
  
  let original_trace_id = azimuth::LogRecord::trace_id(log_record)
  let deserialized_trace_id = azimuth::LogRecord::trace_id(deserialized_log)
  assert_eq(original_trace_id, deserialized_trace_id)
}

test "resource serialization and deserialization" {
  // 创建测试资源
  let resource = azimuth::Resource::new()
  azimuth::Resource::set_attribute(resource, "service.name", azimuth::StringValue("test-service"))
  azimuth::Resource::set_attribute(resource, "service.version", azimuth::StringValue("1.0.0"))
  azimuth::Resource::set_attribute(resource, "service.instance.id", azimuth::StringValue("instance-123"))
  azimuth::Resource::set_attribute(resource, "host.name", azimuth::StringValue("test-host"))
  azimuth::Resource::set_attribute(resource, "env", azimuth::StringValue("test"))
  azimuth::Resource::set_attribute(resource, "region", azimuth::StringValue("us-west-2"))
  
  // 序列化资源
  let serialized = azimuth::Resource::serialize(resource)
  assert_true(serialized.length > 0)
  
  // 反序列化资源
  let deserialized_resource = azimuth::Resource::deserialize(serialized)
  assert_not_eq(deserialized_resource, None)
  
  // 验证反序列化的属性
  let service_name = azimuth::Resource::get_attribute(deserialized_resource, "service.name")
  let service_version = azimuth::Resource::get_attribute(deserialized_resource, "service.version")
  let host_name = azimuth::Resource::get_attribute(deserialized_resource, "host.name")
  let env = azimuth::Resource::get_attribute(deserialized_resource, "env")
  
  assert_eq(service_name, Some(azimuth::StringValue("test-service")))
  assert_eq(service_version, Some(azimuth::StringValue("1.0.0")))
  assert_eq(host_name, Some(azimuth::StringValue("test-host")))
  assert_eq(env, Some(azimuth::StringValue("test")))
}

test "batch serialization and deserialization" {
  // 创建多个测试对象
  let spans = []
  let log_records = []
  
  // 创建多个span
  for i in 0..10 {
    let span_ctx = azimuth::SpanContext::new("batch-trace-" + i.to_string(), "batch-span-" + i.to_string(), true, "")
    let span = azimuth::Span::new("batch-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    azimuth::Span::set_attribute(span, "batch.index", azimuth::IntValue(i))
    spans.push(span)
  }
  
  // 创建多个日志记录
  let logger_provider = azimuth::LoggerProvider::default()
  let logger = azimuth::LoggerProvider::get_logger(logger_provider, "batch-test-logger")
  
  for i in 0..5 {
    let log_record = azimuth::Logger::create_log_record(logger)
    azimuth::LogRecord::set_body(log_record, "Batch log message " + i.to_string())
    azimuth::LogRecord::set_severity(log_record, azimuth::INFO)
    azimuth::LogRecord::set_attribute(log_record, "batch.index", azimuth::IntValue(i))
    log_records.push(log_record)
  }
  
  // 批量序列化
  let serialized_spans = azimuth::BatchSerializer::serialize_spans(spans)
  let serialized_logs = azimuth::BatchSerializer::serialize_log_records(log_records)
  
  assert_true(serialized_spans.length > 0)
  assert_true(serialized_logs.length > 0)
  
  // 批量反序列化
  let deserialized_spans = azimuth::BatchSerializer::deserialize_spans(serialized_spans)
  let deserialized_logs = azimuth::BatchSerializer::deserialize_log_records(serialized_logs)
  
  // 验证批量反序列化的正确性
  assert_eq(deserialized_spans.length, spans.length)
  assert_eq(deserialized_logs.length, log_records.length)
  
  // 验证具体数据
  for i in 0..spans.length {
    let original_name = azimuth::Span::name(spans[i])
    let deserialized_name = azimuth::Span::name(deserialized_spans[i])
    assert_eq(original_name, deserialized_name)
  }
  
  for i in 0..log_records.length {
    let original_body = azimuth::LogRecord::body(log_records[i])
    let deserialized_body = azimuth::LogRecord::body(deserialized_logs[i])
    assert_eq(original_body, deserialized_body)
  }
}

test "serialization format compatibility" {
  // 测试不同序列化格式的兼容性
  let span_ctx = azimuth::SpanContext::new("compat-trace", "compat-span", true, "")
  let span = azimuth::Span::new("compat-operation", azimuth::Internal, span_ctx)
  azimuth::Span::set_attribute(span, "compat.test", azimuth::StringValue("true"))
  
  // 测试JSON格式序列化
  let json_serialized = azimuth::Span::serialize_to_json(span)
  assert_true(json_serialized.length > 0)
  
  let json_deserialized = azimuth::Span::deserialize_from_json(json_serialized)
  assert_not_eq(json_deserialized, None)
  
  let json_name = azimuth::Span::name(json_deserialized)
  assert_eq(json_name, "compat-operation")
  
  // 测试二进制格式序列化
  let binary_serialized = azimuth::Span::serialize_to_binary(span)
  assert_true(binary_serialized.length > 0)
  
  let binary_deserialized = azimuth::Span::deserialize_from_binary(binary_serialized)
  assert_not_eq(binary_deserialized, None)
  
  let binary_name = azimuth::Span::name(binary_deserialized)
  assert_eq(binary_name, "compat-operation")
  
  // 验证不同格式的结果一致
  assert_eq(json_name, binary_name)
}