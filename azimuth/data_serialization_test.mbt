// 数据序列化测试用例
// 测试 Azimuth 遥测系统中的数据序列化功能

test "telemetry_span_serialization" {
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "serialization-test")
  
  // 创建span
  let span = Tracer::start_span(tracer, "serialization-test-span")
  let span_ctx = SpanContext::from_span(span)
  
  // 设置span属性
  let attrs = Attributes::new()
  Attributes::set(attrs, "user.id", StringValue("12345"))
  Attributes::set(attrs, "operation.type", StringValue("database.query"))
  Attributes::set(attrs, "retry.count", IntValue(3))
  
  Span::set_attributes(span, attrs)
  
  // 添加事件
  let event_attrs = Attributes::new()
  Attributes::set(event_attrs, "event.type", StringValue("query.start"))
  Attributes::set(event_attrs, "query.sql", StringValue("SELECT * FROM users"))
  
  Span::add_event(span, "query_started", Some(event_attrs))
  
  // 设置状态
  Span::set_status(span, StatusCode::OK)
  
  // 序列化span
  let serialized_span = Span::serialize(span)
  
  // 验证序列化结果不为空
  assert_true(serialized_span.length() > 0, "Serialized span should not be empty")
  
  // 反序列化span
  let deserialized_span = Span::deserialize(serialized_span)
  
  // 验证反序列化结果
  assert_eq(Span::name(deserialized_span), "serialization-test-span")
  
  let deserialized_ctx = SpanContext::from_span(deserialized_span)
  assert_eq(SpanContext::trace_id(deserialized_ctx), SpanContext::trace_id(span_ctx))
  assert_eq(SpanContext::span_id(deserialized_ctx), SpanContext::span_id(span_ctx))
  
  Span::end(span)
}

test "telemetry_metric_serialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization-metrics")
  
  // 创建指标
  let counter = Meter::create_counter(meter, "serialization.counter", Some("Test counter"), Some("count"))
  let histogram = Meter::create_histogram(meter, "serialization.histogram", Some("Test histogram"), Some("ms"))
  let gauge = Meter::create_gauge(meter, "serialization.gauge", Some("Test gauge"), Some("bytes"))
  
  // 记录指标值
  Counter::add(counter, 42.0)
  Histogram::record(histogram, 123.45)
  Gauge::record(gauge, 1024.0)
  
  // 序列化指标
  let serialized_counter = Metric::serialize(counter)
  let serialized_histogram = Metric::serialize(histogram)
  let serialized_gauge = Metric::serialize(gauge)
  
  // 验证序列化结果不为空
  assert_true(serialized_counter.length() > 0, "Serialized counter should not be empty")
  assert_true(serialized_histogram.length() > 0, "Serialized histogram should not be empty")
  assert_true(serialized_gauge.length() > 0, "Serialized gauge should not be empty")
  
  // 反序列化指标
  let deserialized_counter = Metric::deserialize(serialized_counter)
  let deserialized_histogram = Metric::deserialize(serialized_histogram)
  let deserialized_gauge = Metric::deserialize(serialized_gauge)
  
  // 验证反序列化结果
  assert_eq(Metric::name(deserialized_counter), "serialization.counter")
  assert_eq(Metric::name(deserialized_histogram), "serialization.histogram")
  assert_eq(Metric::name(deserialized_gauge), "serialization.gauge")
  
  assert_eq(Metric::description(deserialized_counter), Some("Test counter"))
  assert_eq(Metric::description(deserialized_histogram), Some("Test histogram"))
  assert_eq(Metric::description(deserialized_gauge), Some("Test gauge"))
}

test "telemetry_log_serialization" {
  let logger = LoggerProvider::get_logger(LoggerProvider::default(), "serialization-logger")
  
  // 创建日志记录
  let log_record = LogRecord::new()
  LogRecord::set_body(log_record, "Serialization test log message")
  LogRecord::set_severity(log_record, Severity::INFO)
  LogRecord::set_attribute(log_record, "log.source", StringValue("test-module"))
  LogRecord::set_attribute(log_record, "log.version", StringValue("1.0.0"))
  LogRecord::set_attribute(log_record, "request.id", StringValue("req-12345"))
  LogRecord::set_trace_id(log_record, "0af7651916cd43dd8448eb211c80319c")
  LogRecord::set_span_id(log_record, "b7ad6b7169203331")
  
  // 序列化日志记录
  let serialized_log = LogRecord::serialize(log_record)
  
  // 验证序列化结果不为空
  assert_true(serialized_log.length() > 0, "Serialized log record should not be empty")
  
  // 反序列化日志记录
  let deserialized_log = LogRecord::deserialize(serialized_log)
  
  // 验证反序列化结果
  assert_eq(LogRecord::body(deserialized_log), "Serialization test log message")
  assert_eq(LogRecord::severity(deserialized_log), Severity::INFO)
  
  let log_source = LogRecord::get_attribute(deserialized_log, "log.source")
  match log_source {
    Some(StringValue(source)) => assert_eq(source, "test-module")
    _ => assert_true(false, "Expected log source")
  }
  
  assert_eq(LogRecord::trace_id(deserialized_log), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(LogRecord::span_id(deserialized_log), "b7ad6b7169203331")
}

test "telemetry_attribute_serialization" {
  let attrs = Attributes::new()
  
  // 设置各种类型的属性
  Attributes::set(attrs, "string.attr", StringValue("test-value"))
  Attributes::set(attrs, "int.attr", IntValue(42))
  Attributes::set(attrs, "float.attr", FloatValue(3.14159))
  Attributes::set(attrs, "bool.attr", BoolValue(true))
  Attributes::set(attrs, "array.string", ArrayStringValue(["item1", "item2", "item3"]))
  Attributes::set(attrs, "array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 序列化属性
  let serialized_attrs = Attributes::serialize(attrs)
  
  // 验证序列化结果不为空
  assert_true(serialized_attrs.length() > 0, "Serialized attributes should not be empty")
  
  // 反序列化属性
  let deserialized_attrs = Attributes::deserialize(serialized_attrs)
  
  // 验证反序列化结果
  let string_attr = Attributes::get(deserialized_attrs, "string.attr")
  match string_attr {
    Some(StringValue(value)) => assert_eq(value, "test-value")
    _ => assert_true(false, "Expected string attribute")
  }
  
  let int_attr = Attributes::get(deserialized_attrs, "int.attr")
  match int_attr {
    Some(IntValue(value)) => assert_eq(value, 42)
    _ => assert_true(false, "Expected int attribute")
  }
  
  let float_attr = Attributes::get(deserialized_attrs, "float.attr")
  match float_attr {
    Some(FloatValue(value)) => assert_eq(value, 3.14159)
    _ => assert_true(false, "Expected float attribute")
  }
  
  let bool_attr = Attributes::get(deserialized_attrs, "bool.attr")
  match bool_attr {
    Some(BoolValue(value)) => assert_true(value)
    _ => assert_true(false, "Expected bool attribute")
  }
  
  let array_string = Attributes::get(deserialized_attrs, "array.string")
  match array_string {
    Some(ArrayStringValue(values)) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "item1")
      assert_eq(values[1], "item2")
      assert_eq(values[2], "item3")
    }
    _ => assert_true(false, "Expected array string attribute")
  }
}

test "telemetry_resource_serialization" {
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("serialization-service")),
    ("service.version", StringValue("1.0.0")),
    ("deployment.environment", StringValue("test")),
    ("host.name", StringValue("test-host")),
    ("process.id", IntValue(1234)),
    ("feature.flags", ArrayStringValue(["feature-a", "feature-b"]))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // 序列化资源
  let serialized_resource = Resource::serialize(resource_with_attrs)
  
  // 验证序列化结果不为空
  assert_true(serialized_resource.length() > 0, "Serialized resource should not be empty")
  
  // 反序列化资源
  let deserialized_resource = Resource::deserialize(serialized_resource)
  
  // 验证反序列化结果
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  match service_name {
    Some(StringValue(name)) => assert_eq(name, "serialization-service")
    _ => assert_true(false, "Expected service name")
  }
  
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  match service_version {
    Some(StringValue(version)) => assert_eq(version, "1.0.0")
    _ => assert_true(false, "Expected service version")
  }
  
  let process_id = Resource::get_attribute(deserialized_resource, "process.id")
  match process_id {
    Some(IntValue(pid)) => assert_eq(pid, 1234)
    _ => assert_true(false, "Expected process ID")
  }
  
  let feature_flags = Resource::get_attribute(deserialized_resource, "feature.flags")
  match feature_flags {
    Some(ArrayStringValue(flags)) => {
      assert_eq(flags.length(), 2)
      assert_eq(flags[0], "feature-a")
      assert_eq(flags[1], "feature-b")
    }
    _ => assert_true(false, "Expected feature flags")
  }
}

test "telemetry_baggage_serialization" {
  let baggage = Baggage::new()
  
  // 设置baggage项
  Baggage::set(baggage, "user.id", "12345")
  Baggage::set(baggage, "session.id", "session-abcdef")
  Baggage::set(baggage, "request.id", "req-xyz")
  Baggage::set(baggage, "trace.state", "custom=value")
  
  // 序列化baggage
  let serialized_baggage = Baggage::serialize(baggage)
  
  // 验证序列化结果不为空
  assert_true(serialized_baggage.length() > 0, "Serialized baggage should not be empty")
  
  // 反序列化baggage
  let deserialized_baggage = Baggage::deserialize(serialized_baggage)
  
  // 验证反序列化结果
  let user_id = Baggage::get(deserialized_baggage, "user.id")
  let session_id = Baggage::get(deserialized_baggage, "session.id")
  let request_id = Baggage::get(deserialized_baggage, "request.id")
  let trace_state = Baggage::get(deserialized_baggage, "trace.state")
  
  assert_eq(user_id, Some("12345"))
  assert_eq(session_id, Some("session-abcdef"))
  assert_eq(request_id, Some("req-xyz"))
  assert_eq(trace_state, Some("custom=value"))
}

test "telemetry_serialization_format_compatibility" {
  // 测试不同序列化格式的兼容性
  let provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(provider, "format-test")
  
  let span = Tracer::start_span(tracer, "format-test-span")
  
  // JSON格式序列化
  let json_serialized = Span::serialize_to_json(span)
  assert_true(json_serialized.length() > 0, "JSON serialization should not be empty")
  
  // 从JSON反序列化
  let json_deserialized = Span::deserialize_from_json(json_serialized)
  assert_eq(Span::name(json_deserialized), "format-test-span")
  
  // 二进制格式序列化
  let binary_serialized = Span::serialize_to_binary(span)
  assert_true(binary_serialized.length() > 0, "Binary serialization should not be empty")
  
  // 从二进制反序列化
  let binary_deserialized = Span::deserialize_from_binary(binary_serialized)
  assert_eq(Span::name(binary_deserialized), "format-test-span")
  
  // 验证不同格式的结果一致性
  assert_eq(Span::name(json_deserialized), Span::name(binary_deserialized))
  
  Span::end(span)
}