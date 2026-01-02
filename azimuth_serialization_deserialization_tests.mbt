// Azimuth Telemetry Data Serialization/Deserialization Tests
// 遥测数据序列化/反序列化测试 - 验证遥测数据的序列化和反序列化功能

test "Span数据JSON序列化反序列化" {
  // 测试Span数据的JSON序列化和反序列化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.json.test")
  
  // 创建包含各种属性的span
  let span = Tracer::start_span(tracer, "json.serialization.test")
  Span::set_attribute(span, "string.attr", StringValue("test.value"))
  Span::set_attribute(span, "int.attr", IntValue(42))
  Span::set_attribute(span, "float.attr", FloatValue(3.14))
  Span::set_attribute(span, "bool.attr", BoolValue(true))
  Span::set_attribute(span, "array.string", ArrayStringValue(["item1", "item2", "item3"]))
  Span::set_attribute(span, "array.int", ArrayIntValue([1, 2, 3, 4, 5]))
  
  // 添加事件
  Span::add_event(span, "test.event.1", Some([
    ("event.string", StringValue("event.value")),
    ("event.int", IntValue(100))
  ]))
  
  Span::add_event(span, "test.event.2", Some([
    ("event.float", FloatValue(2.71)),
    ("event.bool", BoolValue(false))
  ]))
  
  // 序列化为JSON
  let json_data = Span::serialize_to_json(span)
  
  // 验证JSON数据不为空
  assert_true(json_data.length() > 0)
  
  // 从JSON反序列化
  let deserialized_span = Span::deserialize_from_json(json_data)
  
  // 验证反序列化的span属性
  let original_string_attr = Span::get_attribute(span, "string.attr")
  let deserialized_string_attr = Span::get_attribute(deserialized_span, "string.attr")
  
  match (original_string_attr, deserialized_string_attr) {
    (Some(StringValue(orig)), Some(StringValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
  
  // 验证反序列化的span事件
  let original_events = Span::get_events(span)
  let deserialized_events = Span::get_events(deserialized_span)
  
  assert_eq(original_events.length(), deserialized_events.length())
  
  Span::end(span)
  assert_true(true)
}

test "Span数据二进制序列化反序列化" {
  // 测试Span数据的二进制序列化和反序列化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.binary.test")
  
  // 创建span
  let span = Tracer::start_span(tracer, "binary.serialization.test")
  Span::set_attribute(span, "binary.test", StringValue("binary.serialization.data"))
  Span::set_attribute(span, "large.data", StringValue("x".repeat(1000)))
  
  // 序列化为二进制数据
  let binary_data = Span::serialize_to_binary(span)
  
  // 验证二进制数据不为空
  assert_true(binary_data.length() > 0)
  
  // 从二进制数据反序列化
  let deserialized_span = Span::deserialize_from_binary(binary_data)
  
  // 验证反序列化的span属性
  let original_attr = Span::get_attribute(span, "binary.test")
  let deserialized_attr = Span::get_attribute(deserialized_span, "binary.test")
  
  match (original_attr, deserialized_attr) {
    (Some(StringValue(orig)), Some(StringValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
  
  // 验证大数据属性
  let original_large_attr = Span::get_attribute(span, "large.data")
  let deserialized_large_attr = Span::get_attribute(deserialized_span, "large.data")
  
  match (original_large_attr, deserialized_large_attr) {
    (Some(StringValue(orig)), Some(StringValue(deser))) => assert_eq(orig, deser)
    _ => assert_true(false)
  }
  
  Span::end(span)
  assert_true(true)
}

test "Context数据序列化反序列化" {
  // 测试Context数据的序列化和反序列化
  let context = Context::root()
  
  // 设置多个键值对
  let key1 = ContextKey::new("test.key.1")
  let key2 = ContextKey::new("test.key.2")
  let key3 = ContextKey::new("test.key.3")
  
  let context_with_values = context
    |> Context::with_value(key1, "value1")
    |> Context::with_value(key2, "value2")
    |> Context::with_value(key3, "value3")
  
  // 序列化context
  let serialized_context = Context::serialize(context_with_values)
  
  // 反序列化context
  let deserialized_context = Context::deserialize(serialized_context)
  
  // 验证反序列化的context值
  let deserialized_value1 = Context::get(deserialized_context, key1)
  let deserialized_value2 = Context::get(deserialized_context, key2)
  let deserialized_value3 = Context::get(deserialized_context, key3)
  
  match deserialized_value1 {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match deserialized_value2 {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match deserialized_value3 {
    Some(value) => assert_eq(value, "value3")
    None => assert_true(false)
  }
  
  assert_true(true)
}

test "SpanContext数据序列化反序列化" {
  // 测试SpanContext数据的序列化和反序列化
  let span_context = SpanContext::new(
    "0af7651916cd43dd8448eb211c80319c",
    "b7ad6b7169203331",
    true,
    "rojo=00f067aa0ba902b7"
  )
  
  // 序列化span context
  let serialized_context = SpanContext::serialize(span_context)
  
  // 反序列化span context
  let deserialized_context = SpanContext::deserialize(serialized_context)
  
  // 验证反序列化的span context
  assert_eq(SpanContext::get_trace_id(deserialized_context), "0af7651916cd43dd8448eb211c80319c")
  assert_eq(SpanContext::get_span_id(deserialized_context), "b7ad6b7169203331")
  assert_eq(SpanContext::is_sampled(deserialized_context), true)
  assert_eq(SpanContext::get_trace_state(deserialized_context), "rojo=00f067aa0ba902b7")
  
  assert_true(true)
}

test "Resource数据序列化反序列化" {
  // 测试Resource数据的序列化和反序列化
  let resource = Resource::new()
  let attributes = [
    ("service.name", StringValue("test.service")),
    ("service.version", StringValue("1.0.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("test")),
    ("host.name", StringValue("test-host")),
    ("host.arch", StringValue("amd64")),
    ("os.type", StringValue("linux")),
    ("os.version", StringValue("5.15.0")),
    ("process.pid", IntValue(12345)),
    ("process.executable.name", StringValue("test-app")),
    ("process.command_args", ArrayStringValue(["./test-app", "--config", "test.toml"]))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, attributes)
  
  // 序列化resource
  let serialized_resource = Resource::serialize(resource_with_attrs)
  
  // 反序列化resource
  let deserialized_resource = Resource::deserialize(serialized_resource)
  
  // 验证反序列化的resource属性
  for (key, original_value) in attributes {
    let deserialized_value = Resource::get_attribute(deserialized_resource, key)
    
    match (original_value, deserialized_value) {
      (StringValue(orig), Some(StringValue(deser))) => assert_eq(orig, deser)
      (IntValue(orig), Some(IntValue(deser))) => assert_eq(orig, deser)
      (ArrayStringValue(orig), Some(ArrayStringValue(deser))) => {
        assert_eq(orig.length(), deser.length())
        for i = 0; i < orig.length(); i = i + 1 {
          assert_eq(orig[i], deser[i])
        }
      }
      _ => assert_true(false)
    }
  }
  
  assert_true(true)
}

test "Baggage数据序列化反序列化" {
  // 测试Baggage数据的序列化和反序列化
  let baggage = Baggage::new()
  
  // 添加多个条目
  let baggage_with_entries = baggage
    |> Baggage::set_entry("user.id", "user-12345")
    |> Baggage::set_entry("session.id", "session-abcdef")
    |> Baggage::set_entry("request.id", "req-67890")
    |> Baggage::set_entry("tenant.id", "tenant-001")
    |> Baggage::set_entry("correlation.id", "corr-xyz")
  
  // 序列化baggage
  let serialized_baggage = Baggage::serialize(baggage_with_entries)
  
  // 反序列化baggage
  let deserialized_baggage = Baggage::deserialize(serialized_baggage)
  
  // 验证反序列化的baggage条目
  let entries = Baggage::get_entries(deserialized_baggage)
  assert_eq(entries.length(), 5)
  
  // 验证特定条目
  let user_id = Baggage::get_entry(deserialized_baggage, "user.id")
  let session_id = Baggage::get_entry(deserialized_baggage, "session.id")
  let request_id = Baggage::get_entry(deserialized_baggage, "request.id")
  
  match user_id {
    Some(value) => assert_eq(value, "user-12345")
    None => assert_true(false)
  }
  
  match session_id {
    Some(value) => assert_eq(value, "session-abcdef")
    None => assert_true(false)
  }
  
  match request_id {
    Some(value) => assert_eq(value, "req-67890")
    None => assert_true(false)
  }
  
  assert_true(true)
}

test "Metrics数据序列化反序列化" {
  // 测试Metrics数据的序列化和反序列化
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "serialization.metrics.test")
  
  // 创建不同类型的指标
  let counter = Meter::create_counter(meter, "test.counter", "test.unit", "Test counter")
  let gauge = Meter::create_gauge(meter, "test.gauge", "test.unit", "Test gauge")
  let histogram = Meter::create_histogram(meter, "test.histogram", "test.unit", "Test histogram")
  
  // 记录指标数据
  Counter::add(counter, 100, Some([("counter.label", StringValue("test"))]))
  Gauge::set(gauge, 42.5, Some([("gauge.label", StringValue("test"))]))
  Histogram::record(histogram, 25.0, Some([("histogram.label", StringValue("test"))]))
  Histogram::record(histogram, 50.0, Some([("histogram.label", StringValue("test"))]))
  Histogram::record(histogram, 75.0, Some([("histogram.label", StringValue("test"))]))
  
  // 获取指标数据
  let counter_metrics = Meter::collect_metrics(counter)
  let gauge_metrics = Meter::collect_metrics(gauge)
  let histogram_metrics = Meter::collect_metrics(histogram)
  
  // 序列化指标数据
  let serialized_counter = Metrics::serialize(counter_metrics)
  let serialized_gauge = Metrics::serialize(gauge_metrics)
  let serialized_histogram = Metrics::serialize(histogram_metrics)
  
  // 反序列化指标数据
  let deserialized_counter = Metrics::deserialize(serialized_counter)
  let deserialized_gauge = Metrics::deserialize(serialized_gauge)
  let deserialized_histogram = Metrics::deserialize(serialized_histogram)
  
  // 验证反序列化的counter指标
  let counter_value = Counter::get_value(deserialized_counter)
  assert_eq(counter_value, 100)
  
  // 验证反序列化的gauge指标
  let gauge_value = Gauge::get_value(deserialized_gauge)
  assert_true(abs(gauge_value - 42.5) < 0.001) // 浮点数比较
  
  // 验证反序列化的histogram指标
  let histogram_stats = Histogram::get_statistics(deserialized_histogram)
  assert_eq(HistogramStats::get_count(histogram_stats), 3)
  assert_eq(HistogramStats::get_sum(histogram_stats), 150.0) // 25.0 + 50.0 + 75.0
  
  assert_true(true)
}

test "Log数据序列化反序列化" {
  // 测试Log数据的序列化和反序列化
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "serialization.log.test")
  
  // 创建日志记录
  let log_record = Logger::create_log_record(logger, LogLevel::INFO, "Test log message")
  LogRecord::set_attribute(log_record, "log.level", StringValue("INFO"))
  LogRecord::set_attribute(log_record, "log.logger", StringValue("serialization.log.test"))
  LogRecord::set_attribute(log_record, "user.id", StringValue("user-12345"))
  LogRecord::set_attribute(log_record, "request.id", StringValue("req-67890"))
  LogRecord::set_timestamp(log_record, Clock::now())
  
  // 序列化日志记录
  let serialized_log = LogRecord::serialize(log_record)
  
  // 反序列化日志记录
  let deserialized_log = LogRecord::deserialize(serialized_log)
  
  // 验证反序列化的日志记录
  assert_eq(LogRecord::get_level(deserialized_log), LogLevel::INFO)
  assert_eq(LogRecord::get_message(deserialized_log), "Test log message")
  
  let log_level_attr = LogRecord::get_attribute(deserialized_log, "log.level")
  let user_id_attr = LogRecord::get_attribute(deserialized_log, "user.id")
  
  match log_level_attr {
    Some(StringValue(value)) => assert_eq(value, "INFO")
    _ => assert_true(false)
  }
  
  match user_id_attr {
    Some(StringValue(value)) => assert_eq(value, "user-12345")
    _ => assert_true(false)
  }
  
  assert_true(true)
}

test "复杂数据结构序列化反序列化" {
  // 测试复杂数据结构的序列化和反序列化
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.complex.test")
  
  // 创建复杂的span层次结构
  let root_span = Tracer::start_span(tracer, "root.span")
  Span::set_attribute(root_span, "span.type", StringValue("root"))
  Span::set_attribute(root_span, "complex.nested.data", StringValue("root.data"))
  
  // 创建子span
  let child_span1 = Tracer::start_span_with_parent(tracer, "child.span.1", Some(Span::get_context(root_span)))
  Span::set_attribute(child_span1, "span.type", StringValue("child"))
  Span::set_attribute(child_span1, "complex.nested.data", StringValue("child1.data"))
  
  let child_span2 = Tracer::start_span_with_parent(tracer, "child.span.2", Some(Span::get_context(root_span)))
  Span::set_attribute(child_span2, "span.type", StringValue("child"))
  Span::set_attribute(child_span2, "complex.nested.data", StringValue("child2.data"))
  
  // 添加复杂事件和链接
  Span::add_event(root_span, "root.event", Some([
    ("event.type", StringValue("root")),
    ("event.data", StringValue("root.event.data")),
    ("event.timestamp", IntValue(Clock::now()))
  ]))
  
  Span::add_event(child_span1, "child1.event", Some([
    ("event.type", StringValue("child")),
    ("event.data", StringValue("child1.event.data")),
    ("event.timestamp", IntValue(Clock::now()))
  ]))
  
  // 添加链接
  let link_context = SpanContext::new("1234567890abcdef1234567890abcdef", "1234567890abcdef", true, "")
  Span::add_link(child_span2, link_context, Some([
    ("link.type", StringValue("external")),
    ("link.data", StringValue("external.link.data"))
  ]))
  
  // 序列化整个span层次结构
  let serialized_root = Span::serialize_to_json(root_span)
  let serialized_child1 = Span::serialize_to_json(child_span1)
  let serialized_child2 = Span::serialize_to_json(child_span2)
  
  // 反序列化span层次结构
  let deserialized_root = Span::deserialize_from_json(serialized_root)
  let deserialized_child1 = Span::deserialize_from_json(serialized_child1)
  let deserialized_child2 = Span::deserialize_from_json(serialized_child2)
  
  // 验证反序列化的复杂结构
  let root_span_type = Span::get_attribute(deserialized_root, "span.type")
  let child1_span_type = Span::get_attribute(deserialized_child1, "span.type")
  let child2_span_type = Span::get_attribute(deserialized_child2, "span.type")
  
  match root_span_type {
    Some(StringValue(value)) => assert_eq(value, "root")
    _ => assert_true(false)
  }
  
  match child1_span_type {
    Some(StringValue(value)) => assert_eq(value, "child")
    _ => assert_true(false)
  }
  
  match child2_span_type {
    Some(StringValue(value)) => assert_eq(value, "child")
    _ => assert_true(false)
  }
  
  // 验证事件
  let root_events = Span::get_events(deserialized_root)
  let child1_events = Span::get_events(deserialized_child1)
  
  assert_eq(root_events.length(), 1)
  assert_eq(child1_events.length(), 1)
  
  // 验证链接
  let child2_links = Span::get_links(deserialized_child2)
  assert_eq(child2_links.length(), 1)
  
  Span::end(child_span2)
  Span::end(child_span1)
  Span::end(root_span)
  
  assert_true(true)
}

test "序列化错误处理测试" {
  // 测试序列化过程中的错误处理
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "serialization.error.test")
  
  // 创建正常span
  let valid_span = Tracer::start_span(tracer, "valid.span")
  Span::set_attribute(valid_span, "valid.attr", StringValue("valid.value"))
  
  // 测试正常序列化
  let valid_serialized = Span::serialize_to_json(valid_span)
  assert_true(valid_serialized.length() > 0)
  
  // 测试从空数据反序列化
  let empty_deserialized = Span::deserialize_from_json("")
  assert_true(empty_deserialized == Span::empty())
  
  // 测试从无效JSON反序列化
  let invalid_deserialized = Span::deserialize_from_json("{invalid json}")
  assert_true(invalid_deserialized == Span::empty())
  
  // 测试从损坏的二进制数据反序列化
  let corrupted_binary = [0xFF, 0xFE, 0xFD, 0xFC]
  let corrupted_deserialized = Span::deserialize_from_binary(corrupted_binary)
  assert_true(corrupted_deserialized == Span::empty())
  
  Span::end(valid_span)
  assert_true(true)
}