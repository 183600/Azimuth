// Azimuth Serialization and Deserialization Tests
// 序列化和反序列化测试用例 - 测试系统的序列化和反序列化功能，包括各种数据格式和复杂结构的处理

// Test 1: JSON序列化和反序列化
test "json serialization and deserialization" {
  let json_serializer = JsonSerializer::new()
  
  // 测试基本数据类型序列化
  let string_value = StringValue("test_string")
  let serialized_string = JsonSerializer::serialize(json_serializer, string_value)
  assert_eq(serialized_string, "\"test_string\"")
  
  let int_value = IntValue(42)
  let serialized_int = JsonSerializer::serialize(json_serializer, int_value)
  assert_eq(serialized_int, "42")
  
  let float_value = FloatValue(3.14)
  let serialized_float = JsonSerializer::serialize(json_serializer, float_value)
  assert_eq(serialized_float, "3.14")
  
  let bool_value = BoolValue(true)
  let serialized_bool = JsonSerializer::serialize(json_serializer, bool_value)
  assert_eq(serialized_bool, "true")
  
  // 测试复合数据类型序列化
  let attributes = Attributes::new()
  Attributes::set(attributes, "name", StringValue("test_object"))
  Attributes::set(attributes, "age", IntValue(25))
  Attributes::set(attributes, "active", BoolValue(true))
  Attributes::set(attributes, "score", FloatValue(95.5))
  
  let serialized_attributes = JsonSerializer::serialize(json_serializer, attributes)
  assert_true(serialized_attributes.contains("\"name\":\"test_object\""))
  assert_true(serialized_attributes.contains("\"age\":25"))
  assert_true(serialized_attributes.contains("\"active\":true"))
  assert_true(serialized_attributes.contains("\"score\":95.5"))
  
  // 测试反序列化
  let deserialized_string = JsonSerializer::deserialize_string(json_serializer, serialized_string)
  match deserialized_string {
    StringValue(v) => assert_eq(v, "test_string")
    _ => assert_true(false)
  }
  
  let deserialized_int = JsonSerializer::deserialize_int(json_serializer, serialized_int)
  match deserialized_int {
    IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  let deserialized_float = JsonSerializer::deserialize_float(json_serializer, serialized_float)
  match deserialized_float {
    FloatValue(v) => assert_eq(v, 3.14)
    _ => assert_true(false)
  }
  
  let deserialized_bool = JsonSerializer::deserialize_bool(json_serializer, serialized_bool)
  match deserialized_bool {
    BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  let deserialized_attributes = JsonSerializer::deserialize_attributes(json_serializer, serialized_attributes)
  match deserialized_attributes {
    attrs => {
      let name_value = Attributes::get(attrs, "name")
      match name_value {
        Some(StringValue(v)) => assert_eq(v, "test_object")
        _ => assert_true(false)
      }
      
      let age_value = Attributes::get(attrs, "age")
      match age_value {
        Some(IntValue(v)) => assert_eq(v, 25)
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// Test 2: 跨度上下文序列化和反序列化
test "span context serialization and deserialization" {
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "test_state")
  
  // 序列化跨度上下文
  let serializer = SpanContextSerializer::new()
  let serialized_ctx = SpanContextSerializer::serialize(serializer, span_ctx)
  
  // 验证序列化结果
  assert_true(serialized_ctx.contains(trace_id))
  assert_true(serialized_ctx.contains(span_id))
  assert_true(serialized_ctx.contains("true"))
  assert_true(serialized_ctx.contains("test_state"))
  
  // 反序列化跨度上下文
  let deserializer = SpanContextDeserializer::new()
  let deserialized_ctx = SpanContextDeserializer::deserialize(deserializer, serialized_ctx)
  
  // 验证反序列化结果
  assert_true(SpanContext::is_valid(deserialized_ctx))
  assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
  assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
  assert_true(SpanContext::is_sampled(deserialized_ctx))
  
  // 测试无效上下文反序列化
  let invalid_serialized_ctx = "invalid_context_data"
  let invalid_deserialized_ctx = SpanContextDeserializer::deserialize(deserializer, invalid_serialized_ctx)
  assert_false(SpanContext::is_valid(invalid_deserialized_ctx))
  
  // 测试部分缺失字段的上下文反序列化
  let partial_serialized_ctx = "{\"trace_id\":\"" + trace_id + "\"}"
  let partial_deserialized_ctx = SpanContextDeserializer::deserialize(deserializer, partial_serialized_ctx)
  assert_false(SpanContext::is_valid(partial_deserialized_ctx))
}

// Test 3: 跨度数据序列化和反序列化
test "span data serialization and deserialization" {
  let trace_id = "trace_12345"
  let span_id = "span_67890"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("test_span", Internal, span_ctx)
  
  // 添加跨度和事件
  Span::add_event(span, "event1", Some([("key1", StringValue("value1"))]))
  Span::add_event(span, "event2", Some([("key2", IntValue(42))]))
  Span::set_status(span, Ok, Some("Operation completed"))
  
  // 序列化跨度数据
  let serializer = SpanSerializer::new()
  let serialized_span = SpanSerializer::serialize(serializer, span)
  
  // 验证序列化结果
  assert_true(serialized_span.contains("\"name\":\"test_span\""))
  assert_true(serialized_span.contains("\"kind\":\"Internal\""))
  assert_true(serialized_span.contains("\"trace_id\":\"trace_12345\""))
  assert_true(serialized_span.contains("\"span_id\":\"span_67890\""))
  assert_true(serialized_span.contains("\"status\":\"Ok\""))
  assert_true(serialized_span.contains("\"event1\""))
  assert_true(serialized_span.contains("\"event2\""))
  
  // 反序列化跨度数据
  let deserializer = SpanDeserializer::new()
  let deserialized_span = SpanDeserializer::deserialize(deserializer, serialized_span)
  
  // 验证反序列化结果
  assert_eq(Span::name(deserialized_span), "test_span")
  match Span::kind(deserialized_span) {
    Internal => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(Span::status(deserialized_span), Ok)
  
  let deserialized_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
  assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
  
  // 测试复杂嵌套结构的序列化
  let complex_attrs = Attributes::new()
  Attributes::set(complex_attrs, "nested_object", StringValue("{\"key\":\"value\",\"number\":123}"))
  Attributes::set(complex_attrs, "array_value", ArrayStringValue(["item1", "item2", "item3"]))
  
  let complex_span = Span::new("complex_span", Server, span_ctx)
  Span::add_event(complex_span, "complex_event", Some(complex_attrs))
  
  let serialized_complex_span = SpanSerializer::serialize(serializer, complex_span)
  let deserialized_complex_span = SpanDeserializer::deserialize(deserializer, serialized_complex_span)
  
  assert_eq(Span::name(deserialized_complex_span), "complex_span")
  match Span::kind(deserialized_complex_span) {
    Server => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 4: 指标数据序列化和反序列化
test "metrics data serialization and deserialization" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_test")
  
  // 创建各种类型的指标
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 100.0)
  
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 50.0)
  Histogram::record(histogram, 100.0)
  Histogram::record(histogram, 150.0)
  
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("value"))
  Gauge::record(gauge, 75.0)
  
  // 序列化指标数据
  let metrics_serializer = MetricsSerializer::new()
  let serialized_metrics = MetricsSerializer::serialize_all(metrics_serializer, [counter, histogram, gauge])
  
  // 验证序列化结果
  assert_true(serialized_metrics.contains("\"name\":\"test_counter\""))
  assert_true(serialized_metrics.contains("\"name\":\"test_histogram\""))
  assert_true(serialized_metrics.contains("\"name\":\"test_gauge\""))
  assert_true(serialized_metrics.contains("\"unit\":\"count\""))
  assert_true(serialized_metrics.contains("\"unit\":\"ms\""))
  assert_true(serialized_metrics.contains("\"unit\":\"value\""))
  
  // 反序列化指标数据
  let metrics_deserializer = MetricsDeserializer::new()
  let deserialized_metrics = MetricsDeserializer::deserialize_all(metrics_deserializer, serialized_metrics)
  
  // 验证反序列化结果
  assert_eq(deserialized_metrics.length(), 3)
  
  for metric in deserialized_metrics {
    match Instrument::name(metric) {
      "test_counter" => {
        match Instrument::as_counter(metric) {
          Some(counter_metric) => {
            let value = Counter::get_value(counter_metric)
            assert_eq(value, 100.0)
          }
          None => assert_true(false)
        }
      }
      "test_histogram" => {
        match Instrument::as_histogram(metric) {
          Some(histogram_metric) => {
            let count = Histogram::get_count(histogram_metric)
            let sum = Histogram::get_sum(histogram_metric)
            assert_eq(count, 3)
            assert_eq(sum, 300.0)
          }
          None => assert_true(false)
        }
      }
      "test_gauge" => {
        match Instrument::as_gauge(metric) {
          Some(gauge_metric) => {
            let value = Gauge::get_value(gauge_metric)
            assert_eq(value, 75.0)
          }
          None => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 测试带属性的指标序列化
  let attrs = Attributes::new()
  Attributes::set(attrs, "service", StringValue("test_service"))
  Attributes::set(attrs, "version", StringValue("1.0.0"))
  
  Counter::add(counter, 50.0, Some(attrs))
  let serialized_metrics_with_attrs = MetricsSerializer::serialize_all(metrics_serializer, [counter])
  
  assert_true(serialized_metrics_with_attrs.contains("\"service\":\"test_service\""))
  assert_true(serialized_metrics_with_attrs.contains("\"version\":\"1.0.0\""))
}

// Test 5: 日志记录序列化和反序列化
test "log record serialization and deserialization" {
  // 创建日志记录
  let log_record = LogRecord::new(Info, "Test log message")
  LogRecord::add_attribute(log_record, "service_name", StringValue("test_service"))
  LogRecord::add_attribute(log_record, "request_id", StringValue("req_12345"))
  LogRecord::add_attribute(log_record, "user_id", IntValue(67890))
  LogRecord::set_timestamp(log_record, 1609459200000L)
  
  // 序列化日志记录
  let log_serializer = LogRecordSerializer::new()
  let serialized_log = LogRecordSerializer::serialize(log_serializer, log_record)
  
  // 验证序列化结果
  assert_true(serialized_log.contains("\"severity_number\":\"Info\""))
  assert_true(serialized_log.contains("\"body\":\"Test log message\""))
  assert_true(serialized_log.contains("\"service_name\":\"test_service\""))
  assert_true(serialized_log.contains("\"request_id\":\"req_12345\""))
  assert_true(serialized_log.contains("\"user_id\":67890"))
  assert_true(serialized_log.contains("\"timestamp\":1609459200000"))
  
  // 反序列化日志记录
  let log_deserializer = LogRecordDeserializer::new()
  let deserialized_log = LogRecordDeserializer::deserialize(log_deserializer, serialized_log)
  
  // 验证反序列化结果
  assert_eq(LogRecord::severity_number(deserialized_log), Info)
  match LogRecord::body(deserialized_log) {
    Some(body) => assert_eq(body, "Test log message")
    None => assert_true(false)
  }
  
  let service_name = LogRecord::get_attribute(deserialized_log, "service_name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "test_service")
    _ => assert_true(false)
  }
  
  let request_id = LogRecord::get_attribute(deserialized_log, "request_id")
  match request_id {
    Some(StringValue(v)) => assert_eq(v, "req_12345")
    _ => assert_true(false)
  }
  
  let user_id = LogRecord::get_attribute(deserialized_log, "user_id")
  match user_id {
    Some(IntValue(v)) => assert_eq(v, 67890)
    _ => assert_true(false)
  }
  
  assert_eq(LogRecord::get_timestamp(deserialized_log), 1609459200000L)
  
  // 测试批量日志序列化
  let log_records = [
    LogRecord::new(Debug, "Debug message"),
    LogRecord::new(Warn, "Warning message"),
    LogRecord::new(Error, "Error message")
  ]
  
  for record in log_records {
    LogRecord::set_timestamp(record, Time::now())
  }
  
  let serialized_batch = LogRecordSerializer::serialize_batch(log_serializer, log_records)
  let deserialized_batch = LogRecordDeserializer::deserialize_batch(log_deserializer, serialized_batch)
  
  assert_eq(deserialized_batch.length(), 3)
  assert_eq(LogRecord::severity_number(deserialized_batch[0]), Debug)
  assert_eq(LogRecord::severity_number(deserialized_batch[1]), Warn)
  assert_eq(LogRecord::severity_number(deserialized_batch[2]), Error)
}

// Test 6: 资源数据序列化和反序列化
test "resource data serialization and deserialization" {
  // 创建资源
  let resource = Resource::new()
  
  // 添加资源属性
  let resource_attrs = [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-abc123")),
    ("host.name", StringValue("test-host")),
    ("host.arch", StringValue("x86_64")),
    ("os.type", StringValue("linux")),
    ("deployment.environment", StringValue("production"))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, resource_attrs)
  
  // 序列化资源数据
  let resource_serializer = ResourceSerializer::new()
  let serialized_resource = ResourceSerializer::serialize(resource_serializer, resource_with_attrs)
  
  // 验证序列化结果
  assert_true(serialized_resource.contains("\"service.name\":\"test_service\""))
  assert_true(serialized_resource.contains("\"service.version\":\"1.2.3\""))
  assert_true(serialized_resource.contains("\"service.instance.id\":\"instance-abc123\""))
  assert_true(serialized_resource.contains("\"host.name\":\"test-host\""))
  assert_true(serialized_resource.contains("\"host.arch\":\"x86_64\""))
  assert_true(serialized_resource.contains("\"os.type\":\"linux\""))
  assert_true(serialized_resource.contains("\"deployment.environment\":\"production\""))
  
  // 反序列化资源数据
  let resource_deserializer = ResourceDeserializer::new()
  let deserialized_resource = ResourceDeserializer::deserialize(resource_deserializer, serialized_resource)
  
  // 验证反序列化结果
  let service_name = Resource::get_attribute(deserialized_resource, "service.name")
  match service_name {
    Some(StringValue(v)) => assert_eq(v, "test_service")
    _ => assert_true(false)
  }
  
  let service_version = Resource::get_attribute(deserialized_resource, "service.version")
  match service_version {
    Some(StringValue(v)) => assert_eq(v, "1.2.3")
    _ => assert_true(false)
  }
  
  let service_instance_id = Resource::get_attribute(deserialized_resource, "service.instance.id")
  match service_instance_id {
    Some(StringValue(v)) => assert_eq(v, "instance-abc123")
    _ => assert_true(false)
  }
  
  let host_name = Resource::get_attribute(deserialized_resource, "host.name")
  match host_name {
    Some(StringValue(v)) => assert_eq(v, "test-host")
    _ => assert_true(false)
  }
  
  let host_arch = Resource::get_attribute(deserialized_resource, "host.arch")
  match host_arch {
    Some(StringValue(v)) => assert_eq(v, "x86_64")
    _ => assert_true(false)
  }
  
  let os_type = Resource::get_attribute(deserialized_resource, "os.type")
  match os_type {
    Some(StringValue(v)) => assert_eq(v, "linux")
    _ => assert_true(false)
  }
  
  let deployment_env = Resource::get_attribute(deserialized_resource, "deployment.environment")
  match deployment_env {
    Some(StringValue(v)) => assert_eq(v, "production")
    _ => assert_true(false)
  }
  
  // 测试资源合并序列化
  let override_resource = Resource::with_attributes(Resource::new(), [
    ("service.version", StringValue("2.0.0")),
    ("new.attribute", StringValue("new_value"))
  ])
  
  let merged_resource = Resource::merge(resource_with_attrs, override_resource)
  let serialized_merged = ResourceSerializer::serialize(resource_serializer, merged_resource)
  
  assert_true(serialized_merged.contains("\"service.version\":\"2.0.0\"")) // 被覆盖
  assert_true(serialized_merged.contains("\"new.attribute\":\"new_value\"")) // 新增
}

// Test 7: 二进制序列化和反序列化
test "binary serialization and deserialization" {
  let binary_serializer = BinarySerializer::new()
  let binary_deserializer = BinaryDeserializer::new()
  
  // 测试基本数据类型的二进制序列化
  let string_value = StringValue("binary_test")
  let binary_string = BinarySerializer::serialize_string(binary_serializer, string_value)
  let deserialized_string = BinaryDeserializer::deserialize_string(binary_deserializer, binary_string)
  assert_eq(deserialized_string, string_value)
  
  let int_value = IntValue(12345)
  let binary_int = BinarySerializer::serialize_int(binary_serializer, int_value)
  let deserialized_int = BinaryDeserializer::deserialize_int(binary_deserializer, binary_int)
  assert_eq(deserialized_int, int_value)
  
  let float_value = FloatValue(3.14159)
  let binary_float = BinarySerializer::serialize_float(binary_serializer, float_value)
  let deserialized_float = BinaryDeserializer::deserialize_float(binary_deserializer, binary_float)
  assert_eq(deserialized_float, float_value)
  
  let bool_value = BoolValue(true)
  let binary_bool = BinarySerializer::serialize_bool(binary_serializer, bool_value)
  let deserialized_bool = BinaryDeserializer::deserialize_bool(binary_deserializer, binary_bool)
  assert_eq(deserialized_bool, bool_value)
  
  // 测试数组的二进制序列化
  let string_array = ArrayStringValue(["item1", "item2", "item3"])
  let binary_string_array = BinarySerializer::serialize_string_array(binary_serializer, string_array)
  let deserialized_string_array = BinaryDeserializer::deserialize_string_array(binary_deserializer, binary_string_array)
  assert_eq(deserialized_string_array.length(), string_array.length())
  for i in 0..=string_array.length() - 1 {
    assert_eq(deserialized_string_array[i], string_array[i])
  }
  
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  let binary_int_array = BinarySerializer::serialize_int_array(binary_serializer, int_array)
  let deserialized_int_array = BinaryDeserializer::deserialize_int_array(binary_deserializer, binary_int_array)
  assert_eq(deserialized_int_array.length(), int_array.length())
  for i in 0..=int_array.length() - 1 {
    assert_eq(deserialized_int_array[i], int_array[i])
  }
  
  // 测试复杂对象的二进制序列化
  let trace_id = "trace_binary_test"
  let span_id = "span_binary_test"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("binary_test_span", Client, span_ctx)
  
  Span::add_event(span, "binary_event", Some([
    ("binary_key", StringValue("binary_value")),
    ("binary_number", IntValue(42))
  ]))
  
  let binary_span = BinarySerializer::serialize_span(binary_serializer, span)
  let deserialized_span = BinaryDeserializer::deserialize_span(binary_deserializer, binary_span)
  
  assert_eq(Span::name(deserialized_span), "binary_test_span")
  match Span::kind(deserialized_span) {
    Client => assert_true(true)
    _ => assert_true(false)
  }
  
  let deserialized_ctx = Span::span_context(deserialized_span)
  assert_eq(SpanContext::trace_id(deserialized_ctx), trace_id)
  assert_eq(SpanContext::span_id(deserialized_ctx), span_id)
  
  // 验证二进制序列化的大小效率
  let json_serializer = JsonSerializer::new()
  let json_span = JsonSerializer::serialize(json_serializer, span)
  
  assert_true(binary_span.length() < json_span.length()) // 二进制应该更紧凑
}

// Test 8: 压缩序列化和反序列化
test "compressed serialization and deserialization" {
  let compressed_serializer = CompressedSerializer::new(CompressionAlgorithm::Gzip)
  let compressed_deserializer = CompressedDeserializer::new()
  
  // 创建大量数据
  let large_attrs = Attributes::new()
  for i in 1..=1000 {
    Attributes::set(large_attrs, "key_" + i.to_string(), StringValue("value_" + i.to_string()))
  }
  
  let trace_id = "trace_compressed_test"
  let span_id = "span_compressed_test"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "")
  let span = Span::new("compressed_test_span", Server, span_ctx)
  
  Span::add_event(span, "large_event", Some(large_attrs))
  
  // 普通序列化
  let normal_serializer = JsonSerializer::new()
  let normal_serialized = JsonSerializer::serialize(normal_serializer, span)
  
  // 压缩序列化
  let compressed_serialized = CompressedSerializer::serialize(compressed_serializer, span)
  
  // 验证压缩效果
  assert_true(compressed_serialized.length() < normal_serialized.length())
  let compression_ratio = compressed_serialized.length().to_float() / normal_serialized.length().to_float()
  assert_true(compression_ratio < 0.8) // 至少20%的压缩率
  
  // 解压缩和反序列化
  let decompressed_span = CompressedDeserializer::deserialize(compressed_deserializer, compressed_serialized)
  
  // 验证解压缩后的数据完整性
  assert_eq(Span::name(decompressed_span), "compressed_test_span")
  match Span::kind(decompressed_span) {
    Server => assert_true(true)
    _ => assert_true(false)
  }
  
  let decompressed_ctx = Span::span_context(decompressed_span)
  assert_eq(SpanContext::trace_id(decompressed_ctx), trace_id)
  assert_eq(SpanContext::span_id(decompressed_ctx), span_id)
  
  // 验证大量属性的正确性
  let events = Span::get_events(decompressed_span)
  assert_eq(events.length(), 1)
  
  let event_attrs = events[0].attributes
  for i in 1..=1000 {
    let key = "key_" + i.to_string()
    let value = Attributes::get(event_attrs, key)
    match value {
      Some(StringValue(v)) => assert_eq(v, "value_" + i.to_string())
      _ => assert_true(false)
    }
  }
  
  // 测试不同压缩算法
  let lz4_serializer = CompressedSerializer::new(CompressionAlgorithm::LZ4)
  let lz4_serialized = CompressedSerializer::serialize(lz4_serializer, span)
  
  let deflate_serializer = CompressedSerializer::new(CompressionAlgorithm::Deflate)
  let deflate_serialized = CompressedSerializer::serialize(deflate_serializer, span)
  
  // 比较不同算法的压缩效果
  assert_true(lz4_serialized.length() < normal_serialized.length())
  assert_true(deflate_serialized.length() < normal_serialized.length())
  
  // 验证不同算法的解压缩结果
  let lz4_deserialized = CompressedDeserializer::deserialize(compressed_deserializer, lz4_serialized)
  let deflate_deserialized = CompressedDeserializer::deserialize(compressed_deserializer, deflate_serialized)
  
  assert_eq(Span::name(lz4_deserialized), "compressed_test_span")
  assert_eq(Span::name(deflate_deserialized), "compressed_test_span")
  
  // 测试压缩性能统计
  let compression_stats = CompressedSerializer::get_statistics(compressed_serializer)
  assert_true(compression_stats.total_compressions > 0)
  assert_true(compression_stats.average_compression_ratio > 0.0)
  assert_true(compression_stats.average_compression_ratio < 1.0)
}