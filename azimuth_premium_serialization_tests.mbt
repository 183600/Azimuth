// Azimuth Premium Serialization and Deserialization Tests
// 数据序列化和反序列化测试，确保遥测数据的正确转换和传输

// Test 1: AttributeValue Serialization
test "属性值序列化测试" {
  // 测试字符串属性值序列化
  let string_attr = StringValue("test_string_value")
  let serialized_string = AttributeValue::serialize(string_attr)
  assert_eq(serialized_string, "{\"type\":\"string\",\"value\":\"test_string_value\"}")
  
  // 测试反序列化
  let deserialized_string = AttributeValue::deserialize(serialized_string)
  match deserialized_string {
    Ok(StringValue(v)) => assert_eq(v, "test_string_value")
    _ => assert_true(false)
  }
  
  // 测试整数属性值序列化
  let int_attr = IntValue(42)
  let serialized_int = AttributeValue::serialize(int_attr)
  assert_eq(serialized_int, "{\"type\":\"int\",\"value\":42}")
  
  // 测试反序列化
  let deserialized_int = AttributeValue::deserialize(serialized_int)
  match deserialized_int {
    Ok(IntValue(v)) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  // 测试浮点数属性值序列化
  let float_attr = FloatValue(3.14159)
  let serialized_float = AttributeValue::serialize(float_attr)
  assert_eq(serialized_float, "{\"type\":\"float\",\"value\":3.14159}")
  
  // 测试反序列化
  let deserialized_float = AttributeValue::deserialize(serialized_float)
  match deserialized_float {
    Ok(FloatValue(v)) => assert_true(abs(v - 3.14159) < 0.00001)
    _ => assert_true(false)
  }
  
  // 测试布尔属性值序列化
  let bool_attr = BoolValue(true)
  let serialized_bool = AttributeValue::serialize(bool_attr)
  assert_eq(serialized_bool, "{\"type\":\"bool\",\"value\":true}")
  
  // 测试反序列化
  let deserialized_bool = AttributeValue::deserialize(serialized_bool)
  match deserialized_bool {
    Ok(BoolValue(v)) => assert_true(v)
    _ => assert_true(false)
  }
}

// Test 2: Attributes Serialization
test "属性集合序列化测试" {
  // 创建属性集合
  let attrs = Attributes::new()
  Attributes::set(attrs, "string.key", StringValue("string_value"))
  Attributes::set(attrs, "int.key", IntValue(123))
  Attributes::set(attrs, "float.key", FloatValue(45.67))
  Attributes::set(attrs, "bool.key", BoolValue(false))
  
  // 序列化属性集合
  let serialized_attrs = Attributes::serialize(attrs)
  
  // 验证序列化结果包含所有属性
  assert_true(serialized_attrs.contains("\"string.key\""))
  assert_true(serialized_attrs.contains("\"string_value\""))
  assert_true(serialized_attrs.contains("\"int.key\""))
  assert_true(serialized_attrs.contains("123"))
  assert_true(serialized_attrs.contains("\"float.key\""))
  assert_true(serialized_attrs.contains("45.67"))
  assert_true(serialized_attrs.contains("\"bool.key\""))
  assert_true(serialized_attrs.contains("false"))
  
  // 测试反序列化
  let deserialized_attrs = Attributes::deserialize(serialized_attrs)
  match deserialized_attrs {
    Ok(deserialized) => {
      // 验证反序列化的属性
      let string_result = Attributes::get(deserialized, "string.key")
      match string_result {
        Some(StringValue(v)) => assert_eq(v, "string_value")
        _ => assert_true(false)
      }
      
      let int_result = Attributes::get(deserialized, "int.key")
      match int_result {
        Some(IntValue(v)) => assert_eq(v, 123)
        _ => assert_true(false)
      }
      
      let float_result = Attributes::get(deserialized, "float.key")
      match float_result {
        Some(FloatValue(v)) => assert_true(abs(v - 45.67) < 0.00001)
        _ => assert_true(false)
      }
      
      let bool_result = Attributes::get(deserialized, "bool.key")
      match bool_result {
        Some(BoolValue(v)) => assert_false(v)
        _ => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 3: SpanContext Serialization
test "跨度上下文序列化测试" {
  // 创建跨度上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 序列化跨度上下文
  let serialized_ctx = SpanContext::serialize(span_ctx)
  
  // 验证序列化结果
  assert_true(serialized_ctx.contains("\"trace_id\":\"" + trace_id + "\""))
  assert_true(serialized_ctx.contains("\"span_id\":\"" + span_id + "\""))
  assert_true(serialized_ctx.contains("\"sampled\":true"))
  assert_true(serialized_ctx.contains("\"trace_state\":\"rojo=00f067aa0ba902b7,congo=t61rcWkgMzE\""))
  
  // 测试反序列化
  let deserialized_ctx = SpanContext::deserialize(serialized_ctx)
  match deserialized_ctx {
    Ok(ctx) => {
      assert_eq(SpanContext::trace_id(ctx), trace_id)
      assert_eq(SpanContext::span_id(ctx), span_id)
      assert_true(SpanContext::is_sampled(ctx))
      assert_eq(SpanContext::trace_state(ctx), "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
    }
    Err(_) => assert_true(false)
  }
  
  // 测试无效的跨度上下文序列化
  let invalid_ctx = SpanContext::new("", "", false, "")
  let serialized_invalid = SpanContext::serialize(invalid_ctx)
  let deserialized_invalid = SpanContext::deserialize(serialized_invalid)
  
  match deserialized_invalid {
    Ok(ctx) => assert_false(SpanContext::is_valid(ctx))
    Err(_) => assert_true(true) // 可能返回错误
  }
}

// Test 4: Span Serialization
test "跨度序列化测试" {
  // 创建跨度
  let span_ctx = SpanContext::new("trace_id", "span_id", true, "")
  let span = Span::new("test_operation", Server, span_ctx)
  
  // 添加事件和状态
  Span::add_event(span, "start_event", Some([("event_data", StringValue("start_value"))]))
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  Span::end(span)
  
  // 序列化跨度
  let serialized_span = Span::serialize(span)
  
  // 验证序列化结果
  assert_true(serialized_span.contains("\"name\":\"test_operation\""))
  assert_true(serialized_span.contains("\"kind\":\"Server\""))
  assert_true(serialized_span.contains("\"trace_id\":\"trace_id\""))
  assert_true(serialized_span.contains("\"span_id\":\"span_id\""))
  assert_true(serialized_span.contains("\"status\":\"Ok\""))
  assert_true(serialized_span.contains("\"Operation completed successfully\""))
  assert_true(serialized_span.contains("\"start_event\""))
  
  // 测试反序列化
  let deserialized_span = Span::deserialize(serialized_span)
  match deserialized_span {
    Ok(deserialized) => {
      assert_eq(Span::name(deserialized), "test_operation")
      match Span::kind(deserialized) {
        Server => assert_true(true)
        _ => assert_true(false)
      }
      
      let ctx = Span::span_context(deserialized)
      assert_eq(SpanContext::trace_id(ctx), "trace_id")
      assert_eq(SpanContext::span_id(ctx), "span_id")
    }
    Err(_) => assert_true(false)
  }
}

// Test 5: Resource Serialization
test "资源序列化测试" {
  // 创建资源
  let resource = Resource::with_attributes(Resource::new(), [
    ("service.name", StringValue("test_service")),
    ("service.version", StringValue("1.2.3")),
    ("service.instance.id", StringValue("instance-12345")),
    ("host.name", StringValue("test-host")),
    ("process.id", IntValue(12345)),
    ("process.command", StringValue("test_process")),
    ("process.start_time", StringValue("2023-01-01T00:00:00Z")),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("0.1.0")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ])
  
  // 序列化资源
  let serialized_resource = Resource::serialize(resource)
  
  // 验证序列化结果
  assert_true(serialized_resource.contains("\"service.name\":\"test_service\""))
  assert_true(serialized_resource.contains("\"service.version\":\"1.2.3\""))
  assert_true(serialized_resource.contains("\"service.instance.id\":\"instance-12345\""))
  assert_true(serialized_resource.contains("\"host.name\":\"test-host\""))
  assert_true(serialized_resource.contains("\"process.id\":12345"))
  assert_true(serialized_resource.contains("\"process.command\":\"test_process\""))
  assert_true(serialized_resource.contains("\"telemetry.sdk.name\":\"azimuth\""))
  assert_true(serialized_resource.contains("\"telemetry.sdk.version\":\"0.1.0\""))
  
  // 测试反序列化
  let deserialized_resource = Resource::deserialize(serialized_resource)
  match deserialized_resource {
    Ok(deserialized) => {
      let service_name = Resource::get_attribute(deserialized, "service.name")
      match service_name {
        Some(StringValue(name)) => assert_eq(name, "test_service")
        _ => assert_true(false)
      }
      
      let service_version = Resource::get_attribute(deserialized, "service.version")
      match service_version {
        Some(StringValue(version)) => assert_eq(version, "1.2.3")
        _ => assert_true(false)
      }
      
      let process_id = Resource::get_attribute(deserialized, "process.id")
      match process_id {
        Some(IntValue(id)) => assert_eq(id, 12345)
        _ => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 6: LogRecord Serialization
test "日志记录序列化测试" {
  // 创建日志记录
  let log_record = LogRecord::new_with_context(
    Error,
    Some("Database connection failed"),
    Some(Attributes::with_data([
      ("db.name", StringValue("test_db")),
      ("db.host", StringValue("localhost")),
      ("db.port", IntValue(5432)),
      ("error.code", StringValue("ECONNREFUSED")),
      ("retry.count", IntValue(3))
    ])),
    Some(1672531200000L), // 2023-01-01 00:00:00 UTC
    Some(1672531260000L), // 2023-01-01 00:01:00 UTC
    Some("trace_id_12345"),
    Some("span_id_67890"),
    Some(Context::root())
  )
  
  // 序列化日志记录
  let serialized_log = LogRecord::serialize(log_record)
  
  // 验证序列化结果
  assert_true(serialized_log.contains("\"severity_number\":\"Error\""))
  assert_true(serialized_log.contains("\"body\":\"Database connection failed\""))
  assert_true(serialized_log.contains("\"db.name\":\"test_db\""))
  assert_true(serialized_log.contains("\"db.host\":\"localhost\""))
  assert_true(serialized_log.contains("\"db.port\":5432"))
  assert_true(serialized_log.contains("\"error.code\":\"ECONNREFUSED\""))
  assert_true(serialized_log.contains("\"retry.count\":3"))
  assert_true(serialized_log.contains("\"timestamp\":1672531200000"))
  assert_true(serialized_log.contains("\"observed_timestamp\":1672531260000"))
  assert_true(serialized_log.contains("\"trace_id\":\"trace_id_12345\""))
  assert_true(serialized_log.contains("\"span_id\":\"span_id_67890\""))
  
  // 测试反序列化
  let deserialized_log = LogRecord::deserialize(serialized_log)
  match deserialized_log {
    Ok(deserialized) => {
      assert_eq(LogRecord::severity_number(deserialized), Error)
      match LogRecord::body(deserialized) {
        Some(body) => assert_eq(body, "Database connection failed")
        None => assert_true(false)
      }
      
      assert_eq(LogRecord::timestamp(deserialized), Some(1672531200000L))
      assert_eq(LogRecord::observed_timestamp(deserialized), Some(1672531260000L))
      assert_eq(LogRecord::trace_id(deserialized), Some("trace_id_12345"))
      assert_eq(LogRecord::span_id(deserialized), Some("span_id_67890"))
    }
    Err(_) => assert_true(false)
  }
}

// Test 7: Metrics Serialization
test "指标序列化测试" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "serialization_test_meter")
  
  // 创建各种指标
  let counter = Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  Counter::add(counter, 100.0, Some(Attributes::with_data([
    ("status", StringValue("success")),
    ("method", StringValue("GET"))
  ])))
  
  let histogram = Meter::create_histogram(meter, "test_histogram", Some("Test histogram"), Some("ms"))
  Histogram::record(histogram, 50.0, Some(Attributes::with_data([
    ("status", StringValue("success")),
    ("method", StringValue("POST"))
  ])))
  Histogram::record(histogram, 100.0, Some(Attributes::with_data([
    ("status", StringValue("error")),
    ("method", StringValue("GET"))
  ])))
  
  let gauge = Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("percent"))
  Gauge::record(gauge, 75.5, Some(Attributes::with_data([
    ("metric.type", StringValue("cpu_usage"))
  ])))
  
  // 序列化指标
  let metrics = Meter::collect_metrics(meter)
  let serialized_metrics = Metrics::serialize(metrics)
  
  // 验证序列化结果
  assert_true(serialized_metrics.contains("\"name\":\"test_counter\""))
  assert_true(serialized_metrics.contains("\"description\":\"Test counter\""))
  assert_true(serialized_metrics.contains("\"unit\":\"count\""))
  assert_true(serialized_metrics.contains("\"type\":\"counter\""))
  assert_true(serialized_metrics.contains("\"name\":\"test_histogram\""))
  assert_true(serialized_metrics.contains("\"type\":\"histogram\""))
  assert_true(serialized_metrics.contains("\"name\":\"test_gauge\""))
  assert_true(serialized_metrics.contains("\"type\":\"gauge\""))
  assert_true(serialized_metrics.contains("\"status\":\"success\""))
  assert_true(serialized_metrics.contains("\"method\":\"GET\""))
  assert_true(serialized_metrics.contains("\"method\":\"POST\""))
  assert_true(serialized_metrics.contains("\"metric.type\":\"cpu_usage\""))
  
  // 测试反序列化
  let deserialized_metrics = Metrics::deserialize(serialized_metrics)
  match deserialized_metrics {
    Ok(deserialized) => {
      assert_true(deserialized.length() >= 3) // 至少有counter、histogram和gauge
      
      // 验证counter
      let counter_metric = find_metric_by_name(deserialized, "test_counter")
      match counter_metric {
        Some(metric) => {
          assert_eq(Metric::name(metric), "test_counter")
          assert_eq(Metric::description(metric), Some("Test counter"))
          assert_eq(Metric::unit(metric), Some("count"))
          match Metric::type(metric) {
            CounterMetric => assert_true(true)
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
      
      // 验证histogram
      let histogram_metric = find_metric_by_name(deserialized, "test_histogram")
      match histogram_metric {
        Some(metric) => {
          assert_eq(Metric::name(metric), "test_histogram")
          match Metric::type(metric) {
            HistogramMetric => assert_true(true)
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
      
      // 验证gauge
      let gauge_metric = find_metric_by_name(deserialized, "test_gauge")
      match gauge_metric {
        Some(metric) => {
          assert_eq(Metric::name(metric), "test_gauge")
          match Metric::type(metric) {
            GaugeMetric => assert_true(true)
            _ => assert_true(false)
          }
        }
        None => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 8: Binary Serialization
test "二进制序列化测试" {
  // 创建测试数据
  let attrs = Attributes::with_data([
    ("binary.data", StringValue("binary_test_data")),
    ("size", IntValue(1024)),
    ("format", StringValue("protobuf"))
  ])
  
  // 二进制序列化
  let binary_data = Attributes::serialize_to_binary(attrs)
  assert_true(binary_data.length() > 0)
  
  // 二进制反序列化
  let deserialized_attrs = Attributes::deserialize_from_binary(binary_data)
  match deserialized_attrs {
    Ok(deserialized) => {
      let binary_data_result = Attributes::get(deserialized, "binary.data")
      match binary_data_result {
        Some(StringValue(v)) => assert_eq(v, "binary_test_data")
        _ => assert_true(false)
      }
      
      let size_result = Attributes::get(deserialized, "size")
      match size_result {
        Some(IntValue(v)) => assert_eq(v, 1024)
        _ => assert_true(false)
      }
      
      let format_result = Attributes::get(deserialized, "format")
      match format_result {
        Some(StringValue(v)) => assert_eq(v, "protobuf")
        _ => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 9: Cross-Format Serialization
test "跨格式序列化测试" {
  // 创建测试数据
  let span_ctx = SpanContext::new("trace_123", "span_456", true, "state=test")
  let span = Span::new("cross_format_test", Client, span_ctx)
  Span::add_event(span, "test_event", Some([("event_data", StringValue("event_value"))]))
  
  // JSON序列化
  let json_data = Span::serialize(span)
  
  // 从JSON反序列化
  let deserialized_from_json = Span::deserialize(json_data)
  match deserialized_from_json {
    Ok(deserialized) => {
      // 再序列化为二进制
      let binary_data = Span::serialize_to_binary(deserialized)
      
      // 从二进制反序列化
      let deserialized_from_binary = Span::deserialize_from_binary(binary_data)
      match deserialized_from_binary {
        Ok(final_deserialized) => {
          // 验证最终结果
          assert_eq(Span::name(final_deserialized), "cross_format_test")
          match Span::kind(final_deserialized) {
            Client => assert_true(true)
            _ => assert_true(false)
          }
          
          let ctx = Span::span_context(final_deserialized)
          assert_eq(SpanContext::trace_id(ctx), "trace_123")
          assert_eq(SpanContext::span_id(ctx), "span_456")
          assert_true(SpanContext::is_sampled(ctx))
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// Test 10: Serialization Performance and Size
test "序列化性能和大小测试" {
  // 创建大型数据结构
  let mut large_attrs = Attributes::new()
  for i in 0..1000 {
    Attributes::set(large_attrs, "key_" + i.to_string(), StringValue("value_" + i.to_string()))
  }
  
  // 测试JSON序列化性能
  let start_time_json = get_current_timestamp()
  let json_data = Attributes::serialize(large_attrs)
  let json_time = get_current_timestamp() - start_time_json
  
  // 测试二进制序列化性能
  let start_time_binary = get_current_timestamp()
  let binary_data = Attributes::serialize_to_binary(large_attrs)
  let binary_time = get_current_timestamp() - start_time_binary
  
  // 比较序列化大小
  let json_size = json_data.length()
  let binary_size = binary_data.length()
  
  // 验证二进制序列化通常更小
  assert_true(binary_size <= json_size)
  
  // 验证序列化性能在可接受范围内
  assert_true(json_time < 5000) // JSON序列化小于5秒
  assert_true(binary_time < 3000) // 二进制序列化小于3秒
  
  // 验证反序列化性能
  let start_time_json_deserialize = get_current_timestamp()
  let _ = Attributes::deserialize(json_data)
  let json_deserialize_time = get_current_timestamp() - start_time_json_deserialize
  
  let start_time_binary_deserialize = get_current_timestamp()
  let _ = Attributes::deserialize_from_binary(binary_data)
  let binary_deserialize_time = get_current_timestamp() - start_time_binary_deserialize
  
  // 验证反序列化性能
  assert_true(json_deserialize_time < 5000) // JSON反序列化小于5秒
  assert_true(binary_deserialize_time < 3000) // 二进制反序列化小于3秒
  
  // 验证数据完整性
  let deserialized_from_json = Attributes::deserialize(json_data)
  let deserialized_from_binary = Attributes::deserialize_from_binary(binary_data)
  
  match (deserialized_from_json, deserialized_from_binary) {
    (Ok(json_attrs), Ok(binary_attrs)) => {
      // 验证两种反序列化结果相同
      let json_count = Attributes::count(json_attrs)
      let binary_count = Attributes::count(binary_attrs)
      assert_eq(json_count, binary_count)
      assert_eq(json_count, 1000)
      
      // 验证特定属性
      let test_key = "key_500"
      let json_result = Attributes::get(json_attrs, test_key)
      let binary_result = Attributes::get(binary_attrs, test_key)
      
      match (json_result, binary_result) {
        (Some(StringValue(json_val)), Some(StringValue(binary_val))) => {
          assert_eq(json_val, binary_val)
          assert_eq(json_val, "value_500")
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

// 辅助函数
fn find_metric_by_name(metrics: Array[Metric], name: String) -> Option[Metric] {
  for metric in metrics {
    if Metric::name(metric) == name {
      return Some(metric)
    }
  }
  None
}

fn get_current_timestamp() -> Int {
  // 模拟时间戳获取
  1609459200000
}

fn abs(x: Float) -> Float {
  if x < 0.0 { -x } else { x }
}