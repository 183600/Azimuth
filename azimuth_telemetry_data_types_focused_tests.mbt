// Azimuth 遥测数据类型专项测试
// 专注于测试遥测系统的核心数据类型和操作

// 测试1: AttributeValue 类型操作
test "attribute_value_type_operations" {
  let string_attr = AttributeValue::StringValue("test-value")
  let int_attr = AttributeValue::IntValue(42)
  let float_attr = AttributeValue::FloatValue(3.14)
  let bool_attr = AttributeValue::BoolValue(true)
  let array_string_attr = AttributeValue::ArrayStringValue(["a", "b", "c"])
  let array_int_attr = AttributeValue::ArrayIntValue([1, 2, 3])
  
  // 验证类型创建
  match string_attr {
    AttributeValue::StringValue(v) => assert_eq(v, "test-value")
    _ => assert_true(false)
  }
  
  match int_attr {
    AttributeValue::IntValue(v) => assert_eq(v, 42)
    _ => assert_true(false)
  }
  
  match float_attr {
    AttributeValue::FloatValue(v) => assert_true(v > 3.0 && v < 3.2)
    _ => assert_true(false)
  }
  
  match bool_attr {
    AttributeValue::BoolValue(v) => assert_true(v)
    _ => assert_true(false)
  }
  
  match array_string_attr {
    AttributeValue::ArrayStringValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_true(arr.contains("b"))
    }
    _ => assert_true(false)
  }
  
  match array_int_attr {
    AttributeValue::ArrayIntValue(arr) => {
      assert_eq(arr.length(), 3)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
    }
    _ => assert_true(false)
  }
}

// 测试2: Attributes 结构操作
test "attributes_structure_operations" {
  let attrs = Attributes { 
    values: [
      ("key1", AttributeValue::StringValue("value1")),
      ("key2", AttributeValue::IntValue(100)),
      ("key3", AttributeValue::BoolValue(false))
    ]
  }
  
  // 验证属性数量
  assert_eq(attrs.values.length(), 3)
  
  // 验证属性值
  let found_key1 = attrs.values.find(fn(pair) { pair.0 == "key1" })
  match found_key1 {
    Some(pair) => {
      match pair.1 {
        AttributeValue::StringValue(v) => assert_eq(v, "value1")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  let found_key2 = attrs.values.find(fn(pair) { pair.0 == "key2" })
  match found_key2 {
    Some(pair) => {
      match pair.1 {
        AttributeValue::IntValue(v) => assert_eq(v, 100)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证不存在的键
  let missing_key = attrs.values.find(fn(pair) { pair.0 == "missing" })
  assert_true(missing_key.is_none())
}

// 测试3: Resource 结构操作
test "resource_structure_operations" {
  let resource = Resource {
    attributes: [
      ("service.name", AttributeValue::StringValue("azimuth-service")),
      ("service.version", AttributeValue::StringValue("1.0.0")),
      ("service.instance.id", AttributeValue::StringValue("instance-123"))
    ]
  }
  
  // 验证资源属性
  assert_eq(resource.attributes.length(), 3)
  
  // 查找服务名称
  let service_name = resource.attributes.find(fn(attr) { attr.0 == "service.name" })
  match service_name {
    Some(attr) => {
      match attr.1 {
        AttributeValue::StringValue(v) => assert_eq(v, "azimuth-service")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 查找服务版本
  let service_version = resource.attributes.find(fn(attr) { attr.0 == "service.version" })
  match service_version {
    Some(attr) => {
      match attr.1 {
        AttributeValue::StringValue(v) => assert_eq(v, "1.0.0")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试4: InstrumentationScope 操作
test "instrumentation_scope_operations" {
  let scope_with_version = InstrumentationScope {
    name: "azimuth.tracer",
    version: Some("2.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  let scope_minimal = InstrumentationScope {
    name: "azimuth.metrics",
    version: None,
    schema_url: None
  }
  
  // 验证完整范围
  assert_eq(scope_with_version.name, "azimuth.tracer")
  match scope_with_version.version {
    Some(v) => assert_eq(v, "2.0.0")
    None => assert_true(false)
  }
  match scope_with_version.schema_url {
    Some(url) => assert_eq(url, "https://example.com/schema")
    None => assert_true(false)
  }
  
  // 验证最小范围
  assert_eq(scope_minimal.name, "azimuth.metrics")
  assert_true(scope_minimal.version.is_none())
  assert_true(scope_minimal.schema_url.is_none())
}

// 测试5: SpanContext 操作
test "span_context_operations" {
  let span_context = SpanContext {
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    sampled: true,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 验证跟踪上下文
  assert_eq(span_context.trace_id.length(), 32)
  assert_eq(span_context.span_id.length(), 16)
  assert_true(span_context.sampled)
  assert_eq(span_context.trace_state, "key1=value1,key2=value2")
  
  // 验证跟踪ID格式（十六进制）
  let trace_id_chars = span_context.trace_id.to_char_array()
  let all_hex = trace_id_chars.all(fn(c) { 
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
  })
  assert_true(all_hex)
  
  // 验证跨度ID格式（十六进制）
  let span_id_chars = span_context.span_id.to_char_array()
  let span_id_all_hex = span_id_chars.all(fn(c) { 
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
  })
  assert_true(span_id_all_hex)
}

// 测试6: SpanKind 枚举操作
test "span_kind_enum_operations" {
  let internal_span = SpanKind::Internal
  let server_span = SpanKind::Server
  let client_span = SpanKind::Client
  let producer_span = SpanKind::Producer
  let consumer_span = SpanKind::Consumer
  
  // 验证枚举值
  match internal_span {
    SpanKind::Internal => assert_true(true)
    _ => assert_true(false)
  }
  
  match server_span {
    SpanKind::Server => assert_true(true)
    _ => assert_true(false)
  }
  
  match client_span {
    SpanKind::Client => assert_true(true)
    _ => assert_true(false)
  }
  
  match producer_span {
    SpanKind::Producer => assert_true(true)
    _ => assert_true(false)
  }
  
  match consumer_span {
    SpanKind::Consumer => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试7: StatusCode 枚举操作
test "status_code_enum_operations" {
  let unset_status = StatusCode::Unset
  let ok_status = StatusCode::Ok
  let error_status = StatusCode::Error
  
  // 验证枚举值
  match unset_status {
    StatusCode::Unset => assert_true(true)
    _ => assert_true(false)
  }
  
  match ok_status {
    StatusCode::Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  match error_status {
    StatusCode::Error => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试8: Instrument 枚举操作
test "instrument_enum_operations" {
  let counter = Instrument::Counter("request_count", Some("requests"), Some("Total number of requests"))
  let histogram = Instrument::Histogram("request_duration", Some("ms"), Some("Request duration in milliseconds"))
  let updown_counter = Instrument::UpDownCounter("active_connections", Some("connections"), Some("Current active connections"))
  let gauge = Instrument::Gauge("memory_usage", Some("bytes"), Some("Current memory usage"))
  
  // 验证计数器
  match counter {
    Instrument::Counter(name, unit, desc) => {
      assert_eq(name, "request_count")
      match unit {
        Some(u) => assert_eq(u, "requests")
        None => assert_true(false)
      }
      match desc {
        Some(d) => assert_eq(d, "Total number of requests")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证直方图
  match histogram {
    Instrument::Histogram(name, unit, desc) => {
      assert_eq(name, "request_duration")
      match unit {
        Some(u) => assert_eq(u, "ms")
        None => assert_true(false)
      }
      match desc {
        Some(d) => assert_eq(d, "Request duration in milliseconds")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证上下计数器
  match updown_counter {
    Instrument::UpDownCounter(name, unit, desc) => {
      assert_eq(name, "active_connections")
      match unit {
        Some(u) => assert_eq(u, "connections")
        None => assert_true(false)
      }
      match desc {
        Some(d) => assert_eq(d, "Current active connections")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 验证仪表
  match gauge {
    Instrument::Gauge(name, unit, desc) => {
      assert_eq(name, "memory_usage")
      match unit {
        Some(u) => assert_eq(u, "bytes")
        None => assert_true(false)
      }
      match desc {
        Some(d) => assert_eq(d, "Current memory usage")
        None => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}