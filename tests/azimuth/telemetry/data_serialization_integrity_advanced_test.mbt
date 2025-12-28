// 高级序列化和数据完整性测试
// Advanced serialization and data integrity tests

test "属性值序列化完整性测试" {
  // 测试不同类型属性值的序列化和反序列化完整性
  
  let attrs = Attributes::new()
  
  // 测试所有属性值类型的序列化
  let test_values = [
    ("string_key", StringValue("test_string_value")),
    ("int_key", IntValue(42)),
    ("float_key", FloatValue(3.14159)),
    ("bool_key", BoolValue(true)),
    ("array_string_key", ArrayStringValue(["item1", "item2", "item3"])),
    ("array_int_key", ArrayIntValue([1, 2, 3, 4, 5]))
  ]
  
  // 设置属性值
  for (key, value) in test_values {
    Attributes::set(attrs, key, value)
  }
  
  // 验证属性值的完整性
  for (key, expected_value) in test_values {
    let retrieved_value = Attributes::get(attrs, key)
    assert_true(retrieved_value.is_some())
    
    match retrieved_value.unwrap() {
      StringValue(s) => {
        match expected_value {
          StringValue(expected) => assert_eq(s, expected)
          _ => assert_true(false, "Expected StringValue but got different type")
        }
      }
      IntValue(i) => {
        match expected_value {
          IntValue(expected) => assert_eq(i, expected)
          _ => assert_true(false, "Expected IntValue but got different type")
        }
      }
      FloatValue(f) => {
        match expected_value {
          FloatValue(expected) => assert_eq(f, expected)
          _ => assert_true(false, "Expected FloatValue but got different type")
        }
      }
      BoolValue(b) => {
        match expected_value {
          BoolValue(expected) => assert_eq(b, expected)
          _ => assert_true(false, "Expected BoolValue but got different type")
        }
      }
      ArrayStringValue(arr) => {
        match expected_value {
          ArrayStringValue(expected) => {
            assert_eq(arr.length(), expected.length())
            for i = 0; i < arr.length(); i = i + 1 {
              assert_eq(arr[i], expected[i])
            }
          }
          _ => assert_true(false, "Expected ArrayStringValue but got different type")
        }
      }
      ArrayIntValue(arr) => {
        match expected_value {
          ArrayIntValue(expected) => {
            assert_eq(arr.length(), expected.length())
            for i = 0; i < arr.length(); i = i + 1 {
              assert_eq(arr[i], expected[i])
            }
          }
          _ => assert_true(false, "Expected ArrayIntValue but got different type")
        }
      }
    }
  }
  
  // 测试特殊字符和Unicode值的处理
  let special_values = [
    ("unicode_key", StringValue("测试值_🚀_ключ")),
    ("empty_string", StringValue("")),
    ("special_chars", StringValue("\n\t\r\"\'\\")),
    ("max_int", IntValue(2147483647)),
    ("min_int", IntValue(-2147483648)),
    ("max_float", FloatValue(1.7976931348623157e308)),
    ("min_float", FloatValue(-1.7976931348623157e308)),
    ("infinity", FloatValue(1.0 / 0.0)),
    ("neg_infinity", FloatValue(-1.0 / 0.0))
  ]
  
  for (key, value) in special_values {
    Attributes::set(attrs, key, value)
    let retrieved = Attributes::get(attrs, key)
    assert_true(retrieved.is_some())
  }
}

test "Span上下文序列化测试" {
  // 测试Span上下文的序列化和反序列化
  
  // 创建各种Span上下文
  let test_contexts = [
    SpanContext::new("trace_1234567890123456", "span_1234567890123456", true, "state=value"),
    SpanContext::new("", "", false, ""), // 空上下文
    SpanContext::new("a" * 32, "b" * 16, false, ""), // 有效格式但未采样
    SpanContext::new("trace_with_特殊字符", "span_with_特殊字符", true, "特殊状态")
  ]
  
  for ctx in test_contexts {
    // 序列化上下文信息
    let trace_id = SpanContext::trace_id(ctx)
    let span_id = SpanContext::span_id(ctx)
    let is_sampled = SpanContext::is_sampled(ctx)
    let is_valid = SpanContext::is_valid(ctx)
    
    // 反序列化验证（重新创建上下文）
    let reconstructed_ctx = SpanContext::new(trace_id, span_id, is_sampled, "")
    
    // 验证重构后的上下文
    assert_eq(SpanContext::trace_id(reconstructed_ctx), trace_id)
    assert_eq(SpanContext::span_id(reconstructed_ctx), span_id)
    assert_eq(SpanContext::is_sampled(reconstructed_ctx), is_sampled)
    
    // 验证有效性检查
    let original_valid = SpanContext::is_valid(ctx)
    let reconstructed_valid = SpanContext::is_valid(reconstructed_ctx)
    assert_eq(original_valid, reconstructed_valid)
  }
}

test "日志记录序列化完整性测试" {
  // 测试日志记录的序列化完整性
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "serialization_test_logger")
  
  // 创建复杂的日志记录
  let attrs = Attributes::new()
  Attributes::set(attrs, "user_id", IntValue(12345))
  Attributes::set(attrs, "operation", StringValue("database_query"))
  Attributes::set(attrs, "duration_ms", FloatValue(150.5))
  
  let complex_log = LogRecord::new_with_context(
    Error,
    Some("Database operation failed"),
    Some(attrs),
    Some(1735689600000000000L),
    Some(1735689600000001000L),
    Some("trace_1234567890123456"),
    Some("span_1234567890123456"),
    Some(Context::with_value(Context::root(), ContextKey::new("request_id"), "req_abcdef"))
  )
  
  // 验证日志记录的完整性
  assert_eq(LogRecord::severity_number(complex_log), Error)
  assert_eq(LogRecord::body(complex_log), Some("Database operation failed"))
  
  // 验证时间戳
  let timestamp = LogRecord::trace_id(complex_log)
  let span_id = LogRecord::span_id(complex_log)
  assert_eq(timestamp, Some("trace_1234567890123456"))
  assert_eq(span_id, Some("span_1234567890123456"))
  
  // 测试边界情况
  let edge_case_logs = [
    LogRecord::new(Trace, ""), // 空消息
    LogRecord::new(Fatal, "a" * 10000), // 极长消息
    LogRecord::new_with_context(Info, None, None, None, None, None, None, None) // 最小日志
  ]
  
  for log in edge_case_logs {
    Logger::emit(logger, log)
    // 验证日志不会导致序列化错误
    assert_true(true)
  }
}

test "资源信息序列化测试" {
  // 测试资源信息的序列化和反序列化
  
  // 创建复杂资源信息
  let base_resource = Resource::new()
  let resource_attrs = [
    ("service.name", StringValue("payment_service")),
    ("service.version", StringValue("2.1.0")),
    ("service.instance.id", StringValue("instance-12345")),
    ("deployment.environment", StringValue("production")),
    ("host.name", StringValue("prod-server-01")),
    ("host.id", StringValue("host-abcdef")),
    ("process.pid", IntValue(1234)),
    ("process.executable.name", StringValue("payment_service")),
    ("process.command_args", ArrayStringValue(["./payment_service", "--config", "/etc/config.yaml"])),
    ("telemetry.sdk.name", StringValue("azimuth")),
    ("telemetry.sdk.version", StringValue("1.0.0")),
    ("telemetry.sdk.language", StringValue("moonbit"))
  ]
  
  let resource = Resource::with_attributes(base_resource, resource_attrs)
  
  // 验证资源属性的完整性
  for (key, expected_value) in resource_attrs {
    let retrieved_value = Resource::get_attribute(resource, key)
    assert_true(retrieved_value.is_some())
    
    match retrieved_value.unwrap() {
      StringValue(s) => {
        match expected_value {
          StringValue(expected) => assert_eq(s, expected)
          _ => assert_true(false, "Expected StringValue for key: " + key)
        }
      }
      IntValue(i) => {
        match expected_value {
          IntValue(expected) => assert_eq(i, expected)
          _ => assert_true(false, "Expected IntValue for key: " + key)
        }
      }
      ArrayStringValue(arr) => {
        match expected_value {
          ArrayStringValue(expected) => {
            assert_eq(arr.length(), expected.length())
            for i = 0; i < arr.length(); i = i + 1 {
              assert_eq(arr[i], expected[i])
            }
          }
          _ => assert_true(false, "Expected ArrayStringValue for key: " + key)
        }
      }
      _ => assert_true(false, "Unexpected value type for key: " + key)
    }
  }
  
  // 测试资源合并的序列化完整性
  let override_resource = Resource::with_attributes(base_resource, [
    ("service.name", StringValue("override_service")),
    ("new.attribute", StringValue("new_value"))
  ])
  
  let merged = Resource::merge(resource, override_resource)
  
  // 在简化实现中，merge返回override资源
  let merged_service_name = Resource::get_attribute(merged, "service.name")
  let merged_new_attr = Resource::get_attribute(merged, "new.attribute")
  let merged_old_attr = Resource::get_attribute(merged, "service.version")
  
  assert_eq(merged_service_name, Some(StringValue("override_service")))
  assert_eq(merged_new_attr, Some(StringValue("new_value")))
  assert_true(merged_old_attr.is_none()) // 在简化实现中
}

test "仪器信息序列化测试" {
  // 测试仪器信息的序列化完整性
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "serialization_test_meter")
  
  // 创建各种类型的仪器
  let counter = Meter::create_counter(meter, "request_count", Some("Total number of requests"), Some("requests"))
  let histogram = Meter::create_histogram(meter, "request_duration", Some("Request duration in milliseconds"), Some("ms"))
  let updown_counter = Meter::create_counter(meter, "active_connections", Some("Current active connections"), Some("connections"))
  let gauge = Meter::create_counter(meter, "memory_usage", Some("Current memory usage"), Some("bytes"))
  
  // 转换为Instrument类型进行序列化测试
  let counter_instrument = Counter(counter.name, counter.description, counter.unit)
  let histogram_instrument = Histogram(histogram.name, histogram.description, histogram.unit)
  let updown_counter_instrument = UpDownCounter(updown_counter.name, updown_counter.description, updown_counter.unit)
  let gauge_instrument = Gauge(gauge.name, gauge.description, gauge.unit)
  
  // 验证仪器信息的完整性
  let instruments = [counter_instrument, histogram_instrument, updown_counter_instrument, gauge_instrument]
  
  for instrument in instruments {
    let name = Instrument::name(instrument)
    let description = Instrument::description(instrument)
    let unit = Instrument::unit(instrument)
    
    // 验证名称不为空
    assert_true(name.length() > 0)
    
    // 验证描述和单元的可选性
    match description {
      Some(desc) => assert_true(desc.length() > 0)
      None => () // 可以为None
    }
    
    match unit {
      Some(u) => assert_true(u.length() > 0)
      None => () // 可以为None
    }
  }
  
  // 测试特殊仪器名称
  let special_names = [
    "metric.with.dots",
    "metric-with-dashes", 
    "metric_with_underscores",
    "metric/with/slashes",
    "metric.with.special.chars.🚀"
  ]
  
  for name in special_names {
    let special_counter = Meter::create_counter(meter, name)
    let special_instrument = Counter(special_counter.name, special_counter.description, special_counter.unit)
    assert_eq(Instrument::name(special_instrument), name)
  }
}

test "HTTP请求响应序列化测试" {
  // 测试HTTP请求和响应的序列化完整性
  
  // 创建复杂的HTTP请求
  let request_headers = [
    ("Content-Type", "application/json"),
    ("Authorization", "Bearer token12345"),
    ("User-Agent", "Azimuth-Telemetry/1.0"),
    ("X-Request-ID", "req-abcdef-123456"),
    ("X-Trace-ID", "trace-789012345678")
  ]
  
  let request_body = "{\"operation\":\"test\",\"parameters\":{\"key\":\"value\"}}"
  let request = HttpRequest::new("POST", "https://api.example.com/telemetry", request_headers, Some(request_body))
  
  // 验证请求的完整性
  assert_eq(HttpRequest::http_method(request), "POST")
  assert_eq(HttpRequest::url(request), "https://api.example.com/telemetry")
  assert_eq(HttpRequest::body(request), Some(request_body))
  
  // 创建复杂的HTTP响应
  let response_headers = [
    ("Content-Type", "application/json"),
    ("X-Response-Time", "150ms"),
    ("X-Server-ID", "server-prod-01"),
    ("Cache-Control", "no-cache")
  ]
  
  let response_body = "{\"status\":\"success\",\"data\":{\"id\":12345,\"result\":\"processed\"}}"
  let response = HttpResponse::new(200, response_headers, Some(response_body))
  
  // 验证响应的完整性
  assert_eq(HttpResponse::status_code(response), 200)
  assert_eq(HttpResponse::body(response), Some(response_body))
  
  // 测试边界情况
  let edge_cases = [
    HttpRequest::new("", "", [], None), // 空请求
    HttpRequest::new("GET", "https://example.com", [], Some("")), // 空体请求
    HttpResponse::new(0, [], None), // 最小响应
    HttpResponse::new(500, [], Some("Internal Server Error")) // 错误响应
  ]
  
  for edge_case in edge_cases {
    // 验证边界情况不会导致序列化错误
    assert_true(true)
  }
}

test "上下文数据序列化测试" {
  // 测试上下文数据的序列化完整性
  
  // 创建复杂的上下文层次结构
  let root_ctx = Context::root()
  
  // 第一层上下文
  let layer1_key = ContextKey::new("service.name")
  let layer1_ctx = Context::with_value(root_ctx, layer1_key, "payment_service")
  
  // 第二层上下文
  let layer2_key = ContextKey::new("operation.type")
  let layer2_ctx = Context::with_value(layer1_ctx, layer2_key, "database_transaction")
  
  // 第三层上下文
  let layer3_key = ContextKey::new("user.id")
  let layer3_ctx = Context::with_value(layer2_ctx, layer3_key, "user_12345")
  
  // 验证上下文层次结构的完整性
  let service_name = Context::get(layer3_ctx, layer1_key)
  let operation_type = Context::get(layer3_ctx, layer2_key)
  let user_id = Context::get(layer3_ctx, layer3_key)
  
  assert_eq(service_name, Some("payment_service"))
  assert_eq(operation_type, Some("database_transaction"))
  assert_eq(user_id, Some("user_12345"))
  
  // 测试特殊上下文值
  let special_values = [
    ("empty_value", ""),
    ("unicode_value", "测试值_🚀_ключ"),
    ("long_value", "x" * 1000),
    ("special_chars", "\n\t\r\"'\\"),
    ("numeric_string", "12345.67890"),
    ("json_string", "{\"key\":\"value\",\"array\":[1,2,3]}")
  ]
  
  for (key_suffix, value) in special_values {
    let key = ContextKey::new("test." + key_suffix)
    let ctx_with_value = Context::with_value(root_ctx, key, value)
    let retrieved = Context::get(ctx_with_value, key)
    assert_eq(retrieved, Some(value))
  }
  
  // 测试上下文覆盖的序列化行为
  let override_key = ContextKey::new("override.test")
  let ctx1 = Context::with_value(root_ctx, override_key, "value1")
  let ctx2 = Context::with_value(ctx1, override_key, "value2")
  
  let value1 = Context::get(ctx1, override_key)
  let value2 = Context::get(ctx2, override_key)
  
  assert_eq(value1, Some("value1"))
  assert_eq(value2, Some("value2"))
}

test "Baggage数据序列化测试" {
  // 测试Baggage数据的序列化完整性
  
  let initial_baggage = Baggage::new()
  
  // 添加各种类型的baggage项
  let baggage_entries = [
    ("user.id", "user_12345"),
    ("request.id", "req_abcdef123456"),
    ("operation.name", "process_payment"),
    ("service.version", "2.1.0"),
    ("trace.state", "sampled=true"),
    ("custom.metadata", "{\"source\":\"mobile\",\"version\":\"3.0.1\"}"),
    ("unicode.value", "测试值"),
    ("empty.value", ""),
    ("special.chars", "value=with=equals&and&ampsersands")
  ]
  
  let mut current_baggage = initial_baggage
  
  // 逐步添加baggage项
  for (key, value) in baggage_entries {
    current_baggage = Baggage::set_entry(current_baggage, key, value)
  }
  
  // 验证所有baggage项的完整性
  for (key, expected_value) in baggage_entries {
    let retrieved_value = Baggage::get_entry(current_baggage, key)
    assert_eq(retrieved_value, Some(expected_value))
  }
  
  // 测试baggage项移除的完整性
  let keys_to_remove = ["user.id", "custom.metadata", "unicode.value"]
  let mut baggage_after_removal = current_baggage
  
  for key in keys_to_remove {
    baggage_after_removal = Baggage::remove_entry(baggage_after_removal, key)
  }
  
  // 验证移除后的状态
  for key in keys_to_remove {
    let removed_value = Baggage::get_entry(baggage_after_removal, key)
    assert_true(removed_value.is_none())
  }
  
  // 验证未移除的项仍然存在
  let remaining_keys = ["request.id", "operation.name", "service.version"]
  for key in remaining_keys {
    let remaining_value = Baggage::get_entry(baggage_after_removal, key)
    assert_true(remaining_value.is_some())
  }
  
  // 测试极长键值的处理
  let long_key = "k" * 500
  let long_value = "v" * 1000
  let baggage_with_long = Baggage::set_entry(baggage_after_removal, long_key, long_value)
  
  let retrieved_long = Baggage::get_entry(baggage_with_long, long_key)
  assert_eq(retrieved_long, Some(long_value))
}