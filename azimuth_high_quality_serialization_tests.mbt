// Azimuth Telemetry System - High Quality Serialization Tests
// 数据序列化和反序列化测试

test "属性值序列化测试" {
  // 测试各种属性值类型的序列化和反序列化
  
  // 字符串属性值
  let string_value = azimuth::StringValue("test_string_value")
  let serialized_string = azimuth::AttributeValue::serialize(string_value)
  let deserialized_string = azimuth::AttributeValue::deserialize(serialized_string)
  
  match deserialized_string {
    Some(azimuth::StringValue(s)) => assert_eq(s, "test_string_value")
    _ => assert_true(false)
  }
  
  // 整数属性值
  let int_value = azimuth::IntValue(42)
  let serialized_int = azimuth::AttributeValue::serialize(int_value)
  let deserialized_int = azimuth::AttributeValue::deserialize(serialized_int)
  
  match deserialized_int {
    Some(azimuth::IntValue(i)) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // 浮点数属性值
  let float_value = azimuth::FloatValue(3.14159)
  let serialized_float = azimuth::AttributeValue::serialize(float_value)
  let deserialized_float = azimuth::AttributeValue::deserialize(serialized_float)
  
  match deserialized_float {
    Some(azimuth::FloatValue(f)) => assert_true(f - 3.14159 < 0.0001)
    _ => assert_true(false)
  }
  
  // 布尔属性值
  let bool_value = azimuth::BoolValue(true)
  let serialized_bool = azimuth::AttributeValue::serialize(bool_value)
  let deserialized_bool = azimuth::AttributeValue::deserialize(serialized_bool)
  
  match deserialized_bool {
    Some(azimuth::BoolValue(b)) => assert_true(b)
    _ => assert_true(false)
  }
  
  // 字符串数组属性值
  let array_string_value = azimuth::ArrayStringValue(["a", "b", "c", "d", "e"])
  let serialized_array_string = azimuth::AttributeValue::serialize(array_string_value)
  let deserialized_array_string = azimuth::AttributeValue::deserialize(serialized_array_string)
  
  match deserialized_array_string {
    Some(azimuth::ArrayStringValue(arr)) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "a")
      assert_eq(arr[1], "b")
      assert_eq(arr[2], "c")
      assert_eq(arr[3], "d")
      assert_eq(arr[4], "e")
    }
    _ => assert_true(false)
  }
  
  // 整数数组属性值
  let array_int_value = azimuth::ArrayIntValue([1, 2, 3, 4, 5])
  let serialized_array_int = azimuth::AttributeValue::serialize(array_int_value)
  let deserialized_array_int = azimuth::AttributeValue::deserialize(serialized_array_int)
  
  match deserialized_array_int {
    Some(azimuth::ArrayIntValue(arr)) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1)
      assert_eq(arr[1], 2)
      assert_eq(arr[2], 3)
      assert_eq(arr[3], 4)
      assert_eq(arr[4], 5)
    }
    _ => assert_true(false)
  }
  
  // 测试特殊字符和转义
  let special_string_value = azimuth::StringValue("special\"characters\\with\nnewlines\tand\ttabs")
  let serialized_special_string = azimuth::AttributeValue::serialize(special_string_value)
  let deserialized_special_string = azimuth::AttributeValue::deserialize(serialized_special_string)
  
  match deserialized_special_string {
    Some(azimuth::StringValue(s)) => assert_eq(s, "special\"characters\\with\nnewlines\tand\ttabs")
    _ => assert_true(false)
  }
  
  // 测试空值和边界情况
  let empty_string_value = azimuth::StringValue("")
  let serialized_empty_string = azimuth::AttributeValue::serialize(empty_string_value)
  let deserialized_empty_string = azimuth::AttributeValue::deserialize(serialized_empty_string)
  
  match deserialized_empty_string {
    Some(azimuth::StringValue(s)) => assert_eq(s, "")
    _ => assert_true(false)
  }
  
  let zero_int_value = azimuth::IntValue(0)
  let serialized_zero_int = azimuth::AttributeValue::serialize(zero_int_value)
  let deserialized_zero_int = azimuth::AttributeValue::deserialize(serialized_zero_int)
  
  match deserialized_zero_int {
    Some(azimuth::IntValue(i)) => assert_eq(i, 0)
    _ => assert_true(false)
  }
  
  let empty_array_string_value = azimuth::ArrayStringValue([])
  let serialized_empty_array_string = azimuth::AttributeValue::serialize(empty_array_string_value)
  let deserialized_empty_array_string = azimuth::AttributeValue::deserialize(serialized_empty_array_string)
  
  match deserialized_empty_array_string {
    Some(azimuth::ArrayStringValue(arr)) => assert_eq(arr.length(), 0)
    _ => assert_true(false)
  }
}

test "属性集合序列化测试" {
  // 创建属性集合
  let attrs = azimuth::Attributes::new()
  
  // 添加各种类型的属性
  azimuth::Attributes::set(attrs, "string.key", azimuth::StringValue("string.value"))
  azimuth::Attributes::set(attrs, "int.key", azimuth::IntValue(123))
  azimuth::Attributes::set(attrs, "float.key", azimuth::FloatValue(45.67))
  azimuth::Attributes::set(attrs, "bool.key", azimuth::BoolValue(true))
  azimuth::Attributes::set(attrs, "array.string.key", azimuth::ArrayStringValue(["x", "y", "z"]))
  azimuth::Attributes::set(attrs, "array.int.key", azimuth::ArrayIntValue([10, 20, 30]))
  
  // 序列化属性集合
  let serialized_attrs = azimuth::Attributes::serialize(attrs)
  assert_true(serialized_attrs.length() > 0)
  
  // 反序列化属性集合
  let deserialized_attrs_option = azimuth::Attributes::deserialize(serialized_attrs)
  match deserialized_attrs_option {
    Some(deserialized_attrs) => {
      // 验证反序列化的属性
      let string_result = azimuth::Attributes::get(deserialized_attrs, "string.key")
      match string_result {
        Some(azimuth::StringValue(s)) => assert_eq(s, "string.value")
        _ => assert_true(false)
      }
      
      let int_result = azimuth::Attributes::get(deserialized_attrs, "int.key")
      match int_result {
        Some(azimuth::IntValue(i)) => assert_eq(i, 123)
        _ => assert_true(false)
      }
      
      let float_result = azimuth::Attributes::get(deserialized_attrs, "float.key")
      match float_result {
        Some(azimuth::FloatValue(f)) => assert_true(f - 45.67 < 0.001)
        _ => assert_true(false)
      }
      
      let bool_result = azimuth::Attributes::get(deserialized_attrs, "bool.key")
      match bool_result {
        Some(azimuth::BoolValue(b)) => assert_true(b)
        _ => assert_true(false)
      }
      
      let array_string_result = azimuth::Attributes::get(deserialized_attrs, "array.string.key")
      match array_string_result {
        Some(azimuth::ArrayStringValue(arr)) => {
          assert_eq(arr.length(), 3)
          assert_eq(arr[0], "x")
          assert_eq(arr[1], "y")
          assert_eq(arr[2], "z")
        }
        _ => assert_true(false)
      }
      
      let array_int_result = azimuth::Attributes::get(deserialized_attrs, "array.int.key")
      match array_int_result {
        Some(azimuth::ArrayIntValue(arr)) => {
          assert_eq(arr.length(), 3)
          assert_eq(arr[0], 10)
          assert_eq(arr[1], 20)
          assert_eq(arr[2], 30)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试大型属性集合的序列化
  let large_attrs = azimuth::Attributes::new()
  
  // 添加大量属性
  for i in 0..100 {
    let key = "large.key." + i.to_string()
    let value = azimuth::StringValue("large.value." + i.to_string())
    azimuth::Attributes::set(large_attrs, key, value)
  }
  
  // 序列化大型属性集合
  let serialized_large_attrs = azimuth::Attributes::serialize(large_attrs)
  assert_true(serialized_large_attrs.length() > 0)
  
  // 反序列化大型属性集合
  let deserialized_large_attrs_option = azimuth::Attributes::deserialize(serialized_large_attrs)
  match deserialized_large_attrs_option {
    Some(deserialized_large_attrs) => {
      // 验证所有属性都已正确反序列化
      for i in 0..100 {
        let key = "large.key." + i.to_string()
        let expected_value = "large.value." + i.to_string()
        
        let result = azimuth::Attributes::get(deserialized_large_attrs, key)
        match result {
          Some(azimuth::StringValue(s)) => assert_eq(s, expected_value)
          _ => assert_true(false)
        }
      }
    }
    None => assert_true(false)
  }
}

test "跨度上下文序列化测试" {
  // 创建跨度上下文
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let span_id = "00f067aa0ba902b7"
  let trace_state = "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  let span_context = azimuth::SpanContext::new(trace_id, span_id, true, trace_state)
  
  // 序列化跨度上下文
  let serialized_span_context = azimuth::SpanContext::serialize(span_context)
  assert_true(serialized_span_context.length() > 0)
  
  // 反序列化跨度上下文
  let deserialized_span_context_option = azimuth::SpanContext::deserialize(serialized_span_context)
  match deserialized_span_context_option {
    Some(deserialized_span_context) => {
      // 验证反序列化的跨度上下文
      assert_eq(azimuth::SpanContext::trace_id(deserialized_span_context), trace_id)
      assert_eq(azimuth::SpanContext::span_id(deserialized_span_context), span_id)
      assert_eq(azimuth::SpanContext::trace_state(deserialized_span_context), trace_state)
      assert_eq(azimuth::SpanContext::is_sampled(deserialized_span_context), true)
      assert_true(azimuth::SpanContext::is_valid(deserialized_span_context))
    }
    None => assert_true(false)
  }
  
  // 测试无效的跨度上下文
  let invalid_span_context = azimuth::SpanContext::new("", "", false, "")
  
  // 序列化无效的跨度上下文
  let serialized_invalid_span_context = azimuth::SpanContext::serialize(invalid_span_context)
  
  // 反序列化无效的跨度上下文
  let deserialized_invalid_span_context_option = azimuth::SpanContext::deserialize(serialized_invalid_span_context)
  match deserialized_invalid_span_context_option {
    Some(deserialized_invalid_span_context) => {
      // 验证反序列化的无效跨度上下文
      assert_eq(azimuth::SpanContext::trace_id(deserialized_invalid_span_context), "")
      assert_eq(azimuth::SpanContext::span_id(deserialized_invalid_span_context), "")
      assert_eq(azimuth::SpanContext::trace_state(deserialized_invalid_span_context), "")
      assert_eq(azimuth::SpanContext::is_sampled(deserialized_invalid_span_context), false)
      assert_false(azimuth::SpanContext::is_valid(deserialized_invalid_span_context))
    }
    None => assert_true(false)
  }
  
  // 测试未采样的跨度上下文
  let unsampled_span_context = azimuth::SpanContext::new(trace_id, span_id, false, "")
  
  // 序列化未采样的跨度上下文
  let serialized_unsampled_span_context = azimuth::SpanContext::serialize(unsampled_span_context)
  
  // 反序列化未采样的跨度上下文
  let deserialized_unsampled_span_context_option = azimuth::SpanContext::deserialize(serialized_unsampled_span_context)
  match deserialized_unsampled_span_context_option {
    Some(deserialized_unsampled_span_context) => {
      // 验证反序列化的未采样跨度上下文
      assert_eq(azimuth::SpanContext::trace_id(deserialized_unsampled_span_context), trace_id)
      assert_eq(azimuth::SpanContext::span_id(deserialized_unsampled_span_context), span_id)
      assert_eq(azimuth::SpanContext::is_sampled(deserialized_unsampled_span_context), false)
      assert_true(azimuth::SpanContext::is_valid(deserialized_unsampled_span_context))
    }
    None => assert_true(false)
  }
}

test "跨度序列化测试" {
  // 创建跨度上下文
  let span_context = azimuth::SpanContext::new("trace_id", "span_id", true, "")
  
  // 创建跨度
  let span = azimuth::Span::new("test_span", azimuth::Server, span_context)
  
  // 添加属性和事件
  let attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(attrs, "http.method", azimuth::StringValue("GET"))
  azimuth::Attributes::set(attrs, "http.url", azimuth::StringValue("/api/test"))
  azimuth::Attributes::set(attrs, "http.status_code", azimuth::IntValue(200))
  
  azimuth::Span::add_event(span, "request_started", Some(attrs))
  azimuth::Span::set_status(span, azimuth::Ok, Some("Request completed successfully"))
  
  // 序列化跨度
  let serialized_span = azimuth::Span::serialize(span)
  assert_true(serialized_span.length() > 0)
  
  // 反序列化跨度
  let deserialized_span_option = azimuth::Span::deserialize(serialized_span)
  match deserialized_span_option {
    Some(deserialized_span) => {
      // 验证反序列化的跨度
      assert_eq(azimuth::Span::name(deserialized_span), "test_span")
      
      match azimuth::Span::kind(deserialized_span) {
        azimuth::Server => assert_true(true)
        _ => assert_true(false)
      }
      
      assert_eq(azimuth::Span::status(deserialized_span), azimuth::Ok)
      
      // 验证跨度上下文
      let deserialized_span_context = azimuth::Span::span_context(deserialized_span)
      assert_eq(azimuth::SpanContext::trace_id(deserialized_span_context), "trace_id")
      assert_eq(azimuth::SpanContext::span_id(deserialized_span_context), "span_id")
      assert_true(azimuth::SpanContext::is_sampled(deserialized_span_context))
      
      // 验证事件
      let events = azimuth::Span::get_events(deserialized_span)
      assert_eq(events.length(), 1)
      
      let event = events[0]
      assert_eq(azimuth::Event::name(event), "request_started")
      
      let event_attrs = azimuth::Event::attributes(event)
      let http_method = azimuth::Attributes::get(event_attrs, "http.method")
      match http_method {
        Some(azimuth::StringValue(method)) => assert_eq(method, "GET")
        _ => assert_true(false)
      }
      
      let http_url = azimuth::Attributes::get(event_attrs, "http.url")
      match http_url {
        Some(azimuth::StringValue(url)) => assert_eq(url, "/api/test")
        _ => assert_true(false)
      }
      
      let http_status_code = azimuth::Attributes::get(event_attrs, "http.status_code")
      match http_status_code {
        Some(azimuth::IntValue(status_code)) => assert_eq(status_code, 200)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试带有多个事件的跨度
  let multi_event_span_context = azimuth::SpanContext::new("multi_trace", "multi_span", true, "")
  let multi_event_span = azimuth::Span::new("multi_event_span", azimuth::Client, multi_event_span_context)
  
  // 添加多个事件
  for i in 0..5 {
    let event_attrs = azimuth::Attributes::new()
    azimuth::Attributes::set(event_attrs, "event.index", azimuth::IntValue(i))
    azimuth::Attributes::set(event_attrs, "event.data", azimuth::StringValue("data_" + i.to_string()))
    
    azimuth::Span::add_event(multi_event_span, "event_" + i.to_string(), Some(event_attrs))
  }
  
  // 序列化多事件跨度
  let serialized_multi_event_span = azimuth::Span::serialize(multi_event_span)
  
  // 反序列化多事件跨度
  let deserialized_multi_event_span_option = azimuth::Span::deserialize(serialized_multi_event_span)
  match deserialized_multi_event_span_option {
    Some(deserialized_multi_event_span) => {
      // 验证所有事件都已正确反序列化
      let events = azimuth::Span::get_events(deserialized_multi_event_span)
      assert_eq(events.length(), 5)
      
      for i in 0..5 {
        let event = events[i]
        assert_eq(azimuth::Event::name(event), "event_" + i.to_string())
        
        let event_attrs = azimuth::Event::attributes(event)
        let event_index = azimuth::Attributes::get(event_attrs, "event.index")
        match event_index {
          Some(azimuth::IntValue(index)) => assert_eq(index, i)
          _ => assert_true(false)
        }
        
        let event_data = azimuth::Attributes::get(event_attrs, "event.data")
        match event_data {
          Some(azimuth::StringValue(data)) => assert_eq(data, "data_" + i.to_string())
          _ => assert_true(false)
        }
      }
    }
    None => assert_true(false)
  }
}

test "日志记录序列化测试" {
  // 创建日志记录
  let log_record = azimuth::LogRecord::new(azimuth::Info, "Test log message")
  
  // 序列化日志记录
  let serialized_log_record = azimuth::LogRecord::serialize(log_record)
  assert_true(serialized_log_record.length() > 0)
  
  // 反序列化日志记录
  let deserialized_log_record_option = azimuth::LogRecord::deserialize(serialized_log_record)
  match deserialized_log_record_option {
    Some(deserialized_log_record) => {
      // 验证反序列化的日志记录
      assert_eq(azimuth::LogRecord::severity_number(deserialized_log_record), azimuth::Info)
      
      match azimuth::LogRecord::body(deserialized_log_record) {
        Some(body) => assert_eq(body, "Test log message")
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试带有属性的日志记录
  let log_attrs = azimuth::Attributes::new()
  azimuth::Attributes::set(log_attrs, "logger.name", azimuth::StringValue("test.logger"))
  azimuth::Attributes::set(log_attrs, "thread.id", azimuth::IntValue(12345))
  azimuth::Attributes::set(log_attrs, "process.id", azimuth::IntValue(67890))
  
  let log_record_with_attrs = azimuth::LogRecord::new_with_attributes(
    azimuth::Error,
    Some("Error occurred"),
    Some(log_attrs),
    Some(1609459200000L),
    Some(1609459200100L),
    Some("trace_id"),
    Some("span_id")
  )
  
  // 序列化带属性的日志记录
  let serialized_log_record_with_attrs = azimuth::LogRecord::serialize(log_record_with_attrs)
  
  // 反序列化带属性的日志记录
  let deserialized_log_record_with_attrs_option = azimuth::LogRecord::deserialize(serialized_log_record_with_attrs)
  match deserialized_log_record_with_attrs_option {
    Some(deserialized_log_record_with_attrs) => {
      // 验证反序列化的带属性日志记录
      assert_eq(azimuth::LogRecord::severity_number(deserialized_log_record_with_attrs), azimuth::Error)
      
      match azimuth::LogRecord::body(deserialized_log_record_with_attrs) {
        Some(body) => assert_eq(body, "Error occurred")
        None => assert_true(false)
      }
      
      match azimuth::LogRecord::timestamp(deserialized_log_record_with_attrs) {
        Some(timestamp) => assert_eq(timestamp, 1609459200000L)
        None => assert_true(false)
      }
      
      match azimuth::LogRecord::trace_id(deserialized_log_record_with_attrs) {
        Some(trace_id) => assert_eq(trace_id, "trace_id")
        None => assert_true(false)
      }
      
      match azimuth::LogRecord::span_id(deserialized_log_record_with_attrs) {
        Some(span_id) => assert_eq(span_id, "span_id")
        None => assert_true(false)
      }
      
      // 验证属性
      let attrs = azimuth::LogRecord::attributes(deserialized_log_record_with_attrs)
      let logger_name = azimuth::Attributes::get(attrs, "logger.name")
      match logger_name {
        Some(azimuth::StringValue(name)) => assert_eq(name, "test.logger")
        _ => assert_true(false)
      }
      
      let thread_id = azimuth::Attributes::get(attrs, "thread.id")
      match thread_id {
        Some(azimuth::IntValue(id)) => assert_eq(id, 12345)
        _ => assert_true(false)
      }
      
      let process_id = azimuth::Attributes::get(attrs, "process.id")
      match process_id {
        Some(azimuth::IntValue(id)) => assert_eq(id, 67890)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

test "资源序列化测试" {
  // 创建资源
  let resource_attrs = [
    ("service.name", azimuth::StringValue("test_service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("instance-123")),
    ("host.name", azimuth::StringValue("test-host")),
    ("host.arch", azimuth::StringValue("x86_64")),
    ("os.type", azimuth::StringValue("linux")),
    ("os.version", azimuth::StringValue("5.4.0")),
    ("process.pid", azimuth::IntValue(12345)),
    ("process.executable.name", azimuth::StringValue("test-app")),
    ("process.command_args", azimuth::ArrayStringValue(["--config", "config.yaml", "--verbose"]))
  ]
  
  let resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource_attrs)
  
  // 序列化资源
  let serialized_resource = azimuth::Resource::serialize(resource)
  assert_true(serialized_resource.length() > 0)
  
  // 反序列化资源
  let deserialized_resource_option = azimuth::Resource::deserialize(serialized_resource)
  match deserialized_resource_option {
    Some(deserialized_resource) => {
      // 验证反序列化的资源属性
      let service_name = azimuth::Resource::get_attribute(deserialized_resource, "service.name")
      match service_name {
        Some(azimuth::StringValue(name)) => assert_eq(name, "test_service")
        _ => assert_true(false)
      }
      
      let service_version = azimuth::Resource::get_attribute(deserialized_resource, "service.version")
      match service_version {
        Some(azimuth::StringValue(version)) => assert_eq(version, "1.0.0")
        _ => assert_true(false)
      }
      
      let service_instance_id = azimuth::Resource::get_attribute(deserialized_resource, "service.instance.id")
      match service_instance_id {
        Some(azimuth::StringValue(id)) => assert_eq(id, "instance-123")
        _ => assert_true(false)
      }
      
      let host_name = azimuth::Resource::get_attribute(deserialized_resource, "host.name")
      match host_name {
        Some(azimuth::StringValue(name)) => assert_eq(name, "test-host")
        _ => assert_true(false)
      }
      
      let host_arch = azimuth::Resource::get_attribute(deserialized_resource, "host.arch")
      match host_arch {
        Some(azimuth::StringValue(arch)) => assert_eq(arch, "x86_64")
        _ => assert_true(false)
      }
      
      let os_type = azimuth::Resource::get_attribute(deserialized_resource, "os.type")
      match os_type {
        Some(azimuth::StringValue(os)) => assert_eq(os, "linux")
        _ => assert_true(false)
      }
      
      let os_version = azimuth::Resource::get_attribute(deserialized_resource, "os.version")
      match os_version {
        Some(azimuth::StringValue(version)) => assert_eq(version, "5.4.0")
        _ => assert_true(false)
      }
      
      let process_pid = azimuth::Resource::get_attribute(deserialized_resource, "process.pid")
      match process_pid {
        Some(azimuth::IntValue(pid)) => assert_eq(pid, 12345)
        _ => assert_true(false)
      }
      
      let process_executable_name = azimuth::Resource::get_attribute(deserialized_resource, "process.executable.name")
      match process_executable_name {
        Some(azimuth::StringValue(name)) => assert_eq(name, "test-app")
        _ => assert_true(false)
      }
      
      let process_command_args = azimuth::Resource::get_attribute(deserialized_resource, "process.command_args")
      match process_command_args {
        Some(azimuth::ArrayStringValue(args)) => {
          assert_eq(args.length(), 2)
          assert_eq(args[0], "--config")
          assert_eq(args[1], "config.yaml")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试资源合并后的序列化
  let resource1_attrs = [
    ("service.name", azimuth::StringValue("service1")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("common.attr", azimuth::StringValue("value1"))
  ]
  
  let resource2_attrs = [
    ("service.name", azimuth::StringValue("service2")),
    ("service.version", azimuth::StringValue("2.0.0")),
    ("additional.attr", azimuth::StringValue("value2"))
  ]
  
  let resource1 = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource1_attrs)
  let resource2 = azimuth::Resource::with_attributes(azimuth::Resource::new(), resource2_attrs)
  
  // 合并资源
  let merged_resource = azimuth::Resource::merge(resource1, resource2)
  
  // 序列化合并的资源
  let serialized_merged_resource = azimuth::Resource::serialize(merged_resource)
  
  // 反序列化合并的资源
  let deserialized_merged_resource_option = azimuth::Resource::deserialize(serialized_merged_resource)
  match deserialized_merged_resource_option {
    Some(deserialized_merged_resource) => {
      // 验证合并后的资源属性
      // 注意：合并行为可能因实现而异，这里假设resource2的属性会覆盖resource1的属性
      
      let service_name = azimuth::Resource::get_attribute(deserialized_merged_resource, "service.name")
      match service_name {
        Some(azimuth::StringValue(name)) => assert_eq(name, "service2")
        _ => assert_true(false)
      }
      
      let service_version = azimuth::Resource::get_attribute(deserialized_merged_resource, "service.version")
      match service_version {
        Some(azimuth::StringValue(version)) => assert_eq(version, "2.0.0")
        _ => assert_true(false)
      }
      
      let common_attr = azimuth::Resource::get_attribute(deserialized_merged_resource, "common.attr")
      match common_attr {
        Some(azimuth::StringValue(value)) => assert_eq(value, "value1")
        _ => assert_true(false)
      }
      
      let additional_attr = azimuth::Resource::get_attribute(deserialized_merged_resource, "additional.attr")
      match additional_attr {
        Some(azimuth::StringValue(value)) => assert_eq(value, "value2")
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}