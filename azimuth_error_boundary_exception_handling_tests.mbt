// Azimuth Error Boundary and Exception Handling Test Suite
// 错误边界和异常处理测试套件，验证遥测系统在各种异常情况下的稳定性和恢复能力

// 测试1: 属性值类型转换错误处理
test "属性值类型转换错误处理测试" {
  // 创建包含各种类型属性值的集合
  let attrs = azimuth::Attributes::new()
  attrs = azimuth::Attributes::set(attrs, "string.value", azimuth::AttributeValue::StringValue("not-a-number"))
  attrs = azimuth::Attributes::set(attrs, "int.value", azimuth::AttributeValue::IntValue(42))
  attrs = azimuth::Attributes::set(attrs, "float.value", azimuth::AttributeValue::FloatValue(3.14))
  attrs = azimuth::Attributes::set(attrs, "bool.value", azimuth::AttributeValue::BoolValue(true))
  attrs = azimuth::Attributes::set(attrs, "array.string.value", azimuth::AttributeValue::ArrayStringValue(["a", "b", "c"]))
  attrs = azimuth::Attributes::set(attrs, "array.int.value", azimuth::AttributeValue::ArrayIntValue([1, 2, 3]))
  
  // 测试字符串到整数的错误转换
  let string_value = azimuth::Attributes::get(attrs, "string.value")
  match string_value {
    Some(azimuth::AttributeValue::StringValue(s)) => {
      let int_result = azimuth::AttributeValue::convert_to_int(s)
      match int_result {
        Ok(_) => assert_true(false) // 不应该成功转换
        Err(error) => assert_eq(error.message, "Cannot convert string 'not-a-number' to integer")
      }
    }
    _ => assert_true(false)
  }
  
  // 测试字符串到浮点数的错误转换
  match string_value {
    Some(azimuth::AttributeValue::StringValue(s)) => {
      let float_result = azimuth::AttributeValue::convert_to_float(s)
      match float_result {
        Ok(_) => assert_true(false) // 不应该成功转换
        Err(error) => assert_eq(error.message, "Cannot convert string 'not-a-number' to float")
      }
    }
    _ => assert_true(false)
  }
  
  // 测试字符串到布尔值的错误转换
  match string_value {
    Some(azimuth::AttributeValue::StringValue(s)) => {
      let bool_result = azimuth::AttributeValue::convert_to_bool(s)
      match bool_result {
        Ok(_) => assert_true(false) // 不应该成功转换
        Err(error) => assert_eq(error.message, "Cannot convert string 'not-a-number' to boolean")
      }
    }
    _ => assert_true(false)
  }
  
  // 测试有效的类型转换
  let int_value = azimuth::Attributes::get(attrs, "int.value")
  match int_value {
    Some(azimuth::AttributeValue::IntValue(i)) => {
      let string_result = azimuth::AttributeValue::convert_to_string(i)
      match string_result {
        Ok(s) => assert_eq(s, "42")
        Err(_) => assert_true(false)
      }
      
      let float_result = azimuth::AttributeValue::convert_to_float(i)
      match float_result {
        Ok(f) => assert_eq(f, 42.0)
        Err(_) => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
  
  // 测试数组类型转换错误
  let array_string_value = azimuth::Attributes::get(attrs, "array.string.value")
  match array_string_value {
    Some(azimuth::AttributeValue::ArrayStringValue(arr)) => {
      let int_array_result = azimuth::AttributeValue::convert_to_int_array(arr)
      match int_array_result {
        Ok(_) => assert_true(false) // 不应该成功转换字符串数组到整数数组
        Err(error) => assert_eq(error.message, "Cannot convert string array to integer array")
      }
    }
    _ => assert_true(false)
  }
}

// 测试2: 跨度操作错误处理
test "跨度操作错误处理测试" {
  // 测试无效跨度上下文处理
  let invalid_trace_id = "" // 空的追踪ID
  let invalid_span_id = "" // 空的跨度ID
  let span_ctx = azimuth::SpanContext::new(invalid_trace_id, invalid_span_id, true, "")
  
  // 验证无效跨度上下文
  assert_false(azimuth::SpanContext::is_valid(span_ctx))
  
  // 测试使用无效跨度上下文创建跨度
  let span_result = azimuth::Span::new_with_context("test_span", span_ctx)
  match span_result {
    Ok(_) => assert_true(false) // 不应该成功创建跨度
    Err(error) => assert_eq(error.message, "Cannot create span with invalid context")
  }
  
  // 测试使用有效跨度上下文创建跨度
  let valid_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let valid_span_id = "00f067aa0ba902b7"
  let valid_span_ctx = azimuth::SpanContext::new(valid_trace_id, valid_span_id, true, "")
  
  let valid_span_result = azimuth::Span::new_with_context("test_span", valid_span_ctx)
  match valid_span_result {
    Ok(span) => {
      // 测试结束已经结束的跨度
      azimuth::Span::end(span)
      let end_again_result = azimuth::Span::end(span)
      match end_again_result {
        Ok(_) => assert_true(false) // 不应该成功结束已经结束的跨度
        Err(error) => assert_eq(error.message, "Cannot end already ended span")
      }
      
      // 测试向已结束的跨度添加事件
      let add_event_result = azimuth::Span::add_event(span, "test_event", None)
      match add_event_result {
        Ok(_) => assert_true(false) // 不应该成功向已结束的跨度添加事件
        Err(error) => assert_eq(error.message, "Cannot add event to ended span")
      }
      
      // 测试设置已结束跨度的状态
      let set_status_result = azimuth::Span::set_status(span, azimuth::SpanStatus::Error, Some("Test error"))
      match set_status_result {
        Ok(_) => assert_true(false) // 不应该成功设置已结束跨度的状态
        Err(error) => assert_eq(error.message, "Cannot set status on ended span")
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试跨度属性错误处理
  let valid_span_result = azimuth::Span::new_with_context("test_span", valid_span_ctx)
  match valid_span_result {
    Ok(span) => {
      // 测试设置空键的属性
      let set_empty_key_result = azimuth::Span::set_attribute(span, "", azimuth::AttributeValue::StringValue("value"))
      match set_empty_key_result {
        Ok(_) => assert_true(false) // 不应该成功设置空键属性
        Err(error) => assert_eq(error.message, "Cannot set attribute with empty key")
      }
      
      // 测试设置过长的属性键
      let long_key = "a".repeat(257) // 超过256字符限制
      let set_long_key_result = azimuth::Span::set_attribute(span, long_key, azimuth::AttributeValue::StringValue("value"))
      match set_long_key_result {
        Ok(_) => assert_true(false) // 不应该成功设置过长键属性
        Err(error) => assert_eq(error.message, "Attribute key exceeds maximum length of 256 characters")
      }
      
      // 测试设置过长的属性值
      let long_value = "a".repeat(10001) // 超过10000字符限制
      let set_long_value_result = azimuth::Span::set_attribute(span, "long.key", azimuth::AttributeValue::StringValue(long_value))
      match set_long_value_result {
        Ok(_) => assert_true(false) // 不应该成功设置过长值属性
        Err(error) => assert_eq(error.message, "Attribute value exceeds maximum length of 10000 characters")
      }
    }
    Err(_) => assert_true(false)
  }
}

// 测试3: 日志记录错误处理
test "日志记录错误处理测试" {
  // 创建日志记录器
  let logger = azimuth::Logger::new("test_logger")
  
  // 测试记录空消息
  let empty_message_result = azimuth::Logger::log(logger, azimuth::LogLevel::Info, "")
  match empty_message_result {
    Ok(_) => assert_true(false) // 不应该成功记录空消息
    Err(error) => assert_eq(error.message, "Cannot log empty message")
  }
  
  // 测试记录过长消息
  let long_message = "a".repeat(100001) // 超过100000字符限制
  let long_message_result = azimuth::Logger::log(logger, azimuth::LogLevel::Info, long_message)
  match long_message_result {
    Ok(_) => assert_true(false) // 不应该成功记录过长消息
    Err(error) => assert_eq(error.message, "Log message exceeds maximum length of 100000 characters")
  }
  
  // 测试记录包含非法字符的消息
  let invalid_chars_message = "Test message with \x00 null character"
  let invalid_chars_result = azimuth::Logger::log(logger, azimuth::LogLevel::Info, invalid_chars_message)
  match invalid_chars_result {
    Ok(_) => assert_true(false) // 不应该成功记录包含非法字符的消息
    Err(error) => assert_eq(error.message, "Log message contains invalid characters")
  }
  
  // 测试记录有效消息
  let valid_message_result = azimuth::Logger::log(logger, azimuth::LogLevel::Info, "Valid log message")
  match valid_message_result {
    Ok(_) => assert_true(true) // 应该成功记录有效消息
    Err(_) => assert_true(false)
  }
  
  // 测试日志记录器关闭后记录日志
  azimuth::Logger::shutdown(logger)
  let after_shutdown_result = azimuth::Logger::log(logger, azimuth::LogLevel::Info, "Message after shutdown")
  match after_shutdown_result {
    Ok(_) => assert_true(false) // 不应该成功在关闭后记录日志
    Err(error) => assert_eq(error.message, "Cannot log to shutdown logger")
  }
}

// 测试4: 指标收集错误处理
test "指标收集错误处理测试" {
  // 创建指标提供者
  let provider = azimuth::MeterProvider::new()
  let meter = azimuth::MeterProvider::get_meter(provider, "test_meter")
  
  // 测试创建空名称的指标
  let empty_name_counter_result = azimuth::Meter::create_counter(meter, "", None, None)
  match empty_name_counter_result {
    Ok(_) => assert_true(false) // 不应该成功创建空名称指标
    Err(error) => assert_eq(error.message, "Cannot create instrument with empty name")
  }
  
  // 测试创建过长名称的指标
  let long_name = "a".repeat(257) // 超过256字符限制
  let long_name_counter_result = azimuth::Meter::create_counter(meter, long_name, None, None)
  match long_name_counter_result {
    Ok(_) => assert_true(false) // 不应该成功创建过长名称指标
    Err(error) => assert_eq(error.message, "Instrument name exceeds maximum length of 256 characters")
  }
  
  // 测试创建包含非法字符名称的指标
  let invalid_chars_name = "metric/name/with/invalid/chars"
  let invalid_chars_counter_result = azimuth::Meter::create_counter(meter, invalid_chars_name, None, None)
  match invalid_chars_counter_result {
    Ok(_) => assert_true(false) // 不应该成功创建包含非法字符名称的指标
    Err(error) => assert_eq(error.message, "Instrument name contains invalid characters")
  }
  
  // 测试创建有效指标
  let valid_counter_result = azimuth::Meter::create_counter(meter, "test_counter", Some("Test counter"), Some("count"))
  match valid_counter_result {
    Ok(counter) => {
      // 测试记录负值到计数器
      let negative_value_result = azimuth::Counter::add(counter, -1.0)
      match negative_value_result {
        Ok(_) => assert_true(false) // 不应该成功记录负值到计数器
        Err(error) => assert_eq(error.message, "Counter value must be non-negative")
      }
      
      // 测试记录NaN值到计数器
      let nan_value_result = azimuth::Counter::add(counter, Float::nan())
      match nan_value_result {
        Ok(_) => assert_true(false) // 不应该成功记录NaN值到计数器
        Err(error) => assert_eq(error.message, "Counter value cannot be NaN")
      }
      
      // 测试记录无穷大值到计数器
      let infinity_value_result = azimuth::Counter::add(counter, Float::infinity())
      match infinity_value_result {
        Ok(_) => assert_true(false) // 不应该成功记录无穷大值到计数器
        Err(error) => assert_eq(error.message, "Counter value cannot be infinity")
      }
      
      // 测试记录有效值到计数器
      let valid_value_result = azimuth::Counter::add(counter, 1.0)
      match valid_value_result {
        Ok(_) => assert_true(true) // 应该成功记录有效值
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试仪表操作错误处理
  let valid_gauge_result = azimuth::Meter::create_gauge(meter, "test_gauge", Some("Test gauge"), Some("value"))
  match valid_gauge_result {
    Ok(gauge) => {
      // 测试记录NaN值到仪表
      let nan_gauge_result = azimuth::Gauge::record(gauge, Float::nan())
      match nan_gauge_result {
        Ok(_) => assert_true(false) // 不应该成功记录NaN值到仪表
        Err(error) => assert_eq(error.message, "Gauge value cannot be NaN")
      }
      
      // 测试记录无穷大值到仪表
      let infinity_gauge_result = azimuth::Gauge::record(gauge, Float::infinity())
      match infinity_gauge_result {
        Ok(_) => assert_true(false) // 不应该成功记录无穷大值到仪表
        Err(error) => assert_eq(error.message, "Gauge value cannot be infinity")
      }
      
      // 测试记录有效值到仪表
      let valid_gauge_value_result = azimuth::Gauge::record(gauge, 42.0)
      match valid_gauge_value_result {
        Ok(_) => assert_true(true) // 应该成功记录有效值
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// 测试5: 序列化错误处理
test "序列化错误处理测试" {
  // 创建包含各种数据的跨度
  let span_ctx = azimuth::SpanContext::new("trace_id", "span_id", true, "")
  let span = azimuth::Span::new("test_span", azimuth::SpanKind::Internal, span_ctx)
  
  // 添加一些属性
  azimuth::Span::set_attribute(span, "string.attr", azimuth::AttributeValue::StringValue("value"))
  azimuth::Span::set_attribute(span, "int.attr", azimuth::AttributeValue::IntValue(42))
  
  // 测试序列化有效跨度
  let serialize_result = azimuth::SpanSerializer::serialize(span)
  match serialize_result {
    Ok(json) => {
      // 测试反序列化有效JSON
      let deserialize_result = azimuth::SpanSerializer::deserialize(json)
      match deserialize_result {
        Ok(deserialized_span) => {
          assert_eq(azimuth::Span::name(deserialized_span), "test_span")
        }
        Err(_) => assert_true(false)
      }
      
      // 测试反序列化无效JSON
      let invalid_json = "{ invalid json }"
      let invalid_deserialize_result = azimuth::SpanSerializer::deserialize(invalid_json)
      match invalid_deserialize_result {
        Ok(_) => assert_true(false) // 不应该成功反序列化无效JSON
        Err(error) => assert_eq(error.message, "Invalid JSON format")
      }
      
      // 测试反序列化空JSON
      let empty_json = ""
      let empty_deserialize_result = azimuth::SpanSerializer::deserialize(empty_json)
      match empty_deserialize_result {
        Ok(_) => assert_true(false) // 不应该成功反序列化空JSON
        Err(error) => assert_eq(error.message, "Empty JSON string")
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试序列化包含循环引用的对象
  let cyclic_object = azimuth::TestObject::new_with_cyclic_reference()
  let cyclic_serialize_result = azimuth::ObjectSerializer::serialize(cyclic_object)
  match cyclic_serialize_result {
    Ok(_) => assert_true(false) // 不应该成功序列化包含循环引用的对象
    Err(error) => assert_eq(error.message, "Cannot serialize object with cyclic reference")
  }
  
  // 测试序列化过大的对象
  let large_object = azimuth::TestObject::new_with_large_data(1000000) // 1MB数据
  let large_serialize_result = azimuth::ObjectSerializer::serialize(large_object)
  match large_serialize_result {
    Ok(_) => assert_true(false) // 不应该成功序列化过大对象
    Err(error) => assert_eq(error.message, "Object size exceeds serialization limit")
  }
}

// 测试6: 网络通信错误处理
test "网络通信错误处理测试" {
  // 创建HTTP客户端
  let client = azimuth::HttpClient::new()
  
  // 测试连接到无效主机
  let invalid_host_result = azimuth::HttpClient::get(client, "http://invalid-host-that-does-not-exist.com")
  match invalid_host_result {
    Ok(_) => assert_true(false) // 不应该成功连接到无效主机
    Err(error) => {
      assert_eq(error.code, azimuth::HttpError::ConnectionError)
      assert_true(error.message.contains("Failed to connect"))
    }
  }
  
  // 测试连接到无效端口
  let invalid_port_result = azimuth::HttpClient::get(client, "http://localhost:99999") // 无效端口
  match invalid_port_result {
    Ok(_) => assert_true(false) // 不应该成功连接到无效端口
    Err(error) => {
      assert_eq(error.code, azimuth::HttpError::ConnectionError)
      assert_true(error.message.contains("Failed to connect"))
    }
  }
  
  // 测试超时处理
  let timeout_client = azimuth::HttpClient::new_with_timeout(100) // 100ms超时
  let slow_server_result = azimuth::HttpClient::get(timeout_client, "http://httpbin.org/delay/5") // 5秒延迟
  match slow_server_result {
    Ok(_) => assert_true(false) // 不应该成功在超时前获取响应
    Err(error) => {
      assert_eq(error.code, azimuth::HttpError::Timeout)
      assert_true(error.message.contains("Request timed out"))
    }
  }
  
  // 测试处理无效URL
  let invalid_url_result = azimuth::HttpClient::get(client, "not-a-valid-url")
  match invalid_url_result {
    Ok(_) => assert_true(false) // 不应该成功处理无效URL
    Err(error) => {
      assert_eq(error.code, azimuth::HttpError::InvalidUrl)
      assert_true(error.message.contains("Invalid URL format"))
    }
  }
  
  // 测试处理过大响应
  let large_response_result = azimuth::HttpClient::get(client, "http://httpbin.org/bytes/10485760") // 10MB响应
  match large_response_result {
    Ok(_) => assert_true(false) // 不应该成功处理过大响应
    Err(error) => {
      assert_eq(error.code, azimuth::HttpError::ResponseTooLarge)
      assert_true(error.message.contains("Response size exceeds limit"))
    }
  }
}

// 测试7: 文件操作错误处理
test "文件操作错误处理测试" {
  // 测试读取不存在的文件
  let non_existent_file_result = azimuth::FileOperations::read_to_string("/path/to/non/existent/file.txt")
  match non_existent_file_result {
    Ok(_) => assert_true(false) // 不应该成功读取不存在的文件
    Err(error) => {
      assert_eq(error.code, azimuth::FileError::NotFound)
      assert_true(error.message.contains("File not found"))
    }
  }
  
  // 测试写入只读目录
  let readonly_dir_result = azimuth::FileOperations::write_string("/readonly/directory/file.txt", "test content")
  match readonly_dir_result {
    Ok(_) => assert_true(false) // 不应该成功写入只读目录
    Err(error) => {
      assert_eq(error.code, azimuth::FileError::PermissionDenied)
      assert_true(error.message.contains("Permission denied"))
    }
  }
  
  // 测试创建过深的目录结构
  let mut deep_path = "/tmp"
  for _ in 0..100 {
    deep_path = deep_path + "/very/deep/nested/directory"
  }
  let deep_path_result = azimuth::FileOperations::create_dir_all(deep_path)
  match deep_path_result {
    Ok(_) => assert_true(false) // 不应该成功创建过深的目录结构
    Err(error) => {
      assert_eq(error.code, azimuth::FileError::PathTooLong)
      assert_true(error.message.contains("Path too long"))
    }
  }
  
  // 测试写入过大文件
  let large_content = "a".repeat(104857601) // 100MB+1字节，超过100MB限制
  let large_file_result = azimuth::FileOperations::write_string("/tmp/large_file.txt", large_content)
  match large_file_result {
    Ok(_) => assert_true(false) // 不应该成功写入过大文件
    Err(error) => {
      assert_eq(error.code, azimuth::FileError::FileTooLarge)
      assert_true(error.message.contains("File size exceeds limit"))
    }
  }
  
  // 测试删除不存在的文件
  let delete_non_existent_result = azimuth::FileOperations::remove_file("/path/to/non/existent/file.txt")
  match delete_non_existent_result {
    Ok(_) => assert_true(false) // 不应该成功删除不存在的文件
    Err(error) => {
      assert_eq(error.code, azimuth::FileError::NotFound)
      assert_true(error.message.contains("File not found"))
    }
  }
}

// 测试8: 内存管理错误处理
test "内存管理错误处理测试" {
  // 测试分配过大内存块
  let large_allocation_result = azimuth::MemoryManager::allocate(1073741824) // 1GB
  match large_allocation_result {
    Ok(_) => assert_true(false) // 不应该成功分配过大内存块
    Err(error) => {
      assert_eq(error.code, azimuth::MemoryError::AllocationTooLarge)
      assert_true(error.message.contains("Allocation size exceeds limit"))
    }
  }
  
  // 测试释放未分配的内存
  let unallocated_ptr = 0x12345 as azimuth::RawPtr
  let free_unallocated_result = azimuth::MemoryManager::free(unallocated_ptr)
  match free_unallocated_result {
    Ok(_) => assert_true(false) // 不应该成功释放未分配的内存
    Err(error) => {
      assert_eq(error.code, azimuth::MemoryError::InvalidPointer)
      assert_true(error.message.contains("Invalid memory pointer"))
    }
  }
  
  // 测试释放已释放的内存
  let allocation_result = azimuth::MemoryManager::allocate(1024) // 1KB
  match allocation_result {
    Ok(ptr) => {
      let first_free_result = azimuth::MemoryManager::free(ptr)
      match first_free_result {
        Ok(_) => assert_true(true), // 应该成功释放
        Err(_) => assert_true(false)
      }
      
      let second_free_result = azimuth::MemoryManager::free(ptr)
      match second_free_result {
        Ok(_) => assert_true(false) // 不应该成功释放已释放的内存
        Err(error) => {
          assert_eq(error.code, azimuth::MemoryError::DoubleFree)
          assert_true(error.message.contains("Double free detected"))
        }
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试内存越界访问
  let small_allocation_result = azimuth::MemoryManager::allocate(10) // 10字节
  match small_allocation_result {
    Ok(ptr) => {
      let out_of_bounds_result = azimuth::MemoryManager::write_byte(ptr, 15, 42) // 越界访问
      match out_of_bounds_result {
        Ok(_) => assert_true(false) // 不应该成功进行越界访问
        Err(error) => {
          assert_eq(error.code, azimuth::MemoryError::OutOfBounds)
          assert_true(error.message.contains("Out of bounds memory access"))
        }
      }
      
      // 正常释放
      let free_result = azimuth::MemoryManager::free(ptr)
      match free_result {
        Ok(_) => assert_true(true), // 应该成功释放
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

// 测试9: 数据验证错误处理
test "数据验证错误处理测试" {
  // 测试验证无效追踪ID
  let invalid_trace_ids = [
    "", // 空字符串
    "short", // 太短
    "g" * 33, // 太长
    "invalid_characters_123", // 包含非法字符
    "1234567890123456789012345678901g" // 包含非十六进制字符
  ]
  
  for invalid_id in invalid_trace_ids {
    let validation_result = azimuth::DataValidator::validate_trace_id(invalid_id)
    match validation_result {
      Ok(_) => assert_true(false), // 不应该成功验证无效追踪ID
      Err(error) => assert_true(error.message.contains("Invalid trace ID"))
    }
  }
  
  // 测试验证有效追踪ID
  let valid_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let valid_validation_result = azimuth::DataValidator::validate_trace_id(valid_trace_id)
  match valid_validation_result {
    Ok(_) => assert_true(true), // 应该成功验证有效追踪ID
    Err(_) => assert_true(false)
  }
  
  // 测试验证无效跨度ID
  let invalid_span_ids = [
    "", // 空字符串
    "short", // 太短
    "g" * 17, // 太长
    "invalid_characters", // 包含非法字符
    "1234567890123456g" // 包含非十六进制字符
  ]
  
  for invalid_id in invalid_span_ids {
    let validation_result = azimuth::DataValidator::validate_span_id(invalid_id)
    match validation_result {
      Ok(_) => assert_true(false), // 不应该成功验证无效跨度ID
      Err(error) => assert_true(error.message.contains("Invalid span ID"))
    }
  }
  
  // 测试验证有效跨度ID
  let valid_span_id = "00f067aa0ba902b7"
  let valid_span_validation_result = azimuth::DataValidator::validate_span_id(valid_span_id)
  match valid_span_validation_result {
    Ok(_) => assert_true(true), // 应该成功验证有效跨度ID
    Err(_) => assert_true(false)
  }
  
  // 测试验证无效属性键
  let invalid_attr_keys = [
    "", // 空字符串
    "a" * 257, // 太长
    "invalid.key.with spaces", // 包含空格
    "invalid/key/with/slashes", // 包含斜杠
    "invalid.key.with\nnewline", // 包含换行符
    "invalid.key.with\ttab" // 包含制表符
  ]
  
  for invalid_key in invalid_attr_keys {
    let validation_result = azimuth::DataValidator::validate_attribute_key(invalid_key)
    match validation_result {
      Ok(_) => assert_true(false), // 不应该成功验证无效属性键
      Err(error) => assert_true(error.message.contains("Invalid attribute key"))
    }
  }
  
  // 测试验证有效属性键
  let valid_attr_keys = [
    "valid.key",
    "valid_key_with_underscores",
    "valid.key.with.numbers123",
    "key.with.dots.and.underscores"
  ]
  
  for valid_key in valid_attr_keys {
    let validation_result = azimuth::DataValidator::validate_attribute_key(valid_key)
    match validation_result {
      Ok(_) => assert_true(true), // 应该成功验证有效属性键
      Err(_) => assert_true(false)
    }
  }
}

// 测试10: 资源限制错误处理
test "资源限制错误处理测试" {
  // 创建资源限制管理器
  let resource_limiter = azimuth::ResourceLimiter::new()
  
  // 设置资源限制
  resource_limiter = azimuth::ResourceLimiter::set_max_spans(resource_limiter, 100)
  resource_limiter = azimuth::ResourceLimiter::set_max_attributes(resource_limiter, 1000)
  resource_limiter = azimuth::ResourceLimiter::set_max_events(resource_limiter, 500)
  resource_limiter = azimuth::ResourceLimiter::set_max_links(resource_limiter, 50)
  
  // 测试超过跨度限制
  let mut spans = []
  for i in 0..101 { // 超过100个跨度限制
    let span_ctx = azimuth::SpanContext::new("trace_id", "span_id_" + i.to_string(), true, "")
    let span_result = azimuth::ResourceLimiter::create_span(resource_limiter, "test_span", span_ctx)
    
    if i < 100 {
      match span_result {
        Ok(span) => spans = spans + [span],
        Err(_) => assert_true(false)
      }
    } else {
      match span_result {
        Ok(_) => assert_true(false), // 不应该成功创建超过限制的跨度
        Err(error) => {
          assert_eq(error.code, azimuth::ResourceError::SpanLimitExceeded)
          assert_true(error.message.contains("Span limit exceeded"))
        }
      }
    }
  }
  
  // 测试超过属性限制
  let span = spans[0]
  for i in 0..1001 { // 超过1000个属性限制
    let key = "attr_" + i.to_string()
    let value = azimuth::AttributeValue::StringValue("value_" + i.to_string())
    
    if i < 1000 {
      let set_result = azimuth::ResourceLimiter::set_attribute(resource_limiter, span, key, value)
      match set_result {
        Ok(_) => assert_true(true),
        Err(_) => assert_true(false)
      }
    } else {
      let set_result = azimuth::ResourceLimiter::set_attribute(resource_limiter, span, key, value)
      match set_result {
        Ok(_) => assert_true(false), // 不应该成功设置超过限制的属性
        Err(error) => {
          assert_eq(error.code, azimuth::ResourceError::AttributeLimitExceeded)
          assert_true(error.message.contains("Attribute limit exceeded"))
        }
      }
    }
  }
  
  // 测试超过事件限制
  for i in 0..501 { // 超过500个事件限制
    let event_name = "event_" + i.to_string()
    
    if i < 500 {
      let add_event_result = azimuth::ResourceLimiter::add_event(resource_limiter, span, event_name, None)
      match add_event_result {
        Ok(_) => assert_true(true),
        Err(_) => assert_true(false)
      }
    } else {
      let add_event_result = azimuth::ResourceLimiter::add_event(resource_limiter, span, event_name, None)
      match add_event_result {
        Ok(_) => assert_true(false), // 不应该成功添加超过限制的事件
        Err(error) => {
          assert_eq(error.code, azimuth::ResourceError::EventLimitExceeded)
          assert_true(error.message.contains("Event limit exceeded"))
        }
      }
    }
  }
  
  // 测试超过链接限制
  for i in 0..51 { // 超过50个链接限制
    let link_ctx = azimuth::SpanContext::new("link_trace_" + i.to_string(), "link_span_" + i.to_string(), true, "")
    
    if i < 50 {
      let add_link_result = azimuth::ResourceLimiter::add_link(resource_limiter, span, link_ctx)
      match add_link_result {
        Ok(_) => assert_true(true),
        Err(_) => assert_true(false)
      }
    } else {
      let add_link_result = azimuth::ResourceLimiter::add_link(resource_limiter, span, link_ctx)
      match add_link_result {
        Ok(_) => assert_true(false), // 不应该成功添加超过限制的链接
        Err(error) => {
          assert_eq(error.code, azimuth::ResourceError::LinkLimitExceeded)
          assert_true(error.message.contains("Link limit exceeded"))
        }
      }
    }
  }
}