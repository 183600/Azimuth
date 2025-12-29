// 日志记录测试用例
// 测试日志记录、严重性级别和LogRecordBuilder功能

test "log_severity_levels" {
  // 测试日志严重性级别
  
  let trace_level = Trace
  let debug_level = Debug
  let info_level = Info
  let warn_level = Warn
  let error_level = Error
  let fatal_level = Fatal
  
  // 创建不同严重性级别的日志记录
  let trace_log = LogRecord{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: trace_level,
    severity_text: Some("TRACE"),
    body: Some("Trace level message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let debug_log = LogRecord{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: debug_level,
    severity_text: Some("DEBUG"),
    body: Some("Debug level message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  let info_log = LogRecord{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: info_level,
    severity_text: Some("INFO"),
    body: Some("Info level message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证严重性级别
  match trace_log.severity_number {
    Trace => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  match debug_log.severity_number {
    Debug => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  match info_log.severity_number {
    Info => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  // 验证严重性文本
  match trace_log.severity_text {
    Some(text) => assert_eq(text, "TRACE")
    None => assert_eq(false, true)
  }
  
  match debug_log.severity_text {
    Some(text) => assert_eq(text, "DEBUG")
    None => assert_eq(false, true)
  }
  
  match info_log.severity_text {
    Some(text) => assert_eq(text, "INFO")
    None => assert_eq(false, true)
  }
}

test "log_record_builder_basic" {
  // 测试LogRecordBuilder基本功能
  
  let builder = LogRecord::builder()
  
  // 构建基本日志记录
  let log_record = builder
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Test log message")
    .build()
  
  // 验证基本字段
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => assert_eq(false, true)
  }
  
  match log_record.body {
    Some(message) => {
      assert_eq(message, "Test log message")
      assert_eq(message.length(), 16)
    }
    None => assert_eq(false, true)
  }
  
  // 验证默认值
  assert_eq(log_record.attributes.length(), 0)
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  assert_eq(log_record.resource, None)
  assert_eq(log_record.instrumentation_scope, None)
}

test "log_record_builder_with_attributes" {
  // 测试LogRecordBuilder带属性功能
  
  let builder = LogRecord::builder()
  
  // 添加属性
  let log_record = builder
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Error occurred")
    .with_attribute("error.code", common::AttributeValue::int(500L))
    .with_attribute("error.message", common::AttributeValue::string("Internal server error"))
    .with_attribute("http.method", common::AttributeValue::string("POST"))
    .with_attribute("http.status_code", common::AttributeValue::int(500L))
    .build()
  
  // 验证属性
  assert_eq(log_record.attributes.length(), 4)
  
  // 验证错误代码属性
  let mut found_error_code = false
  let mut found_error_message = false
  let mut found_http_method = false
  let mut found_http_status = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "error.code" => {
        found_error_code = true
        match value {
          common::IntValue(code) => assert_eq(code, 500L)
          _ => assert_eq(false, true)
        }
      }
      "error.message" => {
        found_error_message = true
        match value {
          common::StringValue(message) => assert_eq(message, "Internal server error")
          _ => assert_eq(false, true)
        }
      }
      "http.method" => {
        found_http_method = true
        match value {
          common::StringValue(method) => assert_eq(method, "POST")
          _ => assert_eq(false, true)
        }
      }
      "http.status_code" => {
        found_http_status = true
        match value {
          common::IntValue(status) => assert_eq(status, 500L)
          _ => assert_eq(false, true)
        }
      }
      _ => assert_eq(false, true)
    }
    
    i = i + 1
  }
  
  assert_eq(found_error_code, true, "error.code attribute not found")
  assert_eq(found_error_message, true, "error.message attribute not found")
  assert_eq(found_http_method, true, "http.method attribute not found")
  assert_eq(found_http_status, true, "http.status_code attribute not found")
}

test "log_record_complex_attributes" {
  // 测试复杂属性值类型
  
  let builder = LogRecord::builder()
  
  // 添加各种类型的属性
  let log_record = builder
    .timestamp(1640995200000000000L)
    .severity(Warn)
    .body("Warning with complex attributes")
    .with_attribute("string.attr", common::AttributeValue::string("string_value"))
    .with_attribute("int.attr", common::AttributeValue::int(12345L))
    .with_attribute("float.attr", common::AttributeValue::float(123.456))
    .with_attribute("bool.attr", common::AttributeValue::bool(true))
    .with_attribute("array.string", common::AttributeValue::array_string(["item1", "item2", "item3"]))
    .with_attribute("array.int", common::AttributeValue::array_int([1L, 2L, 3L, 4L, 5L]))
    .with_attribute("array.float", common::AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("array.bool", common::AttributeValue::array_bool([true, false, true]))
    .build()
  
  // 验证属性数量
  assert_eq(log_record.attributes.length(), 8)
  
  // 验证各种属性类型
  let mut string_found = false
  let mut int_found = false
  let mut float_found = false
  let mut bool_found = false
  let mut array_string_found = false
  let mut array_int_found = false
  let mut array_float_found = false
  let mut array_bool_found = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    
    match key {
      "string.attr" => {
        string_found = true
        match value {
          common::StringValue(s) => assert_eq(s, "string_value")
          _ => assert_eq(false, true)
        }
      }
      "int.attr" => {
        int_found = true
        match value {
          common::IntValue(n) => assert_eq(n, 12345L)
          _ => assert_eq(false, true)
        }
      }
      "float.attr" => {
        float_found = true
        match value {
          common::FloatValue(f) => assert_eq(f - 123.456 < 0.001, true)
          _ => assert_eq(false, true)
        }
      }
      "bool.attr" => {
        bool_found = true
        match value {
          common::BoolValue(b) => assert_eq(b, true)
          _ => assert_eq(false, true)
        }
      }
      "array.string" => {
        array_string_found = true
        match value {
          common::ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], "item1")
            assert_eq(arr[1], "item2")
            assert_eq(arr[2], "item3")
          }
          _ => assert_eq(false, true)
        }
      }
      "array.int" => {
        array_int_found = true
        match value {
          common::ArrayIntValue(arr) => {
            assert_eq(arr.length(), 5)
            assert_eq(arr[0], 1L)
            assert_eq(arr[4], 5L)
          }
          _ => assert_eq(false, true)
        }
      }
      "array.float" => {
        array_float_found = true
        match value {
          common::ArrayFloatValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0] - 1.1 < 0.001, true)
            assert_eq(arr[2] - 3.3 < 0.001, true)
          }
          _ => assert_eq(false, true)
        }
      }
      "array.bool" => {
        array_bool_found = true
        match value {
          common::ArrayBoolValue(arr) => {
            assert_eq(arr.length(), 3)
            assert_eq(arr[0], true)
            assert_eq(arr[1], false)
            assert_eq(arr[2], true)
          }
          _ => assert_eq(false, true)
        }
      }
      _ => assert_eq(false, true)
    }
    
    i = i + 1
  }
  
  assert_eq(string_found, true)
  assert_eq(int_found, true)
  assert_eq(float_found, true)
  assert_eq(bool_found, true)
  assert_eq(array_string_found, true)
  assert_eq(array_int_found, true)
  assert_eq(array_float_found, true)
  assert_eq(array_bool_found, true)
}

test "log_record_timestamps" {
  // 测试日志记录时间戳
  
  let timestamp = 1640995200000000000L  // 2022-01-01 00:00:00 UTC
  let observed_timestamp = 1640995200000000500L  // 稍晚的观察时间
  
  let log_record = LogRecord::builder()
    .timestamp(timestamp)
    .severity(Info)
    .body("Timestamp test")
    .build()
  
  // 创建带观察时间戳的日志记录
  let log_with_observed = LogRecord{
    timestamp_unix_nanos: timestamp,
    observed_timestamp_unix_nanos: Some(observed_timestamp),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Timestamp test with observed time"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证时间戳
  assert_eq(log_record.timestamp_unix_nanos, timestamp)
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  
  assert_eq(log_with_observed.timestamp_unix_nanos, timestamp)
  match log_with_observed.observed_timestamp_unix_nanos {
    Some(observed) => {
      assert_eq(observed, observed_timestamp)
      assert_eq(observed > timestamp, true)
    }
    None => assert_eq(false, true)
  }
  
  // 验证时间戳格式（纳秒精度）
  let timestamp_seconds = timestamp / 1000000000L
  let timestamp_nanos = timestamp % 1000000000L
  
  assert_eq(timestamp_seconds, 1640995200L)
  assert_eq(timestamp_nanos, 0L)
}

test "log_record_trace_context" {
  // 测试日志记录的追踪上下文
  
  let trace_id_bytes = [0x0a, 0xf7, 0x65, 0x19, 0x16, 0xcd, 0x43, 0xdd, 0x84, 0x48, 0xeb, 0x21, 0x1c, 0x80, 0x31, 0x9c]
  let span_id_bytes = [0xb7, 0xad, 0x6b, 0x71, 0x69, 0x20, 0x33, 0x31]
  let trace_flags = 0x01
  
  let log_record = LogRecord{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Log with trace context"),
    attributes: [],
    trace_id: Some(trace_id_bytes),
    span_id: Some(span_id_bytes),
    trace_flags: Some(trace_flags),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证追踪上下文
  match log_record.trace_id {
    Some(trace_id) => {
      assert_eq(trace_id.length(), 16)
      assert_eq(trace_id[0], 0x0a)
      assert_eq(trace_id[15], 0x9c)
    }
    None => assert_eq(false, true)
  }
  
  match log_record.span_id {
    Some(span_id) => {
      assert_eq(span_id.length(), 8)
      assert_eq(span_id[0], 0xb7)
      assert_eq(span_id[7], 0x31)
    }
    None => assert_eq(false, true)
  }
  
  match log_record.trace_flags {
    Some(flags) => {
      assert_eq(flags, 0x01)
    }
    None => assert_eq(false, true)
  }
}

test "noop_logger_functionality" {
  // 测试NoopLogger功能
  
  let logger = NoopLogger{}
  let provider = NoopLoggerProvider{}
  
  // 测试获取logger
  let test_logger = provider.get_logger("test-logger", Some("1.0.0"), Some("http://example.com/schema"))
  
  // 测试emit方法（no-op，应该不会出错）
  let log_record = LogRecord{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Test message"),
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  // 这些调用应该执行成功但无效果
  test_logger.emit(log_record)
  test_logger.debug("Debug message", [])
  test_logger.info("Info message", [])
  test_logger.warn("Warn message", [])
  test_logger.error("Error message", [])
  test_logger.fatal("Fatal message", [])
  
  // 验证所有调用都能成功执行（无异常）
  assert_eq(true, true)  // 如果到达这里说明所有调用都成功了
}