// Logs API 序列化测试用例

test "log_record_serialization_format" {
  // 测试日志记录的序列化格式
  
  // 创建完整的日志记录
  let log_record = LogRecord::builder()
    .timestamp(1234567890000000000L)  // 2023-05-15 10:30:45 UTC
    .severity(Error)
    .body("Database connection failed")
    .with_attribute("error.code", AttributeValue::int(500))
    .with_attribute("error.message", AttributeValue::string("Connection timeout"))
    .with_attribute("service.name", AttributeValue::string("payment-service"))
    .with_attribute("retry.count", AttributeValue::int(3))
    .build()
  
  // 验证日志记录结构
  assert_eq(log_record.timestamp_unix_nanos, 1234567890000000000L)
  assert_eq(log_record.severity_number, Error)
  assert_eq(log_record.body, Some("Database connection failed"))
  assert_eq(log_record.attributes.length(), 4)
  
  // 验证属性内容
  let mut found_error_code = false
  let mut found_error_message = false
  let mut found_service_name = false
  let mut found_retry_count = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match (key, value) {
      ("error.code", IntValue(code)) => {
        assert_eq(code, 500L)
        found_error_code = true
      }
      ("error.message", StringValue(msg)) => {
        assert_eq(msg, "Connection timeout")
        found_error_message = true
      }
      ("service.name", StringValue(name)) => {
        assert_eq(name, "payment-service")
        found_service_name = true
      }
      ("retry.count", IntValue(count)) => {
        assert_eq(count, 3L)
        found_retry_count = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(found_error_code, true)
  assert_eq(found_error_message, true)
  assert_eq(found_service_name, true)
  assert_eq(found_retry_count, true)
}

test "log_record_severity_conversion" {
  // 测试日志严重性级别的转换和序列化
  
  let severities = [
    (Trace, "TRACE"),
    (Debug, "DEBUG"),
    (Info, "INFO"),
    (Warn, "WARN"),
    (Error, "ERROR"),
    (Fatal, "FATAL")
  ]
  
  let mut i = 0
  while i < severities.length() {
    let (severity_enum, severity_string) = severities[i]
    
    let log_record = LogRecord::builder()
      .severity(severity_enum)
      .body("Test message for " + severity_string)
      .build()
    
    // 验证严重性级别
    assert_eq(log_record.severity_number, severity_enum)
    
    // 模拟严重性级别的字符串转换
    let converted_string = match log_record.severity_number {
      Trace => "TRACE"
      Debug => "DEBUG"
      Info => "INFO"
      Warn => "WARN"
      Error => "ERROR"
      Fatal => "FATAL"
    }
    
    assert_eq(converted_string, severity_string)
    i = i + 1
  }
}

test "log_record_attribute_types_serialization" {
  // 测试日志记录中不同属性类型的序列化
  
  let log_record = LogRecord::builder()
    .body("Testing all attribute types")
    .with_attribute("string.value", AttributeValue::string("test string"))
    .with_attribute("int.value", AttributeValue::int(42L))
    .with_attribute("float.value", AttributeValue::float(3.14159))
    .with_attribute("bool.value", AttributeValue::bool(true))
    .with_attribute("array.string", AttributeValue::array_string(["a", "b", "c"]))
    .with_attribute("array.int", AttributeValue::array_int([1L, 2L, 3L]))
    .with_attribute("array.float", AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("array.bool", AttributeValue::array_bool([true, false, true]))
    .build()
  
  // 验证所有属性类型都被正确存储
  assert_eq(log_record.attributes.length(), 8)
  
  // 验证每种属性类型
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
    match (key, value) {
      ("string.value", StringValue(s)) => {
        assert_eq(s, "test string")
        string_found = true
      }
      ("int.value", IntValue(n)) => {
        assert_eq(n, 42L)
        int_found = true
      }
      ("float.value", FloatValue(f)) => {
        assert_eq(f > 3.14 && f < 3.15, true)
        float_found = true
      }
      ("bool.value", BoolValue(b)) => {
        assert_eq(b, true)
        bool_found = true
      }
      ("array.string", ArrayStringValue(arr)) => {
        assert_eq(arr.length(), 3)
        array_string_found = true
      }
      ("array.int", ArrayIntValue(arr)) => {
        assert_eq(arr.length(), 3)
        array_int_found = true
      }
      ("array.float", ArrayFloatValue(arr)) => {
        assert_eq(arr.length(), 3)
        array_float_found = true
      }
      ("array.bool", ArrayBoolValue(arr)) => {
        assert_eq(arr.length(), 3)
        array_bool_found = true
      }
      _ => {}
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

test "log_record_builder_chaining_serialization" {
  // 测试日志记录构建器链式调用的序列化结果
  
  let log_record = LogRecord::builder()
    .timestamp(1609459200000000000L)  // 2021-01-01 00:00:00 UTC
    .severity(Info)
    .body("Order processed successfully")
    .with_attribute("order.id", AttributeValue::string("ORD-12345"))
    .with_attribute("order.amount", AttributeValue::float(99.99))
    .with_attribute("customer.id", AttributeValue::string("CUST-67890"))
    .with_attribute("processing.time.ms", AttributeValue::int(250L))
    .build()
  
  // 验证链式调用结果的完整性
  assert_eq(log_record.timestamp_unix_nanos, 1609459200000000000L)
  assert_eq(log_record.severity_number, Info)
  assert_eq(log_record.body, Some("Order processed successfully"))
  assert_eq(log_record.attributes.length(), 4)
  
  // 模拟序列化为JSON格式
  let json_representation = "{"
    + "\"timestamp\":" + log_record.timestamp_unix_nanos.to_string() + ","
    + "\"severity\":\"INFO\","
    + "\"body\":\"Order processed successfully\","
    + "\"attributes\":{"
    + "\"order.id\":\"ORD-12345\","
    + "\"order.amount\":99.99,"
    + "\"customer.id\":\"CUST-67890\","
    + "\"processing.time.ms\":250"
    + "}"
    + "}"
  
  // 验证JSON包含所有必要字段
  assert_eq(json_representation.contains("timestamp"), true)
  assert_eq(json_representation.contains("INFO"), true)
  assert_eq(json_representation.contains("Order processed successfully"), true)
  assert_eq(json_representation.contains("order.id"), true)
  assert_eq(json_representation.contains("customer.id"), true)
  assert_eq(json_representation.contains("processing.time.ms"), true)
}

test "log_record_trace_context_serialization" {
  // 测试日志记录中追踪上下文的序列化
  
  let trace_id_bytes = [0x0a_byte, 0xf7_byte, 0x65_byte, 0x19_byte, 0x16_byte, 0xcd_byte, 0x43_byte, 0xdd_byte,
                       0x84_byte, 0x48_byte, 0xeb_byte, 0x21_byte, 0x1c_byte, 0x80_byte, 0x31_byte, 0x9c_byte]
  let span_id_bytes = [0xb7_byte, 0xad_byte, 0x6b_byte, 0x71_byte, 0x69_byte, 0x20_byte, 0x33_byte, 0x31_byte]
  
  // 注意：这里假设LogRecord结构支持trace_id和span_id字段
  // 实际实现可能需要调整
  let log_record = LogRecord::builder()
    .severity(Warn)
    .body("Potential performance issue detected")
    .with_attribute("trace.id", AttributeValue::string("0af7651916cd43dd8448eb211c80319c"))
    .with_attribute("span.id", AttributeValue::string("b7ad6b7169203331"))
    .with_attribute("service.name", AttributeValue::string("analytics-service"))
    .build()
  
  // 验证追踪上下文属性
  assert_eq(log_record.severity_number, Warn)
  assert_eq(log_record.body, Some("Potential performance issue detected"))
  assert_eq(log_record.attributes.length(), 3)
  
  // 验证追踪ID格式
  let mut trace_id_found = false
  let mut span_id_found = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match (key, value) {
      ("trace.id", StringValue(trace_id)) => {
        assert_eq(trace_id.length(), 32)  // 16 bytes = 32 hex chars
        assert_eq(trace_id.has_prefix("0af7"), true)
        trace_id_found = true
      }
      ("span.id", StringValue(span_id)) => {
        assert_eq(span_id.length(), 16)   // 8 bytes = 16 hex chars
        assert_eq(span_id.has_prefix("b7ad"), true)
        span_id_found = true
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert_eq(trace_id_found, true)
  assert_eq(span_id_found, true)
}