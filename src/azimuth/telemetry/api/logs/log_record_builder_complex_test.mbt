// Logs模块LogRecordBuilder复杂场景测试用例
// 测试LogRecordBuilder的复杂使用场景

test "log_record_builder_with_complex_attributes" {
  // 测试LogRecordBuilder与复杂属性构建
  
  // 创建带有多种类型属性的LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(Error)
    .body("Database connection failed")
    .with_attribute("error.code", AttributeValue::int(5001L))
    .with_attribute("error.type", AttributeValue::string("ConnectionTimeout"))
    .with_attribute("retry.count", AttributeValue::int(3L))
    .with_attribute("connection.timeout_ms", AttributeValue::float(5000.0))
    .with_attribute("is.critical", AttributeValue::bool(true))
    .with_attribute("affected.tables", AttributeValue::array_string(["users", "orders", "payments"]))
    .with_attribute("error.codes", AttributeValue::array_int([5001L, 5002L, 5003L]))
    .with_attribute("response.times", AttributeValue::array_float([1.2, 2.5, 3.1]))
    .with_attribute("retry.success", AttributeValue::array_bool([false, false, true]))
    .build()
  
  // 验证基本属性
  assert_eq(log_record.timestamp_unix_nanos, 1640995200123456789L)
  assert_eq(log_record.body, Some("Database connection failed"))
  
  match log_record.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  
  // 验证复杂属性
  assert_eq(log_record.attributes.length(), 9)
  
  // 验证字符串属性
  let mut found_error_type = false
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    if key == "error.type" {
      match value {
        AttributeValue::StringValue(error_type) => {
          assert_eq(error_type, "ConnectionTimeout")
          found_error_type = true
        }
        _ => @test.fail("Expected StringValue for error.type")
      }
    }
    i = i + 1
  }
  assert_eq(found_error_type, true)
  
  // 验证数组属性
  let mut found_affected_tables = false
  i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    if key == "affected.tables" {
      match value {
        AttributeValue::ArrayStringValue(tables) => {
          assert_eq(tables.length(), 3)
          assert_eq(tables[0], "users")
          assert_eq(tables[1], "orders")
          assert_eq(tables[2], "payments")
          found_affected_tables = true
        }
        _ => @test.fail("Expected ArrayStringValue for affected.tables")
      }
    }
    i = i + 1
  }
  assert_eq(found_affected_tables, true)
  
  // 验证数值属性
  let mut found_timeout = false
  i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    if key == "connection.timeout_ms" {
      match value {
        AttributeValue::FloatValue(timeout) => {
          assert_eq(timeout, 5000.0)
          found_timeout = true
        }
        _ => @test.fail("Expected FloatValue for connection.timeout_ms")
      }
    }
    i = i + 1
  }
  assert_eq(found_timeout, true)
}

test "log_record_builder_with_trace_context" {
  // 测试LogRecordBuilder与追踪上下文
  
  // 创建带有追踪上下文的LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(Warn)
    .body("Request processing is taking longer than expected")
    .with_attribute("http.method", AttributeValue::string("POST"))
    .with_attribute("http.url", AttributeValue::string("/api/v1/process"))
    .with_attribute("http.status_code", AttributeValue::int(200L))
    .with_attribute("duration_ms", AttributeValue::float(2500.0))
    .build()
  
  // 手动设置追踪上下文（在实际实现中应该由Context提供）
  let log_record_with_trace = LogRecord::{
    ..log_record,
    trace_id: Some([10_byte, 247_byte, 101_byte, 25_byte, 22_byte, 108_byte, 67_byte, 221_byte,
                   132_byte, 72_byte, 235_byte, 33_byte, 200_byte, 3_byte, 25_byte, 156_byte]),
    span_id: Some([183_byte, 173_byte, 107_byte, 113_byte, 105_byte, 32_byte, 51_byte, 49_byte]),
    trace_flags: Some(1_byte)
  }
  
  // 验证追踪上下文
  match log_record_with_trace.trace_id {
    Some(trace_id) => {
      assert_eq(trace_id.length(), 16)
      assert_eq(trace_id[0], 10_byte)   // 0x0a
      assert_eq(trace_id[15], 156_byte) // 0x9c
    }
    None => @test.fail("Expected trace_id")
  }
  
  match log_record_with_trace.span_id {
    Some(span_id) => {
      assert_eq(span_id.length(), 8)
      assert_eq(span_id[0], 183_byte)   // 0xb7
      assert_eq(span_id[7], 49_byte)    // 0x31
    }
    None => @test.fail("Expected span_id")
  }
  
  match log_record_with_trace.trace_flags {
    Some(flags) => assert_eq(flags, 1_byte)
    None => @test.fail("Expected trace_flags")
  }
  
  // 验证日志内容
  assert_eq(log_record_with_trace.body, Some("Request processing is taking longer than expected"))
  
  match log_record_with_trace.severity_number {
    Warn => assert_eq(true, true)
    _ => @test.fail("Expected Warn severity")
  }
}

test "log_record_builder_with_resource_and_instrumentation" {
  // 测试LogRecordBuilder与资源和InstrumentationScope
  
  // 创建资源
  let resource = Resource::default("payment-service")
  let resource_with_attrs = Resource::{
    ..resource,
    service_version: Some("2.1.0"),
    attributes: [
      ("service.namespace", AttributeValue::string("ecommerce")),
      ("service.instance.id", AttributeValue::string("payment-pod-123")),
      ("deployment.environment", AttributeValue::string("production"))
    ]
  }
  
  // 创建InstrumentationScope（模拟）
  let instrumentation_scope = InstrumentationScope::{
    name: "payment-processor",
    version: Some("1.5.2"),
    schema_url: Some("https://opentelemetry.io/schemas/v1.20.0")
  }
  
  // 创建带有资源和InstrumentationScope的LogRecord
  let log_record = LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(Fatal)
    .body("Payment processing failed - system cannot recover")
    .with_attribute("payment.id", AttributeValue::string("pay_1234567890"))
    .with_attribute("payment.amount", AttributeValue::float(99.99))
    .with_attribute("payment.currency", AttributeValue::string("USD"))
    .with_attribute("error.code", AttributeValue::int(9001L))
    .with_attribute("error.message", AttributeValue::string("Insufficient funds"))
    .build()
  
  // 设置资源和InstrumentationScope
  let complete_log_record = LogRecord::{
    ..log_record,
    resource: Some(resource_with_attrs),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证资源信息
  match complete_log_record.resource {
    Some(resource) => {
      assert_eq(resource.service_name, "payment-service")
      match resource.service_version {
        Some(version) => assert_eq(version, "2.1.0")
        None => @test.fail("Expected service version")
      }
      assert_eq(resource.attributes.length(), 3)
    }
    None => @test.fail("Expected resource")
  }
  
  // 验证InstrumentationScope
  match complete_log_record.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "payment-processor")
      match scope.version {
        Some(version) => assert_eq(version, "1.5.2")
        None => @test.fail("Expected instrumentation scope version")
      }
      match scope.schema_url {
        Some(schema_url) => assert_eq(schema_url, "https://opentelemetry.io/schemas/v1.20.0")
        None => @test.fail("Expected schema URL")
      }
    }
    None => @test.fail("Expected instrumentation scope")
  }
  
  // 验证日志严重性
  match complete_log_record.severity_number {
    Fatal => assert_eq(true, true)
    _ => @test.fail("Expected Fatal severity")
  }
}

test "log_record_builder_batch_operations" {
  // 测试LogRecordBuilder批量操作
  
  // 批量创建多个LogRecord
  let create_log_records = fn() : Array[LogRecord] {
    let base_timestamp = 1640995200123456789L
    let records = []
    
    // 创建不同严重性的日志记录
    let debug_record = LogRecord::builder()
      .timestamp(base_timestamp)
      .severity(Debug)
      .body("Starting user authentication process")
      .with_attribute("user.id", AttributeValue::string("user123"))
      .with_attribute("auth.method", AttributeValue::string("oauth2"))
      .build()
    
    let info_record = LogRecord::builder()
      .timestamp(base_timestamp + 1000000L)
      .severity(Info)
      .body("User authenticated successfully")
      .with_attribute("user.id", AttributeValue::string("user123"))
      .with_attribute("auth.duration_ms", AttributeValue::float(150.0))
      .build()
    
    let warn_record = LogRecord::builder()
      .timestamp(base_timestamp + 2000000L)
      .severity(Warn)
      .body("User has multiple active sessions")
      .with_attribute("user.id", AttributeValue::string("user123"))
      .with_attribute("session.count", AttributeValue::int(3L))
      .with_attribute("max.allowed", AttributeValue::int(2L))
      .build()
    
    let error_record = LogRecord::builder()
      .timestamp(base_timestamp + 3000000L)
      .severity(Error)
      .body("Failed to refresh user token")
      .with_attribute("user.id", AttributeValue::string("user123"))
      .with_attribute("error.code", AttributeValue::int(4001L))
      .with_attribute("error.message", AttributeValue::string("Token expired"))
      .build()
    
    records.push(debug_record)
    records.push(info_record)
    records.push(warn_record)
    records.push(error_record)
    
    records
  }
  
  let log_records = create_log_records()
  
  // 验证批量创建的日志记录
  assert_eq(log_records.length(), 4)
  
  // 验证每条记录的属性
  let verify_record = fn(record : LogRecord, expected_severity : SeverityNumber, expected_body_prefix : String) {
    match record.severity_number {
      s => assert_eq(s, expected_severity)
    }
    
    match record.body {
      Some(body) => assert_eq(body.has_prefix(expected_body_prefix), true)
      None => @test.fail("Expected log body")
    }
    
    assert_eq(record.attributes.length() >= 2, true)  // 至少有user.id和一个其他属性
  }
  
  verify_record(log_records[0], Debug, "Starting user authentication")
  verify_record(log_records[1], Info, "User authenticated successfully")
  verify_record(log_records[2], Warn, "User has multiple active")
  verify_record(log_records[3], Error, "Failed to refresh user")
  
  // 测试日志记录聚合统计
  let aggregate_log_stats = fn(records : Array[LogRecord]) -> (Int, Int, Int, Int, Int) {
    let mut debug_count = 0
    let mut info_count = 0
    let mut warn_count = 0
    let mut error_count = 0
    let mut fatal_count = 0
    
    let mut i = 0
    while i < records.length() {
      match records[i].severity_number {
        Debug => debug_count = debug_count + 1
        Info => info_count = info_count + 1
        Warn => warn_count = warn_count + 1
        Error => error_count = error_count + 1
        Fatal => fatal_count = fatal_count + 1
      }
      i = i + 1
    }
    
    (debug_count, info_count, warn_count, error_count, fatal_count)
  }
  
  let (debug_count, info_count, warn_count, error_count, fatal_count) = aggregate_log_stats(log_records)
  
  assert_eq(debug_count, 1)
  assert_eq(info_count, 1)
  assert_eq(warn_count, 1)
  assert_eq(error_count, 1)
  assert_eq(fatal_count, 0)
  
  // 测试时间戳顺序验证
  let verify_timestamp_order = fn(records : Array[LogRecord]) -> Bool {
    let mut i = 1
    let mut ordered = true
    
    while i < records.length() && ordered {
      if records[i].timestamp_unix_nanos < records[i-1].timestamp_unix_nanos {
        ordered = false
      }
      i = i + 1
    }
    
    ordered
  }
  
  assert_eq(verify_timestamp_order(log_records), true)
  
  // 测试用户ID一致性检查
  let verify_user_consistency = fn(records : Array[LogRecord], expected_user_id : String) -> Bool {
    let mut i = 0
    let mut consistent = true
    let mut found_user_id = false
    
    while i < records.length() && consistent {
      let mut j = 0
      while j < records[i].attributes.length() && consistent {
        let (key, value) = records[i].attributes[j]
        if key == "user.id" {
          found_user_id = true
          match value {
            AttributeValue::StringValue(user_id) => {
              if user_id != expected_user_id {
                consistent = false
              }
            }
            _ => consistent = false
          }
        }
        j = j + 1
      }
      i = i + 1
    }
    
    consistent && found_user_id
  }
  
  assert_eq(verify_user_consistency(log_records, "user123"), true)
}

test "log_record_builder_error_scenarios" {
  // 测试LogRecordBuilder错误场景
  
  // 测试空日志体
  let empty_body_record = LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(Info)
    .body("")
    .with_attribute("test.empty", AttributeValue::bool(true))
    .build()
  
  match empty_body_record.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Expected empty body")
  }
  
  // 测试大量属性
  let many_attributes_record = LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(Debug)
    .body("Record with many attributes")
    .with_attribute("attr.1", AttributeValue::string("value1"))
    .with_attribute("attr.2", AttributeValue::int(2L))
    .with_attribute("attr.3", AttributeValue::float(3.0))
    .with_attribute("attr.4", AttributeValue::bool(true))
    .with_attribute("attr.5", AttributeValue::array_string(["a", "b"]))
    .with_attribute("attr.6", AttributeValue::array_int([1L, 2L]))
    .with_attribute("attr.7", AttributeValue::array_float([1.1, 2.2]))
    .with_attribute("attr.8", AttributeValue::array_bool([true, false]))
    .build()
  
  assert_eq(many_attributes_record.attributes.length(), 8)
  
  // 测试特殊字符属性值
  let special_chars_record = LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(Warn)
    .body("Record with special characters")
    .with_attribute("special.chars", AttributeValue::string("Special: !@#$%^&*(){}[]|\\:;\"'<>?,./"))
    .with_attribute("unicode.chars", AttributeValue::string("Unicode: 测试 🚀 中文"))
    .with_attribute("newlines", AttributeValue::string("Line1\nLine2\r\nLine3"))
    .with_attribute("tabs", AttributeValue::string("Col1\tCol2\tCol3"))
    .build()
  
  let mut found_special_chars = false
  let mut i = 0
  while i < special_chars_record.attributes.length() {
    let (key, value) = special_chars_record.attributes[i]
    if key == "special.chars" {
      match value {
        AttributeValue::StringValue(chars) => {
          assert_eq(chars.contains("!@#$%"), true)
          found_special_chars = true
        }
        _ => @test.fail("Expected StringValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_special_chars, true)
  
  // 测试边界值时间戳
  let min_timestamp_record = LogRecord::builder()
    .timestamp(0L)
    .severity(Info)
    .body("Minimum timestamp")
    .build()
  
  assert_eq(min_timestamp_record.timestamp_unix_nanos, 0L)
  
  let max_timestamp_record = LogRecord::builder()
    .timestamp(9223372036854775807L)  // Int64最大值
    .severity(Info)
    .body("Maximum timestamp")
    .build()
  
  assert_eq(max_timestamp_record.timestamp_unix_nanos, 9223372036854775807L)
  
  // 测试极值数值属性
  let extreme_values_record = LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(Error)
    .body("Record with extreme values")
    .with_attribute("max.int64", AttributeValue::int(9223372036854775807L))
    .with_attribute("min.int64", AttributeValue::int(-9223372036854775808L))
    .with_attribute("max.double", AttributeValue::float(1.7976931348623157e+308))
    .with_attribute("min.double", AttributeValue::float(-1.7976931348623157e+308))
    .with_attribute("infinity", AttributeValue::float(1.0/0.0))
    .with_attribute("neg.infinity", AttributeValue::float(-1.0/0.0))
    .build()
  
  let mut found_max_int64 = false
  i = 0
  while i < extreme_values_record.attributes.length() {
    let (key, value) = extreme_values_record.attributes[i]
    if key == "max.int64" {
      match value {
        AttributeValue::IntValue(int_val) => {
          assert_eq(int_val, 9223372036854775807L)
          found_max_int64 = true
        }
        _ => @test.fail("Expected IntValue")
      }
    }
    i = i + 1
  }
  assert_eq(found_max_int64, true)
}