// LogRecordBuilder高级链式调用测试用例
// 测试复杂的链式调用场景和边界情况

test "log_record_builder_fluent_chaining" {
  // 测试LogRecordBuilder的流畅链式调用
  
  // 测试长链式调用
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
    .severity(Error)
    .body("Database connection failed")
    .with_attribute("service.name", AttributeValue::string("user-service"))
    .with_attribute("service.version", AttributeValue::string("1.2.3"))
    .with_attribute("deployment.environment", AttributeValue::string("production"))
    .with_attribute("error.type", AttributeValue::string("database.connection"))
    .with_attribute("error.code", AttributeValue::int(5003L))
    .with_attribute("error.message", AttributeValue::string("Connection timeout after 30 seconds"))
    .with_attribute("database.host", AttributeValue::string("db-primary.prod.example.com"))
    .with_attribute("database.port", AttributeValue::int(5432L))
    .with_attribute("database.name", AttributeValue::string("userdb"))
    .with_attribute("connection.pool.size", AttributeValue::int(20L))
    .with_attribute("connection.pool.active", AttributeValue::int(20L))
    .with_attribute("connection.pool.idle", AttributeValue::int(0L))
    .with_attribute("retry.count", AttributeValue::int(3L))
    .with_attribute("request.id", AttributeValue::string("req-7c9e6679-7425-40de-944b-e07fc1f90ae7"))
    .with_attribute("session.id", AttributeValue::string("sess-1f2e3d4c-5b6a-7b8c-9d0e-1f2a3b4c5d6e"))
    .with_attribute("user.id", AttributeValue::int(12345678L))
    .with_attribute("trace.id", AttributeValue::string("0af7651916cd43dd8448eb211c80319c"))
    .with_attribute("span.id", AttributeValue::string("b7ad6b7169203331"))
    .with_attribute("response.time.ms", AttributeValue::float(30000.5))
    .with_attribute("server.healthy", AttributeValue::bool(false))
    .with_attribute("alert.severity", AttributeValue::string("critical"))
    .with_attribute("tags", AttributeValue::array_string(["database", "error", "critical", "production"]))
    .with_attribute("error.codes", AttributeValue::array_int([5003L, 5004L, 5005L]))
    .with_attribute("latency.thresholds", AttributeValue::array_float([1000.0, 5000.0, 10000.0]))
    .with_attribute("feature.flags", AttributeValue::array_bool([true, false, true, false]))
    .build()
  
  // 验证所有字段都正确设置
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  match log_record.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  match log_record.body {
    Some(body) => assert_eq(body, "Database connection failed")
    None => @test.fail("Expected Some(body)")
  }
  assert_eq(log_record.attributes.length(), 25)
}

test "log_record_builder_conditional_chaining" {
  // 测试条件链式调用（模拟不同场景）
  
  let is_debug = false
  let include_user_info = true
  let include_performance_metrics = true
  let error_occurred = true
  
  let mut builder = LogRecord::builder()
    .timestamp(1640995200000000000L)
  
  // 条件设置严重级别
  if error_occurred {
    builder = builder.severity(Error).body("An error occurred")
  } else if is_debug {
    builder = builder.severity(Debug).body("Debug information")
  } else {
    builder = builder.severity(Info).body("Normal operation")
  }
  
  // 条件添加用户信息
  if include_user_info {
    builder = builder
      .with_attribute("user.id", AttributeValue::int(12345L))
      .with_attribute("user.role", AttributeValue::string("admin"))
      .with_attribute("user.permissions", AttributeValue::array_string(["read", "write", "delete"]))
  }
  
  // 条件添加性能指标
  if include_performance_metrics {
    builder = builder
      .with_attribute("cpu.usage", AttributeValue::float(75.5))
      .with_attribute("memory.usage", AttributeValue::float(60.2))
      .with_attribute("disk.io", AttributeValue::float(1024.7))
      .with_attribute("network.latency", AttributeValue::float(12.3))
  }
  
  let log_record = builder.build()
  
  // 验证条件设置的字段
  match log_record.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  match log_record.body {
    Some(body) => assert_eq(body, "An error occurred")
    None => @test.fail("Expected Some(body)")
  }
  
  // 验证用户信息被添加
  assert_eq(log_record.attributes.length(), 9)
  
  // 验证性能指标被添加
  let mut found_cpu = false
  let mut found_memory = false
  let mut i = 0
  while i < log_record.attributes.length() {
    match log_record.attributes[i].0 {
      "cpu.usage" => found_cpu = true,
      "memory.usage" => found_memory = true,
      _ => ()
    }
    i = i + 1
  }
  assert_eq(found_cpu, true)
  assert_eq(found_memory, true)
}

test "log_record_builder_dynamic_attribute_building" {
  // 测试动态构建属性
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Dynamic attribute test")
    .build()
  
  // 动态添加属性
  let mut builder = LogRecord::builder()
    .timestamp(log_record.timestamp_unix_nanos)
    .severity(log_record.severity_number)
    .body(log_record.body.unwrap_or(""))
  
  // 动态添加各种类型的属性
  let dynamic_attributes = [
    ("string.value", AttributeValue::string("dynamic string")),
    ("int.value", AttributeValue::int(42L)),
    ("float.value", AttributeValue::float(3.14159)),
    ("bool.value", AttributeValue::bool(true)),
    ("string.array", AttributeValue::array_string(["a", "b", "c"])),
    ("int.array", AttributeValue::array_int([1L, 2L, 3L])),
    ("float.array", AttributeValue::array_float([1.1, 2.2, 3.3])),
    ("bool.array", AttributeValue::array_bool([true, false, true]))
  ]
  
  let mut i = 0
  while i < dynamic_attributes.length() {
    let (key, value) = dynamic_attributes[i]
    builder = builder.with_attribute(key, value)
    i = i + 1
  }
  
  let final_log_record = builder.build()
  
  // 验证动态属性
  assert_eq(final_log_record.attributes.length(), 8)
  
  // 验证每种类型的属性
  let mut found_string = false
  let mut found_int = false
  let mut found_float = false
  let mut found_bool = false
  let mut found_string_array = false
  let mut found_int_array = false
  let mut found_float_array = false
  let mut found_bool_array = false
  
  let mut j = 0
  while j < final_log_record.attributes.length() {
    let (key, value) = final_log_record.attributes[j]
    match key {
      "string.value" => {
        match value {
          AttributeValue::StringValue(s) => {
            assert_eq(s, "dynamic string")
            found_string = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      "int.value" => {
        match value {
          AttributeValue::IntValue(i) => {
            assert_eq(i, 42L)
            found_int = true
          }
          _ => @test.fail("Expected IntValue")
        }
      }
      "float.value" => {
        match value {
          AttributeValue::FloatValue(f) => {
            assert_eq(f, 3.14159)
            found_float = true
          }
          _ => @test.fail("Expected FloatValue")
        }
      }
      "bool.value" => {
        match value {
          AttributeValue::BoolValue(b) => {
            assert_eq(b, true)
            found_bool = true
          }
          _ => @test.fail("Expected BoolValue")
        }
      }
      "string.array" => {
        match value {
          AttributeValue::ArrayStringValue(arr) => {
            assert_eq(arr.length(), 3)
            found_string_array = true
          }
          _ => @test.fail("Expected ArrayStringValue")
        }
      }
      "int.array" => {
        match value {
          AttributeValue::ArrayIntValue(arr) => {
            assert_eq(arr.length(), 3)
            found_int_array = true
          }
          _ => @test.fail("Expected ArrayIntValue")
        }
      }
      "float.array" => {
        match value {
          AttributeValue::ArrayFloatValue(arr) => {
            assert_eq(arr.length(), 3)
            found_float_array = true
          }
          _ => @test.fail("Expected ArrayFloatValue")
        }
      }
      "bool.array" => {
        match value {
          AttributeValue::ArrayBoolValue(arr) => {
            assert_eq(arr.length(), 3)
            found_bool_array = true
          }
          _ => @test.fail("Expected ArrayBoolValue")
        }
      }
      _ => ()
    }
    j = j + 1
  }
  
  assert_eq(found_string, true)
  assert_eq(found_int, true)
  assert_eq(found_float, true)
  assert_eq(found_bool, true)
  assert_eq(found_string_array, true)
  assert_eq(found_int_array, true)
  assert_eq(found_float_array, true)
  assert_eq(found_bool_array, true)
}

test "log_record_builder_error_scenarios" {
  // 测试LogRecordBuilder的错误场景
  
  // 测试空字符串和特殊值
  let log_record1 = LogRecord::builder()
    .timestamp(0L)
    .severity(Info)
    .body("")
    .with_attribute("empty.string", AttributeValue::string(""))
    .with_attribute("empty.array", AttributeValue::array_string([]))
    .with_attribute("zero.int", AttributeValue::int(0L))
    .with_attribute("zero.float", AttributeValue::float(0.0))
    .with_attribute("false.bool", AttributeValue::bool(false))
    .build()
  
  match log_record1.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Expected Some(body)")
  }
  assert_eq(log_record1.attributes.length(), 5)
  
  // 测试极值
  let log_record2 = LogRecord::builder()
    .timestamp(9223372036854775807L)  // Int64最大值
    .severity(Fatal)
    .body("Extreme values test")
    .with_attribute("max.int", AttributeValue::int(9223372036854775807L))
    .with_attribute("min.int", AttributeValue::int(-9223372036854775808L))
    .with_attribute("max.float", AttributeValue::float(1.7976931348623157e+308))
    .with_attribute("min.float", AttributeValue::float(-1.7976931348623157e+308))
    .with_attribute("infinity.float", AttributeValue::float(1.0/0.0))
    .with_attribute("neg.infinity.float", AttributeValue::float(-1.0/0.0))
    .with_attribute("nan.float", AttributeValue::float(0.0/0.0))
    .build()
  
  assert_eq(log_record2.timestamp_unix_nanos, 9223372036854775807L)
  match log_record2.severity_number {
    Fatal => assert_eq(true, true)
    _ => @test.fail("Expected Fatal severity")
  }
  assert_eq(log_record2.attributes.length(), 7)
  
  // 测试Unicode和特殊字符
  let log_record3 = LogRecord::builder()
    .body("Unicode测试: 🚀🎉💻")
    .with_attribute("unicode.key", AttributeValue::string("Unicode键: 测试"))
    .with_attribute("emoji.value", AttributeValue::string("🌟⭐✨"))
    .with_attribute("special.chars", AttributeValue::string("!@#$%^&*()_+-={}[]|\\:;\"'<>?,./"))
    .with_attribute("whitespace", AttributeValue::string("  \t\n\r  "))
    .with_attribute("newlines", AttributeValue::string("line1\nline2\nline3"))
    .with_attribute("tabs", AttributeValue::string("col1\tcol2\tcol3"))
    .build()
  
  match log_record3.body {
    Some(body) => assert_eq(body, "Unicode测试: 🚀🎉💻")
    None => @test.fail("Expected Some(body)")
  }
  assert_eq(log_record3.attributes.length(), 6)
}

test "log_record_builder_performance_patterns" {
  // 测试LogRecordBuilder的性能模式
  
  // 测试大量属性的builder
  let mut builder = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Performance test with many attributes")
  
  // 添加大量属性
  let mut i = 0
  while i < 1000 {
    let key = "attr." + i.to_string()
    let value = "value." + i.to_string()
    builder = builder.with_attribute(key, AttributeValue::string(value))
    i = i + 1
  }
  
  let log_record = builder.build()
  assert_eq(log_record.attributes.length(), 1000)
  
  // 验证部分属性
  match log_record.attributes[0].1 {
    AttributeValue::StringValue(s) => assert_eq(s, "value.0")
    _ => @test.fail("Expected StringValue")
  }
  
  match log_record.attributes[999].1 {
    AttributeValue::StringValue(s) => assert_eq(s, "value.999")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试嵌套builder模式
  let nested_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Nested builder test")
    .with_attribute("nested.level1", AttributeValue::string("value1"))
    .build()
  
  // 基于现有log_record创建新的builder
  let enhanced_log = LogRecord::builder()
    .timestamp(nested_log.timestamp_unix_nanos)
    .severity(nested_log.severity_number)
    .body(nested_log.body.unwrap_or(""))
    .with_attribute("nested.level1", AttributeValue::string("value1"))
    .with_attribute("nested.level2", AttributeValue::string("value2"))
    .with_attribute("nested.level3", AttributeValue::string("value3"))
    .build()
  
  assert_eq(enhanced_log.attributes.length(), 3)
}

test "log_record_builder_severity_transitions" {
  // 测试不同严重级别的builder模式
  
  // 测试所有严重级别
  let severity_levels = [
    (Trace, "Trace level message"),
    (Debug, "Debug level message"),
    (Info, "Info level message"),
    (Warn, "Warning level message"),
    (Error, "Error level message"),
    (Fatal, "Fatal level message")
  ]
  
  let mut i = 0
  while i < severity_levels.length() {
    let (severity, message) = severity_levels[i]
    
    let log_record = LogRecord::builder()
      .timestamp(1640995200000000000L + (i as Int64))
      .severity(severity)
      .body(message)
      .with_attribute("severity.index", AttributeValue::int(i as Int64))
      .with_attribute("severity.name", AttributeValue::string(severity.to_string()))
      .build()
    
    // 验证严重级别
    match log_record.severity_number {
      s => assert_eq(s, severity)
    }
    
    // 验证消息
    match log_record.body {
      Some(body) => assert_eq(body, message)
      None => @test.fail("Expected Some(body)")
    }
    
    // 验证属性
    assert_eq(log_record.attributes.length(), 2)
    
    i = i + 1
  }
  
  // 测试严重级别递进
  let mut progressive_builder = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Trace)
    .body("Starting operation")
  
  progressive_builder = progressive_builder
    .severity(Debug)
    .body("Debugging operation")
    .with_attribute("operation.stage", AttributeValue::string("debug"))
  
  progressive_builder = progressive_builder
    .severity(Info)
    .body("Operation in progress")
    .with_attribute("operation.stage", AttributeValue::string("progress"))
  
  progressive_builder = progressive_builder
    .severity(Warn)
    .body("Operation warning")
    .with_attribute("operation.stage", AttributeValue::string("warning"))
  
  progressive_builder = progressive_builder
    .severity(Error)
    .body("Operation failed")
    .with_attribute("operation.stage", AttributeValue::string("failed"))
  
  let final_log = progressive_builder.build()
  
  match final_log.severity_number {
    Error => assert_eq(true, true)
    _ => @test.fail("Expected Error severity")
  }
  
  match final_log.body {
    Some(body) => assert_eq(body, "Operation failed")
    None => @test.fail("Expected Some(body)")
  }
  
  // 由于每次调用severity()都会覆盖之前的值，只有最后一个stage属性会被保留
  assert_eq(final_log.attributes.length(), 1)
  match final_log.attributes[0].1 {
    AttributeValue::StringValue(stage) => assert_eq(stage, "failed")
    _ => @test.fail("Expected StringValue")
  }
}

test "log_record_builder_complex_array_attributes" {
  // 测试复杂数组属性的构建
  
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(Info)
    .body("Complex array attributes test")
    .with_attribute("string.array.empty", AttributeValue::array_string([]))
    .with_attribute("string.array.single", AttributeValue::array_string(["single"]))
    .with_attribute("string.array.multiple", AttributeValue::array_string(["first", "second", "third"]))
    .with_attribute("string.array.unicode", AttributeValue::array_string(["测试", "🚀", "混合"]))
    .with_attribute("string.array.special", AttributeValue::array_string(["!@#$%", " spaces ", "line\nbreak"]))
    .with_attribute("int.array.empty", AttributeValue::array_int([]))
    .with_attribute("int.array.single", AttributeValue::array_int([42L]))
    .with_attribute("int.array.multiple", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L]))
    .with_attribute("int.array.extreme", AttributeValue::array_int([0L, -1L, 9223372036854775807L, -9223372036854775808L]))
    .with_attribute("float.array.empty", AttributeValue::array_float([]))
    .with_attribute("float.array.single", AttributeValue::array_float([3.14159]))
    .with_attribute("float.array.multiple", AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("float.array.special", AttributeValue::array_float([0.0, 1.0/0.0, -1.0/0.0, 0.0/0.0]))
    .with_attribute("bool.array.empty", AttributeValue::array_bool([]))
    .with_attribute("bool.array.single", AttributeValue::array_bool([true]))
    .with_attribute("bool.array.multiple", AttributeValue::array_bool([true, false, true, false]))
    .build()
  
  assert_eq(log_record.attributes.length(), 16)
  
  // 验证各种数组类型
  let mut string_arrays_found = 0
  let mut int_arrays_found = 0
  let mut float_arrays_found = 0
  let mut bool_arrays_found = 0
  
  let mut i = 0
  while i < log_record.attributes.length() {
    let (key, value) = log_record.attributes[i]
    match value {
      AttributeValue::ArrayStringValue(_) => string_arrays_found = string_arrays_found + 1,
      AttributeValue::ArrayIntValue(_) => int_arrays_found = int_arrays_found + 1,
      AttributeValue::ArrayFloatValue(_) => float_arrays_found = float_arrays_found + 1,
      AttributeValue::ArrayBoolValue(_) => bool_arrays_found = bool_arrays_found + 1,
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(string_arrays_found, 4)
  assert_eq(int_arrays_found, 4)
  assert_eq(float_arrays_found, 4)
  assert_eq(bool_arrays_found, 4)
}