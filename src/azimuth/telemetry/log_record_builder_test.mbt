// Azimuth Telemetry - LogRecord Builder Tests
// 测试LogRecordBuilder的完整功能

test "log_record_builder_observed_timestamp" {
  // 测试LogRecordBuilder的observed_timestamp方法
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L) // 2022-01-01 00:00:00 UTC
    .observed_timestamp(1640995200000000100L) // 100ns later
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Log with observed timestamp")
    .build()
  
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.observed_timestamp_unix_nanos, Some(1640995200000000100L))
  assert_eq(log_record.severity_number, @azimuth.telemetry.api.logs.Info)
  assert_eq(log_record.body, Some("Log with observed timestamp"))
}

test "log_record_builder_severity_text" {
  // 测试LogRecordBuilder的severity_text方法
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Error)
    .severity_text("CUSTOM_ERROR")
    .body("Custom error message")
    .build()
  
  assert_eq(log_record.severity_number, @azimuth.telemetry.api.logs.Error)
  assert_eq(log_record.severity_text, Some("CUSTOM_ERROR"))
  assert_eq(log_record.body, Some("Custom error message"))
}

test "log_record_builder_comprehensive" {
  // 测试LogRecordBuilder的完整功能
  let trace_id = Array::make(16, 1_byte)
  let span_id = Array::make(8, 2_byte)
  let resource = @azimuth.telemetry.api.common.Resource::default("test-service")
  let scope = @azimuth.telemetry.api.common.InstrumentationScope::{
    name: "test-scope",
    version: Some("1.0.0"),
    schema_url: Some("http://example.com/schema")
  }
  
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .observed_timestamp(1640995200000000100L)
    .severity(@azimuth.telemetry.api.logs.Warn)
    .severity_text("WARNING")
    .body("Comprehensive log message")
    .with_attribute("attr1", @azimuth.telemetry.api.common.AttributeValue::string("value1"))
    .with_attribute("attr2", @azimuth.telemetry.api.common.AttributeValue::int(42L))
    .with_attribute("attr3", @azimuth.telemetry.api.common.AttributeValue::float(3.14))
    .with_attribute("attr4", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .build()
  
  // 验证所有字段
  assert_eq(log_record.timestamp_unix_nanos, 1640995200000000000L)
  assert_eq(log_record.observed_timestamp_unix_nanos, Some(1640995200000000100L))
  assert_eq(log_record.severity_number, @azimuth.telemetry.api.logs.Warn)
  assert_eq(log_record.severity_text, Some("WARNING"))
  assert_eq(log_record.body, Some("Comprehensive log message"))
  assert_eq(log_record.attributes.length(), 4)
}

test "log_record_builder_chaining" {
  // 测试LogRecordBuilder的方法链式调用
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("Chained builder test")
    .with_attribute("key1", @azimuth.telemetry.api.common.AttributeValue::string("value1"))
    .with_attribute("key2", @azimuth.telemetry.api.common.AttributeValue::int(123L))
    .with_attribute("key3", @azimuth.telemetry.api.common.AttributeValue::float(456.789))
    .with_attribute("key4", @azimuth.telemetry.api.common.AttributeValue::bool(false))
    .with_attribute("key5", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(["a", "b", "c"]))
    .build()
  
  assert_eq(log_record.body, Some("Chained builder test"))
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证所有属性
  match log_record.attributes[0] {
    ("key1", @azimuth.telemetry.api.common.AttributeValue::StringValue(value)) => {
      assert_eq(value, "value1")
    }
    _ => @test.fail("Expected key1 attribute")
  }
  
  match log_record.attributes[1] {
    ("key2", @azimuth.telemetry.api.common.AttributeValue::IntValue(value)) => {
      assert_eq(value, 123L)
    }
    _ => @test.fail("Expected key2 attribute")
  }
  
  match log_record.attributes[2] {
    ("key3", @azimuth.telemetry.api.common.AttributeValue::FloatValue(value)) => {
      assert_eq(value, 456.789)
    }
    _ => @test.fail("Expected key3 attribute")
  }
  
  match log_record.attributes[3] {
    ("key4", @azimuth.telemetry.api.common.AttributeValue::BoolValue(value)) => {
      assert_eq(value, false)
    }
    _ => @test.fail("Expected key4 attribute")
  }
  
  match log_record.attributes[4] {
    ("key5", @azimuth.telemetry.api.common.AttributeValue::ArrayStringValue(value)) => {
      assert_eq(value.length(), 3)
      assert_eq(value[0], "a")
      assert_eq(value[1], "b")
      assert_eq(value[2], "c")
    }
    _ => @test.fail("Expected key5 attribute")
  }
}

test "log_record_builder_default_values" {
  // 测试LogRecordBuilder的默认值
  let log_record = @azimuth.telemetry.api.logs.LogRecord::builder()
    .build() // 不设置任何字段
  
  // 验证默认值
  assert_eq(log_record.timestamp_unix_nanos, 0L)
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  assert_eq(log_record.severity_number, @azimuth.telemetry.api.logs.Info)
  assert_eq(log_record.severity_text, None)
  assert_eq(log_record.body, None)
  assert_eq(log_record.attributes.length(), 0)
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  assert_eq(log_record.resource, None)
  assert_eq(log_record.instrumentation_scope, None)
}

test "log_record_builder_edge_cases" {
  // 测试LogRecordBuilder的边界情况
  // 测试极值时间戳
  let min_time_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(-9223372036854775808L) // Int64最小值
    .observed_timestamp(-9223372036854775808L)
    .severity(@azimuth.telemetry.api.logs.Fatal)
    .body("Min time test")
    .build()
  
  let max_time_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(9223372036854775807L) // Int64最大值
    .observed_timestamp(9223372036854775807L)
    .severity(@azimuth.telemetry.api.logs.Trace)
    .body("Max time test")
    .build()
  
  assert_eq(min_time_log.timestamp_unix_nanos, -9223372036854775808L)
  assert_eq(min_time_log.observed_timestamp_unix_nanos, Some(-9223372036854775808L))
  assert_eq(max_time_log.timestamp_unix_nanos, 9223372036854775807L)
  assert_eq(max_time_log.observed_timestamp_unix_nanos, Some(9223372036854775807L))
  
  // 测试空字符串和特殊字符
  let empty_body_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Info)
    .body("")
    .with_attribute("empty.key", @azimuth.telemetry.api.common.AttributeValue::string(""))
    .build()
  
  let special_chars_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Error)
    .body("Special chars: \n\t\r\"'<>{}[]|&;`")
    .with_attribute("special.key", @azimuth.telemetry.api.common.AttributeValue::string("key.with.special@chars#123"))
    .build()
  
  assert_eq(empty_body_log.body, Some(""))
  assert_eq(special_chars_log.body, Some("Special chars: \n\t\r\"'<>{}[]|&;`"))
  
  // 测试Unicode
  let unicode_log = @azimuth.telemetry.api.logs.LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(@azimuth.telemetry.api.logs.Warn)
    .body("Unicode测试: 🔥 🚀 ✨ 中文测试")
    .with_attribute("unicode.key", @azimuth.telemetry.api.common.AttributeValue::string("键名中文测试"))
    .build()
  
  assert_eq(unicode_log.body, Some("Unicode测试: 🔥 🚀 ✨ 中文测试"))
}