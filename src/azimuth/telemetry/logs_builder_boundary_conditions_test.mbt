// Logs API LogRecordBuilder 边界条件测试
// 测试LogRecordBuilder在各种边界条件下的行为

test "log_record_builder_empty_state" {
  // 测试完全空的builder状态
  let builder = LogRecordBuilder::new()
  let log_record = builder.build()
  
  // 验证默认值
  assert_eq(log_record.timestamp_unix_nanos, 0L)
  assert_eq(log_record.observed_timestamp_unix_nanos, None)
  match log_record.severity_number {
    Info => @test.succeed()  // 默认严重级别
    _ => @test.fail("Expected default Info severity")
  }
  assert_eq(log_record.severity_text, None)
  assert_eq(log_record.body, None)
  assert_eq(log_record.attributes.length(), 0)
  assert_eq(log_record.trace_id, None)
  assert_eq(log_record.span_id, None)
  assert_eq(log_record.trace_flags, None)
  assert_eq(log_record.resource, None)
  assert_eq(log_record.instrumentation_scope, None)
}

test "log_record_builder_extreme_timestamps" {
  // 测试极值时间戳
  let min_timestamp = -9223372036854775808L  // Int64最小值
  let max_timestamp = 9223372036854775807L   // Int64最大值
  
  let log_record_min = LogRecordBuilder::new()
    .timestamp(min_timestamp)
    .build()
  
  let log_record_max = LogRecordBuilder::new()
    .timestamp(max_timestamp)
    .build()
  
  assert_eq(log_record_min.timestamp_unix_nanos, min_timestamp)
  assert_eq(log_record_max.timestamp_unix_nanos, max_timestamp)
  
  // 测试观察时间戳的极值
  let log_record_with_observed = LogRecordBuilder::new()
    .timestamp(max_timestamp)
    .build()
  
  // 手动设置观察时间戳（因为builder没有对应方法）
  let log_record_with_custom_observed = LogRecord::{
    timestamp_unix_nanos: max_timestamp,
    observed_timestamp_unix_nanos: Some(min_timestamp),
    severity_number: Info,
    severity_text: None,
    body: None,
    attributes: [],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  match log_record_with_custom_observed.observed_timestamp_unix_nanos {
    Some(timestamp) => assert_eq(timestamp, min_timestamp)
    None => @test.fail("Expected observed timestamp")
  }
}

test "log_record_builder_all_severity_levels" {
  let base_body = "Test message for severity level"
  
  let trace_record = LogRecordBuilder::new()
    .severity(Trace)
    .body(base_body)
    .build()
  
  let debug_record = LogRecordBuilder::new()
    .severity(Debug)
    .body(base_body)
    .build()
  
  let info_record = LogRecordBuilder::new()
    .severity(Info)
    .body(base_body)
    .build()
  
  let warn_record = LogRecordBuilder::new()
    .severity(Warn)
    .body(base_body)
    .build()
  
  let error_record = LogRecordBuilder::new()
    .severity(Error)
    .body(base_body)
    .build()
  
  let fatal_record = LogRecordBuilder::new()
    .severity(Fatal)
    .body(base_body)
    .build()
  
  // 验证所有严重级别
  match trace_record.severity_number { Trace => @test.succeed() _ => @test.fail("Expected Trace") }
  match debug_record.severity_number { Debug => @test.succeed() _ => @test.fail("Expected Debug") }
  match info_record.severity_number { Info => @test.succeed() _ => @test.fail("Expected Info") }
  match warn_record.severity_number { Warn => @test.succeed() _ => @test.fail("Expected Warn") }
  match error_record.severity_number { Error => @test.succeed() _ => @test.fail("Expected Error") }
  match fatal_record.severity_number { Fatal => @test.succeed() _ => @test.fail("Expected Fatal") }
  
  // 验证所有记录都有相同的body
  match trace_record.body { Some(msg) => assert_eq(msg, base_body) None => @test.fail("Expected body") }
  match debug_record.body { Some(msg) => assert_eq(msg, base_body) None => @test.fail("Expected body") }
  match info_record.body { Some(msg) => assert_eq(msg, base_body) None => @test.fail("Expected body") }
  match warn_record.body { Some(msg) => assert_eq(msg, base_body) None => @test.fail("Expected body") }
  match error_record.body { Some(msg) => assert_eq(msg, base_body) None => @test.fail("Expected body") }
  match fatal_record.body { Some(msg) => assert_eq(msg, base_body) None => @test.fail("Expected body") }
}

test "log_record_builder_extreme_body_lengths" {
  // 测试空字符串body
  let empty_body_record = LogRecordBuilder::new()
    .body("")
    .build()
  
  match empty_body_record.body {
    Some(body) => assert_eq(body, "")
    None => @test.fail("Expected empty string body")
  }
  
  // 测试长字符串body
  let long_body = "A".repeat(10000)  // 10000个字符
  let long_body_record = LogRecordBuilder::new()
    .body(long_body)
    .build()
  
  match long_body_record.body {
    Some(body) => assert_eq(body.length(), 10000)
    None => @test.fail("Expected long body")
  }
  
  // 测试包含特殊字符的body
  let special_body = "Special chars: \n\t\r\"'\\{}[]()<>@#$%^&*()_+-=|/?.>,<~`"
  let special_body_record = LogRecordBuilder::new()
    .body(special_body)
    .build()
  
  match special_body_record.body {
    Some(body) => assert_eq(body, special_body)
    None => @test.fail("Expected special characters body")
  }
  
  // 测试Unicode字符
  let unicode_body = "Unicode: 你好 🌍 ñáéíóú 🚀 📊"
  let unicode_body_record = LogRecordBuilder::new()
    .body(unicode_body)
    .build()
  
  match unicode_body_record.body {
    Some(body) => assert_eq(body, unicode_body)
    None => @test.fail("Expected unicode body")
  }
}

test "log_record_builder_attribute_edge_cases" {
  // 测试空属性键和值
  let empty_key_record = LogRecordBuilder::new()
    .with_attribute("", @azimuth.telemetry.api.common.AttributeValue::string("value"))
    .build()
  
  assert_eq(empty_key_record.attributes.length(), 1)
  match empty_key_record.attributes[0] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "")
      assert_eq(value, "value")
    }
    _ => @test.fail("Expected string attribute with empty key")
  }
  
  // 测试空字符串值
  let empty_value_record = LogRecordBuilder::new()
    .with_attribute("key", @azimuth.telemetry.api.common.AttributeValue::string(""))
    .build()
  
  assert_eq(empty_value_record.attributes.length(), 1)
  match empty_value_record.attributes[0] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "key")
      assert_eq(value, "")
    }
    _ => @test.fail("Expected empty string value")
  }
  
  // 测试极值数值
  let extreme_values_record = LogRecordBuilder::new()
    .with_attribute("max_int", @azimuth.telemetry.api.common.AttributeValue::int(9223372036854775807L))
    .with_attribute("min_int", @azimuth.telemetry.api.common.AttributeValue::int(-9223372036854775808L))
    .with_attribute("max_float", @azimuth.telemetry.api.common.AttributeValue::float(1.7976931348623157e308))
    .with_attribute("min_float", @azimuth.telemetry.api.common.AttributeValue::float(-1.7976931348623157e308))
    .with_attribute("infinity", @azimuth.telemetry.api.common.AttributeValue::float(1.0/0.0))
    .with_attribute("neg_infinity", @azimuth.telemetry.api.common.AttributeValue::float(-1.0/0.0))
    .build()
  
  assert_eq(extreme_values_record.attributes.length(), 6)
  
  // 测试空数组
  let empty_array_record = LogRecordBuilder::new()
    .with_attribute("empty_string_array", @azimuth.telemetry.api.common.AttributeValue::array_string([]))
    .with_attribute("empty_int_array", @azimuth.telemetry.api.common.AttributeValue::array_int([]))
    .with_attribute("empty_float_array", @azimuth.telemetry.api.common.AttributeValue::array_float([]))
    .with_attribute("empty_bool_array", @azimuth.telemetry.api.common.AttributeValue::array_bool([]))
    .build()
  
  assert_eq(empty_array_record.attributes.length(), 4)
}

test "log_record_builder_multiple_same_attributes" {
  // 测试添加多个相同键的属性
  let multiple_attrs_record = LogRecordBuilder::new()
    .with_attribute("key", @azimuth.telemetry.api.common.AttributeValue::string("value1"))
    .with_attribute("key", @azimuth.telemetry.api.common.AttributeValue::string("value2"))
    .with_attribute("key", @azimuth.telemetry.api.common.AttributeValue::string("value3"))
    .build()
  
  assert_eq(multiple_attrs_record.attributes.length(), 3)
  
  // 验证所有属性都被添加
  let mut i = 0
  while i < multiple_attrs_record.attributes.length() {
    match multiple_attrs_record.attributes[i] {
      (key, @azimuth.telemetry.api.common.StringValue(value)) => {
        assert_eq(key, "key")
        match i {
          0 => assert_eq(value, "value1")
          1 => assert_eq(value, "value2")
          2 => assert_eq(value, "value3")
          _ => @test.fail("Invalid index")
        }
      }
      _ => @test.fail("Expected string attribute")
    }
    i = i + 1
  }
}

test "log_record_builder_chained_operations" {
  // 测试链式操作的边界情况
  let chained_record = LogRecordBuilder::new()
    .timestamp(1640995200000000000L)
    .severity(Error)
    .body("Chained operations test")
    .with_attribute("attr1", @azimuth.telemetry.api.common.AttributeValue::string("value1"))
    .with_attribute("attr2", @azimuth.telemetry.api.common.AttributeValue::int(42L))
    .with_attribute("attr3", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .with_attribute("attr4", @azimuth.telemetry.api.common.AttributeValue::float(3.14))
    .build()
  
  assert_eq(chained_record.timestamp_unix_nanos, 1640995200000000000L)
  match chained_record.severity_number { Error => @test.succeed() _ => @test.fail("Expected Error") }
  match chained_record.body { Some(msg) => assert_eq(msg, "Chained operations test") None => @test.fail("Expected body") }
  assert_eq(chained_record.attributes.length(), 4)
  
  // 验证所有属性
  let expected_values = [
    ("attr1", "value1"),
    ("attr2", "42"),
    ("attr3", "true"),
    ("attr4", "3.14")
  ]
  
  let mut i = 0
  while i < expected_values.length() {
    let (expected_key, expected_value) = expected_values[i]
    match chained_record.attributes[i] {
      (key, attribute_value) => {
        assert_eq(key, expected_key)
        match attribute_value {
          @azimuth.telemetry.api.common.StringValue(value) => assert_eq(value, expected_value)
          @azimuth.telemetry.api.common.IntValue(value) => assert_eq(value.to_string(), expected_value)
          @azimuth.telemetry.api.common.BoolValue(value) => assert_eq(value.to_string(), expected_value)
          @azimuth.telemetry.api.common.FloatValue(value) => assert_eq(value.to_string(), expected_value)
          _ => @test.fail("Unexpected attribute type")
        }
      }
    }
    i = i + 1
  }
}

test "log_record_builder_large_attribute_sets" {
  // 测试大量属性的添加
  let mut builder = LogRecordBuilder::new()
    .body("Large attribute set test")
  
  // 添加100个属性
  let mut i = 0
  while i < 100 {
    let key = "attr_" + i.to_string()
    let value = "value_" + i.to_string()
    builder = builder.with_attribute(key, @azimuth.telemetry.api.common.AttributeValue::string(value))
    i = i + 1
  }
  
  let large_attrs_record = builder.build()
  
  assert_eq(large_attrs_record.attributes.length(), 100)
  
  // 验证第一个和最后一个属性
  match large_attrs_record.attributes[0] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "attr_0")
      assert_eq(value, "value_0")
    }
    _ => @test.fail("Expected first attribute")
  }
  
  match large_attrs_record.attributes[99] {
    (key, @azimuth.telemetry.api.common.StringValue(value)) => {
      assert_eq(key, "attr_99")
      assert_eq(value, "value_99")
    }
    _ => @test.fail("Expected last attribute")
  }
}

test "log_record_builder_mixed_attribute_types" {
  // 测试混合类型属性
  let mixed_attrs_record = LogRecordBuilder::new()
    .body("Mixed attribute types")
    .with_attribute("string_attr", @azimuth.telemetry.api.common.AttributeValue::string("string_value"))
    .with_attribute("int_attr", @azimuth.telemetry.api.common.AttributeValue::int(123L))
    .with_attribute("float_attr", @azimuth.telemetry.api.common.AttributeValue::float(45.67))
    .with_attribute("bool_attr", @azimuth.telemetry.api.common.AttributeValue::bool(true))
    .with_attribute("string_array", @azimuth.telemetry.api.common.AttributeValue::array_string(["a", "b", "c"]))
    .with_attribute("int_array", @azimuth.telemetry.api.common.AttributeValue::array_int([1L, 2L, 3L]))
    .with_attribute("float_array", @azimuth.telemetry.api.common.AttributeValue::array_float([1.1, 2.2, 3.3]))
    .with_attribute("bool_array", @azimuth.telemetry.api.common.AttributeValue::array_bool([true, false, true]))
    .build()
  
  assert_eq(mixed_attrs_record.attributes.length(), 8)
  
  // 验证每种类型
  let mut string_found = false
  let mut int_found = false
  let mut float_found = false
  let mut bool_found = false
  let mut string_array_found = false
  let mut int_array_found = false
  let mut float_array_found = false
  let mut bool_array_found = false
  
  let mut i = 0
  while i < mixed_attrs_record.attributes.length() {
    match mixed_attrs_record.attributes[i] {
      (key, @azimuth.telemetry.api.common.StringValue(_)) => {
        if key == "string_attr" { string_found = true }
        if key == "a" { string_array_found = true }
      }
      (key, @azimuth.telemetry.api.common.IntValue(_)) => {
        if key == "int_attr" { int_found = true }
        if key == "1" { int_array_found = true }
      }
      (key, @azimuth.telemetry.api.common.FloatValue(_)) => {
        if key == "float_attr" { float_found = true }
        if key == "1.1" { float_array_found = true }
      }
      (key, @azimuth.telemetry.api.common.BoolValue(_)) => {
        if key == "bool_attr" { bool_found = true }
        if key == "true" { bool_array_found = true }
      }
      _ => {}
    }
    i = i + 1
  }
  
  assert(string_found, "String attribute not found")
  assert(int_found, "Int attribute not found")
  assert(float_found, "Float attribute not found")
  assert(bool_found, "Bool attribute not found")
}