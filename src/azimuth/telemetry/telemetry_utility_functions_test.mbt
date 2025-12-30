// 遥测系统实用工具函数测试
// 测试各种辅助函数、工具方法和便捷操作

test "attribute_value_creation_utilities" {
  // 测试AttributeValue创建工具函数
  
  // 测试字符串类型
  let string_attr = common::AttributeValue::string("test_string")
  match string_attr {
    common::StringValue(value) => assert_eq(value, "test_string")
    _ => @test.fail("Expected StringValue")
  }
  
  // 测试整数类型
  let int_attr = common::AttributeValue::int(42L)
  match int_attr {
    common::IntValue(value) => assert_eq(value, 42L)
    _ => @test.fail("Expected IntValue")
  }
  
  // 测试浮点类型
  let float_attr = common::AttributeValue::float(3.14159)
  match float_attr {
    common::FloatValue(value) => assert_eq(value, 3.14159)
    _ => @test.fail("Expected FloatValue")
  }
  
  // 测试布尔类型
  let bool_attr = common::AttributeValue::bool(true)
  match bool_attr {
    common::BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Expected BoolValue")
  }
  
  // 测试数组类型
  let string_array = common::AttributeValue::array_string(["a", "b", "c"])
  match string_array {
    common::ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  let int_array = common::AttributeValue::array_int([1L, 2L, 3L])
  match int_array {
    common::ArrayIntValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1L)
      assert_eq(values[1], 2L)
      assert_eq(values[2], 3L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  let float_array = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  match float_array {
    common::ArrayFloatValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1.1)
      assert_eq(values[1], 2.2)
      assert_eq(values[2], 3.3)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  let bool_array = common::AttributeValue::array_bool([true, false, true])
  match bool_array {
    common::ArrayBoolValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], true)
      assert_eq(values[1], false)
      assert_eq(values[2], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}

test "resource_creation_utilities" {
  // 测试Resource创建工具函数
  
  // 测试默认资源创建
  let default_resource = common::Resource::default("test_service")
  assert_eq(default_resource.service_name, "test_service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 测试带有版本的资源创建
  let resource_with_version = common::Resource::{
    ..default_resource,
    service_version: Some("1.2.3")
  }
  assert_eq(resource_with_version.service_version.unwrap(), "1.2.3")
  
  // 测试带有属性的资源创建
  let resource_with_attrs = common::Resource::{
    ..default_resource,
    attributes: [
      ("env", common::AttributeValue::string("production")),
      ("version", common::AttributeValue::string("1.0.0")),
      ("region", common::AttributeValue::string("us-west-2"))
    ]
  }
  assert_eq(resource_with_attrs.attributes.length(), 3)
  
  // 测试完整资源创建
  let full_resource = common::Resource::{
    service_name: "full_service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "custom_sdk",
    telemetry_sdk_version: "1.5.2",
    attributes: [
      ("host", common::AttributeValue::string("server-01")),
      ("datacenter", common::AttributeValue::string("dc1"))
    ]
  }
  assert_eq(full_resource.service_name, "full_service")
  assert_eq(full_resource.service_version.unwrap(), "2.1.0")
  assert_eq(full_resource.telemetry_sdk_name, "custom_sdk")
  assert_eq(full_resource.telemetry_sdk_version, "1.5.2")
  assert_eq(full_resource.attributes.length(), 2)
}

test "context_management_utilities" {
  // 测试上下文管理工具函数
  
  // 测试空上下文创建
  let empty_context = context::Context::empty()
  assert_eq(empty_context.values.length(), 0)
  
  // 测试ContextKey创建
  let key1 = context::create_key("test_key_1")
  let key2 = context::create_key("test_key_2")
  let key3 = context::create_key("test_key_3")
  assert_eq(key1.name, "test_key_1")
  assert_eq(key2.name, "test_key_2")
  assert_eq(key3.name, "test_key_3")
  
  // 测试上下文值设置
  let context_with_values = empty_context
    .with_value(key1, "value1")
    .with_value(key2, "value2")
    .with_value(key3, "value3")
  
  assert_eq(context_with_values.values.length(), 3)
  
  // 测试上下文值获取
  assert_eq(context_with_values.get(key1).unwrap(), "value1")
  assert_eq(context_with_values.get(key2).unwrap(), "value2")
  assert_eq(context_with_values.get(key3).unwrap(), "value3")
  
  // 测试不存在的键
  let non_existent_key = context::create_key("non_existent")
  assert_eq(context_with_values.get(non_existent_key), None)
  
  // 测试键值覆盖
  let context_with_override = context_with_values.with_value(key1, "new_value1")
  assert_eq(context_with_override.get(key1).unwrap(), "new_value1")
  assert_eq(context_with_override.values.length(), 3)  // 应该仍然是3个值
}

test "baggage_management_utilities" {
  // 测试baggage管理工具函数
  
  // 测试空baggage创建
  let empty_baggage = context::Baggage::empty()
  assert_eq(empty_baggage.entries.length(), 0)
  
  // 测试baggage条目添加
  let baggage_with_entries = empty_baggage
    .with_entry("user_id", "12345")
    .with_entry("session_id", "abcdef")
    .with_entry("request_id", "req789")
  
  assert_eq(baggage_with_entries.entries.length(), 3)
  
  // 测试baggage值获取
  assert_eq(baggage_with_entries.get("user_id").unwrap(), "12345")
  assert_eq(baggage_with_entries.get("session_id").unwrap(), "abcdef")
  assert_eq(baggage_with_entries.get("request_id").unwrap(), "req789")
  
  // 测试不存在的键
  assert_eq(baggage_with_entries.get("non_existent"), None)
  
  // 测试baggage条目覆盖
  let baggage_with_override = baggage_with_entries.with_entry("user_id", "67890")
  assert_eq(baggage_with_override.get("user_id").unwrap(), "67890")
  assert_eq(baggage_with_override.entries.length(), 3)  // 应该仍然是3个条目
  
  // 测试空键和空值
  let baggage_with_empty = empty_baggage
    .with_entry("", "empty_key_value")
    .with_entry("empty_value", "")
  
  assert_eq(baggage_with_empty.get("").unwrap(), "empty_key_value")
  assert_eq(baggage_with_empty.get("empty_value").unwrap(), "")
  assert_eq(baggage_with_empty.entries.length(), 2)
}

test "log_record_builder_utilities" {
  // 测试LogRecordBuilder工具函数
  
  // 测试builder创建
  let builder = logs::LogRecord::builder()
  assert_eq(builder.timestamp_unix_nanos, None)
  assert_eq(builder.severity_number, None)
  assert_eq(builder.body, None)
  assert_eq(builder.attributes.length(), 0)
  
  // 测试builder链式调用
  let built_log = builder
    .timestamp(1234567890000000L)
    .severity(logs::Error)
    .body("Test log message")
    .with_attribute("key1", common::AttributeValue::string("value1"))
    .with_attribute("key2", common::AttributeValue::int(42L))
    .with_attribute("key3", common::AttributeValue::bool(true))
    .build()
  
  assert_eq(built_log.timestamp_unix_nanos, 1234567890000000L)
  assert_eq(built_log.severity_number, logs::Error)
  assert_eq(built_log.body.unwrap(), "Test log message")
  assert_eq(built_log.attributes.length(), 3)
  
  // 验证属性值
  match built_log.attributes[0] {
    (key, common::StringValue(value)) => {
      assert_eq(key, "key1")
      assert_eq(value, "value1")
    }
    _ => @test.fail("Expected string attribute")
  }
  
  match built_log.attributes[1] {
    (key, common::IntValue(value)) => {
      assert_eq(key, "key2")
      assert_eq(value, 42L)
    }
    _ => @test.fail("Expected int attribute")
  }
  
  match built_log.attributes[2] {
    (key, common::BoolValue(value)) => {
      assert_eq(key, "key3")
      assert_eq(value, true)
    }
    _ => @test.fail("Expected bool attribute")
  }
  
  // 测试builder重用
  let reused_builder = logs::LogRecord::builder()
    .timestamp(9999999990000000L)
    .severity(logs::Info)
  
  let log1 = reused_builder
    .body("First log")
    .with_attribute("log_id", common::AttributeValue::int(1L))
    .build()
  
  let log2 = reused_builder
    .body("Second log")
    .with_attribute("log_id", common::AttributeValue::int(2L))
    .build()
  
  assert_eq(log1.body.unwrap(), "First log")
  assert_eq(log1.attributes.length(), 1)
  assert_eq(log2.body.unwrap(), "Second log")
  assert_eq(log2.attributes.length(), 1)
}

test "instrumentation_scope_utilities" {
  // 测试InstrumentationScope工具函数
  
  // 测试最小scope创建
  let minimal_scope = common::InstrumentationScope {
    name: "minimal_scope",
    version: None,
    schema_url: None
  }
  assert_eq(minimal_scope.name, "minimal_scope")
  assert_eq(minimal_scope.version, None)
  assert_eq(minimal_scope.schema_url, None)
  
  // 测试完整scope创建
  let full_scope = common::InstrumentationScope {
    name: "full_scope",
    version: Some("1.2.3"),
    schema_url: Some("http://example.com/schema")
  }
  assert_eq(full_scope.name, "full_scope")
  assert_eq(full_scope.version.unwrap(), "1.2.3")
  assert_eq(full_scope.schema_url.unwrap(), "http://example.com/schema")
  
  // 测试只有版本的scope
  let version_only_scope = common::InstrumentationScope {
    name: "version_scope",
    version: Some("2.0.0"),
    schema_url: None
  }
  assert_eq(version_only_scope.name, "version_scope")
  assert_eq(version_only_scope.version.unwrap(), "2.0.0")
  assert_eq(version_only_scope.schema_url, None)
  
  // 测试只有schema URL的scope
  let schema_only_scope = common::InstrumentationScope {
    name: "schema_scope",
    version: None,
    schema_url: Some("https://example.org/schema/v1")
  }
  assert_eq(schema_only_scope.name, "schema_scope")
  assert_eq(schema_only_scope.version, None)
  assert_eq(schema_only_scope.schema_url.unwrap(), "https://example.org/schema/v1")
}

test "span_context_utilities" {
  // 测试SpanContext工具函数
  
  // 测试基本span context创建
  let trace_id = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0, 
                  0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  let span_id = [0x12, 0x34, 0x56, 0x78, 0x9a, 0xbc, 0xde, 0xf0]
  
  let span_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 1_byte,
    trace_state: "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE"
  }
  
  assert_eq(span_context.trace_id.length(), 16)
  assert_eq(span_context.span_id.length(), 8)
  assert_eq(span_context.trace_flags, 1_byte)
  assert_eq(span_context.trace_state, "rojo=00f067aa0ba902b7,congo=t61rcWkgMzE")
  
  // 测试最小span context
  let minimal_trace_id = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte })
  let minimal_span_id = [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte })
  
  let minimal_span_context = trace::SpanContext::{
    trace_id: minimal_trace_id,
    span_id: minimal_span_id,
    trace_flags: 0_byte,
    trace_state: ""
  }
  
  assert_eq(minimal_span_context.trace_id.length(), 16)
  assert_eq(minimal_span_context.span_id.length(), 8)
  assert_eq(minimal_span_context.trace_flags, 0_byte)
  assert_eq(minimal_span_context.trace_state, "")
  
  // 测试最大trace flags
  let max_flags_context = trace::SpanContext::{
    trace_id: trace_id,
    span_id: span_id,
    trace_flags: 255_byte,
    trace_state: ""
  }
  assert_eq(max_flags_context.trace_flags, 255_byte)
}

test "attribute_array_utilities" {
  // 测试属性数组工具函数
  
  // 测试创建混合类型属性数组
  let mixed_attributes = [
    ("string_attr", common::AttributeValue::string("test")),
    ("int_attr", common::AttributeValue::int(42L)),
    ("float_attr", common::AttributeValue::float(3.14)),
    ("bool_attr", common::AttributeValue::bool(true)),
    ("string_array", common::AttributeValue::array_string(["a", "b"])),
    ("int_array", common::AttributeValue::array_int([1L, 2L])),
    ("float_array", common::AttributeValue::array_float([1.1, 2.2])),
    ("bool_array", common::AttributeValue::array_bool([true, false]))
  ]
  
  assert_eq(mixed_attributes.length(), 8)
  
  // 测试属性查找
  let find_string_attr = mixed_attributes.find(fn(attr) {
    match attr {
      (key, _) => key == "string_attr"
    }
  })
  
  match find_string_attr {
    Some((key, common::StringValue(value))) => {
      assert_eq(key, "string_attr")
      assert_eq(value, "test")
    }
    _ => @test.fail("Expected to find string attribute")
  }
  
  // 测试属性过滤
  let numeric_attributes = mixed_attributes.filter(fn(attr) {
    match attr {
      (_, common::IntValue(_)) => true
      (_, common::FloatValue(_)) => true
      _ => false
    }
  })
  
  assert_eq(numeric_attributes.length(), 2)
  
  // 测试属性映射
  let attribute_keys = mixed_attributes.map(fn(attr) {
    match attr {
      (key, _) => key
    }
  })
  
  assert_eq(attribute_keys.length(), 8)
  assert_eq(attribute_keys.contains("string_attr"), true)
  assert_eq(attribute_keys.contains("int_attr"), true)
}