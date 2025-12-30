// 数据序列化兼容性高级测试
// 测试遥测数据的序列化和反序列化兼容性

test "attribute_value_serialization_compatibility" {
  // 测试属性值的序列化兼容性
  let test_attributes = [
    ("string_attr", @azimuth.telemetry.api.common.AttributeValue::string("test_string")),
    ("empty_string_attr", @azimuth.telemetry.api.common.AttributeValue::string("")),
    ("unicode_string_attr", @azimuth.telemetry.api.common.AttributeValue::string("测试字符串")),
    ("int_attr", @azimuth.telemetry.api.common.AttributeValue::int(42L)),
    ("zero_int_attr", @azimuth.telemetry.api.common.AttributeValue::int(0L)),
    ("negative_int_attr", @azimuth.telemetry.api.common.AttributeValue::int(-42L)),
    ("float_attr", @azimuth.telemetry.api.common.AttributeValue::float(3.14159)),
    ("zero_float_attr", @azimuth.telemetry.api.common.AttributeValue::float(0.0)),
    ("bool_true_attr", @azimuth.telemetry.api.common.AttributeValue::bool(true)),
    ("bool_false_attr", @azimuth.telemetry.api.common.AttributeValue::bool(false))
  ]
  
  // 模拟序列化过程
  let serialized_data = test_attributes.map(fn(attr) {
    let key = attr[0]
    let value_str = match attr[1] {
      @azimuth.telemetry.api.common.AttributeValue::StringValue(s) => "string:" + s
      @azimuth.telemetry.api.common.AttributeValue::IntValue(i) => "int:" + i.to_string()
      @azimuth.telemetry.api.common.AttributeValue::FloatValue(f) => "float:" + f.to_string()
      @azimuth.telemetry.api.common.AttributeValue::BoolValue(b) => "bool:" + b.to_string()
      _ => "unknown"
    }
    (key, value_str)
  })
  
  // 验证序列化数据
  assert_eq(serialized_data.length(), test_attributes.length())
  
  // 验证字符串属性的序列化
  let string_serialized = serialized_data.find(fn(attr) { attr[0] == "string_attr" })
  match string_serialized {
    Some((_, "string:test_string")) => assert_eq(true, true)
    _ => assert_eq(true, false)
  }
}