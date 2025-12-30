// 属性值转换的类型安全测试
// 测试各种属性值类型的转换和类型安全性

test "attribute_string_value_type_safety" {
  // 测试字符串属性值的类型安全
  let normal_string = @azimuth.telemetry.api.common.AttributeValue::string("normal_string")
  let empty_string = @azimuth.telemetry.api.common.AttributeValue::string("")
  let unicode_string = @azimuth.telemetry.api.common.AttributeValue::string("测试字符串")
  let special_chars_string = @azimuth.telemetry.api.common.AttributeValue::string("special!@#$%^&*()")
  
  // 验证所有字符串值都是StringValue类型
  match normal_string {
    @azimuth.telemetry.api.common.AttributeValue::StringValue(_) => assert_eq(true, true)
    _ => assert_eq(true, false)
  }
  
  match empty_string {
    @azimuth.telemetry.api.common.AttributeValue::StringValue("") => assert_eq(true, true)
    _ => assert_eq(true, false)
  }
  
  match unicode_string {
    @azimuth.telemetry.api.common.AttributeValue::StringValue("测试字符串") => assert_eq(true, true)
    _ => assert_eq(true, false)
  }
}