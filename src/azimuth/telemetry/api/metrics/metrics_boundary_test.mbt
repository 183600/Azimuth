test "metric_instrument_edge_cases" {
  // 测试metric instruments的边界情况
  
  // 测试极值
  let max_int_value = 9223372036854775807L
  let min_int_value = -9223372036854775808L
  let max_float_value = 1.7976931348623157e+308  // f64::MAX
  let min_float_value = -1.7976931348623157e+308 // f64::MIN
  let inf_value = 1.0/0.0
  let neg_inf_value = -1.0/0.0
  let nan_value = 0.0/0.0
  
  // 创建包含极值的measurement
  let max_int_measurement = Measurement::{
    value: max_int_value.to_float(),
    attributes: [("type", AttributeValue::string("max_int"))]
  }
  
  let min_int_measurement = Measurement::{
    value: min_int_value.to_float(),
    attributes: [("type", AttributeValue::string("min_int"))]
  }
  
  let inf_measurement = Measurement::{
    value: inf_value,
    attributes: [("type", AttributeValue::string("infinity"))]
  }
  
  let nan_measurement = Measurement::{
    value: nan_value,
    attributes: [("type", AttributeValue::string("nan"))]
  }
  
  // 验证极值
  assert_eq(max_int_measurement.value > 0.0, true)
  assert_eq(min_int_measurement.value < 0.0, true)
  assert_eq(inf_measurement.value.is_inf(), true)
  assert_eq(inf_measurement.value > 0.0, true)
  assert_eq(nan_measurement.value.is_nan(), true)
}

test "metric_attributes_complex_scenarios" {
  // 测试metric属性的复杂场景
  
  // 测试大量属性
  let many_attrs : Attributes = [
    ("attr1", AttributeValue::string("value1")),
    ("attr2", AttributeValue::string("value2")),
    ("attr3", AttributeValue::string("value3")),
    ("attr4", AttributeValue::string("value4")),
    ("attr5", AttributeValue::string("value5")),
    ("attr6", AttributeValue::string("value6")),
    ("attr7", AttributeValue::string("value7")),
    ("attr8", AttributeValue::string("value8")),
    ("attr9", AttributeValue::string("value9")),
    ("attr10", AttributeValue::string("value10"))
  ]
  
  assert_eq(many_attrs.length(), 10)
  
  // 测试属性值的复杂类型
  let complex_attrs : Attributes = [
    ("string_array", AttributeValue::array_string(["a", "b", "c", "d", "e"])),
    ("int_array", AttributeValue::array_int([1L, 2L, 3L, 4L, 5L])),
    ("float_array", AttributeValue::array_float([1.1, 2.2, 3.3, 4.4, 5.5])),
    ("bool_array", AttributeValue::array_bool([true, false, true, false, true])),
    ("nested", AttributeValue::string("complex.value.with.dots")),
    ("unicode", AttributeValue::string("测试中文🌟")),
    ("special", AttributeValue::string("!@#$%^&*()_+-=[]{}|;':\",./<>?"))
  ]
  
  assert_eq(complex_attrs.length(), 7)
  
  // 验证字符串数组
  match complex_attrs[0].1 {
    ArrayStringValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], "a")
      assert_eq(arr[4], "e")
    }
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 验证整数数组
  match complex_attrs[1].1 {
    ArrayIntValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1L)
      assert_eq(arr[4], 5L)
    }
    _ => @test.fail("Expected ArrayIntValue")
  }
  
  // 验证浮点数组
  match complex_attrs[2].1 {
    ArrayFloatValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], 1.1)
      assert_eq(arr[4], 5.5)
    }
    _ => @test.fail("Expected ArrayFloatValue")
  }
  
  // 验证布尔数组
  match complex_attrs[3].1 {
    ArrayBoolValue(arr) => {
      assert_eq(arr.length(), 5)
      assert_eq(arr[0], true)
      assert_eq(arr[1], false)
      assert_eq(arr[4], true)
    }
    _ => @test.fail("Expected ArrayBoolValue")
  }
}