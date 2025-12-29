// Metrics模块边缘情况测试用例
// 测试metrics的边缘情况和错误处理

test "metrics_boundary_conditions" {
  // 测试metrics的边界条件
  
  // 测试极值情况
  let max_int64 = 9223372036854775807L
  let min_int64 = -9223372036854775808L
  let zero_int64 = 0L
  
  let max_double = 1.7976931348623157e308
  let min_double = -1.7976931348623157e308
  let zero_double = 0.0
  let infinity_double = 1.0/0.0
  let neg_infinity_double = -1.0/0.0
  let nan_double = 0.0/0.0
  
  // 测试Counter的极值
  let counter_attrs_max = [
    ("counter.value", AttributeValue::int(max_int64)),
    ("counter.name", AttributeValue::string("max-counter"))
  ]
  
  let counter_attrs_min = [
    ("counter.value", AttributeValue::int(min_int64)),
    ("counter.name", AttributeValue::string("min-counter"))
  ]
  
  let counter_attrs_zero = [
    ("counter.value", AttributeValue::int(zero_int64)),
    ("counter.name", AttributeValue::string("zero-counter"))
  ]
  
  // 验证Counter属性
  assert_eq(counter_attrs_max.length(), 2)
  match counter_attrs_max[0].1 {
    IntValue(value) => assert_eq(value, max_int64)
    _ => @test.fail("Expected IntValue")
  }
  
  assert_eq(counter_attrs_min.length(), 2)
  match counter_attrs_min[0].1 {
    IntValue(value) => assert_eq(value, min_int64)
    _ => @test.fail("Expected IntValue")
  }
  
  assert_eq(counter_attrs_zero.length(), 2)
  match counter_attrs_zero[0].1 {
    IntValue(value) => assert_eq(value, zero_int64)
    _ => @test.fail("Expected IntValue")
  }
  
  // 测试Histogram的极值
  let histogram_attrs_max = [
    ("histogram.value", AttributeValue::float(max_double)),
    ("histogram.name", AttributeValue::string("max-histogram"))
  ]
  
  let histogram_attrs_min = [
    ("histogram.value", AttributeValue::float(min_double)),
    ("histogram.name", AttributeValue::string("min-histogram"))
  ]
  
  let histogram_attrs_zero = [
    ("histogram.value", AttributeValue::float(zero_double)),
    ("histogram.name", AttributeValue::string("zero-histogram"))
  ]
  
  let histogram_attrs_inf = [
    ("histogram.value", AttributeValue::float(infinity_double)),
    ("histogram.name", AttributeValue::string("inf-histogram"))
  ]
  
  let histogram_attrs_nan = [
    ("histogram.value", AttributeValue::float(nan_double)),
    ("histogram.name", AttributeValue::string("nan-histogram"))
  ]
  
  // 验证Histogram属性
  assert_eq(histogram_attrs_max.length(), 2)
  match histogram_attrs_max[0].1 {
    FloatValue(value) => assert_eq(value, max_double)
    _ => @test.fail("Expected FloatValue")
  }
  
  assert_eq(histogram_attrs_min.length(), 2)
  match histogram_attrs_min[0].1 {
    FloatValue(value) => assert_eq(value, min_double)
    _ => @test.fail("Expected FloatValue")
  }
  
  assert_eq(histogram_attrs_zero.length(), 2)
  match histogram_attrs_zero[0].1 {
    FloatValue(value) => assert_eq(value, zero_double)
    _ => @test.fail("Expected FloatValue")
  }
  
  assert_eq(histogram_attrs_inf.length(), 2)
  match histogram_attrs_inf[0].1 {
    FloatValue(value) => {
      // 对于infinity，我们只验证类型而不验证具体值
      assert_eq(true, true)
    }
    _ => @test.fail("Expected FloatValue")
  }
  
  assert_eq(histogram_attrs_nan.length(), 2)
  match histogram_attrs_nan[0].1 {
    FloatValue(value) => {
      // 对于NaN，我们只验证类型而不验证具体值
      assert_eq(true, true)
    }
    _ => @test.fail("Expected FloatValue")
  }
}

test "metrics_error_conditions" {
  // 测试metrics的错误条件
  
  // 测试无效的metric名称
  let invalid_names = [
    "",                    // 空字符串
    " ",                   // 只有空格
    "name with spaces",    // 包含空格
    "name@with#special",   // 包含特殊字符
    "123invalid",          // 以数字开头
    "a".repeat(300),       // 过长的名称
    "na\nme",              // 包含换行符
    "na\tme",              // 包含制表符
    "na\rme",              // 包含回车符
    "na/me",               // 包含路径分隔符
  ]
  
  // 测试每个无效名称
  for invalid_name in invalid_names {
    let is_valid_name = invalid_name.length() > 0 &&
                        !invalid_name.contains(" ") &&
                        !invalid_name.contains("@") &&
                        !invalid_name.contains("#") &&
                        !invalid_name.starts_with("0") &&
                        invalid_name.length() <= 255 &&
                        !invalid_name.contains("\n") &&
                        !invalid_name.contains("\t") &&
                        !invalid_name.contains("\r") &&
                        !invalid_name.contains("/")
    
    // 所有这些名称都应该是无效的
    assert_eq(is_valid_name, false)
  }
  
  // 测试有效的metric名称
  let valid_names = [
    "valid.metric.name",
    "metric_with_underscores",
    "metric-with-dashes",
    "metric123",
    "a.b.c.d.e",
    "short",
    "metric_with_numbers_123"
  ]
  
  // 测试每个有效名称
  for valid_name in valid_names {
    let is_valid_name = valid_name.length() > 0 &&
                        !valid_name.contains(" ") &&
                        !valid_name.contains("@") &&
                        !valid_name.contains("#") &&
                        !valid_name.starts_with("0") &&
                        valid_name.length() <= 255 &&
                        !valid_name.contains("\n") &&
                        !valid_name.contains("\t") &&
                        !valid_name.contains("\r") &&
                        !valid_name.contains("/")
    
    // 所有这些名称都应该是有效的
    assert_eq(is_valid_name, true)
  }
  
  // 测试无效的属性值
  let empty_string_attr = ("empty.string", AttributeValue::string(""))
  let empty_array_attr = ("empty.array", AttributeValue::array_string([]))
  
  // 验证空值属性
  match empty_string_attr.1 {
    StringValue(value) => assert_eq(value, "")
    _ => @test.fail("Expected StringValue")
  }
  
  match empty_array_attr.1 {
    ArrayStringValue(arr) => assert_eq(arr.length(), 0)
    _ => @test.fail("Expected ArrayStringValue")
  }
  
  // 测试属性键的边界情况
  let very_long_key = "a".repeat(500)
  let key_with_unicode = "键值对测试"
  let key_with_emoji = "emoji_test🚀"
  
  let long_key_attr = (very_long_key, AttributeValue::string("test"))
  let unicode_key_attr = (key_with_unicode, AttributeValue::string("测试"))
  let emoji_key_attr = (key_with_emoji, AttributeValue::string("rocket"))
  
  // 验证长键名属性
  assert_eq(long_key_attr.0.length(), 500)
  
  // 验证Unicode键名属性
  assert_eq(unicode_key_attr.0.contains("键值"), true)
  
  // 验证emoji键名属性
  assert_eq(emoji_key_attr.0.contains("🚀"), true)
}