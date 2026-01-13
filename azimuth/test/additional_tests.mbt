// 额外的 MoonBit 测试用例
// 覆盖更多边界情况和功能

// 测试加法的最小值边界情况
test "add_with_min_value" {
  let min_val = -2147483648
  let result1 = @azimuth.add(min_val, 0)
  let result2 = @azimuth.add(0, min_val)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)
}

// 测试加法的最小值与正数
test "add_min_value_with_positive" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, 100)
  @azimuth.assert_eq(-2147483548, result)
}

// 测试加法的最小值与负数（应该返回最小值）
test "add_min_value_with_negative" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, result)
}

// 测试乘法的最小值边界情况
test "multiply_min_value_by_one" {
  let min_val = -2147483648
  let result1 = @azimuth.multiply(min_val, 1)
  let result2 = @azimuth.multiply(1, min_val)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)
}

// 测试乘法的最小值与-1（应该返回最小值，因为2147483648会溢出）
test "multiply_min_value_by_negative_one" {
  let min_val = -2147483648
  let result1 = @azimuth.multiply(min_val, -1)
  let result2 = @azimuth.multiply(-1, min_val)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)
}

// 测试乘法的溢出保护
test "multiply_overflow_protection" {
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result)
}

// 测试乘法的负溢出保护
test "multiply_negative_overflow_protection" {
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, -2)
  @azimuth.assert_eq(-2147483648, result)
}

// 测试字符串处理：长字符串
test "greet_with_long_string" {
  let long_name = "这是一个很长的名字用来测试字符串处理功能"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, 这是一个很长的名字用来测试字符串处理功能!", result)
}

// 测试字符串处理：特殊字符
test "greet_with_special_characters" {
  let special_name = "世界@#$%^&*()"
  let result = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, 世界@#$%^&*()!", result)
}

// 测试加法的混合符号
test "add_mixed_signs" {
  let result1 = @azimuth.add(100, -50)
  let result2 = @azimuth.add(-100, 50)
  @azimuth.assert_eq(50, result1)
  @azimuth.assert_eq(-50, result2)
}