// 新增的 MoonBit 测试用例
// 覆盖更多边界情况和功能测试

test "add_min_value_handling" {
  // 测试最小值处理
  let min_val = -2147483648
  let result1 = @azimuth.add(min_val, 0)
  let result2 = @azimuth.add(0, min_val)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)
}

test "add_mixed_signs" {
  // 测试正负数混合相加
  let result1 = @azimuth.add(100, -50)
  let result2 = @azimuth.add(-100, 50)
  @azimuth.assert_eq(50, result1)
  @azimuth.assert_eq(-50, result2)
}

test "add_underflow_protection" {
  // 测试下溢保护
  let min_val = -2147483648
  let result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

test "multiply_min_value_special_cases" {
  // 测试最小值乘法的特殊情况
  let min_val = -2147483648
  let result1 = @azimuth.multiply(min_val, 1)
  let result2 = @azimuth.multiply(min_val, -1)
  let result3 = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)  // 溢出保护
  @azimuth.assert_eq(min_val, result3)  // 溢出保护
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let result1 = @azimuth.multiply(10000, 100000)
  let result2 = @azimuth.multiply(-10000, 100000)
  @azimuth.assert_eq(1000000000, result1)
  @azimuth.assert_eq(-1000000000, result2)
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result)
}

test "greet_special_characters" {
  // 测试包含特殊字符的字符串
  let result1 = @azimuth.greet("世界")
  let result2 = @azimuth.greet("123!@#")
  @azimuth.assert_eq_string("Hello, 世界!", result1)
  @azimuth.assert_eq_string("Hello, 123!@#!", result2)
}

test "greet_long_string" {
  // 测试长字符串
  let long_name = "This is a very long name that contains multiple words and spaces"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, This is a very long name that contains multiple words and spaces!", result)
}

test "complex_arithmetic_combination" {
  // 测试复合算术运算
  let a = 100
  let b = 200
  let c = -50
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(sum1, c)
  let product = @azimuth.multiply(sum2, 2)
  
  @azimuth.assert_eq(300, sum1)
  @azimuth.assert_eq(250, sum2)
  @azimuth.assert_eq(500, product)
}