// 额外的 MoonBit 测试用例
// 覆盖更多边界情况和功能

test "add_mixed_numbers" {
  let result1 = @azimuth.add(-10, 20)
  let result2 = @azimuth.add(10, -20)
  @azimuth.assert_eq(10, result1)
  @azimuth.assert_eq(-10, result2)
}

test "add_min_value_handling" {
  let min_val = -2147483648
  let result1 = @azimuth.add(min_val, 0)
  let result2 = @azimuth.add(0, min_val)
  let result3 = @azimuth.add(min_val, 100)
  let result4 = @azimuth.add(100, min_val)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)
  @azimuth.assert_eq(min_val, result3)
  @azimuth.assert_eq(min_val, result4)
}

test "add_negative_overflow_protection" {
  let min_val = -2147483648
  let result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, result)
}

test "multiply_large_numbers" {
  let result = @azimuth.multiply(1000, 1000)
  @azimuth.assert_eq(1000000, result)
}

test "multiply_negative_numbers" {
  let result1 = @azimuth.multiply(-5, 5)
  let result2 = @azimuth.multiply(-5, -5)
  @azimuth.assert_eq(-25, result1)
  @azimuth.assert_eq(25, result2)
}

test "multiply_min_value_handling" {
  let min_val = -2147483648
  let result1 = @azimuth.multiply(min_val, 1)
  let result2 = @azimuth.multiply(min_val, -1)
  let result3 = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, result1)
  @azimuth.assert_eq(min_val, result2)  // 溢出保护
  @azimuth.assert_eq(min_val, result3)  // 溢出保护
}

test "multiply_overflow_protection" {
  let max_val = 2147483647
  let result = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, result)  // 溢出保护
}

test "greet_special_characters" {
  let result1 = @azimuth.greet("张三")
  let result2 = @azimuth.greet("!@#$%^&*()")
  @azimuth.assert_eq_string("Hello, 张三!", result1)
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", result2)
}

test "complex_arithmetic_combinations" {
  // 测试复合运算
  let temp1 = @azimuth.add(10, 20)
  let temp2 = @azimuth.multiply(temp1, 3)
  let result = @azimuth.add(temp2, -90)
  @azimuth.assert_eq(0, result)
}