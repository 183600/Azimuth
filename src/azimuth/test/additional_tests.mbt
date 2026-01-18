// 额外的 MoonBit 测试用例

test "add_negative_numbers" {
  let result = azimuth::add(-5, -3)
  azimuth::assert_eq(-8, result)
}

test "add_mixed_numbers" {
  let result = azimuth::add(10, -7)
  azimuth::assert_eq(3, result)
}

test "multiply_negative_numbers" {
  let result = azimuth::multiply(-4, -6)
  azimuth::assert_eq(24, result)
}

test "multiply_mixed_numbers" {
  let result = azimuth::multiply(8, -3)
  azimuth::assert_eq(-24, result)
}

test "greet_with_special_characters" {
  let result = azimuth::greet("测试用户123")
  azimuth::assert_eq_string("Hello, 测试用户123!", result)
}

test "add_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与0相加
  let result1 = azimuth::add(max_val, 0)
  azimuth::assert_eq(max_val, result1)
  
  // 测试最小值与0相加
  let result2 = azimuth::add(min_val, 0)
  azimuth::assert_eq(min_val, result2)
}

test "multiply_with_zero" {
  // 测试任何数与0相乘
  let result1 = azimuth::multiply(12345, 0)
  azimuth::assert_eq(0, result1)
  
  let result2 = azimuth::multiply(-6789, 0)
  azimuth::assert_eq(0, result2)
}

test "complex_calculation" {
  // 测试复合计算：(5 + 3) * (10 - 4)
  let sum = azimuth::add(5, 3)
  let diff = azimuth::add(10, -4)
  let result = azimuth::multiply(sum, diff)
  azimuth::assert_eq(48, result)
}