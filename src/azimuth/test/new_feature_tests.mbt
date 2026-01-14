// 新增功能测试用例 - 使用标准 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  let result = azimuth::add(-10, -20)
  @builtin.assert_eq(result, -30)
}

test "add_mixed_positive_negative" {
  // 测试正负数混合相加
  let result = azimuth::add(100, -50)
  @builtin.assert_eq(result, 50)
}

test "multiply_by_negative" {
  // 测试乘以负数
  let result = azimuth::multiply(15, -3)
  @builtin.assert_eq(result, -45)
}

test "multiply_two_negatives" {
  // 测试两个负数相乘
  let result = azimuth::multiply(-8, -7)
  @builtin.assert_eq(result, 56)
}

test "greet_with_multiline_name" {
  // 测试包含换行的名字
  let result = azimuth::greet("Line1\nLine2")
  @builtin.assert_eq(result, "Hello, Line1\nLine2!")
}

test "complex_calculation" {
  // 测试复杂计算：(5 + 3) * (10 - 4) = 8 * 6 = 48
  let sum = azimuth::add(5, 3)
  let diff = azimuth::add(10, -4)
  let result = azimuth::multiply(sum, diff)
  @builtin.assert_eq(result, 48)
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let result = azimuth::add(max_val, 0)
  @builtin.assert_eq(result, max_val)
}

test "boundary_value_multiplication" {
  // 测试边界值乘法
  let min_val = -2147483648
  let result = azimuth::multiply(min_val, 1)
  @builtin.assert_eq(result, min_val)
}

test "string_with_special_chars" {
  // 测试包含特殊字符的字符串
  let result = azimuth::greet("Test@#$%")
  @builtin.assert_eq(result, "Hello, Test@#$%!")
}

test "calculation_sequence" {
  // 测试计算序列：((2 + 3) * 4) + 5 = 20 + 5 = 25
  let step1 = azimuth::add(2, 3)
  let step2 = azimuth::multiply(step1, 4)
  let result = azimuth::add(step2, 5)
  @builtin.assert_eq(result, 25)
}