// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_positive_numbers" {
  // 测试正数加法
  let result = azimuth::add(15, 25)
  assert_eq(40, result)
}

test "add_negative_numbers" {
  // 测试负数加法
  let result = azimuth::add(-10, -20)
  assert_eq(-30, result)
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  let result = azimuth::add(50, -30)
  assert_eq(20, result)
}

test "multiply_positive_numbers" {
  // 测试正数乘法
  let result = azimuth::multiply(8, 7)
  assert_eq(56, result)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  let result = azimuth::multiply(-6, -9)
  assert_eq(54, result)
}

test "multiply_mixed_numbers" {
  // 测试正负数混合乘法
  let result = azimuth::multiply(12, -4)
  assert_eq(-48, result)
}

test "greet_standard_name" {
  // 测试标准名称问候
  let result = azimuth::greet("Alice")
  assert_eq_string("Hello, Alice!", result)
}

test "greet_numeric_string" {
  // 测试数字字符串问候
  let result = azimuth::greet("123")
  assert_eq_string("Hello, 123!", result)
}

test "complex_arithmetic_operation" {
  // 测试复合算术运算：(a + b) * c
  let a = 5
  let b = 3
  let c = 4
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  assert_eq(32, result)
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加法溢出保护
  let overflow_result = azimuth::add(max_val, 1)
  assert_eq(max_val, overflow_result)
  
  // 测试最小值乘法
  let min_multiply = azimuth::multiply(min_val, 1)
  assert_eq(min_val, min_multiply)
}