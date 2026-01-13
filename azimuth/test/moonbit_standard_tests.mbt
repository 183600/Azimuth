// MoonBit 标准测试用例
// 测试 azimuth 库的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  let result = @azimuth.add(15, 27)
  @azimuth.assert_eq(42, result)
}

test "add_mixed_numbers" {
  // 测试正负数混合相加
  let result1 = @azimuth.add(100, -50)
  let result2 = @azimuth.add(-30, 70)
  @azimuth.assert_eq(50, result1)
  @azimuth.assert_eq(40, result2)
}

test "add_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  let result1 = @azimuth.add(max_val, -1)
  let result2 = @azimuth.add(min_val, 1)
  @azimuth.assert_eq(2147483646, result1)
  @azimuth.assert_eq(-2147483647, result2)
}

test "multiply_positive_numbers" {
  // 测试正数相乘
  let result = @azimuth.multiply(12, 8)
  @azimuth.assert_eq(96, result)
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  let result1 = @azimuth.multiply(-5, -7)
  let result2 = @azimuth.multiply(-3, 9)
  @azimuth.assert_eq(35, result1)
  @azimuth.assert_eq(-27, result2)
}

test "multiply_large_numbers" {
  // 测试大数相乘
  let result = @azimuth.multiply(12345, 678)
  @azimuth.assert_eq(8371410, result)
}

test "greet_with_various_names" {
  // 测试不同名称的问候
  let result1 = @azimuth.greet("Alice")
  let result2 = @azimuth.greet("Bob")
  let result3 = @azimuth.greet("世界")
  @azimuth.assert_eq_string("Hello, Alice!", result1)
  @azimuth.assert_eq_string("Hello, Bob!", result2)
  @azimuth.assert_eq_string("Hello, 世界!", result3)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的名称
  let result1 = @azimuth.greet("John Doe")
  let result2 = @azimuth.greet("user@domain.com")
  @azimuth.assert_eq_string("Hello, John Doe!", result1)
  @azimuth.assert_eq_string("Hello, user@domain.com!", result2)
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let a = 10
  let b = 20
  let c = 30
  
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(sum1, c)
  let product = @azimuth.multiply(a, b)
  let final_result = @azimuth.add(product, c)
  
  @azimuth.assert_eq(60, sum2)
  @azimuth.assert_eq(200, product)
  @azimuth.assert_eq(230, final_result)
}

test "edge_case_operations" {
  // 测试边缘情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值相加（应该被限制）
  let overflow_add = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_add)
  
  // 测试最小值相乘
  let min_multiply = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, min_multiply)
}