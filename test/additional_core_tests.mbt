// 额外的核心功能测试用例
// 测试 azimuth 包的核心功能

test "additional_negative_addition" {
  // 测试负数加法
  assert_eq(-5, add(-2, -3))
  assert_eq(-10, add(-7, -3))
  assert_eq(0, add(-5, 5))
  assert_eq(-100, add(-150, 50))
}

test "additional_large_multiplication" {
  // 测试大数乘法
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(86400000, multiply(86400, 1000))
  assert_eq(-1000000, multiply(-1000, 1000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "additional_unicode_greeting" {
  // 测试Unicode字符串处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ✓✗!", greet("✓✗"))
}

test "additional_mathematical_series" {
  // 测试数学级数计算
  // 等差数列求和：1 + 2 + 3 + 4 + 5 = 15
  let sum_1_to_5 = add(add(add(add(1, 2), 3), 4), 5)
  assert_eq(15, sum_1_to_5)
  
  // 等比数列求和：2 + 4 + 8 + 16 = 30
  let sum_powers = add(add(add(2, 4), 8), 16)
  assert_eq(30, sum_powers)
}

test "additional_boundary_values" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与1的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(max_val, multiply(max_val, 1))
  
  // 最小值与1的运算
  assert_eq(min_val, add(min_val, 0))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 边界值相加
  assert_eq(-1, add(max_val, min_val))
}

test "additional_complex_strings" {
  // 测试复杂字符串场景
  // 数字字符串
  assert_eq_string("Hello, 2023!", greet("2023"))
  
  // 特殊符号组合
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  
  // 混合内容
  assert_eq_string("Hello, Test123!", greet("Test123"))
  
  // 空格处理
  assert_eq_string("Hello, Leading Space!", greet(" Leading Space"))
}

test "additional_calculation_precision" {
  // 测试计算精度
  // 连续加法
  let result = add(add(add(1, 2), 3), 4)
  assert_eq(10, result)
  
  // 连续乘法
  let product = multiply(multiply(2, 3), 4)
  assert_eq(24, product)
  
  // 混合运算
  let mixed = add(multiply(2, 3), multiply(4, 5))
  assert_eq(26, mixed)
}