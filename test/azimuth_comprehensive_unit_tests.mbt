// Azimuth 综合单元测试
// 覆盖核心功能的边界情况和实际应用场景

test "add_sequence_operations" {
  // 测试连续加法运算
  let result = add(add(1, 2), add(3, 4))
  assert_eq(10, result)
}

test "multiply_nested_operations" {
  // 测试嵌套乘法运算
  let result = multiply(multiply(2, 3), multiply(4, 5))
  assert_eq(120, result)
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let result = multiply(add(5, 3), 2)
  assert_eq(16, result)
}

test "greet_with_empty_string" {
  // 测试空字符串问候
  assert_eq_string("Hello, !", greet(""))
}

test "greet_with_unicode" {
  // 测试 Unicode 字符串问候
  assert_eq_string("Hello, 世界!", greet("世界"))
}

test "divide_with_ceil_large_numbers" {
  // 测试大数的向上取整除法
  assert_eq(1000000, divide_with_ceil(999999, 1))
  assert_eq(500000, divide_with_ceil(1000000, 2))
}

test "divide_with_ceil_fractional_results" {
  // 测试产生小数结果的向上取整除法
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(7, 2))
  assert_eq(5, divide_with_ceil(9, 2))
}

test "divide_with_ceil_negative_dividend" {
  // 测试负被除数的向上取整除法
  assert_eq(-2, divide_with_ceil(-3, 2))
  assert_eq(-1, divide_with_ceil(-1, 2))
}

test "power_simulation_using_multiply" {
  // 使用乘法模拟幂运算
  let result = multiply(multiply(2, 2), multiply(2, 2))
  assert_eq(16, result)
}

test "factorial_simulation" {
  // 使用乘法模拟阶乘运算
  let result = multiply(multiply(1, 2), multiply(3, 4))
  assert_eq(24, result)
}