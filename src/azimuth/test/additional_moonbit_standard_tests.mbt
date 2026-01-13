// 额外的 MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "addition_with_positive_numbers" {
  // 测试正数相加
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(100, azimuth::add(50, 50))
  assert_eq(0, azimuth::add(0, 0))
}

test "addition_with_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(-5, azimuth::add(-2, -3))
  assert_eq(0, azimuth::add(-5, 5))
}

test "addition_with_mixed_numbers" {
  // 测试正负数混合相加
  assert_eq(0, azimuth::add(10, -10))
  assert_eq(-3, azimuth::add(5, -8))
  assert_eq(7, azimuth::add(-3, 10))
}

test "multiplication_with_positive_numbers" {
  // 测试正数相乘
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(100, azimuth::multiply(10, 10))
  assert_eq(0, azimuth::multiply(0, 5))
}

test "multiplication_with_negative_numbers" {
  // 测试负数相乘
  assert_eq(15, azimuth::multiply(-3, -5))
  assert_eq(-15, azimuth::multiply(3, -5))
  assert_eq(0, azimuth::multiply(-5, 0))
}

test "greet_function_with_different_names" {
  // 测试 greet 函数与不同的名字
  assert_eq_string("Hello, Alice!", azimuth::greet("Alice"))
  assert_eq_string("Hello, Bob!", azimuth::greet("Bob"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
}

test "assert_functions_functionality" {
  // 测试断言函数的功能
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let a = 5
  let b = 3
  let c = 2
  
  // 计算 (a + b) * c
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  assert_eq(16, result)
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值与0的运算
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
}

test "string_handling_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, A!", azimuth::greet("A"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}