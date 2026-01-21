// 简单的 MoonBit 测试用例
// 针对 azimuth 包的核心功能测试

test "enhanced_add_basic" {
  // 增强的加法基本测试
  assert_eq(15, add(7, 8))
  assert_eq(-3, add(5, -8))
  assert_eq(0, add(100, -100))
}

test "enhanced_multiply_edge_cases" {
  // 增强的乘法边界情况测试
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  assert_eq(-12345, multiply(12345, -1))
}

test "enhanced_greet_variations" {
  // 增强的问候函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "enhanced_complex_calculation" {
  // 增强的复杂计算测试
  let base = 100
  let multiplier = 3
  let addition = 50
  
  // 计算 (base * multiplier) + addition
  let step1 = multiply(base, multiplier)
  let result = add(step1, addition)
  
  assert_eq(350, result)
}

test "enhanced_boundary_combinations" {
  // 增强的边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}