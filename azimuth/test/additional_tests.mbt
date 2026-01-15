// 额外的 MoonBit 测试用例
// 这些测试用例覆盖 azimuth 模块的核心功能和边界情况

// 导入主模块中的函数
use azimuth

test "add_zero_handling" {
  // 测试加法中的零值处理
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(-17, add(0, -17))
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(5, add(-3, 8))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-25, 25))
}

test "multiply_zero_handling" {
  // 测试乘法中的零值处理
  assert_eq(0, multiply(42, 0))
  assert_eq(0, multiply(0, 42))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-17, 0))
  assert_eq(0, multiply(0, -17))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(100, multiply(-10, -10))
  assert_eq(-100, multiply(-10, 10))
}

test "multiply_by_one" {
  // 测试乘以1的情况
  assert_eq(42, multiply(42, 1))
  assert_eq(42, multiply(1, 42))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(-17, multiply(1, -17))
  assert_eq(0, multiply(0, 1))
  assert_eq(0, multiply(1, 0))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的情况
  assert_eq(-42, multiply(42, -1))
  assert_eq(-42, multiply(-1, 42))
  assert_eq(17, multiply(-17, -1))
  assert_eq(17, multiply(-1, -17))
  assert_eq(0, multiply(0, -1))
  assert_eq(0, multiply(-1, 0))
}

test "greet_function_special_cases" {
  // 测试greet函数的特殊情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 12345!", greet("12345"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, line\nbreak!", greet("line\nbreak"))
}

test "complex_arithmetic_expressions" {
  // 测试复杂的算术表达式
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(900, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(230, result2)
  
  // (a + c) * (b - a)
  let result3 = multiply(add(a, c), add(b, -a))
  assert_eq(400, result3)
}

test "extreme_boundary_values" {
  // 测试极值边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值加法
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 最小值加法
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 最大值乘法
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(max_val, multiply(1, max_val))
  assert_eq(min_val, multiply(max_val, -1))
  assert_eq(min_val, multiply(-1, max_val))
  
  // 最小值乘法
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(min_val, multiply(1, min_val))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(-1, min_val))
}