// 质量增强测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边界情况
  assert_eq(10, subtract(20, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(-5, subtract(10, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_comprehensive_test" {
  // 全面测试向上取整除法
  // 正数情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数情况
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-11, -3))
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}