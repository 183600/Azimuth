// MoonBit 核心模块标准测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 基本加法功能测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(-2, 2))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_boundary_values" {
  // 边界值加法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(max_val, add(0, max_val))
  assert_eq(min_val, add(0, min_val))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  assert_eq(6, multiply(2, 3))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(5, multiply(5, 1))
}

test "multiply_boundary_values" {
  // 边界值乘法测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
}

test "greet_functionality" {
  // greet 函数功能测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-8, 3))
}

test "assert_functions" {
  // 断言函数测试
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
}

test "complex_operations" {
  // 复杂运算测试
  let a = 10
  let b = 3
  
  // 加法后乘法
  assert_eq(39, multiply(add(a, b), 3))
  
  // 乘法后加法
  assert_eq(33, add(multiply(a, b), 3))
  
  // 复杂表达式
  assert_eq(16, add(multiply(2, 5), divide_with_ceil(10, 3)))
}

test "mathematical_properties" {
  // 数学性质测试
  
  // 加法结合律（在安全范围内）
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))
  
  // 乘法结合律（在安全范围内）
  assert_eq(multiply(multiply(2, 3), 4), multiply(2, multiply(3, 4)))
  
  // 分配律（在安全范围内）
  assert_eq(multiply(2, add(3, 4)), add(multiply(2, 3), multiply(2, 4)))
}