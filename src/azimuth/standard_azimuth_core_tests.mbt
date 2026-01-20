// 标准 Azimuth 核心测试用例
// 测试项目的核心功能，包括数学运算、字符串处理和断言功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 测试负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
  
  // 测试边界情况
  assert_eq(-1, add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(0, 5))
  assert_eq(5, multiply(5, 1))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(max_val, multiply(46341, 46341))
  
  // 测试负数溢出
  assert_eq(min_val, multiply(min_val, 2))
  assert_eq(min_val, multiply(max_val, -1))
  
  // 测试最小值特殊处理
  assert_eq(min_val, multiply(min_val, -1))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_special_characters" {
  // 测试特殊字符处理
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, user@123!", greet("user@123"))
  assert_eq_string("Hello, \"John\"!", greet("\"John\""))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(4, divide_with_ceil(12, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-3, divide_with_ceil(-11, 3))
}

test "assertion_functions" {
  // 测试断言函数
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("test", "test")
  
  // 测试数学表达式
  assert_true(add(2, 3) == 5)
  assert_false(add(2, 3) == 6)
  assert_eq(multiply(4, 5), 20)
}

test "complex_calculation_scenario" {
  // 测试复合计算场景
  let length = 8
  let width = 6
  
  // 计算矩形周长：2 × (length + width)
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
  
  // 计算矩形面积：length × width
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算总边长：perimeter + diagonal (简化计算)
  let diagonal_approx = add(length, width)
  let total_edge = add(perimeter, diagonal_approx)
  assert_eq(42, total_edge)
}

test "boundary_value_combinations" {
  // 测试边界值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值与0的运算
  assert_eq(max_val, add(max_val, 0))
  assert_eq(min_val, add(min_val, 0))
  assert_eq(0, multiply(max_val, 0))
  assert_eq(0, multiply(min_val, 0))
  
  // 边界值与1的运算
  assert_eq(max_val, multiply(max_val, 1))
  assert_eq(min_val, multiply(min_val, 1))
  
  // 边界值与-1的运算
  assert_eq(min_val, multiply(max_val, -1))
  assert_eq(min_val, multiply(min_val, -1))
  
  // 边界值自身运算
  assert_eq(max_val, add(max_val, min_val))
}