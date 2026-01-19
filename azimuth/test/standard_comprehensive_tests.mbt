// 标准 MoonBit 测试用例
// 覆盖 azimuth 包的核心功能

test "add_function_basic_operations" {
  // 测试加法函数的基本操作
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-3, 3))
  @azimuth.assert_eq(-10, @azimuth.add(-7, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_boundary_values" {
  // 测试加法函数的边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-42, @azimuth.add(-42, 0))
}

test "multiply_function_basic_operations" {
  // 测试乘法函数的基本操作
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(8, @azimuth.multiply(-2, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
}

test "multiply_function_special_cases" {
  // 测试乘法函数的特殊情况
  @azimuth.assert_eq(25, @azimuth.multiply(25, 1))
  @azimuth.assert_eq(-25, @azimuth.multiply(25, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
}

test "divide_with_ceil_basic_operations" {
  // 测试向上取整除法函数的基本操作
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法函数的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(5, 5))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(6, 3))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  let sum = @azimuth.add(15, 25)
  let product = @azimuth.multiply(4, 6)
  @azimuth.assert_eq(40, sum)
  @azimuth.assert_eq(24, product)
  
  // 组合运算
  let combined = @azimuth.multiply(@azimuth.add(2, 3), @azimuth.add(4, 5))
  @azimuth.assert_eq(45, combined)
}

test "assertion_functions_verification" {
  // 测试断言函数的功能
  @azimuth.assert_true(5 > 3)
  @azimuth.assert_true(0 == 0)
  @azimuth.assert_false(5 < 3)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}