// 标准 MoonBit 测试用例集合
// 测试 azimuth 包的核心功能，使用标准的 MoonBit 测试语法

test "add_function_basic_cases" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(1, -2))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "add_function_overflow_cases" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "multiply_function_basic_cases" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "multiply_function_boundary_cases" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_function_unicode_cases" {
  // 测试Unicode字符处理
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "divide_with_ceil_basic_cases" {
  // 测试向上取整除法的基本情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-8, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))  // 除数为0的情况
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "complex_arithmetic_combinations" {
  // 测试复杂算术组合
  let sum_result = @azimuth.add(10, 20)
  let mul_result = @azimuth.multiply(5, 6)
  let combined = @azimuth.multiply(@azimuth.add(2, 3), 4)
  
  @azimuth.assert_eq(30, sum_result)
  @azimuth.assert_eq(30, mul_result)
  @azimuth.assert_eq(20, combined)
}

test "assertion_functions_verification" {
  // 测试断言函数
  @azimuth.assert_true(true)
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_false(false)
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
}