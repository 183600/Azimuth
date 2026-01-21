// 标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_functionality" {
  // 测试加法函数的基本功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_function_basic_functionality" {
  // 测试乘法函数的基本功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "greet_function_basic_functionality" {
  // 测试问候函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "divide_with_ceil_function_basic_functionality" {
  // 测试向上取整除法函数的基本功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(9, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
}

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(1, @azimuth.add(max_val, min_val + 1))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))
}

test "greet_function_special_characters" {
  // 测试问候函数的特殊字符处理
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "complex_arithmetic_operations" {
  // 测试复杂的算术运算组合
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(sum, 3)
  @azimuth.assert_eq(90, product)
  
  let another_sum = @azimuth.add(@azimuth.multiply(5, 4), @azimuth.multiply(3, 2))
  @azimuth.assert_eq(26, another_sum)
}

test "assertion_functions_test" {
  // 测试断言函数
  @azimuth.assert_eq(5, 5)
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(1, 1)
  @azimuth.assert_eq_string("test", "test")
  @azimuth.assert_eq_string("hello", "hello")
}