// 标准 MoonBit 测试用例
// 覆盖 azimuth 项目的核心功能

test "add_function_basic" {
  // 测试基本的加法功能
  @azimuth.assert_eq(30, @azimuth.add(10, 20))
}

test "multiply_function_basic" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(30, @azimuth.multiply(5, 6))
}

test "greet_function_basic" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "add_function_with_negative" {
  // 测试包含负数的加法
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
}

test "multiply_function_with_negative" {
  // 测试包含负数的乘法
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
}

test "add_function_overflow_protection" {
  // 测试加法的溢出保护
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
}

test "multiply_function_overflow_protection" {
  // 测试乘法的溢出保护
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
}

test "greet_function_with_special_chars" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试@123!", @azimuth.greet("测试@123"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  let length = 10
  let width = 5
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "edge_case_min_value_handling" {
  // 测试最小值的边界情况处理
  let min_val = -2147483648
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
}