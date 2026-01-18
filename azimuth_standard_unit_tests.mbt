// Azimuth 项目标准单元测试
// 使用标准 MoonBit 测试语法，覆盖核心功能

// 导入必要的模块
use azimuth

test "add_function_basic_cases" {
  // 测试加法函数的基本情况
  @assertion.assert_eq(5, azimuth.add(2, 3))?
  @assertion.assert_eq(0, azimuth.add(-2, 2))?
  @assertion.assert_eq(-5, azimuth.add(-2, -3))?
  @assertion.assert_eq(100, azimuth.add(50, 50))?
}

test "add_function_zero_handling" {
  // 测试加法函数对0的处理
  @assertion.assert_eq(7, azimuth.add(0, 7))?
  @assertion.assert_eq(7, azimuth.add(7, 0))?
  @assertion.assert_eq(0, azimuth.add(0, 0))?
}

test "add_function_boundary_values" {
  // 测试加法函数的边界值
  let max_int = 2147483647
  let min_int = -2147483648
  
  @assertion.assert_eq(max_int, azimuth.add(max_int, 0))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, 0))?
  @assertion.assert_eq(max_int, azimuth.add(max_int, 1))?  // 应该饱和到最大值
}

test "multiply_function_basic_cases" {
  // 测试乘法函数的基本情况
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(0, azimuth.multiply(5, 0))?
  @assertion.assert_eq(-6, azimuth.multiply(-2, 3))?
  @assertion.assert_eq(6, azimuth.multiply(-2, -3))?
}

test "multiply_function_identity_elements" {
  // 测试乘法函数的单位元素
  @assertion.assert_eq(7, azimuth.multiply(7, 1))?
  @assertion.assert_eq(7, azimuth.multiply(1, 7))?
  @assertion.assert_eq(0, azimuth.multiply(7, 0))?
  @assertion.assert_eq(0, azimuth.multiply(0, 7))?
}

test "multiply_function_negative_one" {
  // 测试乘法函数对-1的处理
  @assertion.assert_eq(-7, azimuth.multiply(7, -1))?
  @assertion.assert_eq(-7, azimuth.multiply(-1, 7))?
  @assertion.assert_eq(7, azimuth.multiply(-7, -1))?
}

test "greet_function_standard_cases" {
  // 测试问候函数的标准情况
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
}

test "greet_function_edge_cases" {
  // 测试问候函数的边界情况
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, A!", azimuth.greet("A"))?
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  // (2 + 3) * 4 = 20
  let result1 = azimuth.multiply(azimuth.add(2, 3), 4)
  @assertion.assert_eq(20, result1)?
  
  // 5 * 6 + 7 = 37
  let result2 = azimuth.add(azimuth.multiply(5, 6), 7)
  @assertion.assert_eq(37, result2)?
  
  // (10 - 3) * (2 + 1) = 21
  let result3 = azimuth.multiply(azimuth.add(10, -3), azimuth.add(2, 1))
  @assertion.assert_eq(21, result3)?
}