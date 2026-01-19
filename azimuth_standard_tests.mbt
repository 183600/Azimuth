// Azimuth 项目标准 MoonBit 测试用例集
// 使用标准 MoonBit 测试语法，提供全面的测试覆盖

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @assertion.assert_eq(15, azimuth.add(7, 8))?
  @assertion.assert_eq(20, azimuth.multiply(4, 5))?
  @assertion.assert_eq(6, azimuth.add(10, -4))?
  @assertion.assert_eq(-8, azimuth.multiply(-2, 4))?
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, Bob!", azimuth.greet("Bob"))?
  @assertion.assert_eq_string("Hello, 世界!", azimuth.greet("世界"))?
}

test "calculation_with_zero" {
  // 零值计算测试
  @assertion.assert_eq(0, azimuth.multiply(0, 123))?
  @assertion.assert_eq(0, azimuth.multiply(456, 0))?
  @assertion.assert_eq(123, azimuth.add(0, 123))?
  @assertion.assert_eq(-456, azimuth.add(0, -456))?
}

test "negative_number_operations" {
  // 负数运算测试
  @assertion.assert_eq(-10, azimuth.add(-3, -7))?
  @assertion.assert_eq(15, azimuth.multiply(-3, -5))?
  @assertion.assert_eq(-12, azimuth.multiply(-3, 4))?
  @assertion.assert_eq(2, azimuth.add(-5, 7))?
}

test "large_number_calculations" {
  // 大数计算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @assertion.assert_eq(3000000, azimuth.add(large_num1, large_num2))?
  @assertion.assert_eq(2000000000000, azimuth.multiply(large_num1, large_num2))?
}

test "sequential_operations" {
  // 连续运算测试
  let result = azimuth.add(5, 3)
  result = azimuth.multiply(result, 2)
  result = azimuth.add(result, 10)
  
  @assertion.assert_eq(26, result)?
}

test "complex_expressions" {
  // 复杂表达式测试
  // (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let temp = azimuth.add(a, b)
  temp = azimuth.multiply(temp, c)
  let result = azimuth.add(temp, -d)
  
  @assertion.assert_eq(38, result)?
}

test "boundary_value_tests" {
  // 边界值测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  @assertion.assert_eq(max_int, azimuth.add(max_int, 0))?
  @assertion.assert_eq(min_int, azimuth.add(min_int, 0))?
  @assertion.assert_eq(0, azimuth.multiply(max_int, 0))?
  @assertion.assert_eq(0, azimuth.multiply(min_int, 0))?
}