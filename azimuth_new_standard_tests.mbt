// Azimuth 项目标准 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，提供实用的测试覆盖

// 导入必要的模块
use azimuth

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  @assertion.assert_eq(15, azimuth.add(7, 8))?
  @assertion.assert_eq(6, azimuth.multiply(2, 3))?
  @assertion.assert_eq(5, azimuth.add(10, -5))?
  @assertion.assert_eq(-8, azimuth.multiply(-2, 4))?
}

test "string_greeting_functionality" {
  // 字符串问候功能测试
  @assertion.assert_eq_string("Hello, World!", azimuth.greet("World"))?
  @assertion.assert_eq_string("Hello, Alice!", azimuth.greet("Alice"))?
  @assertion.assert_eq_string("Hello, Bob!", azimuth.greet("Bob"))?
}

test "calculation_with_zero" {
  // 零值计算测试
  @assertion.assert_eq(0, azimuth.multiply(0, 12345))?
  @assertion.assert_eq(12345, azimuth.add(0, 12345))?
  @assertion.assert_eq(-12345, azimuth.add(0, -12345))?
  @assertion.assert_eq(0, azimuth.multiply(999, 0))?
}

test "negative_number_operations" {
  // 负数运算测试
  @assertion.assert_eq(-10, azimuth.add(-3, -7))?
  @assertion.assert_eq(21, azimuth.multiply(-3, -7))?
  @assertion.assert_eq(-4, azimuth.add(3, -7))?
  @assertion.assert_eq(14, azimuth.multiply(-2, -7))?
}

test "large_number_operations" {
  // 大数运算测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  @assertion.assert_eq(3000000, azimuth.add(large_num1, large_num2))?
  @assertion.assert_eq(2000000000000, azimuth.multiply(large_num1, large_num2))?
  @assertion.assert_eq(1000000, azimuth.add(large_num2, -large_num1))?
}

test "sequential_operations" {
  // 连续运算测试
  let result = azimuth.add(5, 3)
  result = azimuth.multiply(result, 2)
  result = azimuth.add(result, 10)
  
  @assertion.assert_eq(26, result)?
}

test "calculation_patterns" {
  // 计算模式测试
  // 测试分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  
  @assertion.assert_eq(left_side, right_side)?
}

test "edge_case_values" {
  // 边界值测试
  let one = 1
  let minus_one = -1
  
  @assertion.assert_eq(0, azimuth.add(1, -1))?
  @assertion.assert_eq(-1, azimuth.multiply(1, -1))?
  @assertion.assert_eq(1, azimuth.multiply(-1, -1))?
  @assertion.assert_eq(0, azimuth.multiply(0, -1))?
}

test "practical_calculations" {
  // 实用计算测试
  // 计算矩形面积和周长
  let length = 8
  let width = 5
  
  let area = azimuth.multiply(length, width)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  
  @assertion.assert_eq(40, area)?
  @assertion.assert_eq(26, perimeter)?
}

test "string_edge_cases" {
  // 字符串边界情况测试
  @assertion.assert_eq_string("Hello, !", azimuth.greet(""))?
  @assertion.assert_eq_string("Hello, A!", azimuth.greet("A"))?
  @assertion.assert_eq_string("Hello, 123!", azimuth.greet("123"))?
}