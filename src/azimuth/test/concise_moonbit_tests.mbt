// 简洁的 MoonBit 测试用例
// 测试项目中的核心功能：add, multiply, greet 函数

// 1. 基本加法测试
test "add_basic" {
  @azimuth.assert_eq(8, @azimuth.add(3, 5))
}

// 2. 加法边界值测试
test "add_boundary" {
  let max = 2147483647
  let min = -2147483648
  @azimuth.assert_eq(max, @azimuth.add(max, 1))
  @azimuth.assert_eq(min, @azimuth.add(min, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max, min))
}

// 3. 基本乘法测试
test "multiply_basic" {
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

// 4. 乘法边界值测试
test "multiply_boundary" {
  let max = 2147483647
  let min = -2147483648
  @azimuth.assert_eq(0, @azimuth.multiply(max, 0))
  @azimuth.assert_eq(min, @azimuth.multiply(min, 1))
  @azimuth.assert_eq(min, @azimuth.multiply(min, -1))
}

// 5. 基本问候测试
test "greet_basic" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

// 6. 问候空字符串测试
test "greet_empty" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

// 7. 问候特殊字符测试
test "greet_special" {
  @azimuth.assert_eq_string("Hello, 测试@123!", @azimuth.greet("测试@123"))
}

// 8. 数学运算组合测试
test "combined_operations" {
  // (2 + 3) * 4 = 20
  let sum = @azimuth.add(2, 3)
  let result = @azimuth.multiply(sum, 4)
  @azimuth.assert_eq(20, result)
}

// 9. 实际应用场景测试 - 计算矩形周长
test "rectangle_perimeter" {
  let length = 10
  let width = 5
  // 周长 = 2 * (长 + 宽)
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(30, perimeter)
}

// 10. 实际应用场景测试 - 简单温度转换
test "temperature_conversion" {
  // 摄氏度转华氏度（简化版）：F = C * 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
}