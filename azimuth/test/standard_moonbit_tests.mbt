// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，专注于测试实际存在的函数

test "add_basic_functionality" {
  // 基本加法功能测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(-5, 15))
}

test "add_edge_cases" {
  // 加法边界情况测试
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(0, -2147483648))
}

test "add_overflow_protection" {
  // 加法溢出保护测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 2147483647))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -2147483648))
}

test "multiply_basic_functionality" {
  // 基本乘法功能测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "multiply_edge_cases" {
  // 乘法边界情况测试
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
}

test "multiply_overflow_protection" {
  // 乘法溢出保护测试
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2))
}

test "multiply_min_value_special_cases" {
  // 最小值乘法特殊情况测试
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(1, -2147483648))
}

test "greet_basic_functionality" {
  // 基本问候功能测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_special_characters" {
  // 特殊字符问候测试
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文字符!", @azimuth.greet("中文字符"))
  @azimuth.assert_eq_string("Hello, emoji😊!", @azimuth.greet("emoji😊"))
}

test "complex_arithmetic_combinations" {
  // 复杂算术组合测试
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(sum, 3)
  @azimuth.assert_eq(90, product)
  
  let nested = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.add(2, 4))
  @azimuth.assert_eq(48, nested)
}