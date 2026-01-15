// 基本功能测试用例 - 覆盖核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(30, @azimuth.add(10, 20))
}

test "add_zero_handling" {
  // 测试零值处理
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
}

test "multiply_zero_and_one" {
  // 测试零和一的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 8))
  @azimuth.assert_eq(8, @azimuth.multiply(8, 1))
  @azimuth.assert_eq(8, @azimuth.multiply(1, 8))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_empty_string" {
  // 测试空字符串处理
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_special_characters" {
  // 测试特殊字符处理
  @azimuth.assert_eq_string("Hello, 测试@123!", @azimuth.greet("测试@123"))
}

test "combined_operations" {
  // 测试组合运算
  let a = 5
  let b = 3
  let c = 2
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(16, result)
}