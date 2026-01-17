// 标准 MoonBit 测试用例
// 覆盖项目的核心功能：add、multiply 和 greet 函数

test "add_basic_functionality" {
  // 测试基本的加法功能
  let result = @azimuth.add(3, 5)
  @azimuth.assert_eq(8, result)
}

test "add_negative_numbers" {
  // 测试负数加法
  let result = @azimuth.add(-3, 7)
  @azimuth.assert_eq(4, result)
}

test "add_zero_handling" {
  // 测试零值处理
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  let result = @azimuth.multiply(4, 6)
  @azimuth.assert_eq(24, result)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(-24, @azimuth.multiply(4, -6))
  @azimuth.assert_eq(24, @azimuth.multiply(-4, -6))
}

test "multiply_zero_handling" {
  // 测试零值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "greet_empty_string" {
  // 测试空字符串处理
  let result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", result)
}

test "greet_special_characters" {
  // 测试特殊字符处理
  let result = @azimuth.greet("测试@123")
  @azimuth.assert_eq_string("Hello, 测试@123!", result)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}