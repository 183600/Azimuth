// 标准 MoonBit 测试用例
// 测试 azimuth 项目的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
}

test "add_with_zero" {
  // 测试与零相加的情况
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_positive_and_negative" {
  // 测试正数和负数相加
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
}

test "multiply_with_one_and_negative_one" {
  // 测试与1和-1相乘的情况
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
}

test "multiply_positive_and_negative" {
  // 测试正数和负数相乘
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(20, @azimuth.multiply(-4, -5))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}

test "greet_with_empty_string" {
  // 测试空字符串的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "combined_operations" {
  // 测试组合运算
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(5, 6)
  let greeting = @azimuth.greet("Test")
  
  @azimuth.assert_eq(30, sum)
  @azimuth.assert_eq(30, product)
  @azimuth.assert_eq_string("Hello, Test!", greeting)
}