// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}

test "add_with_zero" {
  // 测试与0相加
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(5, @azimuth.add(10, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  let result = @azimuth.multiply(4, 5)
  @azimuth.assert_eq(20, result)
}

test "multiply_with_zero_and_one" {
  // 测试与0和1相乘
  @azimuth.assert_eq(0, @azimuth.multiply(7, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 7))
  @azimuth.assert_eq(9, @azimuth.multiply(9, 1))
  @azimuth.assert_eq(9, @azimuth.multiply(1, 9))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  let result = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "greet_edge_cases" {
  // 测试边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "combined_operations" {
  // 测试组合运算
  let sum_result = @azimuth.add(10, 20)
  let product_result = @azimuth.multiply(5, 6)
  let greeting_result = @azimuth.greet("World")
  
  @azimuth.assert_eq(30, sum_result)
  @azimuth.assert_eq(30, product_result)
  @azimuth.assert_eq_string("Hello, World!", greeting_result)
}