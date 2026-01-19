// 标准 MoonBit 测试用例
// 测试 Azimuth 项目的基本功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-3, @azimuth.add(-1, -2))
  @azimuth.assert_eq(10, @azimuth.add(-5, 15))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
}

test "greet_function_basic_tests" {
  // 测试 greet 函数的基本功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "addition_with_zero" {
  // 测试与0相加的情况
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-10, 0))
}

test "multiplication_with_one_and_zero" {
  // 测试与1和0相乘的情况
  @azimuth.assert_eq(8, @azimuth.multiply(8, 1))
  @azimuth.assert_eq(8, @azimuth.multiply(1, 8))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 8))
}

test "addition_overflow_handling" {
  // 测试加法溢出处理
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
}

test "multiplication_overflow_handling" {
  // 测试乘法溢出处理
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.multiply(46341, 46341))
  
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "greet_function_with_special_characters" {
  // 测试 greet 函数处理特殊字符
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   spaced!", @azimuth.greet("  spaced"))
}

test "negative_value_operations" {
  // 测试负值运算
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(3, @azimuth.add(-5, 8))
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (2 + 3) * 4 - 5 = 20 - 5 = 15
  let sum = @azimuth.add(2, 3)
  let product = @azimuth.multiply(sum, 4)
  let result = @azimuth.add(product, -5)
  @azimuth.assert_eq(15, result)
  
  // 计算矩形的周长：2 * (长 + 宽) = 2 * (6 + 4) = 20
  let length = 6
  let width = 4
  let perimeter = @azimuth.multiply(2, @azimuth.add(length, width))
  @azimuth.assert_eq(20, perimeter)
}