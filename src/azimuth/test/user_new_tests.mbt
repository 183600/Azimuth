// 用户自定义的 MoonBit 测试用例
// 测试 Azimuth 项目的核心功能

test "add_basic_functionality" {
  // 测试基本的加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(-2, 2))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
}

test "add_with_zero" {
  // 测试与0相加的情况
  @azimuth.assert_eq(7, @azimuth.add(7, 0))
  @azimuth.assert_eq(7, @azimuth.add(0, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
}

test "multiply_with_special_values" {
  // 测试与特殊值相乘的情况
  @azimuth.assert_eq(8, @azimuth.multiply(8, 1))
  @azimuth.assert_eq(8, @azimuth.multiply(1, 8))
  @azimuth.assert_eq(-8, @azimuth.multiply(-8, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(8, 0))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
}

test "boundary_values_add" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(0, @azimuth.add(min_val, 2147483647))
}

test "boundary_values_multiply" {
  // 测试边界值的乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景
  // 计算 (2 + 3) * (4 + 5) - 10
  let sum1 = @azimuth.add(2, 3)  // 5
  let sum2 = @azimuth.add(4, 5)  // 9
  let product = @azimuth.multiply(sum1, sum2)  // 45
  let result = @azimuth.add(product, -10)  // 35
  
  @azimuth.assert_eq(35, result)
}

test "string_concatenation_edge_cases" {
  // 测试字符串连接的边界情况
  let empty = ""
  let single_char = "A"
  let long_string = "ThisIsAVeryLongStringForTestingPurposes"
  let with_spaces = "Hello World"
  
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(empty))
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet(single_char))
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
  @azimuth.assert_eq_string("Hello, Hello World!", @azimuth.greet(with_spaces))
}