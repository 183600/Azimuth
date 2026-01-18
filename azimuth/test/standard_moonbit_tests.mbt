// 标准MoonBit测试用例
// 测试azimuth包的核心功能

test "add_function_basic_cases" {
  // 基本加法测试
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
  @azimuth.assert_eq(0, @azimuth.add(7, -7))
}

test "add_function_edge_cases" {
  // 边界情况测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483646, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "multiply_function_basic_cases" {
  // 基本乘法测试
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
}

test "multiply_function_edge_cases" {
  // 边界情况测试
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
}

test "greet_function_basic_cases" {
  // 基本问候测试
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "greet_function_special_characters" {
  // 特殊字符测试
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, @#$!", @azimuth.greet("@#$"))
  @azimuth.assert_eq_string("Hello, space !", @azimuth.greet("space "))
}

test "combined_operations" {
  // 组合操作测试
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(60, product)
  
  let greeting = @azimuth.greet("MoonBit")
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting)
}

test "overflow_protection" {
  // 溢出保护测试
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -1))
}