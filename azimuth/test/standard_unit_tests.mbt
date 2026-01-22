test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(10, @azimuth.multiply(10, 1))
}

test "multiply_special_cases" {
  // 测试乘法特殊情况
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
}

test "greet_standard_cases" {
  // 测试标准问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "divide_with_ceil_positive_numbers" {
  // 测试正数向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 5))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-10, -3))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  let sum = @azimuth.add(15, 25)
  let product = @azimuth.multiply(sum, 2)
  @azimuth.assert_eq(80, product)
  
  let result = @azimuth.divide_with_ceil(product, 15)
  @azimuth.assert_eq(6, result)
}

test "mixed_operations_with_strings" {
  // 测试混合运算和字符串处理
  let num1 = 100
  let num2 = 200
  let sum = @azimuth.add(num1, num2)
  let product = @azimuth.multiply(sum, 3)
  let greeting = @azimuth.greet("User")
  
  @azimuth.assert_eq(900, product)
  @azimuth.assert_eq_string("Hello, User!", greeting)
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(sum, 150))
}