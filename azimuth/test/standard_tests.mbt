// 标准 MoonBit 测试文件
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(1, @azimuth.add(-2, 3))
  @azimuth.assert_eq(-1, @azimuth.add(2, -3))
  @azimuth.assert_eq(-5, @azimuth.add(-2, -3))
}

test "add_zero_handling" {
  // 测试加法的零值处理
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(0, -5))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(2147483647, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(-2147483648, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(-6, @azimuth.multiply(2, -3))
  @azimuth.assert_eq(6, @azimuth.multiply(-2, -3))
}

test "multiply_zero_and_one" {
  // 测试乘法的零值和一值处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, @azimuth.multiply(-1, 5))
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(2147483647, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(50000, 50000))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-50000, 50000))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  @azimuth.assert_eq_string("Hello, space test!", @azimuth.greet("space test"))
}

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(13, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-12, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-13, 3))
}

test "complex_arithmetic_operations" {
  // 测试复合算术运算
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 2
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(16, result)
  
  // a * b + c
  let product = @azimuth.multiply(a, b)
  let result2 = @azimuth.add(product, c)
  @azimuth.assert_eq(17, result2)
}