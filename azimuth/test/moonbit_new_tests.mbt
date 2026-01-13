// 新增的 MoonBit 测试用例
// 测试 add 函数的各种场景

test "add_negative_and_positive" {
  // 测试负数和正数相加
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
  @azimuth.assert_eq(5, @azimuth.add(-3, 8))
  @azimuth.assert_eq(0, @azimuth.add(-100, 100))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
}

test "add_large_negative_numbers" {
  // 测试大负数相加
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(-2000000000, @azimuth.add(-1000000000, -1000000000))
}

// 测试 multiply 函数的各种场景

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-30, @azimuth.multiply(-5, 6))
  @azimuth.assert_eq(-30, @azimuth.multiply(5, -6))
}

test "multiply_large_numbers" {
  // 测试大数相乘
  @azimuth.assert_eq(100000000, @azimuth.multiply(10000, 10000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
}

test "multiply_with_boundary_values" {
  // 测试边界值相乘
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

// 测试 greet 函数的各种场景

test "greet_with_numbers" {
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test123!", @azimuth.greet("test123"))
  @azimuth.assert_eq_string("Hello, 2023!", @azimuth.greet("2023"))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   spaced   out!", @azimuth.greet("  spaced   out"))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, user+tag!", @azimuth.greet("user+tag"))
  @azimuth.assert_eq_string("Hello, path/to/file!", @azimuth.greet("path/to/file"))
}

// 综合测试

test "complex_calculation" {
  // 测试复杂计算场景
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  
  @azimuth.assert_eq(900, result)
}