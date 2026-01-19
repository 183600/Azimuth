// Azimuth 增强测试用例
// 补充更多边界情况和组合测试

test "add_large_numbers" {
  // 测试大数加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 1000, 1000))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 1000, -1000))
  
  // 测试大数与小数的组合
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483640, 6))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483640, -7))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(2147483646, @azimuth.multiply(1073741823, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-1073741824, 2))
  
  // 测试大数与小数的乘法
  @azimuth.assert_eq(4294967294, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(-4294967296, @azimuth.multiply(-2147483648, 2))
}

test "greet_unicode_and_spaces" {
  // 测试包含Unicode字符和空格的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello,  MoonBit  !", @azimuth.greet("  MoonBit  "))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 100
  let y = -50
  let z = 25
  
  // 计算 (x + y) * z
  let sum = @azimuth.add(x, y)
  let product = @azimuth.multiply(sum, z)
  @azimuth.assert_eq(-1250, product)
  
  // 计算 x * y + z
  let product2 = @azimuth.multiply(x, y)
  let sum2 = @azimuth.add(product2, z)
  @azimuth.assert_eq(-4975, sum2)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.divide_with_ceil(-2147483648, 1))
}

test "add_alternating_signs" {
  // 测试交替符号的加法
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(1, @azimuth.add(2147483647, -2147483646))
  @azimuth.assert_eq(-1, @azimuth.add(-2147483648, 2147483647))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  @azimuth.assert_eq(-5, @azimuth.add(10, -15))
}

test "multiply_alternating_signs" {
  // 测试交替符号的乘法
  @azimuth.assert_eq(-10000, @azimuth.multiply(100, -100))
  @azimuth.assert_eq(10000, @azimuth.multiply(-100, -100))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(2147483647, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(-2147483647, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(-1, -2147483647))
}

test "greet_empty_and_special_strings" {
  // 测试空字符串和特殊字符串的问候
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quote\"!", @azimuth.greet("\"quote\""))
  @azimuth.assert_eq_string("Hello, \\backslash\\!", @azimuth.greet("\\backslash\\"))
}

test "chained_operations" {
  // 测试链式操作
  let result = @azimuth.add(@azimuth.multiply(10, 5), @azimuth.multiply(-3, 2))
  @azimuth.assert_eq(44, result)
  
  let result2 = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(-3, 2))
  @azimuth.assert_eq(-5, result2)
  
  let result3 = @azimuth.divide_with_ceil(@azimuth.add(10, 5), 4)
  @azimuth.assert_eq(4, result3)
}

test "extreme_boundary_values" {
  // 测试极端边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法边界测试
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 乘法边界测试
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
}