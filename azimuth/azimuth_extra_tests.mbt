// Azimuth 额外测试用例
// 使用标准的 MoonBit 测试语法
// 覆盖一些额外的边界情况和组合测试

test "add_large_numbers" {
  // 测试大数加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 接近最大值的加法
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val - 100, 99))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val - 1000, 1000))
  
  // 接近最小值的加法
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val + 100, -99))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val + 1000, -1000))
}

test "add_mixed_signs" {
  // 测试混合符号加法的各种情况
  @azimuth.assert_eq(0, @azimuth.add(2147483647, -2147483647))
  @azimuth.assert_eq(-1, @azimuth.add(2147483647, -2147483648))
  @azimuth.assert_eq(1, @azimuth.add(-2147483647, 2147483648))
  
  // 测试一正一负但结果不为零的情况
  @azimuth.assert_eq(100, @azimuth.add(200, -100))
  @azimuth.assert_eq(-100, @azimuth.add(-200, 100))
  @azimuth.assert_eq(2147483646, @azimuth.add(2147483647, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.add(-2147483648, 1))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的乘法
  @azimuth.assert_eq(2147483646, @azimuth.multiply(2, 1073741823))
  @azimuth.assert_eq(-2147483646, @azimuth.multiply(-2, 1073741823))
  @azimuth.assert_eq(-2147483646, @azimuth.multiply(2, -1073741823))
  @azimuth.assert_eq(2147483646, @azimuth.multiply(-2, -1073741823))
}

test "multiply_small_numbers" {
  // 测试小数乘法
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(-1, 1))
  @azimuth.assert_eq(-1, @azimuth.multiply(1, -1))
  @azimuth.assert_eq(1, @azimuth.multiply(-1, -1))
  
  @azimuth.assert_eq(2, @azimuth.multiply(1, 2))
  @azimuth.assert_eq(2, @azimuth.multiply(2, 1))
  @azimuth.assert_eq(-2, @azimuth.multiply(-1, 2))
  @azimuth.assert_eq(-2, @azimuth.multiply(1, -2))
}

test "greet_with_spaces" {
  // 测试包含空格的问候
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, Hello World!", @azimuth.greet("Hello World"))
  @azimuth.assert_eq_string("Hello, leading space!", @azimuth.greet(" leading space"))
  @azimuth.assert_eq_string("Hello, trailing space !", @azimuth.greet("trailing space "))
}

test "divide_with_ceil_negative_cases" {
  // 测试向上取整除法的负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 4))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-11, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(-1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(1, -1))
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 1))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.divide_with_ceil(-2147483647, -1))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let result1 = @azimuth.add(10, 20)
  let result2 = @azimuth.multiply(result1, 3)
  let result3 = @azimuth.add(result2, -15)
  let result4 = @azimuth.divide_with_ceil(result3, 5)
  @azimuth.assert_eq(13, result4)
  
  // 另一个序列：((a + b) * c) / d
  let a = 5
  let b = 7
  let c = 4
  let d = 3
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let final_result = @azimuth.divide_with_ceil(product, d)
  @azimuth.assert_eq(16, final_result)
}

test "string_operations_with_greet" {
  // 测试与greet函数相关的字符串操作
  let greeting1 = @azimuth.greet("MoonBit")
  let greeting2 = @azimuth.greet("Azimuth")
  
  // 验证字符串前缀
  @azimuth.assert_true(String::has_prefix(greeting1, "Hello, "))
  @azimuth.assert_true(String::has_prefix(greeting2, "Hello, "))
  
  // 验证字符串后缀
  @azimuth.assert_true(String::has_suffix(greeting1, "!"))
  @azimuth.assert_true(String::has_suffix(greeting2, "!"))
  
  // 验证完整字符串
  @azimuth.assert_eq_string("Hello, MoonBit!", greeting1)
  @azimuth.assert_eq_string("Hello, Azimuth!", greeting2)
}