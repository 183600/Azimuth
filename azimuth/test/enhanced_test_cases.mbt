test "add_mixed_sign_numbers" {
  // 测试混合符号数的加法
  @azimuth.assert_eq(10, @azimuth.add(15, -5))
  @azimuth.assert_eq(-10, @azimuth.add(-15, 5))
  @azimuth.assert_eq(0, @azimuth.add(100, -100))
  @azimuth.assert_eq(-5, @azimuth.add(-10, 5))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-20, @azimuth.multiply(-4, 5))
  @azimuth.assert_eq(-30, @azimuth.multiply(6, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-10, 0))
}

test "add_min_value_edge_cases" {
  // 测试最小值的各种边界情况
  let min_val = -2147483648
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

test "multiply_large_numbers" {
  // 测试大数乘法
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-1000000, @azimuth.multiply(1000, -1000))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(46340, 46341))  // 接近溢出边界
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
}

test "greet_various_inputs" {
  // 测试不同输入的greet函数
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列
  let result1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @azimuth.multiply(@azimuth.add(10, 20), @azimuth.add(5, 3))
  @azimuth.assert_eq(240, result2)  // (10+20)*(5+3) = 30*8 = 240
  
  let result3 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result3)  // (1+2)+(3+4) = 3+7 = 10
}

test "add_extreme_values" {
  // 测试极值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与负数的加法
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(2147483647 - 1000, @azimuth.add(max_val, -1000))
  
  // 测试最小值与正数的加法
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(-2147483648 + 1000, @azimuth.add(min_val, 1000))
}

test "multiply_by_negative_one" {
  // 测试乘以-1的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(-5, @azimuth.multiply(5, -1))
  @azimuth.assert_eq(5, @azimuth.multiply(-5, -1))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(max_val, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(min_val, -1))  // 最小值特例
}

test "string_concatenation_edge_cases" {
  // 测试字符串拼接的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, a!", @azimuth.greet("a"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, verylongname!", @azimuth.greet("verylongname"))
}