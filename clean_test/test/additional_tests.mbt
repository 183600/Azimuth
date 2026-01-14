// 额外的测试用例，补充现有测试覆盖范围

// 测试加法函数的连续运算
test "add_chained_operations" {
  let result1 = @clean_test.add(@clean_test.add(1, 2), 3)
  @clean_test.assert_eq(6, result1)
  
  let result2 = @clean_test.add(@clean_test.add(-10, 5), 15)
  @clean_test.assert_eq(10, result2)
}

// 测试乘法函数的连续运算
test "multiply_chained_operations" {
  let result1 = @clean_test.multiply(@clean_test.multiply(2, 3), 4)
  @clean_test.assert_eq(24, result1)
  
  let result2 = @clean_test.multiply(@clean_test.multiply(-2, 3), 4)
  @clean_test.assert_eq(-24, result2)
}

// 测试加法和乘法的组合运算
test "add_multiply_combination" {
  let result1 = @clean_test.add(@clean_test.multiply(2, 3), @clean_test.multiply(4, 5))
  @clean_test.assert_eq(26, result1) // 6 + 20 = 26
  
  let result2 = @clean_test.multiply(@clean_test.add(2, 3), @clean_test.add(4, 5))
  @clean_test.assert_eq(45, result2) // 5 * 9 = 45
}

// 测试greet函数的空格和特殊符号处理
test "greet_whitespace_and_symbols" {
  @clean_test.assert_eq_string("Hello,   !", @clean_test.greet("  "))
  @clean_test.assert_eq_string("Hello, 
	!", @clean_test.greet("
	"))
  @clean_test.assert_eq_string("Hello, \"quoted\"!", @clean_test.greet("\"quoted\""))
  @clean_test.assert_eq_string("Hello, 'single'!", @clean_test.greet("'single'"))
}

// 测试加法函数的极大数处理
test "add_extreme_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近最大值的加法
  @clean_test.assert_eq(max_val, @clean_test.add(max_val - 1, 2))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val - 100, 101))
  
  // 测试接近最小值的加法
  @clean_test.assert_eq(min_val, @clean_test.add(min_val + 1, -2))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val + 100, -101))
}

// 测试乘法函数的平方运算
test "multiply_square_operations" {
  @clean_test.assert_eq(0, @clean_test.multiply(0, 0))
  @clean_test.assert_eq(1, @clean_test.multiply(1, 1))
  @clean_test.assert_eq(1, @clean_test.multiply(-1, -1))
  @clean_test.assert_eq(4, @clean_test.multiply(2, 2))
  @clean_test.assert_eq(9, @clean_test.multiply(3, 3))
  @clean_test.assert_eq(16, @clean_test.multiply(4, 4))
  @clean_test.assert_eq(25, @clean_test.multiply(5, 5))
}

// 测试greet函数的Unicode字符处理
test "greet_unicode_characters" {
  @clean_test.assert_eq_string("Hello, 🚀!", @clean_test.greet("🚀"))
  @clean_test.assert_eq_string("Hello, 🌙!", @clean_test.greet("🌙"))
  @clean_test.assert_eq_string("Hello, ⭐!", @clean_test.greet("⭐"))
  @clean_test.assert_eq_string("Hello, ❤️!", @clean_test.greet("❤️"))
}

// 测试加法函数的零值附近运算
test "add_near_zero_operations" {
  @clean_test.assert_eq(0, @clean_test.add(1, -1))
  @clean_test.assert_eq(0, @clean_test.add(-1, 1))
  @clean_test.assert_eq(1, @clean_test.add(0, 1))
  @clean_test.assert_eq(-1, @clean_test.add(0, -1))
  @clean_test.assert_eq(1, @clean_test.add(2, -1))
  @clean_test.assert_eq(-1, @clean_test.add(-2, 1))
}

// 测试乘法函数的幂运算
test "multiply_power_operations" {
  @clean_test.assert_eq(8, @clean_test.multiply(@clean_test.multiply(2, 2), 2)) // 2^3
  @clean_test.assert_eq(27, @clean_test.multiply(@clean_test.multiply(3, 3), 3)) // 3^3
  @clean_test.assert_eq(-8, @clean_test.multiply(@clean_test.multiply(-2, 2), 2)) // (-2)^3
}