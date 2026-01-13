// 测试lib.mbt中的函数
test "basic_add_test" {
  let result = @clean_test.add(2, 3)
  @clean_test.assert_eq(5, result)
}

test "add_with_zero" {
  // 测试与0相加的情况
  @clean_test.assert_eq(5, @clean_test.add(5, 0))
  @clean_test.assert_eq(5, @clean_test.add(0, 5))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
}

test "add_with_negative_numbers" {
  // 测试负数相加
  @clean_test.assert_eq(-5, @clean_test.add(-2, -3))
  @clean_test.assert_eq(0, @clean_test.add(5, -5))
  @clean_test.assert_eq(-1, @clean_test.add(3, -4))
}

test "add_overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, 1))
  @clean_test.assert_eq(max_val, @clean_test.add(max_val, max_val))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, -1))
  @clean_test.assert_eq(min_val, @clean_test.add(min_val, min_val))
}

test "basic_multiply_test" {
  let result = @clean_test.multiply(2, 3)
  @clean_test.assert_eq(6, result)
}

test "multiply_with_zero_and_one" {
  // 测试与0和1相乘的情况
  @clean_test.assert_eq(0, @clean_test.multiply(5, 0))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 5))
  @clean_test.assert_eq(5, @clean_test.multiply(5, 1))
  @clean_test.assert_eq(5, @clean_test.multiply(1, 5))
}

test "multiply_with_negative_numbers" {
  // 测试负数相乘
  @clean_test.assert_eq(-6, @clean_test.multiply(2, -3))
  @clean_test.assert_eq(-6, @clean_test.multiply(-2, 3))
  @clean_test.assert_eq(6, @clean_test.multiply(-2, -3))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  @clean_test.assert_eq(max_val, @clean_test.multiply(max_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, 2))
  @clean_test.assert_eq(min_val, @clean_test.multiply(min_val, -1))
}

test "basic_greet_test" {
  let result = @clean_test.greet("World")
  @clean_test.assert_eq_string("Hello, World!", result)
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的字符串
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  @clean_test.assert_eq_string("Hello, 张三!", @clean_test.greet("张三"))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
  @clean_test.assert_eq_string("Hello, @#$!", @clean_test.greet("@#$"))
}