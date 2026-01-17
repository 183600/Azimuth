test "basic_add_test" {
  let result = @azimuth.add(2, 3)
  @azimuth.assert_eq(5, result)
}
test "basic_multiply_test" {
  let result = @azimuth.multiply(2, 3)
  @azimuth.assert_eq(6, result)
}
test "basic_greet_test" {
  let result = @azimuth.greet("World")
  @azimuth.assert_eq_string("Hello, World!", result)
}
test "user_added_test" {
  let result = @azimuth.add(10, 20)
  @azimuth.assert_eq(30, result)
}
test "user_add_with_zero" {
  @azimuth.assert_eq(10, @azimuth.add(10, 0))
  @azimuth.assert_eq(10, @azimuth.add(0, 10))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}
test "user_add_positive_and_negative" {
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(3, @azimuth.add(10, -7))
  @azimuth.assert_eq(-3, @azimuth.add(-10, 7))
}
test "user_multiply_with_one_and_negative_one" {
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
}
test "user_greet_with_special_characters" {
  @azimuth.assert_eq_string("Hello, test@123!", @azimuth.greet("test@123"))
  @azimuth.assert_eq_string("Hello, 用户!", @azimuth.greet("用户"))
}
test "user_greet_with_empty_string" {
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}
test "user_combined_operations" {
  let sum = @azimuth.add(10, 20)
  let product = @azimuth.multiply(5, 6)
  let greeting = @azimuth.greet("Test")
  
  @azimuth.assert_eq(30, sum)
  @azimuth.assert_eq(30, product)
  @azimuth.assert_eq_string("Hello, Test!", greeting)
}

// 新增的标准测试用例
test "add_zero_edge_cases" {
  // 测试加法的零值边界情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.add(0, -2147483648))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, 0))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 1))
  @azimuth.assert_eq(2147483647, @azimuth.add(2147483647, 2147483647))
  @azimuth.assert_eq(2147483647, @azimuth.add(1000000000, 2000000000))
}

test "add_underflow_protection" {
  // 测试加法下溢保护
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-2147483648, -2147483648))
  @azimuth.assert_eq(-2147483648, @azimuth.add(-1000000000, -2000000000))
}

test "multiply_zero_and_one" {
  // 测试乘法的零值和一值情况
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 42))
  @azimuth.assert_eq(0, @azimuth.multiply(42, 0))
  @azimuth.assert_eq(42, @azimuth.multiply(1, 42))
  @azimuth.assert_eq(42, @azimuth.multiply(42, 1))
  @azimuth.assert_eq(-42, @azimuth.multiply(-1, 42))
  @azimuth.assert_eq(-42, @azimuth.multiply(42, -1))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 2))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(1000000, 3000))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

test "multiply_min_value_special_cases" {
  // 测试最小值乘法的特殊情况
  @azimuth.assert_eq(0, @azimuth.multiply(-2147483648, 0))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(-2147483648, 2))
  @azimuth.assert_eq(-2147483648, @azimuth.multiply(2, -2147483648))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !@#$!", @azimuth.greet("!@#$"))
  @azimuth.assert_eq_string("Hello, 中文字符!", @azimuth.greet("中文字符"))
  @azimuth.assert_eq_string("Hello, emoji😊!", @azimuth.greet("emoji😊"))
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet(" "))
  @azimuth.assert_eq_string("Hello, \n!", @azimuth.greet("\n"))
  @azimuth.assert_eq_string("Hello, \t!", @azimuth.greet("\t"))
}

test "complex_arithmetic_combinations" {
  // 测试复杂的算术组合
  // (a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(75, result)
  
  // 测试嵌套运算
  let x = @azimuth.multiply(@azimuth.add(5, 3), @azimuth.add(2, 4))
  @azimuth.assert_eq(48, x)
}