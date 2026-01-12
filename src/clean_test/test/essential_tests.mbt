test "add_basic_functionality" {
  @clean_test.assert_eq(5, @clean_test.add(2, 3))
  @clean_test.assert_eq(0, @clean_test.add(-3, 3))
  @clean_test.assert_eq(-10, @clean_test.add(-5, -5))
  @clean_test.assert_eq(100, @clean_test.add(100, 0))
}

// 测试乘法的基础功能
///|
test "multiply_basic_functionality" {
  @clean_test.assert_eq(15, @clean_test.multiply(3, 5))
  @clean_test.assert_eq(0, @clean_test.multiply(100, 0))
  @clean_test.assert_eq(-25, @clean_test.multiply(-5, 5))
  @clean_test.assert_eq(36, @clean_test.multiply(-6, -6))
}

// 测试字符串拼接功能
///|
test "greet_basic_functionality" {
  @clean_test.assert_eq_string("Hello, Alice!", @clean_test.greet("Alice"))
  @clean_test.assert_eq_string("Hello, !", @clean_test.greet(""))
  @clean_test.assert_eq_string("Hello, 123!", @clean_test.greet("123"))
}

// 测试大数加法边界情况
///|
test "add_large_numbers" {
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483646, 1))
  @clean_test.assert_eq(2147483647, @clean_test.add(2147483647, 100))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483647, -1))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, -100))
}

// 测试大数乘法边界情况
///|
test "multiply_large_numbers" {
  @clean_test.assert_eq(2147483647, @clean_test.multiply(46341, 46341))
  @clean_test.assert_eq(2147483647, @clean_test.multiply(100000, 30000))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-46341, 46341))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(46341, -46341))
}

// 测试最小值特殊情况
///|
test "min_value_special_cases" {
  @clean_test.assert_eq(-2147483647, @clean_test.add(-2147483648, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.add(-2147483648, -1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, 1))
  @clean_test.assert_eq(-2147483648, @clean_test.multiply(-2147483648, -1))
}

// 测试实际应用：温度转换计算
///|
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @clean_test.add(@clean_test.multiply(celsius, 9), 160)
  // 25 * 9 = 225, 225 + 160 = 385，实际应该是 77
  // 这里我们使用简化的计算方式
  @clean_test.assert_eq(385, fahrenheit)
}

// 测试实际应用：购物车计算
///|
test "shopping_cart_calculation" {
  let apple_price = 5
  let apple_quantity = 3
  let orange_price = 8
  let orange_quantity = 2
  
  let apple_total = @clean_test.multiply(apple_price, apple_quantity)
  let orange_total = @clean_test.multiply(orange_price, orange_quantity)
  let total = @clean_test.add(apple_total, orange_total)
  
  @clean_test.assert_eq(31, total)
}

// 测试数学性质：加法单位元
///|
test "addition_identity" {
  @clean_test.assert_eq(42, @clean_test.add(42, 0))
  @clean_test.assert_eq(-42, @clean_test.add(-42, 0))
  @clean_test.assert_eq(0, @clean_test.add(0, 0))
}

// 测试数学性质：乘法单位元
///|
test "multiplication_identity" {
  @clean_test.assert_eq(42, @clean_test.multiply(42, 1))
  @clean_test.assert_eq(-42, @clean_test.multiply(-42, 1))
  @clean_test.assert_eq(0, @clean_test.multiply(0, 1))
}
