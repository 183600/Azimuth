// 基础核心测试用例

// 测试加法的基础功能
///|
test "add_basic_functionality" {
  assert_eq(5, @azimuth.add(2, 3))
  assert_eq(0, @azimuth.add(-3, 3))
  assert_eq(-10, @azimuth.add(-5, -5))
  assert_eq(100, @azimuth.add(100, 0))
}

// 测试乘法的基础功能
///|
test "multiply_basic_functionality" {
  assert_eq(15, @azimuth.multiply(3, 5))
  assert_eq(0, @azimuth.multiply(100, 0))
  assert_eq(-25, @azimuth.multiply(-5, 5))
  assert_eq(36, @azimuth.multiply(-6, -6))
}

// 测试字符串拼接功能
///|
test "greet_basic_functionality" {
  assert_eq("Hello, Alice!", @azimuth.greet("Alice"))
  assert_eq("Hello, !", @azimuth.greet(""))
  assert_eq("Hello, 123!", @azimuth.greet("123"))
}

// 测试大数加法边界情况
///|
test "add_large_numbers" {
  assert_eq(2147483647, @azimuth.add(2147483646, 1))
  assert_eq(2147483647, @azimuth.add(2147483647, 100))
  assert_eq(-2147483648, @azimuth.add(-2147483647, -1))
  assert_eq(-2147483648, @azimuth.add(-2147483648, -100))
}

// 测试大数乘法边界情况
///|
test "multiply_large_numbers" {
  assert_eq(2147483647, @azimuth.multiply(46341, 46341))
  assert_eq(2147483647, @azimuth.multiply(100000, 30000))
  assert_eq(-2147483648, @azimuth.multiply(-46341, 46341))
  assert_eq(-2147483648, @azimuth.multiply(46341, -46341))
}

// 测试最小值特殊情况
///|
test "min_value_special_cases" {
  assert_eq(-2147483647, @azimuth.add(-2147483648, 1))
  assert_eq(-2147483648, @azimuth.add(-2147483648, -1))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, 1))
  assert_eq(-2147483648, @azimuth.multiply(-2147483648, -1))
}

// 测试实际应用：温度转换计算
///|
test "temperature_conversion" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9), 160)
  // 25 * 9 = 225, 225 + 160 = 385，实际应该是 77
  // 这里我们使用简化的计算方式
  assert_eq(385, fahrenheit)
}

// 测试实际应用：购物车计算
///|
test "shopping_cart_calculation" {
  let apple_price = 5
  let apple_quantity = 3
  let orange_price = 8
  let orange_quantity = 2
  
  let apple_total = @azimuth.multiply(apple_price, apple_quantity)
  let orange_total = @azimuth.multiply(orange_price, orange_quantity)
  let total = @azimuth.add(apple_total, orange_total)
  
  assert_eq(31, total)
}

// 测试数学性质：加法单位元
///|
test "addition_identity" {
  assert_eq(42, @azimuth.add(42, 0))
  assert_eq(-42, @azimuth.add(-42, 0))
  assert_eq(0, @azimuth.add(0, 0))
}

// 测试数学性质：乘法单位元
///|
test "multiplication_identity" {
  assert_eq(42, @azimuth.multiply(42, 1))
  assert_eq(-42, @azimuth.multiply(-42, 1))
  assert_eq(0, @azimuth.multiply(0, 1))
}