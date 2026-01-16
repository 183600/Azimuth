// 用户请求的标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_positive_numbers" {
  // 测试正数相加
  @azimuth.assert_eq(10, @azimuth.add(4, 6))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-4, -6))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
}

test "multiply_positive_numbers" {
  // 测试正数相乘
  @azimuth.assert_eq(12, @azimuth.multiply(3, 4))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
}

test "multiply_negative_numbers" {
  // 测试负数相乘
  @azimuth.assert_eq(12, @azimuth.multiply(-3, -4))
  @azimuth.assert_eq(-12, @azimuth.multiply(3, -4))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

test "greet_simple_names" {
  // 测试简单名称的问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "greet_complex_strings" {
  // 测试复杂字符串的问候
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello, user123!", @azimuth.greet("user123"))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "calculation_area_rectangle" {
  // 测试矩形面积计算
  let length = 10
  let width = 5
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(50, area)
}

test "calculation_perimeter_rectangle" {
  // 测试矩形周长计算
  let length = 10
  let width = 5
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(30, perimeter)
}

test "calculation_total_price" {
  // 测试总价计算
  let price = 25
  let quantity = 4
  let shipping = 10
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(110, total)
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 7
  let b = 13
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}