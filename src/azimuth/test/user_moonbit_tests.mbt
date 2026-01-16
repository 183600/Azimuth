// 用户自定义 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "overflow_boundary_conditions" {
  // 测试溢出边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 8
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 10
  let width = 5
  
  let area = @azimuth.multiply(length, width)
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  
  @azimuth.assert_eq(50, area)
  @azimuth.assert_eq(30, perimeter)
}

test "string_with_unicode_characters" {
  // 测试包含Unicode字符的字符串处理
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, émoji!", @azimuth.greet("émoji"))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "zero_value_operations" {
  // 测试零值运算
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
}

test "financial_calculation_simple" {
  // 测试简单金融计算：计算总价
  let price = 199
  let quantity = 3
  let tax = 50
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, tax)
  
  @azimuth.assert_eq(647, total)
}

test "sequential_operations" {
  // 测试连续运算
  let result = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, result)
  
  let complex_result = @azimuth.multiply(@azimuth.add(10, 5), @azimuth.add(3, 2))
  @azimuth.assert_eq(75, complex_result)
}