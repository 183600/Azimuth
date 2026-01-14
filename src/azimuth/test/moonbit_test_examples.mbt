// MoonBit 测试用例示例
// 测试 azimuth 模块的核心功能

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(3, 5))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
}

test "edge_case_zero_handling" {
  // 测试零值处理的边界情况
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
}

test "negative_number_operations" {
  // 测试负数运算
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(2, @azimuth.add(-3, 5))
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
}

test "large_number_calculations" {
  // 测试大数计算
  @azimuth.assert_eq(2000000, @azimuth.add(1000000, 1000000))
  @azimuth.assert_eq(1000000, @azimuth.multiply(1000, 1000))
  @azimuth.assert_eq(-2000000, @azimuth.add(-1000000, -1000000))
}

test "overflow_boundary_conditions" {
  // 测试溢出边界条件
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价计算
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 10
  
  let subtotal = @azimuth.multiply(item_price, quantity)
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(674, total)
}

test "mathematical_property_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律
  @azimuth.assert_eq(
    @azimuth.multiply(a, @azimuth.add(b, 3)),
    @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3))
  )
}

test "unicode_string_processing" {
  // 测试 Unicode 字符串处理
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, αβγδε!", @azimuth.greet("αβγδε"))
  @azimuth.assert_eq_string("Hello, 😊👍!", @azimuth.greet("😊👍"))
}

test "practical_application_scenario" {
  // 测试实际应用场景：温度转换
  // 摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing = @azimuth.add(@azimuth.multiply(0, 2), 30)
  @azimuth.assert_eq(30, freezing)
  
  // 测试沸点
  let boiling = @azimuth.add(@azimuth.multiply(100, 2), 30)
  @azimuth.assert_eq(230, boiling)
}