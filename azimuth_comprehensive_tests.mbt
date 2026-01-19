// Azimuth 项目综合测试用例
// 使用标准 MoonBit 测试语法，测试核心功能

test "basic_addition_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(10, @azimuth.add(3, 7))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-5, @azimuth.add(2, -7))
}

test "basic_multiplication_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(21, @azimuth.multiply(3, 7))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(-14, @azimuth.multiply(-2, 7))
}

test "greet_function_with_various_inputs" {
  // 测试问候功能与各种输入
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 3
  let c = 2
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算购物车总价
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  let shipping_fee = 15
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(325, total)  // (100×2) + (50×3) + 15 = 200 + 150 + 15 = 365
}

test "boundary_value_addition" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
}

test "zero_multiplication_property" {
  // 测试零乘法性质
  let test_values = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_values[0]))
  @azimuth.assert_eq(0, @azimuth.multiply(test_values[1], 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, test_values[2]))
  @azimuth.assert_eq(0, @azimuth.multiply(test_values[3], 0))
}

test "string_greeting_with_special_characters" {
  // 测试包含特殊字符的字符串问候
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let test_value = 42
  
  // 加法恒等元：a + 0 = a
  @azimuth.assert_eq(test_value, @azimuth.add(test_value, 0))
  @azimuth.assert_eq(test_value, @azimuth.add(0, test_value))
  
  // 乘法恒等元：a * 1 = a
  @azimuth.assert_eq(test_value, @azimuth.multiply(test_value, 1))
  @azimuth.assert_eq(test_value, @azimuth.multiply(1, test_value))
}

test "temperature_conversion_calculation" {
  // 测试温度转换计算（简化版）
  // 摄氏度转华氏度：F = C × 2 + 30
  
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)  // 20 × 2 + 30 = 70
}