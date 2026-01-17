// 用户自定义的 MoonBit 测试用例
// 包含不超过10个标准测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(-8, @azimuth.multiply(-2, 4))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 12
  let b = 7
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 测试负数的交换律
  let c = -5
  let d = 3
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  @azimuth.assert_eq(@azimuth.multiply(c, d), @azimuth.multiply(d, c))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的面积和周长
  let length = 8
  let width = 6
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(48, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(28, perimeter)
}

test "financial_calculation_simple" {
  // 测试简单金融计算：计算总价含税
  let price = 100
  let quantity = 5
  let tax_rate = 8  // 8%
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(price, quantity)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价
  let total = @azimuth.add(subtotal, tax)
  @azimuth.assert_eq(540, total)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // 使用简化公式：F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
}

test "sequential_operations" {
  // 测试连续运算
  // 计算 (a + b) * c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 7
  
  let sum = @azimuth.add(a, b)  // 15
  let product = @azimuth.multiply(sum, c)  // 45
  let result = @azimuth.add(product, -d)  // 38
  
  @azimuth.assert_eq(38, result)
}

test "error_handling_validation" {
  // 测试错误处理验证
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  @azimuth.assert_eq(42, 42)
  @azimuth.assert_eq_string("test", "test")
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 2
  let shipping = 15
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(413, total)
}