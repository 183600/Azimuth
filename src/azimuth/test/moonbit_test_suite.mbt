// MoonBit 测试套件
// 包含10个标准的 MoonBit 测试用例

test "basic_arithmetic_operations" {
  // 测试基本算术运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(5, -5))
  @azimuth.assert_eq(42, @azimuth.multiply(6, 7))
  @azimuth.assert_eq(-24, @azimuth.multiply(-4, 6))
}

test "string_greeting_functionality" {
  // 测试字符串问候功能
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 测试!", @azimuth.greet("测试"))
}

test "mathematical_commutativity" {
  // 测试数学交换律
  let a = 23
  let b = 17
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "boundary_value_handling" {
  // 测试边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 边界值加法
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 边界值乘法
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形面积和周长
  let length = 15
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(120, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(46, perimeter)
}

test "financial_calculation_simple" {
  // 测试简单金融计算：折扣价格计算
  let original_price = 200
  let discount_percentage = 15
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  let final_price = @azimuth.add(original_price, -discount_amount)
  
  @azimuth.assert_eq(30, discount_amount)
  @azimuth.assert_eq(170, final_price)
}

test "temperature_conversion" {
  // 测试温度转换：摄氏度转华氏度（简化版）
  // 公式：F = C × 2 + 30（避免小数运算）
  
  let celsius_0 = 0
  let fahrenheit_0 = @azimuth.add(@azimuth.multiply(celsius_0, 2), 30)
  @azimuth.assert_eq(30, fahrenheit_0)
  
  let celsius_25 = 25
  let fahrenheit_25 = @azimuth.add(@azimuth.multiply(celsius_25, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_25)
}

test "sequential_operations" {
  // 测试连续运算：计算 (a + b) × c - d
  let a = 10
  let b = 5
  let c = 3
  let d = 12
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(33, result)
}

test "error_handling_and_edge_cases" {
  // 测试错误处理和边界情况
  @azimuth.assert_true(true)
  @azimuth.assert_false(false)
  
  // 测试溢出保护
  let max_val = 2147483647
  @azimuth.assert_true(@azimuth.add(max_val, 100) == max_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  
  // 测试零值特性
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123456))
  @azimuth.assert_eq(123456, @azimuth.add(0, 123456))
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 99
  let quantity = 4
  let shipping = 12
  let tax_rate = 8
  
  // 计算商品总价
  let subtotal = @azimuth.multiply(item_price, quantity)
  
  // 计算税费
  let tax = @azimuth.multiply(subtotal, tax_rate) / 100
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(396, subtotal)
  @azimuth.assert_eq(31, tax)
  @azimuth.assert_eq(439, total)
}