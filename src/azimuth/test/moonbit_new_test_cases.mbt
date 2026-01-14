// 新的 MoonBit 测试用例
// 这些测试用例涵盖了各种场景，包括数学运算、字符串处理和实际应用
test "hexadecimal_conversion_simulation" {
  // 模拟十六进制转换：将十进制数255转换为十六进制表示
  // 通过数学运算模拟转换过程
  let decimal_value = 255
  
  // 模拟十六进制转换：255 = 15 * 16 + 15 = FF
  // 使用乘法来模拟除法：15 * 16 = 240
  let high_part = azimuth::@azimuth.multiply(15, 16)  // 240
  let low_part = azimuth::@azimuth.add(decimal_value, -high_part)  // 255 - 240 = 15
  
  // 验证转换结果
  @azimuth.assert_eq(240, high_part)
  @azimuth.assert_eq(15, low_part)
  
  // 测试另一个值：268 = 16 * 16 + 12 = 10C
  let another_value = 268
  let another_high_part = azimuth::@azimuth.multiply(16, 16)  // 256
  let another_low_part = azimuth::@azimuth.add(another_value, -another_high_part)  // 268 - 256 = 12
  
  @azimuth.assert_eq(256, another_high_part)
  @azimuth.assert_eq(12, another_low_part)
}
test "simple_math_operations" {
  // 测试基本的数学运算组合
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum_ab = azimuth::@azimuth.add(a, b)  // 30
  let product = azimuth::@azimuth.multiply(sum_ab, c)  // 900
  @azimuth.assert_eq(900, product)
  
  // 计算 a * (b + c)
  let sum_bc = azimuth::@azimuth.add(b, c)  // 50
  let product2 = azimuth::@azimuth.multiply(a, sum_bc)  // 500
  @azimuth.assert_eq(500, product2)
}
test "string_processing_test" {
  // 测试字符串处理功能
  let name1 = "Alice"
  let name2 = "Bob"
  let name3 = "Charlie"
  
  // 测试greet函数
  let greeting1 = azimuth::@azimuth.greet(name1)
  let greeting2 = azimuth::@azimuth.greet(name2)
  let greeting3 = azimuth::@azimuth.greet(name3)
  
  @azimuth.assert_eq_string("Hello, Alice!", greeting1)
  @azimuth.assert_eq_string("Hello, Bob!", greeting2)
  @azimuth.assert_eq_string("Hello, Charlie!", greeting3)
  
  // 测试空字符串
  let empty_greeting = azimuth::@azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", empty_greeting)
}
test "boundary_value_test" {
  // 测试边界值的处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数的加法
  let max_add_result = azimuth::@azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, max_add_result)
  
  // 测试最小值与负数的加法
  let min_add_result = azimuth::@azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, min_add_result)
  
  // 测试最大值与正数的乘法
  let max_mul_result = azimuth::@azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, max_mul_result)
  
  // 测试最小值与-1的乘法
  let min_mul_result = azimuth::@azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, min_mul_result)
}
test "financial_calculation_test" {
  // 测试金融计算场景
  let principal = 1000  // 本金
  let rate = 5  // 利率（%）
  let years = 3  // 年数
  
  // 计算简单利息：本金 * 利率 * 年数
  let interest = azimuth::@azimuth.multiply(principal, azimuth::@azimuth.multiply(rate, years))
  let total = azimuth::@azimuth.add(principal, interest)
  
  // 1000 + (1000 * 5 * 3) = 16000（表示1600元）
  @azimuth.assert_eq(16000, total)
}
test "temperature_conversion_test" {
  // 测试温度转换（简化版，避免浮点数）
  let celsius = 25
  
  // 摄氏度转华氏度：F = C * 2 + 30（简化公式）
  let fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_c = 0
  let freezing_f = azimuth::@azimuth.add(azimuth::@azimuth.multiply(freezing_c, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling_c = 100
  let boiling_f = azimuth::@azimuth.add(azimuth::@azimuth.multiply(boiling_c, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}
test "shopping_cart_test" {
  // 测试购物车计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  // 计算每种商品的总价
  let total1 = azimuth::@azimuth.multiply(item1_price, item1_quantity)  // 398
  let total2 = azimuth::@azimuth.multiply(item2_price, item2_quantity)  // 297
  
  // 计算购物车总价
  let cart_total = azimuth::@azimuth.add(total1, total2)  // 695
  @azimuth.assert_eq(695, cart_total)
}
test "area_calculation_test" {
  // 测试面积计算
  let length = 10
  let width = 5
  
  // 计算矩形面积
  let rectangle_area = azimuth::@azimuth.multiply(length, width)
  @azimuth.assert_eq(50, rectangle_area)
  
  // 计算三角形面积（底 × 高 ÷ 2）
  let base = 8
  let height = 6
  let triangle_area = azimuth::@azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(24, triangle_area)
}
test "conditional_logic_test" {
  // 测试条件逻辑
  let x = 15
  let y = 20
  
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let condition = x < y
  let result = if condition { azimuth::@azimuth.add(x, y) } else { azimuth::@azimuth.multiply(x, y) }
  
  // 由于 15 < 20 为真，所以应该计算 15 + 20 = 35
  @azimuth.assert_eq(35, result)
  
  // 反向测试
  let condition2 = x > y
  let result2 = if condition2 { azimuth::@azimuth.add(x, y) } else { azimuth::@azimuth.multiply(x, y) }
  
  // 由于 15 > 20 为假，所以应该计算 15 * 20 = 300
  @azimuth.assert_eq(300, result2)
}
test "assertion_functions_test" {
  // 测试断言函数
  let true_value = true
  let false_value = false
  let equal_value = 42
  let string_value = "test"
  
  // 测试断言函数
  @azimuth.assert_true(true_value)
  @azimuth.assert_false(false_value)
  @azimuth.assert_eq(equal_value, 42)
  @azimuth.assert_eq_string(string_value, "test")
}