// 简单的 MoonBit 测试用例
// 导入 azimuth 包以使用其中的函数
test "add_positive_numbers" {
  // 测试两个正数相加
  @azimuth.assert_eq(10, azimuth::@azimuth.add(3, 7))
  @azimuth.assert_eq(100, azimuth::@azimuth.add(50, 50))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
}
test "add_negative_numbers" {
  // 测试两个负数相加
  @azimuth.assert_eq(-10, azimuth::@azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, azimuth::@azimuth.add(-50, -50))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
}
test "add_mixed_numbers" {
  // 测试正负数相加
  @azimuth.assert_eq(0, azimuth::@azimuth.add(5, -5))
  @azimuth.assert_eq(-2, azimuth::@azimuth.add(3, -5))
  @azimuth.assert_eq(2, azimuth::@azimuth.add(-3, 5))
}
test "multiply_basic" {
  // 测试基本乘法
  @azimuth.assert_eq(6, azimuth::@azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(5, 0))
  @azimuth.assert_eq(5, azimuth::@azimuth.multiply(5, 1))
  @azimuth.assert_eq(-5, azimuth::@azimuth.multiply(5, -1))
}
test "multiply_negative" {
  // 测试负数乘法
  @azimuth.assert_eq(15, azimuth::@azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, azimuth::@azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, azimuth::@azimuth.multiply(-3, 5))
}
test "greet_basic" {
  // 测试基本问候功能
  @azimuth.assert_eq_string("Hello, World!", azimuth::@azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, MoonBit!", azimuth::@azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
}
test "greet_special_chars" {
  // 测试包含特殊字符的问候
  @azimuth.assert_eq_string("Hello, 测试!", azimuth::@azimuth.greet("测试"))
  @azimuth.assert_eq_string("Hello, 123!", azimuth::@azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, @#$!", azimuth::@azimuth.greet("@#$"))
}
test "complex_operations" {
  // 测试复合运算
  // (2 + 3) * 4 = 20
  let sum = azimuth::@azimuth.add(2, 3)
  let result = azimuth::@azimuth.multiply(sum, 4)
  @azimuth.assert_eq(20, result)
  
  // 5 + (6 * 7) = 47
  let product = azimuth::@azimuth.multiply(6, 7)
  let result2 = azimuth::@azimuth.add(5, product)
  @azimuth.assert_eq(47, result2)
}
test "boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值溢出
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  
  // 测试最小值溢出
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
}
test "sequential_operations" {
  // 测试连续运算
  // 1 + 2 + 3 + 4 + 5 = 15
  let result1 = azimuth::@azimuth.add(azimuth::@azimuth.add(1, 2), azimuth::@azimuth.add(3, 4))
  let final_sum = azimuth::@azimuth.add(result1, 5)
  @azimuth.assert_eq(15, final_sum)
  
  // 2 * 3 * 4 = 24
  let result2 = azimuth::@azimuth.multiply(azimuth::@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, result2)
}
test "string_processing" {
  // 测试字符串处理
  @azimuth.assert_eq_string("Hello, John Doe!", azimuth::@azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,   spaced   !", azimuth::@azimuth.greet("  spaced  "))
  @azimuth.assert_eq_string("Hello, emoji😊!", azimuth::@azimuth.greet("emoji😊"))
  @azimuth.assert_eq_string("Hello, newline\n!", azimuth::@azimuth.greet("newline\n"))
}
test "mathematical_sequences" {
  // 测试数学序列
  // 等差数列：1 + 3 + 5 + 7 + 9 = 25
  let sum1 = azimuth::@azimuth.add(azimuth::@azimuth.add(1, 3), azimuth::@azimuth.add(5, 7))
  let total1 = azimuth::@azimuth.add(sum1, 9)
  @azimuth.assert_eq(25, total1)
  
  // 等比数列：2 + 4 + 8 = 14
  let sum2 = azimuth::@azimuth.add(azimuth::@azimuth.multiply(1, 2), azimuth::@azimuth.multiply(2, 2))
  let total2 = azimuth::@azimuth.add(sum2, azimuth::@azimuth.multiply(3, 2))
  @azimuth.assert_eq(14, total2)
}
test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物车计算：商品价格 * 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 15
  
  let subtotal = azimuth::@azimuth.multiply(item_price, quantity)
  let total = azimuth::@azimuth.add(subtotal, shipping)
  @azimuth.assert_eq(612, total)  // 199 * 3 + 15 = 597 + 15 = 612
  
  // 温度转换：摄氏度转华氏度（简化版）
  let celsius = 25
  let fahrenheit = azimuth::@azimuth.add(azimuth::@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}