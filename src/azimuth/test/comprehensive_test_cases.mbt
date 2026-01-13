// 综合测试用例 - 覆盖各种场景

test "add_negative_numbers" {
  // 测试负数相加
  @azimuth.assert_eq(@azimuth.add(-5, -3), -8)
  @azimuth.assert_eq(@azimuth.add(-10, 5), -5)
  @azimuth.assert_eq(@azimuth.add(7, -2), 5)
}

test "multiply_decimal_simulation" {
  // 模拟小数乘法：通过整数运算实现
  // 2.5 * 3.2 = 8.0，通过 25 * 32 / 100 = 8 模拟
  let result = @azimuth.multiply(25, 32) / 100
  @azimuth.assert_eq(result, 8)
  
  // 1.5 * 4.0 = 6.0，通过 15 * 40 / 100 = 6 模拟
  let result2 = @azimuth.multiply(15, 40) / 100
  @azimuth.assert_eq(result2, 6)
}

test "greet_with_unicode_names" {
  // 测试包含 Unicode 字符的名称
  @azimuth.assert_eq_string(@azimuth.greet("张三"), "Hello, 张三!")
  @azimuth.assert_eq_string(@azimuth.greet("José"), "Hello, José!")
  @azimuth.assert_eq_string(@azimuth.greet("🌟Star🌟"), "Hello, 🌟Star🌟!")
}

test "complex_expression_evaluation" {
  // 测试复杂表达式：(a + b) * c - d
  let a = 10
  let b = 20
  let c = 3
  let d = 15
  
  let sum_ab = @azimuth.add(a, b)
  let product = @azimuth.multiply(sum_ab, c)
  let result = @azimuth.add(product, -d)
  
  @azimuth.assert_eq(result, 75)  // (10 + 20) * 3 - 15 = 75
}

test "boundary_value_operations" {
  // 测试边界值操作
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与1相加（应该返回最大值，防止溢出）
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  
  // 测试最小值与-1相加（应该返回最小值，防止溢出）
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  
  // 测试最大值与-1相乘
  @azimuth.assert_eq(@azimuth.multiply(max_val, -1), min_val)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(total1, total2)
  
  @azimuth.assert_eq(cart_total, 1195)  // 299*2 + 199*3 = 598 + 597 = 1195
}

test "temperature_conversion" {
  // 测试温度转换（简化版）
  // 摄氏度转华氏度：F = C * 2 + 30（整数近似）
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(fahrenheit, 70)  // 20 * 2 + 30 = 70
}

test "area_perimeter_calculation" {
  // 测试面积和周长计算
  let length = 10
  let width = 5
  
  // 计算矩形面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(area, 50)
  
  // 计算矩形周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(perimeter, 30)
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet(" "), "Hello,  !")
  @azimuth.assert_eq_string(@azimuth.greet("A"), "Hello, A!")
  @azimuth.assert_eq_string(@azimuth.greet("VeryLongNameForTesting"), "Hello, VeryLongNameForTesting!")
}

test "financial_calculation" {
  // 测试金融计算场景
  let principal = 1000
  let rate = 5  // 5%
  let years = 3
  
  // 简单利息计算：本金 + 本金 * 利率 * 年数
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(total, 16000)  // 1000 + 1000*5*3 = 16000
}