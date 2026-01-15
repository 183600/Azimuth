// 用户请求的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "string_concatenation_with_unicode" {
  // 测试包含 Unicode 字符的字符串连接
  let chinese_name = "张三"
  let result = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 张三!", result)
  
  // 测试包含 emoji 的字符串
  let emoji_name = "🌍"
  let result2 = @azimuth.greet(emoji_name)
  @azimuth.assert_eq_string("Hello, 🌍!", result2)
}

test "mathematical_operations_with_negative_numbers" {
  // 测试负数的数学运算
  let a = -10
  let b = -20
  
  // 负数相加
  let sum_result = @azimuth.add(a, b)
  @azimuth.assert_eq(-30, sum_result)
  
  // 负数相乘
  let mul_result = @azimuth.multiply(a, b)
  @azimuth.assert_eq(200, mul_result)
  
  // 正负数相乘
  let mixed_result = @azimuth.multiply(a, 5)
  @azimuth.assert_eq(-50, mixed_result)
}

test "boundary_value_arithmetic" {
  // 测试边界值的算术运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值相加
  let sum_result = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, sum_result)
  
  // 最大值与1相加（应保持最大值，防止溢出）
  let max_add_result = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, max_add_result)
  
  // 最小值与-1相加（应保持最小值，防止溢出）
  let min_add_result = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, min_add_result)
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算圆的周长和面积
  let radius = 5
  let pi_approx = 3  // 简化的π值
  
  // 计算周长：2 × π × r
  let circumference = @azimuth.multiply(@azimuth.multiply(2, pi_approx), radius)
  @azimuth.assert_eq(30, circumference)
  
  // 计算面积：π × r²
  let area = @azimuth.multiply(pi_approx, @azimuth.multiply(radius, radius))
  @azimuth.assert_eq(75, area)
}

test "financial_calculation_compound_interest" {
  // 测试金融计算：复利计算
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 简化的复利计算：本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))
  let total = @azimuth.multiply(principal, growth_factor)
  @azimuth.assert_eq(160000, total)  // 10000 × (1 + 5×3) = 10000 × 16 = 160000
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30
  
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "shopping_cart_total_calculation" {
  // 测试购物车总价计算
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  
  // 计算最终总价（含运费）
  let total = @azimuth.add(subtotal, shipping_fee)
  
  // 验证结果：199×2 + 299×1 + 20 = 398 + 299 + 20 = 717
  @azimuth.assert_eq(717, total)
}

test "average_calculation_of_numbers" {
  // 测试数字平均值计算
  let num1 = 85
  let num2 = 90
  let num3 = 95
  let num4 = 80
  let num5 = 100
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(num1, num2), num3), num4), num5)
  
  // 计算平均值
  let average = sum / 5
  
  // 验证结果：(85 + 90 + 95 + 80 + 100) / 5 = 450 / 5 = 90
  @azimuth.assert_eq(90, average)
}

test "sequential_operations_evaluation" {
  // 测试连续运算的求值
  let x = 10
  let y = 20
  let z = 5
  
  // 计算 (x + y) * z - (x - y)
  let sum_xy = @azimuth.add(x, y)
  let product = @azimuth.multiply(sum_xy, z)
  let diff_xy = @azimuth.add(x, -y)
  let result = @azimuth.add(product, diff_xy)
  
  // 验证结果：(10 + 20) * 5 - (10 - 20) = 30 * 5 - (-10) = 150 + 10 = 160
  @azimuth.assert_eq(160, result)
}

test "string_edge_cases_handling" {
  // 测试字符串边界情况处理
  // 测试空字符串
  let empty_result = @azimuth.greet("")
  @azimuth.assert_eq_string("Hello, !", empty_result)
  
  // 测试包含空格的字符串
  let spaced_name = " John Doe "
  let spaced_result = @azimuth.greet(spaced_name)
  @azimuth.assert_eq_string("Hello,  John Doe !", spaced_result)
  
  // 测试包含特殊字符的字符串
  let special_name = "user@domain.com"
  let special_result = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, user@domain.com!", special_result)
}