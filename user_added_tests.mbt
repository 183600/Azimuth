// 用户添加的 MoonBit 测试用例
// 使用标准 MoonBit 测试语法，测试 azimuth 模块的功能

test "calculation_of_circle_properties" {
  // 圆形属性计算测试
  let radius = 5
  
  // 计算圆的周长（简化公式：2 × 3 × r）
  let circumference = multiply(multiply(2, 3), radius)
  assert_eq(30, circumference)
  
  // 计算圆的面积（简化公式：3 × r²）
  let area = multiply(3, multiply(radius, radius))
  assert_eq(75, area)
}

test "temperature_conversion_celsius_to_fahrenheit" {
  // 摄氏度转华氏度测试（简化公式：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  // 测试沸点
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
}

test "shopping_cart_total_calculation" {
  // 购物车总价计算测试
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 299
  let item2_quantity = 1
  let shipping_fee = 20
  
  // 计算商品总价
  let item1_total = multiply(item1_price, item1_quantity)
  let item2_total = multiply(item2_price, item2_quantity)
  let subtotal = add(item1_total, item2_total)
  
  // 计算最终总价（含运费）
  let total = add(subtotal, shipping_fee)
  
  // 验证结果：199*2 + 299*1 + 20 = 398 + 299 + 20 = 717
  assert_eq(717, total)
}

test "simple_interest_calculation" {
  // 简单利息计算测试
  let principal = 10000
  let rate = 5  // 5%
  let time = 3  // 3年
  
  // 计算利息：本金 × 利率 × 时间
  let interest = multiply(principal, multiply(rate, time))
  
  // 计算本息和：本金 + 利息
  let total = add(principal, interest)
  
  // 验证结果：10000 + 10000*5*3 = 10000 + 150000 = 160000
  assert_eq(160000, total)
}

test "array_index_2d_to_1d_conversion" {
  // 二维数组转一维数组索引计算测试
  let row = 3
  let col = 4
  let width = 5
  
  // 计算一维数组索引：index = row * width + col
  let index = add(multiply(row, width), col)
  assert_eq(19, index)
  
  // 测试边界情况：第一个元素
  let first_index = add(multiply(0, 0), 0)
  assert_eq(0, first_index)
}

test "time_conversion_hours_to_seconds" {
  // 时间转换测试：小时转秒
  let hours = 2
  let minutes = 30
  
  // 转换为总分钟数
  let total_minutes = add(multiply(hours, 60), minutes)
  assert_eq(150, total_minutes)
  
  // 转换为总秒数
  let total_seconds = multiply(total_minutes, 60)
  assert_eq(9000, total_seconds)
}

test "percentage_discount_calculation" {
  // 百分比折扣计算测试
  let original_price = 500
  let discount_percentage = 20  // 20%折扣
  
  // 计算折扣金额
  let discount_amount = multiply(original_price, discount_percentage) / 100
  
  // 计算折后价格
  let discounted_price = add(original_price, -discount_amount)
  
  // 验证结果：500 - 500*20/100 = 500 - 100 = 400
  assert_eq(400, discounted_price)
}

test "basic_statistics_calculation" {
  // 基础统计计算测试
  let num1 = 85
  let num2 = 90
  let num3 = 78
  let num4 = 92
  let num5 = 80
  
  // 计算总和
  let sum = add(add(add(add(num1, num2), num3), num4), num5)
  assert_eq(425, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(85, average)
}

test "string_with_special_characters" {
  // 特殊字符字符串测试
  let email = "user@example.com"
  let phone = "123-456-7890"
  let alphanumeric = "ABC123xyz"
  
  // 测试包含特殊字符的字符串
  let email_result = greet(email)
  assert_eq_string("Hello, user@example.com!", email_result)
  
  let phone_result = greet(phone)
  assert_eq_string("Hello, 123-456-7890!", phone_result)
  
  let alphanumeric_result = greet(alphanumeric)
  assert_eq_string("Hello, ABC123xyz!", alphanumeric_result)
}

test "compound_calculation_scenario" {
  // 复合计算场景测试：计算矩形的面积和周长
  let length = 8
  let width = 6
  
  // 计算面积：长 × 宽
  let area = multiply(length, width)
  assert_eq(48, area)
  
  // 计算周长：2 × (长 + 宽)
  let perimeter = multiply(2, add(length, width))
  assert_eq(28, perimeter)
  
  // 计算对角线长度的平方（勾股定理）：a² + b²
  let diagonal_squared = add(multiply(length, length), multiply(width, width))
  assert_eq(100, diagonal_squared)
}