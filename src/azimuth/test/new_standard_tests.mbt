// 新增 MoonBit 标准测试用例
// 专注于尚未充分测试的场景

test "numeric_range_validation" {
  // 测试数值范围验证
  // 测试大数相加是否在合理范围内
  let large_sum = @azimuth.add(1000000, 2000000)
  @azimuth.assert_true(large_sum > 0)
  @azimuth.assert_true(large_sum >= 3000000)
  
  // 测试负数范围
  let negative_sum = @azimuth.add(-1000000, -2000000)
  @azimuth.assert_true(negative_sum < 0)
  @azimuth.assert_true(negative_sum <= -3000000)
  
  // 测试零值
  @azimuth.assert_eq(0, @azimuth.multiply(0, 123456789))
  @azimuth.assert_eq(123456789, @azimuth.add(0, 123456789))
}

test "string_formatting_scenarios" {
  // 测试字符串格式化场景
  // 测试短字符串
  let short_name = "Bob"
  let short_result = @azimuth.greet(short_name)
  @azimuth.assert_eq_string("Hello, Bob!", short_result)
  
  // 测试包含特殊字符的字符串
  let special_name = "user@domain.com"
  let special_result = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, user@domain.com!", special_result)
  
  // 测试包含数字的字符串
  let numeric_name = "Test123"
  let numeric_result = @azimuth.greet(numeric_name)
  @azimuth.assert_eq_string("Hello, Test123!", numeric_result)
}

test "math_function_combination" {
  // 测试数学函数组合
  // 计算 (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  
  let sum_ab = @azimuth.add(a, b)  // 8
  let product = @azimuth.multiply(sum_ab, c)  // 32
  @azimuth.assert_eq(32, product)
  
  // 计算 a * b + c * d
  let d = 2
  let product1 = @azimuth.multiply(a, b)  // 15
  let product2 = @azimuth.multiply(c, d)  // 8
  let result = @azimuth.add(product1, product2)  // 23
  @azimuth.assert_eq(23, result)
}

test "boundary_value_special_handling" {
  // 测试边界值特殊处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与正数相加
  let max_add = @azimuth.add(max_val, 100)
  @azimuth.assert_eq(max_val, max_add)
  
  // 测试最小值与负数相加
  let min_add = @azimuth.add(min_val, -100)
  @azimuth.assert_eq(min_val, min_add)
  
  // 测试最大值与负数相乘
  let max_mul_neg = @azimuth.multiply(max_val, -1)
  @azimuth.assert_eq(min_val, max_mul_neg)
  
  // 测试零与边界值
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
}

test "average_calculation" {
  // 测试平均值计算
  // 计算 (a + b + c) / 3
  let num1 = 90
  let num2 = 80
  let num3 = 85
  
  // 计算总和：90 + 80 + 85 = 255
  let sum = @azimuth.add(@azimuth.add(num1, num2), num3)
  // 计算平均值：255 / 3 = 85（整数除法）
  let average = sum / 3
  
  @azimuth.assert_eq(85, average)
}

test "area_calculations" {
  // 测试面积计算
  // 计算矩形面积：长 × 宽
  let length = 12
  let width = 8
  
  let rectangle_area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, rectangle_area)
  
  // 计算三角形面积：底 × 高 ÷ 2
  let base = 10
  let height = 6
  
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(30, triangle_area)
}

test "financial_calculation" {
  // 测试金融计算场景
  // 计算简单利息：本金 × 利率 × 时间
  let principal = 10000
  let rate = 5  // 5%
  let years = 3
  
  // 计算利息：10000 × 5 × 3 = 150000
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  // 计算总额：10000 + 150000 = 160000
  let total = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(160000, total)
}

test "temperature_conversion" {
  // 测试温度转换
  // 摄氏度转华氏度（简化版）：F = C × 2 + 30
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
  
  // 测试冰点
  let freezing = 0
  let freezing_f = @azimuth.add(@azimuth.multiply(freezing, 2), 30)
  @azimuth.assert_eq(30, freezing_f)
  
  // 测试沸点
  let boiling = 100
  let boiling_f = @azimuth.add(@azimuth.multiply(boiling, 2), 30)
  @azimuth.assert_eq(230, boiling_f)
}

test "conditional_logic" {
  // 测试条件逻辑
  // 如果 x < y，则计算 x + y，否则计算 x * y
  let x = 15
  let y = 20
  
  // 由于 15 < 20 为真，所以应该计算 15 + 20 = 35
  let condition = x < y
  let result = if condition { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  @azimuth.assert_eq(35, result)
  
  // 反向测试：如果 x > y，则计算 x + y，否则计算 x * y
  let condition2 = x > y
  let result2 = if condition2 { @azimuth.add(x, y) } else { @azimuth.multiply(x, y) }
  // 由于 15 > 20 为假，所以应该计算 15 * 20 = 300
  @azimuth.assert_eq(300, result2)
}

test "shopping_cart_total" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping = 20
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  
  // 计算商品总价
  let subtotal = @azimuth.add(total1, total2)
  
  // 计算最终总价（含运费）
  let final_total = @azimuth.add(subtotal, shipping)
  
  // 验证总价：299*2 + 199*3 + 20 = 598 + 597 + 20 = 1215
  @azimuth.assert_eq(1215, final_total)
}