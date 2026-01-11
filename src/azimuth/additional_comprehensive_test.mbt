// 额外的综合测试用例

// 测试斐波那契数列相关计算
///|
test "fibonacci_sequence_calculations" {
  // 使用加法计算前几个斐波那契数
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)  // 2
  let fib4 = @azimuth.add(fib2, fib3)  // 3
  let fib5 = @azimuth.add(fib3, fib4)  // 5
  let fib6 = @azimuth.add(fib4, fib5)  // 8
  let fib7 = @azimuth.add(fib5, fib6)  // 13
  let fib8 = @azimuth.add(fib6, fib7)  // 21
  
  assert_eq(2, fib3)
  assert_eq(3, fib4)
  assert_eq(5, fib5)
  assert_eq(8, fib6)
  assert_eq(13, fib7)
  assert_eq(21, fib8)
  
  // 测试斐波那契数的乘法性质
  let fib_product = @azimuth.multiply(fib5, fib6)  // 5 * 8 = 40
  assert_eq(40, fib_product)
}

// 测试阶乘计算
///|
test "factorial_calculations" {
  // 计算 5! = 5 * 4 * 3 * 2 * 1
  let fact1 = 1
  let fact2 = @azimuth.multiply(fact1, 2)  // 2
  let fact3 = @azimuth.multiply(fact2, 3)  // 6
  let fact4 = @azimuth.multiply(fact3, 4)  // 24
  let fact5 = @azimuth.multiply(fact4, 5)  // 120
  
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
}

// 测试幂运算（通过重复乘法）
///|
test "power_operations" {
  // 计算 2^3 = 2 * 2 * 2
  let power2_1 = 2
  let power2_2 = @azimuth.multiply(power2_1, 2)  // 4
  let power2_3 = @azimuth.multiply(power2_2, 2)  // 8
  
  assert_eq(4, power2_2)
  assert_eq(8, power2_3)
  
  // 计算 3^2 = 3 * 3
  let power3_2 = @azimuth.multiply(3, 3)
  assert_eq(9, power3_2)
  
  // 计算 (-2)^3 = -2 * -2 * -2
  let neg_power2_1 = -2
  let neg_power2_2 = @azimuth.multiply(neg_power2_1, -2)  // 4
  let neg_power2_3 = @azimuth.multiply(neg_power2_2, -2)  // -8
  
  assert_eq(4, neg_power2_2)
  assert_eq(-8, neg_power2_3)
}

// 测试平均数计算
///|
test "average_calculations" {
  // 计算 (10 + 20 + 30) / 3 = 20
  let sum1 = @azimuth.add(10, 20)  // 30
  let sum2 = @azimuth.add(sum1, 30)  // 60
  // 注意：这里不能直接除法，所以我们用乘法来验证
  let average_times_3 = sum2  // 60
  let expected_times_3 = @azimuth.multiply(20, 3)  // 60
  assert_eq(expected_times_3, average_times_3)
  
  // 测试负数的平均数
  let neg_sum = @azimuth.add(-10, -20)  // -30
  let neg_sum_total = @azimuth.add(neg_sum, -30)  // -60
  let neg_expected = @azimuth.multiply(-20, 3)  // -60
  assert_eq(neg_expected, neg_sum_total)
}

// 测试字符串处理的高级场景
///|
test "advanced_string_processing" {
  // 测试长字符串拼接
  let long_name = "非常长的用户名称用于测试字符串处理功能"
  let greeting = @azimuth.greet(long_name)
  assert_eq(true, greeting.contains("非常长的用户名称用于测试字符串处理功能"))
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()"
  let special_greeting = @azimuth.greet(special_chars)
  assert_eq(true, special_greeting.contains("!@#$%^&*()"))
  
  // 测试包含空格的字符串
  let spaced_name = "张 三"
  let spaced_greeting = @azimuth.greet(spaced_name)
  assert_eq(true, spaced_greeting.contains("张 三"))
}

// 测试数列求和
///|
test "arithmetic_series_sum" {
  // 计算等差数列 1 + 2 + 3 + 4 + 5 = 15
  let sum1 = @azimuth.add(1, 2)  // 3
  let sum2 = @azimuth.add(sum1, 3)  // 6
  let sum3 = @azimuth.add(sum2, 4)  // 10
  let sum4 = @azimuth.add(sum3, 5)  // 15
  
  assert_eq(15, sum4)
  
  // 计算等差数列 10 + 20 + 30 + 40 = 100
  let series_sum1 = @azimuth.add(10, 20)  // 30
  let series_sum2 = @azimuth.add(series_sum1, 30)  // 60
  let series_sum3 = @azimuth.add(series_sum2, 40)  // 100
  
  assert_eq(100, series_sum3)
  
  // 使用乘法验证等差数列求和公式：n/2 * (first + last)
  // 对于数列 1, 2, 3, 4, 5：n=5, first=1, last=5
  // 和 = 5/2 * (1 + 5) = 2 * 6 = 12（这里因为整数除法，所以不完全准确）
  // 我们用乘法来验证：5 * (1 + 5) = 30，然后我们知道实际和是15
  let formula_check = @azimuth.multiply(5, @azimuth.add(1, 5))  // 30
  assert_eq(30, formula_check)
}

// 测试几何计算
///|
test "geometric_calculations" {
  // 计算正方形的面积和周长
  let side = 10
  let square_area = @azimuth.multiply(side, side)  // 100
  let square_perimeter = @azimuth.multiply(4, side)  // 40
  
  assert_eq(100, square_area)
  assert_eq(40, square_perimeter)
  
  // 计算三角形的面积（底 × 高 ÷ 2）
  let triangle_base = 8
  let triangle_height = 6
  let triangle_area_double = @azimuth.multiply(triangle_base, triangle_height)  // 48
  // 实际面积应该是 48 ÷ 2 = 24，但我们不能直接除法
  // 所以我们验证 24 × 2 = 48
  let expected_area_times_2 = @azimuth.multiply(24, 2)  // 48
  assert_eq(expected_area_times_2, triangle_area_double)
  
  // 计算梯形的面积（(上底 + 下底) × 高 ÷ 2）
  let trapezoid_top = 5
  let trapezoid_bottom = 10
  let trapezoid_height = 4
  let trapezoid_sum = @azimuth.add(trapezoid_top, trapezoid_bottom)  // 15
  let trapezoid_area_double = @azimuth.multiply(trapezoid_sum, trapezoid_height)  // 60
  // 实际面积应该是 60 ÷ 2 = 30
  let expected_trapezoid_times_2 = @azimuth.multiply(30, 2)  // 60
  assert_eq(expected_trapezoid_times_2, trapezoid_area_double)
}

// 测试时间计算（秒到分钟和小时的转换）
///|
test "time_calculations" {
  // 3600 秒 = 60 分钟 = 1 小时
  let seconds_in_minute = 60
  let seconds_in_hour = @azimuth.multiply(seconds_in_minute, 60)  // 3600
  let hours_in_day = 24
  let seconds_in_day = @azimuth.multiply(seconds_in_hour, hours_in_day)  // 86400
  
  assert_eq(3600, seconds_in_hour)
  assert_eq(86400, seconds_in_day)
  
  // 计算一天半的秒数
  let day_and_a_half_hours = @azimuth.multiply(hours_in_day, 3)  // 72
  let day_and_a_half_seconds = @azimuth.multiply(day_and_a_half_hours, seconds_in_hour)  // 259200
  assert_eq(259200, day_and_a_half_seconds)
}

// 测试货币计算
///|
test "currency_calculations" {
  // 计算总价：商品价格 × 数量
  let price1 = 199  // 商品1价格（分）
  let quantity1 = 3  // 商品1数量
  let total1 = @azimuth.multiply(price1, quantity1)  // 597
  
  let price2 = 299  // 商品2价格（分）
  let quantity2 = 2  // 商品2数量
  let total2 = @azimuth.multiply(price2, quantity2)  // 598
  
  let price3 = 99  // 商品3价格（分）
  let quantity3 = 5  // 商品3数量
  let total3 = @azimuth.multiply(price3, quantity3)  // 495
  
  // 计算所有商品的总价
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)  // 1690
  
  assert_eq(597, total1)
  assert_eq(598, total2)
  assert_eq(495, total3)
  assert_eq(1690, subtotal)
  
  // 计算含税价格（假设税率10%）
  // 税额 = 总价 × 10 ÷ 100
  // 我们先计算 总价 × 10
  let tax_times_100 = @azimuth.multiply(subtotal, 10)  // 16900
  // 税额应该是 16900 ÷ 100 = 169
  let expected_tax = 169
  let tax_times_100_check = @azimuth.multiply(expected_tax, 100)  // 16900
  assert_eq(tax_times_100, tax_times_100_check)
  
  // 总含税价格
  let total_with_tax = @azimuth.add(subtotal, expected_tax)  // 1859
  assert_eq(1859, total_with_tax)
}