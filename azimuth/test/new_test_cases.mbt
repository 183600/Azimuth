// 新增的 MoonBit 测试用例
// 这些测试用例提供了对 azimuth 库更多场景的验证

// 测试平方和立方计算
test "power_calculations" {
  // 计算平方
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(10, 10))
  @azimuth.assert_eq(144, @azimuth.multiply(12, 12))
  
  // 计算立方
  @azimuth.assert_eq(125, @azimuth.multiply(@azimuth.multiply(5, 5), 5))
  @azimuth.assert_eq(1000, @azimuth.multiply(@azimuth.multiply(10, 10), 10))
  @azimuth.assert_eq(1728, @azimuth.multiply(@azimuth.multiply(12, 12), 12))
  
  // 负数的平方和立方
  @azimuth.assert_eq(25, @azimuth.multiply(-5, -5))
  @azimuth.assert_eq(-125, @azimuth.multiply(@azimuth.multiply(-5, -5), -5))
}

// 测试阶乘计算（6!到10!）
test "advanced_factorial_calculations" {
  // 6! = 720
  let fact6 = @azimuth.multiply(6, @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1)))))
  @azimuth.assert_eq(720, fact6)
  
  // 7! = 5040
  let fact7 = @azimuth.multiply(7, fact6)
  @azimuth.assert_eq(5040, fact7)
  
  // 8! = 40320
  let fact8 = @azimuth.multiply(8, fact7)
  @azimuth.assert_eq(40320, fact8)
}

// 测试平均数计算
test "average_calculations" {
  // 计算两个数的平均数
  let a = 15
  let b = 25
  let sum = @azimuth.add(a, b)
  let avg = sum / 2
  @azimuth.assert_eq(20, avg)
  
  // 计算三个数的平均数
  let x = 12
  let y = 18
  let z = 24
  let sum3 = @azimuth.add(@azimuth.add(x, y), z)
  let avg3 = sum3 / 3
  @azimuth.assert_eq(18, avg3)
  
  // 包含负数的平均数
  let p = 10
  let q = -5
  let sum_neg = @azimuth.add(p, q)
  let avg_neg = sum_neg / 2
  @azimuth.assert_eq(2, avg_neg)
}

// 测试余数计算相关
test "remainder_related_calculations" {
  // 模拟余数计算：a = q * d + r，其中 r < d
  let dividend = 17
  let divisor = 5
  let quotient = 3  // 17 / 5 = 3 余 2
  let product = @azimuth.multiply(divisor, quotient)
  let remainder = @azimuth.add(dividend, -product)
  @azimuth.assert_eq(2, remainder)
  
  // 另一个例子
  let dividend2 = 29
  let divisor2 = 4
  let quotient2 = 7  // 29 / 4 = 7 余 1
  let product2 = @azimuth.multiply(divisor2, quotient2)
  let remainder2 = @azimuth.add(dividend2, -product2)
  @azimuth.assert_eq(1, remainder2)
}

// 测试百分比计算
test "percentage_calculations" {
  // 计算百分比：x% of y = y * x / 100
  let value = 500
  let percentage = 20
  let result = @azimuth.multiply(value, percentage) / 100
  @azimuth.assert_eq(100, result)  // 20% of 500 = 100
  
  // 计算增长：原始值 + 增长值
  let original = 1000
  let growth_rate = 15
  let growth_amount = @azimuth.multiply(original, growth_rate) / 100
  let new_value = @azimuth.add(original, growth_amount)
  @azimuth.assert_eq(1150, new_value)  // 1000 + 15% of 1000 = 1150
  
  // 计算折扣：原价 - 折扣值
  let original_price = 200
  let discount_rate = 25
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(150, discounted_price)  // 200 - 25% of 200 = 150
}

// 测试字符串长度和复杂度
test "string_length_and_complexity" {
  // 测试包含各种字符的字符串
  let mixed_chars = "a1!b2@c3#d4$e5%f6^g7&h8*i9(j0)k_l+m=n-o"
  let result = @azimuth.greet(mixed_chars)
  let expected = "Hello, " + mixed_chars + "!"
  @azimuth.assert_eq_string(expected, result)
  
  // 测试只包含数字的字符串
  let numbers_only = "1234567890"
  let result2 = @azimuth.greet(numbers_only)
  let expected2 = "Hello, " + numbers_only + "!"
  @azimuth.assert_eq_string(expected2, result2)
  
  // 测试只包含特殊字符的字符串
  let special_only = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result3 = @azimuth.greet(special_only)
  let expected3 = "Hello, " + special_only + "!"
  @azimuth.assert_eq_string(expected3, result3)
}

// 测试数列求和
test "series_summation" {
  // 等差数列求和：1 + 2 + 3 + ... + 10
  let sum1 = @azimuth.add(1, @azimuth.add(2, @azimuth.add(3, @azimuth.add(4, @azimuth.add(5, @azimuth.add(6, @azimuth.add(7, @azimuth.add(8, @azimuth.add(9, 10)))))))))
  @azimuth.assert_eq(55, sum1)
  
  // 等比数列求和：2 + 4 + 8 + 16
  let term1 = 2
  let term2 = @azimuth.multiply(2, 2)
  let term3 = @azimuth.multiply(@azimuth.multiply(2, 2), 2)
  let term4 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)
  let sum2 = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(30, sum2)
  
  // 平方数列求和：1² + 2² + 3² + 4² + 5²
  let sq1 = @azimuth.multiply(1, 1)
  let sq2 = @azimuth.multiply(2, 2)
  let sq3 = @azimuth.multiply(3, 3)
  let sq4 = @azimuth.multiply(4, 4)
  let sq5 = @azimuth.multiply(5, 5)
  let sum3 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(sq1, sq2), sq3), sq4), sq5)
  @azimuth.assert_eq(55, sum3)
}

// 测试面积和体积计算
test "area_and_volume_calculations" {
  // 圆形面积（近似值，使用π≈3）
  let radius = 5
  let pi_approx = 3
  let circle_area = @azimuth.multiply(@azimuth.multiply(radius, radius), pi_approx)
  @azimuth.assert_eq(75, circle_area)  // 5² * 3 = 75
  
  // 三角形面积：底 × 高 ÷ 2
  let base = 8
  let height = 6
  let triangle_area = @azimuth.multiply(base, height) / 2
  @azimuth.assert_eq(24, triangle_area)
  
  // 梯形面积：(上底 + 下底) × 高 ÷ 2
  let top_base = 4
  let bottom_base = 10
  let trapezoid_height = 5
  let trapezoid_area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), trapezoid_height) / 2
  @azimuth.assert_eq(35, trapezoid_area)
  
  // 长方体体积：长 × 宽 × 高
  let length = 6
  let width = 4
  let height = 3
  let volume = @azimuth.multiply(@azimuth.multiply(length, width), height)
  @azimuth.assert_eq(72, volume)
}

// 测试速度和距离计算
test "speed_and_distance_calculations" {
  // 距离 = 速度 × 时间
  let speed = 60  // km/h
  let time = 2.5  // 小时（使用整数运算，表示为25/10）
  let distance = @azimuth.multiply(speed, 25) / 10
  @azimuth.assert_eq(150, distance)  // 60 * 2.5 = 150 km
  
  // 平均速度：总距离 ÷ 总时间
  let total_distance = 300
  let total_time = 5
  let avg_speed = total_distance / total_time
  @azimuth.assert_eq(60, avg_speed)
  
  // 相对速度：同向相减，反向相加
  let speed1 = 80
  let speed2 = 60
  let same_direction = @azimuth.add(speed1, -speed2)  // 同向行驶
  let opposite_direction = @azimuth.add(speed1, speed2)  // 反向行驶
  @azimuth.assert_eq(20, same_direction)
  @azimuth.assert_eq(140, opposite_direction)
}

// 测试货币和汇率转换
test "currency_and_exchange_calculations" {
  // 货币转换：美元转人民币（假设汇率1:7）
  let usd = 100
  let exchange_rate = 7
  let cny = @azimuth.multiply(usd, exchange_rate)
  @azimuth.assert_eq(700, cny)
  
  // 计算含税价格：原价 + 税额
  let original_price = 500
  let tax_rate = 8  // 8%
  let tax_amount = @azimuth.multiply(original_price, tax_rate) / 100
  let price_with_tax = @azimuth.add(original_price, tax_amount)
  @azimuth.assert_eq(540, price_with_tax)
  
  // 计算折扣后的最终价格
  let listed_price = 1000
  let discount1 = 10  // 第一级折扣10%
  let discount2 = 5   // 第二级折扣5%
  
  let discounted_price1 = @azimuth.add(listed_price, -@azimuth.multiply(listed_price, discount1) / 100)
  let final_price = @azimuth.add(discounted_price1, -@azimuth.multiply(discounted_price1, discount2) / 100)
  @azimuth.assert_eq(855, final_price)  // 1000 * 0.9 * 0.95 = 855
}