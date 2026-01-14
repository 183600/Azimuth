// 额外的 MoonBit 测试用例
// 测试实际应用场景和边界情况

test "calculate_discounted_price" {
  // 测试折扣计算：原价 - 折扣金额
  let original_price = 1000
  // 20% 折扣金额：1000 * 20 / 100 = 200
  let discount_amount = 200
  let final_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(800, final_price)
}

test "calculate_compound_interest" {
  // 测试复利计算（简化版）：本金 + 本金 × 利率 × 年数
  let principal = 5000
  let rate = 8  // 8% 年利率
  let years = 3
  // 计算利息：5000 * 8 * 3 / 100 = 1200
  let interest = @azimuth.multiply(@azimuth.multiply(principal, rate), years) / 100
  let total = @azimuth.add(principal, interest)
  @azimuth.assert_eq(6200, total)  // 5000 + 1200 = 6200
}

test "calculate_bmi_category" {
  // 测试BMI分类计算（简化版）：使用近似值计算
  let weight_kg = 70
  let height_cm = 175
  
  // 使用简化的BMI近似计算：BMI ≈ 体重(kg) × 10000 / (身高(cm) × 身高(cm))
  // 由于只有整数运算，我们使用预计算的近似值
  let bmi_approx = @azimuth.multiply(weight_kg, 10000) / @azimuth.multiply(height_cm, height_cm)
  @azimuth.assert_eq(22, bmi_approx)  // 70*10000/(175*175) ≈ 22.86，取整数部分22
}

test "calculate_triangle_area" {
  // 测试三角形面积计算：底 × 高 ÷ 2
  let base = 12
  let height = 8
  // 使用乘法和减法来模拟除法：area = base * height / 2
  let product = @azimuth.multiply(base, height)
  // 由于不能直接除法，我们使用预计算结果
  let area = product / 2  // 12 * 8 / 2 = 48
  @azimuth.assert_eq(48, area)
}

test "calculate_celsius_to_fahrenheit" {
  // 测试摄氏度转华氏度：使用近似公式 F ≈ C × 2 + 32
  let celsius = 25
  // 简化的温度转换（近似值）
  let fahrenheit_approx = @azimuth.add(@azimuth.multiply(celsius, 2), 32)
  @azimuth.assert_eq(82, fahrenheit_approx)  // 25 * 2 + 32 = 82（近似值）
}

test "calculate_shipping_cost" {
  // 测试运费计算：基础费用 + 重量费用 + 距离费用
  let base_fee = 50
  let weight_kg = 3
  let weight_rate = 10  // 每公斤10元
  let distance_km = 15
  let distance_rate = 2  // 每公里2元
  
  let weight_cost = @azimuth.multiply(weight_kg, weight_rate)
  let distance_cost = @azimuth.multiply(distance_km, distance_rate)
  let total_cost = @azimuth.add(@azimuth.add(base_fee, weight_cost), distance_cost)
  
  @azimuth.assert_eq(100, total_cost)  // 50 + 3*10 + 15*2 = 50 + 30 + 30 = 110
}

test "calculate_tax_with_brackets" {
  // 测试分段税率计算（简化版）
  let income = 50000
  let first_bracket_limit = 20000
  let first_bracket_rate = 10  // 10%
  let second_bracket_rate = 20  // 20%
  
  // 第一段税款：20000 * 10 / 100 = 2000
  let first_bracket_tax = @azimuth.multiply(first_bracket_limit, first_bracket_rate) / 100
  
  // 第二段税款：(50000-20000) * 20 / 100 = 6000
  let remaining_income = @azimuth.add(income, -first_bracket_limit)
  let second_bracket_tax = @azimuth.multiply(remaining_income, second_bracket_rate) / 100
  
  // 总税款
  let total_tax = @azimuth.add(first_bracket_tax, second_bracket_tax)
  
  @azimuth.assert_eq(8000, total_tax)  // 2000 + 6000 = 8000
}

test "calculate_elapsed_time" {
  // 测试经过时间计算：结束时间 - 开始时间
  let start_hour = 9
  let start_minute = 30
  let end_hour = 14
  let end_minute = 45
  
  // 转换为分钟
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)
  
  // 计算差值
  let elapsed_minutes = @azimuth.add(end_total, -start_total)
  
  @azimuth.assert_eq(315, elapsed_minutes)  // (14*60+45) - (9*60+30) = 885 - 570 = 315分钟
}

test "greet_with_unicode_characters" {
  // 测试包含Unicode字符的问候
  let unicode_name = "张三"
  let expected_result = "Hello, 张三!"
  
  // 测试Unicode名称
  let result1 = @azimuth.greet(unicode_name)
  @azimuth.assert_eq_string(expected_result, result1)
  
  // 测试包含特殊字符的名称
  let special_name = "John & Jane"
  let result2 = @azimuth.greet(special_name)
  @azimuth.assert_eq_string("Hello, John & Jane!", result2)
}
