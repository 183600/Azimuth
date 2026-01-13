// 额外的 MoonBit 测试用例
// 包含10个新的测试场景，覆盖各种边界情况和实际应用

// 测试用例 1: 阶乘计算
test "factorial_calculation" {
  // 计算阶乘：5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  
  // 手动计算阶乘
  let step1 = @azimuth.multiply(n, 4)  // 5 × 4 = 20
  let step2 = @azimuth.multiply(step1, 3)  // 20 × 3 = 60
  let step3 = @azimuth.multiply(step2, 2)  // 60 × 2 = 120
  let factorial = @azimuth.multiply(step3, 1)  // 120 × 1 = 120
  
  @azimuth.assert_eq(factorial, 120)
}

// 测试用例 2: 二进制转十进制
test "binary_number_conversion" {
  // 二进制转十进制的简化版本
  // 二进制数 1011 = 1×2³ + 0×2² + 1×2¹ + 1×2⁰ = 8 + 0 + 2 + 1 = 11
  
  let bit3 = 1  // 2³位
  let bit2 = 0  // 2²位
  let bit1 = 1  // 2¹位
  let bit0 = 1  // 2⁰位
  
  // 计算每一位的值
  let value3 = @azimuth.multiply(bit3, 8)  // 1 × 8 = 8
  let value2 = @azimuth.multiply(bit2, 4)  // 0 × 4 = 0
  let value1 = @azimuth.multiply(bit1, 2)  // 1 × 2 = 2
  let value0 = @azimuth.multiply(bit0, 1)  // 1 × 1 = 1
  
  // 求和
  let decimal_value = @azimuth.add(@azimuth.add(@azimuth.add(value3, value2), value1), value0)
  
  @azimuth.assert_eq(decimal_value, 11)
}

// 测试用例 3: 简化的BMI计算
test "simple_bmi_calculation" {
  // 简化的BMI计算场景
  // 使用近似公式：BMI ≈ 体重 + 身高差值
  
  let weight_kg = 70      // 体重：70kg
  let height_cm = 175     // 身高：175cm
  let ideal_height = 170  // 理想身高：170cm
  
  // 简化计算：BMI ≈ 体重 + (理想身高 - 实际身高)
  let height_diff = ideal_height - height_cm  // 170 - 175 = -5
  let bmi_approx = @azimuth.add(weight_kg, height_diff)  // 70 + (-5) = 65
  
  @azimuth.assert_eq(bmi_approx, 65)
}

// 测试用例 4: 简单利息计算
test "simple_interest_calculation" {
  // 简单利息计算
  // 总利息 = 本金 × 利率 × 年数
  
  let principal = 10000  // 本金
  let rate = 5           // 利率5%
  let years = 3          // 年数3年
  
  // 计算利息：10000 × 5 × 3 = 150000
  let interest = @azimuth.multiply(principal, @azimuth.multiply(rate, years))
  
  // 计算总金额：本金 + 利息/100
  let total_amount = @azimuth.add(principal, interest / 100)
  
  @azimuth.assert_eq(total_amount, 11500)  // 10000 + 150000/100 = 10000 + 1500 = 11500
}

// 测试用例 5: 简单的数字各位相加
test "simple_digit_sum" {
  // 简单的数字各位相加
  // 计算9875的各位数字之和：9 + 8 + 7 + 5 = 29
  
  // 使用简化的方法分解数字
  let thousands = 9000
  let hundreds = 800
  let tens = 70
  let ones = 5
  
  // 分解各位数字
  let digit1 = thousands / 1000  // 9
  let digit2 = hundreds / 100    // 8
  let digit3 = tens / 10         // 7
  let digit4 = ones              // 5
  
  // 求和
  let digit_sum = @azimuth.add(@azimuth.add(@azimuth.add(digit1, digit2), digit3), digit4)
  
  @azimuth.assert_eq(digit_sum, 29)
}

// 测试用例 6: 不同语言的问候语
test "greeting_with_different_languages" {
  // 测试不同语言的问候语
  
  // 英文
  let english_name = "Alice"
  let english_greeting = @azimuth.greet(english_name)
  @azimuth.assert_eq_string("Hello, Alice!", english_greeting)
  
  // 中文
  let chinese_name = "小明"
  let chinese_greeting = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 小明!", chinese_greeting)
  
  // 日文
  let japanese_name = "さくら"
  let japanese_greeting = @azimuth.greet(japanese_name)
  @azimuth.assert_eq_string("Hello, さくら!", japanese_greeting)
  
  // 韩文
  let korean_name = "김철수"
  let korean_greeting = @azimuth.greet(korean_name)
  @azimuth.assert_eq_string("Hello, 김철수!", korean_greeting)
}

// 测试用例 7: 简化的日期计算
test "simple_date_calculation" {
  // 简化的日期计算
  // 计算一年中的第几天（假设每月30天）
  
  let day = 15
  let month = 3
  
  // 计算天数：前两个月的天数 + 当前月的天数
  let previous_months_days = @azimuth.multiply(month - 1, 30)  // (3-1) × 30 = 60
  let day_of_year = @azimuth.add(previous_months_days, day)     // 60 + 15 = 75
  
  @azimuth.assert_eq(day_of_year, 75)
}

// 测试用例 8: 简化的速度单位转换
test "simple_speed_conversion" {
  // 简化的速度单位转换
  // km/h 转 m/s（使用近似值：除以3.6，近似为除以4）
  
  let speed_kmh = 80  // 80 km/h
  
  // 简化转换：80 / 4 = 20 m/s
  let speed_ms = speed_kmh / 4
  
  @azimuth.assert_eq(speed_ms, 20)
  
  // 反向验证：20 × 4 = 80 km/h
  let reverse_speed_kmh = @azimuth.multiply(speed_ms, 4)
  @azimuth.assert_eq(reverse_speed_kmh, 80)
}

// 测试用例 9: 边界值操作测试
test "boundary_value_operations" {
  // 边界值操作测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与1的加法（应该溢出）
  let max_add_one = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_add_one, max_val)
  
  // 测试最小值与-1的加法（应该溢出）
  let min_add_neg_one = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_add_neg_one, min_val)
  
  // 测试最大值与-1的乘法（应该溢出）
  let max_multiply_neg_one = @azimuth.multiply(max_val, -1)
  @azimuth.assert_eq(max_multiply_neg_one, min_val)
  
  // 测试最小值与1的乘法
  let min_multiply_one = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_multiply_one, min_val)
}

// 测试用例 10: 简单的利润计算
test "simple_profit_calculation" {
  // 简单的利润计算
  // 利润 = 收入 - 成本
  
  let unit_price = 50       // 单价
  let sales_volume = 1000   // 销量
  let total_cost = 45000    // 总成本
  
  // 计算总收入
  let total_revenue = @azimuth.multiply(unit_price, sales_volume)
  
  // 计算利润
  let profit = @azimuth.add(total_revenue, -total_cost)
  
  @azimuth.assert_eq(total_revenue, 50000)  // 50 × 1000 = 50000
  @azimuth.assert_eq(profit, 5000)          // 50000 - 45000 = 5000
}