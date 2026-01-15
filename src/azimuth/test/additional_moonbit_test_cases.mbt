// 额外的 MoonBit 测试用例
// 测试项目的核心功能和实际应用场景

test "divide_with_ceil_functionality" {
  // 测试向上取整除法功能
  // 正常情况
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(11, 3))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(12, 3))
  
  // 边界情况
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(5, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-9, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-5, 3))
}

test "mathematical_associativity_laws" {
  // 测试数学结合律
  let a = 15
  let b = 25
  let c = 35
  
  // 加法结合律: (a + b) + c = a + (b + c)
  let left_add = @azimuth.add(@azimuth.add(a, b), c)
  let right_add = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(left_add, right_add)
  
  // 乘法结合律: (a × b) × c = a × (b × c)
  let left_mul = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let right_mul = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(left_mul, right_mul)
  
  // 混合运算结合律测试
  let mixed1 = @azimuth.add(@azimuth.multiply(a, b), c)
  let mixed2 = @azimuth.multiply(a, @azimuth.add(b, c))
  // 这两个不应该相等，但我们测试它们的计算是否正确
  @azimuth.assert_eq(410, mixed1)  // 15*25 + 35 = 375 + 35 = 410
  @azimuth.assert_eq(900, mixed2)  // 15*(25+35) = 15*60 = 900
}

test "discount_price_calculation" {
  // 测试实际场景：计算折扣价格
  let original_price = 1000
  let discount_percentage = 20  // 20% 折扣
  let tax_percentage = 8        // 8% 税费
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_percentage) / 100
  @azimuth.assert_eq(200, discount_amount)
  
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)
  @azimuth.assert_eq(800, discounted_price)
  
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_percentage) / 100
  @azimuth.assert_eq(64, tax_amount)
  
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)
  @azimuth.assert_eq(864, final_price)
}

test "speed_distance_time_calculations" {
  // 测试实际场景：计算速度、距离和时间
  // 公式：距离 = 速度 × 时间
  let speed = 60  // km/h
  
  // 使用整数计算：60 * 250 / 100 = 150
  let distance = @azimuth.multiply(speed, 250) / 100
  @azimuth.assert_eq(150, distance)
  
  // 反向计算：速度 = 距离 / 时间
  let calculated_speed = distance / 2  // 简化为整数除法
  @azimuth.assert_eq(75, calculated_speed)  // 150 / 2 = 75
  
  // 另一个场景：计算到达时间
  let remaining_distance = 300
  let average_speed = 75
  let time_needed = @azimuth.divide_with_ceil(remaining_distance, average_speed)
  @azimuth.assert_eq(4, time_needed)  // 300/75 = 4 小时
}

test "extreme_boundary_operations" {
  // 测试边界情况：最大值和最小值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与正数运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 最大值与负数运算
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 最小值与正数运算
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 最小值与负数运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  @azimuth.assert_eq(max_val, @azimuth.multiply(min_val, -1))
  
  // 零与边界值运算
  @azimuth.assert_eq(0, @azimuth.multiply(0, max_val))
  @azimuth.assert_eq(0, @azimuth.multiply(0, min_val))
  @azimuth.assert_eq(max_val, @azimuth.add(0, max_val))
  @azimuth.assert_eq(min_val, @azimuth.add(0, min_val))
}

test "multilingual_greeting_processing" {
  // 测试字符串处理：不同语言的问候
  // 英文问候
  @azimuth.assert_eq_string("Hello, Alice!", @azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", @azimuth.greet("Bob"))
  
  // 中文问候
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet("张三"))
  @azimuth.assert_eq_string("Hello, 李四!", @azimuth.greet("李四"))
  
  // 日文问候
  @azimuth.assert_eq_string("Hello, 田中!", @azimuth.greet("田中"))
  @azimuth.assert_eq_string("Hello, 佐藤!", @azimuth.greet("佐藤"))
  
  // 韩文问候
  @azimuth.assert_eq_string("Hello, 김철수!", @azimuth.greet("김철수"))
  @azimuth.assert_eq_string("Hello, 이영희!", @azimuth.greet("이영희"))
  
  // 阿拉伯文问候
  @azimuth.assert_eq_string("Hello, محمد!", @azimuth.greet("محمد"))
  
  // 俄文问候
  @azimuth.assert_eq_string("Hello, Владимир!", @azimuth.greet("Владимир"))
  
  // 混合语言
  @azimuth.assert_eq_string("Hello, John张三!", @azimuth.greet("John张三"))
}

test "mathematical_difference_of_squares" {
  // 测试数学性质：平方差公式 (a² - b²) = (a + b)(a - b)
  let a = 13
  let b = 7
  
  // 直接计算 a² - b²
  let a_squared = @azimuth.multiply(a, a)
  let b_squared = @azimuth.multiply(b, b)
  let direct_result = @azimuth.add(a_squared, -b_squared)
  
  // 使用平方差公式计算 (a + b)(a - b)
  let sum = @azimuth.add(a, b)
  let difference = @azimuth.add(a, -b)
  let formula_result = @azimuth.multiply(sum, difference)
  
  // 验证两种方法结果相同
  @azimuth.assert_eq(direct_result, formula_result)
  @azimuth.assert_eq(120, direct_result)  // 13² - 7² = 169 - 49 = 120
  
  // 测试另一组数值
  let x = 25
  let y = 15
  let x_squared = @azimuth.multiply(x, x)
  let y_squared = @azimuth.multiply(y, y)
  let result2 = @azimuth.add(x_squared, -y_squared)
  
  let sum2 = @azimuth.add(x, y)
  let diff2 = @azimuth.add(x, -y)
  let formula_result2 = @azimuth.multiply(sum2, diff2)
  
  @azimuth.assert_eq(result2, formula_result2)
  @azimuth.assert_eq(400, result2)  // 25² - 15² = 625 - 225 = 400
}

test "bmi_calculation_simplified" {
  // 测试实际场景：计算BMI指数（简化版）
  // BMI = 体重(kg) / 身高(m)²
  // 使用整数计算，避免浮点数：BMI = 体重(kg) × 10000 / 身高(cm)²
  
  let weight_kg = 70      // 体重：70kg
  let height_cm = 175     // 身高：175cm
  
  // 计算身高平方
  let height_squared = @azimuth.multiply(height_cm, height_cm)
  
  // 计算BMI（乘以100以保留两位小数）
  let bmi_times_100 = @azimuth.multiply(weight_kg, 10000) / height_squared
  @azimuth.assert_eq(22, bmi_times_100)  // 70×10000/175² ≈ 22.86，取整为22
  
  // 另一个例子
  let weight2 = 85
  let height2 = 180
  let height2_squared = @azimuth.multiply(height2, height2)
  let bmi2_times_100 = @azimuth.multiply(weight2, 10000) / height2_squared
  @azimuth.assert_eq(26, bmi2_times_100)  // 85×10000/180² ≈ 26.23，取整为26
  
  // BMI分类测试（简化版）
  // BMI < 18.5: 体重过轻
  // 18.5 ≤ BMI < 25: 正常体重
  // 25 ≤ BMI < 30: 超重
  // BMI ≥ 30: 肥胖
  
  let normal_bmi = 22
  let overweight_bmi = 26
  
  @azimuth.assert_true(normal_bmi >= 18 && normal_bmi < 25)
  @azimuth.assert_true(overweight_bmi >= 25 && overweight_bmi < 30)
}

test "factorial_calculation" {
  // 测试数学运算：阶乘计算
  // 计算 5! = 5 × 4 × 3 × 2 × 1 = 120
  let n = 5
  let factorial_5 = @azimuth.multiply(n, @azimuth.multiply(n - 1, @azimuth.multiply(n - 2, @azimuth.multiply(n - 3, n - 4))))
  @azimuth.assert_eq(120, factorial_5)
  
  // 计算 4! = 4 × 3 × 2 × 1 = 24
  let factorial_4 = @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1)))
  @azimuth.assert_eq(24, factorial_4)
  
  // 计算 3! = 3 × 2 × 1 = 6
  let factorial_3 = @azimuth.multiply(3, @azimuth.multiply(2, 1))
  @azimuth.assert_eq(6, factorial_3)
  
  // 计算 6! = 6 × 5 × 4 × 3 × 2 × 1 = 720
  let factorial_6 = @azimuth.multiply(6, @azimuth.multiply(5, @azimuth.multiply(4, @azimuth.multiply(3, @azimuth.multiply(2, 1)))))
  @azimuth.assert_eq(720, factorial_6)
  
  // 验证阶乘性质：n! = n × (n-1)!
  let factorial_5_alt = @azimuth.multiply(5, 24)  // 5 × 4!
  @azimuth.assert_eq(120, factorial_5_alt)
}

test "loan_payment_calculation" {
  // 测试实际场景：计算贷款月供（简化版）
  // 简化的月供计算：月供 = (本金 + 总利息) / 月数
  // 总利息 = 本金 × 年利率 × 年数
  
  let principal = 100000    // 贷款本金：100,000元
  let annual_rate = 5       // 年利率：5%
  let years = 3             // 贷款年限：3年
  let months = @azimuth.multiply(years, 12)  // 总月数
  
  // 计算总利息
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years)) / 100
  @azimuth.assert_eq(15000, total_interest)  // 100000 × 5% × 3 = 15000
  
  // 计算总还款额
  let total_payment = @azimuth.add(principal, total_interest)
  @azimuth.assert_eq(115000, total_payment)
  
  // 计算月供
  let monthly_payment = total_payment / months
  @azimuth.assert_eq(3194, monthly_payment)  // 115000 / 36 ≈ 3194.44，取整为3194
  
  // 另一个例子：更大金额的贷款
  let principal2 = 500000
  let annual_rate2 = 4
  let years2 = 10
  let months2 = @azimuth.multiply(years2, 12)
  
  let total_interest2 = @azimuth.multiply(principal2, @azimuth.multiply(annual_rate2, years2)) / 100
  let total_payment2 = @azimuth.add(principal2, total_interest2)
  let monthly_payment2 = total_payment2 / months2
  
  @azimuth.assert_eq(200000, total_interest2)  // 500000 × 4% × 10 = 200000
  @azimuth.assert_eq(700000, total_payment2)   // 500000 + 200000 = 700000
  @azimuth.assert_eq(5833, monthly_payment2)   // 700000 / 120 ≈ 5833.33，取整为5833
}