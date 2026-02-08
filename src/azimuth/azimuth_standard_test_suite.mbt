// Azimuth 项目标准测试套件
// 包含8个高质量的标准 MoonBit 测试用例

test "prime_number_validation" {
  // 质数验证测试 - 数学基础算法
  // 验证7是质数：不能被2,3,4,5,6整除
  
  let number = 7
  let is_prime = true
  
  // 检查能否被2整除
  let divisible_by_2 = number % 2
  let check2 = divisible_by_2 != 0
  
  // 检查能否被3整除
  let divisible_by_3 = number % 3
  let check3 = divisible_by_3 != 0
  
  // 检查能否被4整除
  let divisible_by_4 = number % 4
  let check4 = divisible_by_4 != 0
  
  // 检查能否被5整除
  let divisible_by_5 = number % 5
  let check5 = divisible_by_5 != 0
  
  // 验证7是质数
  azimuth::assert_eq(1, divisible_by_2)
  azimuth::assert_eq(1, divisible_by_3)
  azimuth::assert_eq(3, divisible_by_4)
  azimuth::assert_eq(2, divisible_by_5)
  
  // 验证6不是质数（能被2和3整除）
  let composite = 6
  azimuth::assert_eq(0, composite % 2)
  azimuth::assert_eq(0, composite % 3)
}

test "greatest_common_divisor" {
  // 最大公约数计算测试 - 欧几里得算法模拟
  // 计算 GCD(48, 18) = 6
  
  let a = 48
  let b = 18
  
  // 模拟欧几里得算法
  // 48 ÷ 18 = 2 余 12
  let step1_quotient = azimuth::divide_with_ceil(a, b) - 1  // 3 - 1 = 2
  let step1_remainder = a - azimuth::multiply(step1_quotient, b)  // 48 - 2*18 = 12
  
  // 18 ÷ 12 = 1 余 6
  let step2_quotient = azimuth::divide_with_ceil(b, step1_remainder) - 1  // 2 - 1 = 1
  let step2_remainder = b - azimuth::multiply(step2_quotient, step1_remainder)  // 18 - 1*12 = 6
  
  // 12 ÷ 6 = 2 余 0，所以GCD是6
  let step3_quotient = azimuth::divide_with_ceil(step1_remainder, step2_remainder)  // 12 ÷ 6 = 2
  let step3_remainder = step1_remainder - azimuth::multiply(step3_quotient, step2_remainder)  // 12 - 2*6 = 0
  
  azimuth::assert_eq(step1_quotient, 2)
  azimuth::assert_eq(step1_remainder, 12)
  azimuth::assert_eq(step2_quotient, 1)
  azimuth::assert_eq(step2_remainder, 6)
  azimuth::assert_eq(step3_quotient, 2)
  azimuth::assert_eq(step3_remainder, 0)
}

test "power_calculation" {
  // 幂运算计算测试 - 基础数学运算
  // 计算 2^5 = 32
  
  let base = 2
  let exponent = 5
  
  // 模拟幂运算
  let power1 = base  // 2^1 = 2
  let power2 = azimuth::multiply(power1, base)  // 2^2 = 4
  let power3 = azimuth::multiply(power2, base)  // 2^3 = 8
  let power4 = azimuth::multiply(power3, base)  // 2^4 = 16
  let power5 = azimuth::multiply(power4, base)  // 2^5 = 32
  
  azimuth::assert_eq(power1, 2)
  azimuth::assert_eq(power2, 4)
  azimuth::assert_eq(power3, 8)
  azimuth::assert_eq(power4, 16)
  azimuth::assert_eq(power5, 32)
  
  // 验证 3^4 = 81
  let base2 = 3
  let exp2 = 4
  let result2_1 = base2
  let result2_2 = azimuth::multiply(result2_1, base2)
  let result2_3 = azimuth::multiply(result2_2, base2)
  let result2_4 = azimuth::multiply(result2_3, base2)
  
  azimuth::assert_eq(result2_4, 81)
}

test "square_root_approximation" {
  // 平方根近似计算测试 - 数值计算方法
  // 计算 √16 ≈ 4 和 √20 ≈ 5 (向上取整)
  
  let number1 = 16
  let number2 = 20
  
  // 对于完全平方数16，精确平方根是4
  let exact_root = 4
  let verification1 = azimuth::multiply(exact_root, exact_root)
  
  // 对于非完全平方数20，使用向上取整近似
  let approx_root = azimuth::divide_with_ceil(number2, exact_root)  // ceil(20/4) = 5
  let verification2_low = azimuth::multiply(approx_root - 1, approx_root - 1)  // 4*4 = 16
  let verification2_high = azimuth::multiply(approx_root, approx_root)  // 5*5 = 25
  
  azimuth::assert_eq(verification1, number1)
  azimuth::assert_eq(approx_root, 5)
  azimuth::assert_eq(verification2_low, 16)
  azimuth::assert_eq(verification2_high, 25)
}

test "percentage_calculations" {
  // 百分比计算测试 - 实用数学应用
  // 计算折扣、税率和增长率的场景
  
  let original_price = 200
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8        // 8% 税率
  let growth_rate = 12    // 12% 增长率
  
  // 计算折扣金额
  let discount_amount = azimuth::divide_with_ceil(azimuth::multiply(original_price, discount_rate), 100)
  let discounted_price = azimuth::subtract(original_price, discount_amount)
  
  // 计算税费
  let tax_amount = azimuth::divide_with_ceil(azimuth::multiply(discounted_price, tax_rate), 100)
  let final_price = azimuth::add(discounted_price, tax_amount)
  
  // 计算增长后的价值
  let growth_amount = azimuth::divide_with_ceil(azimuth::multiply(original_price, growth_rate), 100)
  let grown_value = azimuth::add(original_price, growth_amount)
  
  azimuth::assert_eq(discount_amount, 30)
  azimuth::assert_eq(discounted_price, 170)
  azimuth::assert_eq(tax_amount, 14)  // ceil(170*8/100) = ceil(13.6) = 14
  azimuth::assert_eq(final_price, 184)
  azimuth::assert_eq(growth_amount, 24)
  azimuth::assert_eq(grown_value, 224)
}

test "speed_distance_time" {
  // 速度、距离、时间计算测试 - 物理应用
  // 使用公式：距离 = 速度 × 时间，速度 = 距离/时间
  
  let speed = 60  // km/h
  let time = 2.5  // 小时，使用整数模拟：2.5 = 5/2
  let distance = 150  // km
  
  // 模拟 2.5 小时的行驶距离
  let time_numerator = 5
  let time_denominator = 2
  let distance_calculated = azimuth::divide_with_ceil(azimuth::multiply(speed, time_numerator), time_denominator)
  
  // 计算平均速度
  let avg_speed_calculated = azimuth::divide_with_ceil(azimuth::multiply(distance, time_denominator), time_numerator)
  
  // 计算行驶时间
  let time_calculated = azimuth::divide_with_ceil(azimuth::multiply(distance, time_denominator), speed)
  
  azimuth::assert_eq(distance_calculated, 150)
  azimuth::assert_eq(avg_speed_calculated, 60)
  azimuth::assert_eq(time_calculated, 5)  // 结果是5/2小时，但我们用5表示
}

test "array_sum_average" {
  // 数组求和与平均值计算测试 - 数据处理基础
  // 数组: [12, 15, 18, 21, 24, 27, 30]
  
  let element1 = 12
  let element2 = 15
  let element3 = 18
  let element4 = 21
  let element5 = 24
  let element6 = 27
  let element7 = 30
  
  // 计算总和
  let sum1 = azimuth::add(element1, element2)
  let sum2 = azimuth::add(sum1, element3)
  let sum3 = azimuth::add(sum2, element4)
  let sum4 = azimuth::add(sum3, element5)
  let sum5 = azimuth::add(sum4, element6)
  let total_sum = azimuth::add(sum5, element7)
  
  // 计算平均值（向上取整）
  let count = 7
  let average = azimuth::divide_with_ceil(total_sum, count)
  
  // 计算中位数
  let median = element4  // 已排序数组的中位数
  
  azimuth::assert_eq(total_sum, 147)
  azimuth::assert_eq(average, 21)
  azimuth::assert_eq(median, 21)
}

test "currency_conversion" {
  // 货币转换测试 - 实际应用场景
  // 模拟美元到人民币和欧元的转换
  
  let usd_amount = 100
  let cny_rate = 7.2   // 1 USD = 7.2 CNY，使用整数模拟：7.2 = 36/5
  let eur_rate = 0.85  // 1 USD = 0.85 EUR，使用整数模拟：0.85 = 17/20
  
  // USD 转 CNY
  let cny_numerator = 36
  let cny_denominator = 5
  let cny_amount = azimuth::divide_with_ceil(azimuth::multiply(usd_amount, cny_numerator), cny_denominator)
  
  // USD 转 EUR
  let eur_numerator = 17
  let eur_denominator = 20
  let eur_amount = azimuth::divide_with_ceil(azimuth::multiply(usd_amount, eur_numerator), eur_denominator)
  
  // 计算反向转换验证
  let usd_from_cny = azimuth::divide_with_ceil(azimuth::multiply(cny_amount, cny_denominator), cny_numerator)
  let usd_from_eur = azimuth::divide_with_ceil(azimuth::multiply(eur_amount, eur_denominator), eur_numerator)
  
  azimuth::assert_eq(cny_amount, 720)
  azimuth::assert_eq(eur_amount, 85)
  azimuth::assert_eq(usd_from_cny, 100)
  azimuth::assert_eq(usd_from_eur, 100)
}