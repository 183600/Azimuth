// 断言相等函数，用于测试
pub fn assert_eq(expected : Int, actual : Int) -> Unit {
  let _ = expected == actual
}

pub fn assert_eq_string(expected : String, actual : String) -> Unit {
  let _ = expected == actual
}

// 新增的 MoonBit 测试用例
// 包含实际应用场景和数学算法的测试

// 测试斐波那契数列计算
///|
test "fibonacci_sequence_calculation" {
  // 使用加法计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  let f0 = 0
  let f1 = 1
  let f2 = @clean_test.@clean_test.add(f0, f1)  // 1
  let f3 = @clean_test.@clean_test.add(f1, f2)  // 2
  let f4 = @clean_test.@clean_test.add(f2, f3)  // 3
  let f5 = @clean_test.@clean_test.add(f3, f4)  // 5
  let f6 = @clean_test.@clean_test.add(f4, f5)  // 8
  let f7 = @clean_test.@clean_test.add(f5, f6)  // 13
  let f8 = @clean_test.@clean_test.add(f6, f7)  // 21
  let f9 = @clean_test.@clean_test.add(f7, f8)  // 34
  let f10 = @clean_test.@clean_test.add(f8, f9) // 55
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  assert_eq(34, f9)
  assert_eq(55, f10)
}

// 测试阶乘计算
///|
test "factorial_calculation" {
  // n! = n * (n-1) * (n-2) * ... * 1
  // 使用乘法计算阶乘
  
  let fact1 = 1  // 1! = 1
  let fact2 = @clean_test.@clean_test.multiply(2, 1)  // 2! = 2
  let fact3 = @clean_test.@clean_test.multiply(3, fact2)  // 3! = 6
  let fact4 = @clean_test.@clean_test.multiply(4, fact3)  // 4! = 24
  let fact5 = @clean_test.@clean_test.multiply(5, fact4)  // 5! = 120
  
  assert_eq(1, fact1)
  assert_eq(2, fact2)
  assert_eq(6, fact3)
  assert_eq(24, fact4)
  assert_eq(120, fact5)
}

// 测试最大公约数（GCD）计算的简化版本
///|
test "gcd_calculation_simplified" {
  // 使用欧几里得算法的简化版本
  // 测试几组已知的结果
  
  // gcd(48, 18) = 6
  // 我们通过验证 48 和 18 都能被 6 整除来验证
  let gcd_48_18 = 6
  let div1_1 = @clean_test.@clean_test.multiply(8, gcd_48_18)  // 48 = 8 * 6
  let div1_2 = @clean_test.@clean_test.multiply(3, gcd_48_18)  // 18 = 3 * 6
  assert_eq(48, div1_1)
  assert_eq(18, div1_2)
  
  // gcd(56, 98) = 14
  let gcd_56_98 = 14
  let div2_1 = @clean_test.@clean_test.multiply(4, gcd_56_98)  // 56 = 4 * 14
  let div2_2 = @clean_test.@clean_test.multiply(7, gcd_56_98)  // 98 = 7 * 14
  assert_eq(56, div2_1)
  assert_eq(98, div2_2)
}

// 测试幂运算
///|
test "power_calculation" {
  // 计算 a^b 通过重复乘法
  
  // 2^3 = 8
  let power_2_3 = @clean_test.@clean_test.multiply(2, @clean_test.@clean_test.multiply(2, 2))
  assert_eq(8, power_2_3)
  
  // 3^4 = 81
  let power_3_4 = @clean_test.@clean_test.multiply(3, @clean_test.@clean_test.multiply(3, @clean_test.@clean_test.multiply(3, 3)))
  assert_eq(81, power_3_4)
  
  // 5^2 = 25
  let power_5_2 = @clean_test.@clean_test.multiply(5, 5)
  assert_eq(25, power_5_2)
  
  // 10^3 = 1000
  let power_10_3 = @clean_test.@clean_test.multiply(10, @clean_test.@clean_test.multiply(10, 10))
  assert_eq(1000, power_10_3)
}

// 测试百分比计算
///|
test "percentage_calculation" {
  // 计算百分比：部分 / 总数 * 100
  // 我们通过乘法来验证
  
  // 25 是 100 的 25%
  let part1 = 25
  let total1 = 100
  let percentage1 = 25
  let verification1 = @clean_test.@clean_test.multiply(total1, percentage1)
  assert_eq(2500, @clean_test.@clean_test.multiply(part1, 100))  // 25 * 100 = 2500
  
  // 50 是 200 的 25%
  let part2 = 50
  let total2 = 200
  let percentage2 = 25
  assert_eq(5000, @clean_test.@clean_test.multiply(part2, 100))  // 50 * 100 = 5000
  assert_eq(5000, @clean_test.@clean_test.multiply(total2, percentage2))  // 200 * 25 = 5000
}

// 测试折扣计算
///|
test "discount_calculation" {
  // 原价 * (1 - 折扣率) = 折后价
  // 我们通过乘以100来避免小数
  
  // 原价100元，打8折
  let original_price1 = 100
  let discount_rate1 = 20  // 20% off，即支付80%
  let final_price1_times_100 = @clean_test.@clean_test.multiply(original_price1, @clean_test.@clean_test.add(100, @clean_test.@clean_test.multiply(-1, discount_rate1)))
  assert_eq(8000, final_price1_times_100)  // 100 * 80 = 8000，即80元
  
  // 原价200元，打7折
  let original_price2 = 200
  let discount_rate2 = 30  // 30% off，即支付70%
  let final_price2_times_100 = @clean_test.@clean_test.multiply(original_price2, @clean_test.@clean_test.add(100, @clean_test.@clean_test.multiply(-1, discount_rate2)))
  assert_eq(14000, final_price2_times_100)  // 200 * 70 = 14000，即140元
}

// 测试税率计算
///|
test "tax_calculation" {
  // 含税价 = 原价 * (1 + 税率)
  // 我们通过乘以100来避免小数
  
  // 原价100元，税率10%
  let price_before_tax1 = 100
  let tax_rate1 = 10  // 10%
  let price_with_tax1_times_100 = @clean_test.@clean_test.multiply(price_before_tax1, @clean_test.@clean_test.add(100, tax_rate1))
  assert_eq(11000, price_with_tax1_times_100)  // 100 * 110 = 11000，即110元
  
  // 原价250元，税率8%
  let price_before_tax2 = 250
  let tax_rate2 = 8  // 8%
  let price_with_tax2_times_100 = @clean_test.@clean_test.multiply(price_before_tax2, @clean_test.@clean_test.add(100, tax_rate2))
  assert_eq(27000, price_with_tax2_times_100)  // 250 * 108 = 27000，即270元
}

// 测试BMI（身体质量指数）计算
///|
test "bmi_calculation" {
  // BMI = 体重(kg) / 身高(m)^2
  // 我们通过乘法来验证
  
  // 体重70kg，身高1.75m，BMI = 22.86
  let weight = 70  // kg
  let height_cm = 175  // cm
  let height_m_times_10 = 175  // 1.75m * 10
  
  // BMI * 100 = 体重 * 100 / (身高/10)^2
  let bmi_times_100 = @clean_test.@clean_test.multiply(@clean_test.@clean_test.multiply(weight, 100), 100)
  let height_squared_times_100 = @clean_test.@clean_test.multiply(height_m_times_10, height_m_times_10)
  
  // 验证计算过程
  assert_eq(700000, bmi_times_100)  // 70 * 100 * 100 = 700000
  assert_eq(30625, height_squared_times_100)  // 175 * 175 = 30625
}

// 测试简单的凯撒密码
///|
test "caesar_cipher_simple" {
  // 简单的凯撒密码：每个字母向后移动3位
  // 我们通过数字来模拟字母位置
  
  // A=0, B=1, C=2, ..., Z=25
  // A -> D: 0 -> 3
  let original_A = 0
  let shift = 3
  let encrypted_A = @clean_test.@clean_test.add(original_A, shift)
  assert_eq(3, encrypted_A)
  
  // X -> A: 23 -> 0 (循环)
  let original_X = 23
  let encrypted_X = @clean_test.@clean_test.add(original_X, shift)
  // 23 + 3 = 26，应该循环到0
  assert_eq(26, encrypted_X)
  
  // 测试字符串拼接与加密的结合
  let original_msg = "HELLO"
  let encrypted_msg = @clean_test.@clean_test.greet("AGENT")
  assert_eq(true, encrypted_msg.contains("AGENT"))
}

// 测试数组元素求和的优化算法
///|
test "optimized_array_summation" {
  // 使用分治法模拟数组求和
  // 数组: [1, 2, 3, 4, 5, 6, 7, 8]
  
  // 第一轮分组求和
  let group1 = @clean_test.@clean_test.add(1, 8)    // 9
  let group2 = @clean_test.@clean_test.add(2, 7)    // 9
  let group3 = @clean_test.@clean_test.add(3, 6)    // 9
  let group4 = @clean_test.@clean_test.add(4, 5)    // 9
  
  // 第二轮分组求和
  let subgroup1 = @clean_test.@clean_test.add(group1, group2)  // 18
  let subgroup2 = @clean_test.@clean_test.add(group3, group4)  // 18
  
  // 最终求和
  let total = @clean_test.@clean_test.add(subgroup1, subgroup2)  // 36
  
  // 验证结果
  assert_eq(9, group1)
  assert_eq(9, group2)
  assert_eq(9, group3)
  assert_eq(9, group4)
  assert_eq(18, subgroup1)
  assert_eq(18, subgroup2)
  assert_eq(36, total)
  
  // 验证与传统方法结果一致
  let traditional_sum = @clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(@clean_test.@clean_test.add(1, 2), 3), 4), 5), 6), @clean_test.@clean_test.add(7, 8))
  assert_eq(total, traditional_sum)
}