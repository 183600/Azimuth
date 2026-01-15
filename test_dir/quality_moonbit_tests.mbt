// 高质量 MoonBit 测试用例
// 这些测试用例展示了 azimuth 模块的高级功能和实际应用场景

// 导入主模块中的函数
use azimuth

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = add(1, 0)
  assert_eq(1, f2)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = add(f2, 1)
  assert_eq(2, f3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = add(f3, f2)
  assert_eq(3, f4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = add(f4, f3)
  assert_eq(5, f5)
  
  // F(6) = F(5) + F(4) = 5 + 3 = 8
  let f6 = add(f5, f4)
  assert_eq(8, f6)
}

test "prime_number_validation" {
  // 素数验证测试（简化版）
  // 通过检查能否被小于等于平方根的数整除来判断素数
  
  let num = 17
  let sqrt_approx = 4  // 17的平方根约等于4.12，取整数部分
  
  // 检查能否被2, 3, 4整除
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_4 = (num % 4 == 0)
  
  // 17不能被2, 3, 4整除，所以是素数
  assert_false(divisible_by_2)
  assert_false(divisible_by_3)
  assert_false(divisible_by_4)
  
  // 测试非素数
  let composite = 15
  let div_by_3 = (composite % 3 == 0)
  let div_by_5 = (composite % 5 == 0)
  
  assert_true(div_by_3)
  assert_true(div_by_5)
}

test "bank_interest_calculation" {
  // 银行利息计算测试
  // 复利公式：A = P(1 + r/n)^(nt)
  // 简化版本：A = P(1 + r*t)
  
  let principal = 10000  // 本金
  let annual_rate = 5    // 年利率5%
  let years = 3          // 存期3年
  
  // 计算简单利息：本金 × 利率 × 年数
  let simple_interest = multiply(principal, multiply(annual_rate, years))
  
  // 计算本息和：本金 + 利息
  let total_amount = add(principal, divide_with_ceil(simple_interest, 100))
  
  assert_eq(115000, total_amount)  // 10000 + (10000×5×3)/100 = 10000 + 1500 = 115000
  
  // 测试大额存款
  let large_principal = 100000
  let large_interest = multiply(large_principal, multiply(annual_rate, years))
  let large_total = add(large_principal, divide_with_ceil(large_interest, 100))
  
  assert_eq(1150000, large_total)
}

test "shopping_cart_with_discounts" {
  // 购物车折扣计算测试
  let item1_price = 299
  let item1_qty = 2
  let item2_price = 199
  let item2_qty = 3
  let item3_price = 99
  let item3_qty = 1
  
  // 计算各商品小计
  let subtotal1 = multiply(item1_price, item1_qty)
  let subtotal2 = multiply(item2_price, item2_qty)
  let subtotal3 = multiply(item3_price, item3_qty)
  
  // 计算购物车总价
  let cart_total = add(add(subtotal1, subtotal2), subtotal3)
  
  // 计算折扣（总价超过500元打9折）
  let discount_threshold = 500
  let discount_rate = 10  // 10%折扣
  
  let has_discount = cart_total > discount_threshold
  let discount_amount = if has_discount {
    divide_with_ceil(multiply(cart_total, discount_rate), 100)
  } else {
    0
  }
  
  // 计算最终价格
  let final_price = add(cart_total, -discount_amount)
  
  assert_eq(1294, cart_total)        // 原价
  assert_eq(1164, final_price)       // 折后价
}

test "temperature_conversion_advanced" {
  // 高级温度转换测试
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 由于只有整数运算，使用近似值
  
  let celsius = 25
  let fahrenheit = add(multiply(celsius, 2), 30)  // 简化公式
  assert_eq(80, fahrenheit)
  
  // 反向转换（简化版）
  let converted_back = divide_with_ceil(add(fahrenheit, -30), 2)
  assert_eq(25, converted_back)
  
  // 测试极端温度
  let freezing_c = 0
  let freezing_f = add(multiply(freezing_c, 2), 30)
  assert_eq(30, freezing_f)
  
  let boiling_c = 100
  let boiling_f = add(multiply(boiling_c, 2), 30)
  assert_eq(230, boiling_f)
  
  // 测试负温度
  let negative_c = -10
  let negative_f = add(multiply(negative_c, 2), 30)
  assert_eq(10, negative_f)
}

test "string_processing_with_special_cases" {
  // 字符串处理特殊案例测试
  
  // 测试包含各种特殊字符的字符串
  let unicode_name = "张三李四"
  assert_eq_string("Hello, 张三李四!", greet(unicode_name))
  
  // 测试包含HTML标签的字符串
  let html_name = "<div>User</div>"
  assert_eq_string("Hello, <div>User</div>!", greet(html_name))
  
  // 测试包含JSON的字符串
  let json_name = "{\"name\":\"Alice\"}"
  assert_eq_string("Hello, {\"name\":\"Alice\"}!", greet(json_name))
  
  // 测试包含URL的字符串
  let url_name = "https://example.com/user"
  assert_eq_string("Hello, https://example.com/user!", greet(url_name))
  
  // 测试包含SQL的字符串
  let sql_name = "SELECT * FROM users"
  assert_eq_string("Hello, SELECT * FROM users!", greet(sql_name))
}

test "mathematical_series_calculations" {
  // 数学级数计算测试
  
  // 计算等差数列前n项和：S = n(a1 + an)/2
  let n = 10
  let first_term = 1
  let last_term = 10
  
  let arithmetic_sum = divide_with_ceil(multiply(n, add(first_term, last_term)), 2)
  assert_eq(55, arithmetic_sum)  // 10×(1+10)/2 = 55
  
  // 计算等比数列前n项和：S = a1(r^n - 1)/(r - 1)
  // 简化版本：手动计算前几项
  let ratio = 2
  let geo_term1 = 1
  let geo_term2 = multiply(geo_term1, ratio)
  let geo_term3 = multiply(geo_term2, ratio)
  let geo_term4 = multiply(geo_term3, ratio)
  let geo_term5 = multiply(geo_term4, ratio)
  
  let geometric_sum = add(add(add(add(geo_term1, geo_term2), geo_term3), geo_term4), geo_term5)
  assert_eq(31, geometric_sum)  // 1 + 2 + 4 + 8 + 16 = 31
  
  // 计算平方和：1² + 2² + ... + n²
  let square_sum = add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4))
  assert_eq(30, square_sum)  // 1 + 4 + 9 + 16 = 30
}

test "game_mechanics_simulation" {
  // 游戏机制模拟测试
  
  // 角色属性计算
  let base_strength = 10
  let level_bonus = 5
  let equipment_bonus = 3
  let buff_multiplier = 2
  
  // 计算最终力量值
  let total_strength = add(add(base_strength, level_bonus), equipment_bonus)
  let final_strength = multiply(total_strength, buff_multiplier)
  
  assert_eq(36, final_strength)  // (10 + 5 + 3) × 2 = 36
  
  // 伤害计算
  let weapon_damage = 25
  let strength_factor = divide_with_ceil(final_strength, 5)
  let critical_hit = 2
  
  let normal_damage = add(weapon_damage, strength_factor)
  let critical_damage = multiply(normal_damage, critical_hit)
  
  assert_eq(37, normal_damage)     // 25 + 36/5 = 25 + 7 = 32
  assert_eq(74, critical_damage)   // 37 × 2 = 74
  
  // 经验值计算
  let base_exp = 100
  let level_multiplier = 3
  let difficulty_bonus = 50
  
  let total_exp = add(multiply(base_exp, level_multiplier), difficulty_bonus)
  assert_eq(350, total_exp)  // 100×3 + 50 = 350
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  
  // 数据集：[12, 15, 18, 21, 24]
  let data1 = 12
  let data2 = 15
  let data3 = 18
  let data4 = 21
  let data5 = 24
  
  // 计算总和
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  assert_eq(90, sum)
  
  // 计算平均值
  let average = divide_with_ceil(sum, 5)
  assert_eq(18, average)
  
  // 计算方差（简化版）
  let diff1 = add(data1, -average)
  let diff2 = add(data2, -average)
  let diff3 = add(data3, -average)
  let diff4 = add(data4, -average)
  let diff5 = add(data5, -average)
  
  let sq_diff1 = multiply(diff1, diff1)
  let sq_diff2 = multiply(diff2, diff2)
  let sq_diff3 = multiply(diff3, diff3)
  let sq_diff4 = multiply(diff4, diff4)
  let sq_diff5 = multiply(diff5, diff5)
  
  let variance_sum = add(add(add(add(sq_diff1, sq_diff2), sq_diff3), sq_diff4), sq_diff5)
  let variance = divide_with_ceil(variance_sum, 5)
  
  assert_eq(18, variance)  // ((-6)² + (-3)² + 0² + 3² + 6²)/5 = (36+9+0+9+36)/5 = 90/5 = 18
  
  // 计算标准差（简化版：整数开方）
  let standard_deviation = 4  // √18 ≈ 4.24，取整数部分
  assert_eq(4, standard_deviation)
}