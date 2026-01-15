// 高级 MoonBit 测试用例
// 测试一些更复杂的场景和边界情况

// 测试大数运算的稳定性
test "large_number_stability" {
  // 测试接近边界的 large number 运算
  let large_num = 1000000000
  let result = add(large_num, large_num)
  assert_eq(2000000000, result)
  
  // 测试大数乘法
  let large_mul = multiply(large_num, 2)
  assert_eq(2000000000, large_mul)
}

// 测试负数运算的复杂性
test "complex_negative_operations" {
  // 测试多个负数的连续运算
  let neg1 = -100
  let neg2 = -200
  let pos1 = 300
  
  // (-100) + (-200) + 300 = 0
  let result = add(add(neg1, neg2), pos1)
  assert_eq(0, result)
  
  // 测试负数乘法的交互
  let neg_mul = multiply(neg1, neg2)
  assert_eq(20000, neg_mul)
  
  // 测试正负数混合乘法
  let mixed_mul = multiply(neg1, pos1)
  assert_eq(-30000, mixed_mul)
}

// 测试字符串处理的边界情况
test "string_boundary_processing" {
  // 测试包含各种特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let result = greet(special_chars)
  assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", result)
  
  // 测试包含数字和字母混合的字符串
  let alphanumeric = "ABC123xyz456"
  let result2 = greet(alphanumeric)
  assert_eq_string("Hello, ABC123xyz456!", result2)
  
  // 测试包含空格的复杂字符串
  let complex_spaces = "  Multiple   Spaces  Between  Words  "
  let result3 = greet(complex_spaces)
  assert_eq_string("Hello,   Multiple   Spaces  Between  Words  !", result3)
}

// 测试数学函数的组合使用
test "math_function_combinations" {
  // 测试复杂数学表达式：(a + b) * (c - d) + e * f
  let a = 10
  let b = 20
  let c = 50
  let d = 30
  let e = 5
  let f = 8
  
  let sum_ab = add(a, b)  // 30
  let diff_cd = add(c, -d)  // 20
  let product_ab_cd = multiply(sum_ab, diff_cd)  // 600
  let product_ef = multiply(e, f)  // 40
  let final_result = add(product_ab_cd, product_ef)  // 640
  
  assert_eq(640, final_result)
}

// 测试实际应用场景：银行利息计算
test "bank_interest_calculation" {
  // 银行利息计算：本金 × (1 + 月利率) ^ 月数
  // 简化为整数计算
  let principal = 10000  // 本金
  let monthly_rate = 5   // 月利率（千分之五）
  let months = 12        // 月数
  
  // 简化计算：10000 + 10000 × 5 × 12 / 1000
  let interest = multiply(principal, multiply(monthly_rate, months)) / 1000
  let total = add(principal, interest)
  
  assert_eq(10600, total)
}

// 测试物理计算场景
test "physics_calculation" {
  // 自由落体距离计算：s = 0.5 × g × t²
  // 简化为整数计算：s = 5 × t²（g取10m/s²）
  let gravity = 10
  let time = 5
  
  let time_squared = multiply(time, time)  // 25
  let distance = multiply(5, time_squared)  // 125
  
  assert_eq(125, distance)
  
  // 动能计算：E = 0.5 × m × v²
  // 简化为整数计算：E = m × v² / 2
  let mass = 1000
  let velocity = 20
  
  let velocity_squared = multiply(velocity, velocity)  // 400
  let kinetic_energy = multiply(mass, velocity_squared) / 2  // 200000
  
  assert_eq(200000, kinetic_energy)
}

// 测试数据验证场景
test "data_validation_scenarios" {
  // 验证身份证号码长度（简化）
  let id_length = 18
  let actual_id_length = 18
  
  let length_match = if (id_length == actual_id_length) { 1 } else { 0 }
  assert_eq(1, length_match)
  
  // 验证手机号码格式（简化）
  let phone_prefix = 138
  let valid_prefix = 138
  
  let prefix_match = if (phone_prefix == valid_prefix) { 1 } else { 0 }
  assert_eq(1, prefix_match)
  
  // 验证密码强度（简化：长度检查）
  let min_password_length = 8
  let actual_password_length = 12
  
  let strength_ok = if (actual_password_length >= min_password_length) { 1 } else { 0 }
  assert_eq(1, strength_ok)
}

// 测试购物折扣计算
test "shopping_discount_calculation" {
  // 购物折扣计算：满100减20，满200减50，满500减150
  let purchase_amount = 350
  
  let discount = if (purchase_amount >= 500) {
    150
  } else if (purchase_amount >= 200) {
    50
  } else if (purchase_amount >= 100) {
    20
  } else {
    0
  }
  
  let final_amount = add(purchase_amount, -discount)
  assert_eq(300, final_amount)
  
  // 测试不同金额段的折扣
  let small_purchase = 80
  let small_discount = if (small_purchase >= 100) { 20 } else { 0 }
  let small_final = add(small_purchase, -small_discount)
  assert_eq(80, small_final)
  
  let large_purchase = 600
  let large_discount = if (large_purchase >= 500) { 150 } else { 50 }
  let large_final = add(large_purchase, -large_discount)
  assert_eq(450, large_final)
}

// 测试温度转换的精确性
test "temperature_conversion_precision" {
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化为整数计算：F = C × 2 + 30（近似值）
  
  let celsius_temps = [0, 10, 20, 30, 40, 100]
  let expected_fahrenheit = [30, 50, 70, 90, 110, 230]
  
  // 测试0°C
  let temp0 = add(multiply(celsius_temps[0], 2), 30)
  assert_eq(expected_fahrenheit[0], temp0)
  
  // 测试20°C
  let temp20 = add(multiply(celsius_temps[2], 2), 30)
  assert_eq(expected_fahrenheit[2], temp20)
  
  // 测试100°C
  let temp100 = add(multiply(celsius_temps[5], 2), 30)
  assert_eq(expected_fahrenheit[5], temp100)
  
  // 华氏度转摄氏度：C = (F - 32) × 5/9
  // 简化为整数计算：C = (F - 30) / 2（近似值）
  let fahrenheit = 70
  let celsius = add(fahrenheit, -30) / 2
  assert_eq(20, celsius)
}

// 测试数列求和
test "series_summation" {
  // 等差数列求和：1 + 2 + 3 + ... + n = n × (n + 1) / 2
  let n = 10
  let sum_n = multiply(n, add(n, 1)) / 2
  assert_eq(55, sum_n)
  
  // 等比数列求和：2 + 4 + 8 + 16 + 32 = 64 - 2 = 62
  let geo_sum = add(add(add(add(2, 4), 8), 16), 32)
  assert_eq(62, geo_sum)
  
  // 平方和：1² + 2² + 3² + ... + 5² = 55
  let square_sum = add(add(add(add(multiply(1, 1), multiply(2, 2)), multiply(3, 3)), multiply(4, 4)), multiply(5, 5))
  assert_eq(55, square_sum)
}