// 额外的标准 MoonBit 测试用例
// 覆盖核心功能的各种使用场景

test "divide_with_ceil_basic" {
  // 测试向上取整除法的基本功能
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(2, 5))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(0, divide_with_ceil(-2, 5))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(2147483647, divide_with_ceil(2147483647, 1))
  assert_eq(-2147483648, divide_with_ceil(-2147483648, 1))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算场景
  // 计算贷款月供：(本金 × 月利率 × (1 + 月利率)^月数) / ((1 + 月利率)^月数 - 1)
  // 简化版本：使用整数计算
  let principal = 100000  // 本金
  let monthly_rate = 5    // 月利率（0.5%，简化为5）
  let months = 12         // 月数
  
  // 简化的月供计算
  let monthly_payment = divide_with_ceil(multiply(principal, monthly_rate), 100)
  let total_payment = multiply(monthly_payment, months)
  
  assert_eq(60000, total_payment)
}

test "statistical_analysis" {
  // 测试统计分析功能
  // 计算一组数据的方差（简化版本）
  let data_point_1 = 10
  let data_point_2 = 15
  let data_point_3 = 20
  let data_point_4 = 25
  let data_point_5 = 30
  
  // 计算平均值
  let sum = add(add(add(add(data_point_1, data_point_2), data_point_3), data_point_4), data_point_5)
  let mean = sum / 5
  
  // 计算方差（简化版本）
  let diff_1 = add(data_point_1, -mean)
  let diff_2 = add(data_point_2, -mean)
  let diff_3 = add(data_point_3, -mean)
  let diff_4 = add(data_point_4, -mean)
  let diff_5 = add(data_point_5, -mean)
  
  let variance = add(add(add(add(multiply(diff_1, diff_1), multiply(diff_2, diff_2)), multiply(diff_3, diff_3)), multiply(diff_4, diff_4))
  let final_variance = divide_with_ceil(variance, 5)
  
  assert_eq(40, final_variance)
}

test "physics_simulation" {
  // 测试物理模拟计算
  // 计算自由落体距离：s = 0.5 × g × t²
  // 简化版本：s = 5 × t²（其中 g ≈ 10 m/s²）
  let gravity = 5  // 简化的重力常数
  let time = 3     // 时间（秒）
  
  let distance = multiply(gravity, multiply(time, time))
  assert_eq(45, distance)
  
  // 计算速度：v = g × t
  let velocity = multiply(multiply(gravity, 2), time)
  assert_eq(30, velocity)
}

test "array_manipulation" {
  // 测试数组操作（通过数学运算模拟）
  // 计算等差数列的第n项：a_n = a_1 + (n-1) × d
  let first_term = 3
  let common_diff = 4
  let n = 7
  
  let nth_term = add(first_term, multiply(n - 1, common_diff))
  assert_eq(27, nth_term)
  
  // 计算等比数列的第n项：a_n = a_1 × r^(n-1)
  let ratio = 2
  let geometric_nth = multiply(first_term, multiply(multiply(ratio, ratio), multiply(ratio, ratio)))
  assert_eq(48, geometric_nth)
}

test "string_validation" {
  // 测试字符串验证功能
  // 验证用户名格式（通过greet函数间接测试）
  let valid_username = "User123"
  let invalid_empty = ""
  let valid_with_symbols = "user_test"
  let valid_with_unicode = "用户123"
  
  // 所有这些都应该能正常处理
  assert_eq_string("Hello, User123!", greet(valid_username))
  assert_eq_string("Hello, !", greet(invalid_empty))
  assert_eq_string("Hello, user_test!", greet(valid_with_symbols))
  assert_eq_string("Hello, 用户123!", greet(valid_with_unicode))
}

test "temperature_conversion_advanced" {
  // 测试高级温度转换
  // 摄氏度转华氏度：F = C × 9/5 + 32
  // 简化版本：F = C × 2 + 30（整数近似）
  let celsius_values = [0, 10, 20, 30, 40]
  
  // 测试冰点
  let freezing_f = add(multiply(0, 2), 30)
  assert_eq(30, freezing_f)
  
  // 测试室温
  let room_f = add(multiply(20, 2), 30)
  assert_eq(70, room_f)
  
  // 测试沸点
  let boiling_f = add(multiply(100, 2), 30)
  assert_eq(230, boiling_f)
  
  // 测试负温度
  let negative_f = add(multiply(-10, 2), 30)
  assert_eq(10, negative_f)
}

test "comprehensive_business_logic" {
  // 测试综合业务逻辑
  // 计算商品折扣价格
  let original_price = 1000
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 8        // 8% 税率
  let shipping_cost = 50
  
  // 计算折扣价格
  let discount_amount = multiply(original_price, discount_rate) / 100
  let discounted_price = add(original_price, -discount_amount)
  
  // 计算税费
  let tax_amount = multiply(discounted_price, tax_rate) / 100
  
  // 计算最终价格
  let final_price = add(add(discounted_price, tax_amount), shipping_cost)
  
  assert_eq(938, final_price)
  
  // 测试批量购买折扣
  let quantity = 5
  let bulk_discount = 10  // 额外10%批量折扣
  let bulk_discount_amount = multiply(final_price, bulk_discount) / 100
  let bulk_final_price = multiply(add(final_price, -bulk_discount_amount), quantity)
  
  assert_eq(4190, bulk_final_price)
}