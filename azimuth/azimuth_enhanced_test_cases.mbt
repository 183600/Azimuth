// Azimuth 增强测试用例 - 专注于独特的测试场景
// 为 azimuth 库添加高质量的标准 MoonBit 测试用例

test "overflow_boundary_protection" {
  // 测试整数边界保护
  // 测试接近 Int 边界的值
  assert_eq(2147483646, add(2147483645, 1))
  assert_eq(-2147483647, add(-2147483646, -1))
  assert_eq(2147483647, multiply(46340, 46341))
}

test "divide_with_ceil_precision_edge_cases" {
  // 测试向上取整除法的精度边缘情况
  assert_eq(1, divide_with_ceil(999999, 1000000))
  assert_eq(1000000, divide_with_ceil(999999999, 999))
  assert_eq(2, divide_with_ceil(1000001, 500000))
}

test "greet_function_unicode_normalization" {
  // 测试问候函数的Unicode标准化处理
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Naïve!", greet("Naïve"))
  assert_eq_string("Hello, 🏳️‍🌈!", greet("🏳️‍🌈"))
  assert_eq_string("Hello, 👨‍👩‍👧‍👦!", greet("👨‍👩‍👧‍👦"))
}

test "complex_financial_amortization" {
  // 测试复杂的金融摊还计算
  let principal = 250000
  let annual_rate = 4
  let loan_term_years = 30
  
  // 计算月利率（简化）
  let monthly_rate = divide_with_ceil(annual_rate, 12)
  
  // 计算总月数
  let total_months = multiply(loan_term_years, 12)
  
  // 计算总利息（简化计算）
  let total_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let total_interest_full_term = multiply(total_interest, loan_term_years)
  
  // 计算月供（简化）
  let total_payment = add(principal, total_interest_full_term)
  let monthly_payment = divide_with_ceil(total_payment, total_months)
  
  assert_eq(1, monthly_rate)
  assert_eq(360, total_months)
  assert_eq(10000, total_interest)
  assert_eq(300000, total_interest_full_term)
  assert_eq(550000, total_payment)
  assert_eq(1528, monthly_payment)
}

test "resource_allocation_fair_division" {
  // 测试资源分配的公平除法算法
  let total_resources = 100
  let participants = 3
  
  // 公平分配，确保每个参与者至少获得基本份额
  let base_share = divide_with_ceil(total_resources, participants)
  assert_eq(34, base_share)
  
  // 验证分配后的剩余资源
  let allocated_total = multiply(base_share, participants)
  let remaining = subtract(allocated_total, total_resources)
  assert_eq(2, remaining)
  
  // 测试资源不足的情况
  let insufficient_resources = 5
  let insufficient_share = divide_with_ceil(insufficient_resources, participants)
  assert_eq(2, insufficient_share)
}

test "string_escape_sequence_handling" {
  // 测试字符串转义序列处理
  assert_eq_string("Hello, \\n\\t\\r!", greet("\\n\\t\\r"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
  assert_eq_string("Hello, 'single'!", greet("'single'"))
  assert_eq_string("Hello, \\backslash!", greet("\\backslash"))
  assert_eq_string("Hello, \\u0041!", greet("\\u0041"))
}

test "mathematical_series_calculation" {
  // 测试数学级数计算
  // 计算等差数列前n项和：S = n/2 * (2a + (n-1)d)
  let n = 10
  let a = 3
  let d = 2
  
  let two_a = multiply(a, 2)
  let n_minus_1 = subtract(n, 1)
  let d_times_n_minus_1 = multiply(d, n_minus_1)
  let bracket_term = add(two_a, d_times_n_minus_1)
  let series_sum = divide_with_ceil(multiply(n, bracket_term), 2)
  
  assert_eq(6, two_a)
  assert_eq(9, n_minus_1)
  assert_eq(18, d_times_n_minus_1)
  assert_eq(24, bracket_term)
  assert_eq(120, series_sum)
}

test "inventory_rotation_optimization" {
  // 测试库存轮换优化算法
  let total_items = 247
  let shelf_life_days = 30
  let daily_consumption = 8
  
  // 计算理论库存天数
  let theoretical_days = divide_with_ceil(total_items, daily_consumption)
  assert_eq(31, theoretical_days)
  
  // 计算过期风险
  let days_until_expiration = subtract(theoretical_days, shelf_life_days)
  let has_expiration_risk = days_until_expiration > 0
  
  // 计算建议的采购量（不超过保质期）
  let safe_consumption = multiply(shelf_life_days, daily_consumption)
  let recommended_purchase = subtract(safe_consumption, total_items)
  
  assert_eq(1, days_until_expiration)
  assert_eq(true, has_expiration_risk)
  assert_eq(240, safe_consumption)
  assert_eq(-7, recommended_purchase)
}

test "complex_time_zone_calculation" {
  // 测试复杂时区计算
  let local_hour = 23
  let utc_offset = -5  // EST (UTC-5)
  let target_offset = 9  // JST (UTC+9)
  
  // 计算UTC时间
  let utc_hour = add(local_hour, utc_offset)
  let normalized_utc = divide_with_ceil(utc_hour, 24)
  let utc_normalized = subtract(utc_hour, multiply(normalized_utc, 24))
  
  // 计算目标时区时间
  let target_hour = add(utc_normalized, target_offset)
  let normalized_target = divide_with_ceil(target_hour, 24)
  let final_hour = subtract(target_hour, multiply(normalized_target, 24))
  
  assert_eq(18, utc_hour)
  assert_eq(1, normalized_utc)
  assert_eq(-6, utc_normalized)
  assert_eq(3, target_hour)
  assert_eq(1, normalized_target)
  assert_eq(3, final_hour)
}

test "algorithm_efficiency_comparison" {
  // 测试算法效率比较
  let dataset_size = 1000
  
  // 模拟O(n²)算法的复杂度
  let quadratic_ops = multiply(dataset_size, dataset_size)
  
  // 模拟O(n log n)算法的复杂度（简化计算）
  let log_approx = divide_with_ceil(dataset_size, 10)
  let n_log_n_ops = multiply(dataset_size, log_approx)
  
  // 模拟O(n)算法的复杂度
  let linear_ops = dataset_size
  
  // 计算效率比率
  let quadratic_vs_linear = divide_with_ceil(quadratic_ops, linear_ops)
  let n_log_n_vs_linear = divide_with_ceil(n_log_n_ops, linear_ops)
  
  assert_eq(1000000, quadratic_ops)
  assert_eq(100, log_approx)
  assert_eq(100000, n_log_n_ops)
  assert_eq(1000, linear_ops)
  assert_eq(1000, quadratic_vs_linear)
  assert_eq(100, n_log_n_vs_linear)
}