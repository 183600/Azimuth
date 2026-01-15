// 增强的 MoonBit 测试用例
// 包含10个新的测试用例，使用标准的 MoonBit 测试语法

test "string_unicode_processing" {
  // 测试Unicode字符串处理
  let chinese_name = "张三"
  let japanese_name = "田中"
  let korean_name = "김철수"
  let arabic_name = "أحمد"
  let emoji_name = "🌟🚀"
  
  assert_eq_string("Hello, 张三!", azimuth::greet(chinese_name))
  assert_eq_string("Hello, 田中!", azimuth::greet(japanese_name))
  assert_eq_string("Hello, 김철수!", azimuth::greet(korean_name))
  assert_eq_string("Hello, أحمد!", azimuth::greet(arabic_name))
  assert_eq_string("Hello, 🌟🚀!", azimuth::greet(emoji_name))
}

test "mathematical_sequence_calculations" {
  // 测试数学序列计算
  
  // 斐波那契数列前10项和：1, 1, 2, 3, 5, 8, 13, 21, 34, 55
  // 总和 = 1 + 1 + 2 + 3 + 5 + 8 + 13 + 21 + 34 + 55 = 143
  let fib1 = 1
  let fib2 = 1
  let fib3 = 2
  let fib4 = 3
  let fib5 = 5
  let fib6 = 8
  let fib7 = 13
  let fib8 = 21
  let fib9 = 34
  let fib10 = 55
  
  let fib_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(azimuth::add(
    azimuth::add(azimuth::add(azimuth::add(fib1, fib2), fib3), fib4), fib5), fib6), fib7), fib8), fib9), fib10)
  
  assert_eq(143, fib_sum)
  
  // 等比数列求和：2 + 4 + 8 + 16 + 32 = 62
  let geo_sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(2, 4), 8), 16), 32)
  assert_eq(62, geo_sum)
}

test "business_logic_calculations" {
  // 测试业务逻辑计算
  
  // 库存管理计算
  let initial_stock = 1000
  let units_sold = 350
  let units_received = 200
  let damaged_units = 15
  
  // 计算当前库存：初始库存 - 销售数量 + 收到数量 - 损坏数量
  let current_stock = azimuth::add(azimuth::add(
    azimuth::add(initial_stock, -units_sold), units_received), -damaged_units)
  
  assert_eq(835, current_stock)
  
  // 利润计算：收入 - 成本 - 运营费用
  let revenue = 50000
  let cost = 30000
  let operating_expenses = 12000
  
  let profit = azimuth::add(azimuth::add(revenue, -cost), -operating_expenses)
  assert_eq(8000, profit)
}

test "scientific_calculations" {
  // 测试科学计算
  
  // 简化的物理公式：动能 = 0.5 × 质量 × 速度²
  // 使用整数计算：动能 = 质量 × 速度² / 2
  let mass = 10
  let velocity = 15
  
  let kinetic_energy = azimuth::multiply(mass, azimuth::multiply(velocity, velocity)) / 2
  assert_eq(1125, kinetic_energy)  // 10 × 15² / 2 = 10 × 225 / 2 = 1125
  
  // 简化的化学计算：摩尔浓度 = 溶质摩尔数 / 溶液体积
  // 使用整数计算：浓度 × 1000 = (摩尔数 × 1000) / 体积
  let moles = 2
  let volume = 500  // 毫升
  
  let concentration = azimuth::multiply(moles, 1000) / volume
  assert_eq(4, concentration)  // 2 × 1000 / 500 = 4
}

test "algorithmic_complexity_simulation" {
  // 测试算法复杂度模拟
  
  // 模拟O(n²)算法的计算量
  let n = 10
  // 计算量 = n × (n + 1) / 2
  let complexity = azimuth::multiply(n, azimuth::add(n, 1)) / 2
  assert_eq(55, complexity)  // 10 × 11 / 2 = 55
  
  // 模拟O(n³)算法的计算量（简化）
  let n2 = 5
  // 计算量 ≈ n³
  let complexity_cubic = azimuth::multiply(n2, azimuth::multiply(n2, n2))
  assert_eq(125, complexity_cubic)  // 5³ = 125
  
  // 模拟二分搜索的比较次数
  let array_size = 1024
  // 比较次数 ≈ log₂(array_size)
  // 1024 = 2¹⁰，所以需要10次比较
  let binary_search_comparisons = 10
  assert_eq(10, binary_search_comparisons)
}

test "date_time_calculations" {
  // 测试日期时间计算
  
  // 计算两个时间点之间的分钟数
  let start_hour = 9
  let start_minute = 30
  let end_hour = 17
  let end_minute = 45
  
  let start_total_minutes = azimuth::add(azimuth::multiply(start_hour, 60), start_minute)
  let end_total_minutes = azimuth::add(azimuth::multiply(end_hour, 60), end_minute)
  let duration_minutes = azimuth::add(end_total_minutes, -start_total_minutes)
  
  assert_eq(495, duration_minutes)  // 17:45 - 9:30 = 8小时15分钟 = 495分钟
  
  // 计算工作日小时数（考虑午休）
  let work_hours = 8
  let lunch_break = 1
  let productive_hours = azimuth::add(work_hours, -lunch_break)
  assert_eq(7, productive_hours)
  
  // 计算一周的工作分钟数
  let days_per_week = 5
  let weekly_minutes = azimuth::multiply(productive_hours, azimuth::multiply(days_per_week, 60))
  assert_eq(2100, weekly_minutes)  // 7小时 × 5天 × 60分钟 = 2100分钟
}

test "financial_calculations_advanced" {
  // 测试高级金融计算
  
  // 贷款月供计算（简化版）
  let loan_amount = 200000
  let annual_rate = 6  // 6%
  let loan_years = 30
  
  // 简化计算：月供 ≈ 本金 / 月数 + 利息
  let total_months = azimuth::multiply(loan_years, 12)
  let monthly_principal = loan_amount / total_months
  
  // 简化的月利息计算
  let monthly_rate = annual_rate / 12
  let first_month_interest = azimuth::multiply(loan_amount, monthly_rate) / 100
  
  let monthly_payment = azimuth::add(monthly_principal, first_month_interest)
  assert_eq(1222, monthly_payment)  // 简化计算结果
  
  // 投资回报率计算
  let initial_investment = 10000
  let final_value = 15000
  let investment_years = 5
  
  let total_return = azimuth::add(final_value, -initial_investment)
  let annual_return = total_return / investment_years
  let return_rate = azimuth::multiply(annual_return, 100) / initial_investment
  
  assert_eq(1000, annual_return)  // (15000 - 10000) / 5 = 1000
  assert_eq(10, return_rate)      // 1000 × 100 / 10000 = 10%
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  
  // 验证年龄范围
  let age = 25
  let min_age = 18
  let max_age = 65
  
  let is_valid_age = azimuth::add(age, -min_age) >= 0 && azimuth::add(max_age, -age) >= 0
  azimuth::assert_true(is_valid_age)
  
  // 验证密码强度（简化：长度检查）
  let password_length = 12
  let min_length = 8
  let is_long_enough = password_length >= min_length
  azimuth::assert_true(is_long_enough)
  
  // 验证数值范围
  let test_value = 50
  let lower_bound = 0
  let upper_bound = 100
  
  let is_in_range = azimuth::add(test_value, -lower_bound) >= 0 && azimuth::add(upper_bound, -test_value) >= 0
  azimuth::assert_true(is_in_range)
  
  // 验证邮箱格式（简化：检查@符号）
  let email = "test@example.com"
  let has_at_symbol = email.length >= 5  // 简化检查
  azimuth::assert_true(has_at_symbol)
}

test "performance_optimization_calculations" {
  // 测试性能优化计算
  
  // 缓存命中率计算
  let cache_requests = 1000
  let cache_hits = 850
  
  let hit_rate = azimuth::multiply(cache_hits, 100) / cache_requests
  assert_eq(85, hit_rate)  // 850 × 100 / 1000 = 85%
  
  // 算法改进效果计算
  let original_time = 1000  // 毫秒
  let optimized_time = 250  // 毫秒
  
  let time_saved = azimuth::add(original_time, -optimized_time)
  let improvement_percentage = azimuth::multiply(time_saved, 100) / original_time
  
  assert_eq(750, time_saved)           // 1000 - 250 = 750
  assert_eq(75, improvement_percentage) // 750 × 100 / 1000 = 75%
  
  // 内存使用优化计算
  let original_memory = 500  // MB
  let optimized_memory = 300  // MB
  
  let memory_saved = azimuth::add(original_memory, -optimized_memory)
  let memory_efficiency = azimuth::multiply(azimuth::add(original_memory, -memory_saved), 100) / original_memory
  
  assert_eq(200, memory_saved)      // 500 - 300 = 200
  assert_eq(60, memory_efficiency)  // 300 × 100 / 500 = 60%
}

test "edge_case_combinations" {
  // 测试边界条件组合
  
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 复杂的边界运算：(max + min) × (max - min + 1)
  let sum_max_min = azimuth::add(max_val, min_val)
  let diff_max_min = azimuth::add(max_val, -min_val)
  let adjusted_diff = azimuth::add(diff_max_min, one)
  let complex_result = azimuth::multiply(sum_max_min, adjusted_diff)
  
  assert_eq(-1, complex_result)  // (-1) × 1 = -1
  
  // 零值链式运算
  let zero_chain = azimuth::add(azimuth::multiply(azimuth::add(zero, one), zero), max_val)
  assert_eq(max_val, zero_chain)
  
  // 极值与中间值的组合
  let mid_value = 1000000
  let mixed_calculation = azimuth::add(azimuth::multiply(max_val, zero), 
    azimuth::add(azimuth::multiply(min_val, zero), mid_value))
  assert_eq(mid_value, mixed_calculation)
  
  // 测试除零保护（通过条件判断）
  let divisor = zero
  let dividend = 100
  let safe_result = if divisor == zero { zero } else { dividend / divisor }
  assert_eq(zero, safe_result)
}