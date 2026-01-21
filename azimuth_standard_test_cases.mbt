// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法，包含实用和有趣的测试场景

test "arithmetical_sequence_sum" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19 (前10个奇数)
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：和 = n/2 * (2a + (n-1)d)
  let sequence_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(100, sequence_sum)  // 前10个奇数的和应该是100
}

test "geometric_progression" {
  // 计算等比数列：2, 4, 8, 16, 32
  let first_term = 2
  let ratio = 2
  let terms = 5
  
  // 手动计算等比数列的和
  let term1 = first_term
  let term2 = multiply(term1, ratio)
  let term3 = multiply(term2, ratio)
  let term4 = multiply(term3, ratio)
  let term5 = multiply(term4, ratio)
  
  let geometric_sum = add(add(add(add(term1, term2), term3), term4), term5)
  
  assert_eq(62, geometric_sum)  // 2 + 4 + 8 + 16 + 32 = 62
}

test "temperature_conversion" {
  // 模拟温度转换：摄氏度到华氏度 F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = add(divide_with_ceil(multiply(celsius, 9), 5), 32)
  
  assert_eq(77, fahrenheit)  // 25°C = 77°F
  
  // 测试冰点
  let freezing_celsius = 0
  let freezing_fahrenheit = add(divide_with_ceil(multiply(freezing_celsius, 9), 5), 32)
  
  assert_eq(32, freezing_fahrenheit)  // 0°C = 32°F
}

test "restaurant_bill_calculation" {
  // 餐厅账单计算：餐费 + 小费 + 税费
  let meal_cost = 85
  let tip_percentage = 15  // 15% 小费
  let tax_percentage = 8   // 8% 税费
  
  // 计算小费和税费（使用向上取整）
  let tip = divide_with_ceil(multiply(meal_cost, tip_percentage), 100)
  let tax = divide_with_ceil(multiply(meal_cost, tax_percentage), 100)
  
  // 计算总账单
  let total_bill = add(add(meal_cost, tip), tax)
  
  assert_eq(110, total_bill)  // 85 + ceil(85*15/100) + ceil(85*8/100) = 85 + 13 + 7 = 105
}

test "package_shipping_calculation" {
  // 包裹运费计算：基础费用 + 重量费用 + 体积费用
  let base_fee = 10
  let weight = 15  // kg
  let weight_rate = 2  // 每kg费用
  let volume = 25  // 立方分米
  let volume_rate = 1  // 每立方分米费用
  
  // 计算重量和体积费用
  let weight_fee = multiply(weight, weight_rate)
  let volume_fee = multiply(volume, volume_rate)
  
  // 计算总运费
  let total_shipping = add(add(base_fee, weight_fee), volume_fee)
  
  assert_eq(65, total_shipping)  // 10 + 15*2 + 25*1 = 10 + 30 + 25 = 65
}

test "loan_interest_calculation" {
  // 贷款利息计算：简单利息
  let principal = 10000
  let annual_rate = 5  // 5% 年利率
  let years = 3
  
  // 计算每年的利息
  let yearly_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  
  // 计算总利息
  let total_interest = multiply(yearly_interest, years)
  
  // 计算总还款额
  let total_repayment = add(principal, total_interest)
  
  assert_eq(11500, total_repayment)  // 10000 + 3*ceil(10000*5/100) = 10000 + 3*500 = 11500
}

test "batch_production_planning" {
  // 批量生产计划：计算生产批次
  let total_products = 157
  let batch_capacity = 25
  
  // 计算需要的批次数
  let batches_needed = divide_with_ceil(total_products, batch_capacity)
  
  // 计算最后一批次的产品数量
  let last_batch_products = add(total_products, -multiply(batches_needed - 1, batch_capacity))
  
  assert_eq(7, batches_needed)  // ceil(157/25) = 7
  assert_eq(7, last_batch_products)  // 157 - 6*25 = 7
}

test "digital_storage_conversion" {
  // 数字存储转换：KB 到 MB
  let total_kb = 2850
  let kb_per_mb = 1024
  
  // 计算完整的MB数
  let full_mb = total_kb / kb_per_mb
  
  // 计算剩余的KB数
  let remaining_kb = add(total_kb, -multiply(full_mb, kb_per_mb))
  
  // 计算需要的总MB数（包括不足1MB的部分）
  let total_mb_needed = divide_with_ceil(total_kb, kb_per_mb)
  
  assert_eq(2, full_mb)  // 2850 / 1024 = 2
  assert_eq(802, remaining_kb)  // 2850 - 2*1024 = 802
  assert_eq(3, total_mb_needed)  // ceil(2850/1024) = 3
}

test "event_seating_arrangement" {
  // 活动座位安排：计算需要的桌子数量
  let total_guests = 85
  let seats_per_table = 8
  let extra_tables_for_stage = 2
  
  // 计算客人需要的桌子数量
  let guest_tables = divide_with_ceil(total_guests, seats_per_table)
  
  // 计算总桌子数量
  let total_tables = add(guest_tables, extra_tables_for_stage)
  
  assert_eq(11, guest_tables)  // ceil(85/8) = 11
  assert_eq(13, total_tables)  // 11 + 2 = 13
}

test "greeting_multilingual_support" {
  // 多语言问候支持测试
  let english_name = "Alice"
  let chinese_name = "小明"
  let spanish_name = "María"
  let japanese_name = "田中"
  let emoji_name = "🌟"
  
  // 测试不同语言的问候
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let spanish_greeting = greet(spanish_name)
  let japanese_greeting = greet(japanese_name)
  let emoji_greeting = greet(emoji_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 小明!", chinese_greeting)
  assert_eq_string("Hello, María!", spanish_greeting)
  assert_eq_string("Hello, 田中!", japanese_greeting)
  assert_eq_string("Hello, 🌟!", emoji_greeting)
}