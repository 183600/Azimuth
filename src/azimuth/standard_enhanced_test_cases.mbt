// 标准 MoonBit 增强测试用例
// 覆盖 azimuth 库的核心功能，包含实际应用场景

test "divide_with_ceil_practical_scenarios" {
  // 实际场景：计算包装箱数量
  let items_per_box = 12
  let total_items_1 = 50
  let total_items_2 = 60
  let total_items_3 = 0
  
  let boxes_needed_1 = divide_with_ceil(total_items_1, items_per_box)
  let boxes_needed_2 = divide_with_ceil(total_items_2, items_per_box)
  let boxes_needed_3 = divide_with_ceil(total_items_3, items_per_box)
  
  assert_eq(5, boxes_needed_1)  // ceil(50/12) = 5
  assert_eq(5, boxes_needed_2)  // ceil(60/12) = 5
  assert_eq(0, boxes_needed_3)  // ceil(0/12) = 0
}

test "divide_with_ceil_negative_dividend" {
  // 测试负被除数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(0, divide_with_ceil(-1, -1))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负除数的向上取整除法
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(-3, divide_with_ceil(11, -3))
  assert_eq(-2, divide_with_ceil(6, -3))
  assert_eq(0, divide_with_ceil(1, -2))
}

test "divide_with_ceil_both_negative" {
  // 测试负被除数和负除数的情况
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(3, divide_with_ceil(-9, -3))
  assert_eq(2, divide_with_ceil(-6, -3))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_with_whitespace" {
  // 测试包含空白字符的问候
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,  Leading Space!", greet(" Leading Space"))
  assert_eq_string("Hello, Trailing Space !", greet("Trailing Space "))
}

test "mathematical_sequence_fibonacci_style" {
  // 类斐波那契数列计算（使用现有函数）
  let a = 1
  let b = 1
  
  // 计算前几项
  let term3 = add(a, b)  // 2
  let term4 = add(b, term3)  // 3
  let term5 = add(term3, term4)  // 5
  let term6 = add(term4, term5)  // 8
  
  assert_eq(2, term3)
  assert_eq(3, term4)
  assert_eq(5, term5)
  assert_eq(8, term6)
  
  // 验证黄金比例近似
  let ratio_approx = multiply(term6, 100) / term5  // 8*100/5 = 160
  assert_eq(160, ratio_approx)  // 近似1.6倍
}

test "business_inventory_calculation" {
  // 库存管理计算
  let product_cost = 45
  let selling_price = 68
  let initial_stock = 120
  let sold_units = 87
  let reorder_threshold = 50
  
  // 计算剩余库存
  let remaining_stock = add(initial_stock, -sold_units)
  assert_eq(33, remaining_stock)
  
  // 计算总收入
  let total_revenue = multiply(selling_price, sold_units)
  assert_eq(5916, total_revenue)
  
  // 计算总成本
  let total_cost = multiply(product_cost, sold_units)
  assert_eq(3915, total_cost)
  
  // 计算利润
  let profit = add(total_revenue, -total_cost)
  assert_eq(2001, profit)
  
  // 判断是否需要补货
  let needs_reorder = if remaining_stock < reorder_threshold { 1 } else { 0 }
  assert_eq(1, needs_reorder)
}

test "time_based_calculations" {
  // 基于时间的计算
  let hours_per_day = 24
  let days_per_week = 7
  let weeks_per_month = 4
  
  // 计算一个月的小时数
  let hours_per_month = multiply(hours_per_day, multiply(days_per_week, weeks_per_month))
  assert_eq(672, hours_per_month)
  
  // 计算工作安排
  let work_hours_per_day = 8
  let work_days_per_week = 5
  let weekly_work_hours = multiply(work_hours_per_day, work_days_per_week)
  assert_eq(40, weekly_work_hours)
  
  // 计算月工作小时
  let monthly_work_hours = multiply(weekly_work_hours, weeks_per_month)
  assert_eq(160, monthly_work_hours)
}

test "geometric_progression_sum" {
  // 等比数列求和（简化版）
  let first_term = 3
  let ratio = 2
  
  // 手动计算各项
  let term1 = first_term  // 3
  let term2 = multiply(term1, ratio)  // 6
  let term3 = multiply(term2, ratio)  // 12
  let term4 = multiply(term3, ratio)  // 24
  let term5 = multiply(term4, ratio)  // 48
  let term6 = multiply(term5, ratio)  // 96
  
  // 计算总和
  let sum = add(add(add(add(add(term1, term2), term3), term4), term5), term6)
  assert_eq(189, sum)
  
  // 验证等比数列性质
  let ratio_check = multiply(term3, term3)  // 12*12 = 144
  let product_check = multiply(term2, term4)  // 6*24 = 144
  assert_eq(ratio_check, product_check)
}

test "complex_string_greeting_scenarios" {
  // 复杂字符串问候场景
  // 测试不同语言的问候
  let chinese_name = "张三"
  let english_name = "John Smith"
  let mixed_name = "José García"
  let emoji_name = "🌟Star🌟"
  
  assert_eq_string("Hello, 张三!", greet(chinese_name))
  assert_eq_string("Hello, John Smith!", greet(english_name))
  assert_eq_string("Hello, José García!", greet(mixed_name))
  assert_eq_string("Hello, 🌟Star🌟!", greet(emoji_name))
  
  // 测试包含数字和特殊字符的名称
  let numeric_name = "User123"
  let special_name = "Test-User_2023"
  
  assert_eq_string("Hello, User123!", greet(numeric_name))
  assert_eq_string("Hello, Test-User_2023!", greet(special_name))
}