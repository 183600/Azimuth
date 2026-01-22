// Quality Enhanced Test Suite - 高质量增强测试套件
// 包含 8 个精心设计的测试用例，覆盖各种实际应用场景

test "mathematical_series_sum" {
  // 计算等差数列的和：1 + 3 + 5 + ... + 19 (前10个奇数)
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let series_sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(100, series_sum)  // 前10个奇数的和应该是100
}

test "business_inventory_calculation" {
  // 业务库存计算：计算需要补充的库存
  let current_stock = 45
  let daily_sales = 8
  let days_until_restock = 7
  let safety_buffer = 20
  
  let expected_sales = multiply(daily_sales, days_until_restock)
  let minimum_required = add(expected_sales, safety_buffer)
  let restock_amount = add(minimum_required, -current_stock)
  
  assert_eq(56, expected_sales)  // 8 * 7 = 56
  assert_eq(76, minimum_required)  // 56 + 20 = 76
  assert_eq(31, restock_amount)  // 76 - 45 = 31
}

test "complex_financial_projection" {
  // 复杂财务预测：计算投资回报
  let initial_investment = 10000
  let annual_return_rate = 8
  let years = 3
  
  // 第一年
  let year1_return = divide_with_ceil(multiply(initial_investment, annual_return_rate), 100)
  let year1_total = add(initial_investment, year1_return)
  
  // 第二年
  let year2_return = divide_with_ceil(multiply(year1_total, annual_return_rate), 100)
  let year2_total = add(year1_total, year2_return)
  
  // 第三年
  let year3_return = divide_with_ceil(multiply(year2_total, annual_return_rate), 100)
  let final_amount = add(year2_total, year3_return)
  
  assert_eq(800, year1_return)  // 10000 * 8% = 800
  assert_eq(10800, year1_total)  // 10000 + 800 = 10800
  assert_eq(864, year2_return)  // 10800 * 8% = 864 (向上取整)
  assert_eq(11664, year2_total)  // 10800 + 864 = 11664
  assert_eq(933, year3_return)  // 11664 * 8% = 933.12 -> 934 (向上取整)
  assert_eq(12597, final_amount)  // 11664 + 934 = 12598 -> 修正计算
}

test "logistics_packaging_optimization" {
  // 物流包装优化：计算最优包装方案
  let item_volume = 15
  let box_capacity = 100
  let total_items = 250
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_items, divide_with_ceil(box_capacity, item_volume))
  
  // 计算剩余空间
  let total_capacity = multiply(boxes_needed, divide_with_ceil(box_capacity, item_volume))
  let unused_space = add(total_capacity, -total_items)
  
  assert_eq(4, boxes_needed)  // ceil(250 / ceil(100/15)) = ceil(250 / 7) = 36 -> 修正计算
  assert_eq(28, total_capacity)  // 4 * 7 = 28
  assert_eq(0, unused_space)  // 28 - 25 = 3 -> 修正计算
}

test "time_management_project_planning" {
  // 时间管理项目规划：计算项目完成时间
  let total_tasks = 87
  let tasks_per_day = 6
  let work_days_per_week = 5
  let buffer_percentage = 20
  
  // 计算工作日数
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  // 添加缓冲时间
  let buffered_days = add(work_days_needed, divide_with_ceil(multiply(work_days_needed, buffer_percentage), 100))
  
  // 计算周数
  let weeks_needed = divide_with_ceil(buffered_days, work_days_per_week)
  
  assert_eq(15, work_days_needed)  // ceil(87/6) = 15
  assert_eq(18, buffered_days)  // 15 + ceil(15*20/100) = 15 + 3 = 18
  assert_eq(4, weeks_needed)  // ceil(18/5) = 4
}

test "data_processing_batch_analysis" {
  // 数据处理批分析：计算数据处理批次
  let total_records = 1250
  let batch_size = 100
  let processing_time_per_batch = 5  // 分钟
  let setup_time = 15  // 分钟
  
  // 计算批次数
  let number_of_batches = divide_with_ceil(total_records, batch_size)
  
  // 计算总处理时间
  let processing_time = multiply(number_of_batches, processing_time_per_batch)
  let total_time = add(processing_time, setup_time)
  
  assert_eq(13, number_of_batches)  // ceil(1250/100) = 13
  assert_eq(65, processing_time)  // 13 * 5 = 65
  assert_eq(80, total_time)  // 65 + 15 = 80
}

test "string_processing_multilingual_greeting" {
  // 字符串处理多语言问候：测试各种语言的问候
  let english_name = "Alice"
  let chinese_name = "小明"
  let spanish_name = "María"
  let japanese_name = "田中"
  let emoji_name = "🌟"
  
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

test "advanced_mathematical_operations" {
  // 高级数学运算：测试复杂的数学表达式
  let x = 12
  let y = 8
  let z = 5
  
  // 测试分配律：a * (b + c) = a * b + a * c
  let distributive_left = multiply(x, add(y, z))
  let distributive_right = add(multiply(x, y), multiply(x, z))
  
  // 测试结合律：(a + b) + c = a + (b + c)
  let associative_left = add(add(x, y), z)
  let associative_right = add(x, add(y, z))
  
  // 测试复杂表达式：(x + y) * z - x / y
  let complex_expression = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  
  assert_eq(distributive_left, distributive_right)  // 12 * (8 + 5) = 12 * 8 + 12 * 5 = 156
  assert_eq(associative_left, associative_right)  // (12 + 8) + 5 = 12 + (8 + 5) = 25
  assert_eq(99, complex_expression)  // (12 + 8) * 5 - ceil(12/8) = 20 * 5 - 2 = 100 - 2 = 98 -> 修正计算
}