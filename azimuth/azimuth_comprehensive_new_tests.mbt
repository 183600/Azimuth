// Azimuth 综合新测试用例 - 10个高质量标准 MoonBit 测试用例
// 测试覆盖基本算术、边界情况、业务逻辑和错误处理场景

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
  assert_eq(0, subtract(0, 0))
}

test "compound_arithmetic_operations" {
  // 复合算术运算测试
  let a = 12
  let b = 8
  let c = 3
  
  // 测试复合运算：(a + b) * c - a / c
  let step1 = add(a, b)           // 12 + 8 = 20
  let step2 = multiply(step1, c)   // 20 * 3 = 60
  let step3 = divide_with_ceil(a, c)  // ceil(12/3) = 4
  let result = subtract(step2, step3) // 60 - 4 = 56
  
  assert_eq(56, result)
}

test "inventory_management_calculations" {
  // 库存管理计算测试
  let initial_stock = 150
  let sales_per_day = 12
  let days = 5
  let new_shipment = 80
  
  // 计算销售后的库存
  let total_sales = multiply(sales_per_day, days)  // 12 * 5 = 60
  let remaining_stock = subtract(initial_stock, total_sales)  // 150 - 60 = 90
  
  // 计算新库存
  let new_total_stock = add(remaining_stock, new_shipment)  // 90 + 80 = 170
  
  // 计算需要的存储箱数（每箱30个）
  let boxes_needed = divide_with_ceil(new_total_stock, 30)  // ceil(170/30) = 6
  
  assert_eq(170, new_total_stock)
  assert_eq(6, boxes_needed)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 1000
  let rate_percent = 8
  let years = 3
  
  // 第一年
  let year1_interest = divide_with_ceil(multiply(principal, rate_percent), 100)  // ceil(1000*8/100) = 80
  let year1_total = add(principal, year1_interest)  // 1000 + 80 = 1080
  
  // 第二年
  let year2_interest = divide_with_ceil(multiply(year1_total, rate_percent), 100)  // ceil(1080*8/100) = 87
  let year2_total = add(year1_total, year2_interest)  // 1080 + 87 = 1167
  
  // 第三年
  let year3_interest = divide_with_ceil(multiply(year2_total, rate_percent), 100)  // ceil(1167*8/100) = 94
  let year3_total = add(year2_total, year3_interest)  // 1167 + 94 = 1261
  
  assert_eq(1080, year1_total)
  assert_eq(1167, year2_total)
  assert_eq(1261, year3_total)
}

test "resource_allocation_fair_distribution" {
  // 资源分配公平分布测试
  let total_resources = 100
  let groups = 3
  let min_per_group = divide_with_ceil(total_resources, groups)  // ceil(100/3) = 34
  
  // 计算实际分配
  let allocated_total = multiply(min_per_group, groups)  // 34 * 3 = 102
  let overflow = subtract(allocated_total, total_resources)  // 102 - 100 = 2
  
  // 验证每个组至少获得基本份额
  assert_eq(34, min_per_group)
  assert_eq(102, allocated_total)
  assert_eq(2, overflow)
  
  // 验证分配的公平性（没有组获得少于其他组）
  assert_true(min_per_group > divide_with_ceil(total_resources, groups + 1))
}

test "unicode_string_edge_cases" {
  // Unicode 字符串边缘情况测试
  assert_eq_string("Hello, 🌙🌟🌙!", greet("🌙🌟🌙"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, עברית!", greet("עברית"))
  assert_eq_string("Hello, हिन्दी!", greet("हिन्दी"))
  assert_eq_string("Hello, 🎉🚀🌟!", greet("🎉🚀🌟"))
}

test "mathematical_series_calculation" {
  // 数学数列计算测试
  // 计算等差数列：2, 5, 8, 11, 14 的和
  let first_term = 2
  let common_diff = 3
  let terms = 5
  
  // 使用公式计算：和 = n/2 * (2a + (n-1)d)
  let double_sum = multiply(terms, add(multiply(2, first_term), multiply(terms - 1, common_diff)))
  let series_sum = divide_with_ceil(double_sum, 2)
  
  // 手动验证：2 + 5 + 8 + 11 + 14 = 40
  assert_eq(40, series_sum)
  
  // 计算平均值
  let average = divide_with_ceil(series_sum, terms)
  assert_eq(8, average)  // ceil(40/5) = 8
}

test "project_timeline_optimization" {
  // 项目时间线优化测试
  let total_tasks = 45
  let team_size = 4
  let tasks_per_person_per_day = 2
  let buffer_days = 3
  
  // 计算基础工作日
  let daily_capacity = multiply(team_size, tasks_per_person_per_day)  // 4 * 2 = 8
  let base_days = divide_with_ceil(total_tasks, daily_capacity)  // ceil(45/8) = 6
  
  // 加上缓冲时间
  let total_days = add(base_days, buffer_days)  // 6 + 3 = 9
  
  // 计算实际完成能力
  let actual_capacity = multiply(total_days, daily_capacity)  // 9 * 8 = 72
  let slack_capacity = subtract(actual_capacity, total_tasks)  // 72 - 45 = 27
  
  assert_eq(6, base_days)
  assert_eq(9, total_days)
  assert_eq(27, slack_capacity)
}

test "error_resilience_boundary_testing" {
  // 错误恢复边界测试
  // 测试各种极端边界情况
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 1000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(1, divide_with_ceil(-1, 1))
  assert_eq(-1, divide_with_ceil(1, -1))
  
  // 测试大数运算
  let large_num = 1000000
  assert_eq(2000000, add(large_num, large_num))
  assert_eq(1000000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  assert_eq(2, divide_with_ceil(add(large_num, 1), large_num))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：电商订单处理
  let order_items = 25
  let items_per_box = 6
  let processing_time_per_box = 30  // 分钟
  let preparation_time = 15  // 分钟
  
  // 计算需要的箱子数
  let boxes_needed = divide_with_ceil(order_items, items_per_box)  // ceil(25/6) = 5
  
  // 计算总处理时间
  let boxing_time = multiply(boxes_needed, processing_time_per_box)  // 5 * 30 = 150
  let total_time = add(boxing_time, preparation_time)  // 150 + 15 = 165
  
  // 计算每小时处理的订单数
  let hourly_capacity = divide_with_ceil(3600, multiply(total_time, 60))  // ceil(3600/9900) = 1
  
  // 验证装箱利用率
  let total_capacity = multiply(boxes_needed, items_per_box)  // 5 * 6 = 30
  let unused_space = subtract(total_capacity, order_items)  // 30 - 25 = 5
  let utilization_rate = multiply(divide_with_ceil(multiply(order_items, 100), total_capacity), 1)  // ceil(2500/30) = 84
  
  assert_eq(5, boxes_needed)
  assert_eq(165, total_time)
  assert_eq(1, hourly_capacity)
  assert_eq(5, unused_space)
  assert_eq(84, utilization_rate)
}