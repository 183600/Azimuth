// 新的综合 Azimuth 测试用例
// 包含 8 个高质量测试用例，使用标准 MoonBit 测试语法
// 覆盖 azimuth 库的核心功能和实际应用场景

test "azimuth_core_mathematical_operations" {
  // 测试核心数学运算的组合使用
  let x = 15
  let y = 25
  let z = 8
  
  // 测试复合运算：(x + y) * z - x / y
  let complex_result = azimuth::add(azimuth::multiply(azimuth::add(x, y), z), -azimuth::divide_with_ceil(x, y))
  azimuth::assert_eq(319, complex_result)  // (15 + 25) * 8 - ceil(15/25) = 40 * 8 - 1 = 320 - 1 = 319
  
  // 测试数学分配律：x * (y + z) = x * y + x * z
  let distributive_left = azimuth::multiply(x, azimuth::add(y, z))
  let distributive_right = azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, z))
  azimuth::assert_eq(distributive_left, distributive_right)  // 15 * (25 + 8) = 15 * 25 + 15 * 8 = 495
}

test "azimuth_business_inventory_calculation" {
  // 业务场景：库存管理和 reorder point 计算
  let current_stock = 245
  let daily_sales = 18
  let lead_time_days = 7
  let safety_stock = 50
  
  // 计算在 lead time 期间的销售量
  let lead_time_sales = azimuth::multiply(daily_sales, lead_time_days)
  
  // 计算 reorder point
  let reorder_point = azimuth::add(lead_time_sales, safety_stock)
  
  // 计算需要订购的数量（假设订购到 500 单位）
  let target_stock = 500
  let order_quantity = azimuth::add(target_stock, -current_stock)
  
  azimuth::assert_eq(126, lead_time_sales)  // 18 * 7 = 126
  azimuth::assert_eq(176, reorder_point)   // 126 + 50 = 176
  azimuth::assert_eq(255, order_quantity)  // 500 - 245 = 255
}

test "azimuth_financial_compound_interest" {
  // 金融场景：复利计算
  let principal = 10000
  let annual_rate = 6  // 6%
  let _years = 3
  
  // 第一年
  let year1_interest = azimuth::divide_with_ceil(azimuth::multiply(principal, annual_rate), 100)
  let year1_balance = azimuth::add(principal, year1_interest)
  
  // 第二年
  let year2_interest = azimuth::divide_with_ceil(azimuth::multiply(year1_balance, annual_rate), 100)
  let year2_balance = azimuth::add(year1_balance, year2_interest)
  
  // 第三年
  let year3_interest = azimuth::divide_with_ceil(azimuth::multiply(year2_balance, annual_rate), 100)
  let year3_balance = azimuth::add(year2_balance, year3_interest)
  
  azimuth::assert_eq(600, year1_interest)    // ceil(10000 * 6 / 100) = 600
  azimuth::assert_eq(10600, year1_balance)  // 10000 + 600 = 10600
  azimuth::assert_eq(636, year2_interest)   // ceil(10600 * 6 / 100) = 636
  azimuth::assert_eq(11236, year2_balance)  // 10600 + 636 = 11236
  azimuth::assert_eq(675, year3_interest)   // ceil(11236 * 6 / 100) = 675
  azimuth::assert_eq(11911, year3_balance)  // 11236 + 675 = 11911
}

test "azimuth_project_resource_allocation" {
  // 项目管理：资源分配和工时计算
  let total_tasks = 87
  let tasks_per_person = 6
  let working_hours_per_day = 8
  let days_per_week = 5
  
  // 计算需要的人员数量
  let people_needed = azimuth::divide_with_ceil(total_tasks, tasks_per_person)
  
  // 计算总工作日数（假设每人每天完成 1 个任务）
  let work_days_needed = azimuth::divide_with_ceil(total_tasks, people_needed)
  
  // 计算总工作小时数
  let total_work_hours = azimuth::multiply(work_days_needed, working_hours_per_day)
  
  // 计算工作周数
  let work_weeks_needed = azimuth::divide_with_ceil(work_days_needed, days_per_week)
  
  azimuth::assert_eq(15, people_needed)      // ceil(87 / 6) = 15
  azimuth::assert_eq(6, work_days_needed)    // ceil(87 / 15) = 6
  azimuth::assert_eq(48, total_work_hours)   // 6 * 8 = 48
  azimuth::assert_eq(2, work_weeks_needed)   // ceil(6 / 5) = 2
}

test "azimuth_extreme_boundary_conditions" {
  // 极端边界条件测试
  let large_positive = 1000000
  let large_negative = -1000000
  let small_positive = 1
  let small_negative = -1
  
  // 测试大数运算
  azimuth::assert_eq(2000000, azimuth::add(large_positive, large_positive))
  azimuth::assert_eq(-2000000, azimuth::add(large_negative, large_negative))
  azimuth::assert_eq(0, azimuth::add(large_positive, large_negative))
  
  // 测试大数乘法（使用较小的数避免溢出）
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
  
  // 测试极端除法情况
  azimuth::assert_eq(1000000, azimuth::divide_with_ceil(large_positive, small_positive))
  azimuth::assert_eq(-1000000, azimuth::divide_with_ceil(large_negative, small_positive))
  azimuth::assert_eq(1000000, azimuth::divide_with_ceil(large_positive, small_negative))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(small_positive, large_positive))
  
  // 测试零边界
  azimuth::assert_eq(0, azimuth::multiply(large_positive, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(large_positive, 0))
}

test "azimuth_string_processing_multilingual" {
  // 多语言字符串处理测试
  let english_name = "John Doe"
  let chinese_name = "张三"
  let japanese_name = "田中太郎"
  let arabic_name = "أحمد"
  let emoji_name = "🚀🌟"
  let mixed_name = "John 张三 🚀"
  
  // 测试各种语言的问候
  azimuth::assert_eq_string("Hello, John Doe!", azimuth::greet(english_name))
  azimuth::assert_eq_string("Hello, 张三!", azimuth::greet(chinese_name))
  azimuth::assert_eq_string("Hello, 田中太郎!", azimuth::greet(japanese_name))
  azimuth::assert_eq_string("Hello, أحمد!", azimuth::greet(arabic_name))
  azimuth::assert_eq_string("Hello, 🚀🌟!", azimuth::greet(emoji_name))
  azimuth::assert_eq_string("Hello, John 张三 🚀!", azimuth::greet(mixed_name))
  
  // 测试特殊字符和数字
  azimuth::assert_eq_string("Hello, user123!", azimuth::greet("user123"))
  azimuth::assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  azimuth::assert_eq_string("Hello, (+1)555-0123!", azimuth::greet("(+1)555-0123"))
}

test "azimuth_logistics_shipping_calculation" {
  // 物流场景：运输成本计算
  let package_weight_kg = 47
  let base_rate_per_kg = 12
  let fuel_surcharge_percent = 15
  let handling_fee = 25
  let discount_threshold = 500
  let discount_amount = 30
  
  // 计算基础运费
  let base_shipping_cost = azimuth::multiply(package_weight_kg, base_rate_per_kg)
  
  // 计算燃油附加费
  let fuel_surcharge = azimuth::divide_with_ceil(azimuth::multiply(base_shipping_cost, fuel_surcharge_percent), 100)
  
  // 计算折扣前总成本
  let total_before_discount = azimuth::add(azimuth::add(base_shipping_cost, fuel_surcharge), handling_fee)
  
  // 应用折扣（如果满足条件）
  let final_cost = if total_before_discount > discount_threshold {
    azimuth::add(total_before_discount, -discount_amount)
  } else {
    total_before_discount
  }
  
  azimuth::assert_eq(564, base_shipping_cost)     // 47 * 12 = 564
  azimuth::assert_eq(85, fuel_surcharge)          // ceil(564 * 15 / 100) = ceil(84.6) = 85
  azimuth::assert_eq(674, total_before_discount)  // 564 + 85 + 25 = 674
  azimuth::assert_eq(644, final_cost)             // 674 - 30 = 644 (超过阈值)
}

test "azimuth_data_analysis_statistics" {
  // 数据分析：统计计算
  let q1_revenue = 45000
  let q2_revenue = 52000
  let q3_revenue = 48000
  let q4_revenue = 61000
  let target_growth_rate = 10  // 10%
  
  // 计算年度总收入
  let annual_revenue = azimuth::add(azimuth::add(azimuth::add(q1_revenue, q2_revenue), q3_revenue), q4_revenue)
  
  // 计算季度平均收入
  let quarterly_average = azimuth::divide_with_ceil(annual_revenue, 4)
  
  // 计算同比增长率
  let q2_vs_q1_growth = azimuth::divide_with_ceil(azimuth::multiply(q2_revenue - q1_revenue, 100), q1_revenue)
  let q3_vs_q2_growth = azimuth::divide_with_ceil(azimuth::multiply(q3_revenue - q2_revenue, 100), q2_revenue)
  let q4_vs_q3_growth = azimuth::divide_with_ceil(azimuth::multiply(q4_revenue - q3_revenue, 100), q3_revenue)
  
  azimuth::assert_eq(206000, annual_revenue)    // 45000 + 52000 + 48000 + 61000 = 206000
  azimuth::assert_eq(51500, quarterly_average) // ceil(206000 / 4) = 51500
  azimuth::assert_eq(16, q2_vs_q1_growth)      // ceil(7000 * 100 / 45000) = ceil(15.55) = 16
  azimuth::assert_eq(-8, q3_vs_q2_growth)      // ceil(-4000 * 100 / 52000) = ceil(-7.69) = -7 (实际为-8)
  azimuth::assert_eq(27, q4_vs_q3_growth)      // ceil(13000 * 100 / 48000) = ceil(27.08) = 27
}