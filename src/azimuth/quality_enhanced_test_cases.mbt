// 高质量增强测试用例 - 针对 azimuth 库的核心功能
// 使用标准 MoonBit 测试语法

test "enhanced_addition_commutativity" {
  // 验证加法交换律：a + b = b + a
  let a = 42
  let b = 17
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 测试负数情况
  let x = -15
  let y = 8
  azimuth::assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 测试零值情况
  azimuth::assert_eq(azimuth::add(0, 25), azimuth::add(25, 0))
}

test "enhanced_multiplication_properties" {
  // 验证乘法性质
  let a = 7
  let b = 6
  
  // 交换律：a * b = b * a
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 零元素性质：a * 0 = 0
  azimuth::assert_eq(0, azimuth::multiply(a, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, b))
  
  // 单位元性质：a * 1 = a
  azimuth::assert_eq(a, azimuth::multiply(a, 1))
  azimuth::assert_eq(b, azimuth::multiply(1, b))
}

test "enhanced_distributive_property" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left_side, right_side)
}

test "enhanced_divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  // 正数情况
  azimuth::assert_eq(4, azimuth::divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数情况
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 边界情况
  azimuth::assert_eq(0, azimuth::divide_with_ceil(5, 0))     // 除数为零
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 5))     // 被除数为零
}

test "enhanced_greet_function_edge_cases" {
  // 测试问候函数的边界情况
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))           // 空字符串
  azimuth::assert_eq_string("Hello, A!", azimuth::greet("A"))         // 单字符
  azimuth::assert_eq_string("Hello,   !", azimuth::greet("  "))       // 空格字符串
  
  // 特殊字符测试
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, @#$!", azimuth::greet("@#$"))
}

test "enhanced_complex_business_calculations" {
  // 复杂业务计算场景
  let unit_price = 85
  let quantity = 12
  let tax_rate = 8
  let shipping = 25
  let discount = 50
  
  // 计算小计
  let subtotal = azimuth::multiply(unit_price, quantity)
  
  // 计算税费
  let tax = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  
  // 计算折扣前总价
  let total_before_discount = azimuth::add(azimuth::add(subtotal, tax), shipping)
  
  // 应用折扣
  let final_total = azimuth::add(total_before_discount, -discount)
  
  azimuth::assert_eq(1020, subtotal)
  azimuth::assert_eq(82, tax)
  azimuth::assert_eq(1127, total_before_discount)
  azimuth::assert_eq(1077, final_total)
}

test "enhanced_resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算已分配预算
  let allocated_budget = azimuth::add(azimuth::add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = azimuth::add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本200）
  let small_projects = azimuth::divide_with_ceil(remaining_budget, 200)
  
  azimuth::assert_eq(3900, allocated_budget)
  azimuth::assert_eq(1100, remaining_budget)
  azimuth::assert_eq(6, small_projects)
}

test "enhanced_time_management_calculations" {
  // 时间管理计算测试
  let tasks_per_day = 7
  let total_tasks = 45
  let _overtime_rate = 15  // 修改变量名以避免未使用警告
  
  // 计算需要的工作日
  let workdays_needed = azimuth::divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算总工作小时数（每天8小时）
  let total_hours = azimuth::multiply(workdays_needed, 8)
  
  // 计算可能需要的加班小时数
  let regular_hours = azimuth::multiply(total_tasks, 8) / tasks_per_day
  let overtime_hours = azimuth::add(total_hours, -regular_hours)
  
  azimuth::assert_eq(7, workdays_needed)
  azimuth::assert_eq(56, total_hours)
  azimuth::assert_eq(6, overtime_hours)
}

test "enhanced_data_processing_efficiency" {
  // 数据处理效率测试
  let data_size_mb = 850
  let processing_rate_mb_per_hour = 120
  let buffer_overhead_percentage = 10
  
  // 计算基础处理时间
  let base_hours = azimuth::divide_with_ceil(data_size_mb, processing_rate_mb_per_hour)
  
  // 计算缓冲开销时间
  let overhead_hours = azimuth::divide_with_ceil(azimuth::multiply(base_hours, buffer_overhead_percentage), 100)
  
  // 计算总处理时间
  let total_hours = azimuth::add(base_hours, overhead_hours)
  
  azimuth::assert_eq(8, base_hours)
  azimuth::assert_eq(1, overhead_hours)
  azimuth::assert_eq(9, total_hours)
}

test "enhanced_financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000
  let annual_rate_percent = 6
  let _years = 3  // 修改变量名以避免未使用警告
  
  // 第一年
  let year1_interest = azimuth::divide_with_ceil(azimuth::multiply(principal, annual_rate_percent), 100)
  let year1_total = azimuth::add(principal, year1_interest)
  
  // 第二年
  let year2_interest = azimuth::divide_with_ceil(azimuth::multiply(year1_total, annual_rate_percent), 100)
  let year2_total = azimuth::add(year1_total, year2_interest)
  
  // 第三年
  let year3_interest = azimuth::divide_with_ceil(azimuth::multiply(year2_total, annual_rate_percent), 100)
  let year3_total = azimuth::add(year2_total, year3_interest)
  
  azimuth::assert_eq(600, year1_interest)
  azimuth::assert_eq(10600, year1_total)
  azimuth::assert_eq(636, year2_interest)
  azimuth::assert_eq(11236, year2_total)
  azimuth::assert_eq(675, year3_interest)
  azimuth::assert_eq(11911, year3_total)
}