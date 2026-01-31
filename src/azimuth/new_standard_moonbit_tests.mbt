// 新增标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "azimuth_core_mathematical_operations" {
  // 测试核心数学运算组合
  let x = 12
  let y = 8
  let z = 5
  
  // 测试复合运算：(x + y) * z - x / y
  let complex_result = azimuth::add(azimuth::multiply(azimuth::add(x, y), z), -azimuth::divide_with_ceil(x, y))
  azimuth::assert_eq(98, complex_result)  // (12 + 8) * 5 - ceil(12/8) = 20*5 - 2 = 100 - 2 = 98
  
  // 测试分配律验证
  let distributive_left = azimuth::multiply(x, azimuth::add(y, z))
  let distributive_right = azimuth::add(azimuth::multiply(x, y), azimuth::multiply(x, z))
  azimuth::assert_eq(distributive_left, distributive_right)  // 12 * (8 + 5) = 12*8 + 12*5 = 156
}

test "azimuth_boundary_value_analysis" {
  // 边界值分析测试
  // 测试零值边界
  azimuth::assert_eq(0, azimuth::add(0, 0))
  azimuth::assert_eq(0, azimuth::multiply(0, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 1))
  
  // 测试单位边界
  azimuth::assert_eq(1, azimuth::add(0, 1))
  azimuth::assert_eq(1, azimuth::multiply(1, 1))
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 1))
  
  // 测试负值边界
  azimuth::assert_eq(-1, azimuth::add(0, -1))
  azimuth::assert_eq(-1, azimuth::multiply(1, -1))
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(-1, 1))
}

test "azimuth_string_processing_comprehensive" {
  // 综合字符串处理测试
  // 测试特殊字符和Unicode
  azimuth::assert_eq_string("Hello, 🌟!", azimuth::greet("🌟"))
  azimuth::assert_eq_string("Hello, 中文测试!", azimuth::greet("中文测试"))
  azimuth::assert_eq_string("Hello, a+b=c!", azimuth::greet("a+b=c"))
  azimuth::assert_eq_string("Hello, 100%!", azimuth::greet("100%"))
  
  // 测试边界字符串
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, A!", azimuth::greet("A"))
  azimuth::assert_eq_string("Hello,   !", azimuth::greet("  "))
}

test "azimuth_division_precision_testing" {
  // 除法精度测试
  // 测试各种除法情况的向上取整
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 2))    // 0.5 -> 1
  azimuth::assert_eq(2, azimuth::divide_with_ceil(3, 2))    // 1.5 -> 2
  azimuth::assert_eq(34, azimuth::divide_with_ceil(100, 3)) // 33.333... -> 34
  azimuth::assert_eq(101, azimuth::divide_with_ceil(201, 2)) // 100.5 -> 101
  
  // 测试负数除法
  azimuth::assert_eq(-1, azimuth::divide_with_ceil(-1, 2))   // -0.5 -> -1 (向上取整)
  azimuth::assert_eq(-2, azimuth::divide_with_ceil(-3, 2))   // -1.5 -> -2
  azimuth::assert_eq(3, azimuth::divide_with_ceil(-6, -2))   // 3 -> 3
}

test "azimuth_real_world_financial_calculations" {
  // 实际金融计算场景
  let principal = 10000
  let interest_rate = 5  // 5%
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = azimuth::add(principal, azimuth::divide_with_ceil(azimuth::multiply(principal, interest_rate), 100))
  let year2_amount = azimuth::add(year1_amount, azimuth::divide_with_ceil(azimuth::multiply(year1_amount, interest_rate), 100))
  let year3_amount = azimuth::add(year2_amount, azimuth::divide_with_ceil(azimuth::multiply(year2_amount, interest_rate), 100))
  
  azimuth::assert_eq(10500, year1_amount)  // 10000 + 500
  azimuth::assert_eq(11025, year2_amount)  // 10500 + 525
  azimuth::assert_eq(11576, year3_amount)  // 11025 + 551 (向上取整)
}

test "azimuth_inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let restock_threshold = 100
  let restock_amount = 300
  
  // 计算10天后的库存
  let total_sold = azimuth::multiply(daily_sales, 10)
  let current_stock = azimuth::add(initial_stock, -total_sold)
  
  // 判断是否需要补货
  let needs_restock = if current_stock <= restock_threshold { true } else { false }
  let final_stock = if needs_restock { azimuth::add(current_stock, restock_amount) } else { current_stock }
  
  azimuth::assert_eq(250, total_sold)
  azimuth::assert_eq(250, current_stock)
  azimuth::assert_eq(true, needs_restock)
  azimuth::assert_eq(550, final_stock)
}

test "azimuth_performance_benchmark_calculations" {
  // 性能基准计算
  let base_operations = 1000
  let optimization_factor = 4
  let overhead_percentage = 20
  
  // 计算优化后的性能
  let optimized_operations = azimuth::multiply(base_operations, optimization_factor)
  let overhead = azimuth::divide_with_ceil(azimuth::multiply(optimized_operations, overhead_percentage), 100)
  let actual_performance = azimuth::add(optimized_operations, -overhead)
  
  // 计算性能提升倍数
  let performance_gain = azimuth::divide_with_ceil(actual_performance, base_operations)
  
  azimuth::assert_eq(4000, optimized_operations)
  azimuth::assert_eq(800, overhead)
  azimuth::assert_eq(3200, actual_performance)
  azimuth::assert_eq(3, performance_gain)
}

test "azimuth_error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除零安全处理
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(-100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
  
  // 测试极值运算
  let large_positive = 10000
  let large_negative = -10000
  azimuth::assert_eq(0, azimuth::add(large_positive, large_negative))
  azimuth::assert_eq(100000000, azimuth::multiply(large_positive, large_positive))
  
  // 测试字符串边界情况
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
  azimuth::assert_eq_string("Hello, x!", azimuth::greet("x"))
}

test "azimuth_data_analysis_statistics" {
  // 数据分析统计测试
  let data_point1 = 15
  let data_point2 = 25
  let data_point3 = 35
  let data_point4 = 45
  let data_point5 = 55
  
  // 计算总和（使用现有函数）
  let sum = azimuth::add(azimuth::add(azimuth::add(azimuth::add(data_point1, data_point2), data_point3), data_point4), data_point5)
  
  // 计算平均值（向上取整）
  let average = azimuth::divide_with_ceil(sum, 5)
  
  // 计算最大值和最小值的差
  let range = azimuth::subtract(data_point5, data_point1)
  
  azimuth::assert_eq(175, sum)
  azimuth::assert_eq(35, average)
  azimuth::assert_eq(40, range)
}

test "azimuth_complex_workflow_simulation" {
  // 复杂工作流程模拟
  let project_tasks = 25
  let team_members = 4
  let working_days = 5
  let tasks_per_person_per_day = 3
  
  // 计算总工作容量
  let daily_capacity = azimuth::multiply(team_members, tasks_per_person_per_day)
  let weekly_capacity = azimuth::multiply(daily_capacity, working_days)
  
  // 计算需要的周数
  let weeks_needed = azimuth::divide_with_ceil(project_tasks, weekly_capacity)
  
  // 计算实际完成天数
  let total_days_needed = azimuth::multiply(weeks_needed, working_days)
  
  azimuth::assert_eq(12, daily_capacity)
  azimuth::assert_eq(60, weekly_capacity)
  azimuth::assert_eq(1, weeks_needed)
  azimuth::assert_eq(5, total_days_needed)
}