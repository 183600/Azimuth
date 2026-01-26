// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

// 导入 azimuth 库
use azimuth

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  assert_eq(42, azimuth.add(15, 27))
  assert_eq(42, azimuth.add(27, 15))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)  // 5 * (8 + 12) = 5 * 20 = 100
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 全面测试向上取整除法的各种场景
  // 正数除法
  assert_eq(4, azimuth.divide_with_ceil(10, 3))   // 10/3 = 3.333... -> 4
  assert_eq(3, azimuth.divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(1, azimuth.divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  
  // 负数除法
  assert_eq(-3, azimuth.divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(3, azimuth.divide_with_ceil(10, -3))   // 10/-3 = -3.333... -> -3
  assert_eq(4, azimuth.divide_with_ceil(-10, -3))  // -10/-3 = 3.333... -> 4
  
  // 边界情况
  assert_eq(0, azimuth.divide_with_ceil(5, 0))     // 除数为0，返回0
  assert_eq(0, azimuth.divide_with_ceil(0, 5))     // 0除以任何数，返回0
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", azimuth.greet("世界"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
  assert_eq_string("Hello, Español!", azimuth.greet("Español"))
  assert_eq_string("Hello, Русский!", azimuth.greet("Русский"))
  assert_eq_string("Hello, العربية!", azimuth.greet("العربية"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景：计算订单总价
  let unit_price = 89
  let quantity = 6
  let tax_rate = 8
  let shipping_cost = 25
  let discount = 50
  
  // 计算小计
  let subtotal = azimuth.multiply(unit_price, quantity)
  
  // 计算税费
  let tax_amount = azimuth.divide_with_ceil(azimuth.multiply(subtotal, tax_rate), 100)
  
  // 计算总价（含运费和折扣）
  let total_before_discount = azimuth.add(azimuth.add(subtotal, tax_amount), shipping_cost)
  let final_total = azimuth.add(total_before_discount, -discount)
  
  assert_eq(534, subtotal)      // 89 * 6 = 534
  assert_eq(43, tax_amount)     // ceil(534 * 8 / 100) = ceil(42.72) = 43
  assert_eq(602, total_before_discount)  // 534 + 43 + 25 = 602
  assert_eq(552, final_total)   // 602 - 50 = 552
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列和等比数列
  // 等差数列：2, 5, 8, 11, 14 的和
  let arithmetic_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(2, 5), 8), 11), 14)
  
  // 等比数列：2, 4, 8, 16, 32 的和
  let geometric_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(2, 4), 8), 16), 32)
  
  assert_eq(40, arithmetic_sum)   // 2 + 5 + 8 + 11 + 14 = 40
  assert_eq(62, geometric_sum)   // 2 + 4 + 8 + 16 + 32 = 62
}

test "resource_allocation_optimization" {
  // 资源分配优化问题
  let total_budget = 10000
  let project_a_cost = 3200
  let project_b_cost = 2800
  let project_c_cost = 2100
  
  // 计算剩余预算
  let used_budget = azimuth.add(azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = azimuth.add(total_budget, -used_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本500）
  let small_projects_count = azimuth.divide_with_ceil(remaining_budget, 500)
  
  assert_eq(8100, used_budget)           // 3200 + 2800 + 2100 = 8100
  assert_eq(1900, remaining_budget)      // 10000 - 8100 = 1900
  assert_eq(4, small_projects_count)     // ceil(1900 / 500) = ceil(3.8) = 4
}

test "time_and_work_calculation" {
  // 时间与工作量计算
  let total_tasks = 87
  let workers_count = 4
  let tasks_per_worker_per_day = 6
  let working_days = 5
  
  // 计算每个工人的任务量
  let tasks_per_worker = azimuth.divide_with_ceil(total_tasks, workers_count)
  
  // 计算完成时间（天数）
  let days_needed = azimuth.divide_with_ceil(tasks_per_worker, tasks_per_worker_per_day)
  
  // 计算总工作日（考虑周末）
  let total_calendar_days = azimuth.multiply(days_needed, 2)  // 假设工作日和休息日1:1
  
  assert_eq(22, tasks_per_worker)        // ceil(87 / 4) = ceil(21.75) = 22
  assert_eq(4, days_needed)              // ceil(22 / 6) = ceil(3.666...) = 4
  assert_eq(8, total_calendar_days)      // 4 * 2 = 8
}

test "data_analysis_statistics" {
  // 数据分析统计计算
  let data_points = [15, 23, 31, 42, 38, 27, 19]  // 模拟数据点
  let data_count = 7
  
  // 计算总和（手动相加）
  let total_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(15, 23), 31), 42), 38), 27), 19)
  
  // 计算平均值（向上取整）
  let average = azimuth.divide_with_ceil(total_sum, data_count)
  
  // 计算最大值和最小值的差
  let max_value = 42
  let min_value = 15
  let range = azimuth.add(max_value, -min_value)
  
  assert_eq(195, total_sum)     // 15 + 23 + 31 + 42 + 38 + 27 + 19 = 195
  assert_eq(28, average)        // ceil(195 / 7) = ceil(27.857...) = 28
  assert_eq(27, range)          // 42 - 15 = 27
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 测试大数运算
  let large_num1 = 100000
  let large_num2 = 200000
  let large_sum = azimuth.add(large_num1, large_num2)
  let large_product = azimuth.multiply(1000, 1000)
  
  // 测试零值运算
  let zero_addition = azimuth.add(0, 12345)
  let zero_multiplication = azimuth.multiply(0, 12345)
  let zero_division = azimuth.divide_with_ceil(0, 12345)
  
  // 测试负数运算
  let negative_addition = azimuth.add(-100, 50)
  let negative_division = azimuth.divide_with_ceil(-25, 4)
  
  assert_eq(300000, large_sum)           // 100000 + 200000 = 300000
  assert_eq(1000000, large_product)     // 1000 * 1000 = 1000000
  assert_eq(12345, zero_addition)       // 0 + 12345 = 12345
  assert_eq(0, zero_multiplication)     // 0 * 12345 = 0
  assert_eq(0, zero_division)           // 0 / 12345 = 0
  assert_eq(-50, negative_addition)     // -100 + 50 = -50
  assert_eq(-6, negative_division)      // ceil(-25 / 4) = ceil(-6.25) = -6
}