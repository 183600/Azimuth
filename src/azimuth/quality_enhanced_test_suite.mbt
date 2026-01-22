// 新增的高质量 MoonBit 测试用例
// 覆盖边界情况、数学性质和实际应用场景

test "extreme_value_calculations" {
  // 测试极端数值的计算
  let large_num = 1000000
  let small_num = 1
  
  // 大数运算
  assert_eq(1000001, add(large_num, small_num))
  assert_eq(1000000, multiply(large_num, small_num))
  
  // 极端除法情况
  assert_eq(1, divide_with_ceil(small_num, large_num))
  assert_eq(large_num, divide_with_ceil(large_num, small_num))
}

test "mathematical_identities" {
  // 验证数学恒等式
  let x = 42
  let y = 17
  
  // 加法恒等式
  assert_eq(add(x, y), add(y, x))  // 交换律
  assert_eq(add(add(x, y), 0), add(x, y))  // 单位元
  
  // 乘法恒等式
  assert_eq(multiply(x, y), multiply(y, x))  // 交换律
  assert_eq(multiply(multiply(x, y), 1), multiply(x, y))  // 单位元
  
  // 分配律验证
  assert_eq(multiply(x, add(y, 1)), add(multiply(x, y), multiply(x, 1)))
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, !", greet(""))  // 空字符串
  assert_eq_string("Hello,   !", greet("  "))  // 空格字符串
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello, 12345!", greet("12345"))  // 数字字符串
  assert_eq_string("Hello, aBcDeF!", greet("aBcDeF"))  // 大小写混合
}

test "complex_nested_operations" {
  // 测试复杂的嵌套运算
  let base = 10
  let multiplier = 3
  let divisor = 4
  let offset = 7
  
  // 嵌套计算：((base * multiplier) / divisor) + offset
  let step1 = multiply(base, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, offset)
  
  assert_eq(30, step1)  // 10 * 3 = 30
  assert_eq(8, step2)   // ceil(30/4) = 8
  assert_eq(15, result) // 8 + 7 = 15
}

test "real_world_financial_calculation" {
  // 实际金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(10500, year1_amount)  // 10000 + ceil(10000*5/100) = 10000 + 500
  assert_eq(11025, year2_amount)  // 10500 + ceil(10500*5/100) = 10500 + 525
  assert_eq(11576, year3_amount)  // 11025 + ceil(11025*5/100) = 11025 + 551
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 800
  let project_c_cost = 1500
  
  // 计算剩余预算
  let allocated = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining = add(total_budget, -allocated)
  
  // 计算可以启动的小项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining, small_project_cost)
  
  assert_eq(3500, allocated)  // 1200 + 800 + 1500 = 3500
  assert_eq(1500, remaining)  // 5000 - 3500 = 1500
  assert_eq(5, additional_projects)  // ceil(1500/300) = 5
}

test "time_management_scenario" {
  // 时间管理场景计算
  let daily_work_hours = 8
  let tasks_per_hour = 2
  let total_tasks = 85
  
  // 计算需要的总工作小时数
  let total_hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  
  // 计算需要的工作天数
  let workdays_needed = divide_with_ceil(total_hours_needed, daily_work_hours)
  
  assert_eq(43, total_hours_needed)  // ceil(85/2) = 43
  assert_eq(6, workdays_needed)     // ceil(43/8) = 6
}

test "inventory_logistics_calculation" {
  // 库存物流计算
  let warehouse_capacity = 1000
  let current_stock = 750
  let incoming_shipment = 300
  let items_per_pallet = 25
  
  // 计算接收后的总库存
  let total_after_receiving = add(current_stock, incoming_shipment)
  
  // 计算需要的托盘数量
  let pallets_needed = divide_with_ceil(total_after_receiving, items_per_pallet)
  
  // 计算是否超过仓库容量
  let exceeds_capacity = total_after_receiving > warehouse_capacity
  
  assert_eq(1050, total_after_receiving)  // 750 + 300 = 1050
  assert_eq(42, pallets_needed)          // ceil(1050/25) = 42
  assert_eq(true, exceeds_capacity)      // 1050 > 1000
}

test "data_analysis_statistics" {
  // 数据分析统计计算
  let data_points = [15, 22, 18, 25, 20]  // 模拟数据点
  let sum_first_two = add(data_points[0], data_points[1])
  let sum_next_two = add(data_points[2], data_points[3])
  let sum_all_five = add(add(sum_first_two, sum_next_two), data_points[4])
  
  // 计算平均值（向上取整）
  let average = divide_with_ceil(sum_all_five, 5)
  
  // 计算最大值与最小值的差
  let max_val = 25
  let min_val = 15
  let range = add(max_val, -min_val)
  
  assert_eq(37, sum_first_two)    // 15 + 22 = 37
  assert_eq(43, sum_next_two)     // 18 + 25 = 43
  assert_eq(100, sum_all_five)    // 37 + 43 + 20 = 100
  assert_eq(20, average)          // ceil(100/5) = 20
  assert_eq(10, range)            // 25 - 15 = 10
}

test "engineering_precision_calculation" {
  // 工程精度计算测试
  let measurement = 100
  let tolerance_percent = 5
  let adjustment_factor = 2
  
  // 计算容差范围
  let tolerance = divide_with_ceil(multiply(measurement, tolerance_percent), 100)
  
  // 计算调整后的测量值
  let adjusted_measurement = add(measurement, multiply(adjustment_factor, tolerance))
  
  // 计算最终验证值
  let final_value = divide_with_ceil(multiply(adjusted_measurement, 3), 4)
  
  assert_eq(5, tolerance)                    // ceil(100*5/100) = 5
  assert_eq(110, adjusted_measurement)       // 100 + 2*5 = 110
  assert_eq(83, final_value)                 // ceil(110*3/4) = ceil(330/4) = 83
}