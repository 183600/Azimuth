// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法

test "azimuth_core_arithmetic_precision" {
  // 测试核心算术运算的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(34, divide_with_ceil(100, 3))
  assert_eq(67, divide_with_ceil(200, 3))
  assert_eq(0, divide_with_ceil(0, 999))
}

test "azimuth_string_boundary_handling" {
  // 测试字符串边界处理
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, very long name test!", greet("very long name test"))
  assert_eq_string("Hello, 123456789!", greet("123456789"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
}

test "azimuth_business_resource_allocation" {
  // 业务场景：资源分配计算
  let team_size = 12
  let projects = 5
  let min_resources_per_project = divide_with_ceil(team_size, projects)
  
  assert_eq(3, min_resources_per_project)
  
  // 验证总分配资源
  let total_allocated = multiply(min_resources_per_project, projects)
  assert_eq(15, total_allocated)
  
  // 计算剩余资源
  let remaining_resources = subtract(team_size, subtract(total_allocated, min_resources_per_project))
  assert_eq(9, remaining_resources)
}

test "azimuth_financial_compound_interest" {
  // 金融场景：复合利息计算
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10800, year1)
  assert_eq(11664, year2)
  assert_eq(12597, year3)
}

test "azimuth_logistics_shipping_optimization" {
  // 物流场景：运输优化计算
  let total_packages = 127
  let truck_capacity = 24
  let trucks_needed = divide_with_ceil(total_packages, truck_capacity)
  
  assert_eq(6, trucks_needed)
  
  // 计算总运力
  let total_capacity = multiply(trucks_needed, truck_capacity)
  assert_eq(144, total_capacity)
  
  // 计算剩余空间
  let unused_capacity = subtract(total_capacity, total_packages)
  assert_eq(17, unused_capacity)
  
  // 计算空间利用率
  let utilization_rate = divide_with_ceil(multiply(total_packages, 100), total_capacity)
  assert_eq(89, utilization_rate)
}

test "azimuth_time_management_task_scheduling" {
  // 时间管理：任务调度计算
  let total_tasks = 89
  let working_days = 15
  let tasks_per_day_needed = divide_with_ceil(total_tasks, working_days)
  
  assert_eq(6, tasks_per_day_needed)
  
  // 计算实际可处理任务数
  let actual_capacity = multiply(tasks_per_day_needed, working_days)
  assert_eq(90, actual_capacity)
  
  // 计算缓冲任务数
  let buffer_tasks = subtract(actual_capacity, total_tasks)
  assert_eq(1, buffer_tasks)
}

test "azimuth_data_processing_batch_calculation" {
  // 数据处理：批次计算
  let total_records = 1047
  let batch_size = 100
  let full_batches = divide_with_ceil(total_records, batch_size)
  
  assert_eq(11, full_batches)
  
  // 计算最后一批的记录数
  let last_batch_size = subtract(total_records, multiply(batch_size, subtract(full_batches, 1)))
  assert_eq(47, last_batch_size)
  
  // 验证总记录数
  let verified_total = add(multiply(batch_size, subtract(full_batches, 1)), last_batch_size)
  assert_eq(1047, verified_total)
}

test "azimuth_energy_consumption_analysis" {
  // 能耗分析计算
  let daily_consumption = 450
  let days_in_month = 30
  let peak_hours_multiplier = 15
  
  // 计算月度基本消耗
  let monthly_base = multiply(daily_consumption, days_in_month)
  assert_eq(13500, monthly_base)
  
  // 计算峰值消耗
  let peak_consumption = divide_with_ceil(multiply(monthly_base, peak_hours_multiplier), 100)
  assert_eq(2025, peak_consumption)
  
  // 计算总消耗
  let total_consumption = add(monthly_base, peak_consumption)
  assert_eq(15525, total_consumption)
}

test "azimuth_combinatorial_mathematics" {
  // 组合数学计算
  let set_size = 8
  let subset_size = 3
  
  // 简化的组合计算 C(8,3) = 8!/(3!*(8-3)!) = 56
  // 使用乘法模拟
  let combinations = divide_with_ceil(multiply(set_size, multiply(subtract(set_size, 1), subtract(set_size, 2))), multiply(subset_size, multiply(subtract(subset_size, 1), subtract(subset_size, 2))))
  
  assert_eq(56, combinations)
  
  // 验证排列数 P(8,3) = 8*7*6 = 336
  let permutations = multiply(set_size, multiply(subtract(set_size, 1), subtract(set_size, 2)))
  assert_eq(336, permutations)
}

test "azimuth_error_boundary_robustness" {
  // 错误边界健壮性测试
  // 除零情况全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极值运算测试
  let max_test = 1000000
  let min_test = -1000000
  
  assert_eq(0, add(max_test, -max_test))
  assert_eq(0, add(min_test, -min_test))
  assert_eq(2000000, add(max_test, max_test))
  assert_eq(-2000000, add(min_test, min_test))
  assert_eq(1000000000000, multiply(max_test, max_test))
}