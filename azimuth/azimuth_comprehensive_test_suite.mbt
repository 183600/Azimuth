// Azimuth 综合测试套件 - 10个标准 MoonBit 测试用例
// 覆盖核心功能的各种使用场景和边界情况

test "arithmetc_sequence_operations" {
  // 测试算术序列操作
  let start = 5
  let difference = 3
  let count = 4
  
  // 计算等差数列的和
  let term1 = start
  let term2 = add(term1, difference)
  let term3 = add(term2, difference)
  let term4 = add(term3, difference)
  
  let sequence_sum = add(add(add(term1, term2), term3), term4)
  assert_eq(32, sequence_sum)  // 5 + 8 + 11 + 14 = 38
  
  // 验证乘法关系
  let doubled_sum = multiply(sequence_sum, 2)
  assert_eq(64, doubled_sum)
}

test "resource_allocation_efficiency" {
  // 测试资源分配效率计算
  let total_resources = 120
  let teams = 5
  let optimal_team_size = divide_with_ceil(total_resources, teams)
  
  assert_eq(24, optimal_team_size)
  
  // 计算实际分配的资源
  let allocated_resources = multiply(optimal_team_size, teams)
  assert_eq(120, allocated_resources)
  
  // 计算资源利用率
  let efficiency_percentage = divide_with_ceil(multiply(allocated_resources, 100), multiply(total_resources, 1))
  assert_eq(100, efficiency_percentage)
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, single!", greet("single"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
  
  // 测试包含特殊字符的字符串
  let special_input = "test@example.com"
  let result = greet(special_input)
  assert_eq_string("Hello, test@example.com!", result)
}

test "mathematical_inverse_operations" {
  // 测试数学逆运算
  let original = 42
  
  // 加法和减法的逆运算
  let added = add(original, 15)
  let subtracted = subtract(added, 15)
  assert_eq(original, subtracted)
  
  // 乘法和除法的逆运算（在能整除的情况下）
  let multiplied = multiply(original, 3)
  let divided = divide_with_ceil(multiplied, 3)
  assert_eq(original, divided)
}

test "batch_processing_workflow" {
  // 测试批处理工作流
  let items_per_batch = 25
  let total_items = 188
  let batches_needed = divide_with_ceil(total_items, items_per_batch)
  
  assert_eq(8, batches_needed)
  
  // 计算每批次的处理时间（假设每项处理时间为2单位）
  let time_per_item = 2
  let batch_time = multiply(items_per_batch, time_per_item)
  assert_eq(50, batch_time)
  
  // 计算总处理时间
  let total_time = multiply(batch_time, batches_needed)
  assert_eq(400, total_time)
  
  // 计算最后一批次的项目数
  let processed_items = multiply(batches_needed - 1, items_per_batch)
  let last_batch_items = subtract(total_items, processed_items)
  assert_eq(13, last_batch_items)
}

test "financial_calculation_precision" {
  // 测试金融计算精度
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 计算简单利息
  let annual_interest = divide_with_ceil(multiply(principal, rate_percent), 100)
  let total_interest = multiply(annual_interest, years)
  let final_amount = add(principal, total_interest)
  
  assert_eq(500, annual_interest)
  assert_eq(1500, total_interest)
  assert_eq(11500, final_amount)
  
  // 计算月度分期
  let months = multiply(years, 12)
  let monthly_payment = divide_with_ceil(final_amount, months)
  assert_eq(320, monthly_payment)  // ceil(11500/36) = 320
}

test "inventory_optimization_scenario" {
  // 测试库存优化场景
  let demand_forecast = 450
  let storage_capacity = 80
  let safety_stock = 50
  
  // 计算需要的存储单元数
  let total_needed = add(demand_forecast, safety_stock)
  let storage_units = divide_with_ceil(total_needed, storage_capacity)
  
  assert_eq(7, storage_units)
  
  // 计算实际存储容量
  let actual_capacity = multiply(storage_units, storage_capacity)
  assert_eq(560, actual_capacity)
  
  // 计算剩余容量
  let remaining_capacity = subtract(actual_capacity, total_needed)
  assert_eq(60, remaining_capacity)
  
  // 计算空间利用率
  let utilization_rate = divide_with_ceil(multiply(total_needed, 100), actual_capacity)
  assert_eq(89, utilization_rate)  // floor(50000/560) = 89%
}

test "complex_number_theory_properties" {
  // 测试复杂数论性质
  let prime1 = 7
  let prime2 = 13
  let composite = 21
  
  // 测试质数乘积的性质
  let product = multiply(prime1, prime2)
  assert_eq(91, product)
  
  // 测试因数分解
  let factor1 = divide_with_ceil(composite, prime1)
  let factor2 = divide_with_ceil(composite, 7)
  assert_eq(3, factor1)
  assert_eq(3, factor2)
  
  // 测试乘法交换律的推广
  let triple_product = multiply(multiply(prime1, prime2), 2)
  assert_eq(182, triple_product)
  
  // 测试结合律
  let associative1 = multiply(prime1, multiply(prime2, 2))
  let associative2 = multiply(multiply(prime1, prime2), 2)
  assert_eq(associative1, associative2)
}

test "time_management_productivity" {
  // 测试时间管理生产力计算
  let daily_hours = 8
  let productive_percentage = 75
  let tasks_per_productive_hour = 4
  let workdays = 5
  
  // 计算每日有效工作时间
  let productive_hours = divide_with_ceil(multiply(daily_hours, productive_percentage), 100)
  assert_eq(6, productive_hours)
  
  // 计算每日完成任务数
  let daily_tasks = multiply(productive_hours, tasks_per_productive_hour)
  assert_eq(24, daily_tasks)
  
  // 计算每周完成任务数
  let weekly_tasks = multiply(daily_tasks, workdays)
  assert_eq(120, weekly_tasks)
  
  // 计算项目总任务数下的工作周数
  let project_tasks = 360
  let weeks_needed = divide_with_ceil(project_tasks, weekly_tasks)
  assert_eq(3, weeks_needed)
}

test "error_boundary_validation" {
  // 测试错误边界验证
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值运算测试
  let max_int = 2147483647
  let min_int = -2147483648
  
  // 测试加法边界
  let safe_add = add(1000000, 1000000)
  assert_eq(2000000, safe_add)
  
  // 测试乘法边界（安全范围内）
  let safe_multiply = multiply(46340, 46340)  // sqrt(INT_MAX) ≈ 46340
  assert_eq(2147395600, safe_multiply)
  
  // 测试减法边界
  let safe_subtract = subtract(1000000, 500000)
  assert_eq(500000, safe_subtract)
  
  // 测试字符串边界
  let very_long_name = "A"
  let repeated_name = very_long_name + "VeryLongUserNameThatTestsBoundaryConditions"
  let greeting_result = greet(repeated_name)
  assert_true(String.length(greeting_result) > String.length(repeated_name))
}