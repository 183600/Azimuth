// 增强 MoonBit 测试用例
// 针对 azimuth 库的高级功能和边界情况进行测试

test "advanced_arithmetic_operations" {
  // 测试高级算术运算
  let x = 15
  let y = 7
  let z = 3
  
  // 测试复合表达式：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(63, result)  // (15 + 7) * 3 - ceil(15/7) = 22*3 - 3 = 66 - 3 = 63
  
  // 测试嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 2), 5)
  assert_eq(10, nested)  // ceil((15 + 7*3) * 2 / 5) = ceil(36 * 2 / 5) = ceil(72/5) = 15
}

test "boundary_value_testing" {
  // 边界值测试
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  
  // 测试接近零的运算
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(0, divide_with_ceil(0, 1000000))
  
  // 测试负数边界
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(-1000, -1000))
}

test "financial_calculations" {
  // 金融计算场景
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 计算复利
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10800, year1)
  assert_eq(11664, year2)
  assert_eq(12597, year3)
}

test "inventory_optimization" {
  // 库存优化场景
  let demand_forecast = 450
  let production_capacity = 80
  let safety_stock = 50
  
  // 计算生产批次数
  let production_batches = divide_with_ceil(demand_forecast, production_capacity)
  
  // 计算总生产量
  let total_production = multiply(production_batches, production_capacity)
  
  // 计算最终库存
  let final_inventory = add(total_production, safety_stock)
  
  assert_eq(6, production_batches)
  assert_eq(480, total_production)
  assert_eq(530, final_inventory)
}

test "resource_allocation" {
  // 资源分配场景
  let total_budget = 50000
  let project_a_cost = 15000
  let project_b_cost = 12000
  let project_c_cost = 8000
  
  // 计算已分配预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 3000
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(35000, allocated_budget)
  assert_eq(15000, remaining_budget)
  assert_eq(5, small_projects_count)
}

test "data_processing_pipeline" {
  // 数据处理管道场景
  let data_points = 1000
  let processing_rate = 75
  let batch_size = 20
  
  // 计算处理批次
  let processing_batches = divide_with_ceil(data_points, batch_size)
  
  // 计算总处理时间（分钟）
  let total_time = divide_with_ceil(multiply(processing_batches, processing_rate), 60)
  
  // 计算每小时处理量
  let hourly_rate = divide_with_ceil(multiply(processing_rate, 60), 60)
  
  assert_eq(50, processing_batches)
  assert_eq(63, total_time)
  assert_eq(75, hourly_rate)
}

test "string_processing_advanced" {
  // 高级字符串处理
  let first_name = "张"
  let last_name = "三"
  let title = "工程师"
  let company = "科技公司"
  
  // 构建完整名片
  let full_name = first_name + last_name
  let business_title = full_name + " - " + title
  let business_card = business_title + " @ " + company
  
  let greeting_result = greet(business_card)
  
  assert_eq_string("Hello, 张三 - 工程师 @ 科技公司!", greeting_result)
  assert_eq_string("Hello, JSON-API-v2.0!", greet("JSON-API-v2.0"))
  assert_eq_string("Hello, C++/Java/Python!", greet("C++/Java/Python"))
}

test "mathematical_series" {
  // 数学级数计算
  let n = 10
  
  // 计算等差数列和：1 + 3 + 5 + ... + (2n-1) = n²
  let arithmetic_sum = multiply(n, n)
  
  // 计算等比数列和：2 + 4 + 8 + ... + 2^n
  let geometric_sum = subtract(multiply(2, multiply(2, n)), 2)
  
  // 验证结果
  assert_eq(100, arithmetic_sum)
  assert_eq(2046, geometric_sum)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的各种情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 测试极端数值组合
  let extreme_result = add(multiply(100000, 10000), divide_with_ceil(-500000, 1000))
  assert_eq(999999500, extreme_result)
}

test "performance_benchmarking" {
  // 性能基准测试场景
  let base_operations = 10000
  let optimization_factor = 4
  let overhead_percentage = 20
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算系统开销
  let system_overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -system_overhead)
  
  // 计算性能提升倍数
  let performance_improvement = divide_with_ceil(actual_performance, base_operations)
  
  assert_eq(40000, optimized_operations)
  assert_eq(8000, system_overhead)
  assert_eq(32000, actual_performance)
  assert_eq(4, performance_improvement)
}