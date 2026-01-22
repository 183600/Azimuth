// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行深度测试

// 导入 azimuth 库
use src.azimuth

test "mathematical_identity_verification" {
  // 验证数学恒等式
  let a = 42
  let b = 17
  
  // 加法恒等式：a + 0 = a
  assert_eq(a, azimuth.add(a, 0))
  assert_eq(b, azimuth.add(b, 0))
  
  // 乘法恒等式：a * 1 = a
  assert_eq(a, azimuth.multiply(a, 1))
  assert_eq(b, azimuth.multiply(b, 1))
  
  // 零乘法性质：a * 0 = 0
  assert_eq(0, azimuth.multiply(a, 0))
  assert_eq(0, azimuth.multiply(b, 0))
}

test "complex_financial_calculation" {
  // 复杂的金融计算场景
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 简单利息计算（使用向上取整）
  let yearly_interest = azimuth.divide_with_ceil(azimuth.multiply(principal, rate_percent), 100)
  let total_interest = azimuth.multiply(yearly_interest, years)
  let final_amount = azimuth.add(principal, total_interest)
  
  assert_eq(800, yearly_interest)
  assert_eq(2400, total_interest)
  assert_eq(12400, final_amount)
}

test "inventory_optimization_problem" {
  // 库存优化问题
  let demand_forecast = 157
  let package_size = 24
  let safety_stock_percentage = 20
  
  // 计算基础包装数量
  let base_packages = azimuth.divide_with_ceil(demand_forecast, package_size)
  
  // 计算安全库存
  let safety_stock = azimuth.divide_with_ceil(azimuth.multiply(demand_forecast, safety_stock_percentage), 100)
  
  // 计算总包装数量（包含安全库存）
  let total_demand = azimuth.add(demand_forecast, safety_stock)
  let total_packages = azimuth.divide_with_ceil(total_demand, package_size)
  
  assert_eq(7, base_packages)
  assert_eq(32, safety_stock)
  assert_eq(8, total_packages)
}

test "string_processing_pipeline" {
  // 字符串处理流水线
  let prefix = "Dear"
  let name = "MoonBit"
  let suffix = "Developer"
  
  // 构建完整称呼
  let full_title = prefix + " " + name + " " + suffix
  let formal_greeting = azimuth.greet(full_title)
  
  assert_eq_string("Hello, Dear MoonBit Developer!", formal_greeting)
  
  // 测试特殊字符处理
  let special_name = "用户@123"
  let special_greeting = azimuth.greet(special_name)
  assert_eq_string("Hello, 用户@123!", special_greeting)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let input_size = 1000
  let linear_factor = 2
  let quadratic_factor = 3
  let constant_overhead = 50
  
  // 模拟 O(n²) 算法复杂度
  let quadratic_component = azimuth.divide_with_ceil(azimuth.multiply(input_size, input_size), quadratic_factor)
  let linear_component = azimuth.multiply(input_size, linear_factor)
  let total_operations = azimuth.add(azimuth.add(quadratic_component, linear_component), constant_overhead)
  
  assert_eq(333334, quadratic_component)
  assert_eq(2000, linear_component)
  assert_eq(335384, total_operations)
}

test "resource_allocation_optimization" {
  // 资源分配优化
  let total_budget = 5000
  let project1_cost = 1200
  let project2_cost = 800
  let project3_cost = 1500
  let project4_cost = 900
  let project5_cost = 600
  
  // 计算项目总成本
  let total_cost = azimuth.add(azimuth.add(azimuth.add(azimuth.add(project1_cost, project2_cost), project3_cost), project4_cost), project5_cost)
  
  // 计算剩余预算
  let remaining_budget = azimuth.add(total_budget, -total_cost)
  
  // 计算可以支持的小项目数量
  let small_project_cost = 400
  let additional_projects = azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(5000, total_cost)
  assert_eq(0, remaining_budget)
  assert_eq(0, additional_projects)
}

test "statistical_analysis_functions" {
  // 统计分析函数
  let data_point1 = 15
  let data_point2 = 23
  let data_point3 = 18
  let data_point4 = 29
  let data_point5 = 12
  let data_point6 = 25
  
  // 计算总和
  let sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(data_point1, data_point2), data_point3), data_point4), data_point5), data_point6)
  
  // 计算平均值（向上取整）
  let count = 6
  let average = azimuth.divide_with_ceil(sum, count)
  
  assert_eq(122, sum)
  assert_eq(21, average)
}

test "time_series_calculation" {
  // 时间序列计算
  let initial_value = 100
  let growth_rate = 15
  
  // 计算复合增长（简化版本）
  let period1_value = azimuth.add(initial_value, azimuth.divide_with_ceil(azimuth.multiply(initial_value, growth_rate), 100))
  let period2_value = azimuth.add(period1_value, azimuth.divide_with_ceil(azimuth.multiply(period1_value, growth_rate), 100))
  let period3_value = azimuth.add(period2_value, azimuth.divide_with_ceil(azimuth.multiply(period2_value, growth_rate), 100))
  let final_value = azimuth.add(period3_value, azimuth.divide_with_ceil(azimuth.multiply(period3_value, growth_rate), 100))
  
  assert_eq(115, period1_value)
  assert_eq(133, period2_value)
  assert_eq(153, period3_value)
  assert_eq(176, final_value)
}

test "engineering_tolerance_calculation" {
  // 工程容差计算
  let nominal_dimension = 100
  let tolerance_percentage = 5
  let measurement1 = 98
  let measurement2 = 102
  let measurement3 = 97
  let measurement4 = 103
  let measurement5 = 99
  let measurement6 = 101
  
  // 计算容差范围
  let tolerance_amount = azimuth.divide_with_ceil(azimuth.multiply(nominal_dimension, tolerance_percentage), 100)
  let upper_limit = azimuth.add(nominal_dimension, tolerance_amount)
  let lower_limit = azimuth.add(nominal_dimension, -tolerance_amount)
  
  // 计算样本平均值
  let sample_sum = azimuth.add(azimuth.add(azimuth.add(azimuth.add(azimuth.add(measurement1, measurement2), measurement3), measurement4), measurement5), measurement6)
  let sample_average = azimuth.divide_with_ceil(sample_sum, 6)
  
  assert_eq(5, tolerance_amount)
  assert_eq(105, upper_limit)
  assert_eq(95, lower_limit)
  assert_eq(100, sample_average)
}

test "logistics_distribution_optimization" {
  // 物流配送优化
  let total_orders = 247
  let vehicle_capacity = 18
  let delivery_routes = 3
  
  // 计算需要的车辆数量
  let vehicles_needed = azimuth.divide_with_ceil(total_orders, vehicle_capacity)
  
  // 计算每条路线的平均订单数
  let orders_per_route = azimuth.divide_with_ceil(total_orders, delivery_routes)
  
  // 计算剩余配送能力
  let total_capacity = azimuth.multiply(vehicles_needed, vehicle_capacity)
  let spare_capacity = azimuth.add(total_capacity, -total_orders)
  
  assert_eq(14, vehicles_needed)
  assert_eq(83, orders_per_route)
  assert_eq(5, spare_capacity)
}