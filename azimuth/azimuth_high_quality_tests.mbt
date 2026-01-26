// Azimuth 库的高质量 MoonBit 测试用例
// 包含 10 个标准测试用例，覆盖核心功能和边界情况

// 导入必要的模块
use moonbitlang/core

test "azimuth_mathematical_precision" {
  // 测试数学精度和边界情况
  let small_fraction = divide_with_ceil(1, 1000)
  let large_number = multiply(100000, 1000)
  let precision_test = add(large_number, small_fraction)
  
  assert_eq(1, small_fraction)  // ceil(0.001) = 1
  assert_eq(100000000, large_number)
  assert_eq(100000001, precision_test)
}

test "azimuth_business_inventory" {
  // 业务场景：库存管理系统
  let product_a_qty = 147
  let product_b_qty = 89
  let product_c_qty = 256
  let box_capacity = 24
  
  let total_products = add(add(product_a_qty, product_b_qty), product_c_qty)
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = subtract(total_capacity, total_products)
  
  assert_eq(492, total_products)
  assert_eq(21, boxes_needed)  // ceil(492/24) = 21
  assert_eq(504, total_capacity)
  assert_eq(12, unused_space)
}

test "azimuth_financial_compound" {
  // 金融场景：复利计算
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  let year1_end = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_end = add(year1_end, divide_with_ceil(multiply(year1_end, rate_percent), 100))
  let year3_end = add(year2_end, divide_with_ceil(multiply(year2_end, rate_percent), 100))
  
  assert_eq(10800, year1_end)  // 10000 + 8%
  assert_eq(11664, year2_end)  // 10800 + 8%
  assert_eq(12597, year3_end)  // 11664 + 8%
}

test "azimuth_time_management" {
  // 时间管理：项目排期
  let total_tasks = 87
  let team_members = 4
  let tasks_per_person_per_day = 3
  let workdays_per_week = 5
  
  let daily_capacity = multiply(team_members, tasks_per_person_per_day)
  let days_needed = divide_with_ceil(total_tasks, daily_capacity)
  let weeks_needed = divide_with_ceil(days_needed, workdays_per_week)
  
  assert_eq(12, daily_capacity)
  assert_eq(8, days_needed)  // ceil(87/12) = 8
  assert_eq(2, weeks_needed)  // ceil(8/5) = 2
}

test "azimuth_string_processing" {
  // 字符串处理：国际化测试
  let english_name = "Alice"
  let chinese_name = "爱丽丝"
  let russian_name = "Алиса"
  let emoji_name = "👩‍💼"
  
  let english_greeting = greet(english_name)
  let chinese_greeting = greet(chinese_name)
  let russian_greeting = greet(russian_name)
  let emoji_greeting = greet(emoji_name)
  
  assert_eq_string("Hello, Alice!", english_greeting)
  assert_eq_string("Hello, 爱丽丝!", chinese_greeting)
  assert_eq_string("Hello, Алиса!", russian_greeting)
  assert_eq_string("Hello, 👩‍💼!", emoji_greeting)
}

test "azimuth_resource_optimization" {
  // 资源优化：服务器负载均衡
  let total_requests = 1250
  let servers = 5
  let max_requests_per_server = 300
  
  let requests_per_server = divide_with_ceil(total_requests, servers)
  let total_capacity = multiply(requests_per_server, servers)
  let overload_percentage = divide_with_ceil(multiply(total_capacity - total_requests, 100), total_requests)
  
  assert_eq(250, requests_per_server)  // ceil(1250/5) = 250
  assert_eq(1250, total_capacity)
  assert_eq(0, overload_percentage)  // 完美均衡
}

test "azimuth_data_analysis" {
  // 数据分析：统计计算
  let data_points = 1000
  let sample_size = 75
  let confidence_level = 95
  
  let samples_needed = divide_with_ceil(data_points, sample_size)
  let confidence_factor = divide_with_ceil(multiply(confidence_level, data_points), 100)
  let adjusted_samples = add(samples_needed, divide_with_ceil(confidence_factor, 100))
  
  assert_eq(14, samples_needed)  // ceil(1000/75) = 14
  assert_eq(950, confidence_factor)
  assert_eq(23, adjusted_samples)  // 14 + ceil(950/100) = 14 + 10 = 24
}

test "azimuth_error_handling" {
  // 错误处理：边界情况
  let normal_case = divide_with_ceil(100, 25)
  let zero_divisor = divide_with_ceil(100, 0)
  let negative_result = divide_with_ceil(-100, 30)
  let fractional_case = divide_with_ceil(101, 25)
  
  assert_eq(4, normal_case)
  assert_eq(0, zero_divisor)  // 安全处理
  assert_eq(-3, negative_result)
  assert_eq(5, fractional_case)
}

test "azimuth_algorithm_complexity" {
  // 算法复杂度：性能测试
  let input_size = 1000
  let linear_complexity = multiply(input_size, 2)
  let logarithmic_complexity = divide_with_ceil(input_size, 10)
  let quadratic_complexity = multiply(input_size, divide_with_ceil(input_size, 100))
  
  assert_eq(2000, linear_complexity)
  assert_eq(100, logarithmic_complexity)
  assert_eq(10000, quadratic_complexity)
}

test "azimuth_real_world_scenario" {
  // 真实世界场景：活动策划
  let participants = 156
  let tables = 12
  let chairs_per_table = 8
  let budget_per_person = 45
  let fixed_costs = 500
  
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  let total_chairs = multiply(tables_needed, chairs_per_table)
  let variable_costs = multiply(participants, budget_per_person)
  let total_budget = add(variable_costs, fixed_costs)
  let budget_per_table = divide_with_ceil(total_budget, tables_needed)
  
  assert_eq(20, tables_needed)  // ceil(156/8) = 20
  assert_eq(160, total_chairs)
  assert_eq(7020, variable_costs)
  assert_eq(7520, total_budget)
  assert_eq(376, budget_per_table)  // ceil(7520/20) = 376
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}