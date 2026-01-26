// Azimuth 库综合单元测试
// 包含针对核心功能的全面测试用例

test "azimuth_division_precision_test" {
  // 测试除法精度和边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "azimuth_string_greeting_comprehensive" {
  // 测试字符串问候功能的综合情况
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "azimuth_mathematical_series" {
  // 测试数学级数计算
  let first_term = 5
  let common_diff = 3
  let n_terms = 6
  
  // 计算等差数列第n项
  let nth_term = add(first_term, multiply(n_terms - 1, common_diff))
  assert_eq(20, nth_term)  // 5 + (6-1)*3 = 20
  
  // 计算等差数列和
  let series_sum = divide_with_ceil(multiply(n_terms, add(first_term, nth_term)), 2)
  assert_eq(75, series_sum)  // 6 * (5 + 20) / 2 = 75
}

test "azimuth_financial_compound_interest" {
  // 测试金融复利计算
  let principal = 1000
  let rate_percent = 5
  
  // 第一年
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  // 第二年
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  // 第三年
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(1050, year1)
  assert_eq(1103, year2)
  assert_eq(1158, year3)
}

test "azimuth_inventory_optimization" {
  // 测试库存优化计算
  let demand_forecast = 450
  let safety_stock = 50
  let reorder_point = add(demand_forecast, safety_stock)
  
  let package_size = 24
  let order_quantity = divide_with_ceil(reorder_point, package_size)
  
  assert_eq(500, reorder_point)
  assert_eq(21, order_quantity)  // ceil(500/24) = 21
}

test "azimuth_project_resource_allocation" {
  // 测试项目资源分配
  let total_budget = 10000
  let team_size = 8
  let project_duration_days = 15
  
  let daily_budget = divide_with_ceil(total_budget, project_duration_days)
  let budget_per_person = divide_with_ceil(daily_budget, team_size)
  let total_person_days = multiply(team_size, project_duration_days)
  
  assert_eq(667, daily_budget)  // ceil(10000/15) = 667
  assert_eq(84, budget_per_person)  // ceil(667/8) = 84
  assert_eq(120, total_person_days)  // 8 * 15 = 120
}

test "azimuth_data_structure_operations" {
  // 测试数据结构操作模拟
  let array_size = 100
  let chunk_size = 7
  
  // 计算需要的块数
  let num_chunks = divide_with_ceil(array_size, chunk_size)
  
  // 计算最后一个块的大小
  let last_chunk_size = add(array_size, -multiply(num_chunks - 1, chunk_size))
  
  assert_eq(15, num_chunks)  // ceil(100/7) = 15
  assert_eq(2, last_chunk_size)  // 100 - 14*7 = 2
}

test "azimuth_time_conversion" {
  // 测试时间转换计算
  let total_minutes = 150
  let hours = divide_with_ceil(total_minutes, 60)
  let remaining_minutes = add(total_minutes, -multiply(hours - 1, 60))
  
  assert_eq(3, hours)  // ceil(150/60) = 3
  assert_eq(30, remaining_minutes)  // 150 - 2*60 = 30
}

test "azimuth_geometric_calculation" {
  // 测试几何计算
  let radius = 5
  let pi_approx = 22  // 22/7 近似π
  
  // 计算圆周长
  let circumference = divide_with_ceil(multiply(2, multiply(radius, pi_approx)), 7)
  
  // 计算圆面积
  let area = divide_with_ceil(multiply(radius, multiply(radius, pi_approx)), 7)
  
  assert_eq(32, circumference)  // ceil(2*5*22/7) = ceil(220/7) = 32
  assert_eq(79, area)  // ceil(5*5*22/7) = ceil(550/7) = 79
}

test "azimuth_performance_metrics" {
  // 测试性能指标计算
  let operations_completed = 1250
  let time_elapsed_seconds = 45
  let target_operations = 2000
  
  let ops_per_second = divide_with_ceil(operations_completed, time_elapsed_seconds)
  let completion_percentage = divide_with_ceil(multiply(operations_completed, 100), target_operations)
  let estimated_total_time = divide_with_ceil(target_operations, ops_per_second)
  
  assert_eq(28, ops_per_second)  // ceil(1250/45) = 28
  assert_eq(63, completion_percentage)  // ceil(1250*100/2000) = 63
  assert_eq(72, estimated_total_time)  // ceil(2000/28) = 72
}