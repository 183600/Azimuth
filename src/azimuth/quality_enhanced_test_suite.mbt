// 高质量增强测试套件
// 针对 azimuth 库的全面测试，包含边界情况、实际应用场景和数学性质验证

test "advanced_mathematical_series" {
  // 测试等差数列和等比数列计算
  let first_term = 3
  let common_diff = 4
  let terms_count = 6
  
  // 计算等差数列第n项：a_n = a_1 + (n-1)d
  let nth_term = add(first_term, multiply(common_diff, terms_count - 1))
  assert_eq(23, nth_term)  // 3 + 5*4 = 23
  
  // 计算等差数列前n项和：S_n = n/2 * (a_1 + a_n)
  let series_sum = divide_with_ceil(multiply(terms_count, add(first_term, nth_term)), 2)
  assert_eq(78, series_sum)  // 6/2 * (3 + 23) = 3 * 26 = 78
}

test "financial_compound_interest" {
  // 测试复利计算
  let principal = 10000
  let annual_rate = 8
  let _years = 3
  
  // 第一年末
  let year1 = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  // 第二年末
  let year2 = add(year1, divide_with_ceil(multiply(year1, annual_rate), 100))
  // 第三年末
  let year3 = add(year2, divide_with_ceil(multiply(year2, annual_rate), 100))
  
  assert_eq(10800, year1)  // 10000 + 800
  assert_eq(11664, year2)  // 10800 + 864
  assert_eq(12597, year3)  // 11664 + 933
}

test "inventory_optimization" {
  // 测试库存优化算法
  let monthly_demand = 450
  let order_cost = 50
  let holding_cost_per_unit = 2
  
  // 计算经济订货量 (EOQ) 的简化版本
  let annual_demand = multiply(monthly_demand, 12)
  let eoq_numerator = multiply(multiply(2, annual_demand), order_cost)
  let eoq = divide_with_ceil(eoq_numerator, holding_cost_per_unit)
  
  // 计算年订货次数
  let orders_per_year = divide_with_ceil(annual_demand, eoq)
  
  assert_eq(5400, annual_demand)
  assert_eq(270000, eoq_numerator)
  assert_eq(135000, eoq)
  assert_eq(1, orders_per_year)
}

test "geometry_area_calculations" {
  // 测试几何面积计算
  let length = 12
  let width = 8
  let height = 5
  
  // 计算矩形面积
  let rectangle_area = multiply(length, width)
  assert_eq(96, rectangle_area)
  
  // 计算矩形周长
  let rectangle_perimeter = multiply(2, add(length, width))
  assert_eq(40, rectangle_perimeter)
  
  // 计算长方体表面积
  let surface_area = multiply(2, add(add(multiply(length, width), multiply(width, height)), multiply(height, length)))
  assert_eq(392, surface_area)
  
  // 计算长方体体积
  let volume = multiply(multiply(length, width), height)
  assert_eq(480, volume)
}

test "time_management_scenarios" {
  // 测试时间管理场景
  let total_tasks = 127
  let tasks_per_hour = 8
  let work_hours_per_day = 8
  let work_days_per_week = 5
  
  // 计算需要的工作小时数
  let total_hours = divide_with_ceil(total_tasks, tasks_per_hour)
  // 计算需要的工作天数
  let total_days = divide_with_ceil(total_hours, work_hours_per_day)
  // 计算需要的周数
  let total_weeks = divide_with_ceil(total_days, work_days_per_week)
  
  assert_eq(16, total_hours)   // ceil(127/8) = 16
  assert_eq(2, total_days)     // ceil(16/8) = 2
  assert_eq(1, total_weeks)    // ceil(2/5) = 1
}

test "data_analysis_statistics" {
  // 测试数据分析统计
  let _data_points = [15, 22, 18, 25, 20, 17, 23, 19, 21, 16]
  let sum_manual = add(add(add(add(add(add(add(add(add(15, 22), 18), 25), 20), 17), 23), 19), 21), 16)
  
  // 计算平均值
  let mean = divide_with_ceil(sum_manual, 10)
  
  // 计算中位数（简化版本）
  let sorted_sum = add(add(add(add(16, 17), 18), 19), 20)  // 前5个数的和
  let median = divide_with_ceil(sorted_sum, 5)
  
  assert_eq(196, sum_manual)
  assert_eq(20, mean)
  assert_eq(18, median)
}

test "telecommunications_bandwidth" {
  // 测试电信带宽计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  // 计算基础传输时间
  let base_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  
  // 计算开销时间
  let overhead_time = divide_with_ceil(multiply(base_time, overhead_percentage), 100)
  
  // 计算总传输时间
  let total_time = add(base_time, overhead_time)
  
  assert_eq(17, base_time)      // ceil(850/50) = 17
  assert_eq(2, overhead_time)   // ceil(17*10/100) = 2
  assert_eq(19, total_time)     // 17 + 2 = 19
}

test "agricultural_planning" {
  // 测试农业规划计算
  let field_area_hectares = 25
  let crop_yield_per_hectare = 800
  let market_price_per_ton = 250
  let labor_cost_per_hectare = 120
  let fertilizer_cost_per_hectare = 80
  
  // 计算总产量
  let total_yield = multiply(field_area_hectares, crop_yield_per_hectare)
  
  // 计算总收入
  let total_revenue = multiply(total_yield, market_price_per_ton)
  
  // 计算总成本
  let total_labor_cost = multiply(field_area_hectares, labor_cost_per_hectare)
  let total_fertilizer_cost = multiply(field_area_hectares, fertilizer_cost_per_hectare)
  let total_cost = add(total_labor_cost, total_fertilizer_cost)
  
  // 计算利润
  let profit = add(total_revenue, -total_cost)
  
  assert_eq(20000, total_yield)
  assert_eq(5000000, total_revenue)
  assert_eq(3000, total_labor_cost)
  assert_eq(2000, total_fertilizer_cost)
  assert_eq(5000, total_cost)
  assert_eq(4995000, profit)
}

test "string_processing_multilingual" {
  // 测试多语言字符串处理
  let english_greeting = greet("World")
  let chinese_greeting = greet("世界")
  let spanish_greeting = greet("Mundo")
  let arabic_greeting = greet("العالم")
  let emoji_greeting = greet("🌍")
  
  // 验证多语言支持
  assert_eq_string("Hello, World!", english_greeting)
  assert_eq_string("Hello, 世界!", chinese_greeting)
  assert_eq_string("Hello, Mundo!", spanish_greeting)
  assert_eq_string("Hello, العالم!", arabic_greeting)
  assert_eq_string("Hello, 🌍!", emoji_greeting)
  
  // 测试特殊字符组合
  let mixed_greeting = greet("User123@email.com")
  assert_eq_string("Hello, User123@email.com!", mixed_greeting)
}

test "advanced_algorithmic_complexity" {
  // 测试算法复杂度计算
  let input_size = 1000
  let linear_constant = 2
  let quadratic_constant = 3
  let logarithmic_base = 2
  
  // O(n) 线性复杂度
  let linear_operations = multiply(input_size, linear_constant)
  
  // O(n²) 二次复杂度（简化计算）
  let quadratic_operations = multiply(multiply(input_size, input_size), quadratic_constant) / 1000
  
  // O(log n) 对数复杂度（简化计算）
  let log_operations = divide_with_ceil(input_size, logarithmic_base)
  
  // 复杂算法总操作数
  let total_operations = add(add(linear_operations, quadratic_operations), log_operations)
  
  assert_eq(2000, linear_operations)
  assert_eq(3000, quadratic_operations)
  assert_eq(500, log_operations)
  assert_eq(5500, total_operations)
}