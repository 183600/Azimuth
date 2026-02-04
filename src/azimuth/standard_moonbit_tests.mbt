// 新增的 MoonBit 标准测试用例
// 针对 azimuth 库核心功能的高质量测试

test "subtract_function_comprehensive" {
  // 全面测试减法功能
  assert_eq(5, subtract(8, 3))
  assert_eq(-5, subtract(3, 8))
  assert_eq(0, subtract(10, 10))
  assert_eq(10, subtract(0, -10))
  assert_eq(-10, subtract(-10, 0))
  assert_eq(20, subtract(-5, -25))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(9999, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let x = 10
  let y = 20
  let z = 5
  
  // 测试 (x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(149, result)
  
  // 测试嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 2), 3)
  assert_eq(67, nested)
}

test "financial_compound_interest" {
  // 金融复利计算测试
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

test "inventory_stock_management" {
  // 库存管理测试
  let initial_stock = 500
  let daily_sales = 25
  let days = 7
  let new_shipment = 100
  
  let sold = multiply(daily_sales, days)
  let remaining = subtract(initial_stock, sold)
  let final_stock = add(remaining, new_shipment)
  
  assert_eq(175, sold)
  assert_eq(325, remaining)
  assert_eq(425, final_stock)
}

test "time_project_planning" {
  // 项目时间规划测试
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(11, days_needed)
  assert_eq(3, weeks_needed)
  assert_eq(88, total_hours)
}

test "unicode_comprehensive_greeting" {
  // 全面Unicode问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, αβγ!", greet("αβγ"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))
  assert_eq_string("Hello, !", greet(""))
}

test "energy_consumption_calculation" {
  // 能源消耗计算测试
  let daily_consumption = 120
  let days_in_month = 30
  let efficiency_improvement = 15
  
  let monthly_consumption = multiply(daily_consumption, days_in_month)
  let improved_consumption = subtract(monthly_consumption, 
    divide_with_ceil(multiply(monthly_consumption, efficiency_improvement), 100))
  
  assert_eq(3600, monthly_consumption)
  assert_eq(3060, improved_consumption)
}

test "data_transfer_calculation" {
  // 数据传输计算测试
  let file_size_mb = 850
  let network_speed_mbps = 50
  let overhead_percentage = 10
  
  let base_time = divide_with_ceil(file_size_mb, network_speed_mbps)
  let overhead_time = divide_with_ceil(multiply(base_time, overhead_percentage), 100)
  let total_time = add(base_time, overhead_time)
  
  assert_eq(17, base_time)
  assert_eq(2, overhead_time)
  assert_eq(19, total_time)
}

test "extreme_boundary_conditions" {
  // 极端边界条件测试
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 测试极值除法
  assert_eq(1, divide_with_ceil(1, 1000000))
  assert_eq(1000000, divide_with_ceil(1000000, 1))
  assert_eq(0, divide_with_ceil(0, 1000000))
  
  // 测试负数极值
  assert_eq(-2000000, add(-1000000, -1000000))
  assert_eq(1000000, multiply(-1000, -1000))
}