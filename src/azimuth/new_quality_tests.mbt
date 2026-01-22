// 新的高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "mathematical_properties_commutative" {
  // 验证加法和乘法交换律
  let a = 12
  let b = 28
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_precision_cases" {
  // 测试向上取整除法的精度情况
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(4, divide_with_ceil(11, 3)) // 3.666... -> 4
  assert_eq(7, divide_with_ceil(13, 2)) // 6.5 -> 7
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
}

test "greet_function_multilingual_support" {
  // 测试多语言问候功能
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_financial_calculation" {
  // 复杂金融计算场景
  let principal = 10000
  let rate_percent = 8
  let years = 3
  
  // 计算复利（简化版本）
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(10800, year1_amount)
  assert_eq(11664, year2_amount)
  assert_eq(12597, year3_amount)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 500
  let daily_sales = 25
  let restock_amount = 100
  let days_to_simulate = 7
  
  let total_sold = multiply(daily_sales, days_to_simulate)
  let stock_after_sales = add(initial_stock, -total_sold)
  let final_stock = add(stock_after_sales, restock_amount)
  
  assert_eq(175, total_sold)
  assert_eq(325, stock_after_sales)
  assert_eq(425, final_stock)
}

test "engineering_structural_calculation" {
  // 工程结构计算测试
  let beam_length = 15
  let load_per_meter = 200
  let safety_factor = 2
  
  // 计算总载荷
  let total_load = multiply(beam_length, load_per_meter)
  
  // 应用安全系数
  let safe_load_capacity = divide_with_ceil(total_load, safety_factor)
  
  assert_eq(3000, total_load)
  assert_eq(1500, safe_load_capacity)
}

test "data_analysis_statistics" {
  // 数据分析统计测试
  let data_points = [45, 67, 23, 89, 12, 56, 78, 34, 90, 41]
  let sum_manual = add(add(add(add(add(add(add(add(add(45, 67), 23), 89), 12), 56), 78), 34), 90), 41)
  let average = divide_with_ceil(sum_manual, 10)
  
  assert_eq(535, sum_manual)
  assert_eq(54, average)
}

test "time_zone_conversion" {
  // 时区转换计算测试
  let local_time = 14  // 2 PM
  let time_difference = 8  // 8小时差异
  
  // 计算目标时区时间
  let target_time = add(local_time, time_difference)
  let adjusted_time = if target_time >= 24 {
    add(target_time, -24)
  } else {
    target_time
  }
  
  assert_eq(22, adjusted_time)
}

test "resource_optimization_algorithm" {
  // 资源优化算法测试
  let total_resources = 1000
  let project_a_needs = 300
  let project_b_needs = 250
  let project_c_needs = 400
  
  // 分配资源
  let allocated_a = project_a_needs
  let allocated_b = project_b_needs
  let allocated_c = project_c_needs
  let total_allocated = add(add(allocated_a, allocated_b), allocated_c)
  let remaining_resources = add(total_resources, -total_allocated)
  
  assert_eq(950, total_allocated)
  assert_eq(50, remaining_resources)
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 测试除零保护
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  assert_eq(0, add(2147483647, -2147483647))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, multiply(0, 2147483647))
}