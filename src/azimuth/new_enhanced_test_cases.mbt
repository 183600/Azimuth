// 增强的 azimuth 测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能和边界情况

test "enhanced_add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = -6789
  assert_eq(add(a, b), add(b, a))
  assert_eq(5556, add(a, b))
}

test "enhanced_multiply_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "enhanced_divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-2, divide_with_ceil(-4, 2))
  assert_eq(-1, divide_with_ceil(-3, 2))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "enhanced_greet_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Ñoño!", greet("Ñoño"))
  assert_eq_string("Hello, αβγδε!", greet("αβγδε"))
}

test "enhanced_complex_financial_calculation" {
  // 复杂金融计算：贷款利息计算
  let principal = 10000
  let annual_rate = 5
  
  // 简化的复利计算
  let year1_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  let year2_interest = divide_with_ceil(multiply(year1_total, annual_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  let year3_interest = divide_with_ceil(multiply(year2_total, annual_rate), 100)
  let final_total = add(year2_total, year3_interest)
  
  assert_eq(10500, year1_total)
  assert_eq(11025, year2_total)
  assert_eq(11576, final_total)
}

test "enhanced_inventory_optimization" {
  // 库存优化：计算最优订货量
  let monthly_demand = 450
  let ordering_cost = 50
  let holding_cost_per_unit = 2
  
  // 简化的EOQ计算（经济订货量）
  let annual_demand = multiply(monthly_demand, 12)
  let eoq_numerator = multiply(multiply(2, annual_demand), ordering_cost)
  let eoq = divide_with_ceil(eoq_numerator, holding_cost_per_unit)
  
  assert_eq(5400, annual_demand)
  assert_eq(540000, eoq_numerator)
  assert_eq(270000, eoq)
}

test "enhanced_time_zone_conversion" {
  // 时区转换计算
  let local_hour = 14
  let time_difference = -8
  
  // 计算目标时区时间（24小时制）
  let target_time_raw = add(local_hour, time_difference)
  let target_time = if target_time_raw < 0 {
    add(target_time_raw, 24)
  } else if target_time_raw >= 24 {
    add(target_time_raw, -24)
  } else {
    target_time_raw
  }
  
  assert_eq(6, target_time)
}

test "enhanced_batch_processing" {
  // 批处理计算：分组处理任务
  let total_tasks = 157
  let batch_size = 25
  let processing_time_per_batch = 30
  
  let number_of_batches = divide_with_ceil(total_tasks, batch_size)
  let total_processing_time = multiply(number_of_batches, processing_time_per_batch)
  
  assert_eq(7, number_of_batches)
  assert_eq(210, total_processing_time)
}

test "enhanced_resource_allocation" {
  // 资源分配：计算项目资源需求
  let team_size = 8
  let project_duration_weeks = 12
  let weekly_working_hours = 40
  let overhead_percentage = 20
  
  let total_person_hours = multiply(team_size, multiply(project_duration_weeks, weekly_working_hours))
  let overhead_hours = divide_with_ceil(multiply(total_person_hours, overhead_percentage), 100)
  let total_project_hours = add(total_person_hours, overhead_hours)
  
  assert_eq(3840, total_person_hours)
  assert_eq(768, overhead_hours)
  assert_eq(4608, total_project_hours)
}

test "enhanced_data_compression_analysis" {
  // 数据压缩分析：计算压缩比和存储节省
  let original_size_mb = 1024
  let compression_algorithm_1_ratio = 65
  let compression_algorithm_2_ratio = 45
  let compression_algorithm_3_ratio = 80
  
  let compressed_size_1 = divide_with_ceil(multiply(original_size_mb, compression_algorithm_1_ratio), 100)
  let compressed_size_2 = divide_with_ceil(multiply(original_size_mb, compression_algorithm_2_ratio), 100)
  let compressed_size_3 = divide_with_ceil(multiply(original_size_mb, compression_algorithm_3_ratio), 100)
  
  let storage_savings_1 = add(original_size_mb, -compressed_size_1)
  let storage_savings_2 = add(original_size_mb, -compressed_size_2)
  let storage_savings_3 = add(original_size_mb, -compressed_size_3)
  
  assert_eq(665, compressed_size_1)
  assert_eq(460, compressed_size_2)
  assert_eq(819, compressed_size_3)
  assert_eq(359, storage_savings_1)
  assert_eq(564, storage_savings_2)
  assert_eq(205, storage_savings_3)
}