// 新的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiplication_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(1, divide_with_ceil(-1, -1))  // 负数相等
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))  // 除数为零
  assert_eq(0, divide_with_ceil(0, 5))  // 零除法
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Mundo!", greet("Mundo"))
  assert_eq_string("Hello, Monde!", greet("Monde"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_financial_calculation" {
  // 测试复杂金融计算：贷款计算
  let principal = 100000
  let annual_rate = 5
  let years = 3
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, annual_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, annual_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, annual_rate), 100))
  
  assert_eq(105000, year1_amount)
  assert_eq(110250, year2_amount)
  assert_eq(115763, year3_amount)
}

test "inventory_optimization" {
  // 测试库存优化计算
  let current_stock = 250
  let daily_demand = 15
  let lead_time_days = 7
  let safety_stock = 50
  
  // 计算再订货点
  let demand_during_lead = multiply(daily_demand, lead_time_days)
  let reorder_point = add(demand_during_lead, safety_stock)
  
  // 计算经济订货量（简化版本）
  let eoq = divide_with_ceil(multiply(daily_demand, 30), 4)  // 30天供应，每季度订货
  
  assert_eq(105, demand_during_lead)
  assert_eq(155, reorder_point)
  assert_eq(113, eoq)
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等差数列
  let first_term = 5
  let common_difference = 3
  let n_terms = 8
  
  // 计算第n项：a_n = a_1 + (n-1)*d
  let nth_term = add(first_term, multiply(n_terms - 1, common_difference))
  
  // 计算前n项和：S_n = n/2 * (a_1 + a_n)
  let sum_n = divide_with_ceil(multiply(n_terms, add(first_term, nth_term)), 2)
  
  assert_eq(26, nth_term)  // 5 + 7*3 = 26
  assert_eq(124, sum_n)    // 8/2 * (5 + 26) = 4 * 31 = 124
}

test "engineering_tolerance_calculation" {
  // 测试工程公差计算
  let nominal_dimension = 100
  let positive_tolerance = 2
  let negative_tolerance = 1
  
  // 计算最大和最小尺寸
  let max_dimension = add(nominal_dimension, positive_tolerance)
  let min_dimension = add(nominal_dimension, -negative_tolerance)
  
  // 计算公差带
  let tolerance_band = subtract(max_dimension, min_dimension)
  
  // 计算可以容纳的零件数量（给定总长度）
  let total_length = 1000
  let parts_count = divide_with_ceil(total_length, max_dimension)
  
  assert_eq(102, max_dimension)
  assert_eq(99, min_dimension)
  assert_eq(3, tolerance_band)
  assert_eq(10, parts_count)
}

test "data_processing_batch_size" {
  // 测试数据处理批大小计算
  let total_records = 10000
  let memory_limit_per_batch = 1500
  let overhead_percentage = 10
  
  // 计算有效批大小（考虑开销）
  let effective_batch_size = subtract(memory_limit_per_batch, 
    divide_with_ceil(multiply(memory_limit_per_batch, overhead_percentage), 100))
  
  // 计算需要的批次数
  let batch_count = divide_with_ceil(total_records, effective_batch_size)
  
  // 计算最后一批的记录数
  let last_batch_size = subtract(total_records, 
    multiply(batch_count - 1, effective_batch_size))
  
  assert_eq(1350, effective_batch_size)
  assert_eq(8, batch_count)
  assert_eq(100, last_batch_size)
}