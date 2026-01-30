// 标准 MoonBit 增强测试用例
// 为 azimuth 库添加的高质量测试，不超过10个测试用例

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "mathematical_invariant_properties" {
  // 测试数学不变性质
  let x = 42
  
  // 加法逆元
  assert_eq(0, add(x, -x))
  
  // 减法的自反性
  assert_eq(0, subtract(x, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  
  // 减去零
  assert_eq(x, subtract(x, 0))
}

test "complex_financial_amortization" {
  // 复杂金融摊销计算
  let loan_principal = 120000
  let annual_rate = 6
  let loan_term_years = 30
  
  // 计算月利率（简化）
  let monthly_rate = divide_with_ceil(annual_rate, 12)
  
  // 计算总月数
  let total_months = multiply(loan_term_years, 12)
  
  // 简化的月供计算
  let monthly_payment = divide_with_ceil(
    add(loan_principal, divide_with_ceil(multiply(loan_principal, annual_rate), 100)),
    total_months
  )
  
  assert_eq(1, monthly_rate)
  assert_eq(360, total_months)
  assert_eq(367, monthly_payment)
}

test "resource_optimization_algorithm" {
  // 资源优化算法测试
  let total_resources = 1000
  let project_a_needs = 234
  let project_b_needs = 189
  let project_c_needs = 156
  
  // 计算剩余资源
  let allocated = add(add(project_a_needs, project_b_needs), project_c_needs)
  let remaining = subtract(total_resources, allocated)
  
  // 计算可以支持的额外小项目数量
  let small_project_size = 45
  let additional_projects = divide_with_ceil(remaining, small_project_size)
  
  assert_eq(579, allocated)
  assert_eq(421, remaining)
  assert_eq(10, additional_projects)
}

test "error_boundary_comprehensive" {
  // 综合错误边界测试
  // 除零的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  
  // 极值减法
  let max_val = 2147483647
  let min_val = -2147483648
  assert_eq(max_val, subtract(max_val, 0))
  assert_eq(min_val, subtract(min_val, 0))
}

test "data_transformation_pipeline" {
  // 数据转换流水线测试
  let raw_data = 12345
  
  // 步骤1: 标准化处理
  let normalized = divide_with_ceil(raw_data, 1000)
  
  // 步骤2: 应用系数
  let scaled = multiply(normalized, 25)
  
  // 步骤3: 调整偏移
  let adjusted = add(scaled, 100)
  
  // 步骤4: 最终标准化
  let final_result = divide_with_ceil(adjusted, 3)
  
  assert_eq(13, normalized)
  assert_eq(325, scaled)
  assert_eq(425, adjusted)
  assert_eq(142, final_result)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试各种特殊字符串组合
  assert_eq_string("Hello, JSON_2024!", greet("JSON_2024"))
  assert_eq_string("Hello, API_v2.0!", greet("API_v2.0"))
  assert_eq_string("Hello, user@domain.com!", greet("user@domain.com"))
  assert_eq_string("Hello, https://example.com!", greet("https://example.com"))
  assert_eq_string("Hello, C++/Java/Python!", greet("C++/Java/Python"))
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试
  // 计算等差数列前n项和: S = n/2 * (2a + (n-1)d)
  let first_term = 7
  let common_diff = 5
  let num_terms = 12
  
  let sum_n = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  // 计算等比数列前n项和（简化版本）
  let geo_first = 3
  let geo_ratio = 2
  let geo_terms = 8
  let geo_sum = multiply(geo_first, multiply(geo_ratio, geo_terms))
  
  assert_eq(474, sum_n)
  assert_eq(48, geo_sum)
}

test "real_world_logistics_optimization" {
  // 真实世界物流优化测试
  let warehouse_capacity = 5000
  let product_weights = [123, 234, 345, 456, 567]  // 模拟5种产品重量
  let daily_demand = [45, 67, 23, 89, 34]  // 模拟每日需求
  
  // 计算每日总需求重量（简化计算）
  let daily_weight = add(add(add(add(
    multiply(123, 45), multiply(234, 67)
  ), multiply(345, 23)), multiply(456, 89)), multiply(567, 34))
  
  // 计算需要的存储天数
  let storage_days = divide_with_ceil(warehouse_capacity, daily_weight)
  
  // 计算安全库存水平（30%缓冲）
  let safety_stock = divide_with_ceil(multiply(warehouse_capacity, 30), 100)
  
  assert_eq(119907, daily_weight)
  assert_eq(1, storage_days)
  assert_eq(1500, safety_stock)
}

test "performance_benchmark_simulation" {
  // 性能基准模拟测试
  let baseline_operations = 10000
  let optimization_factor = 3
  let overhead_percentage = 12
  
  // 计算优化后的性能
  let optimized_ops = multiply(baseline_operations, optimization_factor)
  
  // 计算系统开销
  let system_overhead = divide_with_ceil(multiply(optimized_ops, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = subtract(optimized_ops, system_overhead)
  
  // 计算性能提升百分比
  let improvement_percentage = divide_with_ceil(
    multiply(subtract(actual_performance, baseline_operations), 100),
    baseline_operations
  )
  
  assert_eq(30000, optimized_ops)
  assert_eq(3600, system_overhead)
  assert_eq(26400, actual_performance)
  assert_eq(164, improvement_percentage)
}