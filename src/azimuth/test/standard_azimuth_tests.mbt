// 标准 MoonBit 测试用例 - 针对 azimuth 库
// 包含核心功能的全面测试，使用标准 MoonBit 测试语法

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 42
  let b = 17
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(59, @azimuth.add(a, b))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 3
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_coverage" {
  // 全面测试向上取整除法的各种情况
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(4, 2))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100))
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 4))
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
}

test "greet_function_international_support" {
  // 测试问候函数的国际化支持
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, Müller!", @azimuth.greet("Müller"))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款利息计算
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  // 简化的复利计算
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, annual_rate), 100))
  let year3_amount = @azimuth.add(year2_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year2_amount, annual_rate), 100))
  
  @azimuth.assert_eq(10500, year1_amount)
  @azimuth.assert_eq(11025, year2_amount)
  @azimuth.assert_eq(11576, year3_amount)
}

test "inventory_optimization_scenario" {
  // 库存优化场景：计算最优订货量
  let monthly_demand = 450
  let order_cost = 50
  let holding_cost = 2
  
  // 经济订货量 (EOQ) 简化计算
  let annual_demand = @azimuth.multiply(monthly_demand, 12)
  let eoq_numerator = @azimuth.multiply(@azimuth.multiply(2, annual_demand), order_cost)
  let eoq = @azimuth.divide_with_ceil(eoq_numerator, holding_cost)
  
  @azimuth.assert_eq(5400, annual_demand)
  @azimuth.assert_eq(540000, eoq_numerator)
  @azimuth.assert_eq(270000, eoq)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 3
  let common_difference = 4
  let number_of_terms = 6
  
  // 使用公式：S = n/2 * (2a + (n-1)d)
  let sum_numerator = @azimuth.multiply(number_of_terms, @azimuth.add(@azimuth.multiply(2, first_term), @azimuth.multiply(number_of_terms - 1, common_difference)))
  let sequence_sum = @azimuth.divide_with_ceil(sum_numerator, 2)
  
  @azimuth.assert_eq(12, sum_numerator)
  @azimuth.assert_eq(6, sequence_sum)
}

test "resource_allocation_efficiency" {
  // 资源分配效率测试
  let total_budget = 50000
  let project_a_cost = 12000
  let project_b_cost = 8000
  let project_c_cost = 15000
  
  // 计算分配后的剩余预算
  let allocated_total = @azimuth.add(@azimuth.add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = @azimuth.add(total_budget, -allocated_total)
  
  // 计算可以支持的额外小项目数量
  let small_project_cost = 2000
  let additional_projects = @azimuth.divide_with_ceil(remaining_budget, small_project_cost)
  
  @azimuth.assert_eq(35000, allocated_total)
  @azimuth.assert_eq(15000, remaining_budget)
  @azimuth.assert_eq(8, additional_projects)
}

test "error_boundary_robustness" {
  // 错误边界健壮性测试
  // 测试极值情况
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.add(0, 12345))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(12345, 0))
  
  // 测试负数边界
  @azimuth.assert_eq(-12345, @azimuth.add(-12345, 0))
  @azimuth.assert_eq(12345, @azimuth.multiply(-12345, -1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(12345, -1))
}

test "real_world_shipping_calculator" {
  // 真实世界运费计算器
  let package_weight = 15
  let base_rate = 5
  let weight_surcharge = 2
  let distance_factor = 3
  let handling_fee = 10
  
  // 计算运费：基础费用 + 重量附加费 + 距离因子 + 手续费
  let base_cost = @azimuth.multiply(package_weight, base_rate)
  let weight_cost = @azimuth.multiply(package_weight, weight_surcharge)
  let distance_cost = @azimuth.multiply(package_weight, distance_factor)
  let total_cost = @azimuth.add(@azimuth.add(@azimuth.add(base_cost, weight_cost), distance_cost), handling_fee)
  
  @azimuth.assert_eq(75, base_cost)
  @azimuth.assert_eq(30, weight_cost)
  @azimuth.assert_eq(45, distance_cost)
  @azimuth.assert_eq(160, total_cost)
}