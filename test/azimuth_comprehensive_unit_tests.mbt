// Azimuth 项目综合单元测试
// 包含 10 个高质量的标准 MoonBit 测试用例

test "mathematical_commutativity_laws" {
  // 测试数学交换律
  let a = 15
  let b = 27
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(42, add(15, 27))
  assert_eq(405, multiply(15, 27))
}

test "boundary_value_analysis" {
  // 边界值分析测试
  // 测试零值边界
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, add(0, 12345))
  assert_eq(0, divide_with_ceil(0, 100))
  
  // 测试单位值边界
  assert_eq(1, multiply(1, 1))
  assert_eq(100, multiply(1, 100))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))
}

test "financial_calculation_scenario" {
  // 金融计算场景：贷款计算
  let principal = 10000
  let annual_rate = 5
  let years = 3
  
  // 简化的单利计算
  let yearly_interest = divide_with_ceil(multiply(principal, annual_rate), 100)
  let total_interest = multiply(yearly_interest, years)
  let total_repayment = add(principal, total_interest)
  
  assert_eq(500, yearly_interest)
  assert_eq(1500, total_interest)
  assert_eq(11500, total_repayment)
}

test "inventory_optimization" {
  // 库存优化问题
  let demand_forecast = 157
  let package_size = 24
  let safety_stock = 20
  
  // 计算需要订购的包裹数量
  let base_packages = divide_with_ceil(demand_forecast, package_size)
  let safety_packages = divide_with_ceil(safety_stock, package_size)
  let total_packages = add(base_packages, safety_packages)
  
  assert_eq(7, base_packages)  // ceil(157/24) = 7
  assert_eq(1, safety_packages)  // ceil(20/24) = 1
  assert_eq(8, total_packages)
}

test "string_processing_internationalization" {
  // 字符串处理国际化测试
  let english_name = "Alice"
  let chinese_name = "张三"
  let spanish_name = "María"
  let emoji_name = "🚀🌟"
  
  // 测试不同语言的问候
  assert_eq_string("Hello, Alice!", greet(english_name))
  assert_eq_string("Hello, 张三!", greet(chinese_name))
  assert_eq_string("Hello, María!", greet(spanish_name))
  assert_eq_string("Hello, 🚀🌟!", greet(emoji_name))
  
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_algorithm_simulation" {
  // 复杂算法模拟：二分搜索中的计算
  let array_size = 1000
  let target_value = 743
  
  // 模拟二分搜索的中间值计算
  let low = 0
  let high = array_size - 1
  let mid = divide_with_ceil(add(low, high), 2)
  
  // 模拟搜索步骤
  let steps_needed = divide_with_ceil(array_size, 2)
  let comparisons = multiply(steps_needed, 2)
  
  assert_eq(500, mid)  // ceil((0 + 999)/2) = 500
  assert_eq(500, steps_needed)
  assert_eq(1000, comparisons)
}

test "engineering_tolerance_calculation" {
  // 工程公差计算
  let nominal_dimension = 100
  let positive_tolerance = 2
  let negative_tolerance = 1
  
  // 计算最大和最小尺寸
  let max_dimension = add(nominal_dimension, positive_tolerance)
  let min_dimension = add(nominal_dimension, -negative_tolerance)
  
  // 计算公差带
  let tolerance_band = add(max_dimension, -min_dimension)
  
  // 计算可以容纳的零件数量（假设每个零件需要最大尺寸空间）
  let available_space = 1000
  let parts_count = divide_with_ceil(available_space, max_dimension)
  
  assert_eq(102, max_dimension)
  assert_eq(99, min_dimension)
  assert_eq(3, tolerance_band)
  assert_eq(10, parts_count)
}

test "data_structure_operations" {
  // 数据结构操作模拟
  let initial_capacity = 8
  let growth_factor = 2
  let load_factor = 75  // 百分比
  
  // 模拟动态数组扩容
  let current_size = 6
  let threshold = divide_with_ceil(multiply(initial_capacity, load_factor), 100)
  let needs_expansion = current_size > threshold
  
  let new_capacity = if needs_expansion {
    multiply(initial_capacity, growth_factor)
  } else {
    initial_capacity
  }
  
  assert_eq(6, threshold)  // ceil(8 * 75 / 100) = 6
  assert_eq(true, needs_expansion)
  assert_eq(16, new_capacity)
}

test "performance_benchmark_calculation" {
  // 性能基准计算
  let baseline_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 10
  
  // 计算优化后的性能
  let optimized_operations = multiply(baseline_operations, optimization_factor)
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  let actual_performance = add(optimized_operations, -overhead)
  
  // 计算性能提升百分比
  let improvement = add(actual_performance, -baseline_operations)
  let improvement_percentage = divide_with_ceil(multiply(improvement, 100), baseline_operations)
  
  assert_eq(3000, optimized_operations)
  assert_eq(300, overhead)
  assert_eq(2700, actual_performance)
  assert_eq(170, improvement_percentage)
}

test "resource_allocation_problem" {
  // 资源分配问题
  let total_budget = 5000
  let project_costs = [1200, 800, 1500, 900]
  
  // 计算项目总成本
  let total_cost = add(add(add(project_costs[0], project_costs[1]), project_costs[2]), project_costs[3])
  
  // 计算剩余预算
  let remaining_budget = add(total_budget, -total_cost)
  
  // 计算可以支持的小型项目数量
  let small_project_cost = 300
  let additional_projects = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(4400, total_cost)
  assert_eq(600, remaining_budget)
  assert_eq(2, additional_projects)
}