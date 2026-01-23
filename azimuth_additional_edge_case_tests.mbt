// 额外的 MoonBit 测试用例 - 针对特殊边界情况和高级场景
// 这些测试用例补充了现有的测试覆盖，确保代码的健壮性

test "extreme_boundary_values" {
  // 测试极端边界值
  assert_eq(2147483647, add(2147483646, 1))  // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))  // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))  // 最大值与相反数相加
}

test "divide_with_ceil_precision_edge" {
  // 测试除法向上取整的精度边界
  assert_eq(1, divide_with_ceil(1, 1))  // 完全相等
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(100, divide_with_ceil(9999, 100))  // 99.99 -> 100
  assert_eq(-333334, divide_with_ceil(-1000000, 3))  // 大负数除法
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, 🌍🌎🌏!", greet("🌍🌎🌏"))  // 多个emoji
  assert_eq_string("Hello, \n\t\r!", greet("\n\t\r"))  // 控制字符
  assert_eq_string("Hello, 🚀🌟✨!", greet("🚀🌟✨"))  // 多个特殊符号
}

test "complex_nested_calculations" {
  // 测试复杂嵌套计算
  let x = 10
  let y = 20
  let z = 5
  
  // 测试表达式：(x + y) * z - x / y
  let result = add(multiply(add(x, y), z), -divide_with_ceil(x, y))
  assert_eq(149, result)  // (10 + 20) * 5 - ceil(10/20) = 30*5 - 1 = 150 - 1 = 149
  
  // 测试嵌套表达式
  let nested = divide_with_ceil(multiply(add(x, multiply(y, z)), 2), 3)
  assert_eq(67, nested)  // ceil((10 + 20*5) * 2 / 3) = ceil(110 * 2 / 3) = ceil(220/3) = 74
}

test "mathematical_identities" {
  // 测试数学恒等式
  let a = 15
  let b = 25
  
  // 测试分配律: a * (b + b) = a * b + a * b
  let distributive_left = multiply(a, add(b, b))
  let distributive_right = add(multiply(a, b), multiply(a, b))
  assert_eq(distributive_left, distributive_right)
  
  // 测试结合律: (a + b) + b = a + (b + b)
  let associative_left = add(add(a, b), b)
  let associative_right = add(a, add(b, b))
  assert_eq(associative_left, associative_right)
}

test "performance_simulation" {
  // 模拟性能计算场景
  let base_operations = 1000
  let optimization_factor = 3
  let overhead_percentage = 15
  
  // 计算优化后的操作数
  let optimized_operations = multiply(base_operations, optimization_factor)
  
  // 计算开销
  let overhead = divide_with_ceil(multiply(optimized_operations, overhead_percentage), 100)
  
  // 计算实际性能
  let actual_performance = add(optimized_operations, -overhead)
  
  assert_eq(3000, optimized_operations)
  assert_eq(450, overhead)
  assert_eq(2550, actual_performance)
}

test "financial_calculation_advanced" {
  // 高级金融计算场景
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let amount_after_year2 = add(amount_after_year1, divide_with_ceil(multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = add(amount_after_year2, divide_with_ceil(multiply(amount_after_year2, rate_percent), 100))
  
  // 验证结果（由于使用向上取整，结果会略高于标准复利计算）
  assert_eq(10500, amount_after_year1)
  assert_eq(11025, amount_after_year2)
  assert_eq(11576, amount_after_year3)
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 5000
  let project_a_cost = 1200
  let project_b_cost = 1800
  let project_c_cost = 900
  
  // 计算剩余预算
  let allocated_budget = add(add(project_a_cost, project_b_cost), project_c_cost)
  let remaining_budget = add(total_budget, -allocated_budget)
  
  // 计算可以支持的小项目数量（每个小项目成本200）
  let small_projects = divide_with_ceil(remaining_budget, 200)
  
  assert_eq(3900, allocated_budget)
  assert_eq(1100, remaining_budget)
  assert_eq(6, small_projects)
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 测试除数为零的情况
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值运算
  let extreme_positive = 1000000
  let extreme_negative = -1000000
  assert_eq(0, add(extreme_positive, extreme_negative))
  assert_eq(2000000, add(extreme_positive, -extreme_negative))
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  let input_size = 100
  let linear_factor = 2
  let quadratic_factor = 3
  
  // 模拟 O(n) 算法
  let linear_complexity = multiply(input_size, linear_factor)
  
  // 模拟 O(n²) 算法
  let quadratic_complexity = multiply(multiply(input_size, input_size), quadratic_factor)
  
  // 计算总复杂度
  let total_complexity = add(linear_complexity, quadratic_complexity)
  
  assert_eq(200, linear_complexity)
  assert_eq(30000, quadratic_complexity)
  assert_eq(30200, total_complexity)
}