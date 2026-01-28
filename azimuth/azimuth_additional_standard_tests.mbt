// Azimuth 额外标准测试用例
// 包含 8 个高质量测试用例，覆盖核心功能

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(10, -5))
  assert_eq(-15, subtract(-10, 5))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(100, divide_with_ceil(199, 2))
  assert_eq(67, divide_with_ceil(200, 3))
  assert_eq(334, divide_with_ceil(1000, 3))
}

test "mathematical_identity_properties_extended" {
  // 扩展数学恒等式测试
  let x = 123
  let y = 456
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(y, add(0, y))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(y, multiply(1, y))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "complex_business_calculation_extended" {
  // 扩展业务计算测试
  // 场景：计算项目预算分配
  let total_budget = 50000
  let departments = 4
  let base_allocation = divide_with_ceil(total_budget, departments)
  
  assert_eq(12500, base_allocation)
  
  // 为每个部门添加应急基金
  let emergency_fund = 2000
  let final_allocation = add(base_allocation, emergency_fund)
  
  assert_eq(14500, final_allocation)
  
  // 计算总分配金额
  let total_allocated = multiply(final_allocation, departments)
  assert_eq(58000, total_allocated)
}

test "string_processing_edge_cases_extended" {
  // 扩展字符串处理边缘情况测试
  assert_eq_string("Hello, empty!", greet(""))
  assert_eq_string("Hello, space!", greet(" "))
  assert_eq_string("Hello, numbers123!", greet("numbers123"))
  assert_eq_string("Hello, mixedCASE123!", greet("mixedCASE123"))
  assert_eq_string("Hello, special@#$%!", greet("special@#$%"))
}

test "resource_optimization_complex_scenario" {
  // 复杂资源优化场景测试
  // 场景：多项目资源分配
  let project_a_tasks = 23
  let project_b_tasks = 41
  let project_c_tasks = 17
  let team_capacity_per_sprint = 20
  
  // 计算每个项目需要的冲刺数
  let sprints_a = divide_with_ceil(project_a_tasks, team_capacity_per_sprint)
  let sprints_b = divide_with_ceil(project_b_tasks, team_capacity_per_sprint)
  let sprints_c = divide_with_ceil(project_c_tasks, team_capacity_per_sprint)
  
  assert_eq(2, sprints_a)  // ceil(23/20)
  assert_eq(3, sprints_b)  // ceil(41/20)
  assert_eq(1, sprints_c)  // ceil(17/20)
  
  // 计算总冲刺数
  let total_sprints = add(add(sprints_a, sprints_b), sprints_c)
  assert_eq(6, total_sprints)
  
  // 计算总工作量
  let total_workload = add(add(project_a_tasks, project_b_tasks), project_c_tasks)
  assert_eq(81, total_workload)
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  assert_eq(0, divide_with_ceil(-2147483648, 0))
  
  // 大数运算的安全性
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
}

test "algorithm_efficiency_validation" {
  // 算法效率验证测试
  // 模拟不同算法的效率比较
  let dataset_size = 1000
  
  // O(n) 算法模拟
  let linear_time = multiply(dataset_size, 5)
  
  // O(log n) 算法模拟（简化）
  let logarithmic_time = divide_with_ceil(dataset_size, 10)
  
  // O(n²) 算法模拟（简化）
  let quadratic_time = multiply(dataset_size, divide_with_ceil(dataset_size, 100))
  
  assert_eq(5000, linear_time)
  assert_eq(100, logarithmic_time)
  assert_eq(10000, quadratic_time)
  
  // 验证算法效率关系
  assert_true(linear_time > logarithmic_time)
  assert_true(quadratic_time > linear_time)
}