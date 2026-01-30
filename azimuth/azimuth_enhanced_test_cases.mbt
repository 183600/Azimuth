// Azimuth 增强测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(-3, subtract(5, 8))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "boundary_values_extreme" {
  // 极值边界测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 999999))
  assert_eq(999999, multiply(1, 999999))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(999999, divide_with_ceil(999999, 1))
  assert_eq(1, divide_with_ceil(999999, 999999))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(10, add(-15, 25))
  assert_eq(25, multiply(-5, -5))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(2, divide_with_ceil(-7, -3))
}

test "string_processing_international" {
  // 字符串国际化处理测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
}

test "complex_business_logic" {
  // 复杂业务逻辑测试：资源分配优化
  let total_budget = 50000
  let departments = 7
  let min_budget_per_dept = divide_with_ceil(total_budget, departments)
  
  assert_eq(7143, min_budget_per_dept)
  
  let total_allocated = multiply(min_budget_per_dept, departments)
  let remaining_budget = subtract(total_allocated, total_budget)
  
  assert_eq(50001, total_allocated)
  assert_eq(1, remaining_budget)
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 24
  let y = 18
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 5)),
    add(multiply(x, y), multiply(x, 5))
  )
  
  // 结合律验证
  assert_eq(
    multiply(multiply(x, y), 2),
    multiply(x, multiply(y, 2))
  )
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(2147483647, 0))
  
  // 边界值运算
  assert_eq(0, multiply(2147483647, 0))
  assert_eq(2147483647, multiply(2147483647, 1))
  assert_eq(-2147483647, multiply(-2147483647, 1))
}

test "performance_large_scale" {
  // 大规模运算性能测试
  let large_num = 100000
  let iterations = 100
  
  let sum_result = add(large_num, multiply(large_num, iterations))
  let product_result = multiply(large_num, iterations)
  let division_result = divide_with_ceil(product_result, large_num)
  
  assert_eq(10100000, sum_result)
  assert_eq(10000000, product_result)
  assert_eq(100, division_result)
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let product_a = 156
  let product_b = 89
  let product_c = 234
  let box_capacity = 25
  
  let boxes_a = divide_with_ceil(product_a, box_capacity)
  let boxes_b = divide_with_ceil(product_b, box_capacity)
  let boxes_c = divide_with_ceil(product_c, box_capacity)
  
  assert_eq(7, boxes_a)   // ceil(156/25) = 7
  assert_eq(4, boxes_b)   // ceil(89/25) = 4
  assert_eq(10, boxes_c)  // ceil(234/25) = 10
  
  let total_boxes = add(add(boxes_a, boxes_b), boxes_c)
  let total_capacity = multiply(total_boxes, box_capacity)
  let total_products = add(add(product_a, product_b), product_c)
  
  assert_eq(21, total_boxes)
  assert_eq(525, total_capacity)
  assert_eq(479, total_products)
  
  let unused_space = subtract(total_capacity, total_products)
  assert_eq(46, unused_space)
}

test "comprehensive_calculation_pipeline" {
  // 综合计算流水线测试
  // 场景：项目成本计算与资源分配
  
  let team_size = 12
  let project_duration = 8
  let hourly_rate = 75
  let overhead_percentage = 20
  
  // 计算总工时
  let total_hours = multiply(team_size, multiply(project_duration, 40))  // 8周，每周40小时
  assert_eq(3840, total_hours)
  
  // 计算基础成本
  let base_cost = multiply(total_hours, hourly_rate)
  assert_eq(288000, base_cost)
  
  // 计算管理费用（向上取整）
  let overhead_cost = divide_with_ceil(multiply(base_cost, overhead_percentage), 100)
  assert_eq(57600, overhead_cost)
  
  // 计算总成本
  let total_cost = add(base_cost, overhead_cost)
  assert_eq(345600, total_cost)
  
  // 计算每个冲刺的成本（项目分为4个冲刺）
  let sprints = 4
  let cost_per_sprint = divide_with_ceil(total_cost, sprints)
  assert_eq(86400, cost_per_sprint)
  
  // 验证总冲刺成本
  let total_sprint_cost = multiply(cost_per_sprint, sprints)
  assert_eq(345600, total_sprint_cost)
  
  // 计算每个团队成员的冲刺成本
  let cost_per_person_per_sprint = divide_with_ceil(cost_per_sprint, team_size)
  assert_eq(7200, cost_per_person_per_sprint)
}