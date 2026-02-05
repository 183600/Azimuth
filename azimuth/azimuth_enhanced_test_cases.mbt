// Azimuth 项目新增的标准 MoonBit 测试用例
// 包含8个高质量测试用例，覆盖各种场景和边界情况

test "arithmetic_identity_properties" {
  // 算术恒等式性质测试
  let x = 42
  
  // 加法恒等式：x + 0 = x
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式：x * 1 = x
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元素：x * 0 = 0
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法的综合场景测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "mathematical_distributive_property" {
  // 数学分配律性质测试
  let a = 6
  let b = 4
  let c = 3
  
  // 乘法对加法的分配律：a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "string_greeting_internationalization" {
  // 字符串问候国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试：项目资源管理
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_duration = 2
  let tasks_per_day = 10
  let sprint_capacity = multiply(sprint_duration, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(40, total_tasks)
  assert_eq(20, sprint_capacity)
  assert_eq(2, sprints_needed)
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 极小值
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 极大值（安全范围内）
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(10001, divide_with_ceil(10000, 1))
  
  // 特殊边界
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
}

test "sequential_calculation_chain" {
  // 连续计算链测试
  let initial = 5
  let step1 = add(initial, multiply(2, 3))     // 5 + 2*3 = 11
  let step2 = divide_with_ceil(step1, 4)       // ceil(11/4) = 3
  let step3 = multiply(step2, add(1, 2))       // 3 * (1+2) = 9
  let step4 = divide_with_ceil(step3, 5)       // ceil(9/5) = 2
  let final_result = add(step4, 3)             // 2 + 3 = 5
  
  assert_eq(5, final_result)
}

test "real_world_inventory_management" {
  // 真实世界库存管理测试
  let total_products = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 验证空间利用率（百分比）
  let utilization = divide_with_ceil(multiply(total_products, 100), total_capacity)
  assert_eq(94, utilization)  // floor(15700/168) = 93%
}