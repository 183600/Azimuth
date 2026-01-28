// Azimuth 综合测试套件 - 高质量 MoonBit 测试用例
// 测试覆盖主要功能：add, multiply, greet, divide_with_ceil, subtract

test "add_function_various_scenarios" {
  // 测试加法函数的各种场景
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_comprehensive" {
  // 测试乘法函数的综合情况
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(56, multiply(-7, -8))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(-1, subtract(7, 8))
  assert_eq(1, subtract(8, 7))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(-7, -22))
  assert_eq(-15, subtract(-22, -7))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  assert_eq(3, divide_with_ceil(7, 3))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(0, divide_with_ceil(7, 0))
}

test "business_logic_inventory_management" {
  // 业务逻辑：库存管理测试
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let x = 42
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 12
  let multiplier = 3
  let divisor = 5
  let addition = 7
  
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(14, result)  // ceil(12*3/5) + 7 = 8 + 7 = 15
}

test "error_handling_and_boundary_conditions" {
  // 错误处理和边界条件测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_project_planning" {
  // 真实世界项目规划测试
  let project_tasks = 87
  let team_members = 8
  let tasks_per_person_per_week = 3
  
  let weekly_capacity = multiply(team_members, tasks_per_person_per_week)
  let weeks_needed = divide_with_ceil(project_tasks, weekly_capacity)
  
  assert_eq(4, weeks_needed)  // ceil(87/(8*3)) = 4
  
  let total_capacity = multiply(weeks_needed, weekly_capacity)
  assert_eq(96, total_capacity)
  
  let buffer_tasks = subtract(total_capacity, project_tasks)
  assert_eq(9, buffer_tasks)
}