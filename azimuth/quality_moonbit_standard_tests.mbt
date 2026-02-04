// 高质量 MoonBit 标准测试用例
// 包含 10 个标准测试用例，使用标准的 MoonBit 测试语法

test "quality_arithmetic_operations" {
  // 高质量算术运算测试
  assert_eq(15, add(7, 8))
  assert_eq(56, multiply(7, 8))
  assert_eq(-1, add(-3, 2))
  assert_eq(-14, multiply(-2, 7))
  assert_eq(28, multiply(-4, -7))
}

test "quality_divide_with_ceil_comprehensive" {
  // 高质量向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "quality_divide_with_ceil_negative_cases" {
  // 高质量向上取整除法负数情况测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "quality_string_greeting_functions" {
  // 高质量字符串问候函数测试
  assert_eq_string("Hello, Universe!", greet("Universe"))
  assert_eq_string("Hello, 北京!", greet("北京"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, empty!", greet(""))
}

test "quality_mathematical_properties" {
  // 高质量数学性质测试
  let a = 25
  let b = 15
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(40, add(25, 15))
  assert_eq(375, multiply(25, 15))
}

test "quality_business_packaging_scenario" {
  // 高质量业务包装场景测试
  let total_products = 250
  let box_capacity = 30
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(9, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(270, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(20, remaining_space)
}

test "quality_complex_workflow_calculation" {
  // 高质量复杂工作流计算测试
  // 场景：项目管理
  let team_size = 12
  let weeks_per_sprint = 2
  let tasks_per_person_per_week = 3
  let total_tasks = 150
  
  // 计算每周处理能力
  let weekly_capacity = multiply(team_size, tasks_per_person_per_week)
  assert_eq(36, weekly_capacity)
  
  // 计算冲刺处理能力
  let sprint_capacity = multiply(weekly_capacity, weeks_per_sprint)
  assert_eq(72, sprint_capacity)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
}

test "quality_boundary_values" {
  // 高质量边界值测试
  // 极小值测试
  assert_eq(1, add(1, 0))
  assert_eq(0, multiply(1, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 极大值测试
  assert_eq(20000, add(10000, 10000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(10001, divide_with_ceil(10000, 1))
  
  // 边界除法测试
  assert_eq(1, divide_with_ceil(9999, 10000))
  assert_eq(10000, divide_with_ceil(10000, 1))
  assert_eq(1, divide_with_ceil(10000, 10000))
}

test "quality_error_handling" {
  // 高质量错误处理测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(9999, 0))
  assert_eq(0, divide_with_ceil(-9999, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值运算测试
  let max_value = 1000000
  let min_value = -1000000
  
  assert_eq(0, multiply(max_value, 0))
  assert_eq(0, multiply(min_value, 0))
  assert_eq(max_value, add(max_value, 0))
  assert_eq(min_value, add(min_value, 0))
}

test "quality_real_world_application" {
  // 高质量真实世界应用场景测试：活动安排
  let participants = 87
  let chairs_per_table = 10
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(9, tables_needed)
  
  // 计算可用座位数
  let available_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(90, available_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(available_seats, participants)
  assert_eq(3, remaining_seats)
  
  // 验证座位充足性
  assert_true(available_seats >= participants)
}