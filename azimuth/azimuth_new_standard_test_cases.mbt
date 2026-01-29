// Azimuth 新增标准测试用例
// 包含10个高质量的标准 MoonBit 测试用例

test "add_function_comprehensive_test" {
  // 加法函数综合测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(-1, add(7, -8))
  assert_eq(1000000, add(500000, 500000))
}

test "multiply_function_comprehensive_test" {
  // 乘法函数综合测试
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(0, 12345))
  assert_eq(56, multiply(-7, -8))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(-56, multiply(7, -8))
  assert_eq(1, multiply(1, 12345))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_comprehensive_test" {
  // 减法函数综合测试
  assert_eq(-1, subtract(7, 8))
  assert_eq(1, subtract(8, 7))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(7, -8))
  assert_eq(-15, subtract(-7, 8))
  assert_eq(0, subtract(0, 0))
  assert_eq(1000000, subtract(1000000, 0))
}

test "divide_with_ceil_function_comprehensive_test" {
  // 向上取整除法函数综合测试
  assert_eq(3, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_comprehensive_test" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, café!", greet("café"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 25
  let y = 17
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 加法结合律
  let z = 10
  assert_eq(add(add(x, y), z), add(x, add(y, z)))
  
  // 乘法结合律
  assert_eq(multiply(multiply(x, y), z), multiply(x, multiply(y, z)))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, z)),
    add(multiply(x, y), multiply(x, z))
  )
}

test "business_logic_packaging_calculation" {
  // 业务逻辑：包装计算测试
  let total_items = 157
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(11, remaining_space)
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  // 场景：项目资源管理计算
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  assert_eq(40, total_tasks)
  
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)
  
  let overhead_tasks = 5
  let final_tasks = add(total_tasks, overhead_tasks)
  let final_sprints = divide_with_ceil(final_tasks, tasks_per_sprint)
  assert_eq(3, final_sprints)
}

test "edge_cases_and_boundary_conditions" {
  // 边界情况和极值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(5, subtract(5, 0))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(5, subtract(5, 0))
  
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动安排
  let participants = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
  
  // 测试问候函数用于活动邀请
  let event_name = "Annual Conference"
  let greeting = greet(event_name)
  assert_eq_string("Hello, Annual Conference!", greeting)
}