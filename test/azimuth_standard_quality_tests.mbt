// Azimuth 标准质量测试用例
// 包含 8 个高质量的标准 MoonBit 测试用例

test "arithmetics_add_comprehensive" {
  // 全面的加法测试
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-2, 7))
  assert_eq(1000000, add(500000, 500000))
}

test "arithmetics_multiply_properties" {
  // 乘法性质测试
  assert_eq(21, multiply(7, 3))
  assert_eq(21, multiply(3, 7))  // 交换律
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
}

test "string_greeting_international" {
  // 国际化字符串问候测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "division_ceil_business_logic" {
  // 向上取整除法的业务逻辑测试
  // 场景：计算需要的运输车辆
  let packages = 47
  let capacity_per_truck = 10
  let trucks_needed = divide_with_ceil(packages, capacity_per_truck)
  assert_eq(5, trucks_needed)
  
  // 验证总运力
  let total_capacity = multiply(trucks_needed, capacity_per_truck)
  assert_eq(50, total_capacity)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(45, total_capacity)
}

test "mathematical_identity_verification" {
  // 数学恒等式验证测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  assert_eq(-x, multiply(x, -1))
}

test "error_boundary_conditions" {
  // 错误边界条件测试
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 边界值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试
  // 场景：活动座位安排
  let participants = 87
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证座位充足
  assert_true(total_seats >= participants)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
}