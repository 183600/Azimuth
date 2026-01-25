// Azimuth 新增测试用例 - 标准MoonBit测试语法
// 包含10个高质量测试用例，覆盖各种功能和场景

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(5, add(-3, 8))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(21, multiply(3, 7))
  assert_eq(0, multiply(0, 123))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, español!", greet("español"))
}

test "divide_with_ceil_logistics_scenario" {
  // 物流场景测试：计算需要的运输车辆
  let packages = 47
  let capacity_per_vehicle = 10
  let vehicles_needed = divide_with_ceil(packages, capacity_per_vehicle)
  assert_eq(5, vehicles_needed)
  
  // 验证总运力
  let total_capacity = multiply(vehicles_needed, capacity_per_vehicle)
  assert_eq(50, total_capacity)
  
  // 验证剩余空间
  let unused_space = subtract(total_capacity, packages)
  assert_eq(3, unused_space)
}

test "complex_project_calculation" {
  // 复杂项目计算场景
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  assert_eq(48, total_tasks)
  
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(4, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(60, total_capacity)
}

test "mathematical_properties_validation" {
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
    add(add(x, y), 5),
    add(x, add(y, 5))
  )
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_budget = 15000
  let departments = 4
  
  // 计算每个部门的最大分配
  let max_allocation = divide_with_ceil(total_budget, departments)
  assert_eq(3750, max_allocation)
  
  // 验证总分配
  let total_allocated = multiply(max_allocation, departments)
  assert_eq(15000, total_allocated)
  
  // 计算平均分配
  let average_allocation = total_budget / departments
  assert_eq(3750, average_allocation)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "string_processing_comprehensive" {
  // 字符串处理综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, mixedCASE!", greet("mixedCASE"))
  assert_eq_string("Hello, space test!", greet("space test"))
}

test "real_world_event_planning" {
  // 真实世界活动规划场景
  let participants = 95
  let tables_available = 8
  let chairs_per_table = 12
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(8, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(96, total_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, participants)
  assert_eq(1, remaining_seats)
  
  // 验证桌子可用性
  assert_true(tables_needed <= tables_available)
}

// 辅助函数：减法
fn subtract(a : Int, b : Int) -> Int {
  a - b
}