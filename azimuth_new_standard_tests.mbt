// Azimuth 项目的新增标准 MoonBit 测试用例
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法函数的各种情况
  assert_eq(15, add(8, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-3, -7))
  assert_eq(4, add(-8, 12))
  assert_eq(100000, add(50000, 50000))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  let a = 6
  let b = 8
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 乘法单位元
  assert_eq(a, multiply(a, 1))
  assert_eq(0, multiply(a, 0))
  
  // 负数乘法
  assert_eq(-48, multiply(-6, 8))
  assert_eq(48, multiply(-6, -8))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  // 零除数处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 精确整除
  assert_eq(5, divide_with_ceil(25, 5))
  assert_eq(-5, divide_with_ceil(-25, 5))
  assert_eq(5, divide_with_ceil(-25, -5))
  
  // 需要向上取整的情况
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(-3, divide_with_ceil(-11, 3))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "business_inventory_calculation" {
  // 库存管理业务场景测试
  let total_products = 158
  let box_capacity = 24
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(7, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(10, remaining_space)
}

test "mathematical_distributive_property" {
  // 测试数学分配律
  let a = 5
  let b = 7
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(3, sprints_needed)
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_capacity)
}

test "subtract_function_comprehensive" {
  // 全面测试减法函数
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
  assert_eq(25, subtract(-5, -30))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  // 除零错误的全面测试
  assert_eq(0, divide_with_ceil(1, 0))
  assert_eq(0, divide_with_ceil(-1, 0))
  assert_eq(0, divide_with_ceil(999999, 0))
  assert_eq(0, divide_with_ceil(-999999, 0))
  
  // 极值运算测试
  let large_num = 100000
  assert_eq(0, multiply(large_num, 0))
  assert_eq(large_num, add(large_num, 0))
  assert_eq(0, subtract(large_num, large_num))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 234
  let tables_available = 30
  let chairs_per_table = 8
  let cost_per_chair = 25
  let total_budget = 6000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(30, tables_needed)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(240, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(6, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(6000, total_cost)
}