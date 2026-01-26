// 标准 MoonBit 测试套件 - 包含10个高质量测试用例
// 测试 azimuth 库的核心功能

test "add_function_basic_operations" {
  // 测试加法函数的基本运算
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(1, add(-2, 3))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(6, multiply(-2, -3))
  assert_eq(10000, multiply(100, 100))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_business_logic" {
  // 测试向上取整除法的业务逻辑场景
  // 场景：计算需要的包装箱数量
  let items = 25
  let capacity_per_box = 6
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  assert_eq(5, boxes_needed)  // ceil(25/6) = 5
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(30, total_capacity)
}

test "subtract_function_basic_operations" {
  // 测试减法函数的基本运算
  assert_eq(2, subtract(5, 3))
  assert_eq(0, subtract(5, 5))
  assert_eq(-2, subtract(3, 5))
  assert_eq(10, subtract(5, -5))
  assert_eq(-10, subtract(-5, 5))
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_pipeline" {
  // 测试复杂计算流水线
  // 场景：计算项目资源需求
  let team_size = 8
  let tasks_per_person = 5
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)  // ceil(40/15) = 3
}

test "edge_cases_and_boundaries" {
  // 测试边缘情况和边界条件
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, multiply(0, 12345))
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 10000
  let team_members = 6
  let max_share = divide_with_ceil(total_budget, team_members)
  assert_eq(1667, max_share)  // ceil(10000/6) = 1667
  
  // 验证总分配
  let total_allocated = multiply(max_share, team_members)
  assert_eq(10002, total_allocated)
}

test "real_world_application_scenario" {
  // 测试真实世界应用场景
  // 场景：活动座位安排
  let participants = 87
  let chairs_per_table = 8
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  assert_eq(11, tables_needed)  // ceil(87/8) = 11
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= participants)
}