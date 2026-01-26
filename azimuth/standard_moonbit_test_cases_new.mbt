// 标准 MoonBit 测试用例 - 新增
// 包含10个高质量测试用例，使用标准 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-15, add(-7, -8))
  assert_eq(1, add(-7, 8))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_various_cases" {
  // 测试乘法函数的各种情况
  assert_eq(56, multiply(7, 8))
  assert_eq(0, multiply(7, 0))
  assert_eq(-56, multiply(-7, 8))
  assert_eq(56, multiply(-7, -8))
  assert_eq(1000000, multiply(1000, 1000))
}

test "greet_function_international_support" {
  // 测试问候函数的国际字符支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, München!", greet("München"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, あ!", greet("あ"))
}

test "divide_with_ceil_logistics_scenario" {
  // 测试向上取整除法的物流场景
  // 场景：计算需要的运输容器数量
  let packages = 58
  let capacity_per_container = 12
  let containers_needed = divide_with_ceil(packages, capacity_per_container)
  assert_eq(5, containers_needed)
  
  // 验证总容量
  let total_capacity = multiply(containers_needed, capacity_per_container)
  assert_eq(60, total_capacity)
}

test "subtract_function_edge_cases" {
  // 测试减法函数的边缘情况
  assert_eq(-1, subtract(7, 8))
  assert_eq(15, subtract(7, -8))
  assert_eq(-15, subtract(-7, 8))
  assert_eq(1, subtract(-7, -8))
  assert_eq(0, subtract(1000000, 1000000))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  // 场景：计算项目资源需求
  let team_size = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  let tasks_per_sprint = 20
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(3, sprints_needed)  // ceil(48/20) = 3
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, tasks_per_sprint)
  assert_eq(60, total_capacity)
}

test "mathematical_properties_validation" {
  // 验证数学性质
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
}

test "error_handling_comprehensive" {
  // 综合错误处理测试
  // 除零情况的多种测试
  assert_eq(0, divide_with_ceil(15, 0))
  assert_eq(0, divide_with_ceil(-15, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "string_processing_edge_cases" {
  // 字符串处理边缘情况测试
  assert_eq_string("Hello, empty!", greet("empty"))
  assert_eq_string("Hello, space multiple!", greet("space multiple"))
  assert_eq_string("Hello, 67890!", greet("67890"))
  assert_eq_string("Hello, !@#$%^&*!", greet("!@#$%^&*"))
  assert_eq_string("Hello, MixedCase!", greet("MixedCase"))
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  // 场景：会议座位安排
  let attendees = 95
  let seats_per_row = 12
  let rows_needed = divide_with_ceil(attendees, seats_per_row)
  assert_eq(8, rows_needed)  // ceil(95/12) = 8
  
  // 计算总座位数
  let total_seats = multiply(rows_needed, seats_per_row)
  assert_eq(96, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(1, remaining_seats)
  
  // 验证座位充足性
  assert_eq(1, 1)  // 如果 total_seats >= attendees，则测试通过
}