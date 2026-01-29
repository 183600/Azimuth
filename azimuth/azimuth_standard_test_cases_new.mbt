// 标准 MoonBit 测试用例 - 新增10个高质量测试用例
// 为 azimuth 库添加额外的测试覆盖

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(15, add(7, 8))
  assert_eq(-1, add(-7, 6))
  assert_eq(0, add(-5, 5))
  
  assert_eq(56, multiply(7, 8))
  assert_eq(-42, multiply(-7, 6))
  assert_eq(0, multiply(0, 123))
  
  assert_eq(3, subtract(10, 7))
  assert_eq(-13, subtract(-7, 6))
  assert_eq(0, subtract(5, 5))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  
  // 大数测试
  assert_eq(1000, divide_with_ceil(1000000, 1000))
  assert_eq(1001, divide_with_ceil(1000001, 1000))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties_verification" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律
  assert_eq(
    multiply(x, add(y, 3)),
    add(multiply(x, y), multiply(x, 3))
  )
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "business_logic_packaging" {
  // 业务逻辑：包装计算测试
  let items = 47
  let capacity_per_box = 12
  let boxes_needed = divide_with_ceil(items, capacity_per_box)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, capacity_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, items)
  assert_eq(1, remaining_space)
}

test "project_management_scenario" {
  // 项目管理场景测试
  let team_size = 5
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  assert_eq(40, total_tasks)
  
  let sprint_duration = 2
  let tasks_per_sprint = multiply(team_size, sprint_duration)
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  assert_eq(4, sprints_needed)
}

test "error_handling_comprehensive" {
  // 错误处理综合测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 极值验证
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let base_value = 15
  let multiplier = 3
  let divisor = 4
  let addition = 7
  
  // 计算流程：((base * multiplier) / divisor) + addition
  let step1 = multiply(base_value, multiplier)
  let step2 = divide_with_ceil(step1, divisor)
  let result = add(step2, addition)
  
  assert_eq(19, result)  // ceil(15*3/4) + 7 = 12 + 7 = 19
}

test "resource_allocation_optimization" {
  // 资源分配优化测试
  let total_resources = 100
  let teams = 3
  let min_resources_per_team = divide_with_ceil(total_resources, teams)
  
  assert_eq(34, min_resources_per_team)
  
  // 验证分配结果
  let allocated_total = multiply(min_resources_per_team, teams)
  assert_eq(102, allocated_total)
  
  // 验证资源利用率
  let utilization = divide_with_ceil(multiply(total_resources, 100), allocated_total)
  assert_eq(98, utilization)  // floor(10000/102) = 98%
}

test "real_world_event_planning" {
  // 真实世界活动规划测试
  let participants = 87
  let tables = 8
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
  
  // 验证座位充足性 - 通过计算差值来验证
  let seat_difference = subtract(available_seats, participants)
  assert_eq(3, seat_difference)  // 90 - 87 = 3，说明座位充足
}