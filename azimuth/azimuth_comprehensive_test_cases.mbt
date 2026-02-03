// Azimuth 综合测试用例 - 10个高质量标准 MoonBit 测试用例

test "basic_arithmetic_comprehensive" {
  // 基本算术运算综合测试
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-10, 6))
  assert_eq(0, add(5, -5))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 100))
  
  assert_eq(-2, subtract(5, 7))
  assert_eq(12, subtract(20, 8))
  assert_eq(0, subtract(15, 15))
}

test "divide_with_ceil_comprehensive_scenarios" {
  // 向上取整除法综合场景测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  
  // 边界情况
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(5, 5))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, São Paulo!", greet("São Paulo"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 加法恒等式
  assert_eq(a, add(a, 0))
  assert_eq(a, add(0, a))
  
  // 乘法恒等式
  assert_eq(a, multiply(a, 1))
  assert_eq(a, multiply(1, a))
  assert_eq(0, multiply(a, 0))
}

test "business_logistics_calculation" {
  // 业务物流计算测试
  // 场景：仓库货物装箱
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
  
  // 验证空间利用率
  let utilization_percent = divide_with_ceil(multiply(total_items, 100), total_capacity)
  assert_eq(94, utilization_percent)  // floor(15700/168) = 93%
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 8
  let tasks_per_member = 6
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 8 * 6 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  
  // 验证总处理能力
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  assert_eq(60, total_capacity)
  
  // 验证剩余处理能力
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, remaining_capacity)
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000001, divide_with_ceil(1000000, 1))
  
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 100))
  assert_eq(0, divide_with_ceil(100, 0))
  
  // 单位值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(2, divide_with_ceil(3, 2))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零错误处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数运算稳定性
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(10000000000, multiply(large_num, large_num))
  assert_eq(1, divide_with_ceil(large_num, large_num))
  
  // 负数运算稳定性
  assert_eq(-200000, add(-large_num, -large_num))
  assert_eq(10000000000, multiply(-large_num, -large_num))
  assert_eq(-1, divide_with_ceil(-large_num, large_num))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  // 场景：会议座位安排
  let attendees = 125
  let tables_available = 15
  let chairs_per_table = 8
  let venue_capacity = 200
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(16, tables_needed)
  
  // 检查桌子可用性
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(128, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(3, remaining_seats)
  
  // 验证场地容量充足
  assert_true(total_seats <= venue_capacity)
  
  // 计算场地利用率
  let venue_utilization = divide_with_ceil(multiply(total_seats, 100), venue_capacity)
  assert_eq(64, venue_utilization)  // floor(12800/200) = 64%
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  
  // 分配律：a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  
  // 验证具体数值
  assert_eq(42, left_side)  // 6 * (4 + 3) = 6 * 7 = 42
  assert_eq(42, right_side)  // 6 * 4 + 6 * 3 = 24 + 18 = 42
  
  // 另一种分配律形式测试
  let x = 8
  let y = 5
  let z = 2
  
  let distributive_result = add(multiply(x, y), multiply(x, z))
  let factored_result = multiply(x, add(y, z))
  assert_eq(distributive_result, factored_result)
}