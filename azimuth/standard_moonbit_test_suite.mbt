// 标准 MoonBit 测试套件
// 为 Azimuth 项目添加的高质量测试用例
// 使用标准 MoonBit 测试语法

test "azimuth_standard_numeric_precision" {
  // 数值精度测试
  assert_eq(1, divide_with_ceil(1, 2))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(1000, divide_with_ceil(1999, 2))
  assert_eq(667, divide_with_ceil(2000, 3))
  assert_eq(500001, divide_with_ceil(1000001, 2))
}

test "azimuth_standard_string_validation" {
  // 字符串验证测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 用户123!", greet("用户123"))
  assert_eq_string("Hello, @#$%!", greet("@#$%"))
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "azimuth_standard_zero_handling" {
  // 零值处理测试
  assert_eq(0, add(0, 0))
  assert_eq(5, add(5, 0))
  assert_eq(-5, add(-5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "azimuth_standard_negative_arithmetic" {
  // 负数算术测试
  assert_eq(-8, add(-5, -3))
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(5, -3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "azimuth_standard_large_number_operations" {
  // 大数运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(0, add(1000000, -1000000))
  assert_eq(1000000000000, multiply(1000000, 1000000))
  assert_eq(1000, divide_with_ceil(999999, 1000))
  assert_eq(1000001, divide_with_ceil(1000001, 1))
}

test "azimuth_standard_business_calculation" {
  // 业务计算测试：资源分配场景
  let total_tasks = 87
  let team_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, team_capacity)
  
  assert_eq(5, sprints_needed)
  
  // 验证总容量
  let total_capacity = multiply(sprints_needed, team_capacity)
  assert_eq(100, total_capacity)
  
  // 验证剩余容量
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(13, remaining_capacity)
}

test "azimuth_standard_mathematical_properties" {
  // 数学性质验证测试
  let a = 17
  let b = 23
  
  // 交换律验证
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 结合律验证
  assert_eq(add(add(a, b), 5), add(a, add(b, 5)))
  assert_eq(multiply(multiply(a, b), 2), multiply(a, multiply(b, 2)))
  
  // 分配律验证
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "azimuth_standard_edge_case_division" {
  // 边界情况除法测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(-1, divide_with_ceil(1, -1))
  assert_eq(-1, divide_with_ceil(-1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "azimuth_standard_sequential_operations" {
  // 连续操作测试
  let initial = 10
  let step1 = add(initial, multiply(3, 4))     // 10 + 3*4 = 22
  let step2 = divide_with_ceil(step1, 5)       // ceil(22/5) = 5
  let step3 = multiply(step2, subtract(8, 3))  // 5 * (8-3) = 25
  let step4 = divide_with_ceil(step3, 6)       // ceil(25/6) = 5
  let final_result = add(step4, 2)             // 5 + 2 = 7
  
  assert_eq(7, final_result)
}

test "azimuth_standard_comprehensive_validation" {
  // 综合验证测试：复杂业务场景
  // 场景：活动策划
  let guests = 156
  let tables_per_room = 8
  let chairs_per_table = 6
  let rooms_available = 4
  let cost_per_chair = 25
  let total_budget = 3000
  
  // 计算需要的桌子数
  let tables_needed = divide_with_ceil(guests, chairs_per_table)
  assert_eq(26, tables_needed)
  
  // 计算需要的房间数
  let rooms_needed = divide_with_ceil(tables_needed, tables_per_room)
  assert_eq(4, rooms_needed)
  
  // 验证房间足够
  assert_true(rooms_needed <= rooms_available)
  
  // 计算总椅子数
  let total_chairs = multiply(tables_needed, chairs_per_table)
  assert_eq(156, total_chairs)
  
  // 计算总成本
  let total_cost = multiply(total_chairs, cost_per_chair)
  assert_eq(3900, total_cost)
  
  // 验证超出的预算
  let budget_exceeded = subtract(total_cost, total_budget)
  assert_eq(900, budget_exceeded)
}