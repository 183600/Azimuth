// 新增的标准 MoonBit 测试用例 - 10个高质量测试
// 覆盖基本功能、边界情况、错误处理和实际业务场景

test "basic_arithmetic_validation" {
  // 基本算术运算验证测试
  assert_eq(15, add(7, 8))
  assert_eq(42, multiply(6, 7))
  assert_eq(-1, subtract(5, 6))
  assert_eq(3, divide_with_ceil(8, 3))
}

test "zero_element_properties" {
  // 零元素性质测试
  assert_eq(5, add(5, 0))
  assert_eq(5, add(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, divide_with_ceil(0, 10))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-3, add(-5, 2))
  assert_eq(10, multiply(-2, -5))
  assert_eq(-10, multiply(2, -5))
  assert_eq(-2, divide_with_ceil(-7, 4))
  assert_eq(2, divide_with_ceil(-7, -4))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(100, 100))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "string_greeting_comprehensive" {
  // 字符串问候综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_commutative_laws" {
  // 数学交换律验证
  let a = 13
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体计算结果
  assert_eq(30, add(13, 17))
  assert_eq(221, multiply(13, 17))
}

test "packaging_calculation_scenario" {
  // 包装计算业务场景
  let total_items = 67
  let items_per_package = 15
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(5, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(75, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(8, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目任务分配
  let team_members = 5
  let tasks_per_member = 12
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(60, total_tasks)  // 5 * 12 = 60
  assert_eq(3, sprints_needed)  // ceil(60/20) = 3
}

test "error_handling_comprehensive" {
  // 错误处理综合测试
  // 除零错误的各种情况
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(-10, 0))
  
  // 极值运算
  assert_eq(0, multiply(1000000, 0))
  assert_eq(1000000, add(1000000, 0))
}

test "real_world_resource_allocation" {
  // 真实世界资源分配测试
  // 场景：会议室座位安排
  let attendees = 85
  let chairs_per_row = 8
  let rows_available = 12
  
  // 计算需要的行数
  let rows_needed = divide_with_ceil(attendees, chairs_per_row)
  assert_eq(11, rows_needed)
  
  // 检查可用行数是否足够
  assert_true(rows_needed <= rows_available)
  
  // 计算总座位数
  let total_seats = multiply(rows_needed, chairs_per_row)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let empty_seats = subtract(total_seats, attendees)
  assert_eq(3, empty_seats)
}