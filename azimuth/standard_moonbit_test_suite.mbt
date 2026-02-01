// 标准 MoonBit 测试套件 - Azimuth 项目
// 包含 10 个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(3, 5))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(7, 5))
  assert_eq(3, divide_with_ceil(8, 3))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 17
  let b = 23
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(40, add(17, 23))
  assert_eq(391, multiply(17, 23))
}

test "divide_with_ceil_functionality" {
  // 向上取整除法功能测试
  // 正数除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 10))
  
  // 除零处理
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数测试
  assert_eq(1000, divide_with_ceil(9999, 10))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "negative_numbers_handling" {
  // 负数处理测试
  // 加法负数测试
  assert_eq(-2, add(-5, 3))
  assert_eq(2, add(-3, 5))
  assert_eq(-8, add(-5, -3))
  assert_eq(0, add(-5, 5))
  
  // 乘法负数测试
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(0, multiply(-3, 0))
  
  // 向上取整除法负数测试
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  // 验证需要的箱子数量
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, items_per_box)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源分配计算
  let team_size = 8
  let tasks_per_person = 6
  let total_tasks = multiply(team_size, tasks_per_person)
  
  let sprint_duration = 10
  let daily_capacity = 5
  let sprint_capacity = multiply(sprint_duration, daily_capacity)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)     // 8 * 6 = 48
  assert_eq(50, sprint_capacity) // 10 * 5 = 50
  assert_eq(1, sprints_needed)   // ceil(48/50) = 1
}

test "mathematical_identity_properties" {
  // 数学恒等式性质测试
  let x = 42
  
  // 加法恒等式
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等式
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  assert_eq(0, multiply(x, 0))
  
  // 减法恒等式
  assert_eq(0, subtract(x, x))
  assert_eq(x, subtract(x, 0))
}

test "edge_cases_and_boundary_values" {
  // 边界情况和极值测试
  // 零值操作
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 单位值操作
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, multiply(5, 1))
  assert_eq(5, add(5, 0))
  
  // 大数操作（在安全范围内）
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "real_world_application_scenario" {
  // 真实世界应用场景测试：活动座位安排
  let attendees = 87
  let tables_available = 12
  let chairs_per_table = 8
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(11, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(88, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(1, remaining_seats)
  
  // 验证座位充足性
  assert_true(total_seats >= attendees)
}