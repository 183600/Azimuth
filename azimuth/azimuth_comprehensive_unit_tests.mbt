// Azimuth 项目综合单元测试
// 包含 10 个高质量标准 MoonBit 测试用例
// 覆盖核心功能的各种场景和边缘情况

test "arithmetics_large_numbers" {
  // 大数算术运算测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(1000001, divide_with_ceil(1000000, 1))
  assert_eq(1, divide_with_ceil(1000000, 1000000))
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

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边缘情况测试
  // 零值测试
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(0, -10))
  assert_eq(0, divide_with_ceil(10, 0))
  
  // 精确整除
  assert_eq(5, divide_with_ceil(10, 2))
  assert_eq(7, divide_with_ceil(21, 3))
  
  // 余数情况
  assert_eq(6, divide_with_ceil(11, 2))
  assert_eq(4, divide_with_ceil(13, 4))
}

test "greet_function_internationalization" {
  // 问候函数国际化测试
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, café!", greet("café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, ñoño!", greet("ñoño"))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目管理资源分配
  let team_size = 8
  let tasks_per_member = 6
  let sprint_capacity = 20
  
  // 计算总任务数
  let total_tasks = multiply(team_size, tasks_per_member)
  assert_eq(48, total_tasks)
  
  // 计算需要的冲刺数
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  assert_eq(3, sprints_needed)
  
  // 计算剩余容量
  let total_capacity = multiply(sprints_needed, sprint_capacity)
  let remaining_capacity = subtract(total_capacity, total_tasks)
  assert_eq(12, remaining_capacity)
}

test "mathematical_distributive_property" {
  // 数学分配律性质测试
  let a = 6
  let b = 4
  let c = 3
  
  // 分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "inventory_management_scenario" {
  // 库存管理场景测试
  let total_products = 157
  let box_capacity = 24
  let cost_per_box = 15
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  assert_eq(7, boxes_needed)
  
  // 计算总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(168, total_capacity)
  
  // 计算剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(11, remaining_space)
  
  // 计算总成本
  let total_cost = multiply(boxes_needed, cost_per_box)
  assert_eq(105, total_cost)
}

test "negative_number_operations" {
  // 负数运算测试
  // 加法负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(5, add(-10, 15))
  
  // 乘法负数测试
  assert_eq(25, multiply(-5, -5))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-15, multiply(5, -3))
  
  // 除法负数测试
  assert_eq(-2, divide_with_ceil(-10, 5))
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
  
  // 减法负数测试
  assert_eq(-10, subtract(-5, 5))
  assert_eq(10, subtract(5, -5))
}

test "string_input_edge_cases" {
  // 字符串输入边缘情况测试
  // 空字符串和空格
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
  
  // 特殊字符
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))
  assert_eq_string("Hello, \n\t!", greet("\n\t"))
  assert_eq_string("Hello, \"quoted\"!", greet("\"quoted\""))
}

test "real_world_event_planning" {
  // 真实世界事件规划测试
  let attendees = 250
  let tables_available = 25
  let chairs_per_table = 10
  let cost_per_chair = 15
  let total_budget = 4000
  
  // 计算需要的桌子数量
  let tables_needed = divide_with_ceil(attendees, chairs_per_table)
  assert_eq(25, tables_needed)
  
  // 检查桌子是否足够
  assert_true(tables_needed <= tables_available)
  
  // 计算总座位数
  let total_seats = multiply(tables_needed, chairs_per_table)
  assert_eq(250, total_seats)
  
  // 计算剩余座位
  let remaining_seats = subtract(total_seats, attendees)
  assert_eq(0, remaining_seats)
  
  // 计算总成本
  let total_cost = multiply(total_seats, cost_per_chair)
  assert_eq(3750, total_cost)
  
  // 验证预算充足
  assert_true(total_cost <= total_budget)
  
  // 计算剩余预算
  let remaining_budget = subtract(total_budget, total_cost)
  assert_eq(250, remaining_budget)
}