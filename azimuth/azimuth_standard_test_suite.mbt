// 标准 MoonBit 测试套件
// 包含10个高质量测试用例，覆盖主要功能和边界情况

test "arithmatic_operations_comprehensive" {
  // 综合算术运算测试
  assert_eq(42, add(20, 22))
  assert_eq(-10, add(-15, 5))
  assert_eq(0, add(0, 0))
  
  assert_eq(100, multiply(10, 10))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(0, 999))
  
  assert_eq(15, subtract(25, 10))
  assert_eq(-15, subtract(10, 25))
  assert_eq(0, subtract(50, 50))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(15, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "mathematical_properties" {
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

test "business_packaging_scenario" {
  // 业务包装场景测试
  let total_items = 47
  let items_per_package = 12
  let packages_needed = divide_with_ceil(total_items, items_per_package)
  
  assert_eq(4, packages_needed)
  
  // 验证总容量
  let total_capacity = multiply(packages_needed, items_per_package)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_items)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源计算
  let team_members = 7
  let tasks_per_member = 9
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 20
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(63, total_tasks)  // 7 * 9 = 63
  assert_eq(4, sprints_needed)  // ceil(63/20) = 4
}

test "boundary_values_and_edge_cases" {
  // 边界值和边缘情况测试
  // 除法边界情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  
  // 特殊问候情况
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(5, add(-10, 15))
  assert_eq(0, add(-8, 8))
  
  assert_eq(25, multiply(-5, -5))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(-5, 0))
  
  assert_eq(-15, subtract(-10, 5))
  assert_eq(15, subtract(5, -10))
  assert_eq(0, subtract(-5, -5))
}

test "sequential_operations" {
  // 连续操作测试
  let initial = 10
  let step1 = add(initial, 5)        // 15
  let step2 = multiply(step1, 2)     // 30
  let step3 = divide_with_ceil(step2, 8)  // 4
  let step4 = subtract(step3, 2)     // 2
  let step5 = multiply(step4, 6)     // 12
  
  assert_eq(12, step5)
}

test "real_world_application" {
  // 真实世界应用测试
  // 场景：活动策划计算
  let guests = 85
  let tables_capacity = 8
  let tables_needed = divide_with_ceil(guests, tables_capacity)
  
  assert_eq(11, tables_needed)
  
  // 计算总成本
  let cost_per_table = 50
  let total_table_cost = multiply(tables_needed, cost_per_table)
  assert_eq(550, total_table_cost)
  
  // 计算剩余座位
  let total_seats = multiply(tables_needed, tables_capacity)
  let empty_seats = subtract(total_seats, guests)
  assert_eq(3, empty_seats)
  
  // 验证问候功能用于邀请函
  assert_eq_string("Hello, VIP Guests!", greet("VIP Guests"))
}