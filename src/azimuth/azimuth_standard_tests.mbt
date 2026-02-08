// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含 8 个高质量测试用例，覆盖核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(-4, add(-10, 6))
  assert_eq(0, add(5, -5))
  
  assert_eq(21, multiply(3, 7))
  assert_eq(-30, multiply(-5, 6))
  assert_eq(0, multiply(8, 0))
  
  assert_eq(5, subtract(12, 7))
  assert_eq(-5, subtract(7, 12))
  assert_eq(0, subtract(8, 8))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 5))
  assert_eq(0, divide_with_ceil(0, 10))
  assert_eq(0, divide_with_ceil(5, 0))
  
  // 负数测试
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(10, -3))
  assert_eq(3, divide_with_ceil(-10, -3))
}

test "greet_function_variations" {
  // 问候函数变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello,   !", greet("  "))
}

test "mathematical_properties" {
  // 数学性质测试
  let a = 8
  let b = 13
  
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
  // 场景：项目资源分配
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(4, sprints_needed)  // ceil(48/15) = 4
  
  // 计算总工作量
  let total_work = add(total_tasks, multiply(sprints_needed, 5))
  assert_eq(68, total_work)  // 48 + 4*5 = 68
}

test "boundary_and_edge_cases" {
  // 边界和边缘情况测试
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

test "sequential_operations_chain" {
  // 连续操作链测试
  let initial = 10
  let step1 = add(initial, multiply(2, 3))     // 10 + 2*3 = 16
  let step2 = divide_with_ceil(step1, 5)       // ceil(16/5) = 4
  let step3 = multiply(step2, subtract(8, 3))  // 4 * (8-3) = 20
  let step4 = divide_with_ceil(step3, 6)       // ceil(20/6) = 4
  let final_result = add(step4, 1)             // 4 + 1 = 5
  
  assert_eq(5, final_result)
}