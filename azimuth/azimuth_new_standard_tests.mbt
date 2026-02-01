// Azimuth 项目的新增标准 MoonBit 测试用例
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(7, 5))
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体结果
  assert_eq(20, add(12, 8))
  assert_eq(96, multiply(12, 8))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-2, subtract(-7, -5))
  assert_eq(-3, divide_with_ceil(-8, 3))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
}

test "boundary_value_testing" {
  // 边界值测试
  // 零值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 10))
  
  // 单位值测试
  assert_eq(5, add(5, 0))
  assert_eq(5, multiply(5, 1))
  assert_eq(1, divide_with_ceil(5, 5))
  
  // 大数测试
  assert_eq(2000, add(1000, 1000))
  assert_eq(1000000, multiply(1000, 1000))
}

test "business_inventory_calculation" {
  // 库存计算业务测试
  let total_products = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  
  assert_eq(4, boxes_needed)
  
  // 验证总容量
  let total_capacity = multiply(boxes_needed, box_capacity)
  assert_eq(48, total_capacity)
  
  // 验证剩余空间
  let remaining_space = subtract(total_capacity, total_products)
  assert_eq(1, remaining_space)
}

test "complex_workflow_calculation" {
  // 复杂工作流计算测试
  // 场景：项目资源规划
  let team_members = 6
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let days_per_sprint = 5
  let tasks_per_day = 10
  let sprint_capacity = multiply(days_per_sprint, tasks_per_day)
  
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)  // 6 * 8 = 48
  assert_eq(50, sprint_capacity)  // 5 * 10 = 50
  assert_eq(1, sprints_needed)  // ceil(48/50) = 1
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 4
  let b = 6
  let c = 3
  
  // 验证分配律: a * (b + c) = a * b + a * c
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 4 * (6 + 3) = 4 * 6 + 4 * 3 = 36
}