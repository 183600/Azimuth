// 标准 MoonBit 测试用例 - 为 azimuth 库添加10个高质量测试用例
// 使用标准的 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(8, add(5, 3))
  assert_eq(15, multiply(5, 3))
  assert_eq(2, divide_with_ceil(5, 3))
  assert_eq(2, subtract(5, 3))
}

test "mathematical_commutative_laws" {
  // 数学交换律测试
  let a = 12
  let b = 7
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "divide_with_ceil_comprehensive" {
  // 向上取整除法综合测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(-2, divide_with_ceil(-5, 3))
  assert_eq(-1, divide_with_ceil(5, -3))
}

test "string_greeting_variations" {
  // 字符串问候变体测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-8, add(-5, -3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(-2, subtract(-5, 3))
  assert_eq(2, subtract(3, -5))
}

test "complex_business_calculation" {
  // 复杂业务计算测试：项目资源规划
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
}

test "edge_case_boundary_values" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, multiply(0, 12345))
  assert_eq(12345, multiply(1, 12345))
  assert_eq(0, add(0, 0))
}

test "mathematical_distributive_property" {
  // 数学分配律测试
  let a = 6
  let b = 4
  let c = 3
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "real_world_packaging_problem" {
  // 真实世界包装问题测试
  let items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(items, box_capacity)
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = subtract(total_capacity, items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "sequential_calculation_pipeline" {
  // 连续计算流水线测试
  let initial = 10
  let step1 = multiply(initial, 3)
  let step2 = add(step1, 5)
  let step3 = divide_with_ceil(step2, 7)
  let final_result = subtract(step3, 1)
  
  assert_eq(30, step1)
  assert_eq(35, step2)
  assert_eq(5, step3)
  assert_eq(4, final_result)
}