// 标准 MoonBit 测试用例 - 为 Azimuth 项目创建
// 包含8个高质量测试用例，覆盖所有核心功能

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(10, add(3, 7))
  assert_eq(15, multiply(3, 5))
  assert_eq(2, subtract(8, 6))
  assert_eq(4, divide_with_ceil(10, 3))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 12
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 零元素性质
  assert_eq(x, add(x, 0))
  assert_eq(0, multiply(x, 0))
}

test "greet_function_comprehensive" {
  // 问候函数综合测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 中文!", greet("中文"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_edge_cases" {
  // 向上取整除法边界情况测试
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-5, add(-2, -3))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-8, subtract(3, 11))
  assert_eq(-3, divide_with_ceil(-10, 4))
  assert_eq(3, divide_with_ceil(-10, -4))
}

test "complex_calculation_workflow" {
  // 复杂计算工作流测试
  // 场景：项目资源计算
  let team_size = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team_size, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints_needed)
}

test "business_packaging_scenario" {
  // 业务包装场景测试
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

test "mathematical_boundary_conditions" {
  // 数学边界条件测试
  // 极值测试
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(1000000, multiply(1000, 1000))
  
  // 单位元素测试
  assert_eq(5, multiply(5, 1))
  assert_eq(5, add(5, 0))
  
  // 特殊除法情况
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(1, divide_with_ceil(-5, -5))
}