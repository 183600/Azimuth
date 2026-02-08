// 标准 MoonBit 测试用例 - 为 Azimuth 项目添加
// 包含8个高质量测试用例，使用标准 MoonBit 测试语法

test "basic_arithmetic_operations" {
  // 基本算术运算测试
  assert_eq(7, add(3, 4))
  assert_eq(12, multiply(3, 4))
  assert_eq(2, subtract(5, 3))
  assert_eq(4, divide_with_ceil(15, 4))
  assert_eq(3, divide_with_ceil(12, 4))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let x = 15
  let y = 8
  
  // 加法交换律
  assert_eq(add(x, y), add(y, x))
  
  // 乘法交换律
  assert_eq(multiply(x, y), multiply(y, x))
  
  // 分配律验证
  assert_eq(
    multiply(x, add(y, 2)),
    add(multiply(x, y), multiply(x, 2))
  )
}

test "edge_cases_and_boundaries" {
  // 边界情况和极值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(5, subtract(5, 0))
}

test "negative_numbers_handling" {
  // 负数处理测试
  assert_eq(-2, add(-5, 3))
  assert_eq(-15, multiply(-5, 3))
  assert_eq(15, multiply(-5, -3))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-1, divide_with_ceil(-3, 4))
  assert_eq(1, divide_with_ceil(-3, -4))
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

test "complex_calculation_scenario" {
  // 复杂计算场景测试 - 项目资源规划
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 15
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  let completed_tasks = multiply(sprints_needed - 1, sprint_capacity)
  let remaining_tasks = subtract(total_tasks, completed_tasks)
  
  assert_eq(40, total_tasks)
  assert_eq(3, sprints_needed)
  assert_eq(10, remaining_tasks)
}

test "business_packaging_optimization" {
  // 业务包装优化测试
  let total_items = 47
  let box_capacity = 12
  let boxes_needed = divide_with_ceil(total_items, box_capacity)
  
  let total_capacity = multiply(boxes_needed, box_capacity)
  let unused_space = subtract(total_capacity, total_items)
  let utilization_rate = multiply(divide_with_ceil(total_items, total_capacity), 100)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
  assert_eq(97, utilization_rate)
}

test "mathematical_sequence_calculation" {
  // 数学序列计算测试 - 斐波那契风格计算
  let a = 1
  let b = 1
  
  // 计算前几项的和
  let term3 = add(a, b)  // 2
  let term4 = add(b, term3)  // 3
  let term5 = add(term3, term4)  // 5
  
  let sequence_sum = add(add(add(a, b), term3), add(term4, term5))
  let average_value = divide_with_ceil(sequence_sum, 5)
  
  assert_eq(2, term3)
  assert_eq(3, term4)
  assert_eq(5, term5)
  assert_eq(12, sequence_sum)
  assert_eq(3, average_value)
}