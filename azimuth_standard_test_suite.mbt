// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法编写的测试集

test "add_function_comprehensive" {
  // 加法函数综合测试
  assert_eq(10, add(6, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-60, -40))
}

test "multiply_function_edge_cases" {
  // 乘法函数边界情况测试
  assert_eq(25, multiply(5, 5))
  assert_eq(0, multiply(100, 0))
  assert_eq(-36, multiply(-6, 6))
  assert_eq(36, multiply(-6, -6))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_various_scenarios" {
  // 向上取整除法各种场景测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(1, divide_with_ceil(2, 3))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "divide_with_ceil_negative_numbers" {
  // 向上取整除法负数测试
  assert_eq(-2, divide_with_ceil(-8, 3))
  assert_eq(-3, divide_with_ceil(8, -3))
  assert_eq(3, divide_with_ceil(-8, -3))
  assert_eq(-1, divide_with_ceil(-1, 2))
  assert_eq(1, divide_with_ceil(-1, -2))
}

test "greet_function_special_inputs" {
  // 问候函数特殊输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文!", greet("中文"))
}

test "subtract_function_comprehensive" {
  // 减法函数综合测试
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(15, 15))
  assert_eq(20, subtract(10, -10))
  assert_eq(-20, subtract(-10, 10))
}

test "mathematical_properties_validation" {
  // 数学性质验证测试
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 验证具体数值
  assert_eq(18, add(7, 11))
  assert_eq(77, multiply(7, 11))
}

test "complex_business_workflow" {
  // 复杂业务工作流测试
  // 场景：计算项目资源需求
  let team_members = 5
  let tasks_per_member = 8
  let total_tasks = multiply(team_members, tasks_per_member)
  
  let sprint_capacity = 12
  let sprints_needed = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(40, total_tasks)  // 5 * 8 = 40
  assert_eq(4, sprints_needed)  // ceil(40/12) = 4
}