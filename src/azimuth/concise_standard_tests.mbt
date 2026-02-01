// 精简标准 MoonBit 测试用例
// 为 Azimuth 项目创建的8个高质量测试用例

test "add_function_core" {
  // 加法核心功能测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "multiply_function_core" {
  // 乘法核心功能测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "divide_with_ceil_basic" {
  // 向上取整除法基础测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_various" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, !", greet(""))
}

test "mathematical_properties" {
  // 数学性质验证测试
  let a = 12
  let b = 7
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "packaging_scenario" {
  // 包装场景测试
  let items = 35
  let capacity = 8
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
  
  let total_capacity = multiply(packages, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "complex_workflow" {
  // 复杂工作流测试
  let team = 6
  let tasks_per_person = 8
  let total_tasks = multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(48, total_tasks)
  assert_eq(4, sprints)
}

test "edge_cases" {
  // 边界情况测试
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
}