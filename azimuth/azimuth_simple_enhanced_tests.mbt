// Azimuth 简单增强测试用例
// 包含8个基本的标准 MoonBit 测试用例

test "simple_addition_test" {
  // 简单加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}

test "simple_multiplication_test" {
  // 简单乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "simple_divide_with_ceil_test" {
  // 简单向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "simple_greet_test" {
  // 简单问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "simple_subtract_test" {
  // 简单减法测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "mathematical_commutative_test" {
  // 数学交换律测试
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "packaging_scenario_test" {
  // 包装场景测试
  let items = 25
  let capacity = 6
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
  
  let total_capacity = multiply(packages, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "complex_workflow_test" {
  // 复杂工作流测试
  let team = 4
  let tasks_per_person = 8
  let total_tasks = multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, sprints)       // ceil(32/15) = 3
}