// 简单的 Azimuth 测试用例
// 包含 8 个标准 MoonBit 测试用例

test "add_basic" {
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}

test "multiply_basic" {
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "divide_with_ceil_basic" {
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_basic" {
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "subtract_basic" {
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "math_commutative" {
  let a = 7
  let b = 13
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "packaging_scenario" {
  let items = 25
  let capacity = 6
  let packages = divide_with_ceil(items, capacity)
  assert_eq(5, packages)
  
  let total_capacity = multiply(packages, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "complex_workflow" {
  let team = 4
  let tasks_per_person = 8
  let total_tasks = multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, sprints)       // ceil(32/15) = 3
}