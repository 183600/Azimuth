// 简单标准测试用例 - 包含10个高质量测试用例
// 使用标准 MoonBit 测试语法，覆盖核心功能

test "basic_addition" {
  // 基本加法测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-1, add(-3, 2))
}

test "basic_multiplication" {
  // 基本乘法测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
}

test "basic_subtraction" {
  // 基本减法测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "basic_divide_with_ceil" {
  // 基本向上取整除法测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "basic_greet" {
  // 基本问候测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "math_commutative_property" {
  // 数学交换律测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "packaging_calculation" {
  // 包装计算测试
  let items = 25
  let capacity = 6
  let packages = divide_with_ceil(items, capacity)
  
  assert_eq(5, packages)
  
  // 验证总容量
  let total_capacity = multiply(packages, capacity)
  let remaining = subtract(total_capacity, items)
  assert_eq(5, remaining)
}

test "complex_workflow" {
  // 复杂工作流测试
  let team = 4
  let tasks_per_person = 8
  let total_tasks = multiply(team, tasks_per_person)
  
  let sprint_capacity = 15
  let sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(32, total_tasks)  // 4 * 8 = 32
  assert_eq(3, sprints)       // ceil(32/15) = 3
}

test "negative_numbers" {
  // 负数测试
  assert_eq(-10, add(-5, -5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-8, subtract(-5, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "boundary_values" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(2000000, add(1000000, 1000000))
}