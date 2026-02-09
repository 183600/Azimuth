// Azimuth 最终测试用例 - 10个标准 MoonBit 测试
// 测试基本数学运算和字符串处理功能

test "final_addition_test" {
  // 测试加法基本功能
  assert_eq(7, add(3, 4))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "final_multiplication_test" {
  // 测试乘法基本功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(5, 0))
  assert_eq(15, multiply(-3, -5))
}

test "final_division_test" {
  // 测试向上取整除法
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "final_subtraction_test" {
  // 测试减法基本功能
  assert_eq(2, subtract(5, 3))
  assert_eq(-3, subtract(3, 6))
  assert_eq(0, subtract(5, 5))
}

test "final_greeting_test" {
  // 测试问候函数
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "final_math_properties_test" {
  // 测试数学性质
  let a = 8
  let b = 12
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "final_business_calculation_test" {
  // 测试业务计算
  let items = 23
  let package_size = 5
  let packages = divide_with_ceil(items, package_size)
  assert_eq(5, packages)
  
  let total_capacity = multiply(packages, package_size)
  let remaining = subtract(total_capacity, items)
  assert_eq(2, remaining)
}

test "final_negative_numbers_test" {
  // 测试负数运算
  assert_eq(-8, add(-3, -5))
  assert_eq(20, multiply(-4, -5))
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(-9, subtract(5, 14))
}

test "final_large_numbers_test" {
  // 测试大数运算
  assert_eq(2000000, add(1000000, 1000000))
  assert_eq(100000000, multiply(10000, 10000))
  assert_eq(5001, divide_with_ceil(10000001, 2000))
}

test "final_complex_workflow_test" {
  // 测试复杂工作流
  let team = 5
  let tasks_per_person = 7
  let total_tasks = multiply(team, tasks_per_person)
  
  let sprint_capacity = 12
  let sprints = divide_with_ceil(total_tasks, sprint_capacity)
  
  assert_eq(35, total_tasks)
  assert_eq(3, sprints)
}