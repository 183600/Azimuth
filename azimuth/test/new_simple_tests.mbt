// 新的简单测试用例 - 为 Azimuth 项目添加
// 使用标准 MoonBit 测试语法，包含8个高质量测试用例

test "basic_addition_test" {
  // 基本加法测试
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
}

test "basic_multiplication_test" {
  // 基本乘法测试
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(-15, multiply(-3, 5))
}

test "greet_function_test" {
  // 问候函数测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_test" {
  // 向上取整除法测试
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(10, 0))
}

test "subtract_function_test" {
  // 减法函数测试
  assert_eq(2, subtract(5, 3))
  assert_eq(-2, subtract(3, 5))
  assert_eq(0, subtract(5, 5))
}

test "mathematical_properties_test" {
  // 数学性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "business_packaging_test" {
  // 业务包装测试
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