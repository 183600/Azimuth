// Azimuth 标准测试用例 - 使用标准的 MoonBit 测试语法
// 包含10个高质量的测试用例，覆盖主要功能

test "basic_addition_operations" {
  // 基本加法运算测试
  assert_eq(5, add(2, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(10, add(-3, 13))
}

test "basic_multiplication_operations" {
  // 基本乘法运算测试
  assert_eq(6, multiply(2, 3))
  assert_eq(0, multiply(5, 0))
  assert_eq(-6, multiply(-2, 3))
  assert_eq(15, multiply(-3, -5))
}

test "greet_function_various_inputs" {
  // 问候函数各种输入测试
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_basic_functionality" {
  // 向上取整除法基本功能测试
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(0, divide_with_ceil(5, 0))  // 除零处理
}

test "subtraction_using_addition" {
  // 使用加法实现减法操作测试
  assert_eq(2, add(5, -3))
  assert_eq(-2, add(3, -5))
  assert_eq(0, add(5, -5))
  assert_eq(10, add(5, 5))
}

test "mathematical_commutative_properties" {
  // 数学交换律性质测试
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试：项目资源规划
  let team_members = 8
  let tasks_per_member = 5
  let total_tasks = multiply(team_members, tasks_per_member)
  let tasks_per_sprint = 15
  let sprints_needed = divide_with_ceil(total_tasks, tasks_per_sprint)
  
  assert_eq(40, total_tasks)     // 8 * 5 = 40
  assert_eq(3, sprints_needed)   // ceil(40/15) = 3
}

test "edge_case_boundary_values" {
  // 边界值和边缘情况测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(0, multiply(0, 1))
}

test "negative_number_operations" {
  // 负数运算测试
  assert_eq(-10, add(-5, -5))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(-2, divide_with_ceil(-7, 3))
}

test "real_world_application_test" {
  // 真实世界应用测试：活动座位安排
  let participants = 87
  let chairs_per_table = 10
  let tables_needed = divide_with_ceil(participants, chairs_per_table)
  let total_chairs = multiply(tables_needed, chairs_per_table)
  let empty_chairs = add(total_chairs, -participants)
  
  assert_eq(9, tables_needed)     // ceil(87/10) = 9
  assert_eq(90, total_chairs)     // 9 * 10 = 90
  assert_eq(3, empty_chairs)      // 90 - 87 = 3
}