// 标准 MoonBit 测试用例
// 为 azimuth 库添加的高质量测试覆盖

test "basic_addition_functionality" {
  // 测试基本加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-4, 2))
}

test "multiplication_properties" {
  // 测试乘法性质
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(7, 0))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
}

test "greeting_function_variations" {
  // 测试问候函数的各种变化
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "ceil_division_scenarios" {
  // 测试向上取整除法的各种场景
  assert_eq(3, divide_with_ceil(8, 3))
  assert_eq(2, divide_with_ceil(6, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-2, divide_with_ceil(-5, 3))
}

test "subtraction_basic_cases" {
  // 测试基本减法情况
  assert_eq(2, subtract(5, 3))
  assert_eq(-3, subtract(2, 5))
  assert_eq(0, subtract(4, 4))
}

test "complex_calculation_workflow" {
  // 测试复杂计算工作流
  let initial = 10
  let doubled = multiply(initial, 2)
  let added = add(doubled, 5)
  let divided = divide_with_ceil(added, 3)
  assert_eq(9, divided)  // ceil((10*2+5)/3) = ceil(25/3) = 9
}

test "mathematical_commutative_laws" {
  // 测试数学交换律
  let a = 7
  let b = 11
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "edge_case_handling" {
  // 测试边缘情况处理
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, multiply(0, 12345))
}

test "boolean_assertions" {
  // 测试布尔断言
  assert_true(5 > 3)
  assert_false(3 > 5)
  assert_true(add(2, 3) == 5)
}

test "real_world_application" {
  // 真实世界应用场景：活动座位安排
  let participants = 45
  let seats_per_table = 6
  let tables_needed = divide_with_ceil(participants, seats_per_table)
  
  assert_eq(8, tables_needed)  // ceil(45/6) = 8
  
  let total_seats = multiply(tables_needed, seats_per_table)
  assert_eq(48, total_seats)
  
  let empty_seats = subtract(total_seats, participants)
  assert_eq(3, empty_seats)
}