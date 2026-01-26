// Azimuth 库的额外标准测试用例
// 包含基本功能测试、边界情况和实际应用场景

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_with_zero" {
  // 测试乘法与零的性质
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-5, 0))
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(7, divide_with_ceil(20, 3))
}

test "greet_function_with_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, test-123!", greet("test-123"))
  assert_eq_string("Hello, user_name!", greet("user_name"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算折扣后的总价
  let unit_price = 45
  let quantity = 6
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let final_total = add(subtotal, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(250, final_total)
}

test "negative_number_operations_comprehensive" {
  // 负数运算的综合测试
  assert_eq(-8, add(-3, -5))
  assert_eq(2, add(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-2, divide_with_ceil(-7, 3))
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_resources = 100
  let team_size = 8
  let resources_per_team = divide_with_ceil(total_resources, team_size)
  
  assert_eq(13, resources_per_team)
}

test "time_management_scenario" {
  // 时间管理场景：计算任务完成时间
  let tasks_per_hour = 4
  let total_tasks = 25
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  
  assert_eq(7, hours_needed)
}

test "string_greeting_edge_cases" {
  // 字符串问候的边界情况
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, ABCDEFGHIJKLMNOPQRSTUVWXYZ!", greet("ABCDEFGHIJKLMNOPQRSTUVWXYZ"))
  assert_eq_string("Hello, 1234567890!", greet("1234567890"))
}

test "mathematical_identity_properties" {
  // 数学恒等性质测试
  let x = 42
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}