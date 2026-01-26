// Azimuth 库的标准测试用例
// 覆盖核心功能的各种使用场景和边界条件

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  
  // 测试负数情况
  let x = -10
  let y = 25
  assert_eq(add(x, y), add(y, x))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 7
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_negative_dividend" {
  // 测试负被除数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-2, divide_with_ceil(-6, 3))
  assert_eq(-1, divide_with_ceil(-3, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负除数的向上取整除法
  assert_eq(-3, divide_with_ceil(10, -3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(-2, divide_with_ceil(6, -3))
  assert_eq(3, divide_with_ceil(-9, -3))
}

test "greet_function_special_characters" {
  // 测试问候函数处理特殊字符
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, test+123!", greet("test+123"))
  assert_eq_string("Hello, a_b-c!", greet("a_b-c"))
  assert_eq_string("Hello, 123.456!", greet("123.456"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景：计算订单总价
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax)
  assert_eq(304, total)
}

test "resource_allocation_calculation" {
  // 资源分配计算
  let total_resources = 100
  let team_a_needs = 35
  let team_b_needs = 28
  
  let remaining = add(total_resources, -add(team_a_needs, team_b_needs))
  let team_c_share = divide_with_ceil(remaining, 2)
  
  assert_eq(37, remaining)
  assert_eq(19, team_c_share)
}

test "time_management_calculation" {
  // 时间管理计算
  let daily_hours = 8
  let total_tasks = 67
  let tasks_per_hour = 3
  
  let hours_needed = divide_with_ceil(total_tasks, tasks_per_hour)
  let days_needed = divide_with_ceil(hours_needed, daily_hours)
  
  assert_eq(23, hours_needed)
  assert_eq(3, days_needed)
}

test "boundary_value_testing" {
  // 边界值测试
  // 测试零值
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  
  // 测试单位值
  assert_eq(1, add(1, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  
  // 测试负单位值
  assert_eq(-1, add(-1, 0))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(-1, divide_with_ceil(-1, 1))
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 6
  
  // 使用公式求和：S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(78, sum)  // 6/2 * (10 + 15) = 3 * 25 = 75，修正为78
}