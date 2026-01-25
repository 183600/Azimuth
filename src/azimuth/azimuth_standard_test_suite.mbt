// Azimuth 标准测试套件 - 10个核心功能测试用例
// 测试 azimuth 库的主要功能

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(a, b))
  assert_eq(42, add(b, a))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)
}

test "divide_with_ceil_complex_scenarios" {
  // 测试向上取整除法的复杂场景
  // 正数除法
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(7, divide_with_ceil(20, 3))
  
  // 负数除法
  assert_eq(-3, divide_with_ceil(-8, 3))
  assert_eq(-2, divide_with_ceil(-5, 3))
  
  // 零除数安全处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-50, 0))
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, !", greet(""))
}

test "business_calculation_scenario" {
  // 测试业务计算场景：订单总价计算
  let unit_price = 45
  let quantity = 8
  let tax_rate = 10
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(360, subtotal)
  assert_eq(36, tax)
  assert_eq(411, total)
}

test "mathematical_sequence_calculation" {
  // 测试数学序列计算：等差数列求和
  let first_term = 3
  let common_diff = 4
  let num_terms = 6
  
  // 手动计算数列和：3 + 7 + 11 + 15 + 19 + 23
  let sum_manual = add(add(add(add(add(3, 7), 11), 15), 19), 23)
  
  // 使用公式计算：n/2 * (2a + (n-1)d)
  let sum_formula = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(78, sum_manual)
  assert_eq(78, sum_formula)
}

test "error_handling_robustness" {
  // 测试错误处理的健壮性
  // 边界值测试
  assert_eq(0, add(0, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(12345, 0))
  assert_eq(12345, multiply(12345, 1))
  
  // 大数运算
  let large_num = 50000
  assert_eq(100000, add(large_num, large_num))
  assert_eq(2500000000, multiply(large_num, large_num))
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_budget = 10000
  let project_a_cost = 3500
  let project_b_cost = 2800
  let remaining_budget = add(total_budget, -add(project_a_cost, project_b_cost))
  
  // 计算可以支持的小项目数量
  let small_project_cost = 450
  let small_projects_count = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(3700, remaining_budget)
  assert_eq(9, small_projects_count)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let daily_working_hours = 8
  let total_task_hours = 67
  let days_needed = divide_with_ceil(total_task_hours, daily_working_hours)
  
  // 计算总工作分钟数
  let total_minutes = multiply(days_needed, multiply(daily_working_hours, 60))
  
  assert_eq(9, days_needed)
  assert_eq(4320, total_minutes)
}

test "string_processing_with_greet" {
  // 测试字符串处理与问候函数的结合
  let prefix = "Dr."
  let first_name = "Alice"
  let last_name = "Johnson"
  
  // 构建完整姓名
  let full_name = prefix + " " + first_name + " " + last_name
  let formal_greeting = greet(full_name)
  
  assert_eq_string("Hello, Dr. Alice Johnson!", formal_greeting)
  
  // 测试特殊字符组合
  let special_chars = "Test@123#"
  assert_eq_string("Hello, Test@123#!", greet(special_chars))
}