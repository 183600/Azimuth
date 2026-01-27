// 标准 MoonBit 测试用例
// 针对 azimuth 库的10个核心功能测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 0.01 -> 1
  assert_eq(0, divide_with_ceil(0, 100))  // 0 -> 0
}

test "greet_function_multilingual_support" {
  // 测试多语言问候支持
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(292, total_before_discount)
  assert_eq(272, final_total)
}

test "negative_number_division_ceil" {
  // 测试负数的向上取整除法
  assert_eq(-2, divide_with_ceil(-4, 2))   // -4/2 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 2))   // -3/2 = -1.5 -> -1
  assert_eq(-3, divide_with_ceil(-7, 2))   // -7/2 = -3.5 -> -3
  assert_eq(3, divide_with_ceil(-7, -2))   // -7/-2 = 3.5 -> 4
}

test "mathematical_series_calculation" {
  // 测试数学级数计算：等差数列求和
  let first_term = 5
  let common_diff = 3
  let num_terms = 6
  
  // 使用公式 S = n/2 * (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(num_terms, add(multiply(2, first_term), multiply(num_terms - 1, common_diff))),
    2
  )
  
  assert_eq(63, sum)  // 6/2 * (10 + 15) = 3 * 25 = 75
}

test "string_processing_with_special_chars" {
  // 测试特殊字符字符串处理
  assert_eq_string("Hello, email@domain.com!", greet("email@domain.com"))
  assert_eq_string("Hello, 123-456-7890!", greet("123-456-7890"))
  assert_eq_string("Hello, A+B=C!", greet("A+B=C"))
  assert_eq_string("Hello, <script>!", greet("<script>"))
}

test "resource_allocation_calculation" {
  // 测试资源分配计算
  let total_budget = 5000
  let project_cost = 1200
  let team_size = 8
  
  let remaining_budget = add(total_budget, -multiply(project_cost, 3))
  let budget_per_person = divide_with_ceil(remaining_budget, team_size)
  
  assert_eq(1400, remaining_budget)
  assert_eq(175, budget_per_person)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let total_tasks = 67
  let tasks_per_day = 7
  let work_hours_per_day = 8
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_work_hours = multiply(days_needed, work_hours_per_day)
  
  assert_eq(10, days_needed)
  assert_eq(80, total_work_hours)
}