// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能
// 包含 10 个高质量测试用例，涵盖主要功能和边界情况

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a × (b + c) = a × b + a × c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))   // -10/3 = -3.333... -> -3
  assert_eq(-4, divide_with_ceil(-11, 3))   // -11/3 = -3.666... -> -4
  assert_eq(3, divide_with_ceil(10, -3))    // 10/-3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-10, -3))   // -10/-3 = 3.333... -> 4
}

test "greet_function_unicode_handling" {
  // 测试问候函数的Unicode字符处理
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax_amount)
  assert_eq(231, total)
}

test "mathematical_series_summation" {
  // 数学级数求和：等差数列 1 + 3 + 5 + ... + 19
  let first_term = 1
  let common_difference = 2
  let number_of_terms = 10
  
  // 使用公式 S = n/2 × (2a + (n-1)d)
  let sum = divide_with_ceil(
    multiply(number_of_terms, add(multiply(2, first_term), multiply(number_of_terms - 1, common_difference))),
    2
  )
  
  assert_eq(100, sum)
}

test "resource_allocation_scenario" {
  // 资源分配场景：计算项目所需资源
  let total_tasks = 47
  let tasks_per_person = 5
  let overtime_cost_per_task = 20
  
  let people_needed = divide_with_ceil(total_tasks, tasks_per_person)
  let overtime_tasks = add(total_tasks, -multiply(people_needed - 1, tasks_per_person))
  let overtime_cost = multiply(overtime_tasks, overtime_cost_per_task)
  
  assert_eq(10, people_needed)
  assert_eq(2, overtime_tasks)
  assert_eq(40, overtime_cost)
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(0, multiply(0, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
}

test "error_handling_robustness" {
  // 错误处理健壮性测试
  assert_eq(0, divide_with_ceil(100, 0))    // 除数为零
  assert_eq(0, divide_with_ceil(-100, 0))   // 负数除以零
  assert_eq(0, divide_with_ceil(0, 0))      // 零除以零
}

test "time_and_work_calculation" {
  // 时间与工作计算：项目完成时间估算
  let total_work_items = 85
  let daily_capacity = 7
  let work_days_per_week = 5
  
  let days_needed = divide_with_ceil(total_work_items, daily_capacity)
  let weeks_needed = divide_with_ceil(days_needed, work_days_per_week)
  
  assert_eq(13, days_needed)
  assert_eq(3, weeks_needed)
}