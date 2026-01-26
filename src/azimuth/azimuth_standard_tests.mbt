// 标准 MoonBit 测试用例
// 针对 azimuth 库的全面测试

test "add_function_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 测试复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 6
  let tax_rate = 8
  let shipping = 12
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping)
  
  assert_eq(270, subtotal)
  assert_eq(22, tax_amount)
  assert_eq(304, total)
}

test "packaging_optimization" {
  // 测试包装优化计算
  let small_items = 15
  let medium_items = 28
  let large_items = 7
  let items_per_box = 10
  
  let total_items = add(add(small_items, medium_items), large_items)
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  assert_eq(50, total_items)
  assert_eq(5, boxes_needed)
}

test "time_management_calculation" {
  // 测试时间管理计算
  let daily_tasks = 7
  let weekly_tasks = 45
  let tasks_per_hour = 3
  
  let work_days_needed = divide_with_ceil(weekly_tasks, daily_tasks)
  let hours_per_day = divide_with_ceil(daily_tasks, tasks_per_hour)
  
  assert_eq(7, work_days_needed)
  assert_eq(3, hours_per_day)
}

test "mathematical_series_sum" {
  // 测试数学级数求和：1 + 2 + 3 + ... + 10
  let n = 10
  let sum = divide_with_ceil(multiply(n, add(n, 1)), 2)
  assert_eq(55, sum)
}

test "string_concatenation_with_greet" {
  // 测试字符串连接与问候函数
  let first_name = "张"
  let last_name = "三"
  let full_name = first_name + last_name
  let greeting = greet(full_name)
  
  assert_eq_string("Hello, 张三!", greeting)
}

test "resource_allocation_optimization" {
  // 测试资源分配优化
  let total_budget = 1000
  let project_a_cost = 350
  let project_b_cost = 280
  let small_project_cost = 80
  
  let remaining_budget = add(total_budget, -add(project_a_cost, project_b_cost))
  let small_projects_possible = divide_with_ceil(remaining_budget, small_project_cost)
  
  assert_eq(370, remaining_budget)
  assert_eq(5, small_projects_possible)
}