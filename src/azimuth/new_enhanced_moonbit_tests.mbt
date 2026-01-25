// 增强的 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "addition_commutative_property" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "multiplication_distributive_property" {
  // 测试乘法分配律
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(7, divide_with_ceil(20, 3))
  assert_eq(0, divide_with_ceil(0, 5))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, ñáéíóú!", greet("ñáéíóú"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let unit_price = 45
  let quantity = 12
  let shipping_cost = 18
  let tax_rate = 8
  
  let subtotal = multiply(unit_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax_amount), shipping_cost)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax_amount)
  assert_eq(602, total)
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(0, add(-5, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "edge_case_calculations" {
  // 边界情况计算
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(12345, 0))
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(100, 1))
}

test "mathematical_sequence_calculation" {
  // 数学序列计算：等差数列
  let first_term = 3
  let common_diff = 4
  let n_terms = 6
  
  // 计算第6项
  let sixth_term = add(first_term, multiply(common_diff, n_terms - 1))
  // 计算前6项和
  let sum = divide_with_ceil(multiply(n_terms, add(first_term, sixth_term)), 2)
  
  assert_eq(23, sixth_term)
  assert_eq(78, sum)
}

test "inventory_management_scenario" {
  // 库存管理场景
  let initial_stock = 250
  let daily_sales = 15
  let days = 7
  let new_shipment = 100
  
  let sold = multiply(daily_sales, days)
  let intermediate_stock = add(initial_stock, -sold)
  let final_stock = add(intermediate_stock, new_shipment)
  
  assert_eq(105, sold)
  assert_eq(145, intermediate_stock)
  assert_eq(245, final_stock)
}

test "time_and_resource_calculation" {
  // 时间和资源计算
  let tasks_per_day = 6
  let total_tasks = 47
  let hours_per_task = 2
  
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, multiply(tasks_per_day, hours_per_task))
  let productive_hours = multiply(total_tasks, hours_per_task)
  
  assert_eq(8, days_needed)
  assert_eq(96, total_hours)
  assert_eq(94, productive_hours)
}