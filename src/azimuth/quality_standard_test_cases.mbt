// 高质量标准 MoonBit 测试用例
// 测试 azimuth 库的核心功能，包含边界值和实际应用场景

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, add(5, -5))
  assert_eq(1000, add(500, 500))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, multiply(-1, -1))
}

test "subtract_function_validation" {
  // 测试减法函数的各种情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(5, 5))
  assert_eq(15, subtract(-5, -20))
  assert_eq(-15, subtract(5, 20))
}

test "greet_function_unicode_support" {
  // 测试问候函数的 Unicode 支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(1, divide_with_ceil(1, 100))
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 12
  let b = 8
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
}

test "complex_business_scenario" {
  // 复杂业务场景测试
  let unit_price = 25
  let quantity = 8
  let shipping_cost = 15
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(200, subtotal)
  assert_eq(215, total_before_discount)
  assert_eq(195, final_total)
}

test "packaging_optimization" {
  // 包装优化测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_capacity = multiply(boxes_needed, items_per_box)
  let unused_space = subtract(total_capacity, total_items)
  
  assert_eq(4, boxes_needed)
  assert_eq(48, total_capacity)
  assert_eq(1, unused_space)
}

test "time_calculation_scenario" {
  // 时间计算场景测试
  let tasks_per_day = 8
  let total_tasks = 35
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let total_hours = multiply(days_needed, 8)
  
  assert_eq(5, days_needed)
  assert_eq(40, total_hours)
}

test "financial_calculation" {
  // 金融计算测试
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year2_interest = divide_with_ceil(multiply(add(principal, year1_interest), interest_rate), 100)
  let year3_interest = divide_with_ceil(multiply(add(add(principal, year1_interest), year2_interest), interest_rate), 100)
  
  let total_after_3_years = add(add(add(principal, year1_interest), year2_interest), year3_interest)
  
  assert_eq(50, year1_interest)
  assert_eq(53, year2_interest)
  assert_eq(55, year3_interest)
  assert_eq(1158, total_after_3_years)
}