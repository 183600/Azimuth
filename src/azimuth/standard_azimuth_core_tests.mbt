// 标准 Azimuth 核心测试用例
// 使用标准的 MoonBit 测试语法

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(12, multiply(3, 4))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_function_comprehensive" {
  // 全面测试向上取整除法功能
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_comprehensive" {
  // 全面测试问候功能
  assert_eq_string("Hello, Azimuth!", greet("Azimuth"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 25
  let quantity = 8
  let tax_rate = 10
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(20, tax)
  assert_eq(235, total)
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算
  let total_items = 47
  let items_per_box = 12
  let weight_per_item = 2
  let max_weight_per_box = 25
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_weight = multiply(total_items, weight_per_item)
  let boxes_by_weight = divide_with_ceil(total_weight, max_weight_per_box)
  
  assert_eq(4, boxes_needed)
  assert_eq(94, total_weight)
  assert_eq(4, boxes_by_weight)
}

test "time_and_resource_management" {
  // 时间和资源管理
  let tasks_per_day = 6
  let total_tasks = 35
  let working_days = divide_with_ceil(total_tasks, tasks_per_day)
  
  let hours_per_task = 2
  let total_hours = multiply(total_tasks, hours_per_task)
  let hours_per_day = multiply(tasks_per_day, hours_per_task)
  
  assert_eq(6, working_days)
  assert_eq(70, total_hours)
  assert_eq(12, hours_per_day)
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 1000
  let interest_rate = 8
  let years = 3
  
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year1_total = add(principal, year1_interest)
  
  let year2_interest = divide_with_ceil(multiply(year1_total, interest_rate), 100)
  let year2_total = add(year1_total, year2_interest)
  
  let year3_interest = divide_with_ceil(multiply(year2_total, interest_rate), 100)
  let final_amount = add(year2_total, year3_interest)
  
  assert_eq(80, year1_interest)
  assert_eq(1080, year1_total)
  assert_eq(87, year2_interest)
  assert_eq(1167, year2_total)
  assert_eq(94, year3_interest)
  assert_eq(1261, final_amount)
}

test "error_handling_and_edge_cases" {
  // 错误处理和边界情况
  // 测试除零安全
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 测试极值
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}