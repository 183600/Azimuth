// 高质量 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行全面测试

test "add_function_comprehensive_test" {
  // 全面测试加法功能
  assert_eq(10, add(7, 3))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-8, 3))
  assert_eq(1000000, add(500000, 500000))
  assert_eq(1, add(2147483647, -2147483646))
}

test "multiply_function_edge_cases" {
  // 测试乘法函数的边界情况
  assert_eq(21, multiply(7, 3))
  assert_eq(0, multiply(0, 100))
  assert_eq(-24, multiply(-8, 3))
  assert_eq(1000000, multiply(1000, 1000))
  assert_eq(1, multiply(1, 1))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(4, divide_with_ceil(-10, -3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let product_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  let subtotal = multiply(product_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 6
  let b = 9
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(b, 3)), add(multiply(a, b), multiply(a, 3)))
}

test "real_world_inventory_calculation" {
  // 真实世界库存计算
  let items_per_box = 24
  let total_items = 65
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let remaining_space = add(multiply(boxes_needed, items_per_box), -total_items)
  
  assert_eq(3, boxes_needed)
  assert_eq(7, remaining_space)
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 1000
  let interest_rate = 5
  let years = 3
  
  let year1_interest = divide_with_ceil(multiply(principal, interest_rate), 100)
  let year2_interest = divide_with_ceil(multiply(add(principal, year1_interest), interest_rate), 100)
  let year3_interest = divide_with_ceil(multiply(add(add(principal, year1_interest), year2_interest), interest_rate), 100)
  
  let total_amount = add(add(add(principal, year1_interest), year2_interest), year3_interest)
  
  assert_eq(50, year1_interest)
  assert_eq(53, year2_interest)
  assert_eq(55, year3_interest)
  assert_eq(1158, total_amount)
}