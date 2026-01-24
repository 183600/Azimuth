// 标准 Azimuth 库测试用例
// 测试核心功能：add, multiply, greet, divide_with_ceil

test "add_function_basic_operations" {

test "add_function_basic_operations" {
  // 测试基本加法运算
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
  assert_eq(100, add(50, 50))
}

test "multiply_function_comprehensive" {
  // 测试乘法功能的各种情况
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(25, multiply(5, 5))
}

test "greet_function_multilingual_support" {
  // 测试多语言问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, !", greet(""))
}

test "divide_with_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(5, 0))
}

test "complex_business_calculation" {
  // 复杂业务计算场景测试
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
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  assert_eq(multiply(a, add(3, 4)), add(multiply(a, 3), multiply(a, 4)))
}

test "negative_number_operations" {
  // 测试负数运算
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 3))
}

test "real_world_inventory_calculation" {
  // 实际库存管理计算
  let items_per_box = 24
  let total_items = 100
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let leftover_space = multiply(boxes_needed, items_per_box) - total_items
  
  assert_eq(5, boxes_needed)
  assert_eq(20, leftover_space)
}

test "string_and_number_combination" {
  // 测试字符串和数字的组合操作
  let user_id = "12345"
  let multiplier = 3
  let base_value = 100
  
  let calculated_value = multiply(base_value, multiplier)
  let greeting = greet("User" + user_id)
  
  assert_eq(300, calculated_value)
  assert_eq_string("Hello, User12345!", greeting)
}

test "comprehensive_financial_calculation" {
  // 综合金融计算测试
  let principal = 1000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, rate_percent), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, rate_percent), 100))
  
  assert_eq(1050, year1_amount)
  assert_eq(1103, year2_amount)
  assert_eq(1158, year3_amount)
}