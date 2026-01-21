// Azimuth 库的高质量测试用例
// 测试核心功能和边界情况

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(7, 3))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-7, -3))
  assert_eq(100, add(50, 50))
  assert_eq(-100, add(-50, -50))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(21, multiply(7, 3))
  assert_eq(-21, multiply(7, -3))
  assert_eq(21, multiply(-7, -3))
  assert_eq(0, multiply(5, 0))
  assert_eq(100, multiply(10, 10))
}

test "divide_with_ceil_negative_numbers" {
  // 测试负数的向上取整除法
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(-4, divide_with_ceil(-11, 3))
  assert_eq(3, divide_with_ceil(-10, -3))
  assert_eq(4, divide_with_ceil(-11, -3))
  assert_eq(0, divide_with_ceil(-5, 0))
}

test "greet_function_various_inputs" {
  // 测试问候函数的各种输入
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob123!", greet("Bob123"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, 太阳!", greet("太阳"))
}

test "complex_calculation_scenario" {
  // 复杂计算场景测试
  let base_price = 50
  let quantity = 3
  let tax_rate = 10
  let shipping = 15
  
  let subtotal = multiply(base_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(180, total)  // 50*3 + ceil(150*10/100) + 15 = 150 + 15 + 15 = 180
}

test "mathematical_identity_properties" {
  // 数学恒等性质测试
  let x = 42
  
  // 加法恒等元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法恒等元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
  
  // 乘法零元
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, x))
}

test "boundary_value_testing" {
  // 边界值测试
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(1, 100))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(1, divide_with_ceil(-1, -1))
  assert_eq(0, divide_with_ceil(0, 1))
  assert_eq(0, divide_with_ceil(0, -1))
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况
  assert_eq_string("Hello, x!", greet("x"))
  assert_eq_string("Hello, ABCDEF!", greet("ABCDEF"))
  assert_eq_string("Hello, 123456!", greet("123456"))
  assert_eq_string("Hello, !@#$%^!", greet("!@#$%^"))
  assert_eq_string("Hello, spaces !", greet("spaces "))
}

test "real_world_inventory_calculation" {
  // 真实世界库存计算
  let items_per_box = 12
  let total_items = 55
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let total_boxes_cost = multiply(boxes_needed, 5)  // 每个箱子5元
  let total_items_cost = multiply(total_items, 2)  // 每个物品2元
  let total_cost = add(total_boxes_cost, total_items_cost)
  
  assert_eq(5, boxes_needed)  // ceil(55/12) = 5
  assert_eq(25, total_boxes_cost)  // 5 * 5 = 25
  assert_eq(110, total_items_cost)  // 55 * 2 = 110
  assert_eq(135, total_cost)  // 25 + 110 = 135
}

test "financial_rounding_calculation" {
  // 金融取整计算
  let amount = 100
  let people = 3
  
  // 计算每人需要支付的金额（向上取整）
  let payment_per_person = divide_with_ceil(amount, people)
  let total_collected = multiply(payment_per_person, people)
  
  assert_eq(34, payment_per_person)  // ceil(100/3) = 34
  assert_eq(102, total_collected)  // 34 * 3 = 102
}