// 标准 MoonBit 测试用例 - 针对 azimuth 库的核心功能

test "add_function_comprehensive" {
  // 全面测试加法功能
  assert_eq(8, add(3, 5))
  assert_eq(0, add(0, 0))
  assert_eq(-2, add(-5, 3))
  assert_eq(1000, add(500, 500))
  assert_eq(-1000, add(-500, -500))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
  assert_eq(10000, multiply(100, 100))
}

test "divide_with_ceil_various_cases" {
  // 测试向上取整除法的各种情况
  assert_eq(4, divide_with_ceil(10, 3))  // 3.333... -> 4
  assert_eq(3, divide_with_ceil(9, 3))   // 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100)) // 0.01 -> 1
  assert_eq(-3, divide_with_ceil(-10, 3)) // -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))   // 除数为0的安全处理
}

test "greet_function_multilingual_support" {
  // 测试问候函数的多语言支持
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, María!", greet("María"))
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 7
  let b = 13
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 零元素性质
  assert_eq(0, multiply(a, 0))
  assert_eq(a, add(a, 0))
}

test "complex_business_calculations" {
  // 复杂业务计算测试
  let unit_price = 25
  let quantity = 8
  let tax_rate = 8
  let shipping = 15
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  
  // 计算总价
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(200, subtotal)
  assert_eq(16, tax)
  assert_eq(231, total)
}

test "packaging_and_shipping_calculations" {
  // 包装和运输计算测试
  let total_items = 47
  let items_per_box = 12
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let shipping_per_box = 8
  let total_shipping = multiply(boxes_needed, shipping_per_box)
  
  assert_eq(4, boxes_needed)  // ceil(47/12) = 4
  assert_eq(32, total_shipping)
}

test "negative_number_operations_comprehensive" {
  // 负数运算的综合测试
  assert_eq(-8, add(-5, -3))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-2, divide_with_ceil(-7, 3))  // -2.333... -> -2
}

test "boundary_and_extreme_values" {
  // 边界和极值测试
  assert_eq(1, add(0, 1))
  assert_eq(-1, add(0, -1))
  assert_eq(1, multiply(1, 1))
  assert_eq(-1, multiply(-1, 1))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(1, divide_with_ceil(99, 100))  // 0.99 -> 1
}

test "real_world_scenarios" {
  // 真实世界场景测试
  // 场景1：计算订单总价
  let product_price = 89
  let quantity = 4
  let discount = 20
  let subtotal = multiply(product_price, quantity)
  let final_total = add(subtotal, -discount)
  
  // 场景2：计算项目时间
  let tasks_per_day = 6
  let total_tasks = 25
  let days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  
  assert_eq(356, subtotal)
  assert_eq(336, final_total)
  assert_eq(5, days_needed)  // ceil(25/6) = 5
}