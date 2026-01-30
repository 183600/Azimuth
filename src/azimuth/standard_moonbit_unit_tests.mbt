// 标准 MoonBit 单元测试用例
// 针对 azimuth 库核心功能的标准测试

test "add_function_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
  assert_eq(-10, add(-3, -7))
  assert_eq(-10, add(-7, -3))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 4
  let b = 6
  let c = 9
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)  // 4 × (6 + 9) = 4 × 6 + 4 × 9 = 60
}

test "divide_with_ceil_precision_test" {
  // 测试向上取整除法的精度
  assert_eq(4, divide_with_ceil(11, 3))   // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))    // 9/3 = 3 -> 3
  assert_eq(1, divide_with_ceil(1, 100))  // 1/100 = 0.01 -> 1
  assert_eq(2, divide_with_ceil(3, 2))    // 3/2 = 1.5 -> 2
  assert_eq(-3, divide_with_ceil(-10, 3)) // -10/3 = -3.333... -> -3
  assert_eq(0, divide_with_ceil(5, 0))    // 除数为0时的安全处理
}

test "greet_function_internationalization" {
  // 测试问候函数的国际化支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
  assert_eq_string("Hello, !", greet(""))  // 空字符串处理
}

test "complex_business_calculation" {
  // 测试复杂业务计算场景
  // 订单计算：商品价格 × 数量 + 税费 - 折扣
  let product_price = 89
  let quantity = 4
  let tax_rate = 8
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total_before_discount = add(subtotal, tax_amount)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(29, tax_amount)
  assert_eq(385, total_before_discount)
  assert_eq(365, final_total)
}

test "mathematical_identity_elements" {
  // 测试数学单位元性质
  // 加法单位元：a + 0 = a
  assert_eq(42, add(42, 0))
  assert_eq(-17, add(-17, 0))
  assert_eq(0, add(0, 0))
  
  // 乘法单位元：a × 1 = a
  assert_eq(42, multiply(42, 1))
  assert_eq(-17, multiply(-17, 1))
  assert_eq(0, multiply(0, 1))
}

test "packaging_optimization_scenario" {
  // 测试包装优化场景
  let small_items = 25
  let medium_items = 47
  let large_items = 83
  let items_per_small_box = 6
  let items_per_medium_box = 12
  let items_per_large_box = 24
  
  let small_boxes = divide_with_ceil(small_items, items_per_small_box)
  let medium_boxes = divide_with_ceil(medium_items, items_per_medium_box)
  let large_boxes = divide_with_ceil(large_items, items_per_large_box)
  let total_boxes = add(add(small_boxes, medium_boxes), large_boxes)
  
  assert_eq(5, small_boxes)   // ceil(25/6) = 5
  assert_eq(4, medium_boxes)  // ceil(47/12) = 4
  assert_eq(4, large_boxes)   // ceil(83/24) = 4
  assert_eq(13, total_boxes)  // 5 + 4 + 4 = 13
}

test "negative_number_arithmetic" {
  // 测试负数算术运算
  // 加法与负数
  assert_eq(-8, add(-3, -5))
  assert_eq(-2, add(3, -5))
  assert_eq(2, add(-3, 5))
  
  // 乘法与负数
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  
  // 除法与负数
  assert_eq(-2, divide_with_ceil(-7, 3))
  assert_eq(3, divide_with_ceil(-7, -3))
  assert_eq(0, divide_with_ceil(0, -5))
}

test "time_calculation_workdays" {
  // 测试工作日计算场景
  let total_tasks = 87
  let tasks_per_day = 8
  let work_days_per_week = 5
  
  let work_days_needed = divide_with_ceil(total_tasks, tasks_per_day)
  let weeks_needed = divide_with_ceil(work_days_needed, work_days_per_week)
  let total_work_hours = multiply(work_days_needed, 8)
  
  assert_eq(11, work_days_needed)  // ceil(87/8) = 11
  assert_eq(3, weeks_needed)       // ceil(11/5) = 3
  assert_eq(88, total_work_hours)  // 11 × 8 = 88
}

test "string_edge_cases_and_special_characters" {
  // 测试字符串边界情况和特殊字符
  assert_eq_string("Hello, A!", greet("A"))  // 单字符
  assert_eq_string("Hello,   !", greet("   "))  // 空格字符串
  assert_eq_string("Hello, \t\n\r!", greet("\t\n\r"))  // 控制字符
  assert_eq_string("Hello, @#$%^&*()!", greet("@#$%^&*()"))  // 特殊符号
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))  // 邮箱格式
  assert_eq_string("Hello, <>&\"'!", greet("<>&\"'"))  // HTML特殊字符
}