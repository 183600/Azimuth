// 高质量 MoonBit 测试用例
// 针对 azimuth 库的全面测试，包含边界情况和实际应用场景

test "mathematical_commutative_and_associative_laws" {
  // 测试数学交换律和结合律
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  azimuth::assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  azimuth::assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 加法结合律
  azimuth::assert_eq(
    azimuth::add(azimuth::add(a, b), c),
    azimuth::add(a, azimuth::add(b, c))
  )
}

test "divide_with_ceil_comprehensive_edge_cases" {
  // 全面测试向上取整除法的边界情况
  // 正数除法
  azimuth::assert_eq(4, azimuth::divide_with_ceil(11, 3))  // 11/3 = 3.666... -> 4
  azimuth::assert_eq(3, azimuth::divide_with_ceil(9, 3))   // 9/3 = 3 -> 3
  azimuth::assert_eq(1, azimuth::divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  
  // 负数除法
  azimuth::assert_eq(-3, azimuth::divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  azimuth::assert_eq(4, azimuth::divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  
  // 零除数安全处理
  azimuth::assert_eq(0, azimuth::divide_with_ceil(100, 0))
  azimuth::assert_eq(0, azimuth::divide_with_ceil(0, 0))
}

test "complex_business_calculation_scenario" {
  // 复杂业务计算场景：订单总价计算
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping = 12
  let discount = 20
  
  // 计算小计
  let subtotal = azimuth::multiply(product_price, quantity)
  // 计算税费
  let tax = azimuth::divide_with_ceil(azimuth::multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total_before_discount = azimuth::add(subtotal, azimuth::add(tax, shipping))
  let final_total = azimuth::add(total_before_discount, -discount)
  
  azimuth::assert_eq(356, subtotal)
  azimuth::assert_eq(29, tax)
  azimuth::assert_eq(397, total_before_discount)
  azimuth::assert_eq(377, final_total)
}

test "string_greeting_multilingual_support" {
  // 测试多语言字符串问候支持
  azimuth::assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  azimuth::assert_eq_string("Hello, Español!", azimuth::greet("Español"))
  azimuth::assert_eq_string("Hello, Русский!", azimuth::greet("Русский"))
  azimuth::assert_eq_string("Hello, العربية!", azimuth::greet("العربية"))
  azimuth::assert_eq_string("Hello, 🚀!", azimuth::greet("🚀"))
  azimuth::assert_eq_string("Hello, !", azimuth::greet(""))
}

test "inventory_management_calculation" {
  // 库存管理计算场景
  let items_per_box = 24
  let total_items_small = 100
  let total_items_medium = 250
  let total_items_large = 1000
  
  let boxes_small = azimuth::divide_with_ceil(total_items_small, items_per_box)
  let boxes_medium = azimuth::divide_with_ceil(total_items_medium, items_per_box)
  let boxes_large = azimuth::divide_with_ceil(total_items_large, items_per_box)
  
  let total_boxes = azimuth::add(azimuth::add(boxes_small, boxes_medium), boxes_large)
  
  azimuth::assert_eq(5, boxes_small)   // ceil(100/24) = 5
  azimuth::assert_eq(11, boxes_medium) // ceil(250/24) = 11
  azimuth::assert_eq(42, boxes_large)  // ceil(1000/24) = 42
  azimuth::assert_eq(58, total_boxes)  // 5 + 11 + 42 = 58
}

test "mathematical_distributive_property" {
  // 测试数学分配律：a * (b + c) = a * b + a * c
  let a = 3
  let b = 4
  let c = 5
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  azimuth::assert_eq(left_side, right_side)  // 3 * (4 + 5) = 3 * 4 + 3 * 5 = 27
}

test "financial_compound_interest_simulation" {
  // 金融复利计算模拟
  let principal = 1000
  let rate_percent = 5
  
  // 简化的复利计算（每年计算一次）
  let amount_after_year1 = azimuth::add(principal, azimuth::divide_with_ceil(azimuth::multiply(principal, rate_percent), 100))
  let amount_after_year2 = azimuth::add(amount_after_year1, azimuth::divide_with_ceil(azimuth::multiply(amount_after_year1, rate_percent), 100))
  let amount_after_year3 = azimuth::add(amount_after_year2, azimuth::divide_with_ceil(azimuth::multiply(amount_after_year2, rate_percent), 100))
  
  azimuth::assert_eq(1050, amount_after_year1)
  azimuth::assert_eq(1103, amount_after_year2)
  azimuth::assert_eq(1158, amount_after_year3)
}

test "extreme_boundary_values_testing" {
  // 极端边界值测试
  // 测试大数运算
  azimuth::assert_eq(2000000, azimuth::add(1000000, 1000000))
  azimuth::assert_eq(1000000, azimuth::multiply(1000, 1000))
  
  // 测试零的运算
  azimuth::assert_eq(0, azimuth::multiply(0, 0))
  azimuth::assert_eq(0, azimuth::multiply(5, 0))
  azimuth::assert_eq(5, azimuth::add(5, 0))
  
  // 测试负数运算
  azimuth::assert_eq(-10, azimuth::add(-3, -7))
  azimuth::assert_eq(15, azimuth::multiply(-3, -5))
  azimuth::assert_eq(-15, azimuth::multiply(3, -5))
}

test "time_and_resource_calculation" {
  // 时间和资源计算场景
  let tasks_per_day = 8
  let total_tasks = 45
  let days_needed = azimuth::divide_with_ceil(total_tasks, tasks_per_day)
  
  // 计算工作小时数（每天8小时）
  let total_hours = azimuth::multiply(days_needed, 8)
  
  // 计算项目总成本（每小时50元）
  let hourly_rate = 50
  let total_cost = azimuth::multiply(total_hours, hourly_rate)
  
  azimuth::assert_eq(6, days_needed)    // ceil(45/8) = 6
  azimuth::assert_eq(48, total_hours)   // 6 * 8 = 48
  azimuth::assert_eq(2400, total_cost)  // 48 * 50 = 2400
}