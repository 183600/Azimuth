// 标准 MoonBit 测试用例
// 针对 azimuth 库核心功能的全面测试

test "add_commutative_property" {
  // 测试加法交换律
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(-5, 5))
  assert_eq(0, add(5, -5))
}

test "multiply_zero_property" {
  // 测试乘法零元素性质
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(-5, 0))
  assert_eq(0, multiply(0, -5))
}

test "divide_with_ceil_fractional_results" {
  // 测试向上取整除法的小数结果
  assert_eq(3, divide_with_ceil(8, 3))  // 2.666... -> 3
  assert_eq(5, divide_with_ceil(21, 4)) // 5.25 -> 5
  assert_eq(7, divide_with_ceil(20, 3)) // 6.666... -> 7
  assert_eq(2, divide_with_ceil(3, 2))  // 1.5 -> 2
}

test "greet_multilingual_support" {
  // 测试多语言支持
  assert_eq_string("Hello, Español!", greet("Español"))
  assert_eq_string("Hello, Русский!", greet("Русский"))
  assert_eq_string("Hello, 日本語!", greet("日本語"))
  assert_eq_string("Hello, العربية!", greet("العربية"))
}

test "combined_calculation_scenario" {
  // 测试组合计算场景
  let base_price = 45
  let quantity = 6
  let tax_rate = 10
  
  let subtotal = multiply(base_price, quantity)
  let tax_amount = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax_amount)
  
  assert_eq(270, subtotal)
  assert_eq(27, tax_amount)
  assert_eq(297, total)
}

test "negative_division_behavior" {
  // 测试负数除法行为
  assert_eq(-2, divide_with_ceil(-8, 4))   // -8/4 = -2 -> -2
  assert_eq(-1, divide_with_ceil(-3, 4))   // -3/4 = -0.75 -> -1
  assert_eq(2, divide_with_ceil(-8, -4))   // -8/-4 = 2 -> 2
  assert_eq(1, divide_with_ceil(-3, -4))   // -3/-4 = 0.75 -> 1
}

test "inventory_calculation" {
  // 测试库存计算场景
  let items_per_box = 24
  let total_items_small = 50
  let total_items_large = 125
  
  let boxes_small = divide_with_ceil(total_items_small, items_per_box)
  let boxes_large = divide_with_ceil(total_items_large, items_per_box)
  let total_boxes = add(boxes_small, boxes_large)
  
  assert_eq(3, boxes_small)   // ceil(50/24) = 3
  assert_eq(6, boxes_large)   // ceil(125/24) = 6
  assert_eq(9, total_boxes)
}

test "mathematical_distributive_law" {
  // 测试数学分配律 a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 5 * (8 + 12) = 5 * 8 + 5 * 12 = 100
}

test "string_special_characters" {
  // 测试特殊字符字符串处理
  assert_eq_string("Hello, test@example.com!", greet("test@example.com"))
  assert_eq_string("Hello, C++!", greet("C++"))
  assert_eq_string("Hello, 100%!", greet("100%"))
  assert_eq_string("Hello, (parentheses)!", greet("(parentheses)"))
}

test "complex_business_workflow" {
  // 测试复杂业务工作流
  let product_cost = 75
  let quantity = 12
  let discount_threshold = 800
  let discount_amount = 50
  let shipping = 25
  
  let subtotal = multiply(product_cost, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = if total_before_discount > discount_threshold {
    add(total_before_discount, -discount_amount)
  } else {
    total_before_discount
  }
  
  assert_eq(900, subtotal)
  assert_eq(925, total_before_discount)
  assert_eq(875, final_total)
}