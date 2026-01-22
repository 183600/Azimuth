// 高质量 MoonBit 测试用例
// 覆盖 azimuth 库的核心功能和边界情况

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  assert_eq(15, add(8, 7))
  assert_eq(15, add(7, 8))
  assert_eq(0, add(5, -5))
  assert_eq(0, add(-5, 5))
}

test "multiply_zero_property" {
  // 测试乘法零元素性质：a × 0 = 0
  assert_eq(0, multiply(10, 0))
  assert_eq(0, multiply(0, 10))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-100, 0))
}

test "greet_with_unicode_and_special_chars" {
  // 测试包含Unicode和特殊字符的问候
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🌟!", greet("🌟"))
  assert_eq_string("Hello, José!", greet("José"))
  assert_eq_string("Hello, !@#$%", greet("!@#$%"))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(11, 3))    // 11/3 = 3.666... -> 4
  assert_eq(3, divide_with_ceil(9, 3))     // 9/3 = 3 -> 3
  assert_eq(-3, divide_with_ceil(-10, 3))  // -10/3 = -3.333... -> -3
  assert_eq(4, divide_with_ceil(-11, -3))  // -11/-3 = 3.666... -> 4
  assert_eq(0, divide_with_ceil(5, 0))     // 除数为0时返回0
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价
  let product_price = 89
  let quantity = 4
  let shipping = 12
  let discount = 20
  
  let subtotal = multiply(product_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(356, subtotal)
  assert_eq(368, total_before_discount)
  assert_eq(348, final_total)
}

test "mathematical_distributive_property" {
  // 测试分配律：a × (b + c) = a × b + a × c
  let a = 3
  let b = 4
  let c = 5
  
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  
  assert_eq(left_side, right_side)  // 3 × (4 + 5) = 3 × 4 + 3 × 5 = 27
}

test "real_world_packaging_scenario" {
  // 真实世界场景：包装计算
  let total_items = 47
  let items_per_box = 12
  
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  let actual_capacity = multiply(boxes_needed, items_per_box)
  let wasted_space = add(actual_capacity, -total_items)
  
  assert_eq(4, boxes_needed)      // ceil(47/12) = 4
  assert_eq(48, actual_capacity)  // 4 × 12 = 48
  assert_eq(1, wasted_space)      // 48 - 47 = 1
}

test "extreme_boundary_values" {
  // 测试极端边界值
  assert_eq(2147483647, add(2147483646, 1))      // 接近Int最大值
  assert_eq(-2147483648, add(-2147483647, -1))   // 接近Int最小值
  assert_eq(0, add(2147483647, -2147483647))     // 最大值与相反数相加
  assert_eq(1, divide_with_ceil(1, 2147483647))  // 极大除数
}

test "string_greeting_edge_cases" {
  // 测试字符串问候的边界情况
  assert_eq_string("Hello, !", greet(""))           // 空字符串
  assert_eq_string("Hello, A!", greet("A"))         // 单字符
  assert_eq_string("Hello,   !", greet("  "))       // 空格字符串
  assert_eq_string("Hello, This is a very long name!", greet("This is a very long name"))  // 长字符串
}

test "complex_financial_calculation" {
  // 复杂金融计算：投资回报
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 简化的复利计算（每年计算一次，使用向上取整）
  let year1 = add(principal, divide_with_ceil(multiply(principal, rate_percent), 100))
  let year2 = add(year1, divide_with_ceil(multiply(year1, rate_percent), 100))
  let year3 = add(year2, divide_with_ceil(multiply(year2, rate_percent), 100))
  
  assert_eq(10500, year1)  // 10000 + ceil(10000×5/100) = 10000 + 500 = 10500
  assert_eq(11025, year2)  // 10500 + ceil(10500×5/100) = 10500 + 525 = 11025
  assert_eq(11576, year3)  // 11025 + ceil(11025×5/100) = 11025 + 551 = 11576
}