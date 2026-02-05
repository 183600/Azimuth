// 标准 MoonBit 测试套件
// 包含10个高质量的标准测试用例，覆盖 azimuth 库的核心功能

test "add_function_edge_cases" {
  // 测试加法函数的边界情况
  assert_eq(0, add(0, 0))
  assert_eq(1000, add(1000, 0))
  assert_eq(-1000, add(-1000, 0))
  assert_eq(0, add(100, -100))
  assert_eq(2000000, add(1000000, 1000000))
}

test "multiply_function_comprehensive" {
  // 全面测试乘法函数
  assert_eq(1, multiply(1, 1))
  assert_eq(0, multiply(0, 100))
  assert_eq(-25, multiply(-5, 5))
  assert_eq(25, multiply(-5, -5))
  assert_eq(1000000, multiply(1000, 1000))
}

test "subtract_function_various_cases" {
  // 测试减法函数的各种情况
  assert_eq(5, subtract(10, 5))
  assert_eq(-5, subtract(5, 10))
  assert_eq(0, subtract(7, 7))
  assert_eq(15, subtract(0, -15))
  assert_eq(-20, subtract(-10, 10))
}

test "divide_with_ceil_precision_tests" {
  // 测试向上取整除法的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(4, divide_with_ceil(11, 3))
  assert_eq(100, divide_with_ceil(999, 10))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_unicode_support" {
  // 测试问候函数的Unicode支持
  assert_eq_string("Hello, 世界!", greet("世界"))
  assert_eq_string("Hello, 🚀!", greet("🚀"))
  assert_eq_string("Hello, Café!", greet("Café"))
  assert_eq_string("Hello, Москва!", greet("Москва"))
  assert_eq_string("Hello, 🌍!", greet("🌍"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let unit_price = 45
  let quantity = 12
  let discount = 50
  let shipping = 15
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(540, subtotal)
  assert_eq(555, total_before_discount)
  assert_eq(505, final_total)
}

test "packaging_and_shipping_calculation" {
  // 包装和运输计算
  let total_items = 87
  let items_per_box = 15
  let boxes_needed = divide_with_ceil(total_items, items_per_box)
  
  let total_weight = multiply(total_items, 2)  // 每件2kg
  let shipping_cost_per_box = 8
  let total_shipping = multiply(boxes_needed, shipping_cost_per_box)
  
  assert_eq(6, boxes_needed)
  assert_eq(174, total_weight)
  assert_eq(48, total_shipping)
}

test "mathematical_properties_verification" {
  // 验证数学性质
  let a = 13
  let b = 17
  
  // 加法交换律
  assert_eq(add(a, b), add(b, a))
  
  // 乘法交换律
  assert_eq(multiply(a, b), multiply(b, a))
  
  // 分配律
  let c = 5
  assert_eq(multiply(a, add(b, c)), add(multiply(a, b), multiply(a, c)))
}

test "error_handling_and_robustness" {
  // 错误处理和健壮性测试
  // 除零处理
  assert_eq(0, divide_with_ceil(100, 0))
  assert_eq(0, divide_with_ceil(-100, 0))
  assert_eq(0, divide_with_ceil(0, 0))
  
  // 大数处理
  let large_num = 100000
  assert_eq(200000, add(large_num, large_num))
  assert_eq(0, multiply(large_num, 0))
}

test "real_world_financial_scenario" {
  // 真实世界金融场景
  let principal = 5000
  let interest_rate = 6
  let years = 3
  
  // 简单的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  let year3_amount = add(year2_amount, divide_with_ceil(multiply(year2_amount, interest_rate), 100))
  
  assert_eq(5300, year1_amount)
  assert_eq(5618, year2_amount)
  assert_eq(5955, year3_amount)
}