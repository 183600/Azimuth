// 额外的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行补充测试

test "add_function_commutative_property" {
  // 测试加法交换律: a + b = b + a
  let a = 15
  let b = 27
  assert_eq(add(a, b), add(b, a))
  assert_eq(42, add(15, 27))
  assert_eq(42, add(27, 15))
}

test "add_function_associative_property" {
  // 测试加法结合律: (a + b) + c = a + (b + c)
  let a = 10
  let b = 20
  let c = 30
  assert_eq(add(add(a, b), c), add(a, add(b, c)))
  assert_eq(60, add(add(10, 20), 30))
  assert_eq(60, add(10, add(20, 30)))
}

test "multiply_function_distributive_property" {
  // 测试乘法分配律: a * (b + c) = a * b + a * c
  let a = 5
  let b = 8
  let c = 12
  let left_side = multiply(a, add(b, c))
  let right_side = add(multiply(a, b), multiply(a, c))
  assert_eq(left_side, right_side)
  assert_eq(100, left_side)
}

test "multiply_function_zero_property" {
  // 测试乘法零性质: a * 0 = 0
  assert_eq(0, multiply(15, 0))
  assert_eq(0, multiply(0, 25))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, multiply(-100, 0))
}

test "greet_function_with_special_characters" {
  // 测试问候函数的特殊字符处理
  assert_eq_string("Hello, @user!", greet("@user"))
  assert_eq_string("Hello, #hashtag!", greet("#hashtag"))
  assert_eq_string("Hello, $price!", greet("$price"))
  assert_eq_string("Hello, %percent!", greet("%percent"))
}

test "greet_function_with_spaces" {
  // 测试问候函数的空格处理
  assert_eq_string("Hello, John Doe!", greet("John Doe"))
  assert_eq_string("Hello,   multiple   spaces!", greet("   multiple   spaces"))
  assert_eq_string("Hello, leading space!", greet(" leading space"))
  assert_eq_string("Hello, trailing space !", greet("trailing space "))
}

test "divide_with_ceil_comprehensive_positive" {
  // 全面测试正数的向上取整除法
  assert_eq(1, divide_with_ceil(5, 5))    // 完全相等
  assert_eq(2, divide_with_ceil(6, 5))    // 1.2 -> 2
  assert_eq(3, divide_with_ceil(16, 6))   // 2.666... -> 3
  assert_eq(10, divide_with_ceil(91, 10))  // 9.1 -> 10
  assert_eq(100, divide_with_ceil(999, 10)) // 99.9 -> 100
}

test "divide_with_ceil_comprehensive_negative" {
  // 全面测试负数的向上取整除法
  assert_eq(-1, divide_with_ceil(-5, 5))   // -1 -> -1
  assert_eq(-1, divide_with_ceil(-6, 5))   // -1.2 -> -1
  assert_eq(-2, divide_with_ceil(-16, 6))  // -2.666... -> -2
  assert_eq(-9, divide_with_ceil(-91, 10)) // -9.1 -> -9
  assert_eq(-99, divide_with_ceil(-999, 10)) // -99.9 -> -99
}

test "complex_business_calculation" {
  // 复杂业务计算：订单总价计算
  let unit_price = 45
  let quantity = 12
  let tax_rate = 8  // 8%
  let shipping_cost = 25
  
  // 计算小计
  let subtotal = multiply(unit_price, quantity)
  // 计算税费（向上取整）
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  // 计算总价
  let total = add(add(subtotal, tax), shipping_cost)
  
  assert_eq(540, subtotal)
  assert_eq(44, tax)
  assert_eq(609, total)
}

test "real_world_inventory_calculation" {
  // 真实世界库存计算
  let total_products = 157
  let box_capacity = 24
  let worker_capacity = 8
  
  // 计算需要的箱子数量
  let boxes_needed = divide_with_ceil(total_products, box_capacity)
  // 计算需要的工人数（每天处理8箱）
  let workers_needed = divide_with_ceil(boxes_needed, worker_capacity)
  
  assert_eq(7, boxes_needed)  // ceil(157/24) = 7
  assert_eq(1, workers_needed)  // ceil(7/8) = 1
}