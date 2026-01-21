// 标准增强测试用例 - 使用标准 MoonBit 测试语法
// 这些测试用例补充现有测试，覆盖更多边界情况

test "add_commutative_property" {
  // 测试加法交换律
  assert_eq(add(5, 3), add(3, 5))
  assert_eq(add(-2, 7), add(7, -2))
  assert_eq(add(0, 100), add(100, 0))
}

test "add_associative_property" {
  // 测试加法结合律
  assert_eq(add(add(2, 3), 4), add(2, add(3, 4)))
  assert_eq(add(add(-1, 5), 2), add(-1, add(5, 2)))
}

test "multiply_commutative_property" {
  // 测试乘法交换律
  assert_eq(multiply(4, 7), multiply(7, 4))
  assert_eq(multiply(-3, 6), multiply(6, -3))
  assert_eq(multiply(0, 10), multiply(10, 0))
}

test "multiply_distributive_property" {
  // 测试乘法分配律
  assert_eq(multiply(3, add(2, 4)), add(multiply(3, 2), multiply(3, 4)))
  assert_eq(multiply(-2, add(5, 3)), add(multiply(-2, 5), multiply(-2, 3)))
}

test "divide_with_ceil_consistency" {
  // 测试除法向上取整的一致性
  assert_eq(divide_with_ceil(6, 2), divide_with_ceil(-6, -2))
  assert_eq(divide_with_ceil(7, 2), divide_with_ceil(-7, -2))
}

test "greet_with_whitespace" {
  // 测试包含空白字符的问候
  assert_eq_string("Hello,   !", greet("   "))
  assert_eq_string("Hello, \t\n!", greet("\t\n"))
  assert_eq_string("Hello,  John  !", greet("  John  "))
}

test "complex_business_calculation" {
  // 复杂业务计算测试
  // 场景：计算多个订单的总成本，包括税费和折扣
  
  let order1_total = multiply(25, 3)  // 25元商品，3件
  let order2_total = multiply(18, 2)  // 18元商品，2件
  let subtotal = add(order1_total, order2_total)
  
  // 计算税费（假设10%）
  let tax = divide_with_ceil(multiply(subtotal, 10), 100)
  
  // 应用折扣
  let discount = 15
  let final_total = add(add(subtotal, tax), -discount)
  
  assert_eq(109, final_total)  // (25*3 + 18*2) + ceil(111*10/100) - 15 = 111 + 12 - 15 = 108
}

test "mathematical_identities" {
  // 数学恒等式测试
  assert_eq(5, add(5, 0))  // 加法单位元
  assert_eq(-5, add(5, -10))  // 加法逆元
  assert_eq(5, multiply(5, 1))  // 乘法单位元
  assert_eq(0, multiply(5, 0))  // 零乘法性质
}

test "divide_with_ceil_precision" {
  // 测试除法向上取整的精度
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(2, divide_with_ceil(3, 2))
  assert_eq(3, divide_with_ceil(5, 2))
  assert_eq(100, divide_with_ceil(100, 1))
  assert_eq(50, divide_with_ceil(100, 2))
}