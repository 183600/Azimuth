// 标准 MoonBit 测试套件
// 包含针对 azimuth 库的高质量测试用例

test "add_function_comprehensive" {
  // 全面测试加法函数
  assert_eq(10, add(3, 7))
  assert_eq(0, add(-5, 5))
  assert_eq(-8, add(-3, -5))
  assert_eq(100, add(50, 50))
  assert_eq(1, add(0, 1))
}

test "multiply_function_properties" {
  // 测试乘法函数的数学性质
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 100))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(12, multiply(-3, -4))
  assert_eq(100, multiply(10, 10))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 张三!", greet("张三"))
}

test "divide_with_ceil_scenarios" {
  // 测试向上取整除法的各种场景
  assert_eq(3, divide_with_ceil(9, 3))
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(1, divide_with_ceil(5, 5))
  assert_eq(0, divide_with_ceil(5, 0))
  assert_eq(-3, divide_with_ceil(-9, 3))
}

test "complex_calculation_pipeline" {
  // 复杂计算流水线测试
  let step1 = add(10, 20)
  let step2 = multiply(step1, 3)
  let step3 = divide_with_ceil(step2, 5)
  assert_eq(18, step3)  // (10+20)*3/5 = 90/5 = 18
}

test "business_order_calculation" {
  // 业务订单计算测试
  let unit_price = 25
  let quantity = 4
  let tax_rate = 10
  let subtotal = multiply(unit_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(subtotal, tax)
  assert_eq(110, total)  // 25*4 + ceil(100*10/100) = 100 + 10 = 110
}

test "mathematical_identity_verification" {
  // 数学恒等式验证
  let a = 7
  let b = 13
  // 验证分配律: a * (b + 1) = a * b + a
  let left = multiply(a, add(b, 1))
  let right = add(multiply(a, b), a)
  assert_eq(left, right)
}

test "edge_case_zero_handling" {
  // 零值处理的边界情况
  assert_eq(5, add(5, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(0, multiply(0, 0))
  assert_eq(0, divide_with_ceil(0, 5))
  assert_eq(0, divide_with_ceil(0, 0))
}

test "negative_number_arithmetic" {
  // 负数算术运算
  assert_eq(-10, add(-3, -7))
  assert_eq(2, add(-5, 7))
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(-3, divide_with_ceil(-10, 4))
}

test "real_world_shopping_cart" {
  // 真实世界购物车计算
  let apple_price = 3
  let apple_qty = 5
  let banana_price = 2
  let banana_qty = 8
  let orange_price = 4
  let orange_qty = 3
  
  let apple_total = multiply(apple_price, apple_qty)
  let banana_total = multiply(banana_price, banana_qty)
  let orange_total = multiply(orange_price, orange_qty)
  
  let grand_total = add(add(apple_total, banana_total), orange_total)
  assert_eq(47, grand_total)  // 3*5 + 2*8 + 4*3 = 15 + 16 + 12 = 43
}