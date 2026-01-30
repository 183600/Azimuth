// 新增的标准 MoonBit 测试用例
// 针对 azimuth 库的核心功能进行测试

test "standard_addition_commutativity" {
  // 测试加法交换律
  let a = 15
  let b = 27
  assert_eq(a + b, b + a)
  assert_eq(42, add(a, b))
  assert_eq(42, add(b, a))
}

test "standard_multiplication_properties" {
  // 测试乘法性质
  let x = 8
  let y = 7
  
  // 交换律
  assert_eq(x * y, y * x)
  assert_eq(56, multiply(x, y))
  assert_eq(56, multiply(y, x))
  
  // 与零相乘
  assert_eq(0, multiply(x, 0))
  assert_eq(0, multiply(0, y))
}

test "standard_divide_ceil_edge_cases" {
  // 测试向上取整除法的边界情况
  assert_eq(1, divide_with_ceil(5, 10))  // 小数向上取整
  assert_eq(3, divide_with_ceil(15, 5))  // 整除
  assert_eq(0, divide_with_ceil(0, 8))   // 零除法
  assert_eq(0, divide_with_ceil(10, 0))  // 除数为零
}

test "standard_greet_functionality" {
  // 测试问候函数的标准功能
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, 2024!", greet("2024"))
  assert_eq_string("Hello, !", greet(""))
}

test "standard_complex_calculations" {
  // 测试复合计算
  let base = 12
  let multiplier = 3
  let offset = 7
  
  // (base * multiplier) + offset
  let result = add(multiply(base, multiplier), offset)
  assert_eq(43, result)
  
  // 测试嵌套调用
  let nested = multiply(add(base, offset), multiplier)
  assert_eq(57, nested)
}

test "standard_negative_number_operations" {
  // 测试负数运算
  let neg_a = -15
  let neg_b = -8
  let pos_c = 10
  
  assert_eq(-23, add(neg_a, neg_b))
  assert_eq(-5, add(neg_a, pos_c))
  assert_eq(120, multiply(neg_a, neg_b))
  assert_eq(-150, multiply(neg_a, pos_c))
}

test "standard_real_world_scenario" {
  // 测试实际应用场景：计算订单总价
  let unit_price = 45
  let quantity = 6
  let shipping_cost = 12
  let discount = 20
  
  let subtotal = multiply(unit_price, quantity)
  let total_before_discount = add(subtotal, shipping_cost)
  let final_total = add(total_before_discount, -discount)
  
  assert_eq(270, subtotal)
  assert_eq(282, total_before_discount)
  assert_eq(262, final_total)
}

test "standard_mathematical_identities" {
  // 测试数学恒等式
  let x = 17
  
  // 加法单位元
  assert_eq(x, add(x, 0))
  assert_eq(x, add(0, x))
  
  // 乘法单位元
  assert_eq(x, multiply(x, 1))
  assert_eq(x, multiply(1, x))
}

test "standard_boundary_values" {
  // 测试边界值
  assert_eq(1, add(1, 0))
  assert_eq(-1, add(-1, 0))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, divide_with_ceil(1, 1))
  assert_eq(100, divide_with_ceil(999, 10))
}

test "standard_string_processing" {
  // 测试字符串处理
  let username = "User123"
  let domain = "example.com"
  let email = username + "@" + domain
  let greeting = greet(email)
  
  assert_eq_string("Hello, User123@example.com!", greeting)
  assert_eq_string("Hello, 测试!", greet("测试"))
}