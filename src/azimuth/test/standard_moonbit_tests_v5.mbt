// 标准 MoonBit 测试用例 v5
// 覆盖 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, add(3, 7))
  assert_eq(0, add(0, 0))
  assert_eq(-5, add(-2, -3))
}

test "add_with_zero" {
  // 测试与零相加的特殊情况
  assert_eq(42, add(42, 0))
  assert_eq(42, add(0, 42))
  assert_eq(0, add(0, 0))
}

test "add_overflow_protection" {
  // 测试溢出保护机制
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数溢出
  assert_eq(max_val, add(max_val, 1))
  assert_eq(max_val, add(max_val, max_val))
  
  // 负数溢出
  assert_eq(min_val, add(min_val, -1))
  assert_eq(min_val, add(min_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, multiply(3, 5))
  assert_eq(0, multiply(0, 5))
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 5))
  assert_eq(5, multiply(5, 1))
}

test "multiply_with_negative_numbers" {
  // 测试负数乘法
  assert_eq(-15, multiply(3, -5))
  assert_eq(-15, multiply(-3, 5))
  assert_eq(15, multiply(-3, -5))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 乘法溢出
  assert_eq(max_val, multiply(max_val, 2))
  assert_eq(min_val, multiply(min_val, -1))
  assert_eq(min_val, multiply(min_val, 2))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
}

test "greet_edge_cases" {
  // 测试问候功能的边界情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello, 123!", greet("123"))
}

test "complex_calculations" {
  // 测试复杂计算场景
  // (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let sum = add(a, b)
  let product = multiply(sum, c)
  let result = add(product, -d)
  
  assert_eq(22, result)
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 100
  let item1_qty = 2
  let item2_price = 50
  let item2_qty = 3
  let shipping = 15
  
  let total1 = multiply(item1_price, item1_qty)
  let total2 = multiply(item2_price, item2_qty)
  let subtotal = add(total1, total2)
  let final_total = add(subtotal, shipping)
  
  assert_eq(265, final_total)
}