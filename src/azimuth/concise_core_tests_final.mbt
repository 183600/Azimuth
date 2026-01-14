test "add_basic_functionality" {
  // 测试基本的加法功能
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(-2, 2))
  assert_eq(-5, azimuth::add(-2, -3))
}

test "add_zero_handling" {
  // 测试零值处理
  assert_eq(5, azimuth::add(5, 0))
  assert_eq(5, azimuth::add(0, 5))
  assert_eq(0, azimuth::add(0, 0))
}

test "add_boundary_values" {
  // 测试边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本的乘法功能
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(-6, azimuth::multiply(-2, 3))
  assert_eq(6, azimuth::multiply(-2, -3))
}

test "multiply_zero_and_one" {
  // 测试零和一的乘法
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(5, azimuth::multiply(5, 1))
  assert_eq(5, azimuth::multiply(1, 5))
}

test "multiply_boundary_values" {
  // 测试乘法边界值
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
}

test "greet_basic_functionality" {
  // 测试基本的问候功能
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
}

test "greet_edge_cases" {
  // 测试问候函数的边界情况
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, A!", azimuth::greet("A"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "complex_calculation" {
  // 测试复合计算
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c
  let result = azimuth::multiply(azimuth::add(a, b), c)
  assert_eq(900, result)
  
  // a * b + c
  let result2 = azimuth::add(azimuth::multiply(a, b), c)
  assert_eq(230, result2)
}

test "real_world_scenario" {
  // 测试实际应用场景
  // 计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  
  assert_eq(617, total)
}
