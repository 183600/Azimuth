// 标准 MoonBit 测试用例
// 测试 azimuth 包的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, azimuth::add(7, 3))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-8, azimuth::add(-3, -5))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(12, azimuth::multiply(3, 4))
  assert_eq(-15, azimuth::multiply(3, -5))
  assert_eq(20, azimuth::multiply(-4, -5))
}

test "multiply_with_zero_and_one" {
  // 测试乘法的零和一特殊情况
  assert_eq(0, azimuth::multiply(12345, 0))
  assert_eq(0, azimuth::multiply(0, 12345))
  assert_eq(12345, azimuth::multiply(12345, 1))
  assert_eq(12345, azimuth::multiply(1, 12345))
}

test "greet_function_basic" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "greet_with_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 张三!", azimuth::greet("张三"))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, @#$!", azimuth::greet("@#$"))
}

test "complex_calculation_sequence" {
  // 测试复杂计算序列
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  assert_eq(900, result)
}

test "mathematical_properties" {
  // 测试数学性质
  let x = 15
  let y = 25
  
  // 加法交换律
  assert_eq(azimuth::add(x, y), azimuth::add(y, x))
  
  // 乘法交换律
  assert_eq(azimuth::multiply(x, y), azimuth::multiply(y, x))
}

test "overflow_protection" {
  // 测试溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  
  // 测试负溢出
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "real_world_application" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 199
  let item1_quantity = 2
  let item2_price = 99
  let item2_quantity = 3
  
  // 计算每种商品总价
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let cart_total = azimuth::add(total1, total2)
  
  assert_eq(694, cart_total)  // 199*2 + 99*3 = 398 + 297 = 695
}