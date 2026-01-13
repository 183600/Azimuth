// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能

test "add_function_positive_numbers" {
  // 测试正数加法
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(100, azimuth::add(50, 50))
  assert_eq(0, azimuth::add(0, 0))
}

test "add_function_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(-5, azimuth::add(-2, -3))
  assert_eq(0, azimuth::add(-5, 5))
}

test "add_function_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 0))
  assert_eq(min_val, azimuth::add(min_val, 0))
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(max_val, azimuth::add(max_val, 1))
}

test "multiply_function_basic_cases" {
  // 测试基本乘法运算
  assert_eq(12, azimuth::multiply(3, 4))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(7, azimuth::multiply(7, 1))
  assert_eq(-7, azimuth::multiply(7, -1))
}

test "multiply_function_negative_numbers" {
  // 测试负数乘法
  assert_eq(12, azimuth::multiply(-3, -4))
  assert_eq(-12, azimuth::multiply(3, -4))
  assert_eq(12, azimuth::multiply(-3, 4))
}

test "multiply_function_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
}

test "greet_function_various_names" {
  // 测试不同名称的问候功能
  assert_eq_string("Hello, Alice!", azimuth::greet("Alice"))
  assert_eq_string("Hello, Bob!", azimuth::greet("Bob"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
}

test "greet_function_special_characters" {
  // 测试包含特殊字符的问候功能
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, test@example.com!", azimuth::greet("test@example.com"))
  assert_eq_string("Hello, User_123!", azimuth::greet("User_123"))
}

test "complex_arithmetic_expressions" {
  // 测试复合算术表达式
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  
  let sum = azimuth::add(a, b)
  let result = azimuth::multiply(sum, c)
  assert_eq(32, result)  // (5 + 3) * 4 = 32
}

test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  
  assert_eq(617, total)  // 199 * 3 + 20 = 617
}