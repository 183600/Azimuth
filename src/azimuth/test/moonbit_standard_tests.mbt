// MoonBit 标准测试用例
// 测试 azimuth 包的核心功能
test "add_function_positive_numbers" {
  // 测试正数加法
  @azimuth.assert_eq(10, azimuth::@azimuth.add(3, 7))
  @azimuth.assert_eq(100, azimuth::@azimuth.add(50, 50))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
}
test "add_function_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, azimuth::@azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, azimuth::@azimuth.add(-2, -3))
  @azimuth.assert_eq(0, azimuth::@azimuth.add(-5, 5))
}
test "add_function_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 0))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, 0))
  @azimuth.assert_eq(-1, azimuth::@azimuth.add(max_val, min_val))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
}
test "multiply_function_basic_cases" {
  // 测试基本乘法运算
  @azimuth.assert_eq(12, azimuth::@azimuth.multiply(3, 4))
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(5, 0))
  @azimuth.assert_eq(7, azimuth::@azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, azimuth::@azimuth.multiply(7, -1))
}
test "multiply_function_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(12, azimuth::@azimuth.multiply(-3, -4))
  @azimuth.assert_eq(-12, azimuth::@azimuth.multiply(3, -4))
  @azimuth.assert_eq(12, azimuth::@azimuth.multiply(-3, 4))
}
test "multiply_function_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(0, azimuth::@azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
}
test "greet_function_various_names" {
  // 测试不同名称的问候功能
  @azimuth.assert_eq_string("Hello, Alice!", azimuth::@azimuth.greet("Alice"))
  @azimuth.assert_eq_string("Hello, Bob!", azimuth::@azimuth.greet("Bob"))
  @azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", azimuth::@azimuth.greet("123"))
}
test "greet_function_special_characters" {
  // 测试包含特殊字符的问候功能
  @azimuth.assert_eq_string("Hello, 世界!", azimuth::@azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, test@example.com!", azimuth::@azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, User_123!", azimuth::@azimuth.greet("User_123"))
}
test "complex_arithmetic_expressions" {
  // 测试复合算术表达式
  // (a + b) * c
  let a = 5
  let b = 3
  let c = 4
  
  let sum = azimuth::@azimuth.add(a, b)
  let result = azimuth::@azimuth.multiply(sum, c)
  @azimuth.assert_eq(32, result)  // (5 + 3) * 4 = 32
}
test "real_world_calculation_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth::@azimuth.multiply(item_price, quantity)
  let total = azimuth::@azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(617, total)  // 199 * 3 + 20 = 617
}