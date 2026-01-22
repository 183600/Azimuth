// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_function_basic" {
  let result = azimuth::@azimuth.add(10, 20)
  azimuth::@azimuth.assert_eq(30, result)
}

test "add_function_with_zero" {
  azimuth::@azimuth.assert_eq(15, azimuth::@azimuth.add(15, 0))
  azimuth::@azimuth.assert_eq(15, azimuth::@azimuth.add(0, 15))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.add(0, 0))
}

test "multiply_function_basic" {
  let result = azimuth::@azimuth.multiply(6, 7)
  azimuth::@azimuth.assert_eq(42, result)
}

test "multiply_function_with_zero_and_one" {
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(5, 0))
  azimuth::@azimuth.assert_eq(0, azimuth::@azimuth.multiply(0, 5))
  azimuth::@azimuth.assert_eq(8, azimuth::@azimuth.multiply(8, 1))
  azimuth::@azimuth.assert_eq(8, azimuth::@azimuth.multiply(1, 8))
}

test "greet_function_basic" {
  let result = azimuth::@azimuth.greet("MoonBit")
  azimuth::@azimuth.assert_eq_string("Hello, MoonBit!", result)
}

test "greet_function_edge_cases" {
  azimuth::@azimuth.assert_eq_string("Hello, !", azimuth::@azimuth.greet(""))
  azimuth::@azimuth.assert_eq_string("Hello, A!", azimuth::@azimuth.greet("A"))
  azimuth::@azimuth.assert_eq_string("Hello, 123!", azimuth::@azimuth.greet("123"))
}

test "mathematical_properties" {
  let a = 12
  let b = 8
  
  // 测试加法交换律
  azimuth::@azimuth.assert_eq(azimuth::@azimuth.add(a, b), azimuth::@azimuth.add(b, a))
  
  // 测试乘法交换律
  azimuth::@azimuth.assert_eq(azimuth::@azimuth.multiply(a, b), azimuth::@azimuth.multiply(b, a))
  
  // 测试分配律 (a * (b + c) = a * b + a * c)
  let c = 5
  let left = azimuth::@azimuth.multiply(a, azimuth::@azimuth.add(b, c))
  let right = azimuth::@azimuth.add(azimuth::@azimuth.multiply(a, b), azimuth::@azimuth.multiply(a, c))
  azimuth::@azimuth.assert_eq(left, right)
}

test "boundary_values_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值加法的溢出保护
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.add(max_val, 1))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.add(min_val, -1))
  
  // 测试边界值乘法的溢出保护
  azimuth::@azimuth.assert_eq(max_val, azimuth::@azimuth.multiply(max_val, 2))
  azimuth::@azimuth.assert_eq(min_val, azimuth::@azimuth.multiply(min_val, -1))
}

test "real_world_scenarios" {
  // 购物车计算场景
  let item_price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth::@azimuth.multiply(item_price, quantity)
  let total = azimuth::@azimuth.add(subtotal, shipping)
  
  azimuth::@azimuth.assert_eq(617, total)  // 199*3 + 20 = 597 + 20 = 617
}

test "string_processing_capabilities" {
  // 测试包含各种字符的字符串处理
  azimuth::@azimuth.assert_eq_string("Hello, Hello!", azimuth::@azimuth.greet("Hello"))
  azimuth::@azimuth.assert_eq_string("Hello, World123!", azimuth::@azimuth.greet("World123"))
  azimuth::@azimuth.assert_eq_string("Hello, test@example.com!", azimuth::@azimuth.greet("test@example.com"))
  azimuth::@azimuth.assert_eq_string("Hello, 用户测试!", azimuth::@azimuth.greet("用户测试"))
  azimuth::@azimuth.assert_eq_string("Hello, 🌟!", azimuth::@azimuth.greet("🌟"))
}