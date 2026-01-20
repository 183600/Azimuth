// 标准 MoonBit 测试用例示例
// 测试 azimuth 库的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  @azimuth.assert_eq(@azimuth.add(3, 7), 10)
  @azimuth.assert_eq(@azimuth.add(-5, 5), 0)
  @azimuth.assert_eq(@azimuth.add(-3, -5), -8)
}

test "add_with_zero" {
  // 测试与0相加的情况
  @azimuth.assert_eq(@azimuth.add(42, 0), 42)
  @azimuth.assert_eq(@azimuth.add(0, 42), 42)
  @azimuth.assert_eq(@azimuth.add(0, 0), 0)
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  @azimuth.assert_eq(@azimuth.multiply(3, 5), 15)
  @azimuth.assert_eq(@azimuth.multiply(3, -4), -12)
  @azimuth.assert_eq(@azimuth.multiply(-3, -4), 12)
}

test "multiply_with_special_values" {
  // 测试与特殊值相乘的情况
  @azimuth.assert_eq(@azimuth.multiply(123, 0), 0)
  @azimuth.assert_eq(@azimuth.multiply(123, 1), 123)
  @azimuth.assert_eq(@azimuth.multiply(123, -1), -123)
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  @azimuth.assert_eq_string(@azimuth.greet("MoonBit"), "Hello, MoonBit!")
  @azimuth.assert_eq_string(@azimuth.greet("World"), "Hello, World!")
}

test "greet_edge_cases" {
  // 测试问候功能的边界情况
  @azimuth.assert_eq_string(@azimuth.greet(""), "Hello, !")
  @azimuth.assert_eq_string(@azimuth.greet("A"), "Hello, A!")
  @azimuth.assert_eq_string(@azimuth.greet("123"), "Hello, 123!")
}

test "overflow_protection" {
  // 测试溢出保护功能
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出保护
  @azimuth.assert_eq(@azimuth.add(max_val, 1), max_val)
  @azimuth.assert_eq(@azimuth.add(min_val, -1), min_val)
  
  // 乘法溢出保护
  @azimuth.assert_eq(@azimuth.multiply(max_val, 2), max_val)
  @azimuth.assert_eq(@azimuth.multiply(min_val, 2), min_val)
}

test "complex_calculations" {
  // 测试复杂计算场景
  // 计算 (2 + 3) * 4 - 5 = 20 - 5 = 15
  let sum = @azimuth.add(2, 3)
  let product = @azimuth.multiply(sum, 4)
  let result = @azimuth.add(product, -5)
  @azimuth.assert_eq(result, 15)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 分配律 (简化测试)
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, 3)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, 3)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格 × 数量 + 运费
  let price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = @azimuth.multiply(price, quantity)
  let total = @azimuth.add(subtotal, shipping)
  
  @azimuth.assert_eq(total, 617)  // 199*3 + 20 = 597 + 20 = 617
}