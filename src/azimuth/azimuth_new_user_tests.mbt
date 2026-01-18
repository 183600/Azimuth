// Azimuth 项目用户测试用例
// 使用标准 MoonBit 测试语法，包含10个测试用例

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, azimuth::add(2, 3))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-10, azimuth::add(-3, -7))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(-15, azimuth::multiply(3, -5))
  assert_eq(12, azimuth::multiply(-4, -3))
}

test "greet_basic_functionality" {
  // 测试基本问候功能
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, 世界!", azimuth::greet("世界"))
  assert_eq_string("Hello, !", azimuth::greet(""))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(max_val, azimuth::add(max_val, max_val))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(min_val, azimuth::add(min_val, min_val))
}

test "multiply_overflow_protection" {
  // 测试乘法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::multiply(max_val, 2))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(min_val, azimuth::multiply(min_val, 2))
  assert_eq(0, azimuth::multiply(max_val, 0))
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let zero = 0
  let one = 1
  let neg_one = -1
  
  assert_eq(5, azimuth::add(5, zero))
  assert_eq(5, azimuth::add(zero, 5))
  assert_eq(5, azimuth::multiply(5, one))
  assert_eq(5, azimuth::multiply(one, 5))
  assert_eq(-5, azimuth::multiply(5, neg_one))
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  let a = 10
  let b = 20
  let c = 5
  
  // (a + b) * c
  let sum_ab = azimuth::add(a, b)
  let result = azimuth::multiply(sum_ab, c)
  assert_eq(150, result)
  
  // a * b + c
  let product_ab = azimuth::multiply(a, b)
  let result2 = azimuth::add(product_ab, c)
  assert_eq(205, result2)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  let long_name = "ThisIsAVeryLongNameForTestingPurposes"
  let special_chars = "test@example.com#123"
  let unicode_chars = "测试🌍123"
  
  assert_eq_string("Hello, " + long_name + "!", azimuth::greet(long_name))
  assert_eq_string("Hello, " + special_chars + "!", azimuth::greet(special_chars))
  assert_eq_string("Hello, " + unicode_chars + "!", azimuth::greet(unicode_chars))
}

test "real_world_calculation_scenarios" {
  // 测试实际计算场景
  // 计算矩形周长：2 * (长 + 宽)
  let length = 15
  let width = 10
  let perimeter = azimuth::multiply(2, azimuth::add(length, width))
  assert_eq(50, perimeter)
  
  // 计算购物车总价：商品1价格*数量 + 商品2价格*数量
  let price1 = 199
  let qty1 = 2
  let price2 = 299
  let qty2 = 1
  let total = azimuth::add(
    azimuth::multiply(price1, qty1),
    azimuth::multiply(price2, qty2)
  )
  assert_eq(697, total)
}

test "mathematical_properties_verification" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  
  // 加法交换律：a + b = b + a
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律：a * b = b * a
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 分配律：a * (b + c) = a * b + a * c
  let c = 5
  let left = azimuth::multiply(a, azimuth::add(b, c))
  let right = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left, right)
}