// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(100, azimuth::add(50, 50))
}

test "add_boundary_values" {
  // 测试边界值加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
  assert_eq(0, azimuth::add(min_val, 1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, azimuth::multiply(2, 3))
  assert_eq(0, azimuth::multiply(5, 0))
  assert_eq(-6, azimuth::multiply(-2, 3))
  assert_eq(15, azimuth::multiply(3, 5))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "greet_function_variations" {
  // 测试问候函数的各种情况
  assert_eq_string("Hello, MoonBit!", azimuth::greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 123!", azimuth::greet("123"))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "complex_arithmetic_operations" {
  // 测试复杂算术运算
  // 计算：(2 + 3) * 4 - 5 = 20 - 5 = 15
  let sum = azimuth::add(2, 3)
  let product = azimuth::multiply(sum, 4)
  let result = azimuth::add(product, -5)
  assert_eq(15, result)
  
  // 计算：10 * (5 + 3) = 10 * 8 = 80
  let sum2 = azimuth::add(5, 3)
  let result2 = azimuth::multiply(10, sum2)
  assert_eq(80, result2)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 分配律
  let c = 3
  assert_eq(azimuth::multiply(a, azimuth::add(b, c)), 
            azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c)))
}

test "real_world_scenarios" {
  // 测试实际应用场景
  // 购物车计算：商品价格 × 数量 + 运费
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let subtotal = azimuth::multiply(item_price, quantity)
  let total = azimuth::add(subtotal, shipping)
  assert_eq(612, total)  // 199 * 3 + 15 = 597 + 15 = 612
  
  // 温度转换：摄氏度转华氏度（简化版：F = C × 2 + 30）
  let celsius = 25
  let fahrenheit = azimuth::add(azimuth::multiply(celsius, 2), 30)
  assert_eq(80, fahrenheit)  // 25 * 2 + 30 = 80
}

test "string_processing_edge_cases" {
  // 测试字符串处理的边界情况
  // 包含特殊字符的字符串
  assert_eq_string("Hello, user@example.com!", azimuth::greet("user@example.com"))
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  assert_eq_string("Hello,   spaced   !", azimuth::greet("  spaced   "))
  assert_eq_string("Hello, 🌍!", azimuth::greet("🌍"))
}

test "performance_oriented_calculations" {
  // 测试性能导向的计算
  // 大数运算
  assert_eq(2000000, azimuth::add(1000000, 1000000))
  assert_eq(1000000, azimuth::multiply(1000, 1000))
  
  // 连续运算
  let chain_result = azimuth::add(
    azimuth::multiply(10, 20),
    azimuth::multiply(30, 40)
  )
  assert_eq(1400, chain_result)  // 200 + 1200 = 1400
  
  // 嵌套运算
  let nested_result = azimuth::multiply(
    azimuth::add(5, 10),
    azimuth::add(3, 7)
  )
  assert_eq(225, nested_result)  // 15 * 15 = 225
}