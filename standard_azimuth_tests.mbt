// 标准 MoonBit 测试用例
// 测试 azimuth 模块的核心功能
// 使用标准的 MoonBit 测试语法

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(5, azimuth.add(2, 3))
  assert_eq(0, azimuth.add(0, 0))
  assert_eq(-3, azimuth.add(-1, -2))
  assert_eq(10, azimuth.add(15, -5))
}

test "add_boundary_values" {
  // 测试边界值的加法
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试零值加法
  assert_eq(42, azimuth.add(42, 0))
  assert_eq(42, azimuth.add(0, 42))
  assert_eq(min_val, azimuth.add(min_val, 0))
  assert_eq(max_val, azimuth.add(0, max_val))
  
  // 测试溢出保护
  assert_eq(max_val, azimuth.add(max_val, 1))
  assert_eq(min_val, azimuth.add(min_val, -1))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(6, azimuth.multiply(2, 3))
  assert_eq(0, azimuth.multiply(5, 0))
  assert_eq(15, azimuth.multiply(3, 5))
  assert_eq(-12, azimuth.multiply(-3, 4))
}

test "multiply_special_cases" {
  // 测试乘法的特殊情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试与1和-1相乘
  assert_eq(42, azimuth.multiply(42, 1))
  assert_eq(42, azimuth.multiply(1, 42))
  assert_eq(-42, azimuth.multiply(42, -1))
  assert_eq(-42, azimuth.multiply(-1, 42))
  
  // 测试最小值与-1相乘的特殊情况
  assert_eq(min_val, azimuth.multiply(min_val, -1))
  
  // 测试溢出保护
  assert_eq(max_val, azimuth.multiply(max_val, 2))
}

test "greet_functionality" {
  // 测试问候功能
  assert_eq_string("Hello, World!", azimuth.greet("World"))
  assert_eq_string("Hello, MoonBit!", azimuth.greet("MoonBit"))
  assert_eq_string("Hello, !", azimuth.greet(""))
}

test "greet_special_characters" {
  // 测试包含特殊字符的问候
  assert_eq_string("Hello, 测试!", azimuth.greet("测试"))
  assert_eq_string("Hello, 🌍!", azimuth.greet("🌍"))
  assert_eq_string("Hello, @#$!", azimuth.greet("@#$"))
}

test "complex_calculation_scenario" {
  // 测试复杂计算场景：计算矩形的周长
  let length = 10
  let width = 5
  
  // 周长 = 2 * (长 + 宽)
  let perimeter = azimuth.multiply(2, azimuth.add(length, width))
  assert_eq(30, perimeter)
}

test "financial_calculation" {
  // 测试金融计算场景
  let principal = 1000  // 本金
  let rate = 5          // 利率 5%
  let years = 3         // 年数
  
  // 简化的复利计算：本息和 = 本金 + 本金 × 利率 × 年数
  let interest = azimuth.multiply(principal, azimuth.multiply(rate, years))
  let total = azimuth.add(principal, interest)
  
  // 1000 + 1000*5*3 = 16000 (表示1600.0元)
  assert_eq(16000, total)
}

test "mathematical_properties" {
  // 测试数学性质
  let a = 7
  let b = 11
  
  // 加法交换律
  assert_eq(azimuth.add(a, b), azimuth.add(b, a))
  
  // 乘法交换律
  assert_eq(azimuth.multiply(a, b), azimuth.multiply(b, a))
  
  // 分配律：a × (b + c) = a × b + a × c
  let c = 3
  let left_side = azimuth.multiply(a, azimuth.add(b, c))
  let right_side = azimuth.add(azimuth.multiply(a, b), azimuth.multiply(a, c))
  assert_eq(left_side, right_side)
}

test "real_world_application" {
  // 测试实际应用场景：计算订单总价
  let item_price = 199
  let quantity = 3
  let shipping = 15
  let tax_rate = 10  // 10%
  
  // 计算商品总价
  let subtotal = azimuth.multiply(item_price, quantity)
  // 计算税费
  let tax = azimuth.multiply(subtotal, tax_rate) / 100
  // 计算最终总价（含运费）
  let total = azimuth.add(azimuth.add(subtotal, tax), shipping)
  
  // (199×3) + (199×3×10/100) + 15 = 597 + 59 + 15 = 674
  assert_eq(674, total)
}