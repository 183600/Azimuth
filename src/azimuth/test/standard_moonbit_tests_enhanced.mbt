// 标准 MoonBit 测试用例 - 增强版
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic_operations" {
  // 测试加法的基本运算
  @azimuth.assert_eq(15, @azimuth.add(7, 8))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-10, @azimuth.add(-5, -5))
  @azimuth.assert_eq(100, @azimuth.add(60, 40))
}

test "multiply_function_properties" {
  // 测试乘法的数学性质
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
  @azimuth.assert_eq(-12, @azimuth.multiply(-3, 4))
  @azimuth.assert_eq(144, @azimuth.multiply(12, 12))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(10, 3))
  @azimuth.assert_eq(5, @azimuth.divide_with_ceil(20, 4))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
}

test "greet_function_various_scenarios" {
  // 测试问候函数的各种场景
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let base_price = 50
  let quantity = 8
  let tax_rate = 10
  let shipping = 25
  
  let subtotal = @azimuth.multiply(base_price, quantity)
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  let total = @azimuth.add(@azimuth.add(subtotal, tax), shipping)
  
  @azimuth.assert_eq(400, subtotal)
  @azimuth.assert_eq(40, tax)
  @azimuth.assert_eq(465, total)
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 17
  let b = 23
  
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
}

test "real_world_packaging_scenario" {
  // 现实世界包装场景
  let total_products = 58
  let products_per_carton = 12
  
  let cartons_needed = @azimuth.divide_with_ceil(total_products, products_per_carton)
  let unused_space = @azimuth.multiply(cartons_needed, products_per_carton) - total_products
  
  @azimuth.assert_eq(5, cartons_needed)
  @azimuth.assert_eq(2, unused_space)
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况测试
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, special@chars!", @azimuth.greet("special@chars"))
  @azimuth.assert_eq_string("Hello, emoji🚀!", @azimuth.greet("emoji🚀"))
}

test "combined_arithmetic_expressions" {
  // 组合算术表达式测试
  let x = 6
  let y = 9
  let z = 4
  
  // 测试 (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(60, result1)
  
  // 测试 x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(58, result2)
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 2000
  let interest_rate = 8
  let years = 2
  
  // 简化的复利计算
  let year1_amount = @azimuth.add(principal, @azimuth.divide_with_ceil(@azimuth.multiply(principal, interest_rate), 100))
  let year2_amount = @azimuth.add(year1_amount, @azimuth.divide_with_ceil(@azimuth.multiply(year1_amount, interest_rate), 100))
  
  @azimuth.assert_eq(2160, year1_amount)
  @azimuth.assert_eq(2333, year2_amount)
}