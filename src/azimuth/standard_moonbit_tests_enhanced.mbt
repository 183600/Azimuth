// 标准 MoonBit 测试用例 - 增强版
// 针对 azimuth 库的核心功能进行测试

test "add_function_basic_operations" {
  // 测试加法的基本运算
  assert_eq(15, add(7, 8))
  assert_eq(0, add(0, 0))
  assert_eq(-10, add(-5, -5))
  assert_eq(100, add(60, 40))
}

test "multiply_function_properties" {
  // 测试乘法的数学性质
  assert_eq(0, multiply(5, 0))
  assert_eq(1, multiply(1, 1))
  assert_eq(-12, multiply(-3, 4))
  assert_eq(144, multiply(12, 12))
}

test "divide_with_ceil_comprehensive" {
  // 全面测试向上取整除法
  assert_eq(4, divide_with_ceil(10, 3))
  assert_eq(5, divide_with_ceil(20, 4))
  assert_eq(-3, divide_with_ceil(-10, 3))
  assert_eq(0, divide_with_ceil(100, 0))
}

test "greet_function_various_scenarios" {
  // 测试问候函数的各种场景
  assert_eq_string("Hello, MoonBit!", greet("MoonBit"))
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, 中文测试!", greet("中文测试"))
}

test "complex_business_calculation" {
  // 复杂业务计算场景
  let base_price = 50
  let quantity = 8
  let tax_rate = 10
  let shipping = 25
  
  let subtotal = multiply(base_price, quantity)
  let tax = divide_with_ceil(multiply(subtotal, tax_rate), 100)
  let total = add(add(subtotal, tax), shipping)
  
  assert_eq(400, subtotal)
  assert_eq(40, tax)
  assert_eq(465, total)
}

test "mathematical_commutative_laws" {
  // 验证数学交换律
  let a = 17
  let b = 23
  
  assert_eq(add(a, b), add(b, a))
  assert_eq(multiply(a, b), multiply(b, a))
}

test "real_world_packaging_scenario" {
  // 现实世界包装场景
  let total_products = 58
  let products_per_carton = 12
  
  let cartons_needed = divide_with_ceil(total_products, products_per_carton)
  let unused_space = multiply(cartons_needed, products_per_carton) - total_products
  
  assert_eq(5, cartons_needed)
  assert_eq(2, unused_space)
}

test "string_greeting_edge_cases" {
  // 字符串问候边界情况测试
  assert_eq_string("Hello, A!", greet("A"))
  assert_eq_string("Hello,   !", greet("  "))
  assert_eq_string("Hello, special@chars!", greet("special@chars"))
  assert_eq_string("Hello, emoji🚀!", greet("emoji🚀"))
}

test "combined_arithmetic_expressions" {
  // 组合算术表达式测试
  let x = 6
  let y = 9
  let z = 4
  
  // 测试 (x + y) * z
  let result1 = multiply(add(x, y), z)
  assert_eq(60, result1)
  
  // 测试 x * y + z
  let result2 = add(multiply(x, y), z)
  assert_eq(58, result2)
}

test "financial_calculation_scenario" {
  // 金融计算场景
  let principal = 2000
  let interest_rate = 8
  let years = 2
  
  // 简化的复利计算
  let year1_amount = add(principal, divide_with_ceil(multiply(principal, interest_rate), 100))
  let year2_amount = add(year1_amount, divide_with_ceil(multiply(year1_amount, interest_rate), 100))
  
  assert_eq(2160, year1_amount)
  assert_eq(2333, year2_amount)
}