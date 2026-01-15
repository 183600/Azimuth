// 简洁的 MoonBit 测试套件 - 包含10个标准测试用例

test "add_basic_functionality" {
  // 测试基本加法功能
  assert_eq(10, azimuth::add(3, 7))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-8, azimuth::add(-3, -5))
}

test "add_overflow_protection" {
  // 测试加法溢出保护
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::add(max_val, 1))
  assert_eq(min_val, azimuth::add(min_val, -1))
  assert_eq(-1, azimuth::add(max_val, min_val))
}

test "multiply_basic_functionality" {
  // 测试基本乘法功能
  assert_eq(15, azimuth::multiply(3, 5))
  assert_eq(-12, azimuth::multiply(-3, 4))
  assert_eq(0, azimuth::multiply(0, 100))
}

test "multiply_boundary_values" {
  // 测试乘法边界值处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, -1))
  assert_eq(0, azimuth::multiply(max_val, 0))
}

test "greet_function_variations" {
  // 测试greet函数的各种情况
  assert_eq_string("Hello, World!", azimuth::greet("World"))
  assert_eq_string("Hello, !", azimuth::greet(""))
  assert_eq_string("Hello, 测试!", azimuth::greet("测试"))
}

test "mathematical_expressions" {
  // 测试复合数学表达式
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result = azimuth::multiply(azimuth::add(a, b), c)
  assert_eq(16, result)
  
  // a * b + c
  let result2 = azimuth::add(azimuth::multiply(a, b), c)
  assert_eq(17, result2)
}

test "practical_calculation_scenario" {
  // 测试实际计算场景：购物车总价
  let price = 199
  let quantity = 3
  let shipping = 20
  
  let subtotal = azimuth::multiply(price, quantity)
  let total = azimuth::add(subtotal, shipping)
  
  assert_eq(617, total)
}

test "string_processing_edge_cases" {
  // 测试字符串处理边界情况
  let long_name = "VeryLongNameForTestingPurposes"
  let special_name = "test@example.com"
  let numeric_name = "12345"
  
  assert_eq_string("Hello, VeryLongNameForTestingPurposes!", azimuth::greet(long_name))
  assert_eq_string("Hello, test@example.com!", azimuth::greet(special_name))
  assert_eq_string("Hello, 12345!", azimuth::greet(numeric_name))
}

test "complex_business_logic" {
  // 测试复杂业务逻辑：订单折扣计算
  let order_total = 1200
  let discount_threshold = 1000
  let discount_rate = 10
  
  // 如果订单超过阈值，应用折扣
  let has_discount = order_total > discount_threshold
  let discount_amount = if has_discount {
    azimuth::multiply(order_total, discount_rate) / 100
  } else {
    0
  }
  
  let final_total = azimuth::add(order_total, -discount_amount)
  assert_eq(1080, final_total)
}

test "assertion_functions_validation" {
  // 测试断言函数的有效性
  assert_true(true)
  assert_false(false)
  assert_eq(42, 42)
  assert_eq_string("MoonBit", "MoonBit")
  
  // 测试计算结果的断言
  let calc_result = azimuth::add(15, 27)
  assert_true(calc_result == 42)
  assert_false(calc_result != 42)
}