// Azimuth 项目增强测试用例
// 使用标准 MoonBit 测试语法

test "add_identity_elements" {
  // 测试加法单位元
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(-100, 0), -100)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(azimuth::add(-10, -20), -30)
  assert_eq(azimuth::add(-15, 25), 10)
  assert_eq(azimuth::add(30, -45), -15)
  assert_eq(azimuth::add(-50, 50), 0)
}

test "multiply_identity_elements" {
  // 测试乘法单位元
  assert_eq(azimuth::multiply(1, 1), 1)
  assert_eq(azimuth::multiply(42, 1), 42)
  assert_eq(azimuth::multiply(1, 42), 42)
  assert_eq(azimuth::multiply(0, 1), 0)
}

test "multiply_zero_property" {
  // 测试乘法零性质
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(123, 0), 0)
  assert_eq(azimuth::multiply(0, 456), 0)
  assert_eq(azimuth::multiply(-789, 0), 0)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(azimuth::multiply(-5, -5), 25)
  assert_eq(azimuth::multiply(-6, 7), -42)
  assert_eq(azimuth::multiply(8, -9), -72)
  assert_eq(azimuth::multiply(-10, -10), 100)
}

test "greet_various_names" {
  // 测试各种名称的问候
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
  assert_eq(azimuth::greet("Charlie"), "Hello, Charlie!")
  assert_eq(azimuth::greet("David"), "Hello, David!")
}

test "greet_numbers_and_symbols" {
  // 测试包含数字和符号的名称
  assert_eq(azimuth::greet("User123"), "Hello, User123!")
  assert_eq(azimuth::greet("test_user"), "Hello, test_user!")
  assert_eq(azimuth::greet("admin@site"), "Hello, admin@site!")
}

test "combined_operations_simple" {
  // 测试简单的组合运算
  // 计算 (2 + 3) * 4
  let temp = azimuth::add(2, 3)
  let result = azimuth::multiply(temp, 4)
  assert_eq(result, 20)
  
  // 计算 5 * (6 + 7)
  let temp2 = azimuth::add(6, 7)
  let result2 = azimuth::multiply(5, temp2)
  assert_eq(result2, 65)
}

test "practical_business_calculations" {
  // 测试实际业务计算
  // 计算总价：单价 × 数量
  let unit_price = 15
  let quantity = 8
  let subtotal = azimuth::multiply(unit_price, quantity)
  
  // 计算税费（假设税率为10%，使用整数运算）
  let tax_rate = 10
  let tax_amount = azimuth::multiply(subtotal, tax_rate) / 100
  
  // 计算总价
  let total = azimuth::add(subtotal, tax_amount)
  
  assert_eq(subtotal, 120)
  assert_eq(tax_amount, 12)
  assert_eq(total, 132)
}