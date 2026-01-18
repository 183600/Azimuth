// Azimuth 项目综合测试套件
// 使用标准 MoonBit 测试语法，全面测试核心功能

test "add_positive_numbers" {
  // 测试正数加法
  assert_eq(5, add(2, 3))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-5, add(-2, -3))
  assert_eq(-100, add(-50, -50))
  assert_eq(0, add(-10, 10))
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  assert_eq(3, add(5, -2))
  assert_eq(-3, add(-5, 2))
  assert_eq(1, add(100, -99))
}

test "multiply_positive_numbers" {
  // 测试正数乘法
  assert_eq(6, multiply(2, 3))
  assert_eq(100, multiply(10, 10))
  assert_eq(0, multiply(0, 5))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(6, multiply(-2, -3))
  assert_eq(-6, multiply(2, -3))
  assert_eq(0, multiply(-5, 0))
}

test "greet_standard_names" {
  // 测试标准名称问候
  assert_eq_string("Hello, World!", greet("World"))
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
}

test "greet_empty_and_special" {
  // 测试空字符串和特殊字符
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello, @#$!", greet("@#$"))
}

test "greet_unicode_characters" {
  // 测试Unicode字符
  assert_eq_string("Hello, 测试!", greet("测试"))
  assert_eq_string("Hello, 🌙!", greet("🌙"))
  assert_eq_string("Hello, Ñoño!", greet("Ñoño"))
}

test "complex_calculations" {
  // 测试复合计算
  let a = 5
  let b = 3
  let c = 2
  
  // (a + b) * c
  let result1 = multiply(add(a, b), c)
  assert_eq(16, result1)
  
  // a * b + c
  let result2 = add(multiply(a, b), c)
  assert_eq(17, result2)
}

test "real_world_scenarios" {
  // 测试实际应用场景
  let price_per_item = 25
  let quantity = 4
  let tax_rate = 10
  
  // 计算小计
  let subtotal = multiply(price_per_item, quantity)
  assert_eq(100, subtotal)
  
  // 计算税额
  let tax = multiply(subtotal, tax_rate)
  assert_eq(1000, tax)
  
  // 计算总价（含税）
  let total = add(subtotal, tax)
  assert_eq(1100, total)
  
  // 测试问候功能在客户服务中的应用
  let customer_name = "张三"
  let welcome_message = greet(customer_name)
  assert_eq_string("Hello, 张三!", welcome_message)
}