// Azimuth 项目综合单元测试
// 使用标准 MoonBit 测试语法，覆盖核心功能的各个方面

test "add_positive_numbers" {
  // 测试正数加法
  assert_eq(10, add(3, 7))
  assert_eq(100, add(50, 50))
  assert_eq(0, add(0, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(-10, add(-3, -7))
  assert_eq(-5, add(-8, 3))
  assert_eq(-100, add(-50, -50))
}

test "add_mixed_numbers" {
  // 测试正负数混合加法
  assert_eq(5, add(10, -5))
  assert_eq(-3, add(-10, 7))
  assert_eq(0, add(20, -20))
}

test "multiply_positive_numbers" {
  // 测试正数乘法
  assert_eq(15, multiply(3, 5))
  assert_eq(100, multiply(10, 10))
  assert_eq(0, multiply(0, 5))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(15, multiply(-3, -5))
  assert_eq(-15, multiply(3, -5))
  assert_eq(0, multiply(-5, 0))
}

test "greet_standard_names" {
  // 测试标准名称问候
  assert_eq_string("Hello, Alice!", greet("Alice"))
  assert_eq_string("Hello, Bob!", greet("Bob"))
  assert_eq_string("Hello, Charlie!", greet("Charlie"))
}

test "greet_edge_cases" {
  // 测试边缘情况
  assert_eq_string("Hello, !", greet(""))
  assert_eq_string("Hello, 123!", greet("123"))
  assert_eq_string("Hello,   !", greet("   "))
}

test "combined_operations" {
  // 测试组合运算
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

test "real_world_scenario" {
  // 测试实际应用场景：计算购物折扣
  let original_price = 100
  let discount_percentage = 20
  let tax_rate = 8
  
  // 计算折扣价
  let discount_amount = multiply(original_price, discount_percentage)
  let discounted_price = add(original_price, -discount_amount)
  
  // 计算最终价格（含税）
  let tax_amount = multiply(discounted_price, tax_rate)
  let final_price = add(discounted_price, tax_amount)
  
  assert_eq(80, discounted_price)
  assert_eq(86, final_price)
}