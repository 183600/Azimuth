// MoonBit 测试用例
// 包含10个标准测试用例，使用标准的 MoonBit 测试语法

test "add_negative_numbers" {
  // 测试负数相加
  assert_eq(-10, azimuth::add(-3, -7))
  assert_eq(0, azimuth::add(-5, 5))
  assert_eq(-100, azimuth::add(-50, -50))
}

test "multiply_by_negative" {
  // 测试乘以负数
  assert_eq(-15, azimuth::multiply(3, -5))
  assert_eq(20, azimuth::multiply(-4, -5))
  assert_eq(0, azimuth::multiply(0, -100))
}

test "greet_with_numbers" {
  // 测试包含数字的字符串
  assert_eq_string("Hello, 2023!", azimuth::greet("2023"))
  assert_eq_string("Hello, Test123!", azimuth::greet("Test123"))
  assert_eq_string("Hello, 007!", azimuth::greet("007"))
}

test "add_large_positive_numbers" {
  // 测试大正数相加
  assert_eq(1000000, azimuth::add(500000, 500000))
  assert_eq(2000000000, azimuth::add(1000000000, 1000000000))
}

test "multiply_boundary_values" {
  // 测试边界值乘法
  let max_val = 2147483647
  let min_val = -2147483648
  
  assert_eq(0, azimuth::multiply(max_val, 0))
  assert_eq(0, azimuth::multiply(min_val, 0))
  assert_eq(max_val, azimuth::multiply(max_val, 1))
  assert_eq(min_val, azimuth::multiply(min_val, 1))
}

test "greet_with_spaces" {
  // 测试包含空格的字符串
  assert_eq_string("Hello, John Doe!", azimuth::greet("John Doe"))
  assert_eq_string("Hello,   Multiple   Spaces!", azimuth::greet("  Multiple   Spaces"))
  assert_eq_string("Hello, Leading Space!", azimuth::greet(" Leading Space"))
  assert_eq_string("Hello, Trailing Space !", azimuth::greet("Trailing Space "))
}

test "complex_calculation_chain" {
  // 测试复杂计算链
  let a = 10
  let b = 20
  let c = 30
  
  // (a + b) * c - a
  let sum = azimuth::add(a, b)
  let product = azimuth::multiply(sum, c)
  let result = azimuth::add(product, -a)
  
  assert_eq(890, result)  // (10 + 20) * 30 - 10 = 30 * 30 - 10 = 900 - 10 = 890
}

test "add_sequence_test" {
  // 测试连续加法序列
  let numbers = [1, 2, 3, 4, 5]
  let sum1 = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  assert_eq(15, sum1)
  
  // 测试不同顺序的加法
  let sum2 = azimuth::add(azimuth::add(azimuth::add(azimuth::add(5, 4), 3), 2), 1)
  assert_eq(sum1, sum2)
}

test "multiply_sequence_test" {
  // 测试连续乘法序列
  // 2 * 3 * 4 = 24
  let product1 = azimuth::multiply(azimuth::multiply(2, 3), 4)
  assert_eq(24, product1)
  
  // 测试不同顺序的乘法
  let product2 = azimuth::multiply(azimuth::multiply(4, 3), 2)
  assert_eq(product1, product2)
  
  // 测试包含负数的序列
  let product3 = azimuth::multiply(azimuth::multiply(-2, 3), 4)
  assert_eq(-24, product3)
}

test "real_world_scenario" {
  // 测试实际应用场景：计算购物车总价
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  let discount = 20
  
  // 计算每种商品的总价
  let total1 = azimuth::multiply(item1_price, item1_quantity)
  let total2 = azimuth::multiply(item2_price, item2_quantity)
  
  // 计算购物车总价
  let subtotal = azimuth::add(total1, total2)
  
  // 应用折扣
  let final_total = azimuth::add(subtotal, -discount)
  
  assert_eq(280, final_total)  // (100*2 + 50*3) - 20 = (200 + 150) - 20 = 350 - 20 = 330
}