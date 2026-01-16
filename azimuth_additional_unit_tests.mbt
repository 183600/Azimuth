// Azimuth 项目额外单元测试
// 使用标准 MoonBit 测试语法

test "add_zero_identity" {
  // 测试加法零恒等性
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(-17, 0), -17)
  assert_eq(azimuth::add(0, -17), -17)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(azimuth::add(-5, -3), -8)
  assert_eq(azimuth::add(-10, 5), -5)
  assert_eq(azimuth::add(7, -12), -5)
  assert_eq(azimuth::add(-100, -200), -300)
}

test "multiply_zero_property" {
  // 测试乘法零性质
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(42, 0), 0)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::multiply(-17, 0), 0)
  assert_eq(azimuth::multiply(0, -17), 0)
}

test "multiply_by_one" {
  // 测试乘以一的性质
  assert_eq(azimuth::multiply(1, 1), 1)
  assert_eq(azimuth::multiply(42, 1), 42)
  assert_eq(azimuth::multiply(1, 42), 42)
  assert_eq(azimuth::multiply(-17, 1), -17)
  assert_eq(azimuth::multiply(1, -17), -17)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(azimuth::multiply(-5, -3), 15)
  assert_eq(azimuth::multiply(-10, 5), -50)
  assert_eq(azimuth::multiply(7, -12), -84)
  assert_eq(azimuth::multiply(-100, -200), 20000)
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  assert_eq(azimuth::greet("!@#$"), "Hello, !@#$!")
  assert_eq(azimuth::greet("()[]{}"), "Hello, ()[]{}!")
  assert_eq(azimuth::greet(".,;:"), "Hello, .,;:!")
  assert_eq(azimuth::greet("\"quotes\""), "Hello, \"quotes\"!")
}

test "greet_multilingual" {
  // 测试多语言问候
  assert_eq(azimuth::greet("世界"), "Hello, 世界!")
  assert_eq(azimuth::greet("العالم"), "Hello, العالم!")
  assert_eq(azimuth::greet("мир"), "Hello, мир!")
  assert_eq(azimuth::greet("monde"), "Hello, monde!")
}

test "combined_operations" {
  // 测试组合运算
  // (a + b) * (c + d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  let result = azimuth::multiply(azimuth::add(a, b), azimuth::add(c, d))
  assert_eq(77, result)
  
  // a * b + c * d
  let result2 = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(c, d))
  assert_eq(42, result2)
}

test "real_world_scenarios" {
  // 测试现实世界场景
  
  // 购物车总价计算
  let item1_price = 25
  let item1_quantity = 2
  let item2_price = 15
  let item2_quantity = 3
  
  let item1_total = azimuth::multiply(item1_price, item1_quantity)
  let item2_total = azimuth::multiply(item2_price, item2_quantity)
  let cart_total = azimuth::add(item1_total, item2_total)
  
  assert_eq(95, cart_total)
  
  // 平均分计算
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let total_score = azimuth::add(azimuth::add(score1, score2), score3)
  let average_score = total_score / 3
  
  assert_eq(84, average_score)
}

test "boundary_values" {
  // 测试边界值
  
  // 大数运算
  let large1 = 100000
  let large2 = 200000
  assert_eq(300000, azimuth::add(large1, large2))
  assert_eq(20000000000, azimuth::multiply(large1, large2))
  
  // 单位数运算
  assert_eq(1, azimuth::add(1, 0))
  assert_eq(1, azimuth::multiply(1, 1))
  
  // 负边界
  assert_eq(-1, azimuth::add(0, -1))
  assert_eq(-1, azimuth::multiply(1, -1))
}