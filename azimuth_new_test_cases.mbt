// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法，包含多样化的测试场景

test "add_sequence_numbers" {
  // 测试序列数字加法
  assert_eq(azimuth::add(1, 2), 3)
  assert_eq(azimuth::add(5, 10), 15)
  assert_eq(azimuth::add(100, 200), 300)
  assert_eq(azimuth::add(999, 1), 1000)
}

test "add_boundary_values" {
  // 测试边界值加法
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(0, 1), 1)
  assert_eq(azimuth::add(-1, 1), 0)
  assert_eq(azimuth::add(-100, 100), 0)
}

test "multiply_powers_of_two" {
  // 测试2的幂次乘法
  assert_eq(azimuth::multiply(2, 2), 4)
  assert_eq(azimuth::multiply(4, 4), 16)
  assert_eq(azimuth::multiply(8, 8), 64)
  assert_eq(azimuth::multiply(16, 16), 256)
}

test "multiply_with_negative" {
  // 测试负数乘法
  assert_eq(azimuth::multiply(-5, 5), -25)
  assert_eq(azimuth::multiply(5, -5), -25)
  assert_eq(azimuth::multiply(-5, -5), 25)
  assert_eq(azimuth::multiply(-10, 3), -30)
}

test "greet_various_names" {
  // 测试各种名称问候
  assert_eq(azimuth::greet("Alice"), "Hello, Alice!")
  assert_eq(azimuth::greet("Bob"), "Hello, Bob!")
  assert_eq(azimuth::greet("MoonBit"), "Hello, MoonBit!")
  assert_eq(azimuth::greet("123"), "Hello, 123!")
}

test "complex_calculations" {
  // 测试复合计算
  // (a + b) * c
  let a = 3
  let b = 4
  let c = 5
  let result = azimuth::multiply(azimuth::add(a, b), c)
  assert_eq(result, 35)
  
  // a * b + c
  let result2 = azimuth::add(azimuth::multiply(a, b), c)
  assert_eq(result2, 17)
}

test "mathematical_patterns" {
  // 测试数学模式
  // 等差数列求和: 1 + 2 + 3 + 4 + 5 = 15
  let sum1 = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  assert_eq(sum1, 15)
  
  // 平方和: 1² + 2² + 3² = 14
  let sum2 = azimuth::add(azimuth::add(azimuth::multiply(1, 1), azimuth::multiply(2, 2)), azimuth::multiply(3, 3))
  assert_eq(sum2, 14)
}

test "real_world_scenarios" {
  // 测试真实世界场景
  // 购物计算: 商品价格 * 数量 + 税费
  let price = 50
  let quantity = 3
  let tax = 15
  let total = azimuth::add(azimuth::multiply(price, quantity), tax)
  assert_eq(total, 165)
  
  // 面积计算: 长 * 宽
  let length = 10
  let width = 6
  let area = azimuth::multiply(length, width)
  assert_eq(area, 60)
}

test "edge_case_calculations" {
  // 测试边缘情况计算
  // 大数运算
  assert_eq(azimuth::add(1000000, 2000000), 3000000)
  assert_eq(azimuth::multiply(10000, 100), 1000000)
  
  // 零值运算
  assert_eq(azimuth::multiply(12345, 0), 0)
  assert_eq(azimuth::add(0, 98765), 98765)
}

test "string_boundary_cases" {
  // 测试字符串边界情况
  assert_eq(azimuth::greet(""), "Hello, !")
  assert_eq(azimuth::greet(" "), "Hello,  !")
  assert_eq(azimuth::greet("!@#"), "Hello, !@#!")
  assert_eq(azimuth::greet("VeryLongNameForTestingPurposes"), "Hello, VeryLongNameForTestingPurposes!")
}