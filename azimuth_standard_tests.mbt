// Azimuth 项目标准测试用例
// 使用标准 MoonBit 测试语法

test "add_commutative_property" {
  // 测试加法交换律
  assert_eq(azimuth::add(12, 8), azimuth::add(8, 12))
  assert_eq(azimuth::add(-5, 15), azimuth::add(15, -5))
  assert_eq(azimuth::add(0, 100), azimuth::add(100, 0))
}

test "add_associative_property" {
  // 测试加法结合律
  assert_eq(azimuth::add(azimuth::add(2, 3), 4), azimuth::add(2, azimuth::add(3, 4)))
  assert_eq(azimuth::add(azimuth::add(-1, 5), 3), azimuth::add(-1, azimuth::add(5, 3)))
}

test "multiply_distributive_property" {
  // 测试乘法分配律
  assert_eq(azimuth::multiply(5, azimuth::add(2, 3)), azimuth::add(azimuth::multiply(5, 2), azimuth::multiply(5, 3)))
  assert_eq(azimuth::multiply(4, azimuth::add(1, 6)), azimuth::add(azimuth::multiply(4, 1), azimuth::multiply(4, 6)))
}

test "multiply_commutative_property" {
  // 测试乘法交换律
  assert_eq(azimuth::multiply(7, 9), azimuth::multiply(9, 7))
  assert_eq(azimuth::multiply(-3, 8), azimuth::multiply(8, -3))
  assert_eq(azimuth::multiply(0, 42), azimuth::multiply(42, 0))
}

test "greet_empty_and_whitespace" {
  // 测试空字符串和空白字符串的问候
  assert_eq(azimuth::greet(""), "Hello, !")
  assert_eq(azimuth::greet(" "), "Hello,  !")
  assert_eq(azimuth::greet("\t"), "Hello, \t!")
}

test "greet_unicode_emoji" {
  // 测试 Unicode 和 Emoji 字符的问候
  assert_eq(azimuth::greet("🚀"), "Hello, 🚀!")
  assert_eq(azimuth::greet("🌙"), "Hello, 🌙!")
  assert_eq(azimuth::greet("⭐"), "Hello, ⭐!")
}

test "mathematical_sequences" {
  // 测试数学序列计算
  
  // 计算前 5 个自然数的和: 1+2+3+4+5 = 15
  let sum_first_5 = azimuth::add(azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4), 5)
  assert_eq(15, sum_first_5)
  
  // 计算前 4 个偶数的和: 2+4+6+8 = 20
  let sum_even_4 = azimuth::add(azimuth::add(azimuth::add(2, 4), 6), 8)
  assert_eq(20, sum_even_4)
}

test "practical_calculations" {
  // 测试实际计算场景
  
  // 计算矩形面积: 长 × 宽
  let length = 12
  let width = 8
  let area = azimuth::multiply(length, width)
  assert_eq(96, area)
  
  // 计算总价: 单价 × 数量
  let unit_price = 25
  let quantity = 4
  let total_price = azimuth::multiply(unit_price, quantity)
  assert_eq(100, total_price)
}

test "edge_case_calculations" {
  // 测试边界情况计算
  
  // 大数相加
  let large_num1 = 1000000
  let large_num2 = 2000000
  assert_eq(3000000, azimuth::add(large_num1, large_num2))
  
  // 负数乘以正数
  assert_eq(-50, azimuth::multiply(-10, 5))
  assert_eq(-36, azimuth::multiply(9, -4))
}

test "complex_expressions" {
  // 测试复杂表达式计算
  
  // 计算 (a + b) * c - d
  let a = 5
  let b = 3
  let c = 4
  let d = 10
  
  let result = azimuth::add(azimuth::multiply(azimuth::add(a, b), c), -d)
  assert_eq(22, result)
  
  // 计算 a * b + c * d
  let result2 = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(c, d))
  assert_eq(55, result2)
}