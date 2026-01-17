// Azimuth 项目新增测试用例
// 使用标准 MoonBit 测试语法

test "add_zero_identity" {
  // 测试加法零元素性质
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, -17), -17)
  assert_eq(azimuth::add(-100, 0), -100)
}

test "add_negative_numbers" {
  // 测试负数加法
  assert_eq(azimuth::add(-5, -3), -8)
  assert_eq(azimuth::add(-10, 7), -3)
  assert_eq(azimuth::add(15, -20), -5)
  assert_eq(azimuth::add(-8, 0), -8)
}

test "multiply_zero_property" {
  // 测试乘法零性质
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(15, 0), 0)
  assert_eq(azimuth::multiply(0, -7), 0)
  assert_eq(azimuth::multiply(0, 1000000), 0)
}

test "multiply_identity" {
  // 测试乘法单位元性质
  assert_eq(azimuth::multiply(1, 1), 1)
  assert_eq(azimuth::multiply(42, 1), 42)
  assert_eq(azimuth::multiply(1, -17), -17)
  assert_eq(azimuth::multiply(-100, 1), -100)
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  assert_eq(azimuth::multiply(-5, -3), 15)
  assert_eq(azimuth::multiply(-4, 7), -28)
  assert_eq(azimuth::multiply(9, -2), -18)
  assert_eq(azimuth::multiply(-1, -1), 1)
}

test "greet_special_characters" {
  // 测试特殊字符的问候
  assert_eq(azimuth::greet("@user"), "Hello, @user!")
  assert_eq(azimuth::greet("#hashtag"), "Hello, #hashtag!")
  assert_eq(azimuth::greet("user123"), "Hello, user123!")
  assert_eq(azimuth::greet("test-case"), "Hello, test-case!")
}

test "greet_multilingual" {
  // 测试多语言字符的问候
  assert_eq(azimuth::greet("世界"), "Hello, 世界!")
  assert_eq(azimuth::greet("мир"), "Hello, мир!")
  assert_eq(azimuth::greet("monde"), "Hello, monde!")
  assert_eq(azimuth::greet("世界🌍"), "Hello, 世界🌍!")
}

test "arithmetic_combinations" {
  // 测试算术组合运算
  
  // 计算 (a * b) + (c * d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  
  let product1 = azimuth::multiply(a, b)
  let product2 = azimuth::multiply(c, d)
  let sum = azimuth::add(product1, product2)
  
  assert_eq(sum, 42)
  
  // 计算 (a + b) * (c + d)
  let sum1 = azimuth::add(a, b)
  let sum2 = azimuth::add(c, d)
  let product = azimuth::multiply(sum1, sum2)
  
  assert_eq(product, 77)
}

test "large_number_operations" {
  // 测试大数运算
  
  let large1 = 10000
  let large2 = 20000
  let large3 = 500
  
  assert_eq(azimuth::add(large1, large2), 30000)
  assert_eq(azimuth::multiply(large3, 100), 50000)
  assert_eq(azimuth::multiply(large1, large3), 5000000)
}

test "sequential_operations" {
  // 测试连续运算
  
  // 计算 1 + 2 + 3 + 4 + 5
  let result1 = azimuth::add(1, 2)
  let result2 = azimuth::add(result1, 3)
  let result3 = azimuth::add(result2, 4)
  let result4 = azimuth::add(result3, 5)
  
  assert_eq(result4, 15)
  
  // 计算 2 * 3 * 4
  let product1 = azimuth::multiply(2, 3)
  let product2 = azimuth::multiply(product1, 4)
  
  assert_eq(product2, 24)
}