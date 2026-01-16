// Azimuth 项目额外测试用例
// 使用标准 MoonBit 测试语法，包含高级功能和边界测试

test "add_fibonacci_sequence" {
  // 测试斐波那契数列中的加法
  assert_eq(azimuth::add(5, 8), 13)
  assert_eq(azimuth::add(13, 21), 34)
  assert_eq(azimuth::add(34, 55), 89)
}

test "multiply_powers_of_two" {
  // 测试2的幂次乘法
  assert_eq(azimuth::multiply(2, 4), 8)
  assert_eq(azimuth::multiply(8, 16), 128)
  assert_eq(azimuth::multiply(32, 64), 2048)
}

test "greet_unicode_characters" {
  // 测试Unicode字符问候
  assert_eq(azimuth::greet("世界"), "Hello, 世界!")
  assert_eq(azimuth::greet("🚀"), "Hello, 🚀!")
  assert_eq(azimuth::greet("Café"), "Hello, Café!")
}

test "mathematical_distributive_property" {
  // 测试乘法对加法的分配律
  let a = 6
  let b = 4
  let c = 3
  
  // a * (b + c) = a * b + a * c
  let left_side = azimuth::multiply(a, azimuth::add(b, c))
  let right_side = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(a, c))
  assert_eq(left_side, right_side)
}

test "prime_number_operations" {
  // 测试质数运算
  assert_eq(azimuth::add(17, 19), 36)
  assert_eq(azimuth::multiply(7, 11), 77)
  assert_eq(azimuth::add(23, 29), 52)
}

test "sequential_operations" {
  // 测试连续运算
  let result1 = azimuth::add(1, 2)
  let result2 = azimuth::add(result1, 3)
  let result3 = azimuth::add(result2, 4)
  assert_eq(result3, 10)
  
  let mult_result1 = azimuth::multiply(2, 3)
  let mult_result2 = azimuth::multiply(mult_result1, 4)
  assert_eq(mult_result2, 24)
}

test "greet_with_whitespace" {
  // 测试包含空白字符的问候
  assert_eq(azimuth::greet("  leading"), "Hello,   leading!")
  assert_eq(azimuth::greet("trailing  "), "Hello, trailing  !")
  assert_eq(azimuth::greet("  both  "), "Hello,   both  !")
}

test "mathematical_associative_property" {
  // 测试加法和乘法的结合律
  let a = 5
  let b = 7
  let c = 9
  
  // (a + b) + c = a + (b + c)
  let add_left = azimuth::add(azimuth::add(a, b), c)
  let add_right = azimuth::add(a, azimuth::add(b, c))
  assert_eq(add_left, add_right)
  
  // (a * b) * c = a * (b * c)
  let mult_left = azimuth::multiply(azimuth::multiply(a, b), c)
  let mult_right = azimuth::multiply(a, azimuth::multiply(b, c))
  assert_eq(mult_left, mult_right)
}

test "extreme_value_operations" {
  // 测试极值运算
  let very_large = 1000000
  let very_small = -1000000
  
  assert_eq(azimuth::add(very_large, very_small), 0)
  assert_eq(azimuth::multiply(very_large, 0), 0)
  assert_eq(azimuth::multiply(very_small, 1), very_small)
}