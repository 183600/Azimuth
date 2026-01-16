// Azimuth 项目额外测试用例
// 补充测试覆盖，使用标准 MoonBit 测试语法

test "zero_value_operations" {
  // 测试零值在各种运算中的行为
  
  // 加法零值测试
  assert_eq(azimuth::add(0, 0), 0)
  assert_eq(azimuth::add(42, 0), 42)
  assert_eq(azimuth::add(0, 42), 42)
  assert_eq(azimuth::add(-17, 0), -17)
  
  // 乘法零值测试
  assert_eq(azimuth::multiply(0, 0), 0)
  assert_eq(azimuth::multiply(42, 0), 0)
  assert_eq(azimuth::multiply(0, 42), 0)
  assert_eq(azimuth::multiply(-17, 0), 0)
}

test "negative_number_operations" {
  // 测试负数运算的各种情况
  
  // 负数加法
  assert_eq(azimuth::add(-5, -3), -8)
  assert_eq(azimuth::add(-10, 3), -7)
  assert_eq(azimuth::add(7, -15), -8)
  
  // 负数乘法
  assert_eq(azimuth::multiply(-4, -6), 24)
  assert_eq(azimuth::multiply(-7, 3), -21)
  assert_eq(azimuth::multiply(5, -9), -45)
  assert_eq(azimuth::multiply(-1, 1), -1)
}

test "greet_special_characters" {
  // 测试特殊字符和字符串的问候功能
  
  // 测试包含空格的名字
  assert_eq(azimuth::greet("John Doe"), "Hello, John Doe!")
  assert_eq(azimuth::greet("Alice Bob"), "Hello, Alice Bob!")
  
  // 测试特殊字符
  assert_eq(azimuth::greet("John@Doe"), "Hello, John@Doe!")
  assert_eq(azimuth::greet("Alice-Bob"), "Hello, Alice-Bob!")
  assert_eq(azimuth::greet("User_123"), "Hello, User_123!")
  
  // 测试数字名字
  assert_eq(azimuth::greet("123"), "Hello, 123!")
}

test "mathematical_power_patterns" {
  // 测试数值运算的幂次模式
  
  // 2的幂次方通过重复乘法计算
  let two_to_1 = azimuth::multiply(2, 1)
  let two_to_2 = azimuth::multiply(2, 2)
  let two_to_3 = azimuth::multiply(two_to_2, 2)
  let two_to_4 = azimuth::multiply(two_to_3, 2)
  
  assert_eq(two_to_1, 2)
  assert_eq(two_to_2, 4)
  assert_eq(two_to_3, 8)
  assert_eq(two_to_4, 16)
  
  // 3的幂次方通过重复乘法计算
  let three_to_1 = azimuth::multiply(3, 1)
  let three_to_2 = azimuth::multiply(3, 3)
  let three_to_3 = azimuth::multiply(three_to_2, 3)
  
  assert_eq(three_to_1, 3)
  assert_eq(three_to_2, 9)
  assert_eq(three_to_3, 27)
}

test "large_number_operations" {
  // 测试大数运算
  
  // 大数相加
  assert_eq(azimuth::add(999999, 1), 1000000)
  assert_eq(azimuth::add(500000, 500000), 1000000)
  assert_eq(azimuth::add(1000000, 1000000), 2000000)
  
  // 大数相乘
  assert_eq(azimuth::multiply(1000, 1000), 1000000)
  assert_eq(azimuth::multiply(2000, 500), 1000000)
  assert_eq(azimuth::multiply(10000, 100), 1000000)
}

test "complex_mathematical_expressions" {
  // 测试复杂数学表达式
  
  // 计算 (a * b) + (c * d) = e
  let a = 10
  let b = 5
  let c = 3
  let d = 15
  let expected = azimuth::add(azimuth::multiply(a, b), azimuth::multiply(c, d))
  assert_eq(expected, 95)
  
  // 计算 (a + b) * (c + d) = e
  let result = azimuth::multiply(azimuth::add(a, b), azimuth::add(c, d))
  assert_eq(result, 260)
  
  // 计算 a * b * c
  let result2 = azimuth::multiply(azimuth::multiply(a, b), c)
  assert_eq(result2, 150)
}

test "greet_multilingual_names" {
  // 测试多语言名字的问候功能
  
  // 中文测试
  assert_eq(azimuth::greet("小明"), "Hello, 小明!")
  assert_eq(azimuth::greet("张三"), "Hello, 张三!")
  
  // 日文测试
  assert_eq(azimuth::greet("さくら"), "Hello, さくら!")
  assert_eq(azimuth::greet("たろう"), "Hello, たろう!")
  
  // 韩文测试
  assert_eq(azimuth::greet("민준"), "Hello, 민준!")
  assert_eq(azimuth::greet("서연"), "Hello, 서연!")
}

test "mathematical_properties_verification" {
  // 测试数学性质的进一步验证
  
  // 验证加法单位元
  let x = 42
  assert_eq(azimuth::add(x, 0), x)
  assert_eq(azimuth::add(0, x), x)
  
  // 验证乘法单位元
  assert_eq(azimuth::multiply(x, 1), x)
  assert_eq(azimuth::multiply(1, x), x)
  
  // 验证乘法零元
  assert_eq(azimuth::multiply(x, 0), 0)
  assert_eq(azimuth::multiply(0, x), 0)
}

test "sequential_operations" {
  // 测试连续运算
  
  // 连续加法
  let result1 = azimuth::add(azimuth::add(azimuth::add(1, 2), 3), 4)
  assert_eq(result1, 10)
  
  // 连续乘法
  let result2 = azimuth::multiply(azimuth::multiply(azimuth::multiply(2, 3), 4), 5)
  assert_eq(result2, 120)
  
  // 混合连续运算
  let result3 = azimuth::add(azimuth::multiply(2, 3), azimuth::multiply(4, 5))
  assert_eq(result3, 26)
}