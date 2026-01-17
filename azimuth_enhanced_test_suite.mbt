// Azimuth 项目增强测试套件
// 使用标准 MoonBit 测试语法，专注于边界条件和错误处理

test "add_large_numbers" {
  // 测试大数加法
  assert_eq(azimuth::add(1000000, 2000000), 3000000)
  assert_eq(azimuth::add(999999, 1), 1000000)
  assert_eq(azimuth::add(-1000000, 1000000), 0)
}

test "add_extreme_values" {
  // 测试极值加法
  assert_eq(azimuth::add(2147483647, 0), 2147483647)  // 接近Int32最大值
  assert_eq(azimuth::add(-2147483648, 0), -2147483648) // 接近Int32最小值
}

test "multiply_large_numbers" {
  // 测试大数乘法
  assert_eq(azimuth::multiply(1000, 1000), 1000000)
  assert_eq(azimuth::multiply(123, 456), 56088)
  assert_eq(azimuth::multiply(-1000, 1000), -1000000)
}

test "multiply_negative_combinations" {
  // 测试负数乘法的各种组合
  assert_eq(azimuth::multiply(-10, -20), 200)    // 负负得正
  assert_eq(azimuth::multiply(15, -3), -45)      // 正负得负
  assert_eq(azimuth::multiply(-7, 8), -56)       // 负正得负
}

test "greet_edge_cases" {
  // 测试问候功能的边界情况
  assert_eq(azimuth::greet(""), "Hello, !")      // 空字符串
  assert_eq(azimuth::greet(" "), "Hello,  !")    // 空格
  assert_eq(azimuth::greet("123"), "Hello, 123!") // 数字字符串
}

test "greet_unicode_characters" {
  // 测试Unicode字符处理
  assert_eq(azimuth::greet("张三"), "Hello, 张三!")
  assert_eq(azimuth::greet("José"), "Hello, José!")
  assert_eq(azimuth::greet("🌟"), "Hello, 🌟!")
}

test "mathematical_properties" {
  // 测试数学性质
  // 交换律
  let a = 17
  let b = 23
  assert_eq(azimuth::add(a, b), azimuth::add(b, a))
  assert_eq(azimuth::multiply(a, b), azimuth::multiply(b, a))
  
  // 结合律
  let c = 31
  assert_eq(azimuth::add(azimuth::add(a, b), c), azimuth::add(a, azimuth::add(b, c)))
  assert_eq(azimuth::multiply(azimuth::multiply(a, b), c), azimuth::multiply(a, azimuth::multiply(b, c)))
}

test "zero_and_identity_properties" {
  // 测试零和单位元的性质
  let x = 42
  
  // 加法零元
  assert_eq(azimuth::add(x, 0), x)
  assert_eq(azimuth::add(0, x), x)
  
  // 乘法单位元
  assert_eq(azimuth::multiply(x, 1), x)
  assert_eq(azimuth::multiply(1, x), x)
  
  // 乘法零元
  assert_eq(azimuth::multiply(x, 0), 0)
  assert_eq(azimuth::multiply(0, x), 0)
}

test "mixed_operations_stress" {
  // 混合运算压力测试
  let a = 100
  let b = -50
  let c = 25
  
  // 复合运算
  let result1 = azimuth::add(a, azimuth::multiply(b, c))  // 100 + (-50 * 25) = 100 - 1250 = -1150
  assert_eq(result1, -1150)
  
  let result2 = azimuth::multiply(azimuth::add(a, b), c)  // (100 + (-50)) * 25 = 50 * 25 = 1250
  assert_eq(result2, 1250)
}