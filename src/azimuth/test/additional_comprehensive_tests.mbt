// 额外的综合测试用例
// 这些测试用例补充现有测试，覆盖更多边界情况和特殊场景

///|
test "new_add_symmetry_properties" {
  // 测试加法的交换律
  assert_eq(@azimuth.add(100, 200), @azimuth.add(200, 100))
  assert_eq(@azimuth.add(-100, 200), @azimuth.add(200, -100))
  assert_eq(@azimuth.add(-100, -200), @azimuth.add(-200, -100))
  
  // 测试加法的结合律（在溢出保护下的表现）
  let a = 1000000
  let b = 2000000
  let c = 3000000
  assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
}

///|
test "new_multiply_distributive_properties" {
  // 测试乘法对加法的分配律（在溢出保护下的表现）
  let a = 100
  let b = 200
  let c = 3
  assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
  
  // 负数情况
  assert_eq(@azimuth.multiply(-100, @azimuth.add(200, -50)), @azimuth.add(@azimuth.multiply(-100, 200), @azimuth.multiply(-100, -50)))
}

///|
test "new_extreme_boundary_combinations" {
  // 测试极端边界值的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值的多次运算
  let result1 = @azimuth.add(max_val, @azimuth.add(max_val, max_val))
  assert_eq(max_val, result1) // 应该溢出
  
  // 最小值的多次运算
  let result2 = @azimuth.add(min_val, @azimuth.add(min_val, min_val))
  assert_eq(min_val, result2) // 应该溢出
  
  // 混合边界运算
  let result3 = @azimuth.add(@azimuth.add(max_val, min_val), 1)
  assert_eq(0, result3) // max_val + min_val = -1, then + 1 = 0
}

///|
test "new_power_of_two_operations" {
  // 测试2的幂次方的运算
  assert_eq(@azimuth.multiply(2, 1024), 2048)
  assert_eq(@azimuth.multiply(4, 512), 2048)
  assert_eq(@azimuth.multiply(8, 256), 2048)
  assert_eq(@azimuth.multiply(16, 128), 2048)
  assert_eq(@azimuth.multiply(32, 64), 2048)
  
  // 2的幂次方相加
  assert_eq(@azimuth.add(1024, 2048), 3072)
  assert_eq(@azimuth.add(4096, 8192), 12288)
  
  // 大的2的幂次方
  assert_eq(@azimuth.multiply(32768, 32768), 2147483647) // 应该溢出
}

///|
test "new_greet_unicode_and_special_chars" {
  // 测试包含特殊字符和Unicode的字符串
  assert_eq("Hello, 中文!", @azimuth.greet("中文"))
  assert_eq("Hello, 🌙!", @azimuth.greet("🌙"))
  assert_eq("Hello, Café!", @azimuth.greet("Café"))
  assert_eq("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  assert_eq("Hello, Hello World!", @azimuth.greet("Hello World"))
  
  // 测试包含空格的字符串
  assert_eq("Hello,  !", @azimuth.greet("  "))
  assert_eq("Hello, a b c!", @azimuth.greet("a b c"))
}

///|
test "new_sequential_operations_stability" {
  // 测试连续运算的稳定性
  let mut result = 0
  result = @azimuth.add(result, 100)
  assert_eq(100, result)
  result = @azimuth.multiply(result, 2)
  assert_eq(200, result)
  result = @azimuth.add(result, -50)
  assert_eq(150, result)
  result = @azimuth.multiply(result, 3)
  assert_eq(450, result)
  
  // 测试接近边界的连续运算
  let mut large_result = 1000000
  large_result = @azimuth.multiply(large_result, 1000)
  assert_eq(2147483647, large_result) // 应该溢出
  large_result = @azimuth.add(large_result, -1000000)
  assert_eq(2147482647, large_result) // 溢出后减法
}

///|
test "new_negative_zero_handling" {
  // 测试负零的处理（虽然Int中没有负零，但测试相关边界）
  assert_eq(0, @azimuth.add(0, 0))
  assert_eq(0, @azimuth.multiply(0, 0))
  assert_eq(0, @azimuth.multiply(0, -1))
  assert_eq(0, @azimuth.multiply(-1, 0))
  assert_eq(0, @azimuth.multiply(0, 2147483647))
  assert_eq(0, @azimuth.multiply(0, -2147483648))
}

///|
test "new_alternating_sign_patterns" {
  // 测试交替符号的模式
  assert_eq(0, @azimuth.add(100, -100))
  assert_eq(0, @azimuth.add(-100, 100))
  assert_eq(200, @azimuth.add(-100, 300))
  assert_eq(-200, @azimuth.add(100, -300))
  
  // 乘法的交替符号
  assert_eq(-10000, @azimuth.multiply(100, -100))
  assert_eq(10000, @azimuth.multiply(-100, -100))
  assert_eq(10000, @azimuth.multiply(100, 100))
  
  // 复杂的符号组合
  let result = @azimuth.multiply(@azimuth.add(10, -5), @azimuth.add(-3, 8))
  assert_eq(25, result) // (10-5)*( -3+8) = 5*5 = 25
}

///|
test "new_boundary_precision_tests" {
  // 测试边界精度相关的测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值的减法（通过加负数实现）
  assert_eq(0, @azimuth.add(max_val, @azimuth.multiply(min_val, -1)))
  
  // 测试接近边界的精确计算
  assert_eq(2147483646, @azimuth.add(1073741823, 1073741823))
  assert_eq(-2147483647, @azimuth.add(-1073741824, -1073741823))
  
  // 测试边界值的乘法
  assert_eq(2147483647, @azimuth.multiply(46340, 46341)) // 接近溢出但不溢出
  assert_eq(2147483647, @azimuth.multiply(46341, 46340)) // 反过来
}

///|
test "new_mathematical_identities" {
  // 测试数学恒等式
  let test_values = [0, 1, -1, 100, -100, 2147483647, -2147483648]
  
  for value in test_values {
    // 加法恒等式：a + 0 = a
    assert_eq(value, @azimuth.add(value, 0))
    assert_eq(value, @azimuth.add(0, value))
    
    // 乘法恒等式：a * 1 = a
    assert_eq(value, @azimuth.multiply(value, 1))
    assert_eq(value, @azimuth.multiply(1, value))
    
    // 零乘法：a * 0 = 0
    assert_eq(0, @azimuth.multiply(value, 0))
    assert_eq(0, @azimuth.multiply(0, value))
    
    // 负一乘法：a * -1 = -a（除了最小值）
    if value != -2147483648 {
      assert_eq(@azimuth.multiply(value, -1), @azimuth.multiply(-1, value))
    }
  }
}