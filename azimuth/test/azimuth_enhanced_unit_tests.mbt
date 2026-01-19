// Azimuth 增强单元测试
// 使用标准的 MoonBit 测试语法，覆盖各种边界情况

test "add_zero_handling" {
  // 测试加法中的零值处理
  @azimuth.assert_eq(42, @azimuth.add(0, 42))
  @azimuth.assert_eq(42, @azimuth.add(42, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-17, @azimuth.add(0, -17))
  @azimuth.assert_eq(-17, @azimuth.add(-17, 0))
}

test "add_negative_numbers" {
  // 测试负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  @azimuth.assert_eq(0, @azimuth.add(-25, 25))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
}

test "add_large_numbers" {
  // 测试大数加法
  @azimuth.assert_eq(1000000, @azimuth.add(500000, 500000))
  @azimuth.assert_eq(-1000000, @azimuth.add(-500000, -500000))
  @azimuth.assert_eq(2000000000, @azimuth.add(1000000000, 1000000000))
}

test "multiply_by_zero_and_one" {
  // 测试乘以0和1的情况
  @azimuth.assert_eq(0, @azimuth.multiply(12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-12345, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 12345))
  @azimuth.assert_eq(12345, @azimuth.multiply(12345, 1))
  @azimuth.assert_eq(-12345, @azimuth.multiply(-12345, 1))
  @azimuth.assert_eq(12345, @azimuth.multiply(1, 12345))
}

test "multiply_negative_numbers" {
  // 测试负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
}

test "multiply_min_value_edge_cases" {
  // 测试最小值乘法的边界情况
  let min_val = -2147483648
  
  // 最小值乘以1应该等于最小值
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  
  // 最小值乘以-1应该等于最小值（因为2147483648会溢出）
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 最小值乘以0应该等于0
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "greet_with_unicode" {
  // 测试包含Unicode字符的问候
  @azimuth.assert_eq_string("Hello, 中文!", @azimuth.greet("中文"))
  @azimuth.assert_eq_string("Hello, 🌙!", @azimuth.greet("🌙"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
}

test "greet_with_spaces" {
  // 测试包含空格的问候
  @azimuth.assert_eq_string("Hello, John Doe!", @azimuth.greet("John Doe"))
  @azimuth.assert_eq_string("Hello,  !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, a b c !", @azimuth.greet("a b c "))
}

test "complex_calculations" {
  // 测试复合计算
  // 计算 (10 + 20) * (30 - 15)
  let sum1 = @azimuth.add(10, 20)
  let diff = @azimuth.add(30, -15)
  let result = @azimuth.multiply(sum1, diff)
  @azimuth.assert_eq(450, result)
  
  // 计算 (-5 + 15) * (-3 + 7)
  let sum2 = @azimuth.add(-5, 15)
  let sum3 = @azimuth.add(-3, 7)
  let result2 = @azimuth.multiply(sum2, sum3)
  @azimuth.assert_eq(40, result2)
}