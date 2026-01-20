// Azimuth 增强单元测试
// 专注于边界条件和复杂场景的测试

test "add_large_number_commutativity" {
  // 测试大数加法的交换律
  let large_pos = 1000000
  let large_neg = -1000000
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 正数交换律
  @azimuth.assert_eq(@azimuth.add(large_pos, max_val - 1000000), @azimuth.add(max_val - 1000000, large_pos))
  
  // 负数交换律
  @azimuth.assert_eq(@azimuth.add(large_neg, min_val + 1000000), @azimuth.add(min_val + 1000000, large_neg))
  
  // 混合符号交换律
  @azimuth.assert_eq(@azimuth.add(large_pos, large_neg), @azimuth.add(large_neg, large_pos))
}

test "add_associativity_with_overflow" {
  // 测试加法结合律在溢出情况下的行为
  let max_val = 2147483647
  let min_val = -2147483648
  let mid_val = 1000000
  
  // (max + mid) + min vs max + (mid + min)
  let result1 = @azimuth.add(@azimuth.add(max_val, mid_val), min_val)
  let result2 = @azimuth.add(max_val, @azimuth.add(mid_val, min_val))
  @azimuth.assert_eq(result1, result2)
}

test "multiply_power_of_two_edge_cases" {
  // 测试2的幂次方的乘法边界情况
  @azimuth.assert_eq(1024, @azimuth.multiply(32, 32))
  @azimuth.assert_eq(1048576, @azimuth.multiply(1024, 1024))
  @azimuth.assert_eq(-1024, @azimuth.multiply(-32, 32))
  @azimuth.assert_eq(1024, @azimuth.multiply(-32, -32))
  
  // 测试接近溢出的2的幂次方
  let max_val = 2147483647
  @azimuth.assert_eq(max_val, @azimuth.multiply(65536, 32768))
}

test "multiply_distributive_property" {
  // 测试乘法分配律 a * (b + c) = a * b + a * c
  let a = 123
  let b = 456
  let c = 789
  
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "divide_with_ceil_negative_dividend" {
  // 测试负数被除数的向上取整除法
  @azimuth.assert_eq(-1, @azimuth.divide_with_ceil(-1, 3))
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-4, 3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-6, 2))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-7, 2))
  @azimuth.assert_eq(-333334, @azimuth.divide_with_ceil(-1000000, 3))
}

test "divide_with_ceil_negative_divisor" {
  // 测试负数除数的向上取整除法
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(10, -3))
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(11, -3))
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(9, -3))
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))
  @azimuth.assert_eq(2, @azimuth.divide_with_ceil(-5, -3))
}

test "greet_unicode_and_whitespace" {
  // 测试包含Unicode字符和空格的问候
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("   "))
  @azimuth.assert_eq_string("Hello, \t\n!", @azimuth.greet("\t\n"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列计算
  // 计算: 1 + 2*3 + 4*5 + 6*7 + 8*9
  let step1 = @azimuth.multiply(2, 3)
  let step2 = @azimuth.multiply(4, 5)
  let step3 = @azimuth.multiply(6, 7)
  let step4 = @azimuth.multiply(8, 9)
  
  let sum1 = @azimuth.add(1, step1)
  let sum2 = @azimuth.add(sum1, step2)
  let sum3 = @azimuth.add(sum2, step3)
  let result = @azimuth.add(sum3, step4)
  
  @azimuth.assert_eq(1 + 6 + 20 + 42 + 72, result)
}

test "extreme_value_combinations" {
  // 测试极值组合的运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值加法组合
  @azimuth.assert_eq(0, @azimuth.add(max_val, min_val + 1))
  @azimuth.assert_eq(-2, @azimuth.add(min_val, min_val + 2))
  
  // 极值乘法组合
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}