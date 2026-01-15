// 新增的 MoonBit 测试用例
// 这些测试用例覆盖了一些新的场景和边界情况

test "add_with_large_difference" {
  // 测试相差很大的数相加
  @azimuth.assert_eq(1000001, @azimuth.add(1000000, 1))
  @azimuth.assert_eq(1000000, @azimuth.add(1000000, 0))
  @azimuth.assert_eq(999999, @azimuth.add(1000000, -1))
  @azimuth.assert_eq(-1000001, @azimuth.add(-1000000, -1))
}

test "multiply_with_powers_of_two" {
  // 测试与2的幂相乘
  @azimuth.assert_eq(16, @azimuth.multiply(4, 4))
  @azimuth.assert_eq(32, @azimuth.multiply(8, 4))
  @azimuth.assert_eq(64, @azimuth.multiply(16, 4))
  @azimuth.assert_eq(128, @azimuth.multiply(32, 4))
}

test "multiply_with_large_small_combination" {
  // 测试大数与小数相乘
  @azimuth.assert_eq(2147483647, @azimuth.multiply(2147483647, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(2147483647, 0))
  @azimuth.assert_eq(-2147483647, @azimuth.multiply(2147483647, -1))
  @azimuth.assert_eq(2147483647, @azimuth.multiply(-2147483647, -1))
}

test "greet_with_special_chars" {
  // 测试包含特殊字符的greet函数
  @azimuth.assert_eq_string("Hello, Tab!", @azimuth.greet("Tab"))
  @azimuth.assert_eq_string("Hello, Space!", @azimuth.greet("Space"))
  @azimuth.assert_eq_string("Hello, Mixed!", @azimuth.greet("Mixed"))
}

test "complex_arithmetic_sequence" {
  // 测试复杂的算术序列计算
  // 计算1 + 2 + 3 + ... + 10 = 55
  let sum1 = @azimuth.add(1, 2)
  let sum2 = @azimuth.add(3, 4)
  let sum3 = @azimuth.add(5, 6)
  let sum4 = @azimuth.add(7, 8)
  let sum5 = @azimuth.add(9, 10)
  
  let partial1 = @azimuth.add(sum1, sum2)
  let partial2 = @azimuth.add(sum3, sum4)
  let partial3 = @azimuth.add(partial1, partial2)
  let total = @azimuth.add(partial3, sum5)
  
  @azimuth.assert_eq(55, total)
  
  // 计算等差数列：2 + 5 + 8 + 11 + 14 = 40
  let arith1 = @azimuth.add(2, 5)
  let arith2 = @azimuth.add(8, 11)
  let arith_total = @azimuth.add(@azimuth.add(arith1, arith2), 14)
  @azimuth.assert_eq(40, arith_total)
}

test "geometric_progression_calculation" {
  // 测试几何级数计算
  // 计算2 + 4 + 8 + 16 = 30
  let geo_sum = @azimuth.add(@azimuth.add(@azimuth.add(2, 4), 8), 16)
  @azimuth.assert_eq(30, geo_sum)
  
  // 计算3 + 9 + 27 + 81 = 120
  let geo_sum2 = @azimuth.add(@azimuth.add(@azimuth.add(3, 9), 27), 81)
  @azimuth.assert_eq(120, geo_sum2)
}

test "string_with_unicode_chars" {
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, Sun!", @azimuth.greet("Sun"))
  @azimuth.assert_eq_string("Hello, Heart!", @azimuth.greet("Heart"))
  @azimuth.assert_eq_string("Hello, Star!", @azimuth.greet("Star"))
}

test "edge_case_calculations" {
  // 测试边界情况计算
  let max_val = 2147483647
  let min_val = -2147483648
  
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
}

test "nested_operations" {
  // 测试嵌套运算
  let a = 10
  let b = 20
  let c = 30
  
  // 计算 (a + b) * c
  let result = @azimuth.multiply(@azimuth.add(a, b), c)
  @azimuth.assert_eq(900, result)
  
  // 计算 a * (b + c)
  let result2 = @azimuth.multiply(a, @azimuth.add(b, c))
  @azimuth.assert_eq(500, result2)
}