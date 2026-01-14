test "add_complex_boundary_tests" {
  // 测试复杂边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值加最小值
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 测试多个边界值相加
  @azimuth.assert_eq(max_val - 1, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(min_val + 1, @azimuth.add(min_val, 1))
  
  // 测试连续相加的边界情况
  let result = @azimuth.add(@azimuth.add(max_val - 2, 1), 1)
  @azimuth.assert_eq(max_val, result)
}

test "multiply_negative_edge_cases" {
  // 测试负数乘法的边界情况
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试负数乘以负数
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -5))
  
  // 测试最小值乘以负数
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -2))
  
  // 测试最大值乘以负数
  @azimuth.assert_eq(-max_val, @azimuth.multiply(max_val, -1))
}

test "greet_unicode_and_special_chars" {
  // 测试Unicode字符和特殊字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌟!", @azimuth.greet("🌟"))
  @azimuth.assert_eq_string("Hello, \n\t!", @azimuth.greet("\n\t"))
  @azimuth.assert_eq_string("Hello, \"quoted\"!", @azimuth.greet("\"quoted\""))
}

test "arithmetic_property_combinations" {
  // 测试数学属性的组合
  let a = 12
  let b = 8
  let c = 5
  
  // 测试分配律：a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 测试结合律：(a + b) + c = a + (b + c)
  let associative_left = @azimuth.add(@azimuth.add(a, b), c)
  let associative_right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(associative_left, associative_right)
}

test "extreme_value_combinations" {
  // 测试极值组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与自身相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试最小值与自身相加
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
  
  // 测试最大值与最小值相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, min_val))
  
  // 测试最小值与自身相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, min_val))
}

test "string_edge_cases" {
  // 测试字符串边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试长字符串
  let long_name = "This is a very long name that contains many words and characters"
  let expected = "Hello, " + long_name + "!"
  @azimuth.assert_eq_string(expected, @azimuth.greet(long_name))
  
  // 测试包含数字的字符串
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
}

test "mixed_arithmetic_operations" {
  // 测试混合算术运算
  let values = [1, 2, 3, 4, 5]
  
  // 计算总和
  let sum1 = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(values[0], values[1]), values[2]), values[3]), values[4])
  @azimuth.assert_eq(15, sum1)
  
  // 计算乘积
  let product1 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(values[0], values[1]), values[2]), values[3])
  @azimuth.assert_eq(24, product1)
  
  // 混合运算：(1 + 2) * (3 + 4)
  let mixed = @azimuth.multiply(@azimuth.add(values[0], values[1]), @azimuth.add(values[2], values[3]))
  @azimuth.assert_eq(21, mixed)
}

test "negative_zero_and_identity_tests" {
  // 测试负零和单位元性质
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(5, @azimuth.multiply(5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(1, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  
  // 测试负数与单位元
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(-5, @azimuth.multiply(-5, 1))
  @azimuth.assert_eq(5, @azimuth.multiply(-5, -1))
}