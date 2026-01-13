// 标准 MoonBit 测试用例集
// 测试 azimuth 包中的核心功能
// 包含10个测试用例，覆盖各种功能和边界情况

// 测试用例1: 基础加法功能
test "basic_addition_functionality" {
  @azimuth.assert_eq(5, @azimuth.add(2, 3))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-2, @azimuth.add(-5, 3))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

// 测试用例2: 基础乘法功能
test "basic_multiplication_functionality" {
  @azimuth.assert_eq(6, @azimuth.multiply(2, 3))
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-10, @azimuth.multiply(2, -5))
  @azimuth.assert_eq(1, @azimuth.multiply(1, 1))
}

// 测试用例3: 字符串拼接功能
test "string_greeting_functionality" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, World!", @azimuth.greet("World"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

// 测试用例4: 加法边界值测试
test "addition_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正数溢出
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 测试负数溢出
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, min_val))
}

// 测试用例5: 乘法边界值测试
test "multiplication_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试乘法溢出
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  
  // 测试最小值的特殊情况
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试用例6: 负数运算测试
test "negative_number_operations" {
  // 负数加法
  @azimuth.assert_eq(-8, @azimuth.add(-3, -5))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(2, @azimuth.add(-10, 12))
  
  // 负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
}

// 测试用例7: 零值处理测试
test "zero_value_handling" {
  // 加法中的零值处理
  @azimuth.assert_eq(5, @azimuth.add(0, 5))
  @azimuth.assert_eq(-3, @azimuth.add(-3, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 乘法中的零值处理
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(-3, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
}

// 测试用例8: 单位元处理测试
test "identity_elements_handling" {
  // 加法单位元（0）处理已在上面测试
  
  // 乘法单位元（1）处理
  @azimuth.assert_eq(7, @azimuth.multiply(1, 7))
  @azimuth.assert_eq(7, @azimuth.multiply(7, 1))
  @azimuth.assert_eq(-7, @azimuth.multiply(1, -7))
  @azimuth.assert_eq(-7, @azimuth.multiply(-7, 1))
  
  // 乘法负单位元（-1）处理
  @azimuth.assert_eq(-7, @azimuth.multiply(-1, 7))
  @azimuth.assert_eq(-7, @azimuth.multiply(7, -1))
  @azimuth.assert_eq(7, @azimuth.multiply(-1, -7))
}

// 测试用例9: 特殊字符和Unicode测试
test "special_characters_and_unicode" {
  // 特殊字符
  @azimuth.assert_eq_string("Hello, @#$%!", @azimuth.greet("@#$%"))
  @azimuth.assert_eq_string("Hello, 12345!", @azimuth.greet("12345"))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  
  // Unicode字符
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
  @azimuth.assert_eq_string("Hello, Ñoño!", @azimuth.greet("Ñoño"))
}

// 测试用例10: 复合运算测试
test "compound_operations" {
  // 复合加法
  let sum1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, sum1)
  
  // 复合乘法
  let product1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, product1)
  
  // 混合运算
  let mixed1 = @azimuth.add(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(26, mixed1)
  
  let mixed2 = @azimuth.multiply(@azimuth.add(2, 3), @azimuth.add(4, 5))
  @azimuth.assert_eq(45, mixed2)
}