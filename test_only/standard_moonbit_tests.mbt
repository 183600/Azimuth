// 标准MoonBit测试用例 - 针对azimuth库的核心功能

// 使用从 lib.mbt 导入的函数

// 测试用例1: 基础加法功能
test "basic_addition" {
  @test_only.assert_eq(5, @test_only.add(2, 3))
  @test_only.assert_eq(0, @test_only.add(0, 0))
  @test_only.assert_eq(-5, @test_only.add(-2, -3))
  @test_only.assert_eq(10, @test_only.add(7, 3))
}

// 测试用例2: 基础乘法功能
test "basic_multiplication" {
  @test_only.assert_eq(6, @test_only.multiply(2, 3))
  @test_only.assert_eq(0, @test_only.multiply(0, 5))
  @test_only.assert_eq(15, @test_only.multiply(3, 5))
  @test_only.assert_eq(-6, @test_only.multiply(-2, 3))
}

// 测试用例3: 字符串拼接功能
test "string_greeting" {
  @test_only.assert_eq_string("Hello, World!", @test_only.greet("World"))
  @test_only.assert_eq_string("Hello, MoonBit!", @test_only.greet("MoonBit"))
  @test_only.assert_eq_string("Hello, !", @test_only.greet(""))
}

// 测试用例4: 加法边界值测试
test "addition_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的情况
  @test_only.assert_eq(max_val, @test_only.add(max_val, 1))
  @test_only.assert_eq(max_val, @test_only.add(max_val, 100))
  @test_only.assert_eq(min_val, @test_only.add(min_val, -1))
  @test_only.assert_eq(min_val, @test_only.add(min_val, -100))
}

// 测试用例5: 乘法边界值测试
test "multiplication_boundary_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值乘法
  @test_only.assert_eq(min_val, @test_only.multiply(min_val, -1))
  @test_only.assert_eq(min_val, @test_only.multiply(min_val, 2))
  @test_only.assert_eq(max_val, @test_only.multiply(max_val, 1))
  @test_only.assert_eq(min_val, @test_only.multiply(-1, min_val))
}

// 测试用例6: 单位元处理
test "identity_elements" {
  // 加法单位元（0）在add函数中的特殊处理已在上面测试
  
  // 乘法单位元（1）处理
  @test_only.assert_eq(5, @test_only.multiply(1, 5))
  @test_only.assert_eq(5, @test_only.multiply(5, 1))
  @test_only.assert_eq(-5, @test_only.multiply(1, -5))
  @test_only.assert_eq(-5, @test_only.multiply(-5, 1))
  
  // 乘法负单位元（-1）处理
  @test_only.assert_eq(-5, @test_only.multiply(-1, 5))
  @test_only.assert_eq(-5, @test_only.multiply(5, -1))
  @test_only.assert_eq(5, @test_only.multiply(-1, -5))
}

// 测试用例7: 负数运算测试
test "negative_number_operations" {
  // 负数加法
  @test_only.assert_eq(-10, @test_only.add(-3, -7))
  @test_only.assert_eq(0, @test_only.add(-5, 5))
  @test_only.assert_eq(-2, @test_only.add(-10, 8))
  
  // 负数乘法
  @test_only.assert_eq(15, @test_only.multiply(-3, -5))
  @test_only.assert_eq(-15, @test_only.multiply(3, -5))
  @test_only.assert_eq(0, @test_only.multiply(-5, 0))
}

// 测试用例8: 复合运算测试
test "compound_operations" {
  // 复合加法
  let result1 = @test_only.add(@test_only.add(1, 2), @test_only.add(3, 4))
  @test_only.assert_eq(10, result1)
  
  // 复合乘法
  let result2 = @test_only.multiply(@test_only.multiply(2, 3), @test_only.multiply(4, 5))
  @test_only.assert_eq(120, result2)
  
  // 混合运算
  let result3 = @test_only.add(@test_only.multiply(2, 3), @test_only.multiply(4, 5))
  @test_only.assert_eq(26, result3)
  
  let result4 = @test_only.multiply(@test_only.add(2, 3), @test_only.add(4, 5))
  @test_only.assert_eq(45, result4)
}

// 测试用例9: 大数运算测试
test "large_number_operations" {
  let large_num = 1000000
  let another_large = 2000000
  
  // 大数加法
  @test_only.assert_eq(3000000, @test_only.add(large_num, another_large))
  
  // 大数乘法（但不会溢出）
  @test_only.assert_eq(1000000, @test_only.multiply(large_num, 1))
  @test_only.assert_eq(2000000, @test_only.multiply(large_num, 2))
}

// 测试用例10: 字符串边界情况测试
test "string_edge_cases" {
  // 测试长字符串
  let long_name = "VeryLongNameThatContainsManyCharacters"
  @test_only.assert_eq_string("Hello, VeryLongNameThatContainsManyCharacters!", @test_only.greet(long_name))
  
  // 测试包含特殊字符的字符串
  @test_only.assert_eq_string("Hello, @#$%^&*()!", @test_only.greet("@#$%^&*()"))
  
  // 测试包含空格的字符串
  @test_only.assert_eq_string("Hello, John Doe!", @test_only.greet("John Doe"))
}