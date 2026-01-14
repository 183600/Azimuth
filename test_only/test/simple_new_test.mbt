// 简单的新测试用例
test "simple_addition_test" {
  @test_only.assert_eq(5, @test_only.add(2, 3))
}

test "simple_multiplication_test" {
  @test_only.assert_eq(6, @test_only.multiply(2, 3))
}

test "simple_greet_test" {
  @test_only.assert_eq_string("Hello, World!", @test_only.greet("World"))
}

// 测试用例4: 加法边界值测试
test "addition_boundary_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试接近边界的情况
  @test_only.assert_eq(max_val, @test_only.add(max_val, 1))
  @test_only.assert_eq(min_val, @test_only.add(min_val, -1))
}

// 测试用例5: 乘法边界值测试
test "multiplication_boundary_test" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试边界值乘法
  @test_only.assert_eq(min_val, @test_only.multiply(min_val, -1))
  @test_only.assert_eq(max_val, @test_only.multiply(max_val, 1))
}

// 测试用例6: 负数运算测试
test "negative_operations_test" {
  // 负数加法
  @test_only.assert_eq(-10, @test_only.add(-3, -7))
  @test_only.assert_eq(0, @test_only.add(-5, 5))
  
  // 负数乘法
  @test_only.assert_eq(15, @test_only.multiply(-3, -5))
  @test_only.assert_eq(-15, @test_only.multiply(3, -5))
}

// 测试用例7: 复合运算测试
test "compound_operations_test" {
  // 复合加法
  let result1 = @test_only.add(@test_only.add(1, 2), @test_only.add(3, 4))
  @test_only.assert_eq(10, result1)
  
  // 复合乘法
  let result2 = @test_only.multiply(@test_only.multiply(2, 3), @test_only.multiply(4, 5))
  @test_only.assert_eq(120, result2)
}

// 测试用例8: 字符串边界情况测试
test "string_edge_cases_test" {
  // 测试包含空格的字符串
  @test_only.assert_eq_string("Hello, John Doe!", @test_only.greet("John Doe"))
  
  // 测试包含特殊字符的字符串
  @test_only.assert_eq_string("Hello, @#$%^&*()!", @test_only.greet("@#$%^&*()"))
}

// 测试用例9: 数学恒等式测试
test "mathematical_identities_test" {
  // 测试加法交换律
  let a = 123
  let b = 456
  @test_only.assert_eq(@test_only.add(a, b), @test_only.add(b, a))
  
  // 测试乘法交换律
  @test_only.assert_eq(@test_only.multiply(a, b), @test_only.multiply(b, a))
}

// 测试用例10: 实际应用场景测试
test "real_world_scenarios_test" {
  // 计算购物总价（单价×数量）
  let price = 25
  let quantity = 4
  let total = @test_only.multiply(price, quantity)
  @test_only.assert_eq(100, total)
  
  // 计算总分数（各科分数相加）
  let math_score = 90
  let english_score = 85
  let science_score = 95
  let total_score = @test_only.add(@test_only.add(math_score, english_score), science_score)
  @test_only.assert_eq(270, total_score)
}