// 增强的MoonBit测试用例 - 针对azimuth库的额外测试

// 测试用例1: 测试add函数的更多边界情况
test "add_edge_cases" {
  // 测试大数相加但不溢出
  @test_only.assert_eq(2000000000, @test_only.add(1000000000, 1000000000))
  
  // 测试一正一负相加
  @test_only.assert_eq(100, @test_only.add(150, -50))
  @test_only.assert_eq(-100, @test_only.add(-150, 50))
  
  // 测试接近边界的值
  @test_only.assert_eq(2147483646, @test_only.add(2147483645, 1))
  @test_only.assert_eq(-2147483647, @test_only.add(-2147483646, -1))
}

// 测试用例2: 测试multiply函数的更多边界情况
test "multiply_edge_cases" {
  // 测试大数相乘但不溢出
  @test_only.assert_eq(1000000, @test_only.multiply(1000, 1000))
  
  // 测试负数乘法
  @test_only.assert_eq(-1000000, @test_only.multiply(1000, -1000))
  @test_only.assert_eq(1000000, @test_only.multiply(-1000, -1000))
  
  // 测试接近边界的值
  @test_only.assert_eq(2147483646, @test_only.multiply(2147483646, 1))
  @test_only.assert_eq(-2147483647, @test_only.multiply(2147483647, -1))
}

// 测试用例3: 测试greet函数的更多边界情况
test "greet_edge_cases" {
  // 测试包含空格的字符串
  @test_only.assert_eq_string("Hello, John Doe!", @test_only.greet("John Doe"))
  
  // 测试包含数字的字符串
  @test_only.assert_eq_string("Hello, 12345!", @test_only.greet("12345"))
  
  // 测试包含特殊字符的字符串
  @test_only.assert_eq_string("Hello, !@#$%^&*()!", @test_only.greet("!@#$%^&*()"))
}

// 测试用例4: 测试复合运算场景
test "compound_calculations" {
  // 测试复合加法
  let result1 = @test_only.add(@test_only.add(10, 20), @test_only.add(30, 40))
  @test_only.assert_eq(100, result1)
  
  // 测试复合乘法
  let result2 = @test_only.multiply(@test_only.multiply(2, 3), @test_only.multiply(4, 5))
  @test_only.assert_eq(120, result2)
  
  // 测试混合运算
  let result3 = @test_only.add(@test_only.multiply(10, 5), @test_only.multiply(3, 4))
  @test_only.assert_eq(62, result3)
}

// 测试用例5: 测试实际应用场景
test "real_world_scenarios" {
  // 计算三角形面积（底×高÷2，这里简化为底×高，然后手动除2）
  let base = 10
  let height = 5
  let area = @test_only.multiply(base, height)
  @test_only.assert_eq(50, area)  // 实际面积应该是25，但这里只是测试乘法
  
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

// 测试用例6: 测试极端值处理
test "extreme_values" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试add函数的极端值
  @test_only.assert_eq(max_val, @test_only.add(max_val, max_val))
  @test_only.assert_eq(min_val, @test_only.add(min_val, min_val))
  @test_only.assert_eq(0, @test_only.add(max_val, min_val))
  
  // 测试multiply函数的极端值
  @test_only.assert_eq(max_val, @test_only.multiply(max_val, 1))
  @test_only.assert_eq(min_val, @test_only.multiply(min_val, 1))
  @test_only.assert_eq(min_val, @test_only.multiply(min_val, -1))
}

// 测试用例7: 测试错误处理路径
test "error_handling_paths" {
  // 测试add函数的溢出处理
  @test_only.assert_eq(2147483647, @test_only.add(2147483640, 10))
  @test_only.assert_eq(-2147483648, @test_only.add(-2147483640, -10))
  
  // 测试multiply函数的溢出处理
  @test_only.assert_eq(2147483647, @test_only.multiply(46341, 46341))
  @test_only.assert_eq(-2147483648, @test_only.multiply(-46341, 46341))
}

// 测试用例8: 测试字符串处理的边界情况
test "string_boundary_cases" {
  // 测试非常长的字符串
  let long_string = "ThisIsAVeryLongStringThatContainsManyCharactersAndWords"
  @test_only.assert_eq_string("Hello, ThisIsAVeryLongStringThatContainsManyCharactersAndWords!", @test_only.greet(long_string))
  
  // 测试包含Unicode字符的字符串
  @test_only.assert_eq_string("Hello, 测试!", @test_only.greet("测试"))
  @test_only.assert_eq_string("Hello, 🌙!", @test_only.greet("🌙"))
  
  // 测试包含换行符的字符串
  @test_only.assert_eq_string("Hello, \n!", @test_only.greet("\n"))
}

// 测试用例9: 测试数学恒等式
test "mathematical_identities" {
  // 测试加法交换律
  let a = 123
  let b = 456
  @test_only.assert_eq(@test_only.add(a, b), @test_only.add(b, a))
  
  // 测试乘法交换律
  @test_only.assert_eq(@test_only.multiply(a, b), @test_only.multiply(b, a))
  
  // 测试乘法分配律
  let c = 789
  @test_only.assert_eq(
    @test_only.multiply(a, @test_only.add(b, c)),
    @test_only.add(@test_only.multiply(a, b), @test_only.multiply(a, c))
  )
}

// 测试用例10: 测试性能相关的场景
test "performance_scenarios" {
  // 测试大量简单运算的组合
  let result = @test_only.add(
    @test_only.multiply(100, 10),
    @test_only.multiply(200, 20)
  )
  @test_only.assert_eq(5000, result)
  
  // 测试嵌套运算
  let nested_result = @test_only.multiply(
    @test_only.add(1, 2),
    @test_only.add(@test_only.add(3, 4), @test_only.add(5, 6))
  )
  @test_only.assert_eq(45, nested_result)
}