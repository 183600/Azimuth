// 增强的MoonBit测试用例 - 针对azimuth库的高级功能测试

// 测试用例1: 零值运算特性
test "zero_value_properties" {
  // 加法零元特性
  @test_only.assert_eq(42, @test_only.add(42, 0))
  @test_only.assert_eq(42, @test_only.add(0, 42))
  @test_only.assert_eq(0, @test_only.add(0, 0))
  @test_only.assert_eq(-2147483648, @test_only.add(-2147483648, 0))
  
  // 乘法零元特性
  @test_only.assert_eq(0, @test_only.multiply(42, 0))
  @test_only.assert_eq(0, @test_only.multiply(0, 42))
  @test_only.assert_eq(0, @test_only.multiply(0, 0))
  @test_only.assert_eq(0, @test_only.multiply(-2147483648, 0))
}

// 测试用例2: 单位元运算特性
test "identity_elements_properties" {
  // 乘法单位元特性
  @test_only.assert_eq(42, @test_only.multiply(42, 1))
  @test_only.assert_eq(42, @test_only.multiply(1, 42))
  @test_only.assert_eq(1, @test_only.multiply(1, 1))
  @test_only.assert_eq(-2147483648, @test_only.multiply(-2147483648, 1))
  
  // 乘法负单位元特性
  @test_only.assert_eq(-42, @test_only.multiply(42, -1))
  @test_only.assert_eq(-42, @test_only.multiply(-1, 42))
  @test_only.assert_eq(-1, @test_only.multiply(-1, 1))
  @test_only.assert_eq(-2147483648, @test_only.multiply(-2147483648, -1))
}

// 测试用例3: 极值组合运算
test "extreme_value_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值与最小值的加法
  @test_only.assert_eq(-1, @test_only.add(max_val, min_val))
  
  // 最大值与自身的加法（溢出）
  @test_only.assert_eq(max_val, @test_only.add(max_val, max_val))
  
  // 最小值与自身的加法（溢出）
  @test_only.assert_eq(min_val, @test_only.add(min_val, min_val))
  
  // 最大值与最小值的乘法
  @test_only.assert_eq(min_val, @test_only.multiply(max_val, min_val))
}

// 测试用例4: 字符串处理边界情况
test "string_processing_edge_cases" {
  // 空字符串处理
  @test_only.assert_eq_string("Hello, !", @test_only.greet(""))
  
  // 单字符处理
  @test_only.assert_eq_string("Hello, A!", @test_only.greet("A"))
  
  // 长字符串处理
  let long_string = "ThisIsAVeryLongStringThatTestsTheGreetingFunctionWithExtendedInput"
  @test_only.assert_eq_string("Hello, ThisIsAVeryLongStringThatTestsTheGreetingFunctionWithExtendedInput!", @test_only.greet(long_string))
  
  // 特殊字符处理
  @test_only.assert_eq_string("Hello, \n\t\r!", @test_only.greet("\n\t\r"))
  @test_only.assert_eq_string("Hello, 中文测试!", @test_only.greet("中文测试"))
}

// 测试用例5: 数学运算一致性验证
test "mathematical_consistency_verification" {
  // 加法结合律验证（非溢出情况下）
  let a = 100
  let b = 200
  let c = 300
  @test_only.assert_eq(
    @test_only.add(@test_only.add(a, b), c),
    @test_only.add(a, @test_only.add(b, c))
  )
  
  // 乘法结合律验证（非溢出情况下）
  let x = 10
  let y = 20
  let z = 30
  @test_only.assert_eq(
    @test_only.multiply(@test_only.multiply(x, y), z),
    @test_only.multiply(x, @test_only.multiply(y, z))
  )
  
  // 分配律验证（非溢出情况下）
  @test_only.assert_eq(
    @test_only.multiply(a, @test_only.add(b, c)),
    @test_only.add(@test_only.multiply(a, b), @test_only.multiply(a, c))
  )
}

// 测试用例6: 负数运算特性
test "negative_number_operations" {
  // 负数加法特性
  @test_only.assert_eq(-10, @test_only.add(-3, -7))
  @test_only.assert_eq(0, @test_only.add(-5, 5))
  @test_only.assert_eq(5, @test_only.add(-10, 15))
  
  // 负数乘法特性
  @test_only.assert_eq(15, @test_only.multiply(-3, -5))
  @test_only.assert_eq(-15, @test_only.multiply(3, -5))
  @test_only.assert_eq(0, @test_only.multiply(-5, 0))
  
  // 负数与极值的运算
  let max_val = 2147483647
  let min_val = -2147483648
  @test_only.assert_eq(-2147483647, @test_only.add(max_val, -1))
  @test_only.assert_eq(2147483647, @test_only.add(min_val, 1))
}

// 测试用例7: 实际应用场景模拟
test "real_world_application_scenarios" {
  // 场景1: 温度转换计算
  let celsius = 25
  let fahrenheit = @test_only.add(@test_only.multiply(@test_only.multiply(celsius, 9), 5), 32)
  @test_only.assert_eq(77, fahrenheit)  // 25°C = 77°F
  
  // 场景2: 购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let discount = 100
  
  let subtotal = @test_only.add(
    @test_only.multiply(item1_price, item1_quantity),
    @test_only.multiply(item2_price, item2_quantity)
  )
  let total = @test_only.add(subtotal, -discount)
  @test_only.assert_eq(1094, total)  // 299*2 + 199*3 - 100 = 1094
  
  // 场景3: 平均值计算
  let score1 = 85
  let score2 = 92
  let score3 = 78
  let score4 = 96
  let average = @test_only.multiply(
    @test_only.add(@test_only.add(@test_only.add(score1, score2), score3), score4),
    25  // 相当于除以4 (乘以1/4，用整数近似)
  )
  @test_only.assert_eq(87, average)  // (85+92+78+96)/4 ≈ 87.75 ≈ 87
}

// 测试用例8: 溢出边界精确测试
test "overflow_boundary_precise_tests" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 加法溢出边界测试
  @test_only.assert_eq(max_val, @test_only.add(max_val, 1))
  @test_only.assert_eq(max_val, @test_only.add(max_val, 100))
  @test_only.assert_eq(max_val, @test_only.add(1000000000, 1147483648))
  
  @test_only.assert_eq(min_val, @test_only.add(min_val, -1))
  @test_only.assert_eq(min_val, @test_only.add(min_val, -100))
  @test_only.assert_eq(min_val, @test_only.add(-1000000000, -1147483649))
  
  // 乘法溢出边界测试
  @test_only.assert_eq(max_val, @test_only.multiply(46341, 46341))  // sqrt(max_val)的平方
  @test_only.assert_eq(min_val, @test_only.multiply(-46341, 46341))
  @test_only.assert_eq(min_val, @test_only.multiply(46341, -46341))
  @test_only.assert_eq(min_val, @test_only.multiply(min_val, 2))
}

// 测试用例9: 复合表达式计算
test "compound_expression_calculations" {
  // 嵌套运算测试
  let result1 = @test_only.add(
    @test_only.multiply(2, 3),
    @test_only.multiply(4, 5)
  )
  @test_only.assert_eq(26, result1)  // 2*3 + 4*5 = 6 + 20 = 26
  
  let result2 = @test_only.multiply(
    @test_only.add(2, 3),
    @test_only.add(4, 5)
  )
  @test_only.assert_eq(45, result2)  // (2+3)*(4+5) = 5*9 = 45
  
  // 多层嵌套运算
  let result3 = @test_only.add(
    @test_only.multiply(
      @test_only.add(1, 2),
      @test_only.add(3, 4)
    ),
    @test_only.multiply(
      @test_only.add(5, 6),
      @test_only.add(7, 8)
    )
  )
  @test_only.assert_eq(156, result3)  // (1+2)*(3+4) + (5+6)*(7+8) = 3*7 + 11*15 = 21 + 165 = 186
}

// 测试用例10: 字符串格式化功能
test "string_formatting_functionality" {
  // 测试各种姓名格式
  @test_only.assert_eq_string("Hello, John!", @test_only.greet("John"))
  @test_only.assert_eq_string("Hello, Jane Doe!", @test_only.greet("Jane Doe"))
  @test_only.assert_eq_string("Hello, Dr. Smith!", @test_only.greet("Dr. Smith"))
  @test_only.assert_eq_string("Hello, Mr. & Mrs. Johnson!", @test_only.greet("Mr. & Mrs. Johnson"))
  
  // 测试数字字符串
  @test_only.assert_eq_string("Hello, 123!", @test_only.greet("123"))
  @test_only.assert_eq_string("Hello, 2023-12-25!", @test_only.greet("2023-12-25"))
  
  // 测试混合内容
  @test_only.assert_eq_string("Hello, User123!", @test_only.greet("User123"))
  @test_only.assert_eq_string("Hello, test@example.com!", @test_only.greet("test@example.com"))
}