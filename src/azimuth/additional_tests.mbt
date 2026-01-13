// 测试用例1: 测试加法运算的更多边界情况
test "add_edge_cases" {
  // 测试连续相加
  let result1 = @azimuth.add(@azimuth.add(1, 2), 3)
  @azimuth.assert_eq(6, result1)
  
  // 测试极大值相加
  let max_val = 2147483647
  let result2 = @azimuth.add(max_val, -2147483647)
  @azimuth.assert_eq(0, result2)
  
  // 测试极小值相加
  let min_val = -2147483648
  let result3 = @azimuth.add(min_val, 2147483647)
  @azimuth.assert_eq(-1, result3)
}

// 测试用例2: 测试乘法运算的更多边界情况
test "multiply_edge_cases" {
  // 测试连续相乘
  let result1 = @azimuth.multiply(@azimuth.multiply(2, 3), 4)
  @azimuth.assert_eq(24, result1)
  
  // 测试负数相乘
  let result2 = @azimuth.multiply(-2, -3)
  @azimuth.assert_eq(6, result2)
  
  // 测试正负数相乘
  let result3 = @azimuth.multiply(-2, 3)
  @azimuth.assert_eq(-6, result3)
  
  // 测试与边界值相乘
  let max_val = 2147483647
  let result4 = @azimuth.multiply(max_val, -1)
  @azimuth.assert_eq(-2147483647, result4)
}

// 测试用例3: 测试字符串拼接的更多情况
test "greet_complex_strings" {
  // 测试包含空格的字符串
  let result1 = @azimuth.greet("John Doe")
  @azimuth.assert_eq_string("Hello, John Doe!", result1)
  
  // 测试包含数字的字符串
  let result2 = @azimuth.greet("User123")
  @azimuth.assert_eq_string("Hello, User123!", result2)
  
  // 测试包含特殊符号的字符串
  let result3 = @azimuth.greet("!@#$%^&*()")
  @azimuth.assert_eq_string("Hello, !@#$%^&*()!", result3)
  
  // 测试长字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result4 = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result4)
}

// 测试用例4: 测试复杂计算场景
test "complex_calculations" {
  // 测试计算 (a + b) * c
  let a = 10
  let b = 20
  let c = 3
  let sum = @azimuth.add(a, b)
  let result = @azimuth.multiply(sum, c)
  @azimuth.assert_eq(90, result)  // (10 + 20) * 3 = 90
  
  // 测试计算 a * b + c * d
  let a2 = 5
  let b2 = 6
  let c2 = 7
  let d2 = 8
  let product1 = @azimuth.multiply(a2, b2)
  let product2 = @azimuth.multiply(c2, d2)
  let result2 = @azimuth.add(product1, product2)
  @azimuth.assert_eq(86, result2)  // 5*6 + 7*8 = 30 + 56 = 86
}

// 测试用例5: 测试边界值和溢出处理
test "overflow_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法正溢出
  let result1 = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, result1)
  
  // 测试加法负溢出
  let result2 = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试乘法正溢出
  let result3 = @azimuth.multiply(max_val, max_val)
  @azimuth.assert_eq(max_val, result3)
  
  // 测试乘法负溢出
  let result4 = @azimuth.multiply(min_val, min_val)
  @azimuth.assert_eq(max_val, result4)  // 负负得正，但会溢出
}

// 测试用例6: 测试数学函数的组合使用
test "function_combinations" {
  // 测试 (a + b) * (c + d)
  let a = 3
  let b = 4
  let c = 5
  let d = 6
  let sum1 = @azimuth.add(a, b)
  let sum2 = @azimuth.add(c, d)
  let result = @azimuth.multiply(sum1, sum2)
  @azimuth.assert_eq(77, result)  // (3 + 4) * (5 + 6) = 7 * 11 = 77
  
  // 测试 a * b + c
  let a2 = 10
  let b2 = 20
  let c2 = 30
  let product = @azimuth.multiply(a2, b2)
  let result2 = @azimuth.add(product, c2)
  @azimuth.assert_eq(230, result2)  // 10 * 20 + 30 = 200 + 30 = 230
}

// 测试用例7: 测试字符串处理的不同场景
test "string_processing_scenarios" {
  // 测试包含中文字符的字符串
  let result1 = @azimuth.greet("中文测试")
  @azimuth.assert_eq_string("Hello, 中文测试!", result1)
  
  // 测试包含日文字符的字符串
  let result2 = @azimuth.greet("こんにちは")
  @azimuth.assert_eq_string("Hello, こんにちは!", result2)
  
  // 测试包含混合字符的字符串
  let result3 = @azimuth.greet("Test测试123")
  @azimuth.assert_eq_string("Hello, Test测试123!", result3)
  
  // 测试包含换行符的字符串（如果支持）
  let result4 = @azimuth.greet("Line1\nLine2")
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", result4)
}

// 测试用例8: 测试断言函数的各种情况
test "assert_functions_various_cases" {
  // 测试 assert_true
  @azimuth.assert_true(1 == 1)
  @azimuth.assert_true(0 < 1)
  @azimuth.assert_true(-1 < 0)
  
  // 测试 assert_false
  @azimuth.assert_false(1 == 0)
  @azimuth.assert_false(0 > 1)
  @azimuth.assert_false(-1 > 0)
  
  // 测试 assert_eq
  @azimuth.assert_eq(0, 0)
  @azimuth.assert_eq(-1, -1)
  @azimuth.assert_eq(2147483647, 2147483647)
  @azimuth.assert_eq(-2147483648, -2147483648)
  
  // 测试 assert_eq_string
  @azimuth.assert_eq_string("", "")
  @azimuth.assert_eq_string("a", "a")
  @azimuth.assert_eq_string("Hello", "Hello")
  @azimuth.assert_eq_string("Hello, World!", "Hello, World!")
}

// 测试用例9: 测试实际应用场景
test "real_world_scenarios" {
  // 场景1: 计算购物车总价（商品价格 * 数量，然后求和）
  let item1_price = 100
  let item1_quantity = 2
  let item2_price = 50
  let item2_quantity = 3
  
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let cart_total = @azimuth.add(item1_total, item2_total)
  
  @azimuth.assert_eq(350, cart_total)  // 100*2 + 50*3 = 200 + 150 = 350
  
  // 场景2: 计算温度转换（摄氏度到华氏度：F = C * 9/5 + 32）
  // 由于我们只有整数运算，我们简化为 F = C * 2 + 30
  let celsius = 20
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  @azimuth.assert_eq(70, fahrenheit)  // 20 * 2 + 30 = 70
}

// 测试用例10: 测试极端边界情况
test "extreme_boundary_cases" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与最小值的运算
  let result1 = @azimuth.add(max_val, min_val)
  @azimuth.assert_eq(-1, result1)  // 2147483647 + (-2147483648) = -1
  
  let result2 = @azimuth.multiply(max_val, min_val)
  @azimuth.assert_eq(min_val, result2)  // 2147483647 * (-2147483648) = 溢出为最小值
  
  // 测试最大值与自身的运算
  let result3 = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, result3)  // 溢出为最大值
  
  let result4 = @azimuth.multiply(max_val, 1)
  @azimuth.assert_eq(max_val, result4)  // 2147483647 * 1 = 2147483647
  
  // 测试最小值与自身的运算
  let result5 = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, result5)  // 溢出为最小值
  
  let result6 = @azimuth.multiply(min_val, 1)
  @azimuth.assert_eq(min_val, result6)  // -2147483648 * 1 = -2147483648
}