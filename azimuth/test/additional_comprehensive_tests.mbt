test "comprehensive_add_test" {
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  @azimuth.assert_eq(-1, @azimuth.add(-2, 1))
  @azimuth.assert_eq(100, @azimuth.add(50, 50))
}

test "comprehensive_multiply_test" {
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(-6, @azimuth.multiply(-2, 3))
  @azimuth.assert_eq(25, @azimuth.multiply(5, 5))
}

test "comprehensive_greet_test" {
  @azimuth.assert_eq_string("Hello, MoonBit!", @azimuth.greet("MoonBit"))
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
}

test "negative_addition_comprehensive" {
  // 测试两个负数相加
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-100, @azimuth.add(-50, -50))
  
  // 测试负数与正数相加
  @azimuth.assert_eq(-2, @azimuth.add(-10, 8))
  @azimuth.assert_eq(5, @azimuth.add(-3, 8))
  
  // 测试对称性
  @azimuth.assert_eq(@azimuth.add(-5, 10), @azimuth.add(10, -5))
}

test "negative_multiplication_comprehensive" {
  // 测试两个负数相乘得正数
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))
  @azimuth.assert_eq(100, @azimuth.multiply(-10, -10))
  
  // 测试负数与正数相乘得负数
  @azimuth.assert_eq(-15, @azimuth.multiply(-3, 5))
  @azimuth.assert_eq(-100, @azimuth.multiply(10, -10))
  
  // 测试乘法分配律 (a * (b + c) = a * b + a * c)
  let a = 3
  let b = 4
  let c = 5
  let left = @azimuth.multiply(a, @azimuth.add(b, c))
  let right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left, right)
}

test "string_greet_edge_cases" {
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatCouldPotentiallyCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  @azimuth.assert_eq_string("Hello, \"John\"!", @azimuth.greet("\"John\""))
  
  // 测试包含反斜杠的字符串
  @azimuth.assert_eq_string("Hello, C:\\Users\\Test!", @azimuth.greet("C:\\Users\\Test"))
}

test "mathematical_function_combination" {
  // 测试多项式计算：2x² + 3x + 1，当 x = 5 时
  let x = 5
  let x_squared = @azimuth.multiply(x, x)  // x²
  let term1 = @azimuth.multiply(2, x_squared)  // 2x²
  let term2 = @azimuth.multiply(3, x)  // 3x
  let result = @azimuth.add(@azimuth.add(term1, term2), 1)  // 2x² + 3x + 1
  @azimuth.assert_eq(66, result)  // 2*25 + 3*5 + 1 = 50 + 15 + 1 = 66
  
  // 测试几何计算：计算梯形面积 (上底 + 下底) * 高 / 2
  let top_base = 5
  let bottom_base = 10
  let height = 4
  let area = @azimuth.multiply(@azimuth.add(top_base, bottom_base), height) / 2
  @azimuth.assert_eq(30, area)  // (5 + 10) * 4 / 2 = 15 * 4 / 2 = 30
}

test "temperature_conversion_scenario" {
  // 摄氏度转华氏度：F = C * 9/5 + 32
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 9) / 5, 32)
  @azimuth.assert_eq(77, fahrenheit)  // 25 * 9/5 + 32 = 45 + 32 = 77
  
  // 华氏度转摄氏度：C = (F - 32) * 5/9
  let fahrenheit2 = 77
  let celsius2 = @azimuth.multiply(fahrenheit2 - 32, 5) / 9
  @azimuth.assert_eq(25, celsius2)  // (77 - 32) * 5/9 = 45 * 5/9 = 25
}

test "data_type_boundary_conditions" {
  // 测试连续运算的边界情况
  let max_val = 2147483647
  
  // 测试达到最大值后继续运算
  let result1 = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, result1)  // 应该饱和到最大值
  
  // 测试从最大值减法
  let result2 = max_val - 100
  @azimuth.assert_eq(2147483547, result2)
  
  // 测试最小值的特殊情况
  let min_val = -2147483648
  let result3 = @azimuth.add(min_val, 1)
  @azimuth.assert_eq(-2147483647, result3)
}

// 新增的10个测试用例
test "sequential_addition" {
  // 测试连续相加的情况
  let result1 = @azimuth.add(@azimuth.add(1, 2), @azimuth.add(3, 4))
  @azimuth.assert_eq(10, result1)
  
  let result2 = @azimuth.add(@azimuth.add(100, 200), @azimuth.add(300, 400))
  @azimuth.assert_eq(1000, result2)
  
  // 测试包含最小值的连续相加
  let min_val = -2147483648
  let result3 = @azimuth.add(@azimuth.add(min_val, 1), @azimuth.add(1, 1))
  @azimuth.assert_eq(-2147483645, result3)
}

test "sequential_multiplication" {
  // 测试连续相乘的情况
  let result1 = @azimuth.multiply(@azimuth.multiply(2, 3), @azimuth.multiply(4, 5))
  @azimuth.assert_eq(120, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(10, 20), @azimuth.multiply(30, 40))
  @azimuth.assert_eq(240000, result2)
  
  // 测试包含负数的连续相乘
  let result3 = @azimuth.multiply(@azimuth.multiply(-2, 3), @azimuth.multiply(4, -5))
  @azimuth.assert_eq(120, result3)
}

test "extreme_value_operations" {
  // 测试极值运算的组合
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 极值相加
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  
  // 极值与接近极值的数相加
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 100))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -100))
  
  // 极值相乘
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

test "string_edge_cases" {
  // 测试字符串连接的边界情况
  // 测试非常长的字符串
  let long_name = "ThisIsAVeryLongNameThatMightCauseIssuesInSomeSystemsButShouldWorkFineInMoonBit"
  let result = @azimuth.greet(long_name)
  @azimuth.assert_eq_string("Hello, " + long_name + "!", result)
  
  // 测试包含引号的字符串
  let quoted_name = "\"John Doe\""
  let result2 = @azimuth.greet(quoted_name)
  @azimuth.assert_eq_string("Hello, \"John Doe\"!", result2)
  
  // 测试包含换行符的字符串
  let newline_name = "Line1\nLine2"
  let result3 = @azimuth.greet(newline_name)
  @azimuth.assert_eq_string("Hello, Line1\nLine2!", result3)
}

test "addition_associativity" {
  // 测试加法结合律 (a + b) + c = a + (b + c)
  let a = 100
  let b = 200
  let c = 300
  
  let result1 = @azimuth.add(@azimuth.add(a, b), c)
  let result2 = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let d = -150
  let e = 250
  let f = -100
  
  let result3 = @azimuth.add(@azimuth.add(d, e), f)
  let result4 = @azimuth.add(d, @azimuth.add(e, f))
  @azimuth.assert_eq(result3, result4)
}

test "multiplication_commutativity" {
  // 测试乘法交换律 a * b = b * a
  let a = 12
  let b = 15
  
  let result1 = @azimuth.multiply(a, b)
  let result2 = @azimuth.multiply(b, a)
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的情况
  let c = -20
  let d = 30
  
  let result3 = @azimuth.multiply(c, d)
  let result4 = @azimuth.multiply(d, c)
  @azimuth.assert_eq(result3, result4)
  
  // 测试包含0的情况
  let e = 0
  let f = 100
  
  let result5 = @azimuth.multiply(e, f)
  let result6 = @azimuth.multiply(f, e)
  @azimuth.assert_eq(result5, result6)
}

test "complex_string_processing" {
  // 测试复杂字符串处理
  // 测试包含多种特殊字符的字符串
  let complex_name = "John@Doe#123$%^&*()"
  let result = @azimuth.greet(complex_name)
  @azimuth.assert_eq_string("Hello, John@Doe#123$%^&*()!", result)
  
  // 测试包含中文字符的字符串
  let chinese_name = "张三"
  let result2 = @azimuth.greet(chinese_name)
  @azimuth.assert_eq_string("Hello, 张三!", result2)
  
  // 测试混合语言字符串
  let mixed_name = "John张三123"
  let result3 = @azimuth.greet(mixed_name)
  @azimuth.assert_eq_string("Hello, John张三123!", result3)
}

test "shopping_cart_calculation" {
  // 测试实际应用场景：购物车计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  
  // 计算每种商品的总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)
  let total2 = @azimuth.multiply(item2_price, item2_quantity)
  let total3 = @azimuth.multiply(item3_price, item3_quantity)
  
  // 计算购物车总价
  let cart_total = @azimuth.add(@azimuth.add(total1, total2), total3)
  
  // 验证总价：299*2 + 199*3 + 99*1 = 598 + 597 + 99 = 1294
  @azimuth.assert_eq(1294, cart_total)
}

test "temperature_conversion" {
  // 测试实际应用场景：温度转换（简化版）
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 由于我们只有整数运算，我们使用近似值：F = C * 2 + 30
  
  let celsius_temp = 25
  let fahrenheit_temp = @azimuth.add(@azimuth.multiply(celsius_temp, 2), 30)
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
}

test "error_handling_scenarios" {
  // 测试错误处理场景
  // 测试断言函数的错误情况（这些测试会故意失败，但在实际使用中被注释掉）
  // @azimuth.assert_true(false)  // 这会导致运行时错误
  // @azimuth.assert_false(true)  // 这会导致运行时错误
  // @azimuth.assert_eq(1, 2)     // 这会导致运行时错误
  // @azimuth.assert_eq_string("hello", "world")  // 这会导致运行时错误
  
  // 测试边界情况的正确处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 确保溢出被正确处理
  @azimuth.assert_true(@azimuth.add(max_val, 1) == max_val)
  @azimuth.assert_true(@azimuth.add(min_val, -1) == min_val)
  @azimuth.assert_true(@azimuth.multiply(max_val, 2) == max_val)
  @azimuth.assert_true(@azimuth.multiply(min_val, -1) == min_val)
  
  // 测试正常情况
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_true(@azimuth.multiply(4, 6) == 24)
  @azimuth.assert_true(@azimuth.greet("Test") == "Hello, Test!")
}