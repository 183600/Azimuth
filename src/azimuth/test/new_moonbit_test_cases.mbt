// 新增的 MoonBit 测试用例
// 这些测试用例专注于验证项目的核心功能和边界情况

// 测试负数运算
test "negative_number_operations" {
  // 测试负数加法
  @azimuth.assert_eq(-30, @azimuth.add(-10, -20))
  
  // 测试负数乘法
  @azimuth.assert_eq(30, @azimuth.multiply(-5, -6))
  
  // 测试正负数混合运算
  @azimuth.assert_eq(10, @azimuth.add(-15, 25))
  
  @azimuth.assert_eq(-28, @azimuth.multiply(-4, 7))
}

// 测试大数运算
test "large_number_operations" {
  // 测试接近边界值的大数相加
  let large_num1 = 1000000000
  let large_num2 = 1000000000
  @azimuth.assert_eq(2000000000, @azimuth.add(large_num1, large_num2))
  
  // 测试大数相乘
  let large_num3 = 10000
  let large_num4 = 10000
  @azimuth.assert_eq(100000000, @azimuth.multiply(large_num3, large_num4))
  
  // 测试大负数运算
  let large_neg1 = -1000000000
  let large_neg2 = -1000000000
  @azimuth.assert_eq(-2000000000, @azimuth.add(large_neg1, large_neg2))
}

// 测试数学性质验证
test "mathematical_properties_verification" {
  let a = 15
  let b = 25
  let c = 10
  
  // 验证加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 验证乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 验证加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 验证分配律: a * (b + c) = a * b + a * c
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

// 测试实际应用场景
test "real_world_scenarios" {
  // 场景1: 计算购物车总价
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let shipping_fee = 15
  
  let item1_total = @azimuth.multiply(item1_price, item1_quantity)
  let item2_total = @azimuth.multiply(item2_price, item2_quantity)
  let subtotal = @azimuth.add(item1_total, item2_total)
  let total = @azimuth.add(subtotal, shipping_fee)
  
  @azimuth.assert_eq(1294, total)
  
  // 场景2: 计算平均分
  let score1 = 85
  let score2 = 90
  let score3 = 78
  let score4 = 92
  let score5 = 88
  
  let sum_scores = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(score1, score2), score3), score4), score5)
  let average_score = sum_scores / 5
  
  @azimuth.assert_eq(86, average_score)
  
  // 场景3: 温度转换 (摄氏度转华氏度: F = C * 2 + 30)
  let celsius = 25
  let fahrenheit = @azimuth.add(@azimuth.multiply(celsius, 2), 30)
  
  @azimuth.assert_eq(80, fahrenheit)
}

// 测试错误处理
test "error_handling" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试正溢出处理
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  
  // 测试负溢出处理
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  
  // 测试乘法溢出处理
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  
  // 测试最小值乘法特殊处理
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
}

// 测试字符串边界情况
test "string_boundary_cases" {
  // 测试空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 测试单字符字符串
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 测试包含特殊字符的字符串
  @azimuth.assert_eq_string("Hello, test@123#!", @azimuth.greet("test@123#"))
  
  // 测试包含Unicode字符的字符串
  @azimuth.assert_eq_string("Hello, 测试123!", @azimuth.greet("测试123"))
  
  // 测试包含空格的字符串
  @azimuth.assert_eq_string("Hello,   John  Doe  !", @azimuth.greet("  John  Doe  "))
}

// 测试复合运算
test "compound_operations" {
  // 测试复杂的数学表达式: (a + b) * (c - d) + e * f
  let a = 10
  let b = 5
  let c = 20
  let d = 8
  let e = 3
  let f = 7
  
  let sum_ab = @azimuth.add(a, b)  // 15
  let diff_cd = @azimuth.add(c, -d)  // 12
  let product_ef = @azimuth.multiply(e, f)  // 21
  let product_ab_cd = @azimuth.multiply(sum_ab, diff_cd)  // 180
  let final_result = @azimuth.add(product_ab_cd, product_ef)  // 201
  
  @azimuth.assert_eq(201, final_result)
  
  // 测试嵌套函数调用: greet(add(add(a, b), add(c, d)))
  let nested_sum = @azimuth.add(@azimuth.add(a, b), @azimuth.add(c, d))  // 43
  @azimuth.assert_eq_string("Hello, 43!", @azimuth.greet("43"))
}

// 测试性能相关计算
test "performance_related_calculations" {
  // 测试大量重复计算
  let base = 100
  let iterations = 100
  
  // 模拟循环累加: base + base*2 + base*3 + ... + base*iterations
  // 使用公式: base * (1 + 2 + ... + iterations) = base * iterations * (iterations + 1) / 2
  let arithmetic_series = @azimuth.multiply(base, @azimuth.multiply(iterations, @azimuth.add(iterations, 1)) / 2)
  
  @azimuth.assert_eq(505000, arithmetic_series)
  
  // 测试指数增长计算: base * 2^10
  let exponential_growth = @azimuth.multiply(base, @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), @azimuth.multiply(2, 2)), @azimuth.multiply(2, 2)))
  
  @azimuth.assert_eq(102400, exponential_growth)
}

// 测试特殊值处理
test "special_values_handling" {
  // 测试零值运算
  @azimuth.assert_eq(123, @azimuth.add(0, 123))
  @azimuth.assert_eq(-456, @azimuth.add(0, -456))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 789))
  @azimuth.assert_eq(0, @azimuth.multiply(0, -101112))
  
  // 测试一值运算
  @azimuth.assert_eq(234, @azimuth.multiply(1, 234))
  @azimuth.assert_eq(-567, @azimuth.multiply(-567, 1))
  
  // 测试负一值运算
  @azimuth.assert_eq(-345, @azimuth.multiply(-1, 345))
  @azimuth.assert_eq(678, @azimuth.multiply(-678, -1))
  
  // 测试相同值运算
  @azimuth.assert_eq(1998, @azimuth.add(999, 999))
  @azimuth.assert_eq(121, @azimuth.multiply(11, 11))
}

// 测试边界条件组合
test "boundary_condition_combinations" {
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试最大值与各种值的组合
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 0))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(2147483646, @azimuth.add(max_val, -1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(max_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, -1))
  
  // 测试最小值与各种值的组合
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, 0))
  @azimuth.assert_eq(-2147483647, @azimuth.add(min_val, 1))
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
  @azimuth.assert_eq(0, @azimuth.multiply(min_val, 0))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 测试极值之间的运算
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(max_val, min_val))
}