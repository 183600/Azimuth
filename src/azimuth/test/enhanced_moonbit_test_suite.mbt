// 增强的 MoonBit 测试套件
// 包含10个精心设计的测试用例，测试 azimuth 包的核心功能

// 测试1: 加法交换律验证
test "addition_commutativity_law" {
  // 验证加法交换律：a + b = b + a
  let a = 12345
  let b = -6789
  
  let result1 = @azimuth.add(a, b)
  let result2 = @azimuth.add(b, a)
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试边界值的交换律
  let max_val = 2147483647
  let min_val = -2147483648
  
  let max_min_result1 = @azimuth.add(max_val, min_val)
  let max_min_result2 = @azimuth.add(min_val, max_val)
  
  @azimuth.assert_eq(max_min_result1, max_min_result2)
}

// 测试2: 乘法结合律验证
test "multiplication_associativity_law" {
  // 验证乘法结合律：(a * b) * c = a * (b * c)
  let a = 12
  let b = 15
  let c = 20
  
  let result1 = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let result2 = @azimuth.multiply(a, @azimuth.multiply(b, c))
  
  @azimuth.assert_eq(result1, result2)
  
  // 测试包含负数的结合律
  let d = -5
  let e = 8
  let f = -3
  
  let result3 = @azimuth.multiply(@azimuth.multiply(d, e), f)
  let result4 = @azimuth.multiply(d, @azimuth.multiply(e, f))
  
  @azimuth.assert_eq(result3, result4)
}

// 测试3: 字符串处理的多语言支持
test "string_processing_multilingual_support" {
  // 测试不同语言的字符串处理
  let english_name = "John"
  let chinese_name = "张三"
  let japanese_name = "田中"
  let korean_name = "김철수"
  let arabic_name = "أحمد"
  let russian_name = "Иван"
  
  @azimuth.assert_eq_string("Hello, John!", @azimuth.greet(english_name))
  @azimuth.assert_eq_string("Hello, 张三!", @azimuth.greet(chinese_name))
  @azimuth.assert_eq_string("Hello, 田中!", @azimuth.greet(japanese_name))
  @azimuth.assert_eq_string("Hello, 김철수!", @azimuth.greet(korean_name))
  @azimuth.assert_eq_string("Hello, أحمد!", @azimuth.greet(arabic_name))
  @azimuth.assert_eq_string("Hello, Иван!", @azimuth.greet(russian_name))
  
  // 测试混合语言字符串
  let mixed_name = "John张三田中"
  @azimuth.assert_eq_string("Hello, John张三田中!", @azimuth.greet(mixed_name))
}

// 测试4: 数学运算的分配律验证
test "mathematical_distributive_law" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 7
  let b = 12
  let c = 8
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的分配律
  let d = -5
  let e = 15
  let f = -3
  
  let left_side2 = @azimuth.multiply(d, @azimuth.add(e, f))
  let right_side2 = @azimuth.add(@azimuth.multiply(d, e), @azimuth.multiply(d, f))
  
  @azimuth.assert_eq(left_side2, right_side2)
}

// 测试5: 边界条件的组合运算
test "boundary_value_combination_operations" {
  // 测试边界值的复杂组合运算
  let max_val = 2147483647
  let min_val = -2147483648
  let zero = 0
  let one = 1
  let neg_one = -1
  
  // 复杂表达式：(max + min) * (1 + (-1)) + (max * 0)
  let expression_result = @azimuth.add(
    @azimuth.multiply(
      @azimuth.add(max_val, min_val),
      @azimuth.add(one, neg_one)
    ),
    @azimuth.multiply(max_val, zero)
  )
  
  @azimuth.assert_eq(0, expression_result)
  
  // 另一个复杂表达式：(min * -1) + (max * 1) + (0 * any)
  let expression_result2 = @azimuth.add(
    @azimuth.add(
      @azimuth.multiply(min_val, neg_one),
      @azimuth.multiply(max_val, one)
    ),
    @azimuth.multiply(zero, 12345)
  )
  
  @azimuth.assert_eq(1, expression_result2)
}

// 测试6: 实际应用场景 - 温度转换
test "real_world_temperature_conversion" {
  // 温度转换场景：摄氏度转华氏度和开尔文
  // 使用简化公式：F = C * 9/5 + 32，K = C + 273
  // 由于只有整数运算，我们使用近似值
  
  let celsius_temp = 25
  
  // 摄氏度转华氏度（简化版：F = C * 2 + 30）
  let fahrenheit_temp = @azimuth.add(
    @azimuth.multiply(celsius_temp, 2),
    30
  )
  
  // 摄氏度转开尔文（简化版：K = C + 273）
  let kelvin_temp = @azimuth.add(celsius_temp, 273)
  
  @azimuth.assert_eq(80, fahrenheit_temp)  // 25 * 2 + 30 = 80
  @azimuth.assert_eq(298, kelvin_temp)    // 25 + 273 = 298
  
  // 测试冰点温度
  let freezing_celsius = 0
  let freezing_fahrenheit = @azimuth.add(
    @azimuth.multiply(freezing_celsius, 2),
    30
  )
  let freezing_kelvin = @azimuth.add(freezing_celsius, 273)
  
  @azimuth.assert_eq(30, freezing_fahrenheit)  // 0 * 2 + 30 = 30
  @azimuth.assert_eq(273, freezing_kelvin)     // 0 + 273 = 273
  
  // 测试沸点温度
  let boiling_celsius = 100
  let boiling_fahrenheit = @azimuth.add(
    @azimuth.multiply(boiling_celsius, 2),
    30
  )
  let boiling_kelvin = @azimuth.add(boiling_celsius, 273)
  
  @azimuth.assert_eq(230, boiling_fahrenheit)  // 100 * 2 + 30 = 230
  @azimuth.assert_eq(373, boiling_kelvin)     // 100 + 273 = 373
}

// 测试7: 实际应用场景 - 简单利息计算
test "real_world_simple_interest_calculation" {
  // 简单利息计算场景
  // 公式：利息 = 本金 × 利率 × 时间
  // 总金额 = 本金 + 利息
  
  let principal = 10000  // 本金
  let rate = 5           // 年利率（百分比）
  let time = 3           // 时间（年）
  
  // 计算利息：10000 × 5% × 3 = 1500
  let interest = @azimuth.multiply(
    @azimuth.multiply(principal, rate),
    time
  ) / 100
  
  // 计算总金额：10000 + 1500 = 11500
  let total_amount = @azimuth.add(principal, interest)
  
  @azimuth.assert_eq(1500, interest)
  @azimuth.assert_eq(11500, total_amount)
  
  // 测试不同期限的利息计算
  let time2 = 5  // 5年
  let interest2 = @azimuth.multiply(
    @azimuth.multiply(principal, rate),
    time2
  ) / 100
  let total_amount2 = @azimuth.add(principal, interest2)
  
  @azimuth.assert_eq(2500, interest2)  // 10000 × 5% × 5 = 2500
  @azimuth.assert_eq(12500, total_amount2)
}

// 测试8: 复杂数学表达式计算
test "complex_mathematical_expression_evaluation" {
  // 复杂数学表达式计算
  // 表达式：(a + b) * (c - d) + e * f - g / h
  
  let a = 15
  let b = 25
  let c = 40
  let d = 10
  let e = 8
  let f = 12
  let g = 100
  let h = 5
  
  // 分步计算
  let sum_ab = @azimuth.add(a, b)  // 15 + 25 = 40
  let diff_cd = @azimuth.add(c, -d)  // 40 - 10 = 30
  let product1 = @azimuth.multiply(sum_ab, diff_cd)  // 40 * 30 = 1200
  let product2 = @azimuth.multiply(e, f)  // 8 * 12 = 96
  let division = g / h  // 100 / 5 = 20
  
  let result = @azimuth.add(
    @azimuth.add(product1, product2),
    -division
  )  // 1200 + 96 - 20 = 1276
  
  @azimuth.assert_eq(1276, result)
  
  // 测试另一个复杂表达式：a^2 + b^2 - 2*a*b (完全平方公式)
  let x = 10
  let y = 6
  
  let x_squared = @azimuth.multiply(x, x)  // 10^2 = 100
  let y_squared = @azimuth.multiply(y, y)  // 6^2 = 36
  let double_product = @azimuth.multiply(
    @azimuth.multiply(2, x),
    y
  )  // 2 * 10 * 6 = 120
  
  let formula_result = @azimuth.add(
    @azimuth.add(x_squared, y_squared),
    -double_product
  )  // 100 + 36 - 120 = 16
  
  @azimuth.assert_eq(16, formula_result)
  
  // 验证这等于 (x - y)^2
  let diff_xy = @azimuth.add(x, -y)  // 10 - 6 = 4
  let diff_squared = @azimuth.multiply(diff_xy, diff_xy)  // 4^2 = 16
  
  @azimuth.assert_eq(formula_result, diff_squared)
}

// 测试9: 错误处理和边界值保护
test "error_handling_and_boundary_value_protection" {
  // 测试溢出保护功能
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法溢出保护
  let overflow_add = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, -1)
  @azimuth.assert_eq(min_val, underflow_add)
  
  // 测试乘法溢出保护
  let overflow_mul = @azimuth.multiply(max_val, 2)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  let underflow_mul = @azimuth.multiply(min_val, 2)
  @azimuth.assert_eq(min_val, underflow_mul)
  
  // 测试特殊值的处理
  let special_case1 = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, special_case1)
  
  let special_case2 = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, special_case2)
  
  // 测试断言函数的使用
  @azimuth.assert_true(@azimuth.add(5, 3) == 8)
  @azimuth.assert_false(@azimuth.add(5, 3) == 9)
  @azimuth.assert_eq(8, @azimuth.add(5, 3))
  @azimuth.assert_eq_string("Hello, Test!", @azimuth.greet("Test"))
}

// 测试10: 函数组合的高级用例
test "advanced_function_composition_scenarios" {
  // 函数组合的高级应用场景
  
  // 场景1：计算二次方程的值 ax^2 + bx + c
  let a = 2
  let b = 3
  let c = 5
  let x = 4
  
  let ax_squared = @azimuth.multiply(a, @azimuth.multiply(x, x))  // 2 * 4^2 = 2 * 16 = 32
  let bx = @azimuth.multiply(b, x)  // 3 * 4 = 12
  let quadratic_result = @azimuth.add(@azimuth.add(ax_squared, bx), c)  // 32 + 12 + 5 = 49
  
  @azimuth.assert_eq(49, quadratic_result)
  
  // 场景2：计算平均速度和距离
  let speed1 = 60  // km/h
  let speed2 = 80  // km/h
  let time1 = 2    // hours
  let time2 = 3    // hours
  
  let distance1 = @azimuth.multiply(speed1, time1)  // 60 * 2 = 120 km
  let distance2 = @azimuth.multiply(speed2, time2)  // 80 * 3 = 240 km
  let total_distance = @azimuth.add(distance1, distance2)  // 120 + 240 = 360 km
  let total_time = @azimuth.add(time1, time2)  // 2 + 3 = 5 hours
  let average_speed = total_distance / total_time  // 360 / 5 = 72 km/h
  
  @azimuth.assert_eq(360, total_distance)
  @azimuth.assert_eq(5, total_time)
  @azimuth.assert_eq(72, average_speed)
  
  // 场景3：计算折扣价格和税费
  let original_price = 1000
  let discount_rate = 15  // 15% 折扣
  let tax_rate = 10       // 10% 税费
  
  // 计算折扣金额
  let discount_amount = @azimuth.multiply(original_price, discount_rate) / 100  // 1000 * 15 / 100 = 150
  // 计算折后价格
  let discounted_price = @azimuth.add(original_price, -discount_amount)  // 1000 - 150 = 850
  // 计算税费
  let tax_amount = @azimuth.multiply(discounted_price, tax_rate) / 100  // 850 * 10 / 100 = 85
  // 计算最终价格
  let final_price = @azimuth.add(discounted_price, tax_amount)  // 850 + 85 = 935
  
  @azimuth.assert_eq(150, discount_amount)
  @azimuth.assert_eq(850, discounted_price)
  @azimuth.assert_eq(85, tax_amount)
  @azimuth.assert_eq(935, final_price)
}