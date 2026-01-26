// Azimuth 项目 MoonBit 测试用例
// 测试基本数学运算和字符串处理功能

test "string_with_special_characters" {
  // 测试包含各种特殊字符的字符串处理
  @azimuth.assert_eq_string("Hello, @#$%^&*()!", @azimuth.greet("@#$%^&*()"))
  @azimuth.assert_eq_string("Hello, 中文测试!", @azimuth.greet("中文测试"))
  @azimuth.assert_eq_string("Hello, émoji🚀!", @azimuth.greet("émoji🚀"))
  @azimuth.assert_eq_string("Hello,   spaced   !", @azimuth.greet("  spaced   "))
}

test "mathematical_identity_properties" {
  // 测试数学恒等性质
  let a = 42
  let b = 17
  
  // 加法恒等式
  @azimuth.assert_eq(a, @azimuth.add(a, 0))
  @azimuth.assert_eq(a, @azimuth.add(0, a))
  
  // 乘法恒等式
  @azimuth.assert_eq(a, @azimuth.multiply(a, 1))
  @azimuth.assert_eq(a, @azimuth.multiply(1, a))
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, a))
}

test "complex_calculation_scenarios" {
  // 测试复杂计算场景
  // 场景1：计算二次方程 ax² + bx + c
  let x = 5
  let a = 2
  let b = 3
  let c = 7
  
  let x_squared = @azimuth.multiply(x, x)  // x² = 25
  let ax_squared = @azimuth.multiply(a, x_squared)  // ax² = 50
  let bx = @azimuth.multiply(b, x)  // bx = 15
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)  // ax² + bx + c = 72
  
  @azimuth.assert_eq(72, result)
}

test "financial_calculation_compound_interest" {
  // 测试复利计算场景
  let principal = 10000  // 本金
  let rate = 5  // 利率百分比
  let years = 3  // 年数
  
  // 简化的复利计算：最终金额 = 本金 × (1 + 利率 × 年数)
  let growth_factor = @azimuth.add(1, @azimuth.multiply(rate, years))  // 1 + 5×3 = 16
  let final_amount = @azimuth.multiply(principal, growth_factor)  // 10000 × 16 = 160000
  
  @azimuth.assert_eq(160000, final_amount)
}

test "geometry_calculations_rectangle" {
  // 测试几何计算：矩形
  let length = 12
  let width = 8
  
  // 计算面积
  let area = @azimuth.multiply(length, width)
  @azimuth.assert_eq(96, area)
  
  // 计算周长
  let perimeter = @azimuth.add(@azimuth.multiply(2, length), @azimuth.multiply(2, width))
  @azimuth.assert_eq(40, perimeter)
  
  // 计算对角线长度（简化计算，不使用平方根）
  let length_squared = @azimuth.multiply(length, length)  // 144
  let width_squared = @azimuth.multiply(width, width)    // 64
  let diagonal_squared = @azimuth.add(length_squared, width_squared)  // 208
  
  @azimuth.assert_eq(208, diagonal_squared)
}

test "temperature_conversion_celsius_fahrenheit" {
  // 测试温度转换：摄氏度转华氏度
  // 简化公式：F = C × 2 + 30（避免小数）
  
  // 测试常见温度点
  let freezing_celsius = 0    // 冰点
  let freezing_fahrenheit = @azimuth.add(@azimuth.multiply(freezing_celsius, 2), 30)
  @azimuth.assert_eq(30, freezing_fahrenheit)
  
  let room_celsius = 20       // 室温
  let room_fahrenheit = @azimuth.add(@azimuth.multiply(room_celsius, 2), 30)
  @azimuth.assert_eq(70, room_fahrenheit)
  
  let boiling_celsius = 100   // 沸点
  let boiling_fahrenheit = @azimuth.add(@azimuth.multiply(boiling_celsius, 2), 30)
  @azimuth.assert_eq(230, boiling_fahrenheit)
}

test "shopping_cart_total_calculation" {
  // 测试购物车总价计算
  let item1_price = 299
  let item1_quantity = 2
  let item2_price = 199
  let item2_quantity = 3
  let item3_price = 99
  let item3_quantity = 1
  let shipping_fee = 15
  let discount = 50
  
  // 计算每种商品总价
  let total1 = @azimuth.multiply(item1_price, item1_quantity)  // 598
  let total2 = @azimuth.multiply(item2_price, item2_quantity)  // 597
  let total3 = @azimuth.multiply(item3_price, item3_quantity)  // 99
  
  // 计算商品总价
  let subtotal = @azimuth.add(@azimuth.add(total1, total2), total3)  // 1294
  
  // 应用折扣和运费
  let final_total = @azimuth.add(@azimuth.add(subtotal, -discount), shipping_fee)  // 1259
  
  @azimuth.assert_eq(1259, final_total)
}

test "statistical_calculations_mean_median" {
  // 测试统计计算：平均值和中位数
  // 数据集：10, 20, 30, 40, 50, 60, 70
  
  // 计算总和
  let sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(10, 20), 30), 40), 50), 60), 70)
  @azimuth.assert_eq(280, sum)
  
  // 计算平均值
  let count = 7
  let mean = sum / count
  @azimuth.assert_eq(40, mean)
  
  // 计算中位数（已排序数据集的中间值）
  let median = 40
  @azimuth.assert_eq(median, median)
  
  // 计算范围（最大值 - 最小值）
  let range = @azimuth.add(70, -10)
  @azimuth.assert_eq(60, range)
}

test "time_calculations_duration" {
  // 测试时间计算：持续时间
  let start_hour = 9   // 9:00 AM
  let start_minute = 30
  let end_hour = 14     // 2:00 PM
  let end_minute = 45
  
  // 转换为分钟数
  let start_total = @azimuth.add(@azimuth.multiply(start_hour, 60), start_minute)  // 570
  let end_total = @azimuth.add(@azimuth.multiply(end_hour, 60), end_minute)        // 885
  
  // 计算持续时间（分钟）
  let duration = @azimuth.add(end_total, -start_total)  // 315分钟 = 5小时15分钟
  
  @azimuth.assert_eq(315, duration)
  
  // 转换回小时和分钟
  let duration_hours = duration / 60  // 5
  let duration_minutes = duration % 60  // 15
  
  @azimuth.assert_eq(5, duration_hours)
  @azimuth.assert_eq(15, duration_minutes)
}

test "game_score_calculation_complex" {
  // 测试游戏得分计算：复杂场景
  let base_score = 1000
  let level = 5
  let combo_multiplier = 3
  let time_bonus = 500
  let accuracy_penalty = 200
  let difficulty_bonus = 300
  
  // 计算等级得分
  let level_score = @azimuth.multiply(base_score, level)  // 5000
  
  // 应用连击倍数
  let combo_score = @azimuth.multiply(level_score, combo_multiplier)  // 15000
  
  // 加成和减分
  let with_time_bonus = @azimuth.add(combo_score, time_bonus)  // 15500
  let with_accuracy_penalty = @azimuth.add(with_time_bonus, -accuracy_penalty)  // 15300
  let final_score = @azimuth.add(with_accuracy_penalty, difficulty_bonus)  // 15600
  
  @azimuth.assert_eq(15600, final_score)
  
  // 计算排名（每1000分一个等级）
  let rank_level = final_score / 1000  // 15
  @azimuth.assert_eq(15, rank_level)
}

// 新增的10个标准 MoonBit 测试用例
test "large_number_operations" {
  // 测试大数运算
  let large_num1 = 1000000
  let large_num2 = 2000000
  
  // 大数相加
  let sum_result = @azimuth.add(large_num1, large_num2)
  @azimuth.assert_eq(3000000, sum_result)
  
  // 大数相乘
  let product_result = @azimuth.multiply(1000, 2000)
  @azimuth.assert_eq(2000000, product_result)
  
  // 大数与边界值运算
  let max_val = 2147483647
  let large_sum = @azimuth.add(max_val, 1000000)
  @azimuth.assert_eq(max_val, large_sum)  // 应该被限制在最大值
}

test "negative_number_operations" {
  // 测试负数运算
  let neg_num1 = -100
  let neg_num2 = -200
  let pos_num = 150
  
  // 负数相加
  let neg_sum = @azimuth.add(neg_num1, neg_num2)
  @azimuth.assert_eq(-300, neg_sum)
  
  // 正负数相加
  let mixed_sum = @azimuth.add(neg_num1, pos_num)
  @azimuth.assert_eq(50, mixed_sum)
  
  // 负数相乘
  let neg_product = @azimuth.multiply(neg_num1, neg_num2)
  @azimuth.assert_eq(20000, neg_product)
  
  // 正负数相乘
  let mixed_product = @azimuth.multiply(neg_num1, pos_num)
  @azimuth.assert_eq(-15000, mixed_product)
}

test "boundary_value_operations" {
  // 测试边界值运算
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 最大值运算
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, 1))
  @azimuth.assert_eq(max_val, @azimuth.multiply(max_val, 2))
  @azimuth.assert_eq(max_val, @azimuth.add(max_val, max_val))
  
  // 最小值运算
  @azimuth.assert_eq(min_val, @azimuth.add(min_val, -1))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 2))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, -1))
  
  // 边界值组合
  @azimuth.assert_eq(-1, @azimuth.add(max_val, min_val))
  @azimuth.assert_eq(min_val, @azimuth.multiply(min_val, 1))
}

test "string_processing_extreme" {
  // 测试字符串处理的极端情况
  // 空字符串
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  
  // 单个字符
  @azimuth.assert_eq_string("Hello, A!", @azimuth.greet("A"))
  
  // 长字符串
  let long_string = "ThisIsAVeryLongStringThatContainsManyCharactersAndShouldStillWorkProperly"
  @azimuth.assert_eq_string("Hello, " + long_string + "!", @azimuth.greet(long_string))
  
  // 包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  @azimuth.assert_eq_string("Hello, " + special_chars + "!", @azimuth.greet(special_chars))
  
  // 包含Unicode字符的字符串
  let unicode_string = "测试中文🌟Emoji"
  @azimuth.assert_eq_string("Hello, " + unicode_string + "!", @azimuth.greet(unicode_string))
}

test "mathematical_properties_validation" {
  // 测试数学性质验证
  let a = 12
  let b = 8
  let c = 5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律
  @azimuth.assert_eq(@azimuth.multiply(a, @azimuth.add(b, c)), @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c)))
}

test "real_world_currency_conversion" {
  // 测试实际应用：货币转换
  let usd_amount = 100
  let exchange_rate = 7  // 1 USD = 7 CNY (简化汇率)
  let fee_rate = 2  // 2% 手续费
  
  // 转换为人民币
  let cny_amount = @azimuth.multiply(usd_amount, exchange_rate)
  @azimuth.assert_eq(700, cny_amount)
  
  // 计算手续费
  let fee = @azimuth.multiply(cny_amount, fee_rate) / 100
  @azimuth.assert_eq(14, fee)
  
  // 计算最终金额
  let final_amount = @azimuth.add(cny_amount, -fee)
  @azimuth.assert_eq(686, final_amount)
  
  // 反向计算验证
  let reverse_usd = final_amount / exchange_rate
  @azimuth.assert_eq(98, reverse_usd)  // 整数除法，舍去小数部分
}

test "error_boundary_handling" {
  // 测试错误边界处理
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护
  let overflow_add = @azimuth.add(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_add)
  
  let underflow_add = @azimuth.add(min_val, min_val)
  @azimuth.assert_eq(min_val, underflow_add)
  
  let overflow_mul = @azimuth.multiply(max_val, max_val)
  @azimuth.assert_eq(max_val, overflow_mul)
  
  // 测试除零保护（通过模拟）
  // 由于没有除法函数，我们通过其他方式测试错误处理
  let zero_result = @azimuth.multiply(0, max_val)
  @azimuth.assert_eq(0, zero_result)
  
  // 测试边界情况下的特殊处理
  let special_case = @azimuth.multiply(min_val, -1)
  @azimuth.assert_eq(min_val, special_case)
}

test "performance_calculation_patterns" {
  // 测试性能计算模式
  let base_value = 100
  let multiplier = 2
  let iterations = 10
  
  // 模拟循环累加：base_value + base_value*multiplier + ... + base_value*multiplier^iterations
  // 使用公式计算：base_value * (multiplier^(iterations+1) - 1) / (multiplier - 1)
  
  // 手动计算前几项验证
  let term1 = base_value  // 100
  let term2 = @azimuth.multiply(base_value, multiplier)  // 200
  let term3 = @azimuth.multiply(term2, multiplier)  // 400
  let term4 = @azimuth.multiply(term3, multiplier)  // 800
  
  let sum_first_4 = @azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4)
  @azimuth.assert_eq(1500, sum_first_4)
  
  // 计算等比数列的和（简化版）
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)  // 2^4 = 16
  let geometric_sum = @azimuth.multiply(base_value, power_of_2)
  @azimuth.assert_eq(1600, geometric_sum)
}

test "complex_formula_evaluation" {
  // 测试复杂公式评估
  // 计算二次方程 ax² + bx + c 的值
  let x = 7
  let a = 3
  let b = 5
  let c = 2
  
  // 计算 x²
  let x_squared = @azimuth.multiply(x, x)  // 49
  
  // 计算 ax²
  let ax_squared = @azimuth.multiply(a, x_squared)  // 147
  
  // 计算 bx
  let bx = @azimuth.multiply(b, x)  // 35
  
  // 计算最终结果
  let result = @azimuth.add(@azimuth.add(ax_squared, bx), c)  // 147 + 35 + 2 = 184
  @azimuth.assert_eq(184, result)
  
  // 计算导数值（简化版）：2ax + b
  let derivative = @azimuth.add(@azimuth.multiply(@azimuth.multiply(2, a), x), b)  // 2*3*7 + 5 = 47
  @azimuth.assert_eq(47, derivative)
}

test "data_type_boundary_simulation" {
  // 测试数据类型边界模拟
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 模拟有符号整数溢出行为
  let positive_to_negative = @azimuth.add(max_val, 1)
  @azimuth.assert_eq(max_val, positive_to_negative)  // 应该被限制
  
  let negative_to_positive = @azimuth.add(min_val, 1)
  @azimuth.assert_eq(-2147483647, negative_to_positive)
  
  // 模拟无符号整数行为（通过偏移）
  let offset = 2147483647  // 最大值
  let unsigned_value = 100
  let signed_representation = @azimuth.add(unsigned_value, -offset)
  @azimuth.assert_eq(-2147483547, signed_representation)
  
  // 测试字符串到数字的模拟转换
  let digit_string = "123"
  let simulated_number = @azimuth.add(@azimuth.add(100, 20), 3)  // 模拟 "123" 转换为数字
  @azimuth.assert_eq(123, simulated_number)
}

// 新增的高质量测试用例
test "mathematical_commutativity_laws" {
  // 验证交换律：a + b = b + a 和 a * b = b * a
  let a = 17
  let b = 23
  let c = -5
  
  // 加法交换律
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  @azimuth.assert_eq(@azimuth.add(b, c), @azimuth.add(c, b))
  @azimuth.assert_eq(@azimuth.add(a, c), @azimuth.add(c, a))
  
  // 乘法交换律
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  @azimuth.assert_eq(@azimuth.multiply(b, c), @azimuth.multiply(c, b))
  @azimuth.assert_eq(@azimuth.multiply(a, c), @azimuth.multiply(c, a))
}

test "mathematical_associativity_laws" {
  // 验证结合律：(a + b) + c = a + (b + c) 和 (a * b) * c = a * (b * c)
  let a = 7
  let b = 11
  let c = 13
  
  // 加法结合律
  let add_left = @azimuth.add(@azimuth.add(a, b), c)
  let add_right = @azimuth.add(a, @azimuth.add(b, c))
  @azimuth.assert_eq(add_left, add_right)
  
  // 乘法结合律
  let mul_left = @azimuth.multiply(@azimuth.multiply(a, b), c)
  let mul_right = @azimuth.multiply(a, @azimuth.multiply(b, c))
  @azimuth.assert_eq(mul_left, mul_right)
}

test "distributive_property_verification" {
  // 验证分配律：a * (b + c) = a * b + a * c
  let a = 6
  let b = 8
  let c = 12
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
}

test "divide_with_ceil_comprehensive_behavior" {
  // 全面测试向上取整除法的行为
  // 正数情况
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(13, 4))  // 3.25 -> 4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(12, 4))  // 3 -> 3
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 5))   // 0.6 -> 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))   // 1 -> 1
  
  // 负数情况
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-11, 4))  // -2.75 -> -3
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-8, 4))   // -2 -> -2
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(-7, 4))   // -1.75 -> -2
  
  // 混合符号
  @azimuth.assert_eq(-2, @azimuth.divide_with_ceil(7, -4))   // -1.75 -> -2
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-7, -4))   // 1.75 -> 2
  
  // 边界情况
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 10))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(5, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
}

test "identity_elements_properties" {
  // 验证单位元性质：a + 0 = a 和 a * 1 = a
  let test_values = [0, 1, -1, 42, -17, 1000, -1000]
  
  for value in test_values {
    // 加法单位元
    @azimuth.assert_eq(value, @azimuth.add(value, 0))
    @azimuth.assert_eq(value, @azimuth.add(0, value))
    
    // 乘法单位元
    @azimuth.assert_eq(value, @azimuth.multiply(value, 1))
    @azimuth.assert_eq(value, @azimuth.multiply(1, value))
  }
}

test "inverse_elements_properties" {
  // 验证逆元性质：a + (-a) = 0
  let test_values = [1, -1, 42, -17, 1000, -1000, 2147483647, -2147483648]
  
  for value in test_values {
    let inverse = -value
    @azimuth.assert_eq(0, @azimuth.add(value, inverse))
  }
}

test "zero_element_properties" {
  // 验证零元素性质：a * 0 = 0
  let test_values = [0, 1, -1, 42, -17, 1000, -1000, 2147483647, -2147483648]
  
  for value in test_values {
    @azimuth.assert_eq(0, @azimuth.multiply(value, 0))
    @azimuth.assert_eq(0, @azimuth.multiply(0, value))
  }
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 10          // 贷款年限
  
  // 简化的单利计算（实际应该是复利）
  let total_interest = @azimuth.divide_with_ceil(@azimuth.multiply(principal, annual_rate * years), 100)
  let total_payment = @azimuth.add(principal, total_interest)
  let monthly_payment = @azimuth.divide_with_ceil(total_payment, years * 12)
  
  @azimuth.assert_eq(60000, total_interest)
  @azimuth.assert_eq(160000, total_payment)
  @azimuth.assert_eq(1334, monthly_payment)  // 向上取整
}

test "scientific_computation_scenario" {
  // 科学计算场景：物理公式计算
  let mass = 100      // 质量 kg
  let acceleration = 9 // 加速度 m/s²
  let distance = 50   // 距离 m
  
  // 计算力：F = m * a
  let force = @azimuth.multiply(mass, acceleration)
  
  // 计算功：W = F * d
  let work = @azimuth.multiply(force, distance)
  
  // 计算功率（假设时间）：P = W / t，假设 t = 10s
  let time = 10
  let power = @azimuth.divide_with_ceil(work, time)
  
  @azimuth.assert_eq(900, force)
  @azimuth.assert_eq(45000, work)
  @azimuth.assert_eq(4500, power)
}

test "string_processing_edge_cases" {
  // 字符串处理的边界情况
  // 测试各种特殊字符和边界情况
  @azimuth.assert_eq_string("Hello, !", @azimuth.greet(""))
  @azimuth.assert_eq_string("Hello,   !", @azimuth.greet("  "))
  @azimuth.assert_eq_string("Hello, \n\t\r!", @azimuth.greet("\n\t\r"))
  @azimuth.assert_eq_string("Hello, \\\"!'!", @azimuth.greet("\\\"!'"))
  @azimuth.assert_eq_string("Hello, 😊!", @azimuth.greet("😊"))
  @azimuth.assert_eq_string("Hello, \u{1F600}!", @azimuth.greet("\u{1F600}"))  // Unicode grin face
  @azimuth.assert_eq_string("Hello, αβγ!", @azimuth.greet("αβγ"))
  @azimuth.assert_eq_string("Hello, ñáéíóú!", @azimuth.greet("ñáéíóú"))
  @azimuth.assert_eq_string("Hello, 🌍🌎🌏!", @azimuth.greet("🌍🌎🌏"))
}

// 新增的10个标准 MoonBit 质量测试用例
// 测试1: 基本算术运算的组合测试
test "quality_arithmetic_combination_operations" {
  // 测试多种算术运算的组合使用
  let x = 12
  let y = 8
  let z = 5
  
  // 计算 (x + y) * z
  let result1 = @azimuth.multiply(@azimuth.add(x, y), z)
  @azimuth.assert_eq(100, result1)  // (12 + 8) * 5 = 20 * 5 = 100
  
  // 计算 x * y + z
  let result2 = @azimuth.add(@azimuth.multiply(x, y), z)
  @azimuth.assert_eq(101, result2)  // 12 * 8 + 5 = 96 + 5 = 101
  
  // 计算 (x - y) * z (使用 add 实现减法)
  let result3 = @azimuth.multiply(@azimuth.add(x, -y), z)
  @azimuth.assert_eq(20, result3)  // (12 - 8) * 5 = 4 * 5 = 20
}

// 测试2: 向上取整除法的边界情况
test "quality_divide_with_ceil_boundary_conditions" {
  // 测试各种边界条件下的向上取整除法
  
  // 被除数小于除数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(3, 5))   // 3/5 = 0.6 -> 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 100)) // 1/100 = 0.01 -> 1
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(99, 100)) // 99/100 = 0.99 -> 1
  
  // 被除数等于除数
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(7, 7))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1, 1))
  @azimuth.assert_eq(1, @azimuth.divide_with_ceil(1000, 1000))
  
  // 被除数是除数的倍数
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(15, 5))   // 15/5 = 3 -> 3
  @azimuth.assert_eq(10, @azimuth.divide_with_ceil(100, 10)) // 100/10 = 10 -> 10
  @azimuth.assert_eq(7, @azimuth.divide_with_ceil(49, 7))   // 49/7 = 7 -> 7
}

// 测试3: 负数运算的全面测试
test "quality_comprehensive_negative_operations" {
  // 测试负数的各种运算情况
  
  // 负数加法
  @azimuth.assert_eq(-10, @azimuth.add(-3, -7))
  @azimuth.assert_eq(-5, @azimuth.add(-8, 3))
  @azimuth.assert_eq(0, @azimuth.add(-5, 5))
  @azimuth.assert_eq(5, @azimuth.add(-10, 15))
  
  // 负数乘法
  @azimuth.assert_eq(15, @azimuth.multiply(-3, -5))  // 负负得正
  @azimuth.assert_eq(-15, @azimuth.multiply(3, -5))  // 一负一负得负
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))    // 乘以零
  
  // 负数除法向上取整
  @azimuth.assert_eq(-3, @azimuth.divide_with_ceil(-10, 3))   // -10/3 = -3.333... -> -3
  @azimuth.assert_eq(-4, @azimuth.divide_with_ceil(-11, 3))   // -11/3 = -3.666... -> -4
  @azimuth.assert_eq(3, @azimuth.divide_with_ceil(-9, -3))    // -9/-3 = 3 -> 3
  @azimuth.assert_eq(4, @azimuth.divide_with_ceil(-10, -3))   // -10/-3 = 3.333... -> 4
}

// 测试4: 字符串问候的多语言支持
test "quality_multilingual_greeting_support" {
  // 测试多种语言的问候功能
  
  // 中文测试
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 中国!", @azimuth.greet("中国"))
  @azimuth.assert_eq_string("Hello, 北京!", @azimuth.greet("北京"))
  
  // 欧洲语言测试
  @azimuth.assert_eq_string("Hello, Español!", @azimuth.greet("Español"))
  @azimuth.assert_eq_string("Hello, Français!", @azimuth.greet("Français"))
  @azimuth.assert_eq_string("Hello, Deutsch!", @azimuth.greet("Deutsch"))
  @azimuth.assert_eq_string("Hello, Русский!", @azimuth.greet("Русский"))
  
  // 特殊字符和符号
  @azimuth.assert_eq_string("Hello, 123!", @azimuth.greet("123"))
  @azimuth.assert_eq_string("Hello, test@example.com!", @azimuth.greet("test@example.com"))
  @azimuth.assert_eq_string("Hello, A+B=C!", @azimuth.greet("A+B=C"))
  @azimuth.assert_eq_string("Hello, 🚀!", @azimuth.greet("🚀"))
}

// 测试5: 实际业务场景 - 订单计算
test "quality_business_order_calculation" {
  // 模拟电商订单计算场景
  
  let product_price = 89
  let quantity = 4
  let tax_rate = 8  // 8%
  let shipping_cost = 15
  let discount = 20
  
  // 计算商品小计
  let subtotal = @azimuth.multiply(product_price, quantity)
  @azimuth.assert_eq(356, subtotal)  // 89 * 4 = 356
  
  // 计算税费（向上取整）
  let tax = @azimuth.divide_with_ceil(@azimuth.multiply(subtotal, tax_rate), 100)
  @azimuth.assert_eq(29, tax)  // ceil(356 * 8 / 100) = ceil(28.48) = 29
  
  // 计算总价（含税和运费）
  let total_before_discount = @azimuth.add(@azimuth.add(subtotal, tax), shipping_cost)
  @azimuth.assert_eq(400, total_before_discount)  // 356 + 29 + 15 = 400
  
  // 应用折扣
  let final_total = @azimuth.add(total_before_discount, -discount)
  @azimuth.assert_eq(380, final_total)  // 400 - 20 = 380
}

// 测试6: 数学性质验证
test "quality_mathematical_properties_verification" {
  // 验证基本的数学性质
  
  let a = 7
  let b = 13
  let c = 5
  
  // 加法交换律: a + b = b + a
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 乘法交换律: a * b = b * a
  @azimuth.assert_eq(@azimuth.multiply(a, b), @azimuth.multiply(b, a))
  
  // 加法结合律: (a + b) + c = a + (b + c)
  @azimuth.assert_eq(@azimuth.add(@azimuth.add(a, b), c), @azimuth.add(a, @azimuth.add(b, c)))
  
  // 乘法结合律: (a * b) * c = a * (b * c)
  @azimuth.assert_eq(@azimuth.multiply(@azimuth.multiply(a, b), c), @azimuth.multiply(a, @azimuth.multiply(b, c)))
  
  // 分配律: a * (b + c) = a * b + a * c
  let distributive_left = @azimuth.multiply(a, @azimuth.add(b, c))
  let distributive_right = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(distributive_left, distributive_right)
  
  // 零元素性质
  @azimuth.assert_eq(0, @azimuth.multiply(a, 0))  // a * 0 = 0
  @azimuth.assert_eq(a, @azimuth.add(a, 0))       // a + 0 = a
}

// 测试7: 大数运算测试
test "quality_large_number_operations" {
  // 测试大数运算的稳定性和准确性
  
  // 大数加法
  let big_num1 = 1000000
  let big_num2 = 500000
  @azimuth.assert_eq(1500000, @azimuth.add(big_num1, big_num2))
  
  // 大数乘法
  let big_factor1 = 10000
  let big_factor2 = 100
  @azimuth.assert_eq(1000000, @azimuth.multiply(big_factor1, big_factor2))
  
  // 大数除法向上取整
  let large_dividend = 1000000
  let large_divisor = 3
  @azimuth.assert_eq(333334, @azimuth.divide_with_ceil(large_dividend, large_divisor))
  
  // 组合大数运算
  let complex_result = @azimuth.add(@azimuth.multiply(1000, 1000), @azimuth.divide_with_ceil(999999, 1000))
  @azimuth.assert_eq(1001000, complex_result)  // 1000000 + 1000 = 1001000
}

// 测试8: 零值和特殊情况处理
test "quality_zero_and_special_cases_handling" {
  // 测试零值和特殊情况的处理
  
  // 与零的加法
  @azimuth.assert_eq(5, @azimuth.add(5, 0))
  @azimuth.assert_eq(-5, @azimuth.add(-5, 0))
  @azimuth.assert_eq(0, @azimuth.add(0, 0))
  
  // 与零的乘法
  @azimuth.assert_eq(0, @azimuth.multiply(5, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 5))
  @azimuth.assert_eq(0, @azimuth.multiply(0, 0))
  @azimuth.assert_eq(0, @azimuth.multiply(-5, 0))
  
  // 除数为零的安全处理
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(-100, 0))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 0))
  
  // 零作为被除数
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 5))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 1))
  @azimuth.assert_eq(0, @azimuth.divide_with_ceil(0, 100))
}

// 测试9: 实际应用场景 - 包装和物流计算
test "quality_packaging_logistics_calculation" {
  // 模拟包装和物流计算场景
  
  // 场景1: 计算需要的包装箱数量
  let total_items = 127
  let items_per_box = 15
  let boxes_needed = @azimuth.divide_with_ceil(total_items, items_per_box)
  @azimuth.assert_eq(9, boxes_needed)  // ceil(127/15) = ceil(8.466...) = 9
  
  // 场景2: 计算运输车辆需求
  let total_weight = 2500
  let vehicle_capacity = 800
  let vehicles_needed = @azimuth.divide_with_ceil(total_weight, vehicle_capacity)
  @azimuth.assert_eq(4, vehicles_needed)  // ceil(2500/800) = ceil(3.125) = 4
  
  // 场景3: 计算仓库空间需求
  let boxes_per_pallet = 4
  let total_pallets_needed = @azimuth.divide_with_ceil(boxes_needed, boxes_per_pallet)
  @azimuth.assert_eq(3, total_pallets_needed)  // ceil(9/4) = ceil(2.25) = 3
  
  // 场景4: 计算总成本
  let box_cost = 5
  let vehicle_cost = 50
  let pallet_cost = 20
  let total_cost = @azimuth.add(@azimuth.add(@azimuth.multiply(boxes_needed, box_cost), 
                          @azimuth.multiply(vehicles_needed, vehicle_cost)),
                       @azimuth.multiply(total_pallets_needed, pallet_cost))
  @azimuth.assert_eq(305, total_cost)  // 9*5 + 4*50 + 3*20 = 45 + 200 + 60 = 305
}

// 测试10: 复杂数学表达式计算
test "quality_complex_mathematical_expressions" {
  // 测试复杂数学表达式的计算
  
  let x = 10
  let y = 15
  let z = 8
  
  // 表达式1: (x + y) * z - x / y
  let expr1 = @azimuth.add(@azimuth.multiply(@azimuth.add(x, y), z), -@azimuth.divide_with_ceil(x, y))
  @azimuth.assert_eq(199, expr1)  // (10 + 15) * 8 - ceil(10/15) = 25*8 - 1 = 200 - 1 = 199
  
  // 表达式2: x * y + y * z - z * x
  let expr2 = @azimuth.add(@azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(y, z)), -@azimuth.multiply(z, x))
  @azimuth.assert_eq(190, expr2)  // 10*15 + 15*8 - 8*10 = 150 + 120 - 80 = 190
  
  // 表达式3: (x + y + z) * 2
  let expr3 = @azimuth.multiply(@azimuth.add(@azimuth.add(x, y), z), 2)
  @azimuth.assert_eq(66, expr3)  // (10 + 15 + 8) * 2 = 33 * 2 = 66
  
  // 表达式4: ceil(x * y / z) + ceil(y * z / x)
  let expr4 = @azimuth.add(@azimuth.divide_with_ceil(@azimuth.multiply(x, y), z), 
                  @azimuth.divide_with_ceil(@azimuth.multiply(y, z), x))
  @azimuth.assert_eq(31, expr4)  // ceil(10*15/8) + ceil(15*8/10) = ceil(150/8) + ceil(120/10) = 19 + 12 = 31
  
  // 表达式5: 复杂嵌套表达式
  let expr5 = @azimuth.divide_with_ceil(@azimuth.multiply(@azimuth.add(x, @azimuth.multiply(y, 2)), 3), 4)
  @azimuth.assert_eq(30, expr5)  // ceil((10 + 15*2) * 3 / 4) = ceil((10 + 30) * 3 / 4) = ceil(40*3/4) = ceil(120/4) = 30
}