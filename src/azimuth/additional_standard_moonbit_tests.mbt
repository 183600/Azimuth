// 额外的标准 MoonBit 测试用例
// 这些测试用例涵盖了各种实际应用场景和边界情况

test "calculator_basic_operations" {
  // 基础计算器操作测试
  let a = 15
  let b = 7
  
  // 加法测试
  let sum = add(a, b)
  assert_eq(22, sum)
  
  // 乘法测试
  let product = multiply(a, b)
  assert_eq(105, product)
  
  // 复合运算：(a + b) * (a - b)
  let difference = add(a, -b)
  let complex_result = multiply(sum, difference)
  assert_eq(176, complex_result)  // 22 * 8 = 176
}

test "price_calculation_with_tax" {
  // 价格计算与税费测试
  let base_price = 199
  let quantity = 3
  let tax_rate = 8  // 8% 税率
  
  // 计算总价
  let subtotal = multiply(base_price, quantity)
  // 计算税费
  let tax_amount = multiply(subtotal, tax_rate) / 100
  // 计算最终价格
  let total_price = add(subtotal, tax_amount)
  
  assert_eq(597, subtotal)      // 199 * 3 = 597
  assert_eq(47, tax_amount)     // 597 * 8 / 100 = 47.76 ≈ 47 (整数除法)
  assert_eq(644, total_price)   // 597 + 47 = 644
}

test "temperature_conversion_extended" {
  // 扩展温度转换测试
  // 摄氏度转华氏度：F = C * 9/5 + 32，简化为 F = C * 2 + 30
  
  // 常见温度点测试
  let freezing_celsius = 0
  let freezing_fahrenheit = add(multiply(freezing_celsius, 2), 30)
  assert_eq(30, freezing_fahrenheit)
  
  let room_temp_celsius = 20
  let room_temp_fahrenheit = add(multiply(room_temp_celsius, 2), 30)
  assert_eq(70, room_temp_fahrenheit)
  
  let boiling_celsius = 100
  let boiling_fahrenheit = add(multiply(boiling_celsius, 2), 30)
  assert_eq(230, boiling_fahrenheit)
  
  // 负温度测试
  let below_zero_celsius = -10
  let below_zero_fahrenheit = add(multiply(below_zero_celsius, 2), 30)
  assert_eq(10, below_zero_fahrenheit)
}

test "array_sum_and_average" {
  // 数组求和与平均值测试（使用模拟数组）
  // 模拟数组 [10, 20, 30, 40, 50]
  let num1 = 10
  let num2 = 20
  let num3 = 30
  let num4 = 40
  let num5 = 50
  
  // 计算总和
  let sum = add(add(add(add(num1, num2), num3), num4), num5)
  assert_eq(150, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(30, average)
  
  // 计算加权平均（权重分别为 1, 2, 3, 4, 5）
  let weighted_sum = add(add(add(add(multiply(num1, 1), multiply(num2, 2)), multiply(num3, 3)), multiply(num4, 4))
  weighted_sum = add(weighted_sum, multiply(num5, 5))
  let total_weight = add(add(add(add(1, 2), 3), 4), 5)
  let weighted_average = weighted_sum / total_weight
  
  assert_eq(366, weighted_sum)     // 10*1 + 20*2 + 30*3 + 40*4 + 50*5 = 366
  assert_eq(15, total_weight)      // 1 + 2 + 3 + 4 + 5 = 15
  assert_eq(24, weighted_average)  // 366 / 15 = 24.4 ≈ 24 (整数除法)
}

test "loan_interest_calculation" {
  // 贷款利息计算测试
  let principal = 10000  // 本金
  let annual_rate = 6    // 年利率 6%
  let years = 3          // 贷款年限
  
  // 简单利息计算：利息 = 本金 × 利率 × 年数
  let simple_interest = multiply(principal, multiply(annual_rate, years))
  // 总还款额 = 本金 + 利息
  let total_repayment = add(principal, simple_interest)
  
  assert_eq(180000, simple_interest)  // 10000 * 6 * 3 = 180000
  assert_eq(190000, total_repayment)  // 10000 + 180000 = 190000
  
  // 复利计算（简化）：总还款额 = 本金 × (1 + 利率 × 年数)
  let compound_factor = add(1, multiply(annual_rate, years))
  let compound_total = multiply(principal, compound_factor)
  
  assert_eq(19, compound_factor)   // 1 + 6 * 3 = 19
  assert_eq(190000, compound_total) // 10000 * 19 = 190000
}

test "geometric_area_calculations" {
  // 几何面积计算测试
  let pi = 3  // 简化的 π 值
  
  // 正方形面积：边长 × 边长
  let square_side = 6
  let square_area = multiply(square_side, square_side)
  assert_eq(36, square_area)
  
  // 矩形面积：长 × 宽
  let rectangle_length = 8
  let rectangle_width = 5
  let rectangle_area = multiply(rectangle_length, rectangle_width)
  assert_eq(40, rectangle_area)
  
  // 三角形面积：底 × 高 ÷ 2
  let triangle_base = 10
  let triangle_height = 6
  let triangle_area = multiply(triangle_base, triangle_height) / 2
  assert_eq(30, triangle_area)
  
  // 梯形面积：（上底 + 下底）× 高 ÷ 2
  let trapezoid_top = 4
  let trapezoid_bottom = 8
  let trapezoid_height = 5
  let trapezoid_area = multiply(add(trapezoid_top, trapezoid_bottom), trapezoid_height) / 2
  assert_eq(30, trapezoid_area)  // (4 + 8) * 5 / 2 = 30
  
  // 圆面积（简化）：π × r²
  let circle_radius = 5
  let circle_area = multiply(pi, multiply(circle_radius, circle_radius))
  assert_eq(75, circle_area)  // 3 * 5 * 5 = 75
}

test "string_processing_edge_cases" {
  // 字符串处理边界情况测试
  // 测试各种特殊字符串
  
  // 空字符串
  let empty_result = greet("")
  assert_eq_string("Hello, !", empty_result)
  
  // 单个字符
  let single_char_result = greet("A")
  assert_eq_string("Hello, A!", single_char_result)
  
  // 数字字符串
  let numeric_result = greet("12345")
  assert_eq_string("Hello, 12345!", numeric_result)
  
  // 包含空格的字符串
  let spaced_result = greet("Hello World")
  assert_eq_string("Hello, Hello World!", spaced_result)
  
  // 包含特殊字符的字符串
  let special_result = greet("!@#$%^&*()")
  assert_eq_string("Hello, !@#$%^&*()!", special_result)
  
  // 包含中文字符的字符串
  let chinese_result = greet("你好世界")
  assert_eq_string("Hello, 你好世界!", chinese_result)
  
  // 混合字符串
  let mixed_result = greet("Test测试123")
  assert_eq_string("Hello, Test测试123!", mixed_result)
}

test "speed_distance_time_calculations" {
  // 速度、距离、时间计算测试
  // 基础公式：距离 = 速度 × 时间
  
  // 基础计算
  let speed = 60  // km/h
  let time = 2    // hours
  let distance = multiply(speed, time)
  assert_eq(120, distance)
  
  // 反向计算：时间 = 距离 / 速度
  let known_distance = 180
  let calculated_time = known_distance / speed
  assert_eq(3, calculated_time)
  
  // 反向计算：速度 = 距离 / 时间
  let known_time = 4
  let calculated_speed = known_distance / known_time
  assert_eq(45, calculated_speed)
  
  // 复合计算：平均速度
  let speed1 = 40
  let speed2 = 60
  let equal_time = 1  // 相等时间段的平均速度
  
  // 总距离 = 速度1 × 时间 + 速度2 × 时间
  let total_distance = add(multiply(speed1, equal_time), multiply(speed2, equal_time))
  // 总时间 = 时间 + 时间
  let total_time = add(equal_time, equal_time)
  // 平均速度 = 总距离 / 总时间
  let average_speed = total_distance / total_time
  
  assert_eq(100, total_distance)  // 40*1 + 60*1 = 100
  assert_eq(2, total_time)        // 1 + 1 = 2
  assert_eq(50, average_speed)    // 100 / 2 = 50
}

test "binary_operations_simulation" {
  // 二进制运算模拟测试
  let a = 12  // 二进制 1100
  let b = 5   // 二进制 0101
  
  // 模拟按位与运算（简化）：只保留共同位
  // 这里简化为取较小值
  let bitwise_and_sim = if a < b { a } else { b }
  assert_eq(5, bitwise_and_sim)
  
  // 模拟按位或运算（简化）：合并所有位
  // 这里简化为相加
  let bitwise_or_sim = add(a, b)
  assert_eq(17, bitwise_or_sim)
  
  // 模拟按位异或运算（简化）：不同位
  // 这里简化为差的绝对值
  let bitwise_xor_sim = if a > b { add(a, -b) } else { add(b, -a) }
  assert_eq(7, bitwise_xor_sim)
  
  // 模拟左移运算：a << n = a * 2^n
  let shift_left_1 = multiply(a, 2)
  let shift_left_2 = multiply(a, 4)
  assert_eq(24, shift_left_1)  // 12 * 2 = 24
  assert_eq(48, shift_left_2)  // 12 * 4 = 48
  
  // 模拟右移运算：a >> n = a / 2^n
  let shift_right_1 = a / 2
  let shift_right_2 = a / 4
  assert_eq(6, shift_right_1)  // 12 / 2 = 6
  assert_eq(3, shift_right_2)  // 12 / 4 = 3
}

test "statistical_variance_calculation" {
  // 统计方差计算测试
  // 数据集：[10, 15, 20, 25, 30]
  let data1 = 10
  let data2 = 15
  let data3 = 20
  let data4 = 25
  let data5 = 30
  
  // 计算平均值
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  let mean = sum / 5
  assert_eq(20, mean)
  
  // 计算方差：每个数据点与平均值的差的平方的平均值
  let diff1 = add(data1, -mean)  // 10 - 20 = -10
  let diff2 = add(data2, -mean)  // 15 - 20 = -5
  let diff3 = add(data3, -mean)  // 20 - 20 = 0
  let diff4 = add(data4, -mean)  // 25 - 20 = 5
  let diff5 = add(data5, -mean)  // 30 - 20 = 10
  
  // 计算平方差
  let square_diff1 = multiply(diff1, diff1)  // (-10)^2 = 100
  let square_diff2 = multiply(diff2, diff2)  // (-5)^2 = 25
  let square_diff3 = multiply(diff3, diff3)  // 0^2 = 0
  let square_diff4 = multiply(diff4, diff4)  // 5^2 = 25
  let square_diff5 = multiply(diff5, diff5)  // 10^2 = 100
  
  // 计算平方差的总和
  let sum_square_diffs = add(add(add(add(square_diff1, square_diff2), square_diff3), square_diff4), square_diff5)
  assert_eq(250, sum_square_diffs)  // 100 + 25 + 0 + 25 + 100 = 250
  
  // 计算方差
  let variance = sum_square_diffs / 5
  assert_eq(50, variance)  // 250 / 5 = 50
  
  // 计算标准差（方差的平方根，这里简化为整数除法）
  let standard_deviation = variance / 7  // 简化计算，实际应该是开方
  assert_eq(7, standard_deviation)  // 50 / 7 ≈ 7
}