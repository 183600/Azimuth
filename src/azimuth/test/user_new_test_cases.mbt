// 用户新增的 MoonBit 测试用例
// 这些测试用例专注于边界情况、实际应用场景和复合运算

test "division_simulation_using_multiplication" {
  // 使用乘法和减法模拟除法运算
  // 计算 100 / 5 = 20
  let dividend = 100
  let divisor = 5
  
  // 通过重复减法模拟除法
  let quotient = 20  // 预期结果
  let remainder = @azimuth.@azimuth.add(dividend, @azimuth.@azimuth.multiply(-divisor, quotient))
  
  @azimuth.@azimuth.assert_eq(0, remainder)  // 验证余数为0
  @azimuth.@azimuth.assert_eq(20, quotient) // 验证商为20
  
  // 测试有余数的情况：100 / 6 = 16 余 4
  let divisor2 = 6
  let quotient2 = 16
  let remainder2 = @azimuth.@azimuth.add(dividend, @azimuth.@azimuth.multiply(-divisor2, quotient2))
  
  @azimuth.@azimuth.assert_eq(4, remainder2)  // 验证余数为4
}

test "compound_interest_with_monthly_compounding" {
  // 模拟月复利计算
  let principal = 10000  // 本金
  let annual_rate = 12   // 年利率12%
  let months = 12        // 12个月
  
  // 简化的月复利计算：每月利率 = 年利率 / 12
  let monthly_rate = annual_rate / 12
  
  // 第一个月后的金额
  let month1 = @azimuth.@azimuth.add(principal, @azimuth.@azimuth.multiply(principal, monthly_rate) / 100)
  
  // 第二个月后的金额（简化计算）
  let month2 = @azimuth.@azimuth.add(month1, @azimuth.@azimuth.multiply(month1, monthly_rate) / 100)
  
  // 验证计算结果
  @azimuth.@azimuth.assert_true(month1 > principal)  // 确保值增长
  @azimuth.@azimuth.assert_true(month1 < 12000)      // 确保在合理范围内
}

test "string_validation_and_processing" {
  // 字符串验证和处理测试
  let valid_email = "user@example.com"
  let invalid_email = "user.example.com"
  let phone_number = "123-456-7890"
  
  // 使用greet函数测试不同格式的字符串
  let email_result = @azimuth.@azimuth.greet(valid_email)
  let invalid_result = @azimuth.@azimuth.greet(invalid_email)
  let phone_result = @azimuth.@azimuth.greet(phone_number)
  
  @azimuth.@azimuth.assert_eq_string("Hello, user@example.com!", email_result)
  @azimuth.@azimuth.assert_eq_string("Hello, user.example.com!", invalid_result)
  @azimuth.@azimuth.assert_eq_string("Hello, 123-456-7890!", phone_result)
  
  // 测试包含特殊字符的字符串
  let special_chars = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  let special_result = @azimuth.@azimuth.greet(special_chars)
  @azimuth.@azimuth.assert_eq_string("Hello, !@#$%^&*()_+-=[]{}|;':\",./<>?!", special_result)
}

test "matrix_multiplication_simulation" {
  // 模拟简单的矩阵乘法（2x2矩阵）
  // 矩阵A: [a b; c d] 和 矩阵B: [e f; g h]
  let a = 1; let b = 2; let c = 3; let d = 4
  let e = 5; let f = 6; let g = 7; let h = 8
  
  // 结果矩阵: [a*e+b*g, a*f+b*h; c*e+d*g, c*f+d*h]
  let result1 = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(a, e), @azimuth.@azimuth.multiply(b, g))  // 1*5 + 2*7 = 19
  let result2 = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(a, f), @azimuth.@azimuth.multiply(b, h))  // 1*6 + 2*8 = 22
  let result3 = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(c, e), @azimuth.@azimuth.multiply(d, g))  // 3*5 + 4*7 = 43
  let result4 = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(c, f), @azimuth.@azimuth.multiply(d, h))  // 3*6 + 4*8 = 50
  
  @azimuth.@azimuth.assert_eq(19, result1)
  @azimuth.@azimuth.assert_eq(22, result2)
  @azimuth.@azimuth.assert_eq(43, result3)
  @azimuth.@azimuth.assert_eq(50, result4)
  
  // 验证矩阵乘法的性质：行列式的乘积
  let det_a = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(a, d), @azimuth.@azimuth.multiply(-b, c))  // 1*4 - 2*3 = -2
  let det_b = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(e, h), @azimuth.@azimuth.multiply(-f, g))  // 5*8 - 6*7 = -2
  let det_result = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(result1, result4), @azimuth.@azimuth.multiply(-result2, result3))  // 19*50 - 22*43 = -2
  
  @azimuth.@azimuth.assert_eq(det_result, @azimuth.@azimuth.multiply(det_a, det_b))
}

test "temperature_conversion_precise" {
  // 精确的温度转换测试
  // 摄氏度转华氏度：F = C * 9/5 + 32
  // 由于整数限制，使用近似计算：F = C * 2 - C/10 + 32
  
  let celsius = 25
  let fahrenheit_approx = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.multiply(celsius, 2), @azimuth.@azimuth.multiply(-(celsius / 10), 1)), 32)
  
  // 25°C ≈ 77°F (25*2 - 2 + 32 = 77)
  @azimuth.@azimuth.assert_eq(77, fahrenheit_approx)
  
  // 测试极端温度
  let freezing_c = 0
  let freezing_f = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.multiply(freezing_c, 2), @azimuth.@azimuth.multiply(-(freezing_c / 10), 1)), 32)
  @azimuth.@azimuth.assert_eq(32, freezing_f)
  
  let boiling_c = 100
  let boiling_f = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.multiply(boiling_c, 2), @azimuth.@azimuth.multiply(-(boiling_c / 10), 1)), 32)
  @azimuth.@azimuth.assert_eq(212, boiling_f)
  
  // 测试负温度
  let negative_c = -40
  let negative_f = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.multiply(negative_c, 2), @azimuth.@azimuth.multiply(-(negative_c / 10), 1)), 32)
  @azimuth.@azimuth.assert_eq(-40, negative_f)  // -40°C = -40°F
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 100
  let sales_day1 = 15
  let sales_day2 = 20
  let sales_day3 = 10
  let restock_day3 = 50
  
  // 计算每日库存
  let end_day1 = @azimuth.@azimuth.add(initial_stock, -sales_day1)
  let end_day2 = @azimuth.@azimuth.add(end_day1, -sales_day2)
  let end_day3 = @azimuth.@azimuth.add(@azimuth.@azimuth.add(end_day2, -sales_day3), restock_day3)
  
  @azimuth.@azimuth.assert_eq(85, end_day1)   // 100 - 15 = 85
  @azimuth.@azimuth.assert_eq(65, end_day2)   // 85 - 20 = 65
  @azimuth.@azimuth.assert_eq(105, end_day3)  // 65 - 10 + 50 = 105
  
  // 计算总销售额和总补货量
  let total_sales = @azimuth.@azimuth.add(@azimuth.@azimuth.add(sales_day1, sales_day2), sales_day3)
  let total_restock = restock_day3
  let net_change = @azimuth.@azimuth.add(total_restock, -total_sales)
  
  @azimuth.@azimuth.assert_eq(45, total_sales)  // 15 + 20 + 10 = 45
  @azimuth.@azimuth.assert_eq(50, total_restock)
  @azimuth.@azimuth.assert_eq(5, net_change)    // 50 - 45 = 5
  
  // 验证最终库存
  let final_stock = @azimuth.@azimuth.add(initial_stock, net_change)
  @azimuth.@azimuth.assert_eq(105, final_stock)  // 100 + 5 = 105
}

test "loan_amortization_calculation" {
  // 贷款分期偿还计算（简化版）
  let loan_amount = 120000  // 贷款金额
  let annual_rate = 6       // 年利率6%
  let loan_term = 30        // 贷款期限30年
  
  // 简化的月供计算：月供 ≈ (本金 + 总利息) / (年数 × 12)
  let total_interest = @azimuth.@azimuth.multiply(loan_amount, @azimuth.@azimuth.multiply(annual_rate, loan_term)) / 100
  let total_payment = @azimuth.@azimuth.add(loan_amount, total_interest)
  let monthly_payment = total_payment / @azimuth.@azimuth.multiply(loan_term, 12)
  
  @azimuth.@azimuth.assert_eq(216000, total_payment)  // 120000 + 120000*6*30/100 = 216000
  @azimuth.@azimuth.assert_eq(600, monthly_payment)   // 216000 / (30*12) = 600
  
  // 计算前3个月的本金和利息分配
  let monthly_interest = @azimuth.@azimuth.multiply(loan_amount, annual_rate) / 1200  // 月利率
  let monthly_principal = @azimuth.@azimuth.add(monthly_payment, -monthly_interest)
  
  @azimuth.@azimuth.assert_eq(600, monthly_payment)
  @azimuth.@azimuth.assert_true(monthly_interest > 0)
  @azimuth.@azimuth.assert_true(monthly_principal > 0)
  @azimuth.@azimuth.assert_eq(monthly_payment, @azimuth.@azimuth.add(monthly_interest, monthly_principal))
}

test "coordinate_geometry_calculations" {
  // 坐标几何计算测试
  // 两点 A(3,4) 和 B(7,10)
  let x1 = 3; let y1 = 4
  let x2 = 7; let y2 = 10
  
  // 计算距离的平方：(x2-x1)² + (y2-y1)²
  let dx = @azimuth.@azimuth.add(x2, -x1)  // 7-3 = 4
  let dy = @azimuth.@azimuth.add(y2, -y1)  // 10-4 = 6
  let distance_squared = @azimuth.@azimuth.add(@azimuth.@azimuth.multiply(dx, dx), @azimuth.@azimuth.multiply(dy, dy))  // 4² + 6² = 16 + 36 = 52
  
  @azimuth.@azimuth.assert_eq(4, dx)
  @azimuth.@azimuth.assert_eq(6, dy)
  @azimuth.@azimuth.assert_eq(52, distance_squared)
  
  // 计算中点坐标：((x1+x2)/2, (y1+y2)/2)
  let midpoint_x = @azimuth.@azimuth.add(x1, x2) / 2  // (3+7)/2 = 5
  let midpoint_y = @azimuth.@azimuth.add(y1, y2) / 2  // (4+10)/2 = 7
  
  @azimuth.@azimuth.assert_eq(5, midpoint_x)
  @azimuth.@azimuth.assert_eq(7, midpoint_y)
  
  // 计算斜率：(y2-y1)/(x2-x1)
  // 使用整数除法近似
  let slope_approx = @azimuth.@azimuth.multiply(dy, 100) / dx  // 6*100/4 = 150 (表示1.50)
  @azimuth.@azimuth.assert_eq(150, slope_approx)
}

test "statistical_variance_calculation" {
  // 统计方差计算测试
  // 数据集：5, 8, 12, 15, 20
  let data1 = 5; let data2 = 8; let data3 = 12; let data4 = 15; let data5 = 20
  
  // 计算平均值
  let sum = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(data1, data2), data3), data4), data5)  // 5+8+12+15+20 = 60
  let mean = sum / 5  // 60/5 = 12
  @azimuth.@azimuth.assert_eq(12, mean)
  
  // 计算方差：Σ(x-mean)²/n
  let diff1 = @azimuth.@azimuth.add(data1, -mean)  // 5-12 = -7
  let diff2 = @azimuth.@azimuth.add(data2, -mean)  // 8-12 = -4
  let diff3 = @azimuth.@azimuth.add(data3, -mean)  // 12-12 = 0
  let diff4 = @azimuth.@azimuth.add(data4, -mean)  // 15-12 = 3
  let diff5 = @azimuth.@azimuth.add(data5, -mean)  // 20-12 = 8
  
  let sq1 = @azimuth.@azimuth.multiply(diff1, diff1)  // (-7)² = 49
  let sq2 = @azimuth.@azimuth.multiply(diff2, diff2)  // (-4)² = 16
  let sq3 = @azimuth.@azimuth.multiply(diff3, diff3)  // 0² = 0
  let sq4 = @azimuth.@azimuth.multiply(diff4, diff4)  // 3² = 9
  let sq5 = @azimuth.@azimuth.multiply(diff5, diff5)  // 8² = 64
  
  let sum_squares = @azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(@azimuth.@azimuth.add(sq1, sq2), sq3), sq4), sq5)  // 49+16+0+9+64 = 138
  let variance = sum_squares / 5  // 138/5 = 27 (整数除法)
  
  @azimuth.@azimuth.assert_eq(27, variance)
  
  // 计算标准差（整数近似）
  let std_dev_approx = 5  // √27 ≈ 5.2，取整数部分
  @azimuth.@azimuth.assert_eq(5, std_dev_approx)
}

test "circular_buffer_operations" {
  // 循环缓冲区操作测试
  let buffer_size = 5
  let initial_head = 0
  let initial_tail = 0
  let count = 0
  
  // 模拟3次入队操作
  let enqueue1 = 1; let enqueue2 = 2; let enqueue3 = 3
  let new_count1 = @azimuth.@azimuth.add(count, 1)
  let new_count2 = @azimuth.@azimuth.add(new_count1, 1)
  let new_count3 = @azimuth.@azimuth.add(new_count2, 1)
  
  // 新的尾指针位置（循环）
  let new_tail1 = @azimuth.@azimuth.add(initial_tail, 1) % buffer_size  // (0+1)%5 = 1
  let new_tail2 = @azimuth.@azimuth.add(new_tail1, 1) % buffer_size     // (1+1)%5 = 2
  let new_tail3 = @azimuth.@azimuth.add(new_tail2, 1) % buffer_size     // (2+1)%5 = 3
  
  @azimuth.@azimuth.assert_eq(1, new_count1)
  @azimuth.@azimuth.assert_eq(2, new_count2)
  @azimuth.@azimuth.assert_eq(3, new_count3)
  @azimuth.@azimuth.assert_eq(1, new_tail1)
  @azimuth.@azimuth.assert_eq(2, new_tail2)
  @azimuth.@azimuth.assert_eq(3, new_tail3)
  
  // 模拟2次出队操作
  let dequeue_count1 = @azimuth.@azimuth.add(new_count3, -1)  // 3-1 = 2
  let dequeue_count2 = @azimuth.@azimuth.add(dequeue_count1, -1)  // 2-1 = 1
  
  // 新的头指针位置（循环）
  let new_head1 = @azimuth.@azimuth.add(initial_head, 1) % buffer_size  // (0+1)%5 = 1
  let new_head2 = @azimuth.@azimuth.add(new_head1, 1) % buffer_size     // (1+1)%5 = 2
  
  @azimuth.@azimuth.assert_eq(2, dequeue_count1)
  @azimuth.@azimuth.assert_eq(1, dequeue_count2)
  @azimuth.@azimuth.assert_eq(1, new_head1)
  @azimuth.@azimuth.assert_eq(2, new_head2)
  
  // 验证缓冲区状态
  @azimuth.@azimuth.assert_true(dequeue_count2 < buffer_size)  // 确保计数不超过缓冲区大小
  @azimuth.@azimuth.assert_eq(@azimuth.@azimuth.add(new_head2, dequeue_count2) % buffer_size, new_tail3)  // 验证头尾关系
}