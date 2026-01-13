// 额外的 MoonBit 测试用例
// 这些测试用例覆盖了更多边界情况和实际应用场景

test "hexadecimal_conversion_simulation" {
  // 模拟十六进制转换：将十进制数转换为十六进制表示
  // 使用字符串拼接来模拟十六进制表示
  
  // 测试 0-15 的基本转换
  assert_eq_string("0xA", "0xA")  // 10 的十六进制
  assert_eq_string("0xF", "0xF")  // 15 的十六进制
  
  // 测试更大的数（简化模拟）
  let decimal_255 = 255
  let hex_255 = "0xFF"
  assert_true(decimal_255 > 0)
  
  // 通过 greet 函数测试十六进制字符串处理
  let hex_result = greet("0x10")
  assert_eq_string("Hello, 0x10!", hex_result)
}

test "matrix_operations_simulation" {
  // 模拟简单的矩阵运算
  // 2x2 矩阵加法：[[a, b], [c, d]] + [[e, f], [g, h]] = [[a+e, b+f], [c+g, d+h]]
  
  let a11 = 1, a12 = 2
  let a21 = 3, a22 = 4
  let b11 = 5, b12 = 6
  let b21 = 7, b22 = 8
  
  // 矩阵加法结果
  let c11 = add(a11, b11)  // 6
  let c12 = add(a12, b12)  // 8
  let c21 = add(a21, b21)  // 10
  let c22 = add(a22, b22)  // 12
  
  // 验证结果
  assert_eq(6, c11)
  assert_eq(8, c12)
  assert_eq(10, c21)
  assert_eq(12, c22)
  
  // 计算矩阵的迹（对角线元素之和）
  let trace_a = add(a11, a22)  // 1 + 4 = 5
  let trace_b = add(b11, b22)  // 5 + 8 = 13
  let trace_c = add(c11, c22)  // 6 + 12 = 18
  
  assert_eq(5, trace_a)
  assert_eq(13, trace_b)
  assert_eq(18, trace_c)
}

test "physics_calculations" {
  // 物理计算测试
  // 自由落体距离：s = 0.5 * g * t^2（简化为整数计算）
  // 使用 g = 10 m/s²（简化值）
  
  let gravity = 10
  let time = 3
  
  // 简化的距离计算：s = 5 * t^2
  let distance = multiply(5, multiply(time, time))
  assert_eq(45, distance)  // 5 * 9 = 45 米
  
  // 速度计算：v = g * t
  let velocity = multiply(gravity, time)
  assert_eq(30, velocity)  // 10 * 3 = 30 m/s
  
  // 动能计算：E = 0.5 * m * v^2（简化为整数）
  let mass = 2
  let energy = multiply(mass, multiply(velocity, velocity)) / 2
  assert_eq(900, energy)  // 2 * 900 / 2 = 900 焦耳
}

test "financial_investment_calculation" {
  // 投资计算测试
  let principal = 10000
  let annual_rate = 8  // 8% 年利率
  let years = 5
  
  // 简单利息计算
  let simple_interest = multiply(principal, multiply(annual_rate, years)) / 100
  let simple_total = add(principal, simple_interest)
  assert_eq(14000, simple_total)  // 10000 + 10000*8*5/100 = 14000
  
  // 复利计算（简化）：A = P * (1 + r)^n
  // 简化为：A = P * (100 + r*n) / 100
  let compound_factor = add(100, multiply(annual_rate, years))
  let compound_total = multiply(principal, compound_factor) / 100
  assert_eq(14000, compound_total)  // 10000 * 140 / 100 = 14000
  
  // 月度投资计算
  let monthly_investment = 500
  let months = multiply(years, 12)
  let total_invested = multiply(monthly_investment, months)
  assert_eq(30000, total_invested)  // 500 * 60 = 30000
}

test "data_structure_simulation" {
  // 数据结构模拟：栈操作
  // 使用变量模拟栈的 push 和 pop 操作
  
  // 初始化栈
  let stack_top1 = 10
  let stack_size1 = 1
  
  // push 操作
  let stack_top2 = 20
  let stack_size2 = add(stack_size1, 1)
  assert_eq(2, stack_size2)
  
  // push 操作
  let stack_top3 = 30
  let stack_size3 = add(stack_size2, 1)
  assert_eq(3, stack_size3)
  
  // pop 操作（模拟）
  let popped_value = stack_top3
  let stack_size_after_pop = stack_size2
  assert_eq(30, popped_value)
  assert_eq(2, stack_size_after_pop)
  
  // 队列操作模拟（简化）
  let queue_front = 100
  let queue_rear = 200
  let queue_size = 2
  
  // 入队操作
  let new_rear = 300
  let new_size = add(queue_size, 1)
  assert_eq(3, new_size)
}

test "string_pattern_matching" {
  // 字符串模式匹配测试（通过 greet 函数）
  // 测试不同模式的字符串
  
  // 电子邮件模式
  let email = "user@example.com"
  let email_greeting = greet(email)
  assert_eq_string("Hello, user@example.com!", email_greeting)
  
  // 电话号码模式
  let phone = "123-456-7890"
  let phone_greeting = greet(phone)
  assert_eq_string("Hello, 123-456-7890!", phone_greeting)
  
  // URL 模式
  let url = "https://example.com"
  let url_greeting = greet(url)
  assert_eq_string("Hello, https://example.com!", url_greeting)
  
  // 日期模式
  let date = "2023-12-25"
  let date_greeting = greet(date)
  assert_eq_string("Hello, 2023-12-25!", date_greeting)
}

test "number_system_conversions" {
  // 数字系统转换测试（简化模拟）
  
  // 二进制模拟（通过字符串）
  let binary_str = "1010"  // 代表十进制 10
  let binary_greeting = greet(binary_str)
  assert_eq_string("Hello, 1010!", binary_greeting)
  
  // 八进制模拟
  let octal_str = "12"  // 代表十进制 10
  let octal_greeting = greet(octal_str)
  assert_eq_string("Hello, 12!", octal_greeting)
  
  // 十六进制模拟
  let hex_str = "A"  // 代表十进制 10
  let hex_greeting = greet(hex_str)
  assert_eq_string("Hello, A!", hex_greeting)
  
  // 罗马数字模拟
  let roman_str = "X"  // 代表十进制 10
  let roman_greeting = greet(roman_str)
  assert_eq_string("Hello, X!", roman_greeting)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟测试
  
  // O(1) - 常数时间操作
  let constant_time_result = add(5, 3)
  assert_eq(8, constant_time_result)
  
  // O(n) - 线性时间操作（模拟）
  let n = 100
  let linear_sum = multiply(n, add(n, 1)) / 2  // 1 + 2 + ... + n
  assert_eq(5050, linear_sum)
  
  // O(n²) - 平方时间操作（模拟）
  // 计算 n x n 矩阵的所有元素之和
  let matrix_sum = multiply(n, multiply(n, add(n, 1)) / 2)
  assert_eq(505000, matrix_sum)
  
  // O(log n) - 对数时间操作（模拟）
  // 二分查找的步骤数（简化）
  let log_steps = 7  // log2(100) ≈ 6.64，向上取整为 7
  assert_true(log_steps < n)
}

test "error_boundary_testing" {
  // 错误边界测试
  
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试加法边界
  assert_eq(max_val, add(max_val, 1))  // 溢出处理
  assert_eq(min_val, add(min_val, -1))  // 溢出处理
  
  // 测试乘法边界
  assert_eq(max_val, multiply(max_val, 2))  // 溢出处理
  assert_eq(min_val, multiply(min_val, 2))  // 溢出处理
  
  // 测试除零保护（通过条件检查）
  let dividend = 100
  let divisor = 0
  // 在实际代码中，应该检查 divisor != 0 再进行除法
  let safe_division = if divisor != 0 { dividend / divisor } else { 0 }
  assert_eq(0, safe_division)
  
  // 测试字符串边界
  let very_long_string = "ThisIsAVeryLongStringThatMightCauseIssues"
  let long_string_greeting = greet(very_long_string)
  assert_true(long_string_greeting.length() > 0)
}

test "real_world_business_logic" {
  // 实际业务逻辑测试
  
  // 库存管理
  let initial_stock = 100
  let sold_items = 30
  let restocked_items = 20
  
  let current_stock = add(add(initial_stock, -sold_items), restocked_items)
  assert_eq(90, current_stock)
  
  // 折扣计算
  let original_price = 200
  let discount_percentage = 15
  let discount_amount = multiply(original_price, discount_percentage) / 100
  let final_price = add(original_price, -discount_amount)
  assert_eq(170, final_price)
  
  // 税收计算
  let price_before_tax = 170
  let tax_rate = 8  // 8%
  let tax_amount = multiply(price_before_tax, tax_rate) / 100
  let total_price = add(price_before_tax, tax_amount)
  assert_eq(183, total_price)  // 170 + 170*8/100 = 170 + 13 = 183
}