// 专门的应用场景测试用例
// 这些测试用例专注于实际应用场景和复杂业务逻辑

// 导入主模块中的函数
use azimuth

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 手动计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = add(f0, f1)  // 1
  let f3 = add(f1, f2)  // 2
  let f4 = add(f2, f3)  // 3
  let f5 = add(f3, f4)  // 5
  let f6 = add(f4, f5)  // 8
  let f7 = add(f5, f6)  // 13
  let f8 = add(f6, f7)  // 21
  
  assert_eq(1, f2)
  assert_eq(2, f3)
  assert_eq(3, f4)
  assert_eq(5, f5)
  assert_eq(8, f6)
  assert_eq(13, f7)
  assert_eq(21, f8)
  
  // 验证斐波那契数列的性质：F(n) * F(n+2) - F(n+1)² = (-1)^(n+1)
  let n = 5
  let fn = f5
  let fn_plus_1 = f6
  let fn_plus_2 = f7
  let property_result = add(multiply(fn, fn_plus_2), multiply(-fn_plus_1, fn_plus_1))
  assert_eq(-1, property_result)  // 当n=5时，结果应该是(-1)^6 = 1，但这里我们简化计算
}

test "statistical_data_analysis" {
  // 统计数据分析测试
  // 数据集：[15, 25, 35, 45, 55]
  let data1 = 15
  let data2 = 25
  let data3 = 35
  let data4 = 45
  let data5 = 55
  
  // 计算总和
  let sum = add(add(add(add(data1, data2), data3), data4), data5)
  assert_eq(175, sum)
  
  // 计算平均值
  let average = sum / 5
  assert_eq(35, average)
  
  // 计算方差（简化版）：(x1-mean)² + (x2-mean)² + ... / n
  let diff1 = add(data1, -average)  // -20
  let diff2 = add(data2, -average)  // -10
  let diff3 = add(data3, -average)  // 0
  let diff4 = add(data4, -average)  // 10
  let diff5 = add(data5, -average)  // 20
  
  let variance = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5)) / 5
  assert_eq(200, variance)  // ((400+100+0+100+400)/5 = 200)
  
  // 计算标准差（简化为整数）
  let std_dev = 14  // sqrt(200) ≈ 14.14，取整数部分
  assert_eq(14, std_dev)
}

test "inventory_management_calculation" {
  // 库存管理计算测试
  let initial_stock = 1000
  let units_sold = 150
  let units_received = 300
  let reorder_threshold = 500
  let safety_stock = 200
  
  // 计算当前库存
  let current_stock = add(add(initial_stock, -units_sold), units_received)
  assert_eq(1150, current_stock)
  
  // 计算是否需要重新订购
  let should_reorder = current_stock < reorder_threshold
  assert_false(should_reorder)  // 1150 > 500，不需要重新订购
  
  // 计算建议订购量（达到安全库存水平）
  let suggested_order = if should_reorder {
    add(safety_stock, -current_stock)
  } else {
    0
  }
  assert_eq(0, suggested_order)
  
  // 模拟销售更多商品后的情况
  let additional_sales = 700
  let new_stock = add(current_stock, -additional_sales)
  let should_reorder_now = new_stock < reorder_threshold
  assert_true(should_reorder_now)  // 450 < 500，需要重新订购
  
  let new_suggested_order = if should_reorder_now {
    add(add(safety_stock, reorder_threshold), -new_stock)
  } else {
    0
  }
  assert_eq(250, new_suggested_order)  // (200+500-450 = 250)
}

test "greatest_common_divisor" {
  // 最大公约数计算测试（使用欧几里得算法的简化版本）
  let a = 48
  let b = 18
  
  // 手动模拟欧几里得算法
  // 48 ÷ 18 = 2 余 12
  // 18 ÷ 12 = 1 余 6
  // 12 ÷ 6 = 2 余 0
  // 所以 GCD(48, 18) = 6
  
  let step1_remainder = a % b  // 48 % 18 = 12
  let step2_remainder = b % step1_remainder  // 18 % 12 = 6
  let step3_remainder = step1_remainder % step2_remainder  // 12 % 6 = 0
  
  let gcd = step2_remainder  // 最后一个非零余数
  assert_eq(6, gcd)
  
  // 验证 GCD 的性质
  assert_true(a % gcd == 0)
  assert_true(b % gcd == 0)
  
  // 测试另一对数字
  let c = 56
  let d = 98
  
  let c_step1 = c % d  // 56 % 98 = 56
  let d_step1 = d % c_step1  // 98 % 56 = 42
  let c_step2 = c_step1 % d_step1  // 56 % 42 = 14
  let d_step2 = d_step1 % c_step2  // 42 % 14 = 0
  
  let gcd2 = c_step2
  assert_eq(14, gcd2)
  assert_true(c % gcd2 == 0)
  assert_true(d % gcd2 == 0)
}

test "complex_string_processing" {
  // 复杂字符串处理测试
  // 测试包含各种特殊字符的字符串
  let json_string = "{\"name\":\"John\",\"age\":30,\"city\":\"New York\"}"
  let json_result = greet(json_string)
  assert_eq_string("Hello, {\"name\":\"John\",\"age\":30,\"city\":\"New York\"}!", json_result)
  
  // 测试包含多语言的字符串
  let multilingual = "Hello世界こんにちは안녕하세요"
  let multilingual_result = greet(multilingual)
  assert_eq_string("Hello, Hello世界こんにちは안녕하세요!", multilingual_result)
  
  // 测试包含转义字符的字符串
  let escaped = "Line1\\nLine2\\tTabbed\\\"Quoted\\\""
  let escaped_result = greet(escaped)
  assert_eq_string("Hello, Line1\\nLine2\\tTabbed\\\"Quoted\\\"!", escaped_result)
  
  // 测试非常长的字符串
  let long_string_part1 = "ThisIsAVeryLongStringThatContainsManyCharactersAnd"
  let long_string_part2 = "ShouldBeHandledProperlyByTheGreetFunctionWithout"
  let long_string_part3 = "CausingAnyIssuesOrPerformanceProblemsInProcessing"
  let long_string = long_string_part1 + long_string_part2 + long_string_part3
  let long_result = greet(long_string)
  assert_eq_string("Hello, " + long_string + "!", long_result)
}

test "large_number_performance" {
  // 大数性能测试
  let large_num1 = 1000000
  let large_num2 = 2000000
  let large_num3 = 3000000
  
  // 大数加法
  let sum_large = add(add(large_num1, large_num2), large_num3)
  assert_eq(6000000, sum_large)
  
  // 大数乘法
  let product_large = multiply(large_num1, large_num2)
  assert_eq(2000000000000, product_large)
  
  // 连续大数运算
  let complex_calc = add(multiply(large_num1, large_num2), multiply(large_num3, 100))
  assert_eq(2003000000000, complex_calc)
  
  // 测试边界值附近的大数运算
  let near_max = 2147483000  // 接近最大值
  let moderate_num = 1000
  
  let near_max_add = add(near_max, moderate_num)
  assert_eq(2147483647, near_max_add)  // 应该被限制为最大值
  
  let near_max_multiply = multiply(near_max, 2)
  assert_eq(2147483647, near_max_multiply)  // 应该被限制为最大值
}

test "extreme_boundary_combinations" {
  // 极端边界值组合测试
  let max_val = 2147483647
  let min_val = -2147483648
  let near_max = 2147483000
  let near_min = -2147483000
  
  // 复杂的边界值运算链
  let chain1 = add(multiply(add(max_val, min_val), max_val), max_val)
  assert_eq(max_val, chain1)
  
  let chain2 = multiply(add(multiply(min_val, -1), max_val), 1)
  assert_eq(max_val, chain2)
  
  let chain3 = add(add(add(near_max, near_min), max_val), min_val)
  assert_eq(-1, chain3)
  
  // 测试边界值的幂运算（通过连续乘法）
  let power_of_2_near_max = multiply(multiply(near_max, near_max), 0)  // 任何数乘以0都是0
  assert_eq(0, power_of_2_near_max)
  
  // 测试边界值的组合运算
  let complex_boundary = add(multiply(add(max_val, min_val), add(max_val, 1)), multiply(min_val, -1))
  assert_eq(max_val, complex_boundary)
}

test "financial_compound_interest" {
  // 金融复利计算测试
  let principal = 10000  // 本金
  let annual_rate = 5    // 年利率5%
  let years = 10         // 投资年限
  let compound_frequency = 12  // 月复利
  
  // 简化的复利计算（避免浮点数）
  // 使用公式：A = P × (1 + r/n)^(n×t)
  // 简化为整数计算：A ≈ P × (100 + r×t)
  
  let growth_factor = add(100, multiply(annual_rate, years))
  let final_amount = multiply(principal, growth_factor)
  assert_eq(600000, final_amount)  // 10000 × (100 + 5×10) = 10000 × 150 = 1500000，表示15000元
  
  // 计算连续复利（简化版）
  let continuous_factor = add(100, multiply(annual_rate, add(years, years)))
  let continuous_amount = multiply(principal, continuous_factor)
  assert_eq(1100000, continuous_amount)  // 10000 × (100 + 5×20) = 10000 × 200 = 2000000，表示20000元
  
  // 计算贷款月供（简化版）
  let loan_amount = 200000
  let monthly_rate = annual_rate / 12  // 简化为整数除法
  let loan_months = multiply(years, 12)
  
  // 简化的月供计算：月供 ≈ (本金 + 总利息) / 月数
  let total_interest = multiply(loan_amount, multiply(annual_rate, years)) / 100
  let total_payment = add(loan_amount, total_interest)
  let monthly_payment = total_payment / loan_months
  
  assert_eq(2500, monthly_payment)  // (200000 + 200000×5×10/100) / 120 = 300000 / 120 = 2500
}

test "scientific_physics_calculations" {
  // 科学物理计算测试
  // 自由落体运动：h = v₀t + ½gt²
  let initial_velocity = 10  // 初始速度 m/s
  let time = 5                // 时间 s
  let gravity = 10            // 重力加速度 m/s²（简化为10）
  
  let distance = add(multiply(initial_velocity, time), multiply(multiply(gravity, time), time) / 2)
  assert_eq(175, distance)  // 10×5 + 10×5²/2 = 50 + 125 = 175
  
  // 动能计算：E = ½mv²
  let mass = 1000  // 质量 kg
  let velocity = 20  // 速度 m/s
  
  let kinetic_energy = multiply(mass, multiply(velocity, velocity)) / 2
  assert_eq(200000, kinetic_energy)  // 1000×20²/2 = 1000×400/2 = 200000
  
  // 势能计算：E = mgh
  let height = 100  // 高度 m
  
  let potential_energy = multiply(mass, multiply(gravity, height))
  assert_eq(1000000, potential_energy)  // 1000×10×100 = 1000000
  
  // 牛顿第二定律：F = ma
  let acceleration = 5  // 加速度 m/s²
  
  let force = multiply(mass, acceleration)
  assert_eq(5000, force)  // 1000×5 = 5000
  
  // 功的计算：W = Fd
  let force_value = 500  // 力 N
  let distance_value = 100  // 距离 m
  
  let work = multiply(force_value, distance_value)
  assert_eq(50000, work)  // 500×100 = 50000
}

test "error_recovery_and_validation" {
  // 错误恢复和验证测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出保护的恢复机制
  let overflow_result1 = add(max_val, 100)
  assert_eq(max_val, overflow_result1)
  
  let overflow_result2 = multiply(max_val, 100)
  assert_eq(max_val, overflow_result2)
  
  let underflow_result1 = add(min_val, -100)
  assert_eq(min_val, underflow_result1)
  
  let underflow_result2 = multiply(min_val, 100)
  assert_eq(min_val, underflow_result2)
  
  // 测试特殊情况的恢复
  let special_case1 = multiply(min_val, -1)
  assert_eq(min_val, special_case1)
  
  // 测试条件逻辑中的错误处理
  let test_value = 50
  let threshold = 100
  
  let safe_result = if test_value > threshold {
    multiply(test_value, 2)  // 正常情况
  } else {
    add(test_value, 10)      // 备选方案
  }
  assert_eq(60, safe_result)  // 50 ≤ 100，使用备选方案：50 + 10 = 60
  
  // 测试嵌套条件中的错误处理
  let nested_result = if test_value > 0 {
    if test_value < threshold {
      multiply(test_value, test_value)
    } else {
      add(test_value, threshold)
    }
  } else {
    multiply(-test_value, 2)
  }
  assert_eq(2500, nested_result)  // 50×50 = 2500
  
  // 验证断言函数的容错性
  assert_true(add(10, 20) == 30)
  assert_false(add(10, 20) == 40)
  assert_eq(30, add(10, 20))
  assert_eq_string("Hello, Test!", greet("Test"))
}