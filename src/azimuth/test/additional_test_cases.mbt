// 额外的 MoonBit 测试用例
// 使用标准的 MoonBit 测试语法

test "add_commutative_property" {
  // 测试加法交换律：a + b = b + a
  let a = 12345
  let b = 67890
  @azimuth.assert_eq(@azimuth.add(a, b), @azimuth.add(b, a))
  
  // 测试负数情况
  let c = -12345
  let d = 67890
  @azimuth.assert_eq(@azimuth.add(c, d), @azimuth.add(d, c))
  
  // 测试两个负数
  let e = -12345
  let f = -67890
  @azimuth.assert_eq(@azimuth.add(e, f), @azimuth.add(f, e))
}

test "multiply_distributive_property" {
  // 测试乘法分配律：a × (b + c) = a × b + a × c
  let a = 7
  let b = 15
  let c = 23
  
  let left_side = @azimuth.multiply(a, @azimuth.add(b, c))
  let right_side = @azimuth.add(@azimuth.multiply(a, b), @azimuth.multiply(a, c))
  @azimuth.assert_eq(left_side, right_side)
  
  // 测试包含负数的情况
  let x = -5
  let y = 12
  let z = -8
  
  let left_side2 = @azimuth.multiply(x, @azimuth.add(y, z))
  let right_side2 = @azimuth.add(@azimuth.multiply(x, y), @azimuth.multiply(x, z))
  @azimuth.assert_eq(left_side2, right_side2)
}

test "greet_multilingual_support" {
  // 测试多语言支持的问候功能
  @azimuth.assert_eq_string("Hello, 世界!", @azimuth.greet("世界"))
  @azimuth.assert_eq_string("Hello, 🌍!", @azimuth.greet("🌍"))
  @azimuth.assert_eq_string("Hello, café!", @azimuth.greet("café"))
  @azimuth.assert_eq_string("Hello, Москва!", @azimuth.greet("Москва"))
  @azimuth.assert_eq_string("Hello, العربية!", @azimuth.greet("العربية"))
}

test "complex_financial_calculation" {
  // 复杂金融计算：贷款分期计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 3           // 3年
  
  // 简化的利息计算：总利息 = 本金 × 年利率 × 年数
  let total_interest = @azimuth.multiply(principal, @azimuth.multiply(annual_rate, years))
  
  // 总还款额 = 本金 + 总利息
  let total_payment = @azimuth.add(principal, total_interest)
  
  // 月还款额（简化计算，不考虑复利）
  let monthly_payment = total_payment / (years * 12)
  
  @azimuth.assert_eq(1180000, total_payment)  // 100000 + 100000*6*3 = 1180000
  @azimuth.assert_eq(32777, monthly_payment)  // 1180000 / 36 ≈ 32777
}

test "physics_simulation" {
  // 物理模拟：自由落体运动计算（简化版）
  let initial_height = 100  // 初始高度（米）
  let time = 4              // 时间（秒）
  
  // 下落距离 = 0.5 × g × t²（简化为 g × t²）
  let distance = @azimuth.multiply(10, @azimuth.multiply(time, time))
  
  // 剩余高度 = 初始高度 - 下落距离
  let remaining_height = @azimuth.add(initial_height, -distance)
  
  @azimuth.assert_eq(160, distance)      // 10 × 4² = 160
  @azimuth.assert_eq(-60, remaining_height)  // 100 - 160 = -60（已落地）
}

test "data_structure_operations" {
  // 数据结构操作模拟
  // 模拟栈操作：push和pop
  let mut stack_size = 0
  
  // push操作
  stack_size = @azimuth.add(stack_size, 1)  // push item1
  stack_size = @azimuth.add(stack_size, 1)  // push item2
  stack_size = @azimuth.add(stack_size, 1)  // push item3
  
  @azimuth.assert_eq(3, stack_size)
  
  // pop操作
  stack_size = @azimuth.add(stack_size, -1)  // pop item3
  stack_size = @azimuth.add(stack_size, -1)  // pop item2
  
  @azimuth.assert_eq(1, stack_size)
  
  // 模拟队列操作：enqueue和dequeue
  let mut queue_size = 0
  let enqueue_count = 5
  let dequeue_count = 2
  
  queue_size = @azimuth.add(queue_size, enqueue_count)   // enqueue 5 items
  queue_size = @azimuth.add(queue_size, -dequeue_count)  // dequeue 2 items
  
  @azimuth.assert_eq(3, queue_size)
}

test "statistical_analysis" {
  // 统计分析测试
  // 模拟数据集：10, 20, 30, 40, 50
  let data1 = 10
  let data2 = 20
  let data3 = 30
  let data4 = 40
  let data5 = 50
  
  // 计算总和
  let data_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(data1, data2), data3), data4), data5)
  let data_count = 5
  
  // 计算平均值
  let average = data_sum / data_count
  
  // 计算方差（简化版）
  let diff1 = @azimuth.add(data1, -average)
  let diff2 = @azimuth.add(data2, -average)
  let diff3 = @azimuth.add(data3, -average)
  let diff4 = @azimuth.add(data4, -average)
  let diff5 = @azimuth.add(data5, -average)
  
  let variance = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.multiply(diff1, diff1), @azimuth.multiply(diff2, diff2)), 
                           @azimuth.multiply(diff3, diff3)), @azimuth.multiply(diff4, diff4)), 
                           @azimuth.multiply(diff5, diff5)) / data_count
  
  @azimuth.assert_eq(150, data_sum)
  @azimuth.assert_eq(30, average)
  @azimuth.assert_eq(200, variance)  // 简化计算
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟
  let n = 100
  
  // O(1) 常数时间复杂度
  let constant_time = 1
  
  // O(n) 线性时间复杂度
  let linear_time = n
  
  // O(n²) 平方时间复杂度
  let quadratic_time = @azimuth.multiply(n, n)
  
  // O(log n) 对数时间复杂度（简化为 n/10）
  let logarithmic_time = n / 10
  
  @azimuth.assert_eq(1, constant_time)
  @azimuth.assert_eq(100, linear_time)
  @azimuth.assert_eq(10000, quadratic_time)
  @azimuth.assert_eq(10, logarithmic_time)
}

test "edge_case_comprehensive" {
  // 综合边界情况测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试连续运算的边界情况
  let result1 = @azimuth.add(@azimuth.add(max_val, -max_val), max_val)
  @azimuth.assert_eq(max_val, result1)
  
  let result2 = @azimuth.multiply(@azimuth.multiply(min_val, -1), -1)
  @azimuth.assert_eq(min_val, result2)
  
  // 测试混合边界运算
  let result3 = @azimuth.add(@azimuth.multiply(max_val, 0), min_val)
  @azimuth.assert_eq(min_val, result3)
  
  let result4 = @azimuth.multiply(@azimuth.add(max_val, min_val), 100)
  @azimuth.assert_eq(-100, result4)
  
  // 测试字符串边界情况
  let very_long_name = "A" + "B" + "C" + "D" + "E" + "F" + "G" + "H" + "I" + "J"
  let long_greeting = @azimuth.greet(very_long_name)
  @azimuth.assert_eq_string("Hello, ABCDEFGHIJ!", long_greeting)
}