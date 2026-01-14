// 增强的 MoonBit 测试用例
// 这些测试用例覆盖了更多实际应用场景和边界情况

// 导入主模块中的函数
use azimuth

test "algorithm_complexity_test" {
  // 算法复杂度测试：计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // F(2) = F(1) + F(0) = 1 + 0 = 1
  let f2 = add(1, 0)
  assert_eq(1, f2)
  
  // F(3) = F(2) + F(1) = 1 + 1 = 2
  let f3 = add(f2, 1)
  assert_eq(2, f3)
  
  // F(4) = F(3) + F(2) = 2 + 1 = 3
  let f4 = add(f3, f2)
  assert_eq(3, f4)
  
  // F(5) = F(4) + F(3) = 3 + 2 = 5
  let f5 = add(f4, f3)
  assert_eq(5, f5)
}

test "data_structure_operations" {
  // 数据结构操作测试：模拟栈操作
  // 使用加减法模拟栈的 push 和 pop 操作
  
  // 初始栈为空（用 0 表示）
  let stack = 0
  
  // push 10: stack = stack * 10 + 10 = 0 * 10 + 10 = 10
  stack = add(multiply(stack, 10), 10)
  assert_eq(10, stack)
  
  // push 20: stack = stack * 100 + 20 = 10 * 100 + 20 = 1020
  stack = add(multiply(stack, 100), 20)
  assert_eq(1020, stack)
  
  // push 30: stack = stack * 100 + 30 = 1020 * 100 + 30 = 102030
  stack = add(multiply(stack, 100), 30)
  assert_eq(102030, stack)
  
  // pop 30: stack = (stack - 30) / 100 = (102030 - 30) / 100 = 1020
  stack = add(stack, -30) / 100
  assert_eq(1020, stack)
  
  // pop 20: stack = (stack - 20) / 100 = (1020 - 20) / 100 = 10
  stack = add(stack, -20) / 100
  assert_eq(10, stack)
}

test "financial_calculations_advanced" {
  // 高级金融计算：贷款分期计算
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率 6%
  let years = 30          // 贷款年限
  
  // 简化的月供计算：本金 × (1 + 年利率 × 年数) / (年数 × 12)
  let total_interest = multiply(principal, multiply(annual_rate, years))
  let total_payment = add(principal, total_interest)
  let monthly_payment = total_payment / multiply(years, 12)
  
  assert_eq(2777, monthly_payment)  // 向下取整
  
  // 计算总支付额
  let actual_total = multiply(monthly_payment, multiply(years, 12))
  assert_eq(1000000, actual_total)
}

test "string_processing_advanced" {
  // 高级字符串处理测试
  // 测试字符串反转的模拟（通过逐个字符处理）
  
  // 原始字符串
  let original = "hello"
  let reversed = "olleh"
  
  // 通过 greet 函数验证字符串处理
  let original_greeting = greet(original)
  let reversed_greeting = greet(reversed)
  
  assert_eq_string("Hello, hello!", original_greeting)
  assert_eq_string("Hello, olleh!", reversed_greeting)
  
  // 测试包含数字和特殊字符的字符串
  let complex_string = "abc123!@#"
  let complex_greeting = greet(complex_string)
  assert_eq_string("Hello, abc123!@#!", complex_greeting)
  
  // 测试 Unicode 字符串
  let unicode_string = "测试123"
  let unicode_greeting = greet(unicode_string)
  assert_eq_string("Hello, 测试123!", unicode_greeting)
}

test "mathematical_optimization" {
  // 数学优化测试：寻找最优解
  // 模拟简单的优化问题：在给定约束下找到最大值
  
  // 问题：在 x + y <= 100 的约束下，最大化 x * y
  // 最优解是 x = 50, y = 50，最大值为 2500
  
  let constraint = 100
  let x = 50
  let y = 50
  
  // 验证约束
  let sum = add(x, y)
  assert_true(sum <= constraint)
  
  // 计算目标函数值
  let product = multiply(x, y)
  assert_eq(2500, product)
  
  // 测试其他解
  let x2 = 30
  let y2 = 70
  let sum2 = add(x2, y2)
  let product2 = multiply(x2, y2)
  
  assert_true(sum2 <= constraint)
  assert_true(product2 < product)  // 2100 < 2500
}

test "error_recovery_mechanisms" {
  // 错误恢复机制测试
  let max_val = 2147483647
  let min_val = -2147483648
  
  // 测试溢出后的恢复机制
  let overflow_result = add(max_val, 100)
  assert_eq(max_val, overflow_result)  // 溢出保护
  
  // 从溢出状态恢复
  let recovered = add(overflow_result, -100)
  assert_eq(add(max_val, -100), recovered)
  
  // 测试下溢后的恢复机制
  let underflow_result = add(min_val, -100)
  assert_eq(min_val, underflow_result)  // 下溢保护
  
  // 从下溢状态恢复
  let recovered2 = add(underflow_result, 100)
  assert_eq(add(min_val, 100), recovered2)
}

test "performance_benchmark_simulation" {
  // 性能基准测试模拟
  // 模拟大数据量处理的性能测试
  
  let base_value = 1000
  let iterations = 1000
  
  // 模拟累加操作的性能测试
  let start_time = 100  // 模拟开始时间
  let end_time = 200    // 模拟结束时间
  let duration = add(end_time, -start_time)
  
  assert_eq(100, duration)
  
  // 计算平均处理时间
  let avg_time = duration / iterations
  assert_eq(0, avg_time)  // 整数除法，向下取整
  
  // 计算总处理量
  let total_processed = multiply(base_value, iterations)
  assert_eq(1000000, total_processed)
  
  // 计算吞吐量
  let throughput = total_processed / duration
  assert_eq(10000, throughput)
}

test "real_world_simulation" {
  // 真实世界场景模拟：库存管理系统
  let initial_stock = 1000
  let sales_per_day = 50
  let restock_amount = 200
  let restock_frequency = 3  // 每3天补货一次
  let simulation_days = 10
  
  // 模拟10天的库存变化
  let day1_stock = add(initial_stock, -sales_per_day)  // 第1天：1000 - 50 = 950
  let day2_stock = add(day1_stock, -sales_per_day)     // 第2天：950 - 50 = 900
  let day3_stock = add(day2_stock, restock_amount)     // 第3天：900 + 200 = 1100
  let day4_stock = add(day3_stock, -sales_per_day)     // 第4天：1100 - 50 = 1050
  let day5_stock = add(day4_stock, -sales_per_day)     // 第5天：1050 - 50 = 1000
  let day6_stock = add(day5_stock, restock_amount)     // 第6天：1000 + 200 = 1200
  let day7_stock = add(day6_stock, -sales_per_day)     // 第7天：1200 - 50 = 1150
  let day8_stock = add(day7_stock, -sales_per_day)     // 第8天：1150 - 50 = 1100
  let day9_stock = add(day8_stock, restock_amount)     // 第9天：1100 + 200 = 1300
  let day10_stock = add(day9_stock, -sales_per_day)    // 第10天：1300 - 50 = 1250
  
  assert_eq(950, day1_stock)
  assert_eq(900, day2_stock)
  assert_eq(1100, day3_stock)
  assert_eq(1050, day4_stock)
  assert_eq(1000, day5_stock)
  assert_eq(1200, day6_stock)
  assert_eq(1150, day7_stock)
  assert_eq(1100, day8_stock)
  assert_eq(1300, day9_stock)
  assert_eq(1250, day10_stock)
  
  // 计算总销售量
  let total_sales = multiply(sales_per_day, simulation_days)
  assert_eq(500, total_sales)
  
  // 计算总补货量
  let restock_times = simulation_days / restock_frequency
  let total_restocked = multiply(restock_amount, restock_times)
  assert_eq(600, total_restocked)
}

test "network_calculation_simulation" {
  // 网络计算模拟：数据包传输计算
  let packet_size = 1024        // 数据包大小（字节）
  let bandwidth = 1000000       // 带宽（字节/秒）
  let latency = 10              // 延迟（毫秒）
  let packet_count = 100        // 数据包数量
  
  // 计算传输时间
  let transmission_time = multiply(packet_size, packet_count) / bandwidth
  assert_eq(0, transmission_time)  // 整数除法
  
  // 计算总延迟
  let total_latency = multiply(latency, packet_count)
  assert_eq(1000, total_latency)
  
  // 计算总传输时间（包括延迟）
  let total_time = add(transmission_time, total_latency)
  assert_eq(1000, total_time)
  
  // 计算吞吐量
  let total_data = multiply(packet_size, packet_count)
  let throughput = total_data / total_time
  assert_eq(102, throughput)  // 向下取整
}

test "cryptography_simulation" {
  // 密码学模拟：简单的加密算法
  let plaintext = 12345
  let key = 54321
  
  // 简单的加密算法：加密 = (明文 + 密钥) mod 最大值
  let max_val = 2147483647
  let encrypted = add(plaintext, key)
  
  // 确保没有溢出
  assert_true(encrypted > plaintext)
  assert_true(encrypted > key)
  
  // 简单的解密算法：解密 = 加密 - 密钥
  let decrypted = add(encrypted, -key)
  assert_eq(plaintext, decrypted)
  
  // 测试更复杂的加密方式
  let key2 = 1000000
  let encrypted2 = add(multiply(plaintext, 2), key2)
  let decrypted2 = add(encrypted2, -key2) / 2
  assert_eq(plaintext, decrypted2)
}