// 高级MoonBit测试用例
// 专注于复杂算法、性能测试和实际应用场景

test "fibonacci_sequence_calculation" {
  // 使用add函数计算斐波那契数列的第10项
  // F(0)=0, F(1)=1, F(2)=1, F(3)=2, F(4)=3, F(5)=5, F(6)=8, F(7)=13, F(8)=21, F(9)=34, F(10)=55
  let f0 = 0
  let f1 = 1
  let f2 = @azimuth.add(f0, f1)  // 1
  let f3 = @azimuth.add(f1, f2)  // 2
  let f4 = @azimuth.add(f2, f3)  // 3
  let f5 = @azimuth.add(f3, f4)  // 5
  let f6 = @azimuth.add(f4, f5)  // 8
  let f7 = @azimuth.add(f5, f6)  // 13
  let f8 = @azimuth.add(f6, f7)  // 21
  let f9 = @azimuth.add(f7, f8)  // 34
  let f10 = @azimuth.add(f8, f9) // 55
  
  @azimuth.assert_eq(55, f10)
}

test "prime_number_verification" {
  // 使用基本运算验证质数性质
  // 验证17是质数：不能被2,3,4,5整除
  let number = 17
  let is_divisible_by_2 = @azimuth.multiply(number / 2, 2) == number
  let is_divisible_by_3 = @azimuth.multiply(number / 3, 3) == number
  let is_divisible_by_4 = @azimuth.multiply(number / 4, 4) == number
  let is_divisible_by_5 = @azimuth.multiply(number / 5, 5) == number
  
  // 17不应该被2,3,4,5整除
  @azimuth.assert_eq(0, 1)(is_divisible_by_2)
  @azimuth.assert_eq(0, 1)(is_divisible_by_3)
  @azimuth.assert_eq(0, 1)(is_divisible_by_4)
  @azimuth.assert_eq(0, 1)(is_divisible_by_5)
  
  // 验证12不是质数：能被2,3,4整除
  let composite = 12
  let is_divisible_by_2_12 = @azimuth.multiply(composite / 2, 2) == composite
  let is_divisible_by_3_12 = @azimuth.multiply(composite / 3, 3) == composite
  let is_divisible_by_4_12 = @azimuth.multiply(composite / 4, 4) == composite
  
  @azimuth.assert_eq(is_divisible_by_2_12, true)
  @azimuth.assert_eq(is_divisible_by_3_12, true)
  @azimuth.assert_eq(is_divisible_by_4_12, true)
}

test "bank_interest_compound_calculation" {
  // 银行复利计算：本金 × (1 + 利率)^年数
  // 简化计算：10000 × (1 + 5%)^3 ≈ 10000 × 116 = 1160000
  let principal = 10000
  let rate_percent = 5
  let years = 3
  
  // 第一年：10000 × 105 = 1050000
  let year1_end = @azimuth.multiply(principal, @azimuth.add(100, rate_percent))
  
  // 第二年：10500 × 105 = 1102500
  let year2_end = @azimuth.multiply(year1_end / 100, @azimuth.add(100, rate_percent))
  
  // 第三年：11025 × 105 = 1157625
  let year3_end = @azimuth.multiply(year2_end / 100, @azimuth.add(100, rate_percent))
  
  @azimuth.assert_eq(1157625, year3_end)
}

test "inventory_management_system" {
  // 库存管理系统测试
  let initial_stock = 100
  let sold_items = 25
  let restocked_items = 50
  let damaged_items = 5
  
  // 计算当前库存：初始库存 - 售出 + 补货 - 损坏
  let current_stock = @azimuth.add(
    @azimuth.add(
      @azimuth.add(initial_stock, -sold_items), 
      restocked_items
    ), 
    -damaged_items
  )
  
  @azimuth.assert_eq(120, current_stock)
  
  // 计算库存价值：当前库存 × 单价
  let unit_price = 50
  let inventory_value = @azimuth.multiply(current_stock, unit_price)
  
  @azimuth.assert_eq(6000, inventory_value)
}

test "data_compression_simulation" {
  // 数据压缩模拟：计算压缩率和存储空间
  let original_size = 1000000  // 1MB
  let compression_ratio = 75   // 75%压缩率
  
  // 计算压缩后大小
  let compressed_size = @azimuth.multiply(original_size, compression_ratio) / 100
  
  @azimuth.assert_eq(750000, compressed_size)
  
  // 计算节省空间
  let saved_space = @azimuth.add(original_size, -compressed_size)
  
  @azimuth.assert_eq(250000, saved_space)
  
  // 如果有10个这样的文件，计算总节省空间
  let file_count = 10
  let total_saved = @azimuth.multiply(saved_space, file_count)
  
  @azimuth.assert_eq(2500000, total_saved)
}

test "network_throughput_calculation" {
  // 网络吞吐量计算
  let file_size_mb = 100
  let network_speed_mbps = 20
  let overhead_percent = 10
  
  // 计算理论传输时间（秒）
  let theoretical_time = @azimuth.multiply(file_size_mb, 8) / network_speed_mbps
  
  @azimuth.assert_eq(40, theoretical_time)
  
  // 考虑开销的实际时间
  let actual_time = @azimuth.multiply(theoretical_time, @azimuth.add(100, overhead_percent)) / 100
  
  @azimuth.assert_eq(44, actual_time)
}

test "geometric_progression_sum" {
  // 等比数列求和：1 + 2 + 4 + 8 + 16 + 32 = 63
  // 使用公式：a × (r^n - 1) / (r - 1)，其中a=1, r=2, n=6
  let first_term = 1
  let ratio = 2
  let terms = 6
  
  // 计算2^6 - 1 = 64 - 1 = 63
  let power_of_2 = @azimuth.multiply(@azimuth.multiply(@azimuth.multiply(2, 2), 2), 2)  // 2^4 = 16
  let power_of_6 = @azimuth.multiply(power_of_2, @azimuth.multiply(2, 2))  // 16 * 4 = 64
  let numerator = @azimuth.add(power_of_6, -1)  // 64 - 1 = 63
  let denominator = @azimuth.add(ratio, -1)     // 2 - 1 = 1
  let sum = @azimuth.multiply(first_term, numerator) / denominator
  
  @azimuth.assert_eq(63, sum)
  
  // 手动验证
  let term1 = 1
  let term2 = @azimuth.multiply(term1, ratio)     // 2
  let term3 = @azimuth.multiply(term2, ratio)     // 4
  let term4 = @azimuth.multiply(term3, ratio)     // 8
  let term5 = @azimuth.multiply(term4, ratio)     // 16
  let term6 = @azimuth.multiply(term5, ratio)     // 32
  let manual_sum = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5), term6)
  
  @azimuth.assert_eq(63, manual_sum)
}

test "cpu_scheduling_simulation" {
  // CPU调度模拟：时间片轮转算法
  let process1_time = 15
  let process2_time = 20
  let process3_time = 10
  let time_slice = 5
  
  // 计算每个进程需要的时间片数
  let process1_slices = @azimuth.add(process1_time / time_slice, 
    if (process1_time % time_slice > 0) { 1 } else { 0 })
  let process2_slices = @azimuth.add(process2_time / time_slice, 
    if (process2_time % time_slice > 0) { 1 } else { 0 })
  let process3_slices = @azimuth.add(process3_time / time_slice, 
    if (process3_time % time_slice > 0) { 1 } else { 0 })
  
  @azimuth.assert_eq(3, process1_slices)
  @azimuth.assert_eq(4, process2_slices)
  @azimuth.assert_eq(2, process3_slices)
  
  // 计算总时间片数
  let total_slices = @azimuth.add(@azimuth.add(process1_slices, process2_slices), process3_slices)
  
  @azimuth.assert_eq(9, total_slices)
  
  // 计算总执行时间（包括上下文切换开销）
  let context_switch_overhead = 1
  let total_time = @azimuth.add(
    @azimuth.add(@azimuth.add(process1_time, process2_time), process3_time),
    @azimuth.multiply(total_slices, context_switch_overhead)
  )
  
  @azimuth.assert_eq(55, total_time)
}

test "string_processing_efficiency" {
  // 字符串处理效率测试
  let base_string = "Hello"
  let append_count = 100
  
  // 模拟字符串拼接操作（通过greet函数）
  let result1 = @azimuth.greet(base_string)
  let result2 = @azimuth.greet(base_string + base_string)
  let result3 = @azimuth.greet(base_string + base_string + base_string)
  
  @azimuth.assert_eq_string("Hello, Hello!", result1)
  @azimuth.assert_eq_string("Hello, HelloHello!", result2)
  @azimuth.assert_eq_string("Hello, HelloHelloHello!", result3)
  
  // 测试不同长度字符串的处理
  let short_name = "A"
  let medium_name = "Alexander"
  let long_name = "ThisIsAVeryLongNameForTestingStringProcessingEfficiency"
  
  let short_result = @azimuth.greet(short_name)
  let medium_result = @azimuth.greet(medium_name)
  let long_result = @azimuth.greet(long_name)
  
  // 验证结果正确性
  @azimuth.assert_eq_string("Hello, A!", short_result)
  @azimuth.assert_eq_string("Hello, Alexander!", medium_result)
  @azimuth.assert_eq_string("Hello, ThisIsAVeryLongNameForTestingStringProcessingEfficiency!", long_result)
}

test "algorithm_complexity_simulation" {
  // 算法复杂度模拟：比较O(n)和O(n²)的运算次数
  let dataset_size = 100
  
  // O(n)算法的运算次数
  let linear_operations = dataset_size
  
  // O(n²)算法的运算次数
  let quadratic_operations = @azimuth.multiply(dataset_size, dataset_size)
  
  @azimuth.assert_eq(100, linear_operations)
  @azimuth.assert_eq(10000, quadratic_operations)
  
  // 计算性能差异倍数
  let performance_difference = quadratic_operations / linear_operations
  
  @azimuth.assert_eq(100, performance_difference)
  
  // 对于更大的数据集
  let large_dataset = 1000
  let large_linear = large_dataset
  let large_quadratic = @azimuth.multiply(large_dataset, large_dataset)
  let large_performance_diff = large_quadratic / large_linear
  
  @azimuth.assert_eq(1000, large_performance_diff)
}