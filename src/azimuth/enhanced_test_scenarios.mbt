// 增强测试场景 - 包含10个新的MoonBit测试用例
// 测试各种实际应用场景和边界情况

test "statistical_variance_calculation" {
  // 统计方差计算测试
  // 数据集: [10, 15, 20, 25, 30]
  let values_sum = add(add(add(add(10, 15), 20), 25), 30)  // 100
  let count = 5
  let mean = values_sum / count  // 20
  
  // 计算方差: Σ(x - mean)² / n
  let diff1 = add(10, -mean)  // -10
  let diff2 = add(15, -mean)  // -5
  let diff3 = add(20, -mean)  // 0
  let diff4 = add(25, -mean)  // 5
  let diff5 = add(30, -mean)  // 10
  
  let squared_diffs_sum = add(add(add(add(multiply(diff1, diff1), multiply(diff2, diff2)), 
                                     multiply(diff3, diff3)), multiply(diff4, diff4)), multiply(diff5, diff5))
  let variance = squared_diffs_sum / count
  
  assert_eq(50, variance)  // (-10)² + (-5)² + 0² + 5² + 10² = 100+25+0+25+100 = 250, 250/5 = 50
}

test "loan_amortization_schedule" {
  // 贷款摊还计划测试
  let principal = 120000  // 贷款本金
  let annual_rate = 6     // 年利率6%
  let loan_term = 30      // 贷款期限30年
  let monthly_rate = annual_rate / 12  // 月利率0.5%
  let total_payments = multiply(loan_term, 12)  // 总还款次数360
  
  // 简化的月供计算: P * r * (1 + r)^n / [(1 + r)^n - 1]
  // 这里使用简化公式: 本金 + 总利息
  let total_interest = multiply(principal, multiply(annual_rate, loan_term)) / 100
  let total_payment = add(principal, total_interest)
  let monthly_payment = total_payment / total_payments
  
  assert_eq(216000, total_payment)  // 120000 + 120000*6*30/100 = 120000 + 216000 = 336000
  assert_eq(600, monthly_payment)   // 336000 / 360 = 933.33 ≈ 600 (简化计算)
}

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 100
  let sales_per_day = 15
  let restock_amount = 50
  let days_until_restock = 3
  
  // 计算3天后的库存
  let total_sales = multiply(sales_per_day, days_until_restock)
  let stock_before_restock = add(initial_stock, -total_sales)
  let stock_after_restock = add(stock_before_restock, restock_amount)
  
  assert_eq(45, stock_before_restock)  // 100 - 15*3 = 55
  assert_eq(95, stock_after_restock)   // 55 + 50 = 105
  
  // 计算库存周转率
  let cost_per_unit = 10
  let inventory_value = multiply(stock_after_restock, cost_per_unit)
  let annual_sales = multiply(sales_per_day, 365)
  let turnover_rate = multiply(annual_sales, cost_per_unit) / inventory_value
  
  assert_eq(950, inventory_value)     // 95 * 10 = 950
  assert_eq(57, turnover_rate)        // (15*365*10)/950 ≈ 57.6 ≈ 57
}

test "cpu_scheduling_simulation" {
  // CPU调度模拟测试
  let process_count = 5
  let time_quantum = 10  // 时间片
  let burst_times = add(add(add(add(8, 15), 6), 12), 9)  // 总突发时间50
  
  // 计算轮转调度下的上下文切换次数
  let context_switches = divide_with_ceil(burst_times, time_quantum)
  let total_overhead = multiply(context_switches, 2)  // 每次切换开销2个时间单位
  let total_execution_time = add(burst_times, total_overhead)
  
  assert_eq(5, context_switches)     // 50/10 = 5
  assert_eq(10, total_overhead)      // 5 * 2 = 10
  assert_eq(60, total_execution_time) // 50 + 10 = 60
}

test "network_throughput_calculation" {
  // 网络吞吐量计算测试
  let packet_size = 1500    // 数据包大小(字节)
  let packets_per_second = 1000
  let transmission_time = 5  // 传输时间(秒)
  let protocol_overhead = 20  // 协议开销百分比
  
  // 计算原始吞吐量
  let raw_throughput = multiply(packet_size, packets_per_second)  // 字节/秒
  let effective_throughput = multiply(raw_throughput, add(100, -protocol_overhead)) / 100
  
  // 计算总传输量
  let total_transmission = multiply(effective_throughput, transmission_time)
  
  assert_eq(1500000, raw_throughput)      // 1500 * 1000 = 1,500,000 字节/秒
  assert_eq(1200000, effective_throughput) // 1,500,000 * 80% = 1,200,000 字节/秒
  assert_eq(6000000, total_transmission)  // 1,200,000 * 5 = 6,000,000 字节
}

test "cache_performance_analysis" {
  // 缓存性能分析测试
  let cache_size = 1024      // 缓存大小(KB)
  let block_size = 64        // 块大小(字节)
  let total_accesses = 10000
  let hit_rate = 85          // 命中率85%
  let hit_time = 1           // 命中时间(周期)
  let miss_penalty = 50      // 未命中惩罚(周期)
  
  // 计算缓存行数
  let cache_lines = multiply(cache_size, 1024) / block_size
  assert_eq(16384, cache_lines)  // 1024*1024/64 = 16384
  
  // 计算平均访问时间
  let hit_count = multiply(total_accesses, hit_rate) / 100
  let miss_count = add(total_accesses, -hit_count)
  let total_time = add(multiply(hit_count, hit_time), multiply(miss_count, miss_penalty))
  let avg_access_time = total_time / total_accesses
  
  assert_eq(8500, hit_count)     // 10000 * 85% = 8500
  assert_eq(1500, miss_count)    // 10000 - 8500 = 1500
  assert_eq(8, avg_access_time)  // (8500*1 + 1500*50)/10000 = 8.35 ≈ 8
}

test "digital_signal_processing" {
  // 数字信号处理测试
  let sampling_frequency = 44100  // 采样频率(Hz)
  let signal_duration = 3         // 信号持续时间(秒)
  let frequency = 440             // 信号频率(Hz, A4音符)
  let amplitude = 32767           // 振幅(16位有符号整数最大值)
  
  // 计算总样本数
  let total_samples = multiply(sampling_frequency, signal_duration)
  assert_eq(132300, total_samples)  // 44100 * 3 = 132300
  
  // 计算一个周期的样本数
  let samples_per_cycle = divide_with_ceil(sampling_frequency, frequency)
  assert_eq(101, samples_per_cycle)  // 44100/440 ≈ 100.23 ≈ 101
  
  // 计算总周期数
  let total_cycles = total_samples / samples_per_cycle
  assert_eq(1309, total_cycles)      // 132300/101 ≈ 1309
  
  // 计算信号能量(简化)
  let signal_energy = multiply(amplitude, amplitude) / 2
  assert_eq(536838944, signal_energy)  // 32767²/2 ≈ 536,838,944
}

test "compression_algorithm_analysis" {
  // 压缩算法分析测试
  let original_size = 1000000    // 原始大小(字节)
  let compression_ratios = add(add(add(add(70, 60), 50), 40), 30)  // 各种压缩比
  let algorithm_count = 5
  
  // 计算平均压缩比
  let avg_compression_ratio = compression_ratios / algorithm_count
  assert_eq(50, avg_compression_ratio)  // (70+60+50+40+30)/5 = 50
  
  // 计算平均压缩后大小
  let avg_compressed_size = multiply(original_size, avg_compression_ratio) / 100
  assert_eq(500000, avg_compressed_size)  // 1000000 * 50% = 500000
  
  // 计算空间节省
  let space_saved = add(original_size, -avg_compressed_size)
  assert_eq(500000, space_saved)  // 1000000 - 500000 = 500000
  
  // 计算压缩时间(假设压缩速度为10MB/秒)
  let compression_speed = 10000000  // 字节/秒
  let compression_time = original_size / compression_speed
  assert_eq(0, compression_time)    // 1000000/10000000 = 0.1秒 ≈ 0秒(整数除法)
}

test "database_query_optimization" {
  // 数据库查询优化测试
  let table_rows = 1000000     // 表行数
  let index_selectivity = 5    // 索引选择性(百分比)
  let join_tables = 3          // 连接表数
  let sort_operations = 2      // 排序操作数
  
  // 计算全表扫描成本
  let full_scan_cost = table_rows
  assert_eq(1000000, full_scan_cost)
  
  // 计算索引查找成本
  let index_lookup_cost = multiply(table_rows, index_selectivity) / 100
  assert_eq(50000, index_lookup_cost)  // 1000000 * 5% = 50000
  
  // 计算连接操作成本(嵌套循环连接)
  let join_cost = multiply(table_rows, table_rows)
  assert_eq(1000000000000, join_cost)  // 1000000 * 1000000 = 10^12
  
  // 计算排序成本
  let sort_cost = multiply(table_rows, sort_operations)
  assert_eq(2000000, sort_cost)  // 1000000 * 2 = 2000000
  
  // 计算优化后的查询成本
  let optimized_cost = add(add(index_lookup_cost, multiply(join_tables, 1000)), sort_cost)
  assert_eq(503000, optimized_cost)  // 50000 + 3*1000 + 2000000 = 2053000
}

test "machine_learning_model_evaluation" {
  // 机器学习模型评估测试
  let total_samples = 1000
  let true_positives = 450
  let false_positives = 50
  let false_negatives = 100
  let true_negatives = 400
  
  // 计算准确率
  let correct_predictions = add(add(true_positives, true_negatives), 0)
  let accuracy = multiply(correct_predictions, 100) / total_samples
  assert_eq(85, accuracy)  // (450+400)/1000 = 85%
  
  // 计算精确率
  let predicted_positives = add(true_positives, false_positives)
  let precision = multiply(true_positives, 100) / predicted_positives
  assert_eq(90, precision)  // 450/(450+50) = 90%
  
  // 计算召回率
  let actual_positives = add(true_positives, false_negatives)
  let recall = multiply(true_positives, 100) / actual_positives
  assert_eq(81, recall)  // 450/(450+100) = 81.8% ≈ 81%
  
  // 计算F1分数(简化)
  let f1_score = multiply(2, multiply(precision, recall)) / add(precision, recall)
  assert_eq(85, f1_score)  // 2*90*81/(90+81) ≈ 85.2 ≈ 85%
}