// 增强的 MoonBit 测试用例
// 包含各种实际应用场景和边界情况测试

test "fibonacci_sequence_calculation" {
  // 计算斐波那契数列的前几项
  // F(0) = 0, F(1) = 1, F(n) = F(n-1) + F(n-2)
  
  // 手动计算前几项
  let f0 = 0
  let f1 = 1
  let f2 = f0 + f1  // 1
  let f3 = f1 + f2  // 2
  let f4 = f2 + f3  // 3
  let f5 = f3 + f4  // 5
  let f6 = f4 + f5  // 8
  let f7 = f5 + f6  // 13
  let f8 = f6 + f7  // 21
  let f9 = f7 + f8  // 34
  let f10 = f8 + f9 // 55
  
  // 使用简单的断言方式
  if (f2 != 1) { @builtin.abort("Test failed") }
  if (f3 != 2) { @builtin.abort("Test failed") }
  if (f4 != 3) { @builtin.abort("Test failed") }
  if (f5 != 5) { @builtin.abort("Test failed") }
  if (f6 != 8) { @builtin.abort("Test failed") }
  if (f7 != 13) { @builtin.abort("Test failed") }
  if (f8 != 21) { @builtin.abort("Test failed") }
  if (f9 != 34) { @builtin.abort("Test failed") }
  if (f10 != 55) { @builtin.abort("Test failed") }
}

test "prime_number_validation" {
  // 验证质数性质（通过模拟）
  // 检查一个数是否能被小于它的数整除
  
  let num = 17
  let is_prime = true
  
  // 模拟质数检查：检查是否能被2, 3, 4, 5整除
  let divisible_by_2 = (num % 2 == 0)
  let divisible_by_3 = (num % 3 == 0)
  let divisible_by_4 = (num % 4 == 0)
  let divisible_by_5 = (num % 5 == 0)
  
  // 17不能被2, 3, 4, 5整除，所以是质数
  if (divisible_by_2 != false) { @builtin.abort("Test failed") }
  if (divisible_by_3 != false) { @builtin.abort("Test failed") }
  if (divisible_by_4 != false) { @builtin.abort("Test failed") }
  if (divisible_by_5 != false) { @builtin.abort("Test failed") }
  
  // 测试非质数
  let composite_num = 15
  let divisible_by_3_15 = (composite_num % 3 == 0)
  let divisible_by_5_15 = (composite_num % 5 == 0)
  
  if (divisible_by_3_15 != true) { @builtin.abort("Test failed") }
  if (divisible_by_5_15 != true) { @builtin.abort("Test failed") }
}

test "loan_amortization_calculation" {
  // 贷款分期计算模拟
  let principal = 100000  // 本金
  let annual_rate = 6     // 年利率6%
  let years = 30          // 贷款年限
  let months = years * 12  // 总月数
  
  // 简化的月利率计算
  let monthly_rate = annual_rate / 12  // 0.5%（简化为整数）
  
  // 简化的月供计算：本金 / 月数 + 利息
  let monthly_principal = principal / months
  let first_month_interest = principal * monthly_rate / 100
  let first_month_payment = monthly_principal + first_month_interest
  
  if (monthly_principal != 277) { @builtin.abort("Test failed") }  // 100000 / 360 ≈ 277
  if (first_month_interest != 500) { @builtin.abort("Test failed") }  // 100000 * 0.5 / 100 = 500
  if (first_month_payment != 777) { @builtin.abort("Test failed") }   // 277 + 500 = 777
}

test "data_compression_simulation" {
  // 数据压缩模拟（简化版）
  let original_data = "AAAAABBBCCCDDE"
  
  // 模拟游程编码：A5B3C3D2E1
  let compressed_size = 10  // 压缩后大小
  let original_size = 15    // 原始大小
  
  // 计算压缩率
  let compression_ratio = compressed_size * 100 / original_size
  let space_saved = 100 - compression_ratio
  
  if (compression_ratio != 66) { @builtin.abort("Test failed") }  // 10*100/15 ≈ 66
  if (space_saved != 34) { @builtin.abort("Test failed") }        // 100-66 = 34
}

test "cache_performance_simulation" {
  // 缓存性能模拟
  let total_requests = 1000
  let cache_hit_rate = 80  // 80%命中率
  let cache_access_time = 1   // 1ms
  let memory_access_time = 10 // 10ms
  
  // 计算缓存命中数
  let cache_hits = total_requests * cache_hit_rate / 100
  let cache_misses = total_requests - cache_hits
  
  // 计算总访问时间
  let cache_time = cache_hits * cache_access_time
  let memory_time = cache_misses * memory_access_time
  let total_time = cache_time + memory_time
  
  if (cache_hits != 800) { @builtin.abort("Test failed") }
  if (cache_misses != 200) { @builtin.abort("Test failed") }
  if (cache_time != 800) { @builtin.abort("Test failed") }
  if (memory_time != 2000) { @builtin.abort("Test failed") }
  if (total_time != 2800) { @builtin.abort("Test failed") }
}

test "network_bandwidth_calculation" {
  // 网络带宽计算
  let file_size_mb = 100    // 100MB文件
  let bandwidth_mbps = 10   // 10Mbps带宽
  
  // 计算传输时间（秒）
  let file_size_mbits = file_size_mb * 8  // 转换为Mbit
  let transfer_time_seconds = file_size_mbits / bandwidth_mbps
  
  // 转换为分钟和秒
  let minutes = transfer_time_seconds / 60
  let seconds = transfer_time_seconds % 60
  
  if (file_size_mbits != 800) { @builtin.abort("Test failed") }  // 100 * 8 = 800
  if (transfer_time_seconds != 80) { @builtin.abort("Test failed") }  // 800 / 10 = 80秒
  if (minutes != 1) { @builtin.abort("Test failed") }  // 80 / 60 = 1分钟
  if (seconds != 20) { @builtin.abort("Test failed") } // 80 % 60 = 20秒
}

test "inventory_management" {
  // 库存管理计算
  let initial_stock = 1000
  let daily_sales = 50
  let restock_amount = 300
  let reorder_threshold = 200
  
  // 模拟5天的库存变化
  let day1_stock = initial_stock - daily_sales  // 950
  let day2_stock = day1_stock - daily_sales    // 900
  let day3_stock = day2_stock - daily_sales    // 850
  let day4_stock = day3_stock - daily_sales    // 800
  let day5_stock = day4_stock - daily_sales    // 750
  
  // 检查是否需要补货
  let needs_reorder = day5_stock < reorder_threshold
  let after_restock = if needs_reorder { 
    day5_stock + restock_amount 
  } else { 
    day5_stock 
  }
  
  if (day5_stock != 750) { @builtin.abort("Test failed") }
  if (needs_reorder != false) { @builtin.abort("Test failed") }  // 750 > 200，不需要补货
  if (after_restock != 750) { @builtin.abort("Test failed") }
}

test "cryptographic_hash_simulation" {
  // 加密哈希模拟（简化版）
  let input_data = "Hello"
  let hash_prime = 31
  let hash_result = 1
  
  // 模拟简单的哈希计算：hash = hash * prime + char_code
  let h_char = hash_result * hash_prime  // 31
  let e_char = h_char * hash_prime       // 31*31
  let l1_char = e_char * hash_prime      // 31*31*31
  let l2_char = l1_char * hash_prime     // 31*31*31*31
  let o_char = l2_char * hash_prime      // 31*31*31*31*31
  
  // 简化的哈希值计算
  let simplified_hash = l2_char + 100  // 简化
  
  if (h_char != 31) { @builtin.abort("Test failed") }
  if (e_char != 961) { @builtin.abort("Test failed") }  // 31*31
  if (simplified_hash <= 1000000) { @builtin.abort("Test failed") }  // 确保哈希值足够大
}

test "machine_learning_prediction" {
  // 机器学习预测模拟（线性回归）
  let feature1 = 2
  let feature2 = 3
  let feature3 = 1
  let weight1 = 5
  let weight2 = -2
  let weight3 = 1
  let bias = 10
  
  // 计算预测值：prediction = w1*x1 + w2*x2 + w3*x3 + bias
  let term1 = feature1 * weight1  // 2*5 = 10
  let term2 = feature2 * weight2  // 3*(-2) = -6
  let term3 = feature3 * weight3  // 1*1 = 1
  
  let prediction = term1 + term2 + term3 + bias
  if (prediction != 15) { @builtin.abort("Test failed") }  // 10 + (-6) + 1 + 10 = 15
  
  // 计算预测误差
  let actual_value = 18
  let error = actual_value - prediction
  if (error != 3) { @builtin.abort("Test failed") }
}

test "database_query_optimization" {
  // 数据库查询优化模拟
  let table_size = 1000000
  let selectivity_percent = 10  // 10%的选择率
  let index_scan_cost = 2
  let full_scan_cost = 100
  
  // 计算索引扫描成本
  let selected_rows = table_size * selectivity_percent / 100
  let index_scan_total = selected_rows * index_scan_cost
  
  // 计算全表扫描成本
  let full_scan_total = table_size * full_scan_cost
  
  // 选择最优扫描方式
  let use_index_scan = index_scan_total < full_scan_total
  let optimal_cost = if use_index_scan { index_scan_total } else { full_scan_total }
  
  if (selected_rows != 100000) { @builtin.abort("Test failed") }
  if (index_scan_total != 200000) { @builtin.abort("Test failed") }
  if (full_scan_total != 100000000) { @builtin.abort("Test failed") }
  if (use_index_scan != true) { @builtin.abort("Test failed") }
  if (optimal_cost != 200000) { @builtin.abort("Test failed") }
}