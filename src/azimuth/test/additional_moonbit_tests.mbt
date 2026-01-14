// 额外的 MoonBit 测试用例
// 覆盖各种计算场景和边界情况

test "recursive_calculation_pattern" {
  // 测试递归计算模式：斐波那契数列前几项
  // fib(0) = 0, fib(1) = 1, fib(2) = 1, fib(3) = 2, fib(4) = 3, fib(5) = 5
  
  // 手动计算前几项
  let fib_0 = 0
  let fib_1 = 1
  let fib_2 = @azimuth.add(fib_0, fib_1)  // 1
  let fib_3 = @azimuth.add(fib_1, fib_2)  // 2
  let fib_4 = @azimuth.add(fib_2, fib_3)  // 3
  let fib_5 = @azimuth.add(fib_3, fib_4)  // 5
  
  @azimuth.assert_eq(0, fib_0)
  @azimuth.assert_eq(1, fib_1)
  @azimuth.assert_eq(1, fib_2)
  @azimuth.assert_eq(2, fib_3)
  @azimuth.assert_eq(3, fib_4)
  @azimuth.assert_eq(5, fib_5)
  
  // 计算前6项的和
  let sum_fib = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(fib_0, fib_1), fib_2), fib_3), fib_4), fib_5)
  @azimuth.assert_eq(12, sum_fib)
}

test "arithmetic_series_calculation" {
  // 测试算术级数计算：1 + 4 + 7 + 10 + 13
  // 使用公式：n/2 * (2a + (n-1)d)
  let n = 5  // 项数
  let a = 1  // 首项
  let d = 3  // 公差
  
  // 手动计算各项
  let term1 = a  // 1
  let term2 = @azimuth.add(term1, d)  // 4
  let term3 = @azimuth.add(term2, d)  // 7
  let term4 = @azimuth.add(term3, d)  // 10
  let term5 = @azimuth.add(term4, d)  // 13
  
  // 计算总和
  let sum_manual = @azimuth.add(@azimuth.add(@azimuth.add(@azimuth.add(term1, term2), term3), term4), term5)
  @azimuth.assert_eq(35, sum_manual)
  
  // 使用公式验证
  let sum_formula = @azimuth.multiply(n, @azimuth.add(@azimuth.multiply(2, a), @azimuth.multiply(n - 1, d))) / 2
  @azimuth.assert_eq(35, sum_formula)
}

test "percentage_and_ratio_calculations" {
  // 测试百分比和比率计算
  let total = 800
  let part1 = 200
  let part2 = 400
  let part3 = 200
  
  // 计算各部分百分比
  let percent1 = @azimuth.multiply(part1, 100) / total
  let percent2 = @azimuth.multiply(part2, 100) / total
  let percent3 = @azimuth.multiply(part3, 100) / total
  
  @azimuth.assert_eq(25, percent1)  // 25%
  @azimuth.assert_eq(50, percent2)  // 50%
  @azimuth.assert_eq(25, percent3)  // 25%
  
  // 验证百分比总和
  let total_percent = @azimuth.add(@azimuth.add(percent1, percent2), percent3)
  @azimuth.assert_eq(100, total_percent)
  
  // 计算比率（part1 : part2 : part3）
  let gcd = 200  // 最大公约数
  let ratio1 = part1 / gcd
  let ratio2 = part2 / gcd
  let ratio3 = part3 / gcd
  
  @azimuth.assert_eq(1, ratio1)
  @azimuth.assert_eq(2, ratio2)
  @azimuth.assert_eq(1, ratio3)
}

test "physics_formula_calculations" {
  // 测试物理公式计算
  // 自由落体运动：h = 0.5 * g * t²
  // 简化为整数计算：h = 5 * t² (g ≈ 10 m/s²)
  
  let t1 = 2  // 2秒
  let t2 = 5  // 5秒
  
  // 计算下落距离
  let height1 = @azimuth.multiply(5, @azimuth.multiply(t1, t1))  // 5 * 4 = 20
  let height2 = @azimuth.multiply(5, @azimuth.multiply(t2, t2))  // 5 * 25 = 125
  
  @azimuth.assert_eq(20, height1)
  @azimuth.assert_eq(125, height2)
  
  // 计算速度：v = g * t
  let velocity1 = @azimuth.multiply(10, t1)  // 20
  let velocity2 = @azimuth.multiply(10, t2)  // 50
  
  @azimuth.assert_eq(20, velocity1)
  @azimuth.assert_eq(50, velocity2)
  
  // 动能计算：E = 0.5 * m * v²
  // 简化为：E = m * v² / 2
  let mass = 10  // 10kg
  let energy1 = @azimuth.multiply(mass, @azimuth.multiply(velocity1, velocity1)) / 2
  let energy2 = @azimuth.multiply(mass, @azimuth.multiply(velocity2, velocity2)) / 2
  
  @azimuth.assert_eq(2000, energy1)
  @azimuth.assert_eq(12500, energy2)
}

test "date_time_calculations" {
  // 测试日期时间计算
  // 计算两个时间点之间的差异
  let start_day = 15
  let start_month = 3  // 3月15日
  let end_day = 10
  let end_month = 4    // 4月10日
  
  // 假设每月30天（简化计算）
  let start_total = @azimuth.add(start_month * 30, start_day)  // 3*30 + 15 = 105
  let end_total = @azimuth.add(end_month * 30, end_day)        // 4*30 + 10 = 130
  
  let days_diff = @azimuth.add(end_total, -start_total)  // 25天
  @azimuth.assert_eq(25, days_diff)
  
  // 计算工作日（假设每周5个工作日）
  let weeks = days_diff / 7  // 3周
  let remaining_days = days_diff % 7  // 4天
  let workdays = @azimuth.add(@azimuth.multiply(weeks, 5), @azimuth.min(remaining_days, 5))
  @azimuth.assert_eq(19, workdays)
  
  // 计算小时数
  let total_hours = @azimuth.multiply(days_diff, 24)
  @azimuth.assert_eq(600, total_hours)
}

test "data_validation_scenarios" {
  // 测试数据验证场景
  // 验证身份证号码（简化版）：18位数字
  let id_length = 18
  let valid_digits = 17  // 前17位是数字
  let last_digit = 1     // 最后一位是校验码
  
  // 模拟验证过程
  let is_valid_length = id_length == 18
  let has_valid_digits = valid_digits == 17
  let has_valid_checksum = last_digit >= 0 && last_digit <= 9
  
  @azimuth.assert_true(is_valid_length)
  @azimuth.assert_true(has_valid_digits)
  @azimuth.assert_true(has_valid_checksum)
  
  // 验证邮箱格式（简化版）
  let email = "user@example.com"
  let has_at = 1  // 假设包含@符号
  let has_dot = 1  // 假设包含.符号
  let length_valid = email.length() > 5  // 假设长度检查
  
  @azimuth.assert_true(has_at > 0)
  @azimuth.assert_true(has_dot > 0)
  @azimuth.assert_true(length_valid)
}

test "algorithm_complexity_scenarios" {
  // 测试算法复杂度场景
  // 线性搜索：O(n)
  let array_size = 100
  let target_position = 75
  
  // 模拟线性搜索的比较次数
  let linear_comparisons = target_position
  @azimuth.assert_eq(75, linear_comparisons)
  
  // 二分搜索：O(log n)
  // 模拟二分搜索的比较次数
  let binary_comparisons = 7  // log2(100) ≈ 6.64，向上取整为7
  @azimuth.assert_eq(7, binary_comparisons)
  
  // 冒泡排序：O(n²)
  let bubble_comparisons = @azimuth.multiply(array_size, array_size - 1) / 2
  @azimuth.assert_eq(4950, bubble_comparisons)
  
  // 快速排序平均情况：O(n log n)
  let quick_comparisons = @azimuth.multiply(array_size, 7)  // 100 * log2(100)
  @azimuth.assert_eq(700, quick_comparisons)
}

test "boundary_combination_scenarios" {
  // 测试边界组合场景
  let max_val = 2147483647
  let min_val = -2147483648
  let mid_val = 0
  
  // 测试三值比较
  let values = [max_val, mid_val, min_val]
  
  // 找出最大值
  let max_found = @azimuth.max(@azimuth.max(values[0], values[1]), values[2])
  @azimuth.assert_eq(max_val, max_found)
  
  // 找出最小值
  let min_found = @azimuth.min(@azimuth.min(values[0], values[1]), values[2])
  @azimuth.assert_eq(min_val, min_found)
  
  // 计算范围
  let range = @azimuth.add(max_found, -min_found)
  @azimuth.assert_eq(@azimuth.add(max_val, -min_val), range)
  
  // 测试边界值附近的运算
  let near_max = @azimuth.add(max_val, -1000)
  let near_min = @azimuth.add(min_val, 1000)
  
  @azimuth.assert_eq(2147482647, near_max)
  @azimuth.assert_eq(-2147482648, near_min)
  
  // 测试边界值的混合运算
  let mixed_result = @azimuth.add(@azimuth.multiply(max_val, 0), @azimuth.add(min_val, max_val))
  @azimuth.assert_eq(-1, mixed_result)
}

test "business_inventory_calculation" {
  // 测试业务库存计算
  let initial_stock = 1000
  let incoming_shipment = 500
  let sales_today = 150
  let returns_today = 20
  let damaged_items = 5
  
  // 计算当日库存变化
  let net_change = @azimuth.add(@azimuth.add(incoming_shipment, -sales_today), returns_today)
  let final_stock = @azimuth.add(initial_stock, @azimuth.add(net_change, -damaged_items))
  
  @azimuth.assert_eq(1365, final_stock)
  
  // 计算库存周转率（简化）
  let monthly_sales = 4500
  let average_stock = 1200
  let turnover_rate = monthly_sales / average_stock
  
  @azimuth.assert_eq(3, turnover_rate)
  
  // 计算补货点
  let daily_sales = 150
  let lead_time = 7  // 天
  let safety_stock = 200
  let reorder_point = @azimuth.add(@azimuth.multiply(daily_sales, lead_time), safety_stock)
  
  @azimuth.assert_eq(1250, reorder_point)
  
  // 判断是否需要补货
  let needs_reorder = final_stock <= reorder_point
  @azimuth.assert_false(needs_reorder)  // 1365 > 1250，不需要补货
}

test "performance_benchmark_calculations" {
  // 测试性能基准计算
  let base_operations = 1000
  let optimization_factor = 2
  let cache_hit_rate = 80  // 80%
  
  // 计算优化后的性能
  let optimized_operations = @azimuth.multiply(base_operations, optimization_factor)
  @azimuth.assert_eq(2000, optimized_operations)
  
  // 计算缓存效果
  let cache_improvement = @azimuth.multiply(optimized_operations, cache_hit_rate) / 100
  let effective_operations = @azimuth.add(optimized_operations, cache_improvement)
  
  @azimuth.assert_eq(3600, effective_operations)
  
  // 计算性能提升百分比
  let improvement_percent = @azimuth.multiply(@azimuth.add(effective_operations, -base_operations), 100) / base_operations
  @azimuth.assert_eq(260, improvement_percent)
  
  // 计算响应时间（简化）
  let base_response_time = 100  // 毫秒
  let improved_response_time = base_response_time * 100 / effective_operations
  @azimuth.assert_eq(27, improved_response_time)  // 整数除法
  
  // 计算吞吐量
  let time_window = 1000  // 1秒 = 1000毫秒
  let throughput = @azimuth.multiply(effective_operations, time_window) / base_response_time
  @azimuth.assert_eq(36000, throughput)
}