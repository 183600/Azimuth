// 最终质量测试用例 - 针对 azimuth 库的综合测试

test "inventory_management" {
  // 库存管理测试
  let initial_stock = 150
  let daily_sales = 12
  let days_to_simulate = 7
  
  let total_sold = @azimuth.multiply(daily_sales, days_to_simulate)
  let final_stock = @azimuth.add(initial_stock, -total_sold)
  
  @azimuth.assert_eq(84, total_sold)
  @azimuth.assert_eq(66, final_stock)
}

test "project_timeline" {
  // 项目时间线计算
  let tasks_per_day = 5
  let total_tasks = 47
  
  let workdays_needed = @azimuth.divide_with_ceil(total_tasks, tasks_per_day)
  
  @azimuth.assert_eq(10, workdays_needed)
}

test "energy_calculation" {
  // 能源消耗计算
  let daily_consumption = 120
  let days_in_month = 30
  
  let monthly_consumption = @azimuth.multiply(daily_consumption, days_in_month)
  
  @azimuth.assert_eq(3600, monthly_consumption)
}

test "fibonacci_sequence" {
  // 斐波那契数列
  let fib1 = 1
  let fib2 = 1
  let fib3 = @azimuth.add(fib1, fib2)
  let fib4 = @azimuth.add(fib2, fib3)
  let fib5 = @azimuth.add(fib3, fib4)
  
  @azimuth.assert_eq(2, fib3)
  @azimuth.assert_eq(3, fib4)
  @azimuth.assert_eq(5, fib5)
}

test "shipping_cost" {
  // 运输成本计算
  let total_weight = 65
  let base_rate_per_kg = 5
  
  let base_cost = @azimuth.multiply(total_weight, base_rate_per_kg)
  
  @azimuth.assert_eq(325, base_cost)
}

test "data_compression" {
  // 数据压缩计算
  let original_size = 1000
  let compression_ratio = 70
  
  let compressed_size = @azimuth.multiply(original_size, compression_ratio) / 100
  
  @azimuth.assert_eq(700, compressed_size)
}

test "network_bandwidth" {
  // 网络带宽计算
  let file_size_mb = 850
  let network_speed_mbps = 50
  
  let transfer_time = @azimuth.divide_with_ceil(file_size_mb, network_speed_mbps)
  
  @azimuth.assert_eq(17, transfer_time)
}

test "educational_grading" {
  // 教育评分系统
  let exam_score = 87
  let assignment_score = 85
  
  let final_grade = @azimuth.add(exam_score, assignment_score) / 2
  
  @azimuth.assert_eq(86, final_grade)
}

test "agricultural_yield" {
  // 农业产量预测
  let field_area = 25
  let yield_per_hectare = 800
  
  let total_yield = @azimuth.multiply(field_area, yield_per_hectare)
  
  @azimuth.assert_eq(20000, total_yield)
}

test "hash_simulation" {
  // 哈希模拟
  let input_value = 12345
  let multiplier = 31
  
  let hash_value = @azimuth.multiply(input_value, multiplier) % 10000
  
  @azimuth.assert_eq(2695, hash_value)
}