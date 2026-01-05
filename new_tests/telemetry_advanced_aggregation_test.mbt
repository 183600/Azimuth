// 遥测高级数据聚合测试用例

test "telemetry_advanced_time_weighted_aggregation" {
  // 测试遥测时间加权聚合算法
  
  let time_series_data = [
    (1640995200L, 10.5),  // 时间戳, 值
    (1640995260L, 15.2),
    (1640995320L, 12.8),
    (1640995380L, 18.3),
    (1640995440L, 14.7)
  ]
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 5)
  assert_eq(time_series_data[0].0, 1640995200L)
  assert_eq(time_series_data[4].1, 14.7)
  
  // 计算时间间隔
  let mut time_intervals = []
  let mut i = 1
  while i < time_series_data.length() {
    let interval = time_series_data[i].0 - time_series_data[i-1].0
    time_intervals.push(interval)
    i = i + 1
  }
  
  // 验证时间间隔
  assert_eq(time_intervals.length(), 4)
  assert_eq(time_intervals[0], 60L)
  assert_eq(time_intervals[3], 60L)
  
  // 计算时间加权平均值
  let mut weighted_sum = 0.0
  let mut total_weight = 0.0
  i = 0
  while i < time_series_data.length() - 1 {
    let current_value = time_series_data[i].1
    let next_value = time_series_data[i + 1].1
    let time_weight = time_intervals[i].to_string().to_double()
    
    // 使用梯形法则计算加权面积
    let weighted_area = (current_value + next_value) * time_weight / 2.0
    weighted_sum = weighted_sum + weighted_area
    total_weight = total_weight + time_weight
    
    i = i + 1
  }
  
  let time_weighted_average = weighted_sum / total_weight
  
  // 验证时间加权平均值
  assert_eq(time_weighted_average > 12.0, true)
  assert_eq(time_weighted_average < 16.0, true)
  
  // 计算简单平均值进行比较
  let mut simple_sum = 0.0
  i = 0
  while i < time_series_data.length() {
    simple_sum = simple_sum + time_series_data[i].1
    i = i + 1
  }
  let simple_average = simple_sum / time_series_data.length().to_string().to_double()
  
  // 验证加权平均值与简单平均值的差异
  let average_difference = (time_weighted_average - simple_average).abs()
  assert_eq(average_difference < 2.0, true)
}

test "telemetry_percentile_aggregation" {
  // 测试遥测百分位数聚合
  
  let response_times = [
    120, 85, 200, 150, 95, 180, 110, 165, 75, 195,
    135, 160, 125, 145, 130, 155, 140, 170, 115, 175,
    105, 185, 100, 190, 80
  ]
  
  // 验证响应时间数据
  assert_eq(response_times.length(), 25)
  assert_eq(response_times[0], 120)
  assert_eq(response_times[24], 80)
  
  // 排序数据（简化冒泡排序）
  let mut sorted_times = response_times
  let mut i = 0
  while i < sorted_times.length() - 1 {
    let mut j = 0
    while j < sorted_times.length() - i - 1 {
      if sorted_times[j] > sorted_times[j + 1] {
        let temp = sorted_times[j]
        sorted_times[j] = sorted_times[j + 1]
        sorted_times[j + 1] = temp
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证排序结果
  assert_eq(sorted_times[0], 75)    // 最小值
  assert_eq(sorted_times[24], 200)  // 最大值
  
  // 计算百分位数
  let percentiles = [50, 90, 95, 99]
  let mut percentile_values = []
  
  i = 0
  while i < percentiles.length() {
    let percentile = percentiles[i]
    let index = (sorted_times.length() * percentile) / 100
    
    // 边界检查
    let adjusted_index = if index >= sorted_times.length() {
      sorted_times.length() - 1
    } else {
      index
    }
    
    percentile_values.push(sorted_times[adjusted_index])
    i = i + 1
  }
  
  // 验证百分位数计算
  assert_eq(percentile_values.length(), 4)
  assert_eq(percentile_values[0], sorted_times[12])  // P50 (中位数)
  assert_eq(percentile_values[1], sorted_times[22])  // P90
  assert_eq(percentile_values[2], sorted_times[23])  // P95
  assert_eq(percentile_values[3], sorted_times[24])  // P99
  
  // 验证百分位数的单调性
  assert_eq(percentile_values[0] <= percentile_values[1], true)
  assert_eq(percentile_values[1] <= percentile_values[2], true)
  assert_eq(percentile_values[2] <= percentile_values[3], true)
  
  // 计算统计特征
  let median = percentile_values[0]
  let p95 = percentile_values[2]
  let range = sorted_times[24] - sorted_times[0]
  let iqr = sorted_times[18] - sorted_times[6]  // Q3 - Q1
  
  // 验证统计特征
  assert_eq(median > 100, true)
  assert_eq(median < 160, true)
  assert_eq(p95 > 180, true)
  assert_eq(range, 125)  // 200 - 75
  assert_eq(iqr > 40, true)
}

test "telemetry_histogram_aggregation" {
  // 测试遥测直方图聚合
  
  let measurements = [
    12.5, 18.3, 25.7, 8.2, 15.6, 22.1, 9.8, 19.4, 13.7, 21.3,
    16.9, 11.4, 24.8, 7.6, 17.2, 14.1, 20.5, 10.9, 23.6, 26.4
  ]
  
  // 定义直方图桶边界
  let bucket_boundaries = [0.0, 10.0, 15.0, 20.0, 25.0, 30.0]
  
  // 验证测量数据
  assert_eq(measurements.length(), 20)
  assert_eq(measurements[0], 12.5)
  assert_eq(measurements[19], 26.4)
  
  // 验证桶边界
  assert_eq(bucket_boundaries.length(), 6)
  assert_eq(bucket_boundaries[0], 0.0)
  assert_eq(bucket_boundaries[5], 30.0)
  
  // 初始化直方图计数
  let mut bucket_counts = []
  let mut i = 0
  while i < bucket_boundaries.length() - 1 {
    bucket_counts.push(0)
    i = i + 1
  }
  
  // 填充直方图
  i = 0
  while i < measurements.length() {
    let value = measurements[i]
    let mut j = 0
    
    // 找到合适的桶
    while j < bucket_boundaries.length() - 1 {
      if value >= bucket_boundaries[j] && value < bucket_boundaries[j + 1] {
        bucket_counts[j] = bucket_counts[j] + 1
        break
      } else if j == bucket_boundaries.length() - 2 && value >= bucket_boundaries[j + 1] {
        // 最后一个桶（包含右边界）
        bucket_counts[j] = bucket_counts[j] + 1
        break
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证直方图计数
  assert_eq(bucket_counts.length(), 5)
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, measurements.length())
  
  // 验证桶分布的合理性
  assert_eq(bucket_counts[0] > 0, true)  // 0-10范围
  assert_eq(bucket_counts[1] > 0, true)  // 10-15范围
  assert_eq(bucket_counts[2] > 0, true)  // 15-20范围
  assert_eq(bucket_counts[3] > 0, true)  // 20-25范围
  assert_eq(bucket_counts[4] > 0, true)  // 25-30范围
  
  // 计算直方图统计
  let mut cumulative_count = 0
  let mut cumulative_percentages = []
  i = 0
  while i < bucket_counts.length() {
    cumulative_count = cumulative_count + bucket_counts[i]
    let cumulative_percentage = (cumulative_count * 100) / measurements.length()
    cumulative_percentages.push(cumulative_percentage)
    i = i + 1
  }
  
  // 验证累积百分比
  assert_eq(cumulative_percentages.length(), 5)
  assert_eq(cumulative_percentages[4], 100)  // 最后一个桶应该是100%
  assert_eq(cumulative_percentages[0] < cumulative_percentages[1], true)
  assert_eq(cumulative_percentages[3] < cumulative_percentages[4], true)
}

test "telemetry_exponential_moving_average" {
  // 测试遥测指数移动平均
  
  let alpha = 0.2  // 平滑因子
  let data_points = [10.0, 12.0, 15.0, 8.0, 20.0, 18.0, 14.0, 22.0, 16.0, 19.0]
  
  // 验证参数
  assert_eq(alpha > 0.0, true)
  assert_eq(alpha <= 1.0, true)
  assert_eq(data_points.length(), 10)
  
  // 计算指数移动平均
  let mut ema_values = []
  let mut current_ema = data_points[0]  // 初始EMA为第一个数据点
  ema_values.push(current_ema)
  
  let mut i = 1
  while i < data_points.length() {
    current_ema = alpha * data_points[i] + (1.0 - alpha) * current_ema
    ema_values.push(current_ema)
    i = i + 1
  }
  
  // 验证EMA计算
  assert_eq(ema_values.length(), data_points.length())
  assert_eq(ema_values[0], data_points[0])  // 第一个值
  
  // 验证EMA的平滑特性
  let mut i = 1
  while i < ema_values.length() {
    let prev_ema = ema_values[i - 1]
    let current_ema = ema_values[i]
    let current_data = data_points[i]
    
    // EMA应该在prev_ema和current_data之间
    assert_eq((current_ema >= prev_ema and current_ema <= current_data) or 
              (current_ema <= prev_ema and current_ema >= current_data), true)
    
    i = i + 1
  }
  
  // 计算简单移动平均进行比较
  let window_size = 3
  let mut sma_values = []
  i = window_size - 1
  while i < data_points.length() {
    let mut sum = 0.0
    let mut j = 0
    while j < window_size {
      sum = sum + data_points[i - j]
      j = j + 1
    }
    let sma = sum / window_size.to_string().to_double()
    sma_values.push(sma)
    i = i + 1
  }
  
  // 验证EMA比SMA更平滑（变化更小）
  let mut ema_variance = 0.0
  let mut sma_variance = 0.0
  
  i = 1
  while i < ema_values.length() {
    ema_variance = ema_variance + (ema_values[i] - ema_values[i - 1]).abs()
    i = i + 1
  }
  
  i = 1
  while i < sma_values.length() {
    sma_variance = sma_variance + (sma_values[i] - sma_values[i - 1]).abs()
    i = i + 1
  }
  
  // 验证EMA的平滑性
  assert_eq(ema_variance < sma_variance * 1.5, true)
}

test "telemetry_cardinality_estimation" {
  // 测试遥测基数估计
  
  let stream_items = [
    "user_123", "user_456", "user_123", "user_789", "user_456",
    "user_101", "user_123", "user_202", "user_303", "user_456",
    "user_404", "user_505", "user_606", "user_789", "user_707",
    "user_808", "user_909", "user_101", "user_202", "user_303"
  ]
  
  // 验证流数据
  assert_eq(stream_items.length(), 20)
  assert_eq(stream_items[0], "user_123")
  assert_eq(stream_items[19], "user_303")
  
  // 计算实际基数（精确值）
  let mut unique_items = []
  let mut i = 0
  while i < stream_items.length() {
    let item = stream_items[i]
    let mut found = false
    let mut j = 0
    
    while j < unique_items.length() {
      if unique_items[j] == item {
        found = true
        break
      }
      j = j + 1
    }
    
    if not found {
      unique_items.push(item)
    }
    
    i = i + 1
  }
  
  let actual_cardinality = unique_items.length()
  
  // 验证实际基数
  assert_eq(actual_cardinality, 10)
  assert_eq(actual_cardinality < stream_items.length(), true)
  
  // 模拟HyperLogLog基数估计（简化版）
  let hash_bits = 8  // 简化的哈希位数
  let register_count = 2 ^ hash_bits
  let mut registers = []
  
  // 初始化寄存器
  i = 0
  while i < register_count {
    registers.push(0)
    i = i + 1
  }
  
  // 处理流中的每个项目
  i = 0
  while i < stream_items.length() {
    let item = stream_items[i]
    
    // 简化的哈希函数（使用字符码的异或和）
    let mut hash_value = 0
    let mut j = 0
    while j < item.length() {
      hash_value = hash_value ^ item.char_code_at(j)
      j = j + 1
    }
    
    // 确定寄存器索引
    let register_index = hash_value % register_count
    
    // 计算前导零的数量（简化版）
    let mut leading_zeros = 0
    let mut temp_hash = hash_value
    while temp_hash % 2 == 0 and leading_zeros < hash_bits {
      leading_zeros = leading_zeros + 1
      temp_hash = temp_hash / 2
    }
    
    // 更新寄存器
    if leading_zeros > registers[register_index] {
      registers[register_index] = leading_zeros
    }
    
    i = i + 1
  }
  
  // 计算估计基数
  let mut register_sum = 0
  i = 0
  while i < registers.length() {
    register_sum = register_sum + registers[i]
    i = i + 1
  }
  
  let average_register = register_sum.to_string().to_double() / registers.length().to_string().to_double()
  let estimated_cardinality = double_to_int(register_count.to_string().to_double() * 2.0.pow(average_register))
  
  // 验证基数估计
  assert_eq(estimated_cardinality > 0, true)
  
  // 计算估计误差
  let error_percentage = ((estimated_cardinality - actual_cardinality).abs() * 100) / actual_cardinality
  
  // 验证估计精度（对于简化的HLL算法，允许较大误差）
  assert_eq(error_percentage < 200, true)  // 允许200%的误差
  
  // 验证基数估计的有用性
  let estimation_useful = estimated_cardinality > actual_cardinality / 2 and 
                         estimated_cardinality < actual_cardinality * 3
  assert_eq(estimation_useful, true)
}