// 遥测数据聚合和统计测试用例

test "telemetry_metric_aggregation_basic" {
  // 测试基本指标聚合
  
  let metric_name = "response_time"
  let metric_values = [120.5, 135.2, 98.7, 145.3, 112.8, 128.9, 105.6, 139.4]
  let aggregation_window_ms = 60000  // 1分钟窗口
  
  // 计算基本统计量
  let sum = 0.0
  let min_val = metric_values[0]
  let max_val = metric_values[0]
  
  for i = 0; i < metric_values.length(); i = i + 1 {
    sum = sum + metric_values[i]
    if metric_values[i] < min_val { min_val = metric_values[i] }
    if metric_values[i] > max_val { max_val = metric_values[i] }
  }
  
  let mean = sum / metric_values.length().to_float()
  let count = metric_values.length()
  
  // 验证基本统计量
  assert_eq(count, 8)
  assert_eq(metric_values[0], 120.5)
  assert_eq(metric_values[7], 139.4)
  assert_eq(min_val, 98.7)
  assert_eq(max_val, 145.3)
  assert_eq(mean > 120.0, true)
  assert_eq(mean < 130.0, true)
  
  // 计算方差和标准差
  let variance = 0.0
  for i = 0; i < metric_values.length(); i = i + 1 {
    variance = variance + (metric_values[i] - mean) * (metric_values[i] - mean)
  }
  variance = variance / count.to_float()
  let std_dev = variance.sqrt()
  
  // 验证方差和标准差
  assert_eq(variance > 0.0, true)
  assert_eq(std_dev > 0.0, true)
  
  // 创建聚合结果
  let aggregation_result = "{"
  aggregation_result = aggregation_result + "\"metric_name\":\"" + metric_name + "\"," 
  aggregation_result = aggregation_result + "\"aggregation_window_ms\":" + aggregation_window_ms.to_string() + ","
  aggregation_result = aggregation_result + "\"statistics\":{"
  aggregation_result = aggregation_result + "\"count\":" + count.to_string() + ","
  aggregation_result = aggregation_result + "\"sum\":" + sum.to_string() + ","
  aggregation_result = aggregation_result + "\"mean\":" + mean.to_string() + ","
  aggregation_result = aggregation_result + "\"min\":" + min_val.to_string() + ","
  aggregation_result = aggregation_result + "\"max\":" + max_val.to_string() + ","
  aggregation_result = aggregation_result + "\"variance\":" + variance.to_string() + ","
  aggregation_result = aggregation_result + "\"std_dev\":" + std_dev.to_string()
  aggregation_result = aggregation_result + "}"
  aggregation_result = aggregation_result + "}"
  
  // 验证聚合结果格式
  assert_eq(aggregation_result.has_prefix("{"), true)
  assert_eq(aggregation_result.has_suffix("}"), true)
  assert_eq(aggregation_result.contains("\"metric_name\":"), true)
  assert_eq(aggregation_result.contains("\"statistics\":"), true)
  assert_eq(aggregation_result.contains("\"count\":"), true)
  assert_eq(aggregation_result.contains("\"mean\":"), true)
}

test "telemetry_percentile_calculation" {
  // 测试百分位数计算
  
  let metric_values = [10.0, 20.0, 30.0, 40.0, 50.0, 60.0, 70.0, 80.0, 90.0, 100.0]
  let percentiles = [50.0, 90.0, 95.0, 99.0]
  let sorted_values = metric_values  // 已经排序
  
  // 验证输入数据
  assert_eq(sorted_values.length(), 10)
  assert_eq(sorted_values[0], 10.0)
  assert_eq(sorted_values[9], 100.0)
  
  // 计算百分位数
  let percentile_results = []
  for i = 0; i < percentiles.length(); i = i + 1 {
    let p = percentiles[i]
    let index = (p / 100.0) * (sorted_values.length() - 1).to_float()
    let lower_index = index.floor().to_int()
    let upper_index = index.ceil().to_int()
    
    let percentile_value = 0.0
    if lower_index == upper_index {
      percentile_value = sorted_values[lower_index]
    } else {
      let weight = index - lower_index.to_float()
      percentile_value = sorted_values[lower_index] * (1.0 - weight) + sorted_values[upper_index] * weight
    }
    
    percentile_results.push(percentile_value)
  }
  
  // 验证百分位数计算结果
  assert_eq(percentile_results.length(), 4)
  assert_eq(percentile_results[0], 55.0)  // 50th percentile (P50)
  assert_eq(percentile_results[1], 91.0)  // 90th percentile (P90)
  assert_eq(percentile_results[2], 95.5)  // 95th percentile (P95)
  assert_eq(percentile_results[3], 99.1)  // 99th percentile (P99)
  
  // 创建百分位数结果
  let percentile_result = "{"
  percentile_result = percentile_result + "\"percentiles\":["
  
  for i = 0; i < percentiles.length(); i = i + 1 {
    if i > 0 { percentile_result = percentile_result + "," }
    percentile_result = percentile_result + "{\"p\":" + percentiles[i].to_string() + ",\"value\":" + percentile_results[i].to_string() + "}"
  }
  
  percentile_result = percentile_result + "]}"
  
  // 验证百分位数结果格式
  assert_eq(percentile_result.has_prefix("{"), true)
  assert_eq(percentile_result.has_suffix("}]"), true)
  assert_eq(percentile_result.contains("\"percentiles\":"), true)
  assert_eq(percentile_result.contains("\"p\":"), true)
  assert_eq(percentile_result.contains("\"value\":"), true)
}

test "telemetry_histogram_aggregation" {
  // 测试直方图聚合
  
  let metric_name = "request_duration"
  let bucket_boundaries = [10.0, 25.0, 50.0, 100.0, 250.0, 500.0, 1000.0, 5000.0]
  let metric_values = [5.0, 15.0, 30.0, 75.0, 120.0, 300.0, 800.0, 2000.0, 6000.0]
  
  // 初始化桶计数
  let bucket_counts = [for i = 0; i < bucket_boundaries.length() + 1; i = i + 1].map(fn(_) { 0 })
  
  // 验证输入数据
  assert_eq(bucket_boundaries.length(), 8)
  assert_eq(metric_values.length(), 9)
  assert_eq(bucket_counts.length(), 9)  // 8个边界 + 1个无穷大桶
  
  // 将值分配到桶中
  for i = 0; i < metric_values.length(); i = i + 1 {
    let value = metric_values[i]
    let bucket_index = bucket_counts.length() - 1  // 默认到最后一个桶
    
    for j = 0; j < bucket_boundaries.length(); j = j + 1 {
      if value <= bucket_boundaries[j] {
        bucket_index = j
        break
      }
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 1)    // <= 10.0: [5.0]
  assert_eq(bucket_counts[1], 1)    // <= 25.0: [15.0]
  assert_eq(bucket_counts[2], 1)    // <= 50.0: [30.0]
  assert_eq(bucket_counts[3], 1)    // <= 100.0: [75.0]
  assert_eq(bucket_counts[4], 1)    // <= 250.0: [120.0]
  assert_eq(bucket_counts[5], 1)    // <= 500.0: [300.0]
  assert_eq(bucket_counts[6], 1)    // <= 1000.0: [800.0]
  assert_eq(bucket_counts[7], 1)    // <= 5000.0: [2000.0]
  assert_eq(bucket_counts[8], 1)    // > 5000.0: [6000.0]
  
  // 创建直方图结果
  let histogram_result = "{"
  histogram_result = histogram_result + "\"metric_name\":\"" + metric_name + "\"," 
  histogram_result = histogram_result + "\"bucket_boundaries\":["
  
  for i = 0; i < bucket_boundaries.length(); i = i + 1 {
    if i > 0 { histogram_result = histogram_result + "," }
    histogram_result = histogram_result + bucket_boundaries[i].to_string()
  }
  
  histogram_result = histogram_result + "],"
  histogram_result = histogram_result + "\"bucket_counts\":["
  
  for i = 0; i < bucket_counts.length(); i = i + 1 {
    if i > 0 { histogram_result = histogram_result + "," }
    histogram_result = histogram_result + bucket_counts[i].to_string()
  }
  
  histogram_result = histogram_result + "],"
  histogram_result = histogram_result + "\"count\":" + metric_values.length().to_string()
  histogram_result = histogram_result + "}"
  
  // 验证直方图结果格式
  assert_eq(histogram_result.has_prefix("{"), true)
  assert_eq(histogram_result.has_suffix("}"), true)
  assert_eq(histogram_result.contains("\"metric_name\":"), true)
  assert_eq(histogram_result.contains("\"bucket_boundaries\":"), true)
  assert_eq(histogram_result.contains("\"bucket_counts\":"), true)
  assert_eq(histogram_result.contains("\"count\":"), true)
}

test "telemetry_time_weighted_aggregation" {
  // 测试时间加权聚合
  
  let metric_name = "cpu_usage"
  let time_series_data = [
    (1634567890, 25.5),
    (1634567950, 35.2),
    (1634568010, 45.8),
    (1634568070, 38.1),
    (1634568130, 28.9)
  ]
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 5)
  assert_eq(time_series_data[0].0, 1634567890)
  assert_eq(time_series_data[0].1, 25.5)
  assert_eq(time_series_data[4].0, 1634568130)
  assert_eq(time_series_data[4].1, 28.9)
  
  // 计算时间加权平均值
  let weighted_sum = 0.0
  let total_time = 0.0
  
  for i = 0; i < time_series_data.length() - 1; i = i + 1 {
    let current_time = time_series_data[i].0.to_float()
    let next_time = time_series_data[i + 1].0.to_float()
    let current_value = time_series_data[i].1
    let next_value = time_series_data[i + 1].1
    
    let duration = next_time - current_time
    let avg_value = (current_value + next_value) / 2.0
    
    weighted_sum = weighted_sum + avg_value * duration
    total_time = total_time + duration
  }
  
  let time_weighted_mean = weighted_sum / total_time
  
  // 验证时间加权计算
  assert_eq(total_time > 0.0, true)
  assert_eq(time_weighted_mean > 25.0, true)
  assert_eq(time_weighted_mean < 45.0, true)
  
  // 创建时间加权聚合结果
  let time_weighted_result = "{"
  time_weighted_result = time_weighted_result + "\"metric_name\":\"" + metric_name + "\"," 
  time_weighted_result = time_weighted_result + "\"aggregation_type\":\"time_weighted_mean\"," 
  time_weighted_result = time_weighted_result + "\"time_range\":{"
  time_weighted_result = time_weighted_result + "\"start\":" + time_series_data[0].0.to_string() + ","
  time_weighted_result = time_weighted_result + "\"end\":" + time_series_data[time_series_data.length() - 1].0.to_string() + ","
  time_weighted_result = time_weighted_result + "\"duration\":" + total_time.to_string()
  time_weighted_result = time_weighted_result + "},"
  time_weighted_result = time_weighted_result + "\"time_weighted_mean\":" + time_weighted_mean.to_string()
  time_weighted_result = time_weighted_result + "}"
  
  // 验证时间加权聚合结果格式
  assert_eq(time_weighted_result.has_prefix("{"), true)
  assert_eq(time_weighted_result.has_suffix("}"), true)
  assert_eq(time_weighted_result.contains("\"metric_name\":"), true)
  assert_eq(time_weighted_result.contains("\"aggregation_type\":"), true)
  assert_eq(time_weighted_result.contains("\"time_range\":"), true)
  assert_eq(time_weighted_result.contains("\"time_weighted_mean\":"), true)
}

test "telemetry_cardinality_estimation" {
  // 测试基数估算
  
  let metric_name = "unique_user_ids"
  let observed_values = [
    "user-001", "user-002", "user-003", "user-001", "user-004",
    "user-005", "user-002", "user-006", "user-007", "user-003",
    "user-008", "user-009", "user-004", "user-010", "user-011"
  ]
  
  // 验证输入数据
  assert_eq(observed_values.length(), 15)
  assert_eq(observed_values[0], "user-001")
  assert_eq(observed_values[14], "user-011")
  
  // 计算实际唯一值数量
  let unique_values = []
  for i = 0; i < observed_values.length(); i = i + 1 {
    let value = observed_values[i]
    let found = false
    
    for j = 0; j < unique_values.length(); j = j + 1 {
      if unique_values[j] == value {
        found = true
        break
      }
    }
    
    if !found {
      unique_values.push(value)
    }
  }
  
  let actual_cardinality = unique_values.length()
  
  // 验证实际基数
  assert_eq(actual_cardinality, 11)  // 应该有11个唯一用户ID
  assert_eq(actual_cardinality < observed_values.length(), true)
  
  // 模拟HyperLogLog基数估算（简化版）
  let hash_buckets = [for i = 0; i < 16; i = i + 1].map(fn(_) { 0 })
  
  for i = 0; i < observed_values.length(); i = i + 1 {
    let value = observed_values[i]
    // 简单哈希函数（仅用于演示）
    let hash_value = 0
    for j = 0; j < value.length(); j = j + 1 {
      hash_value = hash_value + value[j].to_int()
    }
    
    let bucket_index = hash_value % 16
    let trailing_zeros = 0
    
    // 计算尾随零的数量（简化版）
    let temp_hash = hash_value
    while temp_hash % 2 == 0 && trailing_zeros < 32 {
      trailing_zeros = trailing_zeros + 1
      temp_hash = temp_hash / 2
    }
    
    if trailing_zeros > hash_buckets[bucket_index] {
      hash_buckets[bucket_index] = trailing_zeros
    }
  }
  
  // 估算基数（简化版HyperLogLog）
  let bucket_sum = 0.0
  for i = 0; i < hash_buckets.length(); i = i + 1 {
    bucket_sum = bucket_sum + (2.0.pow(hash_buckets[i].to_float()))
  }
  
  let estimated_cardinality = (0.79402 * 16.0 * 16.0 / bucket_sum).to_int()
  
  // 验证基数估算
  assert_eq(estimated_cardinality > 0, true)
  assert_eq(estimated_cardinality <= observed_values.length(), true)
  
  // 创建基数估算结果
  let cardinality_result = "{"
  cardinality_result = cardinality_result + "\"metric_name\":\"" + metric_name + "\"," 
  cardinality_result = cardinality_result + "\"total_observations\":" + observed_values.length().to_string() + ","
  cardinality_result = cardinality_result + "\"actual_cardinality\":" + actual_cardinality.to_string() + ","
  cardinality_result = cardinality_result + "\"estimated_cardinality\":" + estimated_cardinality.to_string() + ","
  cardinality_result = cardinality_result + "\"estimation_error\":" + ((estimated_cardinality - actual_cardinality).abs().to_float() / actual_cardinality.to_float() * 100.0).to_string()
  cardinality_result = cardinality_result + "}"
  
  // 验证基数估算结果格式
  assert_eq(cardinality_result.has_prefix("{"), true)
  assert_eq(cardinality_result.has_suffix("}"), true)
  assert_eq(cardinality_result.contains("\"metric_name\":"), true)
  assert_eq(cardinality_result.contains("\"total_observations\":"), true)
  assert_eq(cardinality_result.contains("\"actual_cardinality\":"), true)
  assert_eq(cardinality_result.contains("\"estimated_cardinality\":"), true)
  assert_eq(cardinality_result.contains("\"estimation_error\":"), true)
}

test "telemetry_rate_aggregation" {
  // 测试速率聚合
  
  let metric_name = "requests_per_second"
  let time_window_ms = 60000  // 1分钟窗口
  let event_counts = [
    (1634567890, 10),
    (1634567950, 15),
    (1634568010, 12),
    (1634568070, 18),
    (1634568130, 14)
  ]
  
  // 验证输入数据
  assert_eq(event_counts.length(), 5)
  assert_eq(event_counts[0].0, 1634567890)
  assert_eq(event_counts[0].1, 10)
  assert_eq(event_counts[4].0, 1634568130)
  assert_eq(event_counts[4].1, 14)
  
  // 计算速率
  let total_events = 0
  let total_duration = 0.0
  
  for i = 0; i < event_counts.length() - 1; i = i + 1 {
    let current_time = event_counts[i].0.to_float()
    let next_time = event_counts[i + 1].0.to_float()
    let current_count = event_counts[i].1
    
    total_events = total_events + current_count
    total_duration = total_duration + (next_time - current_time)
  }
  
  // 添加最后一个时间段的计数
  total_events = total_events + event_counts[event_counts.length() - 1].1
  
  let rate_per_second = total_events.to_float() / total_duration
  let rate_per_minute = rate_per_second * 60.0
  
  // 验证速率计算
  assert_eq(total_events, 69)  // 10 + 15 + 12 + 18 + 14
  assert_eq(total_duration > 0.0, true)
  assert_eq(rate_per_second > 0.0, true)
  assert_eq(rate_per_minute > rate_per_second, true)
  
  // 计算每个时间段的速率
  let interval_rates = []
  for i = 0; i < event_counts.length() - 1; i = i + 1 {
    let current_time = event_counts[i].0.to_float()
    let next_time = event_counts[i + 1].0.to_float()
    let current_count = event_counts[i].1
    let duration = next_time - current_time
    let rate = current_count.to_float() / duration
    
    interval_rates.push(rate)
  }
  
  // 验证间隔速率
  assert_eq(interval_rates.length(), 4)
  assert_eq(interval_rates[0] > 0.0, true)
  assert_eq(interval_rates[3] > 0.0, true)
  
  // 创建速率聚合结果
  let rate_result = "{"
  rate_result = rate_result + "\"metric_name\":\"" + metric_name + "\"," 
  rate_result = rate_result + "\"time_window_ms\":" + time_window_ms.to_string() + ","
  rate_result = rate_result + "\"total_events\":" + total_events.to_string() + ","
  rate_result = rate_result + "\"total_duration\":" + total_duration.to_string() + ","
  rate_result = rate_result + "\"rate_per_second\":" + rate_per_second.to_string() + ","
  rate_result = rate_result + "\"rate_per_minute\":" + rate_per_minute.to_string() + ","
  rate_result = rate_result + "\"interval_rates\":["
  
  for i = 0; i < interval_rates.length(); i = i + 1 {
    if i > 0 { rate_result = rate_result + "," }
    rate_result = rate_result + interval_rates[i].to_string()
  }
  
  rate_result = rate_result + "]}"
  
  // 验证速率聚合结果格式
  assert_eq(rate_result.has_prefix("{"), true)
  assert_eq(rate_result.has_suffix("}]"), true)
  assert_eq(rate_result.contains("\"metric_name\":"), true)
  assert_eq(rate_result.contains("\"total_events\":"), true)
  assert_eq(rate_result.contains("\"rate_per_second\":"), true)
  assert_eq(rate_result.contains("\"interval_rates\":"), true)
}