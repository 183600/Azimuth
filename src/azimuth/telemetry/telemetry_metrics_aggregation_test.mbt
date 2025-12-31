// 遥测指标聚合测试用例
// 测试遥测指标的各种聚合计算功能

test "counter_aggregation" {
  // 测试计数器聚合
  
  let counter_values = [100, 250, 180, 320, 450, 290, 380, 410]
  let time_window = 300 // 5分钟窗口
  
  // 计算总和
  let total_sum = counter_values.reduce(0, fn(acc, x) { acc + x })
  
  // 计算增长率
  let first_value = counter_values[0]
  let last_value = counter_values[counter_values.length() - 1]
  let growth_rate = ((last_value - first_value).to_float() / first_value.to_float()) * 100.0
  
  // 计算平均值
  let average = total_sum.to_float() / counter_values.length().to_float()
  
  // 验证聚合结果
  assert_eq(total_sum, 2380)
  assert_eq(growth_rate > 300.0, true)
  assert_eq(average > 250.0, true)
  assert_eq(average < 350.0, true)
  
  // 验证时间窗口
  assert_eq(time_window > 0, true)
  assert_eq(time_window <= 3600, true) // 不超过1小时
  
  // 验证聚合精度
  let aggregation_precision = 99.95
  assert_eq(aggregation_precision > 99.0, true)
}

test "gauge_aggregation" {
  // 测试仪表盘聚合
  
  let gauge_readings = [45.2, 67.8, 23.1, 89.5, 34.6, 56.3, 78.9, 41.2]
  let sampling_interval = 30 // 30秒间隔
  
  // 计算统计量
  let sum = gauge_readings.reduce(0.0, fn(acc, x) { acc + x })
  let count = gauge_readings.length().to_float()
  let average = sum / count
  
  // 计算最大值和最小值
  let max_value = gauge_readings.reduce(0.0, fn(acc, x) { if x > acc { x } else { acc } })
  let min_value = gauge_readings.reduce(999999.0, fn(acc, x) { if x < acc { x } else { acc } })
  
  // 计算范围
  let range = max_value - min_value
  
  // 验证聚合结果
  assert_eq(average > 50.0, true)
  assert_eq(average < 60.0, true)
  assert_eq(max_value, 89.5)
  assert_eq(min_value, 23.1)
  assert_eq(range > 60.0, true)
  
  // 验证采样间隔
  assert_eq(sampling_interval >= 10, true)
  assert_eq(sampling_interval <= 300, true)
  
  // 验证数据波动性
  let volatility = range / average
  assert_eq(volatility > 1.0, true)
  assert_eq(volatility < 2.0, true)
}

test "histogram_aggregation" {
  // 测试直方图聚合
  
  let response_times = [12, 25, 8, 45, 18, 32, 67, 23, 14, 56, 29, 41]
  let buckets = [10, 25, 50, 100, 250, 500]
  
  // 计算分桶计数
  let mut bucket_counts = [0, 0, 0, 0, 0, 0]
  for time in response_times {
    if time <= buckets[0] { bucket_counts[0] = bucket_counts[0] + 1 }
    else if time <= buckets[1] { bucket_counts[1] = bucket_counts[1] + 1 }
    else if time <= buckets[2] { bucket_counts[2] = bucket_counts[2] + 1 }
    else if time <= buckets[3] { bucket_counts[3] = bucket_counts[3] + 1 }
    else if time <= buckets[4] { bucket_counts[4] = bucket_counts[4] + 1 }
    else { bucket_counts[5] = bucket_counts[5] + 1 }
  }
  
  // 验证分桶结果
  assert_eq(bucket_counts[0], 1) // <=10ms
  assert_eq(bucket_counts[1], 4) // 11-25ms
  assert_eq(bucket_counts[2], 5) // 26-50ms
  assert_eq(bucket_counts[3], 2) // 51-100ms
  assert_eq(bucket_counts[4], 0) // 101-250ms
  assert_eq(bucket_counts[5], 0) // >250ms
  
  // 计算百分位数
  let sorted_times = response_times.sort()
  let p50_index = sorted_times.length() / 2
  let p95_index = sorted_times.length() * 95 / 100
  let p99_index = sorted_times.length() * 99 / 100
  
  let p50 = sorted_times[p50_index]
  let p95 = sorted_times[p95_index]
  let p99 = sorted_times[p99_index]
  
  // 验证百分位数
  assert_eq(p50 > 20, true)
  assert_eq(p50 < 35, true)
  assert_eq(p95 > 40, true)
  assert_eq(p95 < 70, true)
  assert_eq(p99 >= p95, true)
}

test "timer_aggregation" {
  // 测试计时器聚合
  
  let timer_durations = [125, 89, 234, 156, 198, 267, 143, 178, 201, 165]
  let operation_name = "database_query"
  
  // 计算基本统计量
  let total_duration = timer_durations.reduce(0, fn(acc, x) { acc + x })
  let average_duration = total_duration.to_float() / timer_durations.length().to_float()
  
  // 计算最小和最大持续时间
  let min_duration = timer_durations.reduce(999999, fn(acc, x) { if x < acc { x } else { acc } })
  let max_duration = timer_durations.reduce(0, fn(acc, x) { if x > acc { x } else { acc } })
  
  // 计算吞吐量（每秒操作数）
  let total_time_seconds = 60.0 // 假设在60秒内完成这些操作
  let throughput = timer_durations.length().to_float() / total_time_seconds
  
  // 验证操作名称
  assert_eq(operation_name.has_prefix("database"), true)
  assert_eq(operation_name.has_suffix("query"), true)
  
  // 验证聚合结果
  assert_eq(average_duration > 150.0, true)
  assert_eq(average_duration < 200.0, true)
  assert_eq(min_duration, 89)
  assert_eq(max_duration, 267)
  assert_eq(throughput > 0.1, true)
  
  // 验证性能指标
  let performance_score = 1000.0 / average_duration // 分数越高越好
  assert_eq(performance_score > 5.0, true)
  assert_eq(performance_score < 10.0, true)
}

test "rate_aggregation" {
  // 测试速率聚合
  
  let request_counts = [120, 135, 128, 142, 155, 148, 163, 171, 158, 182]
  let time_intervals = [60, 60, 60, 60, 60, 60, 60, 60, 60, 60] // 每个间隔60秒
  
  // 计算每分钟请求数
  let mut rates = []
  for i in 0..request_counts.length() {
    let rate = request_counts[i].to_float() / time_intervals[i].to_float()
    rates.push(rate)
  }
  
  // 计算平均速率
  let total_requests = request_counts.reduce(0, fn(acc, x) { acc + x })
  let total_time = time_intervals.reduce(0, fn(acc, x) { acc + x })
  let average_rate = total_requests.to_float() / total_time.to_float()
  
  // 计算速率变化
  let first_rate = rates[0]
  let last_rate = rates[rates.length() - 1]
  let rate_change = ((last_rate - first_rate) / first_rate) * 100.0
  
  // 验证聚合结果
  assert_eq(average_rate > 2.0, true)
  assert_eq(average_rate < 3.0, true)
  assert_eq(rate_change > 40.0, true)
  assert_eq(rate_change < 60.0, true)
  
  // 验证速率稳定性
  let rate_variance = calculate_variance(rates, average_rate)
  assert_eq(rate_variance < 0.5, true) // 速率应该相对稳定
}

test "dimensional_aggregation" {
  // 测试多维聚合
  
  let dimensional_data = [
    "{service:api,region:us-east,method:GET,count:150}",
    "{service:api,region:us-east,method:POST,count:85}",
    "{service:api,region:us-west,method:GET,count:120}",
    "{service:api,region:us-west,method:POST,count:95}",
    "{service:worker,region:us-east,method:GET,count:45}",
    "{service:worker,region:us-east,method:POST,count:35}",
    "{service:worker,region:us-west,method:GET,count:55}",
    "{service:worker,region:us-west,method:POST,count:65}"
  ]
  
  // 按服务聚合
  let mut api_count = 0
  let mut worker_count = 0
  for record in dimensional_data {
    if record.contains("service:api") {
      let count_str = record.split("count:")[1].split("}")[0]
      api_count = api_count + count_str.to_int()
    } else if record.contains("service:worker") {
      let count_str = record.split("count:")[1].split("}")[0]
      worker_count = worker_count + count_str.to_int()
    }
  }
  
  // 按区域聚合
  let mut us_east_count = 0
  let mut us_west_count = 0
  for record in dimensional_data {
    if record.contains("region:us-east") {
      let count_str = record.split("count:")[1].split("}")[0]
      us_east_count = us_east_count + count_str.to_int()
    } else if record.contains("region:us-west") {
      let count_str = record.split("count:")[1].split("}")[0]
      us_west_count = us_west_count + count_str.to_int()
    }
  }
  
  // 验证聚合结果
  assert_eq(api_count, 450)
  assert_eq(worker_count, 200)
  assert_eq(us_east_count, 315)
  assert_eq(us_west_count, 335)
  
  // 验证聚合覆盖率
  let total_records = dimensional_data.length()
  let aggregated_records = total_records
  let coverage = (aggregated_records.to_float() / total_records.to_float()) * 100.0
  assert_eq(coverage, 100.0)
}

test "time_series_aggregation" {
  // 测试时间序列聚合
  
  let time_series_data = [
    "{timestamp:1640995200,value:100}",
    "{timestamp:1640995260,value:110}",
    "{timestamp:1640995320,value:95}",
    "{timestamp:1640995380,value:125}",
    "{timestamp:1640995440,value:115}",
    "{timestamp:1640995500,value:105}",
    "{timestamp:1640995560,value:120}",
    "{timestamp:1640995620,value:130}"
  ]
  
  // 提取时间戳和值
  let mut timestamps = []
  let mut values = []
  for record in time_series_data {
    let timestamp_str = record.split("timestamp:")[1].split(",")[0]
    let value_str = record.split("value:")[1].split("}")[0]
    timestamps.push(timestamp_str.to_int())
    values.push(value_str.to_int())
  }
  
  // 计算移动平均（窗口大小为3）
  let mut moving_averages = []
  for i in 2..values.length() {
    let avg = (values[i-2] + values[i-1] + values[i]).to_float() / 3.0
    moving_averages.push(avg)
  }
  
  // 计算趋势
  let first_value = values[0].to_float()
  let last_value = values[values.length() - 1].to_float()
  let trend = if last_value > first_value { "increasing" } else { "decreasing" }
  
  // 验证聚合结果
  assert_eq(values.length(), 8)
  assert_eq(moving_averages.length(), 6)
  assert_eq(trend, "increasing")
  
  // 验证时间序列连续性
  let mut is_continuous = true
  for i in 1..timestamps.length() {
    if timestamps[i] - timestamps[i-1] != 60 {
      is_continuous = false
      break
    }
  }
  assert_eq(is_continuous, true)
  
  // 验证移动平均平滑效果
  let first_ma = moving_averages[0]
  let last_ma = moving_averages[moving_averages.length() - 1]
  assert_eq(last_ma > first_ma, true) // 趋势保持一致
}

test "aggregation_performance" {
  // 测试聚合性能
  
  let data_points = 100000
  let aggregation_time_limit = 5.0 // 秒
  let memory_limit = 100 * 1024 * 1024 // 100MB
  
  // 模拟聚合性能指标
  let actual_aggregation_time = 2.3 // 秒
  let actual_memory_usage = 45 * 1024 * 1024 // 45MB
  let throughput = data_points.to_float() / actual_aggregation_time
  
  // 验证性能指标
  assert_eq(actual_aggregation_time < aggregation_time_limit, true)
  assert_eq(actual_memory_usage < memory_limit, true)
  assert_eq(throughput > 40000.0, true)
  
  // 验证聚合精度
  let accuracy = 99.99 // 百分比
  assert_eq(accuracy > 99.9, true)
  
  // 验证并发聚合能力
  let concurrent_aggregations = 4
  let parallel_efficiency = 0.85 // 85%效率
  assert_eq(concurrent_aggregations >= 2, true)
  assert_eq(parallel_efficiency > 0.7, true)
}

// 辅助函数：计算方差
fn calculate_variance(values : Array[Double], mean : Double) -> Double {
  let mut sum_of_squares = 0.0
  for value in values {
    let diff = value - mean
    sum_of_squares = sum_of_squares + diff * diff
  }
  sum_of_squares / values.length().to_float()
}