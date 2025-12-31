// 遥测数据聚合算法测试用例

test "telemetry_time_weighted_aggregation" {
  // 测试遥测时间加权聚合算法
  
  let aggregation_config = {
    "time_window_seconds": 60,
    "weight_function": "linear",
    "decay_factor": 0.95,
    "min_data_points": 5
  }
  
  // 验证聚合配置
  assert_eq(aggregation_config["time_window_seconds"], "60")
  assert_eq(aggregation_config["weight_function"], "linear")
  assert_eq(aggregation_config["decay_factor"], "0.95")
  assert_eq(aggregation_config["min_data_points"], "5")
  
  // 模拟时间序列数据点
  let time_series_data = [
    {"timestamp": 1703123450, "value": 10.0},
    {"timestamp": 1703123460, "value": 20.0},
    {"timestamp": 1703123470, "value": 15.0},
    {"timestamp": 1703123480, "value": 25.0},
    {"timestamp": 1703123490, "value": 30.0}
  ]
  
  // 验证时间序列数据
  assert_eq(time_series_data.length(), 5)
  
  // 计算时间加权平均值
  let base_timestamp = time_series_data[0]["timestamp"].to_int()
  let mut weighted_sum = 0.0
  let mut weight_sum = 0.0
  
  let mut i = 0
  while i < time_series_data.length() {
    let data_point = time_series_data[i]
    let timestamp = data_point["timestamp"].to_int()
    let value = data_point["value"].to_float()
    
    // 计算时间权重（越新的数据权重越高）
    let time_diff = timestamp - base_timestamp
    let time_weight = 1.0 + (time_diff.to_float() / aggregation_config["time_window_seconds"].to_float())
    
    weighted_sum = weighted_sum + (value * time_weight)
    weight_sum = weight_sum + time_weight
    
    i = i + 1
  }
  
  let time_weighted_avg = weighted_sum / weight_sum
  
  // 验证时间加权平均计算
  assert_eq(time_weighted_avg > 15.0, true)
  assert_eq(time_weighted_avg < 30.0, true)
  
  // 计算指数衰减加权平均
  let mut decay_weighted_sum = 0.0
  let mut decay_weight_sum = 0.0
  let decay_factor = aggregation_config["decay_factor"].to_float()
  
  // 从最新数据开始计算
  let mut i = time_series_data.length() - 1
  while i >= 0 {
    let data_point = time_series_data[i]
    let value = data_point["value"].to_float()
    let age = time_series_data.length() - 1 - i
    
    let decay_weight = decay_factor.pow(age.to_float())
    decay_weighted_sum = decay_weighted_sum + (value * decay_weight)
    decay_weight_sum = decay_weight_sum + decay_weight
    
    i = i - 1
  }
  
  let decay_weighted_avg = decay_weighted_sum / decay_weight_sum
  
  // 验证指数衰减加权平均
  assert_eq(decay_weighted_avg > 15.0, true)
  assert_eq(decay_weighted_avg < 30.0, true)
  
  // 比较不同聚合方法的结果
  let simple_avg = (10.0 + 20.0 + 15.0 + 25.0 + 30.0) / 5.0
  
  // 验证简单平均值
  assert_eq(simple_avg, 20.0)
  
  // 验证加权平均与简单平均的差异
  let time_weight_diff = (time_weighted_avg - simple_avg).abs()
  let decay_weight_diff = (decay_weighted_avg - simple_avg).abs()
  
  assert_eq(time_weight_diff > 0.0, true)
  assert_eq(decay_weight_diff > 0.0, true)
}

test "telemetry_percentile_aggregation" {
  // 测试遥测百分位数聚合算法
  
  let percentile_config = {
    "percentiles": [50, 90, 95, 99],
    "interpolation_method": "linear",
    "sample_size": 1000
  }
  
  // 验证百分位数配置
  assert_eq(percentile_config["percentiles"].length(), 4)
  assert_eq(percentile_config["interpolation_method"], "linear")
  assert_eq(percentile_config["sample_size"], "1000")
  
  // 模拟性能指标数据
  let latency_data = [
    10, 15, 20, 25, 30, 35, 40, 45, 50, 55,
    60, 65, 70, 75, 80, 85, 90, 95, 100, 105,
    110, 115, 120, 125, 130, 135, 140, 145, 150, 155,
    160, 165, 170, 175, 180, 185, 190, 195, 200, 205,
    210, 215, 220, 225, 230, 235, 240, 245, 250, 255
  ]
  
  // 验证延迟数据
  assert_eq(latency_data.length(), 50)
  
  // 排序数据（已排序）
  let sorted_data = latency_data
  
  // 计算百分位数
  let mut percentile_results = {}
  let mut i = 0
  
  while i < percentile_config["percentiles"].length() {
    let percentile = percentile_config["percentiles"][i].to_int()
    let index = (percentile * sorted_data.length()) / 100
    
    // 边界检查
    let adjusted_index = 
      if index >= sorted_data.length() { sorted_data.length() - 1 }
      else if index < 0 { 0 }
      else { index }
    
    let percentile_value = sorted_data[adjusted_index]
    percentile_results["p" + percentile.to_string()] = percentile_value.to_string()
    
    i = i + 1
  }
  
  // 验证百分位数计算结果
  assert_eq(percentile_results["p50"], "100")    // 中位数
  assert_eq(percentile_results["p90"], "220")    // 第90百分位
  assert_eq(percentile_results["p95"], "235")    // 第95百分位
  assert_eq(percentile_results["p99"], "250")    // 第99百分位
  
  // 验证百分位数的单调性
  let p50 = percentile_results["p50"].to_int()
  let p90 = percentile_results["p90"].to_int()
  let p95 = percentile_results["p95"].to_int()
  let p99 = percentile_results["p99"].to_int()
  
  assert_eq(p50 <= p90, true)
  assert_eq(p90 <= p95, true)
  assert_eq(p95 <= p99, true)
  
  // 计算统计摘要
  let min_value = sorted_data[0]
  let max_value = sorted_data[sorted_data.length() - 1]
  let sum = 6375  // 手动计算或实际求和
  let avg = sum.to_float() / sorted_data.length().to_float()
  
  // 验证统计摘要
  assert_eq(min_value, 10)
  assert_eq(max_value, 255)
  assert_eq(avg, 127.5)
  
  // 验证百分位数在合理范围内
  assert_eq(p50 >= min_value and p50 <= max_value, true)
  assert_eq(p99 >= min_value and p99 <= max_value, true)
}

test "telemetry_histogram_aggregation" {
  // 测试遥测直方图聚合算法
  
  let histogram_config = {
    "bucket_boundaries": [0, 10, 50, 100, 500, 1000],
    "aggregation_method": "sum",
    "reset_interval_seconds": 300
  }
  
  // 验证直方图配置
  assert_eq(histogram_config["bucket_boundaries"].length(), 6)
  assert_eq(histogram_config["aggregation_method"], "sum")
  assert_eq(histogram_config["reset_interval_seconds"], "300")
  
  // 模拟直方图数据点
  let histogram_data = [
    {"value": 5, "count": 100},    // 落在[0,10)桶
    {"value": 25, "count": 200},   // 落在[10,50)桶
    {"value": 75, "count": 150},   // 落在[50,100)桶
    {"value": 300, "count": 80},   // 落在[100,500)桶
    {"value": 800, "count": 30}    // 落在[500,1000)桶
  ]
  
  // 验证直方图数据
  assert_eq(histogram_data.length(), 5)
  
  // 初始化桶计数
  let bucket_boundaries = histogram_config["bucket_boundaries"]
  let mut bucket_counts = []
  
  let mut i = 0
  while i < bucket_boundaries.length() {
    bucket_counts.push(0)
    i = i + 1
  }
  
  // 分配数据点到桶
  let mut i = 0
  while i < histogram_data.length() {
    let data_point = histogram_data[i]
    let value = data_point["value"].to_int()
    let count = data_point["count"].to_int()
    
    // 找到合适的桶
    let mut bucket_index = bucket_boundaries.length() - 1  // 默认最后一个桶
    
    let mut j = 0
    while j < bucket_boundaries.length() - 1 {
      if value >= bucket_boundaries[j].to_int() and value < bucket_boundaries[j + 1].to_int() {
        bucket_index = j
        break
      }
      j = j + 1
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + count
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 100)  // [0,10)
  assert_eq(bucket_counts[1], 200)  // [10,50)
  assert_eq(bucket_counts[2], 150)  // [50,100)
  assert_eq(bucket_counts[3], 80)   // [100,500)
  assert_eq(bucket_counts[4], 30)   // [500,1000)
  assert_eq(bucket_counts[5], 0)    // [1000,+∞)
  
  // 计算累积分布
  let mut cumulative_counts = []
  let mut cumulative_sum = 0
  
  let mut i = 0
  while i < bucket_counts.length() {
    cumulative_sum = cumulative_sum + bucket_counts[i]
    cumulative_counts.push(cumulative_sum)
    i = i + 1
  }
  
  // 验证累积分布
  assert_eq(cumulative_counts[0], 100)
  assert_eq(cumulative_counts[1], 300)  // 100+200
  assert_eq(cumulative_counts[2], 450)  // 300+150
  assert_eq(cumulative_counts[3], 530)  // 450+80
  assert_eq(cumulative_counts[4], 560)  // 530+30
  assert_eq(cumulative_counts[5], 560)  // 560+0
  
  // 计算百分位数估计
  let total_count = cumulative_sum
  let p50_index = total_count / 2
  let p95_index = (total_count * 95) / 100
  
  // 找到对应百分位数的桶
  let mut p50_bucket = 0
  let mut p95_bucket = 0
  
  let mut i = 0
  while i < cumulative_counts.length() {
    if cumulative_counts[i] >= p50_index and p50_bucket == 0 {
      p50_bucket = i
    }
    
    if cumulative_counts[i] >= p95_index and p95_bucket == 0 {
      p95_bucket = i
    }
    
    i = i + 1
  }
  
  // 验证百分位数桶
  assert_eq(p50_bucket, 1)  // 第50百分位在[10,50)桶
  assert_eq(p95_bucket, 3)  // 第95百分位在[100,500)桶
  
  // 验证桶边界
  assert_eq(bucket_boundaries[p50_bucket], 10)
  assert_eq(bucket_boundaries[p50_bucket + 1], 50)
  assert_eq(bucket_boundaries[p95_bucket], 100)
  assert_eq(bucket_boundaries[p95_bucket + 1], 500)
}