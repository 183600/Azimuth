// 遥测数据聚合测试用例，确保数据统计和汇总的正确性

test "telemetry_counter_aggregation" {
  // 测试计数器聚合
  
  let counter_metrics = [
    ("http_requests_total", 100),
    ("http_requests_total", 150),
    ("http_requests_total", 200),
    ("http_requests_total", 50)
  ]
  
  // 聚合计数器值
  let mut aggregated_value = 0
  let mut i = 0
  while i < counter_metrics.length() {
    let metric = counter_metrics[i]
    aggregated_value = aggregated_value + metric.1
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(aggregated_value, 500)
  assert_eq(aggregated_value > 0, true)
  
  // 测试带时间窗口的计数器聚合
  let time_windowed_metrics = [
    ("http_requests_total", 100, 1704067200L),  // 1小时前
    ("http_requests_total", 150, 1704070800L),  // 当前时间
    ("http_requests_total", 200, 1704074400L),  // 1小时后
    ("http_requests_total", 50, 1704078000L)    // 2小时后
  ]
  
  let current_time = 1704070800L
  let window_size = 3600L  // 1小时窗口
  let mut windowed_sum = 0
  
  i = 0
  while i < time_windowed_metrics.length() {
    let metric = time_windowed_metrics[i]
    let time_diff = (metric.2 - current_time).abs()
    
    if time_diff <= window_size {
      windowed_sum = windowed_sum + metric.1
    }
    i = i + 1
  }
  
  // 验证时间窗口聚合（只包含1小时内的数据）
  assert_eq(windowed_sum, 250)  // 100 + 150
}

test "telemetry_gauge_aggregation" {
  // 测试仪表聚合
  
  let gauge_metrics = [
    ("cpu_usage", 25.5),
    ("cpu_usage", 30.2),
    ("cpu_usage", 28.7),
    ("cpu_usage", 35.1),
    ("cpu_usage", 27.8)
  ]
  
  // 计算统计指标
  let mut sum = 0.0
  let mut min_value = gauge_metrics[0].1
  let mut max_value = gauge_metrics[0].1
  
  let mut i = 0
  while i < gauge_metrics.length() {
    let value = gauge_metrics[i].1
    sum = sum + value
    
    if value < min_value {
      min_value = value
    }
    if value > max_value {
      max_value = value
    }
    i = i + 1
  }
  
  let average = sum / gauge_metrics.length().to_double()
  
  // 验证统计结果
  assert_eq(min_value, 25.5)
  assert_eq(max_value, 35.1)
  assert_eq(average > 29.0, true)  // (25.5+30.2+28.7+35.1+27.8)/5 = 29.46
  assert_eq(average < 30.0, true)
  
  // 验证最新值（仪表通常关注当前值）
  let latest_value = gauge_metrics[gauge_metrics.length() - 1].1
  assert_eq(latest_value, 27.8)
}

test "telemetry_histogram_aggregation" {
  // 测试直方图聚合
  
  let response_times = [
    10.5, 15.2, 8.7, 25.1, 12.3, 
    18.9, 7.2, 22.4, 14.6, 19.8
  ]
  
  // 定义桶边界
  let bucket_bounds = [5.0, 10.0, 15.0, 20.0, 25.0, 30.0]
  let mut bucket_counts = [0, 0, 0, 0, 0, 0, 0]  // 包括inf桶
  
  // 将响应时间分配到桶中
  let mut i = 0
  while i < response_times.length() {
    let value = response_times[i]
    let mut bucket_index = 0
    
    // 找到合适的桶
    let mut j = 0
    while j < bucket_bounds.length() {
      if value <= bucket_bounds[j] {
        bucket_index = j
        break
      }
      j = j + 1
    }
    
    // 如果超过所有边界，放入inf桶
    if bucket_index == 0 && value > bucket_bounds[bucket_bounds.length() - 1] {
      bucket_index = bucket_bounds.length()
    }
    
    // 增加该桶及之前所有桶的计数
    let mut k = bucket_index
    while k < bucket_counts.length() {
      bucket_counts[k] = bucket_counts[k] + 1
      k = k + 1
    }
    
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 2)  // <=5.0: 无
  assert_eq(bucket_counts[1], 3)  // <=10.0: 8.7, 7.2
  assert_eq(bucket_counts[2], 6)  // <=15.0: 10.5, 15.2, 12.3, 14.6
  assert_eq(bucket_counts[3], 8)  // <=20.0: 18.9, 19.8
  assert_eq(bucket_counts[4], 9)  // <=25.0: 25.1
  assert_eq(bucket_counts[5], 10) // <=30.0: 22.4
  assert_eq(bucket_counts[6], 10) // <=inf: 所有
  
  // 验证总数
  assert_eq(bucket_counts[bucket_counts.length() - 1], response_times.length())
}

test "telemetry_percentile_calculation" {
  // 测试百分位数计算
  
  let sorted_values = [
    1.0, 2.0, 3.0, 4.0, 5.0, 
    6.0, 7.0, 8.0, 9.0, 10.0,
    11.0, 12.0, 13.0, 14.0, 15.0,
    16.0, 17.0, 18.0, 19.0, 20.0
  ]
  
  let total_count = sorted_values.length()
  
  // 计算50百分位数（中位数）
  let p50_index = (50 * total_count) / 100
  let p50_value = if p50_index < total_count {
    sorted_values[p50_index]
  } else {
    sorted_values[total_count - 1]
  }
  
  // 计算95百分位数
  let p95_index = (95 * total_count) / 100
  let p95_value = if p95_index < total_count {
    sorted_values[p95_index]
  } else {
    sorted_values[total_count - 1]
  }
  
  // 计算99百分位数
  let p99_index = (99 * total_count) / 100
  let p99_value = if p99_index < total_count {
    sorted_values[p99_index]
  } else {
    sorted_values[total_count - 1]
  }
  
  // 验证百分位数
  assert_eq(p50_value, 10.0)
  assert_eq(p95_value, 19.0)
  assert_eq(p99_value, 20.0)
  
  // 验证百分位数顺序
  assert_eq(p50_value <= p95_value, true)
  assert_eq(p95_value <= p99_value, true)
}

test "telemetry_rate_calculation" {
  // 测试速率计算
  
  let time_series_data = [
    (1704067200L, 1000),  // 0秒
    (1704067260L, 1050),  // 60秒后
    (1704067320L, 1120),  // 120秒后
    (1704067380L, 1180),  // 180秒后
    (1704067440L, 1250)   // 240秒后
  ]
  
  // 计算时间间隔内的速率
  let mut rates = []
  let mut i = 1
  while i < time_series_data.length() {
    let prev_data = time_series_data[i - 1]
    let curr_data = time_series_data[i]
    
    let time_diff = curr_data.0 - prev_data.0
    let value_diff = curr_data.1 - prev_data.1
    let rate = value_diff.to_double() / time_diff.to_double()
    
    rates.push(rate)
    i = i + 1
  }
  
  // 验证速率计算
  assert_eq(rates.length(), 4)
  assert_eq(rates[0] > 0.8, true)  // (1050-1000)/60 ≈ 0.83
  assert_eq(rates[0] < 0.9, true)
  
  assert_eq(rates[1] > 1.0, true)  // (1120-1050)/60 ≈ 1.17
  assert_eq(rates[1] < 1.2, true)
  
  assert_eq(rates[2] > 0.9, true)  // (1180-1120)/60 = 1.0
  assert_eq(rates[2] < 1.1, true)
  
  assert_eq(rates[3] > 1.0, true)  // (1250-1180)/60 ≈ 1.17
  assert_eq(rates[3] < 1.2, true)
  
  // 计算平均速率
  let mut total_rate = 0.0
  i = 0
  while i < rates.length() {
    total_rate = total_rate + rates[i]
    i = i + 1
  }
  let average_rate = total_rate / rates.length()
  
  assert_eq(average_rate > 0.9, true)
  assert_eq(average_rate < 1.1, true)
}

test "telemetry_time_bucket_aggregation" {
  // 测试时间桶聚合
  
  let metrics_with_timestamps = [
    ("cpu_usage", 25.5, 1704067200L),  // 00:00
    ("cpu_usage", 30.2, 1704067260L),  // 00:01
    ("cpu_usage", 28.7, 1704067320L),  // 00:02
    ("cpu_usage", 35.1, 1704067380L),  // 00:03
    ("cpu_usage", 27.8, 1704067440L),  // 00:04
    ("cpu_usage", 32.4, 1704067500L),  // 00:05
    ("cpu_usage", 29.6, 1704067560L),  // 00:06
    ("cpu_usage", 31.2, 1704067620L)   // 00:07
  ]
  
  let bucket_size = 300L  // 5分钟桶
  let base_time = 1704067200L  // 基准时间
  
  // 创建时间桶
  let mut time_buckets = []
  let mut i = 0
  while i < metrics_with_timestamps.length() {
    let metric = metrics_with_timestamps[i]
    let timestamp = metric.2
    let bucket_index = (timestamp - base_time) / bucket_size
    
    // 确保桶数组足够大
    while time_buckets.length() <= bucket_index.to_int() {
      time_buckets.push([])
    }
    
    // 将指标添加到对应桶
    time_buckets[bucket_index.to_int()].push(metric)
    i = i + 1
  }
  
  // 验证时间桶分布
  assert_eq(time_buckets.length(), 2)  // 两个5分钟桶
  
  // 第一个桶 (00:00-00:05): 6个数据点
  assert_eq(time_buckets[0].length(), 6)
  
  // 第二个桶 (00:05-00:10): 2个数据点
  assert_eq(time_buckets[1].length(), 2)
  
  // 计算每个桶的平均值
  let mut bucket_averages = []
  i = 0
  while i < time_buckets.length() {
    let bucket = time_buckets[i]
    let mut sum = 0.0
    
    let mut j = 0
    while j < bucket.length() {
      sum = sum + bucket[j].1
      j = j + 1
    }
    
    let average = sum / bucket.length().to_double()
    bucket_averages.push(average)
    i = i + 1
  }
  
  // 验证桶平均值
  assert_eq(bucket_averages[0] > 29.0, true)  // 第一个桶平均值
  assert_eq(bucket_averages[0] < 31.0, true)
  
  assert_eq(bucket_averages[1] > 30.0, true)  // 第二个桶平均值
  assert_eq(bucket_averages[1] < 31.0, true)
}

test "telemetry_dimension_aggregation" {
  // 测试维度聚合
  
  let multi_dimensional_metrics = [
    ("http_requests", "GET", "/api/users", 200, 120),
    ("http_requests", "GET", "/api/users", 200, 95),
    ("http_requests", "POST", "/api/orders", 201, 45),
    ("http_requests", "POST", "/api/orders", 400, 8),
    ("http_requests", "GET", "/api/products", 200, 230),
    ("http_requests", "GET", "/api/products", 404, 15),
    ("http_requests", "PUT", "/api/users/123", 200, 28),
    ("http_requests", "DELETE", "/api/orders/456", 204, 12)
  ]
  
  // 按HTTP方法聚合
  let mut method_aggregates = {}
  let mut i = 0
  while i < multi_dimensional_metrics.length() {
    let metric = multi_dimensional_metrics[i]
    let method = metric.1
    let count = metric.4
    
    if method_aggregates.contains_key(method) {
      method_aggregates[method] = method_aggregates[method] + count
    } else {
      method_aggregates[method] = count
    }
    i = i + 1
  }
  
  // 验证方法聚合
  assert_eq(method_aggregates["GET"], 120 + 95 + 230 + 15)  // 460
  assert_eq(method_aggregates["POST"], 45 + 8)              // 53
  assert_eq(method_aggregates["PUT"], 28)                    // 28
  assert_eq(method_aggregates["DELETE"], 12)                 // 12
  
  // 按状态码聚合
  let mut status_aggregates = {}
  i = 0
  while i < multi_dimensional_metrics.length() {
    let metric = multi_dimensional_metrics[i]
    let status = metric.3.to_string()
    let count = metric.4
    
    if status_aggregates.contains_key(status) {
      status_aggregates[status] = status_aggregates[status] + count
    } else {
      status_aggregates[status] = count
    }
    i = i + 1
  }
  
  // 验证状态码聚合
  assert_eq(status_aggregates["200"], 120 + 95 + 230 + 28)  // 473
  assert_eq(status_aggregates["201"], 45)                    // 45
  assert_eq(status_aggregates["204"], 12)                    // 12
  assert_eq(status_aggregates["400"], 8)                     // 8
  assert_eq(status_aggregates["404"], 15)                    // 15
  
  // 按端点聚合
  let mut endpoint_aggregates = {}
  i = 0
  while i < multi_dimensional_metrics.length() {
    let metric = multi_dimensional_metrics[i]
    let endpoint = metric.2
    let count = metric.4
    
    if endpoint_aggregates.contains_key(endpoint) {
      endpoint_aggregates[endpoint] = endpoint_aggregates[endpoint] + count
    } else {
      endpoint_aggregates[endpoint] = count
    }
    i = i + 1
  }
  
  // 验证端点聚合
  assert_eq(endpoint_aggregates["/api/users"], 120 + 95 + 28)     // 243
  assert_eq(endpoint_aggregates["/api/orders"], 45 + 8 + 12)     // 65
  assert_eq(endpoint_aggregates["/api/products"], 230 + 15)      // 245
  assert_eq(endpoint_aggregates["/api/users/123"], 28)           // 28
  assert_eq(endpoint_aggregates["/api/orders/456"], 12)          // 12
}

test "telemetry_exponential_moving_average" {
  // 测试指数移动平均
  
  let metric_values = [10.0, 15.0, 12.0, 18.0, 14.0, 20.0, 16.0, 22.0]
  let alpha = 0.3  // 平滑因子
  
  // 计算指数移动平均
  let mut ema_values = []
  let mut ema = metric_values[0]  // 初始EMA为第一个值
  ema_values.push(ema)
  
  let mut i = 1
  while i < metric_values.length() {
    let current_value = metric_values[i]
    ema = alpha * current_value + (1.0 - alpha) * ema
    ema_values.push(ema)
    i = i + 1
  }
  
  // 验证EMA计算
  assert_eq(ema_values.length(), metric_values.length())
  assert_eq(ema_values[0], 10.0)  // 初始值
  
  // 手动计算几个值进行验证
  // EMA[1] = 0.3 * 15.0 + 0.7 * 10.0 = 4.5 + 7.0 = 11.5
  assert_eq(ema_values[1] > 11.4, true)
  assert_eq(ema_values[1] < 11.6, true)
  
  // EMA[2] = 0.3 * 12.0 + 0.7 * 11.5 = 3.6 + 8.05 = 11.65
  assert_eq(ema_values[2] > 11.6, true)
  assert_eq(ema_values[2] < 11.7, true)
  
  // 验证EMA的平滑特性
  let mut max_diff = 0.0
  i = 1
  while i < metric_values.length() {
    let raw_diff = (metric_values[i] - metric_values[i-1]).abs()
    let ema_diff = (ema_values[i] - ema_values[i-1]).abs()
    
    if ema_diff > max_diff {
      max_diff = ema_diff
    }
    
    // EMA的变化应该小于原始数据的变化
    assert_eq(ema_diff <= raw_diff, true)
    i = i + 1
  }
  
  // 验证EMA在合理范围内
  let min_value = metric_values[0]
  let max_value = metric_values[0]
  i = 1
  while i < metric_values.length() {
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    i = i + 1
  }
  
  i = 0
  while i < ema_values.length() {
    assert_eq(ema_values[i] >= min_value, true)
    assert_eq(ema_values[i] <= max_value, true)
    i = i + 1
  }
}