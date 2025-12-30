// 指标聚合和统计测试用例
// 测试Azimuth Telemetry指标聚合和统计功能

test "counter_metric_aggregation" {
  // 测试计数器指标聚合
  
  let metric_name = "http_requests_total"
  let metric_values = [10L, 15L, 25L, 30L, 20L]
  let metric_labels = [
    [("method", "GET"), ("status", "200")],
    [("method", "POST"), ("status", "201")],
    [("method", "GET"), ("status", "404")],
    [("method", "PUT"), ("status", "200")],
    [("method", "DELETE"), ("status", "204")]
  ]
  
  assert_eq(metric_values.length(), metric_labels.length())
  assert_eq(metric_values.length(), 5)
  
  // 计算总和
  let mut total = 0L
  let mut i = 0
  while i < metric_values.length() {
    total = total + metric_values[i]
    i = i + 1
  }
  
  assert_eq(total, 100L)
  
  // 按状态码分组聚合
  let mut success_total = 0L
  let mut client_error_total = 0L
  i = 0
  while i < metric_labels.length() {
    let labels = metric_labels[i]
    let mut status = ""
    let mut j = 0
    while j < labels.length() {
      let (key, value) = labels[j]
      if key == "status" {
        status = value
      }
      j = j + 1
    }
    
    if status == "200" || status == "201" || status == "204" {
      success_total = success_total + metric_values[i]
    } else if status == "404" {
      client_error_total = client_error_total + metric_values[i]
    }
    i = i + 1
  }
  
  assert_eq(success_total, 85L)
  assert_eq(client_error_total, 15L)
  
  // 按方法分组聚合
  let mut get_total = 0L
  let mut post_total = 0L
  i = 0
  while i < metric_labels.length() {
    let labels = metric_labels[i]
    let mut method = ""
    let mut j = 0
    while j < labels.length() {
      let (key, value) = labels[j]
      if key == "method" {
        method = value
      }
      j = j + 1
    }
    
    if method == "GET" {
      get_total = get_total + metric_values[i]
    } else if method == "POST" {
      post_total = post_total + metric_values[i]
    }
    i = i + 1
  }
  
  assert_eq(get_total, 35L)
  assert_eq(post_total, 15L)
}

test "histogram_metric_statistics" {
  // 测试直方图指标统计
  
  let metric_name = "request_duration_seconds"
  let bucket_bounds = [0.1, 0.5, 1.0, 2.5, 5.0, 10.0]
  let bucket_counts = [5L, 25L, 60L, 85L, 95L, 100L]
  let sum = 187.5
  let count = 100L
  
  assert_eq(bucket_bounds.length(), bucket_counts.length())
  assert_eq(bucket_bounds.length(), 6)
  
  // 验证bucket递增
  let mut i = 1
  while i < bucket_bounds.length() {
    assert_eq(bucket_bounds[i] > bucket_bounds[i-1], true)
    i = i + 1
  }
  
  // 验证bucket计数递增
  i = 1
  while i < bucket_counts.length() {
    assert_eq(bucket_counts[i] >= bucket_counts[i-1], true)
    i = i + 1
  }
  
  // 计算每个区间的计数
  let interval_counts = [
    bucket_counts[0], // <= 0.1
    bucket_counts[1] - bucket_counts[0], // (0.1, 0.5]
    bucket_counts[2] - bucket_counts[1], // (0.5, 1.0]
    bucket_counts[3] - bucket_counts[2], // (1.0, 2.5]
    bucket_counts[4] - bucket_counts[3], // (2.5, 5.0]
    bucket_counts[5] - bucket_counts[4], // (5.0, 10.0]
    count - bucket_counts[5] // > 10.0
  ]
  
  assert_eq(interval_counts[0], 5L)
  assert_eq(interval_counts[1], 20L)
  assert_eq(interval_counts[2], 35L)
  assert_eq(interval_counts[3], 25L)
  assert_eq(interval_counts[4], 10L)
  assert_eq(interval_counts[5], 5L)
  assert_eq(interval_counts[6], 0L)
  
  // 验证总和
  let mut interval_sum = 0L
  i = 0
  while i < interval_counts.length() {
    interval_sum = interval_sum + interval_counts[i]
    i = i + 1
  }
  assert_eq(interval_sum, count)
  
  // 计算平均值
  let average = sum / count.to_double()
  assert_eq(average > 1.5, true)
  assert_eq(average < 2.0, true)
  
  // 计算中位数近似值（基于bucket）
  let median_bucket_index = 0
  i = 0
  while i < bucket_counts.length() {
    if bucket_counts[i] >= count / 2L {
      median_bucket_index = i
      break
    }
    i = i + 1
  }
  
  assert_eq(median_bucket_index, 2) // 1.0 bucket
  assert_eq(bucket_bounds[median_bucket_index], 1.0)
}

test "gauge_metric_fluctuation" {
  // 测试仪表指标波动
  
  let metric_name = "cpu_usage_percent"
  let metric_values = [25.5, 30.2, 45.8, 60.1, 55.3, 40.7, 35.2, 28.9]
  let timestamps = [
    1640995200L,
    1640995260L,
    1640995320L,
    1640995380L,
    1640995440L,
    1640995500L,
    1640995560L,
    1640995620L
  ]
  
  assert_eq(metric_values.length(), timestamps.length())
  assert_eq(metric_values.length(), 8)
  
  // 计算最大值和最小值
  let mut max_value = metric_values[0]
  let mut min_value = metric_values[0]
  let mut i = 1
  while i < metric_values.length() {
    if metric_values[i] > max_value {
      max_value = metric_values[i]
    }
    if metric_values[i] < min_value {
      min_value = metric_values[i]
    }
    i = i + 1
  }
  
  assert_eq(max_value, 60.1)
  assert_eq(min_value, 25.5)
  
  // 计算范围
  let range = max_value - min_value
  assert_eq(range, 34.6)
  
  // 计算平均值
  let mut sum = 0.0
  i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  let average = sum / metric_values.length().to_double()
  assert_eq(average > 35.0, true)
  assert_eq(average < 45.0, true)
  
  // 计算变化率
  let mut max_increase = 0.0
  let mut max_decrease = 0.0
  i = 1
  while i < metric_values.length() {
    let change = metric_values[i] - metric_values[i-1]
    if change > max_increase {
      max_increase = change
    }
    if change < max_decrease {
      max_decrease = change
    }
    i = i + 1
  }
  
  assert_eq(max_increase > 14.0, true) // 45.8 - 30.2 = 15.6
  assert_eq(max_decrease < -10.0, true) // 60.1 - 55.3 = -4.8, 55.3 - 40.7 = -14.6
  
  // 验证时间戳递增
  i = 1
  while i < timestamps.length() {
    assert_eq(timestamps[i] > timestamps[i-1], true)
    i = i + 1
  }
  
  // 计算时间间隔
  let time_interval = timestamps[1] - timestamps[0]
  assert_eq(time_interval, 60L) // 1分钟间隔
}

test "metric_rate_calculation" {
  // 测试指标速率计算
  
  let counter_name = "operations_total"
  let counter_values = [100L, 150L, 250L, 400L, 550L, 700L, 900L, 1100L]
  let timestamps = [
    1640995200L, // 0分钟
    1640995260L, // 1分钟
    1640995320L, // 2分钟
    1640995380L, // 3分钟
    1640995440L, // 4分钟
    1640995500L, // 5分钟
    1640995560L, // 6分钟
    1640995620L  // 7分钟
  ]
  
  assert_eq(counter_values.length(), timestamps.length())
  
  // 计算每分钟速率
  let rates = []
  let mut i = 1
  while i < counter_values.length() {
    let value_diff = counter_values[i] - counter_values[i-1]
    let time_diff = timestamps[i] - timestamps[i-1]
    let rate = value_diff.to_double() / time_diff.to_double() * 60.0 // 转换为每分钟
    rates.push(rate)
    i = i + 1
  }
  
  assert_eq(rates.length(), 7)
  
  // 验证速率计算
  assert_eq(rates[0], 50.0)   // (150-100)/60*60
  assert_eq(rates[1], 100.0)  // (250-150)/60*60
  assert_eq(rates[2], 150.0)  // (400-250)/60*60
  assert_eq(rates[3], 150.0)  // (550-400)/60*60
  assert_eq(rates[4], 150.0)  // (700-550)/60*60
  assert_eq(rates[5], 200.0)  // (900-700)/60*60
  assert_eq(rates[6], 200.0)  // (1100-900)/60*60
  
  // 计算平均速率
  let mut rate_sum = 0.0
  i = 0
  while i < rates.length() {
    rate_sum = rate_sum + rates[i]
    i = i + 1
  }
  let average_rate = rate_sum / rates.length().to_double()
  assert_eq(average_rate > 140.0, true)
  assert_eq(average_rate < 145.0, true)
  
  // 计算最大和最小速率
  let mut max_rate = rates[0]
  let mut min_rate = rates[0]
  i = 1
  while i < rates.length() {
    if rates[i] > max_rate {
      max_rate = rates[i]
    }
    if rates[i] < min_rate {
      min_rate = rates[i]
    }
    i = i + 1
  }
  
  assert_eq(max_rate, 200.0)
  assert_eq(min_rate, 50.0)
}

test "metric_percentile_calculation" {
  // 测试指标百分位数计算
  
  let response_times = [
    0.05, 0.08, 0.12, 0.15, 0.18, 0.22, 0.25, 0.28, 0.32, 0.35,
    0.38, 0.42, 0.45, 0.48, 0.52, 0.55, 0.58, 0.62, 0.65, 0.68,
    0.72, 0.75, 0.78, 0.82, 0.85, 0.88, 0.92, 0.95, 0.98, 1.02,
    1.05, 1.08, 1.12, 1.15, 1.18, 1.22, 1.25, 1.28, 1.32, 1.35,
    1.38, 1.42, 1.45, 1.48, 1.52, 1.55, 1.58, 1.62, 1.65, 1.68
  ]
  
  assert_eq(response_times.length(), 50)
  
  // 验证数据已排序
  let mut i = 1
  while i < response_times.length() {
    assert_eq(response_times[i] >= response_times[i-1], true)
    i = i + 1
  }
  
  // 计算百分位数
  let p50_index = (response_times.length() * 50) / 100
  let p90_index = (response_times.length() * 90) / 100
  let p95_index = (response_times.length() * 95) / 100
  let p99_index = (response_times.length() * 99) / 100
  
  // 调整索引（从0开始）
  let p50_value = response_times[p50_index - 1]
  let p90_value = response_times[p90_index - 1]
  let p95_value = response_times[p95_index - 1]
  let p99_value = response_times[p99_index - 1]
  
  assert_eq(p50_value, 0.72)  // 第25个值
  assert_eq(p90_value, 1.52)  // 第45个值
  assert_eq(p95_value, 1.62)  // 第48个值
  assert_eq(p99_value, 1.68)  // 第50个值
  
  // 验证百分位数递增
  assert_eq(p50_value < p90_value, true)
  assert_eq(p90_value < p95_value, true)
  assert_eq(p95_value < p99_value, true)
  
  // 计算平均值
  let mut sum = 0.0
  i = 0
  while i < response_times.length() {
    sum = sum + response_times[i]
    i = i + 1
  }
  let average = sum / response_times.length().to_double()
  assert_eq(average > 0.8, true)
  assert_eq(average < 1.0, true)
  
  // 验证平均值在P50和P90之间
  assert_eq(average > p50_value, true)
  assert_eq(average < p90_value, true)
}

test "metric_aggregation_with_labels" {
  // 测试带标签的指标聚合
  
  let metric_data = [
    {
      name: "http_request_duration_seconds",
      value: 0.25,
      labels: [("method", "GET"), ("status", "200"), ("endpoint", "/api/users")]
    },
    {
      name: "http_request_duration_seconds",
      value: 0.45,
      labels: [("method", "POST"), ("status", "201"), ("endpoint", "/api/users")]
    },
    {
      name: "http_request_duration_seconds",
      value: 0.15,
      labels: [("method", "GET"), ("status", "200"), ("/api/users")]
    },
    {
      name: "http_request_duration_seconds",
      value: 1.25,
      labels: [("method", "GET"), ("status", "404"), ("/api/orders")]
    },
    {
      name: "http_request_duration_seconds",
      value: 0.85,
      labels: [("method", "PUT"), ("status", "200"), ("/api/orders")]
    }
  ]
  
  assert_eq(metric_data.length(), 5)
  
  // 按方法分组聚合
  let method_stats = {
    "GET": { count: 0, sum: 0.0, min: 999.0, max: 0.0 },
    "POST": { count: 0, sum: 0.0, min: 999.0, max: 0.0 },
    "PUT": { count: 0, sum: 0.0, min: 999.0, max: 0.0 }
  }
  
  let mut i = 0
  while i < metric_data.length() {
    let data = metric_data[i]
    let mut method = ""
    let mut j = 0
    while j < data.labels.length() {
      let (key, value) = data.labels[j]
      if key == "method" {
        method = value
      }
      j = j + 1
    }
    
    // 更新统计信息
    match method {
      "GET" => {
        method_stats["GET"].count = method_stats["GET"].count + 1
        method_stats["GET"].sum = method_stats["GET"].sum + data.value
        if data.value < method_stats["GET"].min {
          method_stats["GET"].min = data.value
        }
        if data.value > method_stats["GET"].max {
          method_stats["GET"].max = data.value
        }
      }
      "POST" => {
        method_stats["POST"].count = method_stats["POST"].count + 1
        method_stats["POST"].sum = method_stats["POST"].sum + data.value
        method_stats["POST"].min = data.value
        method_stats["POST"].max = data.value
      }
      "PUT" => {
        method_stats["PUT"].count = method_stats["PUT"].count + 1
        method_stats["PUT"].sum = method_stats["PUT"].sum + data.value
        method_stats["PUT"].min = data.value
        method_stats["PUT"].max = data.value
      }
      _ => {}
    }
    i = i + 1
  }
  
  // 验证GET方法统计
  assert_eq(method_stats["GET"].count, 3)
  assert_eq(method_stats["GET"].sum > 1.5, true)
  assert_eq(method_stats["GET"].min, 0.15)
  assert_eq(method_stats["GET"].max, 1.25)
  
  // 验证POST方法统计
  assert_eq(method_stats["POST"].count, 1)
  assert_eq(method_stats["POST"].sum, 0.45)
  assert_eq(method_stats["POST"].min, 0.45)
  assert_eq(method_stats["POST"].max, 0.45)
  
  // 验证PUT方法统计
  assert_eq(method_stats["PUT"].count, 1)
  assert_eq(method_stats["PUT"].sum, 0.85)
  assert_eq(method_stats["PUT"].min, 0.85)
  assert_eq(method_stats["PUT"].max, 0.85)
  
  // 计算各方法平均值
  let get_average = method_stats["GET"].sum / method_stats["GET"].count.to_double()
  let post_average = method_stats["POST"].sum / method_stats["POST"].count.to_double()
  let put_average = method_stats["PUT"].sum / method_stats["PUT"].count.to_double()
  
  assert_eq(get_average > 0.5, true)
  assert_eq(post_average, 0.45)
  assert_eq(put_average, 0.85)
}