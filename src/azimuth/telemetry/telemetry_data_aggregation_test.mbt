// 遥测数据聚合测试用例
// 测试遥测数据的聚合、统计和分析功能

test "metric_sum_aggregation" {
  // 测试指标求和聚合
  
  let metric_values = [10.5, 15.2, 8.7, 12.3, 9.8, 11.1, 14.6, 7.9, 13.4, 10.0]
  let metric_name = "response_time_ms"
  let metric_unit = "milliseconds"
  
  // 计算总和
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证总和计算
  assert_eq(sum > 100.0, true)
  assert_eq(sum < 120.0, true)
  assert_eq(metric_values.length(), 10)
  
  // 验证特定值
  assert_eq(metric_values[0], 10.5)
  assert_eq(metric_values[9], 10.0)
  assert_eq(metric_values[3], 12.3)
  
  // 创建聚合结果
  let aggregation_result = {
    "metric_name": metric_name,
    "metric_unit": metric_unit,
    "aggregation_type": "sum",
    "value": sum,
    "count": metric_values.length(),
    "timestamp": 1640995200L
  }
  
  // 验证聚合结果
  assert_eq(aggregation_result["metric_name"], "response_time_ms")
  assert_eq(aggregation_result["metric_unit"], "milliseconds")
  assert_eq(aggregation_result["aggregation_type"], "sum")
  assert_eq(aggregation_result["count"], 10)
  assert_eq(aggregation_result["timestamp"], 1640995200L)
}

test "metric_average_aggregation" {
  // 测试指标平均值聚合
  
  let metric_values = [25.0, 30.0, 20.0, 35.0, 40.0, 15.0, 45.0, 50.0, 10.0, 60.0]
  let metric_name = "cpu_usage_percent"
  
  // 计算总和和平均值
  let mut sum = 0.0
  let mut i = 0
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  let average = sum / metric_values.length().to_double()
  
  // 验证平均值计算
  assert_eq(average, 33.0)
  assert_eq(average > 30.0, true)
  assert_eq(average < 35.0, true)
  
  // 验证总和
  assert_eq(sum, 330.0)
  
  // 验证极值
  let mut min_value = metric_values[0]
  let mut max_value = metric_values[0]
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
  
  assert_eq(min_value, 10.0)
  assert_eq(max_value, 60.0)
  
  // 创建统计聚合结果
  let stats_result = {
    "metric_name": metric_name,
    "count": metric_values.length(),
    "sum": sum,
    "average": average,
    "min": min_value,
    "max": max_value,
    "range": max_value - min_value
  }
  
  // 验证统计结果
  assert_eq(stats_result["metric_name"], "cpu_usage_percent")
  assert_eq(stats_result["count"], 10)
  assert_eq(stats_result["sum"], 330.0)
  assert_eq(stats_result["average"], 33.0)
  assert_eq(stats_result["min"], 10.0)
  assert_eq(stats_result["max"], 60.0)
  assert_eq(stats_result["range"], 50.0)
}

test "metric_percentile_aggregation" {
  // 测试指标百分位数聚合
  
  // 创建已排序的指标值
  let metric_values = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 16.0, 17.0, 18.0, 19.0, 20.0]
  let metric_name = "latency_ms"
  
  // 计算百分位数
  let calculate_percentile = fn(values : Array[Double], percentile : Double) -> Double {
    let index = (percentile / 100.0) * (values.length().to_double() - 1.0)
    let lower_index = index.floor().to_int()
    let upper_index = index.ceil().to_int()
    
    if lower_index == upper_index {
      values[lower_index]
    } else {
      let weight = index - lower_index.to_double()
      values[lower_index] * (1.0 - weight) + values[upper_index] * weight
    }
  }
  
  // 计算各种百分位数
  let p50 = calculate_percentile(metric_values, 50.0)
  let p90 = calculate_percentile(metric_values, 90.0)
  let p95 = calculate_percentile(metric_values, 95.0)
  let p99 = calculate_percentile(metric_values, 99.0)
  
  // 验证百分位数
  assert_eq(p50, 10.5) // 中位数
  assert_eq(p90, 18.1)
  assert_eq(p95, 19.05)
  assert_eq(p99, 19.81)
  
  // 验证边界条件
  let p0 = calculate_percentile(metric_values, 0.0)
  let p100 = calculate_percentile(metric_values, 100.0)
  assert_eq(p0, 1.0)
  assert_eq(p100, 20.0)
  
  // 创建百分位数聚合结果
  let percentile_result = {
    "metric_name": metric_name,
    "count": metric_values.length(),
    "p50": p50,
    "p90": p90,
    "p95": p95,
    "p99": p99,
    "min": p0,
    "max": p100
  }
  
  // 验证百分位数结果
  assert_eq(percentile_result["metric_name"], "latency_ms")
  assert_eq(percentile_result["count"], 20)
  assert_eq(percentile_result["p50"], 10.5)
  assert_eq(percentile_result["p90"], 18.1)
  assert_eq(percentile_result["p95"], 19.05)
  assert_eq(percentile_result["p99"], 19.81)
}

test "histogram_aggregation" {
  // 测试直方图聚合
  
  let metric_values = [2.5, 8.7, 15.2, 3.1, 12.6, 7.9, 18.4, 4.3, 9.8, 14.1, 6.2, 11.5, 16.7, 5.4, 13.9, 1.8, 17.3, 10.7, 19.6, 20.0]
  let bucket_bounds = [5.0, 10.0, 15.0, 20.0]
  let metric_name = "request_duration"
  
  // 初始化桶计数
  let bucket_counts = [0, 0, 0, 0, 0] // 包括最后一个桶（大于等于最大边界）
  
  // 将值分配到桶中
  let mut i = 0
  while i < metric_values.length() {
    let value = metric_values[i]
    let mut bucket_index = bucket_bounds.length() // 默认到最后一个桶
    
    let mut j = 0
    while j < bucket_bounds.length() {
      if value < bucket_bounds[j] {
        bucket_index = j
        break
      }
      j = j + 1
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 4) // < 5.0: [2.5, 3.1, 4.3, 1.8]
  assert_eq(bucket_counts[1], 4) // 5.0-9.99: [8.7, 7.9, 9.8, 6.2]
  assert_eq(bucket_counts[2], 5) // 10.0-14.99: [15.2, 12.6, 11.5, 13.9, 10.7]
  assert_eq(bucket_counts[3], 4) // 15.0-19.99: [18.4, 16.7, 17.3, 19.6]
  assert_eq(bucket_counts[4], 3) // >= 20.0: [20.0] + 其他边界值
  
  // 验证总计数
  let mut total_count = 0
  i = 0
  while i < bucket_counts.length() {
    total_count = total_count + bucket_counts[i]
    i = i + 1
  }
  assert_eq(total_count, metric_values.length())
  
  // 创建直方图聚合结果
  let histogram_result = {
    "metric_name": metric_name,
    "bucket_bounds": bucket_bounds,
    "bucket_counts": bucket_counts,
    "total_count": total_count
  }
  
  // 验证直方图结果
  assert_eq(histogram_result["metric_name"], "request_duration")
  assert_eq(histogram_result["bucket_bounds"].length(), 4)
  assert_eq(histogram_result["bucket_counts"].length(), 5)
  assert_eq(histogram_result["total_count"], 20)
}

test "time_window_aggregation" {
  // 测试时间窗口聚合
  
  let time_series_data = [
    (1640995200L, 100.0), // 2022-01-01 00:00:00
    (1640995260L, 120.0), // 2022-01-01 00:01:00
    (1640995320L, 90.0),  // 2022-01-01 00:02:00
    (1640995380L, 110.0), // 2022-01-01 00:03:00
    (1640995440L, 130.0), // 2022-01-01 00:04:00
    (1640995500L, 80.0),  // 2022-01-01 00:05:00
    (1640995560L, 95.0),  // 2022-01-01 00:06:00
    (1640995620L, 115.0), // 2022-01-01 00:07:00
    (1640995680L, 105.0), // 2022-01-01 00:08:00
    (1640995740L, 125.0)  // 2022-01-01 00:09:00
  ]
  
  let window_size = 300L // 5分钟窗口
  let metric_name = "throughput_ops_per_sec"
  
  // 按时间窗口聚合
  let windowed_data = []
  let mut i = 0
  while i < time_series_data.length() {
    let (timestamp, value) = time_series_data[i]
    let window_start = (timestamp / window_size) * window_size
    
    // 查找或创建窗口
    let mut window_found = false
    let mut j = 0
    while j < windowed_data.length() {
      if windowed_data[j].0 == window_start {
        // 更新现有窗口
        let current_sum = windowed_data[j].1
        let current_count = windowed_data[j].2
        windowed_data[j] = (window_start, current_sum + value, current_count + 1)
        window_found = true
        break
      }
      j = j + 1
    }
    
    // 创建新窗口
    if not window_found {
      windowed_data.push((window_start, value, 1))
    }
    
    i = i + 1
  }
  
  // 验证窗口聚合结果
  assert_eq(windowed_data.length(), 2) // 应该有2个5分钟窗口
  
  // 验证第一个窗口（00:00-00:05）
  assert_eq(windowed_data[0].0, 1640995200L)
  assert_eq(windowed_data[0].2, 5) // 5个数据点
  assert_eq(windowed_data[0].1, 550.0) // 总和
  
  // 验证第二个窗口（00:05-00:10）
  assert_eq(windowed_data[1].0, 1640995500L)
  assert_eq(windowed_data[1].2, 5) // 5个数据点
  assert_eq(windowed_data[1].1, 520.0) // 总和
  
  // 计算窗口平均值
  let window_averages = []
  i = 0
  while i < windowed_data.length() {
    let (window_start, sum, count) = windowed_data[i]
    let average = sum / count.to_double()
    window_averages.push((window_start, average))
    i = i + 1
  }
  
  // 验证窗口平均值
  assert_eq(window_averages[0].1, 110.0) // 550.0 / 5
  assert_eq(window_averages[1].1, 104.0) // 520.0 / 5
}

test "dimension_aggregation" {
  // 测试维度聚合
  
  let dimensional_data = [
    ("GET", "/api/users", 200, 120.5),
    ("POST", "/api/users", 201, 250.8),
    ("GET", "/api/users", 404, 15.2),
    ("PUT", "/api/users", 200, 180.3),
    ("DELETE", "/api/users", 204, 95.7),
    ("GET", "/api/orders", 200, 200.1),
    ("POST", "/api/orders", 201, 350.6),
    ("GET", "/api/orders", 400, 25.4),
    ("GET", "/api/products", 200, 80.9),
    ("POST", "/api/products", 201, 420.2)
  ]
  
  // 按HTTP方法聚合
  let method_aggregation = {}
  let mut i = 0
  while i < dimensional_data.length() {
    let (method, endpoint, status, duration) = dimensional_data[i]
    
    if method_aggregation.contains(method) {
      let current_data = method_aggregation[method]
      method_aggregation[method] = {
        "count": current_data["count"] + 1,
        "total_duration": current_data["total_duration"] + duration,
        "avg_duration": (current_data["total_duration"] + duration) / (current_data["count"] + 1).to_double()
      }
    } else {
      method_aggregation[method] = {
        "count": 1,
        "total_duration": duration,
        "avg_duration": duration
      }
    }
    
    i = i + 1
  }
  
  // 验证方法聚合结果
  assert_eq(method_aggregation["GET"]["count"], 5)
  assert_eq(method_aggregation["POST"]["count"], 3)
  assert_eq(method_aggregation["PUT"]["count"], 1)
  assert_eq(method_aggregation["DELETE"]["count"], 1)
  
  // 按端点聚合
  let endpoint_aggregation = {}
  i = 0
  while i < dimensional_data.length() {
    let (method, endpoint, status, duration) = dimensional_data[i]
    
    if endpoint_aggregation.contains(endpoint) {
      let current_data = endpoint_aggregation[endpoint]
      endpoint_aggregation[endpoint] = {
        "count": current_data["count"] + 1,
        "total_duration": current_data["total_duration"] + duration,
        "avg_duration": (current_data["total_duration"] + duration) / (current_data["count"] + 1).to_double()
      }
    } else {
      endpoint_aggregation[endpoint] = {
        "count": 1,
        "total_duration": duration,
        "avg_duration": duration
      }
    }
    
    i = i + 1
  }
  
  // 验证端点聚合结果
  assert_eq(endpoint_aggregation["/api/users"]["count"], 4)
  assert_eq(endpoint_aggregation["/api/orders"]["count"], 3)
  assert_eq(endpoint_aggregation["/api/products"]["count"], 2)
  
  // 按状态码聚合
  let status_aggregation = {}
  i = 0
  while i < dimensional_data.length() {
    let (method, endpoint, status, duration) = dimensional_data[i]
    let status_str = status.to_string()
    
    if status_aggregation.contains(status_str) {
      let current_data = status_aggregation[status_str]
      status_aggregation[status_str] = {
        "count": current_data["count"] + 1,
        "total_duration": current_data["total_duration"] + duration,
        "avg_duration": (current_data["total_duration"] + duration) / (current_data["count"] + 1).to_double()
      }
    } else {
      status_aggregation[status_str] = {
        "count": 1,
        "total_duration": duration,
        "avg_duration": duration
      }
    }
    
    i = i + 1
  }
  
  // 验证状态码聚合结果
  assert_eq(status_aggregation["200"]["count"], 5)
  assert_eq(status_aggregation["201"]["count"], 3)
  assert_eq(status_aggregation["404"]["count"], 1)
  assert_eq(status_aggregation["400"]["count"], 1)
  assert_eq(status_aggregation["204"]["count"], 1)
}