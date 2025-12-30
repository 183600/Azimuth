// 遥测数据聚合测试用例
// 测试各种数据聚合算法和统计计算

test "telemetry_metric_sum_aggregation" {
  // 测试指标求和聚合
  
  let metric_values = [10.5, 20.3, 15.7, 8.2, 12.9]
  let mut sum = 0.0
  let mut i = 0
  
  while i < metric_values.length() {
    sum = sum + metric_values[i]
    i = i + 1
  }
  
  // 验证求和结果
  assert_eq(sum > 60.0, true)
  assert_eq(sum < 70.0, true)
  assert_eq(sum, 67.6)
  
  // 验证聚合元数据
  let aggregation_metadata = {
    "operation": "sum",
    "count": metric_values.length(),
    "result": sum,
    "unit": "milliseconds"
  }
  
  assert_eq(aggregation_metadata["operation"], "sum")
  assert_eq(aggregation_metadata["count"], 5)
  assert_eq(aggregation_metadata["result"], 67.6)
  assert_eq(aggregation_metadata["unit"], "milliseconds")
}

test "telemetry_metric_average_aggregation" {
  // 测试指标平均值聚合
  
  let response_times = [120.5, 98.3, 145.7, 88.2, 110.9, 132.1]
  let mut sum = 0.0
  let mut i = 0
  
  while i < response_times.length() {
    sum = sum + response_times[i]
    i = i + 1
  }
  
  let average = sum / response_times.length().to_double()
  
  // 验证平均值计算
  assert_eq(average > 100.0, true)
  assert_eq(average < 150.0, true)
  assert_eq(average, 115.95)
  
  // 验证聚合统计
  let aggregation_stats = {
    "min": 88.2,
    "max": 145.7,
    "mean": average,
    "count": response_times.length(),
    "sum": sum
  }
  
  assert_eq(aggregation_stats["min"], 88.2)
  assert_eq(aggregation_stats["max"], 145.7)
  assert_eq(aggregation_stats["mean"], 115.95)
  assert_eq(aggregation_stats["count"], 6)
  assert_eq(aggregation_stats["sum"], 695.7)
}

test "telemetry_metric_percentile_aggregation" {
  // 测试指标百分位数聚合
  
  let sorted_values = [10.0, 25.0, 50.0, 75.0, 90.0, 100.0, 150.0, 200.0, 300.0, 500.0]
  let count = sorted_values.length()
  
  // 计算50百分位数（中位数）
  let p50_index = (count * 50) / 100
  let p50_value = sorted_values[p50_index]
  assert_eq(p50_value, 90.0)
  
  // 计算95百分位数
  let p95_index = (count * 95) / 100
  let p95_value = sorted_values[p95_index]
  assert_eq(p95_value, 500.0)
  
  // 计算99百分位数
  let p99_index = (count * 99) / 100
  let p99_value = sorted_values[p99_index]
  assert_eq(p99_value, 500.0)
  
  // 验证百分位数聚合结果
  let percentile_aggregation = {
    "p50": p50_value,
    "p90": sorted_values[(count * 90) / 100],
    "p95": p95_value,
    "p99": p99_value,
    "sample_count": count
  }
  
  assert_eq(percentile_aggregation["p50"], 90.0)
  assert_eq(percentile_aggregation["p90"], 300.0)
  assert_eq(percentile_aggregation["p95"], 500.0)
  assert_eq(percentile_aggregation["p99"], 500.0)
  assert_eq(percentile_aggregation["sample_count"], 10)
}

test "telemetry_histogram_aggregation" {
  // 测试直方图聚合
  
  let bucket_boundaries = [10.0, 50.0, 100.0, 500.0, 1000.0]
  let measurements = [5.0, 15.0, 75.0, 200.0, 600.0, 1200.0, 25.0, 150.0]
  let mut bucket_counts = [0, 0, 0, 0, 0, 0]  // 包括+inf桶
  
  // 将测量值分配到桶中
  let mut i = 0
  while i < measurements.length() {
    let value = measurements[i]
    let mut bucket_index = bucket_boundaries.length()  // 默认+inf桶
    
    let mut j = 0
    while j < bucket_boundaries.length() {
      if value <= bucket_boundaries[j] {
        bucket_index = j
        break
      }
      j = j + 1
    }
    
    bucket_counts[bucket_index] = bucket_counts[bucket_index] + 1
    i = i + 1
  }
  
  // 验证桶计数
  assert_eq(bucket_counts[0], 1)  // <= 10.0: [5.0]
  assert_eq(bucket_counts[1], 2)  // <= 50.0: [15.0, 25.0]
  assert_eq(bucket_counts[2], 2)  // <= 100.0: [75.0, 150.0]
  assert_eq(bucket_counts[3], 1)  // <= 500.0: [200.0]
  assert_eq(bucket_counts[4], 1)  // <= 1000.0: [600.0]
  assert_eq(bucket_counts[5], 1)  // +inf: [1200.0]
  
  // 验证直方图聚合
  let histogram_aggregation = {
    "buckets": bucket_boundaries,
    "counts": bucket_counts,
    "total_count": measurements.length(),
    "sum": 2270.0
  }
  
  assert_eq(histogram_aggregation["total_count"], 8)
  assert_eq(histogram_aggregation["sum"], 2270.0)
}

test "telemetry_counter_aggregation" {
  // 测试计数器聚合
  
  let service_counters = [
    ("service_a", 100),
    ("service_b", 250),
    ("service_c", 150),
    ("service_a", 50),   // service_a的另一个计数
    ("service_d", 75),
    ("service_b", 100)   // service_b的另一个计数
  ]
  
  // 按服务名称聚合计数
  let mut aggregated_counters = {}
  let mut i = 0
  
  while i < service_counters.length() {
    let (service_name, count) = service_counters[i]
    
    if aggregated_counters.contains_key(service_name) {
      let current_count = aggregated_counters[service_name]
      aggregated_counters[service_name] = current_count + count
    } else {
      aggregated_counters[service_name] = count
    }
    
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(aggregated_counters["service_a"], 150)
  assert_eq(aggregated_counters["service_b"], 350)
  assert_eq(aggregated_counters["service_c"], 150)
  assert_eq(aggregated_counters["service_d"], 75)
  
  // 验证总计数
  let mut total_count = 0
  for (_, count) in aggregated_counters {
    total_count = total_count + count
  }
  
  assert_eq(total_count, 725)
}

test "telemetry_time_window_aggregation" {
  // 测试时间窗口聚合
  
  let current_time = 1640995200L  // 2022-01-01 00:00:00
  let window_size_seconds = 60L   // 1分钟窗口
  let metrics = [
    (current_time - 30L, 10.0),   // 30秒前
    (current_time - 45L, 15.0),   // 45秒前
    (current_time - 90L, 20.0),   // 90秒前（窗口外）
    (current_time - 10L, 25.0),   // 10秒前
    (current_time - 120L, 30.0),  // 120秒前（窗口外）
    (current_time - 5L, 35.0)     // 5秒前
  ]
  
  // 过滤时间窗口内的指标
  let mut window_metrics = []
  let mut i = 0
  
  while i < metrics.length() {
    let (timestamp, value) = metrics[i]
    let age_seconds = current_time - timestamp
    
    if age_seconds <= window_size_seconds {
      window_metrics.push((timestamp, value))
    }
    
    i = i + 1
  }
  
  // 验证窗口内指标
  assert_eq(window_metrics.length(), 4)  // 只有4个指标在1分钟窗口内
  
  // 计算窗口聚合
  let mut window_sum = 0.0
  i = 0
  while i < window_metrics.length() {
    window_sum = window_sum + window_metrics[i].1
    i = i + 1
  }
  
  let window_average = window_sum / window_metrics.length().to_double()
  
  assert_eq(window_sum, 85.0)
  assert_eq(window_average, 21.25)
  
  // 验证时间窗口聚合元数据
  let window_aggregation = {
    "window_start": current_time - window_size_seconds,
    "window_end": current_time,
    "window_size_seconds": window_size_seconds,
    "metric_count": window_metrics.length(),
    "sum": window_sum,
    "average": window_average
  }
  
  assert_eq(window_aggregation["window_size_seconds"], 60L)
  assert_eq(window_aggregation["metric_count"], 4)
  assert_eq(window_aggregation["sum"], 85.0)
  assert_eq(window_aggregation["average"], 21.25)
}

test "telemetry_group_by_aggregation" {
  // 测试分组聚合
  
  let request_metrics = [
    ("GET", "/api/users", 200, 120.5),
    ("POST", "/api/orders", 201, 250.3),
    ("GET", "/api/products", 200, 89.7),
    ("GET", "/api/users", 404, 15.2),
    ("POST", "/api/orders", 400, 45.8),
    ("GET", "/api/products", 500, 210.1)
  ]
  
  // 按HTTP方法和状态码分组
  let mut grouped_metrics = {}
  let mut i = 0
  
  while i < request_metrics.length() {
    let (method, path, status, duration) = request_metrics[i]
    let group_key = method + ":" + status.to_string()
    
    if grouped_metrics.contains_key(group_key) {
      let group_data = grouped_metrics[group_key]
      group_data["count"] = group_data["count"] + 1
      group_data["total_duration"] = group_data["total_duration"] + duration
      group_data["paths"].push(path)
    } else {
      grouped_metrics[group_key] = {
        "count": 1,
        "total_duration": duration,
        "paths": [path]
      }
    }
    
    i = i + 1
  }
  
  // 验证分组聚合结果
  assert_eq(grouped_metrics["GET:200"]["count"], 2)
  assert_eq(grouped_metrics["POST:201"]["count"], 1)
  assert_eq(grouped_metrics["GET:404"]["count"], 1)
  
  // 计算每组的平均持续时间
  let get200_avg = grouped_metrics["GET:200"]["total_duration"] / grouped_metrics["GET:200"]["count"].to_double()
  assert_eq(get200_avg, 105.1)  // (120.5 + 89.7) / 2
  
  // 验证路径聚合
  let get200_paths = grouped_metrics["GET:200"]["paths"]
  assert_eq(get200_paths.contains("/api/users"), true)
  assert_eq(get200_paths.contains("/api/products"), true)
}

test "telemetry_rate_aggregation" {
  // 测试速率聚合
  
  let time_series_data = [
    (1640995200L, 100),  // 每秒请求数
    (1640995201L, 120),
    (1640995202L, 95),
    (1640995203L, 110),
    (1640995204L, 130),
    (1640995205L, 105)
  ]
  
  // 计算平均速率
  let mut total_requests = 0
  let mut i = 0
  
  while i < time_series_data.length() {
    total_requests = total_requests + time_series_data[i].1
    i = i + 1
  }
  
  let time_span_seconds = time_series_data[time_series_data.length() - 1].0 - time_series_data[0].0
  let average_rate = total_requests.to_double() / time_span_seconds.to_double()
  
  // 验证速率计算
  assert_eq(total_requests, 660)
  assert_eq(time_span_seconds, 5L)
  assert_eq(average_rate, 132.0)
  
  // 计算峰值速率
  let mut peak_rate = 0
  i = 0
  while i < time_series_data.length() {
    if time_series_data[i].1 > peak_rate {
      peak_rate = time_series_data[i].1
    }
    i = i + 1
  }
  
  assert_eq(peak_rate, 130)
  
  // 验证速率聚合
  let rate_aggregation = {
    "average_rate": average_rate,
    "peak_rate": peak_rate,
    "total_requests": total_requests,
    "time_span_seconds": time_span_seconds,
    "data_points": time_series_data.length()
  }
  
  assert_eq(rate_aggregation["average_rate"], 132.0)
  assert_eq(rate_aggregation["peak_rate"], 130)
  assert_eq(rate_aggregation["total_requests"], 660)
  assert_eq(rate_aggregation["time_span_seconds"], 5L)
  assert_eq(rate_aggregation["data_points"], 6)
}