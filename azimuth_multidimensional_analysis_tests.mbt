// Azimuth 多维分析测试用例
// 测试遥测数据的多维分析功能

test "时间序列数据聚合" {
  // 测试时间序列数据的聚合功能
  let time_series_data = [
    (1630000000L, 25.5),
    (1630000060L, 27.3),
    (1630000120L, 26.8),
    (1630000180L, 29.1),
    (1630000240L, 28.4)
  ]
  
  // 计算平均值
  let sum = time_series_data.reduce(fn(acc, pair) { acc + pair.1 }, 0.0)
  let avg = sum / Int64::to_float(time_series_data.length())
  
  assert_true(avg > 25.0 && avg < 30.0)
  
  // 找出最大值和最小值
  let max_value = time_series_data.reduce(fn(acc, pair) { 
    if pair.1 > acc { pair.1 } else { acc } 
  }, time_series_data[0].1)
  
  let min_value = time_series_data.reduce(fn(acc, pair) { 
    if pair.1 < acc { pair.1 } else { acc } 
  }, time_series_data[0].1)
  
  assert_true(max_value > min_value)
  assert_eq(max_value, 29.1)
  assert_eq(min_value, 25.5)
}

test "多维属性查询" {
  // 测试多维属性查询功能
  let attributes = [
    ("service.name", "payment-service"),
    ("service.version", "2.1.0"),
    ("deployment.environment", "production"),
    ("region", "us-west-2"),
    ("availability.zone", "us-west-2a"),
    ("instance.id", "i-1234567890abcdef0")
  ]
  
  // 按服务名称过滤
  let service_filtered = attributes.filter(fn(attr) { 
    attr.0 == "service.name" || attr.0.starts_with("service.") 
  })
  
  assert_eq(service_filtered.length(), 2)
  
  // 按地理位置过滤
  let geo_filtered = attributes.filter(fn(attr) { 
    attr.0 == "region" || attr.0 == "availability.zone" 
  })
  
  assert_eq(geo_filtered.length(), 2)
  
  // 查询特定属性值
  let service_name = attributes.find(fn(attr) { attr.0 == "service.name" })
  match service_name {
    Some((key, value)) => assert_eq(value, "payment-service")
    None => assert_true(false)
  }
}

test "遥测数据相关性分析" {
  // 测试遥测数据的相关性分析
  let latency_data = [120.5, 135.2, 98.7, 156.3, 142.8]
  let error_rate_data = [0.02, 0.03, 0.01, 0.05, 0.04]
  let throughput_data = [1250, 1180, 1320, 980, 1050]
  
  // 计算延迟和错误率的简单相关性
  let latency_error_pairs = latency_data.zip(error_rate_data)
  let high_latency_high_errors = latency_error_pairs.filter(fn(pair) { 
    pair.0 > 130.0 && pair.1 > 0.025 
  })
  
  assert_eq(high_latency_high_errors.length(), 3)
  
  // 计算延迟和吞吐量的负相关性
  let latency_throughput_pairs = latency_data.zip(throughput_data)
  let high_latency_low_throughput = latency_throughput_pairs.filter(fn(pair) { 
    pair.0 > 130.0 && pair.1 < 1100 
  })
  
  assert_eq(high_latency_low_throughput.length(), 2)
}

test "异常检测算法" {
  // 测试异常检测算法
  let normal_data = [10.2, 9.8, 10.5, 10.1, 9.9, 10.3, 10.0, 9.7, 10.4, 9.6]
  let anomaly_data = [10.2, 9.8, 25.5, 10.1, 9.9, 10.3, 10.0, 9.7, 10.4, 9.6]
  
  // 计算正常数据的平均值和标准差
  let normal_mean = normal_data.reduce(fn(acc, x) { acc + x }, 0.0) / Int64::to_float(normal_data.length())
  let normal_variance = normal_data.reduce(fn(acc, x) { 
    acc + (x - normal_mean) * (x - normal_mean) 
  }, 0.0) / Int64::to_float(normal_data.length())
  let normal_std = normal_variance.sqrt()
  
  // 检测异常数据中的异常点
  let anomalies = anomaly_data.filter(fn(x) { 
    (x - normal_mean).abs() > 3.0 * normal_std 
  })
  
  assert_eq(anomalies.length(), 1)
  assert_eq(anomalies[0], 25.5)
}

test "动态阈值调整" {
  // 测试动态阈值调整功能
  let hourly_metrics = [
    (1, 150.2),   // 1:00
    (2, 145.8),   // 2:00
    (3, 152.1),   // 3:00
    (14, 280.5),  // 14:00 (高峰期)
    (15, 295.3),  // 15:00 (高峰期)
    (16, 275.9),  // 16:00 (高峰期)
    (22, 165.4),  // 22:00
    (23, 158.7)   // 23:00
  ]
  
  // 计算不同时间段的阈值
  let night_hours = hourly_metrics.filter(fn(pair) { pair.0 < 6 || pair.0 > 20 })
  let day_hours = hourly_metrics.filter(fn(pair) { pair.0 >= 6 && pair.0 <= 20 })
  
  // 夜间阈值
  let night_avg = night_hours.reduce(fn(acc, pair) { acc + pair.1 }, 0.0) / Int64::to_float(night_hours.length())
  let night_threshold = night_avg * 1.2
  
  // 白天阈值
  let day_avg = day_hours.reduce(fn(acc, pair) { acc + pair.1 }, 0.0) / Int64::to_float(day_hours.length())
  let day_threshold = day_avg * 1.2
  
  assert_true(day_threshold > night_threshold)
  
  // 验证高峰期值超过白天阈值
  let peak_values = hourly_metrics.filter(fn(pair) { pair.0 >= 14 && pair.0 <= 16 })
  let above_threshold = peak_values.filter(fn(pair) { pair.1 > day_threshold })
  
  assert_eq(above_threshold.length(), 3)
}

test "数据采样策略" {
  // 测试数据采样策略
  let all_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
  
  // 固定比例采样 (25%)
  let sample_ratio = 0.25
  let sample_size = Int64::to_int(Int64::to_float(all_data.length()) * sample_ratio)
  let sampled_data = all_data.take(sample_size)
  
  assert_eq(sampled_data.length(), 5)
  assert_eq(sampled_data, [1, 2, 3, 4, 5])
  
  // 基于时间窗口的采样
  let time_stamped_data = [
    (1630000000L, 1),
    (1630000100L, 2),
    (1630000200L, 3),
    (1630000300L, 4),
    (1630000400L, 5),
    (1630000500L, 6)
  ]
  
  let window_start = 1630000150L
  let window_end = 1630000450L
  let window_sampled = time_stamped_data.filter(fn(pair) { 
    pair.0 >= window_start && pair.0 <= window_end 
  })
  
  assert_eq(window_sampled.length(), 3)
  assert_eq(window_sampled.map(fn(pair) { pair.1 }), [3, 4, 5])
}

test "性能基准测试" {
  // 测试性能基准测试功能
  let operations = [
    ("database.query", 120),
    ("cache.get", 5),
    ("api.request", 250),
    ("cache.set", 8),
    ("database.update", 180),
    ("api.response", 95)
  ]
  
  // 按操作类型分组
  let db_ops = operations.filter(fn(op) { op.0.starts_with("database") })
  let cache_ops = operations.filter(fn(op) { op.0.starts_with("cache") })
  let api_ops = operations.filter(fn(op) { op.0.starts_with("api") })
  
  // 计算各类操作的平均性能
  let db_avg = db_ops.reduce(fn(acc, op) { acc + op.1 }, 0) / db_ops.length()
  let cache_avg = cache_ops.reduce(fn(acc, op) { acc + op.1 }, 0) / cache_ops.length()
  let api_avg = api_ops.reduce(fn(acc, op) { acc + op.1 }, 0) / api_ops.length()
  
  assert_true(db_avg > cache_avg)
  assert_true(api_avg > cache_avg)
  
  // 找出最慢的操作
  let slowest_op = operations.reduce(fn(acc, op) { 
    if op.1 > acc.1 { op } else { acc } 
  }, operations[0])
  
  assert_eq(slowest_op.0, "api.request")
  assert_eq(slowest_op.1, 250)
  
  // 找出最快的操作
  let fastest_op = operations.reduce(fn(acc, op) { 
    if op.1 < acc.1 { op } else { acc } 
  }, operations[0])
  
  assert_eq(fastest_op.0, "cache.get")
  assert_eq(fastest_op.1, 5)
}

test "资源利用率分析" {
  // 测试资源利用率分析
  let resource_metrics = [
    (1630000000L, 45.2, 1024, 60.5),  // (timestamp, cpu%, memoryMB, disk%)
    (1630000600L, 52.8, 1152, 62.1),
    (1630001200L, 48.1, 1089, 61.3),
    (1630001800L, 67.3, 1356, 65.8),
    (1630002400L, 71.5, 1423, 66.2)
  ]
  
  // 计算CPU利用率趋势
  let cpu_values = resource_metrics.map(fn(metric) { metric.1 })
  let cpu_avg = cpu_values.reduce(fn(acc, val) { acc + val }, 0.0) / Int64::to_float(cpu_values.length())
  let cpu_max = cpu_values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, cpu_values[0])
  
  assert_true(cpu_avg > 40.0 && cpu_avg < 80.0)
  assert_eq(cpu_max, 71.5)
  
  // 计算内存使用量增长
  let memory_values = resource_metrics.map(fn(metric) { metric.2 })
  let memory_growth = memory_values[memory_values.length() - 1] - memory_values[0]
  
  assert_eq(memory_growth, 399)
  
  // 检测资源压力时段
  let high_pressure_periods = resource_metrics.filter(fn(metric) { 
    metric.1 > 65.0 || metric.3 > 65.0 
  })
  
  assert_eq(high_pressure_periods.length(), 2)
}