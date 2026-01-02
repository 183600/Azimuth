// 度量聚合功能测试
// 测试Azimuth遥测系统的度量数据聚合功能

test "计数器聚合和时间窗口统计" {
  // 创建计数器和聚合器
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "counter.aggregation.test")
  
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("count"))
  let aggregator = CounterAggregator::new(request_counter)
  
  // 模拟不同时间窗口的请求
  let time_windows = [
    ("2025-01-02T10:00:00Z", "2025-01-02T10:01:00Z"),
    ("2025-01-02T10:01:00Z", "2025-01-02T10:02:00Z"),
    ("2025-01-02T10:02:00Z", "2025-01-02T10:03:00Z")
  ]
  
  for (start_time, end_time) in time_windows {
    // 在每个时间窗口内添加不同类型的请求
    Counter::add_with_attributes(request_counter, 10.0, [
      ("method", "GET"),
      ("status", "200"),
      ("endpoint", "/api/users"),
      ("time_window", start_time)
    ])
    
    Counter::add_with_attributes(request_counter, 3.0, [
      ("method", "POST"),
      ("status", "201"),
      ("endpoint", "/api/orders"),
      ("time_window", start_time)
    ])
    
    Counter::add_with_attributes(request_counter, 2.0, [
      ("method", "GET"),
      ("status", "404"),
      ("endpoint", "/api/invalid"),
      ("time_window", start_time)
    ])
  }
  
  // 执行聚合
  let aggregated_data = aggregator.aggregate_by_time_window("time_window")
  
  // 验证聚合结果
  assert_eq(aggregated_data.length(), 3)
  
  // 验证每个时间窗口的总计数
  for window_data in aggregated_data {
    let total_requests = window_data.get_total()
    assert_eq(total_requests, 15.0)  // 10 + 3 + 2
    
    // 验证按状态码分组的计数
    let success_count = window_data.get_by_attribute("status", "200")
    let created_count = window_data.get_by_attribute("status", "201")
    let not_found_count = window_data.get_by_attribute("status", "404")
    
    assert_eq(success_count, 10.0)
    assert_eq(created_count, 3.0)
    assert_eq(not_found_count, 2.0)
  }
  
  // 测试跨时间窗口的总聚合
  let total_aggregated = aggregator.aggregate_all()
  let grand_total = total_aggregated.get_total()
  assert_eq(grand_total, 45.0)  // 15 * 3 windows
}

test "直方图聚合和百分位数计算" {
  // 创建直方图和聚合器
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "histogram.aggregation.test")
  
  let response_time_histogram = Meter::create_histogram(
    meter, 
    "http.response.time", 
    Some("HTTP response time"), 
    Some("ms")
  )
  
  let histogram_aggregator = HistogramAggregator::new(response_time_histogram)
  
  // 模拟不同服务端点的响应时间数据
  let endpoints = [
    ("/api/users", [45.2, 67.8, 123.4, 89.1, 156.7, 234.5, 78.9, 90.3, 145.6, 198.2]),
    ("/api/orders", [234.5, 345.6, 456.7, 123.4, 267.8, 389.1, 412.3, 298.7, 356.9, 401.2]),
    ("/api/products", [89.3, 95.7, 102.4, 87.6, 93.2, 98.8, 105.1, 91.5, 96.9, 99.7])
  ]
  
  for (endpoint, times) in endpoints {
    for time in times {
      Histogram::record_with_attributes(response_time_histogram, time, [
        ("endpoint", endpoint),
        ("method", "GET")
      ])
    }
  }
  
  // 执行聚合
  let aggregated_histograms = histogram_aggregator.aggregate_by_attribute("endpoint")
  
  // 验证每个端点的聚合结果
  for (endpoint, histogram_data) in aggregated_histograms {
    let count = histogram_data.get_count()
    let sum = histogram_data.get_sum()
    let mean = histogram_data.get_mean()
    let min = histogram_data.get_min()
    let max = histogram_data.get_max()
    
    assert_eq(count, 10)
    assert_true(sum > 0.0)
    assert_true(mean > 0.0)
    assert_true(min > 0.0)
    assert_true(max > 0.0)
    assert_true(min <= mean)
    assert_true(mean <= max)
    
    // 验证百分位数
    let p50 = histogram_data.get_percentile(50.0)
    let p90 = histogram_data.get_percentile(90.0)
    let p95 = histogram_data.get_percentile(95.0)
    let p99 = histogram_data.get_percentile(99.0)
    
    assert_true(p50 > 0.0)
    assert_true(p90 > 0.0)
    assert_true(p95 > 0.0)
    assert_true(p99 > 0.0)
    assert_true(p50 <= p90)
    assert_true(p90 <= p95)
    assert_true(p95 <= p99)
  }
  
  // 测试全局聚合
  let global_histogram = histogram_aggregator.aggregate_all()
  let global_count = global_histogram.get_count()
  assert_eq(global_count, 30)  // 10 * 3 endpoints
}

test "仪表度量和实时值聚合" {
  // 创建仪表度量和聚合器
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge.aggregation.test")
  
  let memory_usage_gauge = Meter::create_gauge(
    meter, 
    "process.memory.usage", 
    Some("Process memory usage"), 
    Some("bytes")
  )
  
  let cpu_usage_gauge = Meter::create_gauge(
    meter, 
    "process.cpu.usage", 
    Some("Process CPU usage"), 
    Some("percent")
  )
  
  let memory_aggregator = GaugeAggregator::new(memory_usage_gauge)
  let cpu_aggregator = GaugeAggregator::new(cpu_usage_gauge)
  
  // 模拟不同实例的度量数据
  let instances = ["instance-1", "instance-2", "instance-3", "instance-4"]
  let memory_values = [1024.0, 2048.0, 1536.0, 1792.0]  // MB
  let cpu_values = [45.5, 67.8, 23.4, 89.1]  // 百分比
  
  for i in 0..instances.length() {
    let instance = instances.get(i)
    let memory_val = memory_values.get(i)
    let cpu_val = cpu_values.get(i)
    
    Gauge::set_with_attributes(memory_usage_gauge, memory_val, [
      ("instance.id", instance),
      ("service", "web-server")
    ])
    
    Gauge::set_with_attributes(cpu_usage_gauge, cpu_val, [
      ("instance.id", instance),
      ("service", "web-server")
    ])
  }
  
  // 执行内存使用聚合
  let memory_aggregated = memory_aggregator.aggregate_by_attribute("instance.id")
  
  // 验证内存聚合结果
  let total_memory = memory_aggregated.get_sum()
  let avg_memory = memory_aggregated.get_mean()
  let max_memory = memory_aggregated.get_max()
  let min_memory = memory_aggregated.get_min()
  
  assert_eq(total_memory, 6400.0)  // 1024 + 2048 + 1536 + 1792
  assert_eq(avg_memory, 1600.0)   // 6400 / 4
  assert_eq(max_memory, 2048.0)
  assert_eq(min_memory, 1024.0)
  
  // 执行CPU使用聚合
  let cpu_aggregated = cpu_aggregator.aggregate_by_attribute("instance.id")
  
  let total_cpu = cpu_aggregated.get_sum()
  let avg_cpu = cpu_aggregated.get_mean()
  let max_cpu = cpu_aggregated.get_max()
  let min_cpu = cpu_aggregated.get_min()
  
  assert_eq(total_cpu, 225.8)  // 45.5 + 67.8 + 23.4 + 89.1
  assert_eq(avg_cpu, 56.45)   // 225.8 / 4
  assert_eq(max_cpu, 89.1)
  assert_eq(min_cpu, 23.4)
}

test "多维度度量聚合" {
  // 测试多维度度量数据的聚合
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "multidimensional.aggregation.test")
  
  let request_counter = Meter::create_counter(meter, "api.requests", Some("API requests"), Some("count"))
  let multi_aggregator = MultiDimensionalAggregator::new(request_counter)
  
  // 模拟具有多个维度的请求数据
  let request_data = [
    (["GET", "/api/users", "200", "us-east-1"], 120.0),
    (["POST", "/api/orders", "201", "us-east-1"], 45.0),
    (["GET", "/api/products", "200", "us-west-2"], 89.0),
    (["PUT", "/api/users", "200", "us-west-2"], 23.0),
    (["DELETE", "/api/orders", "204", "eu-west-1"], 12.0),
    (["GET", "/api/users", "404", "eu-west-1"], 34.0),
    (["POST", "/api/products", "400", "eu-west-1"], 8.0),
    (["GET", "/api/orders", "200", "us-east-1"], 67.0)
  ]
  
  let dimensions = ["method", "endpoint", "status", "region"]
  
  for (dimension_values, count) in request_data {
    let mut attributes = []
    for i in 0..dimensions.length() {
      attributes = attributes.push((dimensions.get(i), dimension_values.get(i)))
    }
    
    Counter::add_with_attributes(request_counter, count, attributes)
  }
  
  // 测试按单个维度聚合
  let method_aggregation = multi_aggregator.aggregate_by_dimension("method")
  let endpoint_aggregation = multi_aggregator.aggregate_by_dimension("endpoint")
  let status_aggregation = multi_aggregator.aggregate_by_dimension("status")
  let region_aggregation = multi_aggregator.aggregate_by_dimension("region")
  
  // 验证方法维度聚合
  assert_eq(method_aggregation.get_value("GET"), 310.0)  // 120 + 89 + 34 + 67
  assert_eq(method_aggregation.get_value("POST"), 53.0)   // 45 + 8
  assert_eq(method_aggregation.get_value("PUT"), 23.0)
  assert_eq(method_aggregation.get_value("DELETE"), 12.0)
  
  // 验证端点维度聚合
  assert_eq(endpoint_aggregation.get_value("/api/users"), 177.0)  // 120 + 23 + 34
  assert_eq(endpoint_aggregation.get_value("/api/orders"), 124.0)  // 45 + 67 + 12
  assert_eq(endpoint_aggregation.get_value("/api/products"), 97.0)  // 89 + 8
  
  // 验证状态码维度聚合
  assert_eq(status_aggregation.get_value("200"), 299.0)  // 120 + 89 + 23 + 67
  assert_eq(status_aggregation.get_value("201"), 45.0)
  assert_eq(status_aggregation.get_value("204"), 12.0)
  assert_eq(status_aggregation.get_value("400"), 8.0)
  assert_eq(status_aggregation.get_value("404"), 34.0)
  
  // 验证区域维度聚合
  assert_eq(region_aggregation.get_value("us-east-1"), 232.0)  // 120 + 45 + 67
  assert_eq(region_aggregation.get_value("us-west-2"), 112.0)  // 89 + 23
  assert_eq(region_aggregation.get_value("eu-west-1"), 54.0)   // 12 + 34 + 8
  
  // 测试多维度组合聚合
  let multi_dimensional = multi_aggregator.aggregate_by_dimensions(["method", "status"])
  let get_success_count = multi_dimensional.get_value(["GET", "200"])
  let post_success_count = multi_dimensional.get_value(["POST", "201"])
  
  assert_eq(get_success_count, 276.0)  // 120 + 89 + 67
  assert_eq(post_success_count, 45.0)
}

test "时间序列度量的滑动窗口聚合" {
  // 测试时间序列度量的滑动窗口聚合
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "timeseries.aggregation.test")
  
  let throughput_counter = Meter::create_counter(meter, "system.throughput", Some("System throughput"), Some("ops/sec"))
  let sliding_aggregator = SlidingWindowAggregator::new(throughput_counter, 300)  // 5分钟窗口
  
  // 模拟时间序列数据
  let time_series_data = [
    (1735689600, 100.0),  // 10:00:00
    (1735689660, 120.0),  // 10:01:00
    (1735689720, 95.0),   // 10:02:00
    (1735689780, 110.0),  // 10:03:00
    (1735689840, 130.0),  // 10:04:00
    (1735689900, 105.0),  // 10:05:00
    (1735689960, 125.0),  // 10:06:00
    (1735690020, 115.0),  // 10:07:00
    (1735690080, 140.0),  // 10:08:00
    (1735690140, 108.0)   // 10:09:00
  ]
  
  // 添加时间序列数据
  for (timestamp, value) in time_series_data {
    Counter::add_with_attributes(throughput_counter, value, [
      ("timestamp", timestamp.to_string()),
      ("metric.type", "throughput")
    ])
  }
  
  // 测试不同时间窗口的聚合
  let window_3min = sliding_aggregator.aggregate_by_window(180)  // 3分钟窗口
  let window_5min = sliding_aggregator.aggregate_by_window(300)  // 5分钟窗口
  let window_10min = sliding_aggregator.aggregate_by_window(600) // 10分钟窗口
  
  // 验证窗口聚合结果
  assert_true(window_3min.length() > 0)
  assert_true(window_5min.length() > 0)
  assert_true(window_10min.length() > 0)
  
  // 验证窗口大小关系
  assert_true(window_3min.length() >= window_5min.length())
  assert_true(window_5min.length() >= window_10min.length())
  
  // 测试滑动窗口的统计指标
  let latest_window = window_5min.get_last()
  let window_avg = latest_window.get_average()
  let window_max = latest_window.get_max()
  let window_min = latest_window.get_min()
  let window_trend = latest_window.get_trend()
  
  assert_true(window_avg > 0.0)
  assert_true(window_max >= window_avg)
  assert_true(window_min <= window_avg)
  assert_true(window_trend == "increasing" || window_trend == "decreasing" || window_trend == "stable")
}

test "度量聚合性能和内存优化" {
  // 测试度量聚合的性能和内存优化
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance.aggregation.test")
  
  let performance_counter = Meter::create_counter(meter, "performance.metrics", Some("Performance metrics"), Some("count"))
  let performance_aggregator = PerformanceOptimizedAggregator::new(performance_counter)
  
  // 生成大量测试数据
  let start_time = 1735689600
  let num_records = 10000
  
  for i in 0..num_records {
    let timestamp = start_time + i
    let value = (i % 100).to_float()
    let category = "category-" + ((i % 10).to_string())
    let subcategory = "subcat-" + ((i % 5).to_string())
    
    Counter::add_with_attributes(performance_counter, value, [
      ("timestamp", timestamp.to_string()),
      ("category", category),
      ("subcategory", subcategory),
      ("batch.id", (i / 100).to_string())
    ])
  }
  
  // 测试聚合性能
  let aggregation_start = 1735689600
  let performance_result = performance_aggregator.aggregate_with_performance_tracking(aggregation_start)
  
  // 验证聚合结果
  let aggregated_count = performance_result.get_record_count()
  let aggregation_time = performance_result.get_aggregation_time_ms()
  let memory_usage = performance_result.get_memory_usage_bytes()
  
  assert_eq(aggregated_count, num_records)
  assert_true(aggregation_time < 1000)  // 应该在1秒内完成
  assert_true(memory_usage > 0)
  
  // 测试内存优化功能
  let optimized_result = performance_aggregator.aggregate_with_memory_optimization(aggregation_start)
  let optimized_memory = optimized_result.get_memory_usage_bytes()
  
  // 验证内存优化效果
  assert_true(optimized_memory <= memory_usage)
  
  // 测试增量聚合
  let incremental_result = performance_aggregator.incremental_aggregate(aggregation_start, 1000)  // 每次处理1000条记录
  let incremental_time = incremental_result.get_total_time_ms()
  
  assert_true(incremental_time > 0)
  
  // 验证增量聚合和全量聚合的结果一致性
  let full_result = performance_aggregator.aggregate_all()
  let incremental_total = incremental_result.get_total_value()
  let full_total = full_result.get_total_value()
  
  assert_eq(incremental_total, full_total)
}