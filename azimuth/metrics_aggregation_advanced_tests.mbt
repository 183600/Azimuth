// Metrics Aggregation Advanced Tests
// 测试度量数据聚合的高级功能

test "counter_aggregation_with_labels" {
  // 带标签的计数器聚合测试
  
  let meter_provider = azimuth.MeterProvider.new()
  let meter = azimuth.MeterProvider.get_meter(meter_provider, "test-service")
  
  // 创建带标签的计数器
  let counter = azimuth.Meter.create_counter(meter, "http_requests_total", "Total HTTP requests", "requests")
  
  // 添加不同标签的数据点
  let labels1 = [ ("method", "GET"), ("status", "200"), ("endpoint", "/api/users") ]
  let labels2 = [ ("method", "POST"), ("status", "201"), ("endpoint", "/api/users") ]
  let labels3 = [ ("method", "GET"), ("status", "404"), ("endpoint", "/api/nonexistent") ]
  
  // 记录不同的请求
  for i = 0; i < 100; i + 1 {
    azimuth.Counter.add(counter, 1, labels1) // 100个成功GET请求
  }
  
  for i = 0; i < 50; i + 1 {
    azimuth.Counter.add(counter, 1, labels2) // 50个成功POST请求
  }
  
  for i = 0; i < 10; i + 1 {
    azimuth.Counter.add(counter, 1, labels3) // 10个404错误
  }
  
  // 验证聚合结果
  let measurement1 = azimuth.Counter.collect(counter, labels1)
  match measurement1 {
    Some(value) => @assertion.assert_equal(value, 100)
    None => @assertion.assert_true(false)
  }
  
  let measurement2 = azimuth.Counter.collect(counter, labels2)
  match measurement2 {
    Some(value) => @assertion.assert_equal(value, 50)
    None => @assertion.assert_true(false)
  }
  
  let measurement3 = azimuth.Counter.collect(counter, labels3)
  match measurement3 {
    Some(value) => @assertion.assert_equal(value, 10)
    None => @assertion.assert_true(false)
  }
}

test "histogram_percentile_calculation" {
  // 直方图百分位数计算测试
  
  let meter_provider = azimuth.MeterProvider.new()
  let meter = azimuth.MeterProvider.get_meter(meter_provider, "test-service")
  
  // 创建响应时间直方图
  let histogram = azimuth.Meter.create_histogram(meter, "response_time_ms", "Response time in milliseconds", "ms")
  
  // 记录不同范围的响应时间数据
  let response_times = [
    10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100,
    120, 140, 160, 180, 200, 250, 300, 350, 400, 450, 500, 600, 700, 800, 900, 1000,
    1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000
  ]
  
  // 记录每个响应时间多次
  for time in response_times {
    for i = 0; i < 5; i + 1 { // 每个值记录5次
      azimuth.Histogram.record(histogram, @unsafe.Double.from_int(time), [])
    }
  }
  
  // 收集统计信息
  let stats = azimuth.Histogram.collect_stats(histogram, [])
  
  // 验证基本统计
  match stats {
    Some(statistics) => {
      @assertion.assert_true(azimuth.HistogramStats.get_count(statistics) > 0)
      @assertion.assert_true(azimuth.HistogramStats.get_sum(statistics) > 0.0)
      
      // 验证百分位数计算
      let p50 = azimuth.HistogramStats.get_percentile(statistics, 0.5)
      let p95 = azimuth.HistogramStats.get_percentile(statistics, 0.95)
      let p99 = azimuth.HistogramStats.get_percentile(statistics, 0.99)
      
      @assertion.assert_true(p50 > 0.0)
      @assertion.assert_true(p95 > p50)
      @assertion.assert_true(p99 >= p95)
      
      // 验证最小值和最大值
      let min_val = azimuth.HistogramStats.get_min(statistics)
      let max_val = azimuth.HistogramStats.get_max(statistics)
      
      @assertion.assert_true(min_val <= p50)
      @assertion.assert_true(max_val >= p99)
    }
    None => @assertion.assert_true(false)
  }
}

test "gauge_fluctuation_tracking" {
  // 仪表盘波动跟踪测试
  
  let meter_provider = azimuth.MeterProvider.new()
  let meter = azimuth.MeterProvider.get_meter(meter_provider, "test-service")
  
  // 创建系统资源仪表盘
  let cpu_gauge = azimuth.Meter.create_gauge(meter, "cpu_usage_percent", "CPU usage percentage", "%")
  let memory_gauge = azimuth.Meter.create_gauge(meter, "memory_usage_bytes", "Memory usage in bytes", "bytes")
  
  // 模拟CPU使用率波动
  let cpu_readings = [23.5, 45.2, 67.8, 34.1, 56.9, 78.3, 41.2, 29.7, 61.4, 52.8]
  for reading in cpu_readings {
    azimuth.Gauge.set(cpu_gauge, reading, [("core", "0")])
  }
  
  // 模拟内存使用变化
  let memory_readings = [1024, 2048, 1536, 3072, 2560, 4096, 3584, 5120, 4608, 6144]
  for reading in memory_readings {
    azimuth.Gauge.set(memory_gauge, @unsafe.Double.from_int(reading), [("type", "heap")])
  }
  
  // 验证当前值（应该是最后设置的值）
  let current_cpu = azimuth.Gauge.collect(cpu_gauge, [("core", "0")])
  match current_cpu {
    Some(value) => @assertion.assert_equal(value, 52.8) // 最后一个CPU读数
    None => @assertion.assert_true(false)
  }
  
  let current_memory = azimuth.Gauge.collect(memory_gauge, [("type", "heap")])
  match current_memory {
    Some(value) => @assertion.assert_equal(value, 6144.0) // 最后一个内存读数
    None => @assertion.assert_true(false)
  }
}

test "updown_counter_cumulative_tracking" {
  // 上下计数器累积跟踪测试
  
  let meter_provider = azimuth.MeterProvider.new()
  let meter = azimuth.MeterProvider.get_meter(meter_provider, "test-service")
  
  // 创建活跃连接计数器
  let active_connections = azimuth.Meter.create_up_down_counter(
    meter, 
    "active_connections", 
    "Number of active connections", 
    "connections"
  )
  
  // 模拟连接建立和断开
  let connection_events = [
    (1, "connect"),    // 连接+1
    (1, "connect"),    // 连接+1
    (-1, "disconnect"), // 连接-1
    (1, "connect"),    // 连接+1
    (1, "connect"),    // 连接+1
    (-1, "disconnect"), // 连接-1
    (-1, "disconnect"), // 连接-1
    (1, "connect"),    // 连接+1
    (-1, "disconnect"), // 连接-1
    (1, "connect")     // 连接+1
  ]
  
  for (delta, event_type) in connection_events {
    azimuth.UpDownCounter.add(active_connections, delta, [
      ("service", "database"),
      ("event", event_type)
    ])
  }
  
  // 验证最终连接数
  let final_connections = azimuth.UpDownCounter.collect(active_connections, [
    ("service", "database")
  ])
  match final_connections {
    Some(value) => @assertion.assert_equal(value, 3) // 应该有3个活跃连接
    None => @assertion.assert_true(false)
  }
}

test "metrics_time_window_aggregation" {
  // 度量时间窗口聚合测试
  
  let meter_provider = azimuth.MeterProvider.new()
  let meter = azimuth.MeterProvider.get_meter(meter_provider, "test-service")
  
  // 创建请求率计数器
  let request_counter = azimuth.Meter.create_counter(meter, "requests_per_second", "Requests per second", "req/s")
  
  // 模拟不同时间窗口的请求
  let time_windows = [
    (0, 100),    // 第一个时间窗口：100个请求
    (1000, 150), // 第二个时间窗口：150个请求
    (2000, 120), // 第三个时间窗口：120个请求
    (3000, 180), // 第四个时间窗口：180个请求
    (4000, 90)   // 第五个时间窗口：90个请求
  ]
  
  for (timestamp, count) in time_windows {
    for i = 0; i < count; i + 1 {
      azimuth.Counter.add_with_timestamp(
        request_counter, 
        1, 
        [("endpoint", "/api/data")],
        timestamp + i * 10 // 每个请求间隔10ms
      )
    }
  }
  
  // 收集不同时间窗口的聚合数据
  let window1_stats = azimuth.Counter.collect_time_window(request_counter, 0, 1000, [("endpoint", "/api/data")])
  let window2_stats = azimuth.Counter.collect_time_window(request_counter, 1000, 2000, [("endpoint", "/api/data")])
  let window3_stats = azimuth.Counter.collect_time_window(request_counter, 2000, 3000, [("endpoint", "/api/data")])
  
  // 验证时间窗口聚合结果
  match window1_stats {
    Some(stats) => @assertion.assert_equal(azimuth.TimeWindowStats.get_count(stats), 100)
    None => @assertion.assert_true(false)
  }
  
  match window2_stats {
    Some(stats) => @assertion.assert_equal(azimuth.TimeWindowStats.get_count(stats), 150)
    None => @assertion.assert_true(false)
  }
  
  match window3_stats {
    Some(stats) => @assertion.assert_equal(azimuth.TimeWindowStats.get_count(stats), 120)
    None => @assertion.assert_true(false)
  }
}

test "metrics_exponential_histogram_aggregation" {
  // 指数直方图聚合测试
  
  let meter_provider = azimuth.MeterProvider.new()
  let meter = azimuth.MeterProvider.get_meter(meter_provider, "test-service")
  
  // 创建指数直方图
  let exp_histogram = azimuth.Meter.create_exponential_histogram(
    meter, 
    "latency_distribution", 
    "Latency distribution", 
    "ms"
  )
  
  // 记录广泛范围的延迟值（从微秒到秒）
  let latency_values = [
    0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 10.0, 50.0, 100.0, 500.0, 1000.0
  ]
  
  // 每个延迟值记录不同次数
  let weights = [1000, 800, 600, 400, 200, 100, 50, 25, 10, 5, 2, 1, 1]
  
  for i = 0; i < @unsafe.Array.length(latency_values); i + 1 {
    let latency = @unsafe.Array.get(latency_values, i)
    let weight = @unsafe.Array.get(weights, i)
    
    for j = 0; j < weight; j + 1 {
      azimuth.ExponentialHistogram.record(exp_histogram, latency, [("service", "api")])
    }
  }
  
  // 收集指数直方图统计
  let exp_stats = azimuth.ExponentialHistogram.collect_stats(exp_histogram, [("service", "api")])
  
  match exp_stats {
    Some(stats) => {
      // 验证基本统计
      @assertion.assert_true(azimuth.ExpHistogramStats.get_count(stats) > 0)
      @assertion.assert_true(azimuth.ExpHistogramStats.get_sum(stats) > 0.0)
      
      // 验证指数桶配置
      let scale = azimuth.ExpHistogramStats.get_scale(stats)
      @assertion.assert_true(scale >= 0) // 比例应该是非负的
      
      // 验证零计数
      let zero_count = azimuth.ExpHistogramStats.get_zero_count(stats)
      @assertion.assert_true(zero_count >= 0)
      
      // 验证正负偏移
      let positive_offset = azimuth.ExpHistogramStats.get_positive_offset(stats)
      let negative_offset = azimuth.ExpHistogramStats.get_negative_offset(stats)
      
      @assertion.assert_true(positive_offset >= 0)
      @assertion.assert_true(negative_offset >= 0)
    }
    None => @assertion.assert_true(false)
  }
}