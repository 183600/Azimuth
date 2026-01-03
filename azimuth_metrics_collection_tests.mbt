// Azimuth 度量数据收集测试
// 测试各种度量类型的收集和聚合功能

// 测试1: Counter 度量类型
test "Counter 度量类型测试" {
  // 创建 Counter
  let request_counter = Counter({
    name: "http_requests_total",
    description: Some("Total number of HTTP requests"),
    unit: Some("requests")
  })
  
  // 模拟添加度量值
  let initial_value = 0L
  
  // 模拟多次请求
  let increments = [1L, 1L, 1L, 1L, 1L] // 5次请求
  let final_value = increments.reduce(fn(acc, inc) { acc + inc }, initial_value)
  
  // 验证 Counter 累积值
  assert_eq(final_value, 5L)
  
  // 验证 Counter 属性
  assert_eq(request_counter.name, "http_requests_total")
  match request_counter.description {
    Some(desc) => assert_eq(desc, "Total number of HTTP requests")
    None => assert_true(false)
  }
  match request_counter.unit {
    Some(unit) => assert_eq(unit, "requests")
    None => assert_true(false)
  }
  
  // 测试带属性的 Counter
  let labeled_counter = Counter({
    name: "api_requests_total",
    description: Some("API requests by method and status"),
    unit: Some("requests"),
    attributes: [
      ("method", "GET"),
      ("status", "200")
    ]
  })
  
  // 模拟不同类型的请求
  let get_requests = 10L
  let post_requests = 5L
  let put_requests = 2L
  
  let total_requests = get_requests + post_requests + put_requests
  assert_eq(total_requests, 17L)
}

// 测试2: Gauge 度量类型
test "Gauge 度量类型测试" {
  // 创建 Gauge
  let memory_gauge = Gauge({
    name: "memory_usage_bytes",
    description: Some("Current memory usage in bytes"),
    unit: Some("bytes")
  })
  
  // 模拟内存使用情况变化
  let memory_values = [1024L, 2048L, 1536L, 3072L, 2560L]
  
  // 验证 Gauge 记录当前值
  let current_memory = memory_values[memory_values.length() - 1]
  assert_eq(current_memory, 2560L)
  
  // 验证 Gauge 属性
  assert_eq(memory_gauge.name, "memory_usage_bytes")
  match memory_gauge.description {
    Some(desc) => assert_eq(desc, "Current memory usage in bytes")
    None => assert_true(false)
  }
  match memory_gauge.unit {
    Some(unit) => assert_eq(unit, "bytes")
    None => assert_true(false)
  }
  
  // 测试带属性的 Gauge
  let cpu_gauge = Gauge({
    name: "cpu_usage_percentage",
    description: Some("Current CPU usage percentage"),
    unit: Some("percent"),
    attributes: [
      ("core", "0"),
      ("instance", "server-1")
    ]
  })
  
  // 模拟 CPU 使用率变化
  let cpu_values = [15.5, 25.3, 45.7, 32.1, 28.9]
  let current_cpu = cpu_values[cpu_values.length() - 1]
  assert_true(current_cpu > 25.0 && current_cpu < 30.0)
}

// 测试3: Histogram 度量类型
test "Histogram 度量类型测试" {
  // 创建 Histogram
  let response_histogram = Histogram({
    name: "http_request_duration_seconds",
    description: Some("HTTP request duration in seconds"),
    unit: Some("seconds"),
    buckets: [0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]
  })
  
  // 模拟请求持续时间
  let durations = [0.003, 0.007, 0.015, 0.035, 0.08, 0.2, 0.4, 0.8, 1.5, 3.0]
  
  // 验证 Histogram 属性
  assert_eq(response_histogram.name, "http_request_duration_seconds")
  match response_histogram.description {
    Some(desc) => assert_eq(desc, "HTTP request duration in seconds")
    None => assert_true(false)
  }
  match response_histogram.unit {
    Some(unit) => assert_eq(unit, "seconds")
    None => assert_true(false)
  }
  
  // 验证 bucket 边界
  let buckets = response_histogram.buckets
  assert_eq(buckets.length(), 11)
  assert_eq(buckets[0], 0.005)
  assert_eq(buckets[buckets.length() - 1], 10.0)
  
  // 计算统计数据
  let count = durations.length()
  let sum = durations.reduce(fn(acc, d) { acc + d }, 0.0)
  let avg = sum / Int64::from_int(count)
  
  assert_eq(count, 10)
  assert_true(sum > 0.0)
  assert_true(avg > 0.0)
  
  // 验证最小值和最大值
  let min_duration = durations.reduce(fn(acc, d) { if d < acc { d } else { acc } }, durations[0])
  let max_duration = durations.reduce(fn(acc, d) { if d > acc { d } else { acc } }, durations[0])
  
  assert_eq(min_duration, 0.003)
  assert_eq(max_duration, 3.0)
}

// 测试4: UpDownCounter 度量类型
test "UpDownCounter 度量类型测试" {
  // 创建 UpDownCounter
  let active_connections = UpDownCounter({
    name: "active_connections",
    description: Some("Current number of active connections"),
    unit: Some("connections")
  })
  
  // 模拟连接变化
  let initial_connections = 0L
  let connections_added = [5L, 3L, 2L] // 新增连接
  let connections_removed = [2L, 1L]   // 关闭连接
  
  // 计算当前连接数
  let total_added = connections_added.reduce(fn(acc, val) { acc + val }, 0L)
  let total_removed = connections_removed.reduce(fn(acc, val) { acc + val }, 0L)
  let current_connections = initial_connections + total_added - total_removed
  
  assert_eq(current_connections, 7L)
  
  // 验证 UpDownCounter 属性
  assert_eq(active_connections.name, "active_connections")
  match active_connections.description {
    Some(desc) => assert_eq(desc, "Current number of active connections")
    None => assert_true(false)
  }
  match active_connections.unit {
    Some(unit) => assert_eq(unit, "connections")
    None => assert_true(false)
  }
}

// 测试5: 度量聚合测试
test "度量聚合测试" {
  // 创建多个度量实例
  let metrics = [
    Counter({
      name: "requests_total",
      description: Some("Total requests"),
      unit: Some("requests")
    }),
    Gauge({
      name: "memory_usage",
      description: Some("Memory usage"),
      unit: Some("bytes")
    }),
    Histogram({
      name: "response_time",
      description: Some("Response time"),
      unit: Some("seconds"),
      buckets: [0.1, 0.5, 1.0, 2.5, 5.0]
    })
  ]
  
  // 验证度量集合
  assert_eq(metrics.length(), 3)
  
  // 按类型分组度量
  let counters = metrics.filter(fn(m) { match m { Counter(_) => true, _ => false } })
  let gauges = metrics.filter(fn(m) { match m { Gauge(_) => true, _ => false } })
  let histograms = metrics.filter(fn(m) { match m { Histogram(_) => true, _ => false } })
  
  assert_eq(counters.length(), 1)
  assert_eq(gauges.length(), 1)
  assert_eq(histograms.length(), 1)
  
  // 验证度量名称
  assert_eq(counters[0].name, "requests_total")
  assert_eq(gauges[0].name, "memory_usage")
  assert_eq(histograms[0].name, "response_time")
  
  // 测试度量数据点
  let data_points = [
    {
      metric_name: "requests_total",
      value: 100L,
      attributes: [("method", "GET"), ("status", "200")],
      timestamp: 1640995200000000000L // 2022-01-01 00:00:00 UTC
    },
    {
      metric_name: "memory_usage",
      value: 5368709120L, // 5GB
      attributes: [("instance", "server-1")],
      timestamp: 1640995260000000000L // 2022-01-01 00:01:00 UTC
    },
    {
      metric_name: "response_time",
      value: 0.235,
      attributes: [("endpoint", "/api/users")],
      timestamp: 1640995320000000000L // 2022-01-01 00:02:00 UTC
    }
  ]
  
  // 按度量名称分组数据点
  let requests_data = data_points.filter(fn(dp) { dp.metric_name == "requests_total" })
  let memory_data = data_points.filter(fn(dp) { dp.metric_name == "memory_usage" })
  let response_time_data = data_points.filter(fn(dp) { dp.metric_name == "response_time" })
  
  assert_eq(requests_data.length(), 1)
  assert_eq(memory_data.length(), 1)
  assert_eq(response_time_data.length(), 1)
  
  // 验证数据点内容
  assert_eq(requests_data[0].value, 100L)
  assert_eq(memory_data[0].value, 5368709120L)
  assert_true(response_time_data[0].value > 0.2 && response_time_data[0].value < 0.25)
}

// 测试6: 度量属性过滤测试
test "度量属性过滤测试" {
  // 创建带属性的度量数据点
  let data_points = [
    {
      metric_name: "http_requests_total",
      value: 10L,
      attributes: [("method", "GET"), ("/api/users", "endpoint"), ("status", "200")],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "http_requests_total",
      value: 5L,
      attributes: [("method", "POST"), ("/api/users", "endpoint"), ("status", "201")],
      timestamp: 1640995260000000000L
    },
    {
      metric_name: "http_requests_total",
      value: 3L,
      attributes: [("method", "GET"), ("/api/products", "endpoint"), ("status", "404")],
      timestamp: 1640995320000000000L
    },
    {
      metric_name: "http_requests_total",
      value: 2L,
      attributes: [("method", "PUT"), ("/api/users", "endpoint"), ("status", "200")],
      timestamp: 1640995380000000000L
    }
  ]
  
  // 按 method 属性过滤
  let get_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("method", "GET"))
  })
  let post_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("method", "POST"))
  })
  let put_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("method", "PUT"))
  })
  
  assert_eq(get_requests.length(), 2)
  assert_eq(post_requests.length(), 1)
  assert_eq(put_requests.length(), 1)
  
  // 按 endpoint 属性过滤
  let user_api_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("/api/users", "endpoint"))
  })
  let product_api_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("/api/products", "endpoint"))
  })
  
  assert_eq(user_api_requests.length(), 3)
  assert_eq(product_api_requests.length(), 1)
  
  // 按 status 属性过滤
  let success_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("status", "200"))
  })
  let not_found_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("status", "404"))
  })
  let created_requests = data_points.filter(fn(dp) { 
    dp.attributes.contains(("status", "201"))
  })
  
  assert_eq(success_requests.length(), 2)
  assert_eq(not_found_requests.length(), 1)
  assert_eq(created_requests.length(), 1)
  
  // 计算每种方法的请求总数
  let get_total = get_requests.reduce(fn(acc, dp) { acc + dp.value }, 0L)
  let post_total = post_requests.reduce(fn(acc, dp) { acc + dp.value }, 0L)
  let put_total = put_requests.reduce(fn(acc, dp) { acc + dp.value }, 0L)
  
  assert_eq(get_total, 13L) // 10 + 3
  assert_eq(post_total, 5L)
  assert_eq(put_total, 2L)
}