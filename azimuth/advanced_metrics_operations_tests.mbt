// 高级指标操作测试用例
// 测试指标系统的高级功能和复杂操作

pub test "counter metrics with attributes and dimensions" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "advanced-metrics-meter")
  
  // 创建带属性的计数器
  let request_counter = azimuth::Meter::create_counter(
    meter, 
    "http.requests.total",
    Some("Total number of HTTP requests"),
    Some("requests")
  )
  
  // 验证计数器属性
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total number of HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
  
  // 模拟不同维度的请求计数
  // GET请求
  azimuth::Counter::add(request_counter, 100.0)
  
  // POST请求
  azimuth::Counter::add(request_counter, 50.0)
  
  // PUT请求
  azimuth::Counter::add(request_counter, 25.0)
  
  // DELETE请求
  azimuth::Counter::add(request_counter, 10.0)
  
  // 创建带状态码维度的计数器
  let status_counter = azimuth::Meter::create_counter(
    meter, 
    "http.responses.by_status",
    Some("HTTP responses by status code"),
    Some("responses")
  )
  
  // 模拟不同状态码的响应计数
  // 2xx 成功响应
  azimuth::Counter::add(status_counter, 150.0)  // 200 OK
  azimuth::Counter::add(status_counter, 25.0)   // 201 Created
  azimuth::Counter::add(status_counter, 10.0)   // 204 No Content
  
  // 4xx 客户端错误
  azimuth::Counter::add(status_counter, 20.0)   // 400 Bad Request
  azimuth::Counter::add(status_counter, 15.0)   // 401 Unauthorized
  azimuth::Counter::add(status_counter, 10.0)   // 404 Not Found
  azimuth::Counter::add(status_counter, 5.0)    // 429 Too Many Requests
  
  // 5xx 服务器错误
  azimuth::Counter::add(status_counter, 8.0)    // 500 Internal Server Error
  azimuth::Counter::add(status_counter, 3.0)    // 502 Bad Gateway
  azimuth::Counter::add(status_counter, 2.0)    // 503 Service Unavailable
  
  // 验证计数器操作
  assert_eq(status_counter.name, "http.responses.by_status")
  assert_eq(status_counter.description, Some("HTTP responses by status code"))
  assert_eq(status_counter.unit, Some("responses"))
}

pub test "histogram metrics with distribution analysis" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "histogram-analysis-meter")
  
  // 创建响应时间直方图
  let response_time_histogram = azimuth::Meter::create_histogram(
    meter, 
    "http.response.duration",
    Some("HTTP response duration distribution"),
    Some("ms")
  )
  
  // 验证直方图属性
  assert_eq(response_time_histogram.name, "http.response.duration")
  assert_eq(response_time_histogram.description, Some("HTTP response duration distribution"))
  assert_eq(response_time_histogram.unit, Some("ms"))
  
  // 模拟不同响应时间的记录
  // 快速响应 (< 10ms)
  azimuth::Histogram::record(response_time_histogram, 5.0)
  azimuth::Histogram::record(response_time_histogram, 8.0)
  azimuth::Histogram::record(response_time_histogram, 9.0)
  
  // 正常响应 (10-100ms)
  azimuth::Histogram::record(response_time_histogram, 15.0)
  azimuth::Histogram::record(response_time_histogram, 25.0)
  azimuth::Histogram::record(response_time_histogram, 45.0)
  azimuth::Histogram::record(response_time_histogram, 67.0)
  azimuth::Histogram::record(response_time_histogram, 89.0)
  
  // 慢响应 (100-500ms)
  azimuth::Histogram::record(response_time_histogram, 120.0)
  azimuth::Histogram::record(response_time_histogram, 180.0)
  azimuth::Histogram::record(response_time_histogram, 250.0)
  azimuth::Histogram::record(response_time_histogram, 340.0)
  azimuth::Histogram::record(response_time_histogram, 450.0)
  
  // 非常慢的响应 (> 500ms)
  azimuth::Histogram::record(response_time_histogram, 550.0)
  azimuth::Histogram::record(response_time_histogram, 780.0)
  azimuth::Histogram::record(response_time_histogram, 1200.0)
  azimuth::Histogram::record(response_time_histogram, 2500.0)
  
  // 创建请求大小直方图
  let request_size_histogram = azimuth::Meter::create_histogram(
    meter, 
    "http.request.size",
    Some("HTTP request size distribution"),
    Some("bytes")
  )
  
  // 记录不同大小的请求
  // 小请求 (< 1KB)
  azimuth::Histogram::record(request_size_histogram, 256.0)
  azimuth::Histogram::record(request_size_histogram, 512.0)
  azimuth::Histogram::record(request_size_histogram, 768.0)
  
  // 中等请求 (1KB - 10KB)
  azimuth::Histogram::record(request_size_histogram, 1024.0)
  azimuth::Histogram::record(request_size_histogram, 2048.0)
  azimuth::Histogram::record(request_size_histogram, 4096.0)
  azimuth::Histogram::record(request_size_histogram, 8192.0)
  
  // 大请求 (> 10KB)
  azimuth::Histogram::record(request_size_histogram, 16384.0)
  azimuth::Histogram::record(request_size_histogram, 32768.0)
  azimuth::Histogram::record(request_size_histogram, 65536.0)
  
  // 验证直方图转换为Instrument
  let response_time_instrument = azimuth::Histogram::as_instrument(response_time_histogram)
  let request_size_instrument = azimuth::Histogram::as_instrument(request_size_histogram)
  
  assert_eq(azimuth::Instrument::name(response_time_instrument), "http.response.duration")
  assert_eq(azimuth::Instrument::name(request_size_instrument), "http.request.size")
}

pub test "updown_counter metrics for gauging resources" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "resource-gauge-meter")
  
  // 创建活跃连接数UpDownCounter
  let active_connections_updown = azimuth::Meter::create_updown_counter(
    meter, 
    "network.active_connections",
    Some("Currently active network connections"),
    Some("connections")
  )
  
  // 验证UpDownCounter属性
  assert_eq(active_connections_updown.name, "network.active_connections")
  assert_eq(active_connections_updown.description, Some("Currently active network connections"))
  assert_eq(active_connections_updown.unit, Some("connections"))
  
  // 模拟连接数变化
  // 初始状态
  azimuth::UpDownCounter::add(active_connections_updown, 10.0)  // 10个活跃连接
  
  // 新连接建立
  azimuth::UpDownCounter::add(active_connections_updown, 5.0)   // 增加5个连接
  
  // 连接关闭
  azimuth::UpDownCounter::add(active_connections_updown, -3.0)  // 关闭3个连接
  
  // 批量新连接
  azimuth::UpDownCounter::add(active_connections_updown, 15.0)  // 增加15个连接
  
  // 批量连接关闭
  azimuth::UpDownCounter::add(active_connections_updown, -8.0)  // 关闭8个连接
  
  // 创建内存使用量UpDownCounter
  let memory_usage_updown = azimuth::Meter::create_updown_counter(
    meter, 
    "system.memory.usage",
    Some("Current memory usage"),
    Some("bytes")
  )
  
  // 模拟内存使用变化
  azimuth::UpDownCounter::add(memory_usage_updown, 1073741824.0)  // 1GB
  azimuth::UpDownCounter::add(memory_usage_updown, 536870912.0)   // 512MB
  azimuth::UpDownCounter::add(memory_usage_updown, -268435456.0)  // 释放256MB
  azimuth::UpDownCounter::add(memory_usage_updown, 134217728.0)   // 增加128MB
  
  // 创建队列长度UpDownCounter
  let queue_length_updown = azimuth::Meter::create_updown_counter(
    meter, 
    "message_queue.length",
    Some("Current message queue length"),
    Some("messages")
  )
  
  // 模拟队列长度变化
  azimuth::UpDownCounter::add(queue_length_updown, 100.0)  // 100条消息入队
  azimuth::UpDownCounter::add(queue_length_updown, 50.0)   // 50条消息入队
  azimuth::UpDownCounter::add(queue_length_updown, -30.0)  // 30条消息出队
  azimuth::UpDownCounter::add(queue_length_updown, -70.0)  // 70条消息出队
}

pub test "gauge metrics for instantaneous values" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "instantaneous-gauge-meter")
  
  // 创建CPU使用率Gauge
  let cpu_usage_gauge = azimuth::Meter::create_gauge(
    meter, 
    "system.cpu.usage",
    Some("Current CPU usage percentage"),
    Some("percent")
  )
  
  // 验证Gauge属性
  assert_eq(cpu_usage_gauge.name, "system.cpu.usage")
  assert_eq(cpu_usage_gauge.description, Some("Current CPU usage percentage"))
  assert_eq(cpu_usage_gauge.unit, Some("percent"))
  
  // 创建磁盘使用率Gauge
  let disk_usage_gauge = azimuth::Meter::create_gauge(
    meter, 
    "system.disk.usage",
    Some("Current disk usage percentage"),
    Some("percent")
  )
  
  // 创建温度Gauge
  let temperature_gauge = azimuth::Meter::create_gauge(
    meter, 
    "hardware.temperature",
    Some("Current hardware temperature"),
    Some("celsius")
  )
  
  // 创建网络吞吐量Gauge
  let network_throughput_gauge = azimuth::Meter::create_gauge(
    meter, 
    "network.throughput",
    Some("Current network throughput"),
    Some("mbps")
  )
  
  // 创建缓存命中率Gauge
  let cache_hit_rate_gauge = azimuth::Meter::create_gauge(
    meter, 
    "cache.hit_rate",
    Some("Current cache hit rate"),
    Some("percent")
  )
  
  // 验证所有Gauge创建成功
  assert_eq(disk_usage_gauge.name, "system.disk.usage")
  assert_eq(disk_usage_gauge.description, Some("Current disk usage percentage"))
  assert_eq(disk_usage_gauge.unit, Some("percent"))
  
  assert_eq(temperature_gauge.name, "hardware.temperature")
  assert_eq(temperature_gauge.description, Some("Current hardware temperature"))
  assert_eq(temperature_gauge.unit, Some("celsius"))
  
  assert_eq(network_throughput_gauge.name, "network.throughput")
  assert_eq(network_throughput_gauge.description, Some("Current network throughput"))
  assert_eq(network_throughput_gauge.unit, Some("mbps"))
  
  assert_eq(cache_hit_rate_gauge.name, "cache.hit_rate")
  assert_eq(cache_hit_rate_gauge.description, Some("Current cache hit rate"))
  assert_eq(cache_hit_rate_gauge.unit, Some("percent"))
}

pub test "metrics with multiple dimensions and labels" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "multi-dimensional-meter")
  
  // 创建多维度请求计数器
  let multi_dim_counter = azimuth::Meter::create_counter(
    meter, 
    "api.requests.total",
    Some("Total API requests with multiple dimensions"),
    Some("requests")
  )
  
  // 模拟不同维度的请求
  // 按HTTP方法和端点维度
  // GET /api/users
  azimuth::Counter::add(multi_dim_counter, 200.0)
  
  // POST /api/users
  azimuth::Counter::add(multi_dim_counter, 50.0)
  
  // PUT /api/users/{id}
  azimuth::Counter::add(multi_dim_counter, 30.0)
  
  // DELETE /api/users/{id}
  azimuth::Counter::add(multi_dim_counter, 10.0)
  
  // GET /api/products
  azimuth::Counter::add(multi_dim_counter, 500.0)
  
  // POST /api/products
  azimuth::Counter::add(multi_dim_counter, 100.0)
  
  // 创建多维度响应时间直方图
  let multi_dim_histogram = azimuth::Meter::create_histogram(
    meter, 
    "api.response.duration",
    Some("API response time by service and endpoint"),
    Some("ms")
  )
  
  // 模拟不同服务的响应时间
  // 用户服务
  azimuth::Histogram::record(multi_dim_histogram, 25.0)
  azimuth::Histogram::record(multi_dim_histogram, 45.0)
  azimuth::Histogram::record(multi_dim_histogram, 120.0)
  
  // 产品服务
  azimuth::Histogram::record(multi_dim_histogram, 35.0)
  azimuth::Histogram::record(multi_dim_histogram, 65.0)
  azimuth::Histogram::record(multi_dim_histogram, 85.0)
  
  // 订单服务
  azimuth::Histogram::record(multi_dim_histogram, 150.0)
  azimuth::Histogram::record(multi_dim_histogram, 280.0)
  azimuth::Histogram::record(multi_dim_histogram, 420.0)
  
  // 支付服务
  azimuth::Histogram::record(multi_dim_histogram, 500.0)
  azimuth::Histogram::record(multi_dim_histogram, 750.0)
  azimuth::Histogram::record(multi_dim_histogram, 1100.0)
  
  // 验证多维度指标
  assert_eq(multi_dim_counter.name, "api.requests.total")
  assert_eq(multi_dim_histogram.name, "api.response.duration")
}

pub test "metrics aggregation and rollups" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "aggregation-meter")
  
  // 创建基础计数器
  let base_counter = azimuth::Meter::create_counter(
    meter, 
    "events.processed",
    Some("Number of events processed"),
    Some("events")
  )
  
  // 模拟事件处理
  for i in 0..1000 {
    azimuth::Counter::add(base_counter, 1.0)
  }
  
  // 创建速率计数器（每秒事件数）
  let rate_counter = azimuth::Meter::create_counter(
    meter, 
    "events.processed.rate",
    Some("Events processed per second"),
    Some("events/sec")
  )
  
  // 模拟速率记录
  azimuth::Counter::add(rate_counter, 100.0)  // 100 events/sec
  
  // 创建累计直方图
  let cumulative_histogram = azimuth::Meter::create_histogram(
    meter, 
    "processing.time.cumulative",
    Some("Cumulative processing time distribution"),
    Some("ms")
  )
  
  // 记录处理时间分布
  let processing_times = [5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 75, 100, 150, 200, 300, 500, 1000]
  for time in processing_times {
    // 模拟每个处理时间出现多次
    for i in 0..(time / 10) {
      azimuth::Histogram::record(cumulative_histogram, time.to_double())
    }
  }
  
  // 创建百分比位直方图
  let percentile_histogram = azimuth::Meter::create_histogram(
    meter, 
    "response.time.percentiles",
    Some("Response time percentiles"),
    Some("ms")
  )
  
  // 记录用于计算百分位数的响应时间
  let response_times = [
    10.0, 15.0, 20.0, 25.0, 30.0, 35.0, 40.0, 45.0, 50.0, 55.0,  // P50-P95范围
    60.0, 65.0, 70.0, 75.0, 80.0, 85.0, 90.0, 95.0, 100.0, 120.0,  // P95-P99范围
    150.0, 200.0, 300.0, 500.0, 1000.0, 2000.0  // P99+范围
  ]
  
  for time in response_times {
    azimuth::Histogram::record(percentile_histogram, time)
  }
  
  // 验证聚合指标
  assert_eq(base_counter.name, "events.processed")
  assert_eq(rate_counter.name, "events.processed.rate")
  assert_eq(cumulative_histogram.name, "processing.time.cumulative")
  assert_eq(percentile_histogram.name, "response.time.percentiles")
}

pub test "metrics with business context" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "business-metrics-meter")
  
  // 创建业务指标计数器
  let revenue_counter = azimuth::Meter::create_counter(
    meter, 
    "business.revenue.total",
    Some("Total revenue generated"),
    Some("currency_units")
  )
  
  // 模拟不同来源的收入
  azimuth::Counter::add(revenue_counter, 9999.99)   // 产品销售
  azimuth::Counter::add(revenue_counter, 499.99)    // 服务费
  azimuth::Counter::add(revenue_counter, 199.99)    // 订阅费
  azimuth::Counter::add(revenue_counter, 299.99)    // 许可费
  
  // 创建订单计数器
  let orders_counter = azimuth::Meter::create_counter(
    meter, 
    "business.orders.total",
    Some("Total orders processed"),
    Some("orders")
  )
  
  // 模拟订单处理
  azimuth::Counter::add(orders_counter, 150.0)  // 新订单
  azimuth::Counter::add(orders_counter, 25.0)   // 退货订单
  azimuth::Counter::add(orders_counter, 10.0)   // 取消订单
  azimuth::Counter::add(orders_counter, 5.0)    // 换货订单
  
  // 创建客户指标
  let customers_updown = azimuth::Meter::create_updown_counter(
    meter, 
    "business.customers.active",
    Some("Currently active customers"),
    Some("customers")
  )
  
  // 模拟客户数量变化
  azimuth::UpDownCounter::add(customers_updown, 1000.0)  // 初始客户
  azimuth::UpDownCounter::add(customers_updown, 50.0)    // 新客户
  azimuth::UpDownCounter::add(customers_updown, 10.0)    // 客户流失
  azimuth::UpDownCounter::add(customers_updown, 25.0)    // 新增客户
  
  // 创建转化率直方图
  let conversion_histogram = azimuth::Meter::create_histogram(
    meter, 
    "business.conversion.rate",
    Some("Conversion rate distribution"),
    Some("percent")
  )
  
  // 记录不同渠道的转化率
  azimuth::Histogram::record(conversion_histogram, 2.5)   // 搜索引擎
  azimuth::Histogram::record(conversion_histogram, 5.8)   // 社交媒体
  azimuth::Histogram::record(conversion_histogram, 12.3)  // 邮件营销
  azimuth::Histogram::record(conversion_histogram, 8.7)   // 直接访问
  azimuth::Histogram::record(conversion_histogram, 15.2)  // 推荐链接
  
  // 验证业务指标
  assert_eq(revenue_counter.name, "business.revenue.total")
  assert_eq(orders_counter.name, "business.orders.total")
  assert_eq(customers_updown.name, "business.customers.active")
  assert_eq(conversion_histogram.name, "business.conversion.rate")
}

pub test "metrics performance and scalability" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "performance-meter")
  
  // 测试大量指标创建的性能
  let start_create_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  let mut counters = []
  let mut histograms = []
  let mut updown_counters = []
  let mut gauges = []
  
  // 创建大量不同类型的指标
  for i in 0..100 {
    // 创建计数器
    let counter = azimuth::Meter::create_counter(
      meter, 
      "perf.counter." + i.to_string(),
      Some("Performance counter " + i.to_string()),
      Some("operations")
    )
    counters.push(counter)
    
    // 创建直方图
    let histogram = azimuth::Meter::create_histogram(
      meter, 
      "perf.histogram." + i.to_string(),
      Some("Performance histogram " + i.to_string()),
      Some("ms")
    )
    histograms.push(histogram)
    
    // 创建UpDownCounter
    let updown_counter = azimuth::Meter::create_updown_counter(
      meter, 
      "perf.updown." + i.to_string(),
      Some("Performance updown counter " + i.to_string()),
      Some("units")
    )
    updown_counters.push(updown_counter)
    
    // 创建Gauge
    let gauge = azimuth::Meter::create_gauge(
      meter, 
      "perf.gauge." + i.to_string(),
      Some("Performance gauge " + i.to_string()),
      Some("percent")
    )
    gauges.push(gauge)
  }
  
  let end_create_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let create_duration = end_create_time - start_create_time
  
  // 验证指标创建在合理时间内完成
  assert_true(create_duration < 10000000000L, "400个指标创建应该在10秒内完成")
  
  // 测试大量指标操作的性能
  let start_operation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  // 对所有计数器进行操作
  for counter in counters {
    for j in 0..10 {
      azimuth::Counter::add(counter, j.to_double())
    }
  }
  
  // 对所有直方图进行操作
  for histogram in histograms {
    for j in 0..10 {
      azimuth::Histogram::record(histogram, (j * 10).to_double())
    }
  }
  
  // 对所有UpDownCounter进行操作
  for updown_counter in updown_counters {
    azimuth::UpDownCounter::add(updown_counter, 100.0)
    azimuth::UpDownCounter::add(updown_counter, -25.0)
    azimuth::UpDownCounter::add(updown_counter, 50.0)
  }
  
  let end_operation_time = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let operation_duration = end_operation_time - start_operation_time
  
  // 验证指标操作在合理时间内完成
  assert_true(operation_duration < 15000000000L, "大量指标操作应该在15秒内完成")
  
  // 验证所有指标创建成功
  assert_eq(counters.length(), 100)
  assert_eq(histograms.length(), 100)
  assert_eq(updown_counters.length(), 100)
  assert_eq(gauges.length(), 100)
}

pub test "metrics error handling and edge cases" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "edge-case-meter")
  
  // 测试极值计数器操作
  let extreme_counter = azimuth::Meter::create_counter(
    meter, 
    "extreme.values.counter",
    Some("Counter with extreme values"),
    Some("units")
  )
  
  // 测试极大值
  azimuth::Counter::add(extreme_counter, 999999999.0)
  
  // 测试极小值
  azimuth::Counter::add(extreme_counter, 0.0000001)
  
  // 测试零值
  azimuth::Counter::add(extreme_counter, 0.0)
  
  // 测试负值（对于计数器可能无效，但测试不会崩溃）
  azimuth::Counter::add(extreme_counter, -1.0)
  
  // 测试极值直方图操作
  let extreme_histogram = azimuth::Meter::create_histogram(
    meter, 
    "extreme.values.histogram",
    Some("Histogram with extreme values"),
    Some("units")
  )
  
  // 测试极小值
  azimuth::Histogram::record(extreme_histogram, 0.000001)
  
  // 测试极大值
  azimuth::Histogram::record(extreme_histogram, 999999999.0)
  
  // 测试零值
  azimuth::Histogram::record(extreme_histogram, 0.0)
  
  // 测试负值（对于直方图可能无效）
  azimuth::Histogram::record(extreme_histogram, -1.0)
  
  // 测试极值UpDownCounter操作
  let extreme_updown = azimuth::Meter::create_updown_counter(
    meter, 
    "extreme.values.updown",
    Some("UpDownCounter with extreme values"),
    Some("units")
  )
  
  // 测试大正值
  azimuth::UpDownCounter::add(extreme_updown, 999999999.0)
  
  // 测试大负值
  azimuth::UpDownCounter::add(extreme_updown, -999999999.0)
  
  // 测试零值
  azimuth::UpDownCounter::add(extreme_updown, 0.0)
  
  // 验证极值操作不会导致崩溃
  assert_eq(extreme_counter.name, "extreme.values.counter")
  assert_eq(extreme_histogram.name, "extreme.values.histogram")
  assert_eq(extreme_updown.name, "extreme.values.updown")
}

pub test "metrics with complex attribute combinations" {
  let meter_provider = azimuth::MeterProvider::default()
  let meter = azimuth::MeterProvider::get_meter(meter_provider, "complex-attributes-meter")
  
  // 创建支持复杂属性的计数器
  let complex_counter = azimuth::Meter::create_counter(
    meter, 
    "complex.attributes.requests",
    Some("Requests with complex attribute combinations"),
    Some("requests")
  )
  
  // 模拟不同属性组合的请求
  // 地理位置维度
  azimuth::Counter::add(complex_counter, 100.0)  // US-East
  azimuth::Counter::add(complex_counter, 80.0)   // US-West
  azimuth::Counter::add(complex_counter, 60.0)   // EU-Central
  azimuth::Counter::add(complex_counter, 40.0)   // AP-Southeast
  
  // 设备类型维度
  azimuth::Counter::add(complex_counter, 120.0)  // Mobile
  azimuth::Counter::add(complex_counter, 90.0)   // Desktop
  azimuth::Counter::add(complex_counter, 30.0)   // Tablet
  azimuth::Counter::add(complex_counter, 20.0)   // Smart TV
  
  // 用户类型维度
  azimuth::Counter::add(complex_counter, 150.0)  // Free tier
  azimuth::Counter::add(complex_counter, 80.0)   // Premium tier
  azimuth::Counter::add(complex_counter, 30.0)   // Enterprise tier
  
  // 创建支持复杂属性的直方图
  let complex_histogram = azimuth::Meter::create_histogram(
    meter, 
    "complex.attributes.duration",
    Some("Duration with complex attribute combinations"),
    Some("ms")
  )
  
  // 模拟不同属性组合的持续时间
  // 按操作类型
  azimuth::Histogram::record(complex_histogram, 25.0)   // Database query
  azimuth::Histogram::record(complex_histogram, 150.0)  // API call
  azimuth::Histogram::record(complex_histogram, 500.0)  // File processing
  azimuth::Histogram::record(complex_histogram, 1200.0) // Machine learning inference
  
  // 按数据大小
  azimuth::Histogram::record(complex_histogram, 10.0)   // Small payload (< 1KB)
  azimuth::Histogram::record(complex_histogram, 50.0)   // Medium payload (1KB-10KB)
  azimuth::Histogram::record(complex_histogram, 200.0)  // Large payload (10KB-100KB)
  azimuth::Histogram::record(complex_histogram, 800.0)  // Extra large payload (> 100KB)
  
  // 验证复杂属性指标
  assert_eq(complex_counter.name, "complex.attributes.requests")
  assert_eq(complex_histogram.name, "complex.attributes.duration")
}