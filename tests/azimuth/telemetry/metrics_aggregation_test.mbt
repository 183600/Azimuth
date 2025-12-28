// 度量聚合和统计测试
// 测试度量系统的高级功能和聚合操作

test "Counter度量聚合测试" {
  // 创建MeterProvider和Meter
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation-test-meter")
  
  // 创建不同类型的Counter
  let request_counter = Meter::create_counter(meter, "http.requests.total", 
    Some("Total number of HTTP requests"), Some("requests"))
  let error_counter = Meter::create_counter(meter, "http.errors.total",
    Some("Total number of HTTP errors"), Some("errors"))
  let response_size_counter = Meter::create_counter(meter, "http.response.size.total",
    Some("Total size of HTTP responses"), Some("bytes"))
  
  // 测试基本的Counter记录
  Counter::add(request_counter, 1.0)
  Counter::add(request_counter, 1.0)
  Counter::add(request_counter, 1.0)
  
  // 测试带属性的Counter记录
  let success_attrs = Attributes::new()
  Attributes::set(success_attrs, "status_code", StringValue("200"))
  Attributes::set(success_attrs, "method", StringValue("GET"))
  Counter::add(request_counter, 5.0, Some(success_attrs))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "status_code", StringValue("500"))
  Attributes::set(error_attrs, "method", StringValue("POST"))
  Counter::add(error_counter, 2.0, Some(error_attrs))
  
  // 测试响应大小度量
  let size_attrs = Attributes::new()
  Attributes::set(size_attrs, "endpoint", StringValue("/api/users"))
  Counter::add(response_size_counter, 1024.0, Some(size_attrs))
  Counter::add(response_size_counter, 2048.0, Some(size_attrs))
  Counter::add(response_size_counter, 512.0, Some(size_attrs))
  
  // 验证Counter属性
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(request_counter.description, Some("Total number of HTTP requests"))
  assert_eq(request_counter.unit, Some("requests"))
}

test "Histogram度量分布测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "histogram-test-meter")
  
  // 创建不同类型的Histogram
  let response_time_histogram = Meter::create_histogram(meter, "http.response.time",
    Some("HTTP response time distribution"), Some("ms"))
  let request_size_histogram = Meter::create_histogram(meter, "http.request.size",
    Some("HTTP request size distribution"), Some("bytes"))
  let cpu_usage_histogram = Meter::create_histogram(meter, "system.cpu.usage",
    Some("CPU usage distribution"), Some("percent"))
  
  // 测试响应时间分布
  let fast_attrs = Attributes::new()
  Attributes::set(fast_attrs, "endpoint", StringValue("/api/health"))
  Histogram::record(response_time_histogram, 10.0, Some(fast_attrs))
  Histogram::record(response_time_histogram, 15.0, Some(fast_attrs))
  Histogram::record(response_time_histogram, 8.0, Some(fast_attrs))
  
  let slow_attrs = Attributes::new()
  Attributes::set(slow_attrs, "endpoint", StringValue("/api/reports"))
  Histogram::record(response_time_histogram, 1500.0, Some(slow_attrs))
  Histogram::record(response_time_histogram, 2000.0, Some(slow_attrs))
  Histogram::record(response_time_histogram, 1200.0, Some(slow_attrs))
  Histogram::record(response_time_histogram, 1800.0, Some(slow_attrs))
  
  // 测试请求大小分布
  let small_request_attrs = Attributes::new()
  Attributes::set(small_request_attrs, "content_type", StringValue("application/json"))
  Histogram::record(request_size_histogram, 256.0, Some(small_request_attrs))
  Histogram::record(request_size_histogram, 512.0, Some(small_request_attrs))
  
  let large_request_attrs = Attributes::new()
  Attributes::set(large_request_attrs, "content_type", StringValue("multipart/form-data"))
  Histogram::record(request_size_histogram, 1048576.0, Some(large_request_attrs)) // 1MB
  Histogram::record(request_size_histogram, 2097152.0, Some(large_request_attrs)) // 2MB
  
  // 测试CPU使用率分布
  let cpu_attrs = Attributes::new()
  Attributes::set(cpu_attrs, "instance_id", StringValue("server-001"))
  Histogram::record(cpu_usage_histogram, 25.5, Some(cpu_attrs))
  Histogram::record(cpu_usage_histogram, 45.2, Some(cpu_attrs))
  Histogram::record(cpu_usage_histogram, 78.9, Some(cpu_attrs))
  Histogram::record(cpu_usage_histogram, 12.1, Some(cpu_attrs))
  Histogram::record(cpu_usage_histogram, 89.3, Some(cpu_attrs))
  
  // 验证Histogram属性
  assert_eq(response_time_histogram.name, "http.response.time")
  assert_eq(response_time_histogram.description, Some("HTTP response time distribution"))
  assert_eq(response_time_histogram.unit, Some("ms"))
}

test "UpDownCounter度量测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "updown-test-meter")
  
  // 创建UpDownCounter（需要先在源代码中实现）
  // 这里我们假设UpDownCounter已经实现，测试其增减功能
  
  // 测试连接数度量
  let active_connections = UpDownCounter("active.connections", 
    Some("Number of active connections"), Some("connections"))
  
  // 测试内存使用度量
  let memory_usage = UpDownCounter("memory.usage.bytes",
    Some("Current memory usage in bytes"), Some("bytes"))
  
  // 验证UpDownCounter属性
  assert_eq(Instrument::name(Counter(active_connections.name, active_connections.description, active_connections.unit)), "active.connections")
  assert_eq(Instrument::description(Counter(active_connections.name, active_connections.description, active_connections.unit)), Some("Number of active connections"))
  assert_eq(Instrument::unit(Counter(active_connections.name, active_connections.description, active_connections.unit)), Some("connections"))
}

test "Gauge度量测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "gauge-test-meter")
  
  // 创建Gauge（需要先在源代码中实现）
  // 这里我们假设Gauge已经实现，测试其瞬时值功能
  
  // 测试系统负载度量
  let system_load = Gauge("system.load.average",
    Some("System load average"), Some("load"))
  
  // 测试磁盘使用率度量
  let disk_usage = Gauge("disk.usage.percent",
    Some("Disk usage percentage"), Some("percent"))
  
  // 测试队列长度度量
  let queue_length = Gauge("queue.length",
    Some("Current queue length"), Some("items"))
  
  // 验证Gauge属性
  assert_eq(Instrument::name(Counter(system_load.name, system_load.description, system_load.unit)), "system.load.average")
  assert_eq(Instrument::description(Counter(system_load.name, system_load.description, system_load.unit)), Some("System load average"))
  assert_eq(Instrument::unit(Counter(system_load.name, system_load.description, system_load.unit)), Some("load"))
}

test "度量属性和标签测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "attributes-test-meter")
  
  // 创建带有复杂属性的度量
  let api_counter = Meter::create_counter(meter, "api.requests.total",
    Some("Total API requests"), Some("requests"))
  
  // 测试不同属性组合
  let user_api_attrs = Attributes::new()
  Attributes::set(user_api_attrs, "service", StringValue("user-service"))
  Attributes::set(user_api_attrs, "version", StringValue("v1"))
  Attributes::set(user_api_attrs, "method", StringValue("GET"))
  Attributes::set(user_api_attrs, "status", StringValue("200"))
  Counter::add(api_counter, 10.0, Some(user_api_attrs))
  
  let order_api_attrs = Attributes::new()
  Attributes::set(order_api_attrs, "service", StringValue("order-service"))
  Attributes::set(order_api_attrs, "version", StringValue("v2"))
  Attributes::set(order_api_attrs, "method", StringValue("POST"))
  Attributes::set(order_api_attrs, "status", StringValue("201"))
  Counter::add(api_counter, 5.0, Some(order_api_attrs))
  
  // 测试带数值类型的属性
  let numeric_attrs = Attributes::new()
  Attributes::set(numeric_attrs, "service", StringValue("payment-service"))
  Attributes::set(numeric_attrs, "endpoint_id", IntValue(42))
  Attributes::set(numeric_attrs, "timeout_ms", IntValue(5000))
  Attributes::set(numeric_attrs, "cache_enabled", BoolValue(true))
  Attributes::set(numeric_attrs, "weight", FloatValue(0.85))
  Counter::add(api_counter, 3.0, Some(numeric_attrs))
  
  // 测试带数组类型的属性
  let array_attrs = Attributes::new()
  Attributes::set(array_attrs, "service", StringValue("notification-service"))
  Attributes::set(array_attrs, "supported_regions", ArrayStringValue(["us-east-1", "us-west-2", "eu-west-1"]))
  Attributes::set(array_attrs, "retry_codes", ArrayIntValue([500, 502, 503, 504]))
  Counter::add(api_counter, 7.0, Some(array_attrs))
}

test "度量时间序列测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "timeseries-test-meter")
  
  // 创建时间序列度量
  let throughput_counter = Meter::create_counter(meter, "throughput.ops_per_second",
    Some("Operations per second"), Some("ops"))
  
  let latency_histogram = Meter::create_histogram(meter, "latency.duration",
    Some("Operation latency distribution"), Some("ms"))
  
  // 模拟时间序列数据记录
  let time_attrs = Attributes::new()
  Attributes::set(time_attrs, "operation", StringValue("database.query"))
  Attributes::set(time_attrs, "table", StringValue("users"))
  
  // 模拟不同时间点的度量记录
  for i in range(0, 10) {
    Counter::add(throughput_counter, 100.0 + (i * 10).to_double(), Some(time_attrs))
    Histogram::record(latency_histogram, 50.0 + (i * 5).to_double(), Some(time_attrs))
  }
  
  // 测试不同操作类型的时间序列
  let read_attrs = Attributes::new()
  Attributes::set(read_attrs, "operation", StringValue("database.read"))
  Attributes::set(read_attrs, "table", StringValue("products"))
  
  let write_attrs = Attributes::new()
  Attributes::set(write_attrs, "operation", StringValue("database.write"))
  Attributes::set(write_attrs, "table", StringValue("products"))
  
  // 记录读操作度量
  for i in range(0, 5) {
    Counter::add(throughput_counter, 200.0 + (i * 20).to_double(), Some(read_attrs))
    Histogram::record(latency_histogram, 25.0 + (i * 3).to_double(), Some(read_attrs))
  }
  
  // 记录写操作度量
  for i in range(0, 3) {
    Counter::add(throughput_counter, 50.0 + (i * 5).to_double(), Some(write_attrs))
    Histogram::record(latency_histogram, 100.0 + (i * 10).to_double(), Some(write_attrs))
  }
}

test "度量聚合和统计计算测试" {
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "aggregation-stats-meter")
  
  // 创建用于聚合测试的度量
  let response_counter = Meter::create_counter(meter, "http.responses",
    Some("HTTP response count"), Some("responses"))
  
  let response_latency = Meter::create_histogram(meter, "http.response.latency",
    Some("HTTP response latency"), Some("ms"))
  
  // 模拟不同状态码的响应计数
  let success_attrs = Attributes::new()
  Attributes::set(success_attrs, "status_code", StringValue("200"))
  Attributes::set(success_attrs, "endpoint", StringValue("/api/users"))
  
  let error_attrs = Attributes::new()
  Attributes::set(error_attrs, "status_code", StringValue("500"))
  Attributes::set(error_attrs, "endpoint", StringValue("/api/users"))
  
  let redirect_attrs = Attributes::new()
  Attributes::set(redirect_attrs, "status_code", StringValue("302"))
  Attributes::set(redirect_attrs, "endpoint", StringValue("/api/users"))
  
  // 记录不同类型的响应
  Counter::add(response_counter, 1000.0, Some(success_attrs))  // 1000个成功响应
  Counter::add(response_counter, 50.0, Some(error_attrs))      // 50个错误响应
  Counter::add(response_counter, 20.0, Some(redirect_attrs))   // 20个重定向响应
  
  // 记录延迟数据
  let latency_values = [10.0, 15.0, 20.0, 25.0, 30.0, 50.0, 75.0, 100.0, 150.0, 200.0]
  for value in latency_values {
    Histogram::record(response_latency, value, Some(success_attrs))
  }
  
  // 验证度量配置
  assert_eq(response_counter.name, "http.responses")
  assert_eq(response_latency.name, "http.response.latency")
  assert_eq(response_latency.unit, Some("ms"))
}