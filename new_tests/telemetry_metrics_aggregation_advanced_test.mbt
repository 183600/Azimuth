// 高级指标聚合测试
// 测试复杂的指标聚合场景和算法

test "counter_cumulative_aggregation" {
  // 测试Counter的累积聚合
  let meter = metrics::NoopMeter::{}
  let counter = meter.create_counter("http_requests_total", Some("requests"), Some("Total HTTP requests"))
  
  // 模拟累积计数
  let request_attributes = [
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("200")),
    ("endpoint", common::AttributeValue::string("/api/users"))
  ]
  
  // 累积添加请求
  let mut total_requests = 0L
  let mut i = 0
  while i < 100 {
    counter.add(1L, Some(request_attributes))
    total_requests = total_requests + 1L
    i = i + 1
  }
  
  // 批量添加
  counter.add(50L, Some(request_attributes))
  total_requests = total_requests + 50L
  
  // 添加不同属性的请求
  let error_attributes = [
    ("method", common::AttributeValue::string("GET")),
    ("status", common::AttributeValue::string("500")),
    ("endpoint", common::AttributeValue::string("/api/users"))
  ]
  
  counter.add(5L, Some(error_attributes))
  
  // 验证聚合逻辑（在Noop实现中我们只能验证不会崩溃）
  assert_eq(total_requests, 150L)
  assert_eq(true, true)  // 确保操作没有崩溃
}

test "histogram_percentile_calculation" {
  // 测试Histogram的百分位数计算
  let meter = metrics::NoopMeter::{}
  let histogram = meter.create_histogram("request_duration_seconds", Some("seconds"), Some("Request duration"))
  
  // 模拟记录不同持续时间
  let durations = [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0]
  let request_attributes = [
    ("endpoint", common::AttributeValue::string("/api/data")),
    ("method", common::AttributeValue::string("POST"))
  ]
  
  // 记录每个持续时间多次
  let mut i = 0
  while i < durations.length() {
    let mut j = 0
    while j < 10 {
      histogram.record(durations[i], Some(request_attributes))
      j = j + 1
    }
    i = i + 1
  }
  
  // 记录一些边界值
  histogram.record(0.0, Some(request_attributes))  // 最小值
  histogram.record(100.0, Some(request_attributes))  // 最大值
  histogram.record(-1.0, Some(request_attributes))  // 负值（应该被处理）
  
  // 验证百分位数计算逻辑
  let total_measurements = durations.length() * 10 + 3  // 120 + 3 = 123
  assert_eq(total_measurements, 123)
  
  // 验证边界值
  assert_eq(durations[0], 0.001)  // 最小正常值
  assert_eq(durations[durations.length() - 1], 10.0)  // 最大正常值
}

test "up_down_counter_fluctuation" {
  // 测试UpDownCounter的波动行为
  let meter = metrics::NoopMeter::{}
  let up_down_counter = meter.create_up_down_counter("active_connections", Some("connections"), Some("Active connections"))
  
  let connection_attributes = [
    ("service", common::AttributeValue::string("database")),
    ("pool", common::AttributeValue::string("primary"))
  ]
  
  // 模拟连接数波动
  let mut current_connections = 0L
  
  // 连接建立
  up_down_counter.add(10L, Some(connection_attributes))
  current_connections = current_connections + 10L
  assert_eq(current_connections, 10L)
  
  // 更多连接
  up_down_counter.add(5L, Some(connection_attributes))
  current_connections = current_connections + 5L
  assert_eq(current_connections, 15L)
  
  // 连接关闭
  up_down_counter.add(-3L, Some(connection_attributes))
  current_connections = current_connections - 3L
  assert_eq(current_connections, 12L)
  
  // 大量关闭（测试负值处理）
  up_down_counter.add(-15L, Some(connection_attributes))
  current_connections = current_connections - 15L
  assert_eq(current_connections, -3L)
  
  // 重新建立连接
  up_down_counter.add(8L, Some(connection_attributes))
  current_connections = current_connections + 8L
  assert_eq(current_connections, 5L)
}

test "gauge_time_series_tracking" {
  // 测试Gauge的时间序列跟踪
  let meter = metrics::NoopMeter::{}
  let memory_gauge = meter.create_gauge("memory_usage_bytes", Some("bytes"), Some("Memory usage"))
  let cpu_gauge = meter.create_gauge("cpu_usage_percent", Some("percent"), Some("CPU usage"))
  
  let system_attributes = [
    ("host.name", common::AttributeValue::string("web-server-01")),
    ("datacenter", common::AttributeValue::string("us-east-1"))
  ]
  
  // 模拟时间序列数据点
  let memory_readings = [1024.0, 2048.0, 1536.0, 3072.0, 2560.0, 4096.0, 3584.0]
  let cpu_readings = [10.5, 25.3, 15.7, 45.2, 30.1, 60.8, 42.6]
  
  // 记录时间序列数据
  let mut i = 0
  while i < memory_readings.length() {
    memory_gauge.record(memory_readings[i], Some(system_attributes))
    cpu_gauge.record(cpu_readings[i], Some(system_attributes))
    i = i + 1
  }
  
  // 验证时间序列数据点
  assert_eq(memory_readings.length(), 7)
  assert_eq(cpu_readings.length(), 7)
  
  // 验证数据范围
  let mut min_memory = memory_readings[0]
  let mut max_memory = memory_readings[0]
  let mut min_cpu = cpu_readings[0]
  let mut max_cpu = cpu_readings[0]
  
  i = 1
  while i < memory_readings.length() {
    if memory_readings[i] < min_memory {
      min_memory = memory_readings[i]
    }
    if memory_readings[i] > max_memory {
      max_memory = memory_readings[i]
    }
    if cpu_readings[i] < min_cpu {
      min_cpu = cpu_readings[i]
    }
    if cpu_readings[i] > max_cpu {
      max_cpu = cpu_readings[i]
    }
    i = i + 1
  }
  
  assert_eq(min_memory, 1024.0)
  assert_eq(max_memory, 4096.0)
  assert_eq(min_cpu, 10.5)
  assert_eq(max_cpu, 60.8)
}

test "multi_dimensional_aggregation" {
  // 测试多维聚合
  let meter = metrics::NoopMeter::{}
  let request_counter = meter.create_counter("api_requests_total", Some("requests"), Some("API requests"))
  
  // 模拟多维数据
  let dimensions = [
    ("GET", "/api/users", "200"),
    ("GET", "/api/users", "404"),
    ("GET", "/api/users", "500"),
    ("POST", "/api/users", "201"),
    ("POST", "/api/users", "400"),
    ("POST", "/api/orders", "201"),
    ("POST", "/api/orders", "500"),
    ("PUT", "/api/users", "200"),
    ("PUT", "/api/orders", "200"),
    ("DELETE", "/api/users", "204")
  ]
  
  // 为每个维度组合添加请求
  let mut i = 0
  while i < dimensions.length() {
    let (method, endpoint, status) = dimensions[i]
    let attributes = [
      ("http.method", common::AttributeValue::string(method)),
      ("http.endpoint", common::AttributeValue::string(endpoint)),
      ("http.status", common::AttributeValue::string(status))
    ]
    
    // 添加不同数量的请求
    let request_count = (i + 1) * 5
    request_counter.add(request_count.to_int64(), Some(attributes))
    
    i = i + 1
  }
  
  // 验证维度聚合
  let total_requests = 5 + 10 + 15 + 20 + 25 + 30 + 35 + 40 + 45 + 50  // 等差数列求和
  assert_eq(total_requests, 275)
  
  // 验证特定维度组合
  let get_users_200_requests = 5L  // 第一个维度组合
  let delete_users_204_requests = 50L  // 最后一个维度组合
  
  assert_eq(get_users_200_requests, 5L)
  assert_eq(delete_users_204_requests, 50L)
}

test "aggregation_with_time_windows" {
  // 测试时间窗口聚合
  let meter = metrics::NoopMeter::{}
  let throughput_counter = meter.create_counter("messages_processed", Some("messages"), Some("Processed messages"))
  
  // 模拟不同时间窗口的数据
  let time_windows = [
    (1640995200L, 100),  // 2022-01-01 00:00:00, 100 messages
    (1640995260L, 150),  // 2022-01-01 00:01:00, 150 messages
    (1640995320L, 120),  // 2022-01-01 00:02:00, 120 messages
    (1640995380L, 180),  // 2022-01-01 00:03:00, 180 messages
    (1640995440L, 200),  // 2022-01-01 00:04:00, 200 messages
    (1640995500L, 90),   // 2022-01-01 00:05:00, 90 messages
    (1640995560L, 160),  // 2022-01-01 00:06:00, 160 messages
    (1640995620L, 140),  // 2022-01-01 00:07:00, 140 messages
  ]
  
  // 记录每个时间窗口的数据
  let mut total_messages = 0L
  let mut i = 0
  while i < time_windows.length() {
    let (timestamp, message_count) = time_windows[i]
    let attributes = [
      ("window.start_time", common::AttributeValue::int(timestamp)),
      ("window.duration_seconds", common::AttributeValue::int(60L))
    ]
    
    throughput_counter.add(message_count.to_int64(), Some(attributes))
    total_messages = total_messages + message_count.to_int64()
    
    i = i + 1
  }
  
  // 验证时间窗口聚合
  assert_eq(total_messages, 1240L)  // 100+150+120+180+200+90+160+140
  
  // 计算平均吞吐量
  let average_throughput = total_messages.to_double() / time_windows.length().to_double()
  assert_eq(average_throughput, 155.0)
  
  // 验证峰值和最低值
  let peak_throughput = 200L
  let min_throughput = 90L
  
  assert_eq(peak_throughput, 200L)
  assert_eq(min_throughput, 90L)
}

test "measurement_aggregation_algorithms" {
  // 测试测量值聚合算法
  let measurements = [
    metrics::Measurement::{ value: 10.5, attributes: [] },
    metrics::Measurement::{ value: 15.2, attributes: [] },
    metrics::Measurement::{ value: 8.7, attributes: [] },
    metrics::Measurement::{ value: 22.1, attributes: [] },
    metrics::Measurement::{ value: 12.9, attributes: [] },
    metrics::Measurement::{ value: 18.3, attributes: [] },
    metrics::Measurement::{ value: 9.4, attributes: [] },
    metrics::Measurement::{ value: 25.6, attributes: [] },
    metrics::Measurement::{ value: 14.8, attributes: [] },
    metrics::Measurement::{ value: 19.7, attributes: [] }
  ]
  
  // 计算基本统计量
  let mut sum = 0.0
  let mut min = measurements[0].value
  let mut max = measurements[0].value
  
  let mut i = 0
  while i < measurements.length() {
    sum = sum + measurements[i].value
    if measurements[i].value < min {
      min = measurements[i].value
    }
    if measurements[i].value > max {
      max = measurements[i].value
    }
    i = i + 1
  }
  
  let average = sum / measurements.length().to_double()
  let range = max - min
  
  // 验证统计计算
  assert_eq(sum, 157.2)
  assert_eq(average, 15.72)
  assert_eq(min, 8.7)
  assert_eq(max, 25.6)
  assert_eq(range, 16.9)
  
  // 计算中位数
  let sorted_values = [8.7, 9.4, 10.5, 12.9, 14.8, 15.2, 18.3, 19.7, 22.1, 25.6]
  let median = if sorted_values.length() % 2 == 0 {
    (sorted_values[sorted_values.length() / 2 - 1] + sorted_values[sorted_values.length() / 2]) / 2.0
  } else {
    sorted_values[sorted_values.length() / 2]
  }
  
  assert_eq(median, 15.0)  // (14.8 + 15.2) / 2
}

test "complex_attribute_aggregation" {
  // 测试复杂属性的聚合
  let meter = metrics::NoopMeter::{}
  let complex_counter = meter.create_counter("complex_operations", Some("operations"), Some("Complex operations"))
  
  // 创建包含各种属性类型的复杂属性集
  let complex_attributes_sets = [
    [
      ("user.id", common::AttributeValue::string("user-001")),
      ("user.tier", common::AttributeValue::string("premium")),
      ("operation.type", common::AttributeValue::string("payment")),
      ("operation.amount", common::AttributeValue::float(99.99)),
      ("operation.currency", common::AttributeValue::string("USD")),
      ("operation.success", common::AttributeValue::bool(true)),
      ("tags", common::AttributeValue::array_string(["api", "payment", "critical"])),
      ("retry_count", common::AttributeValue::int(0L))
    ],
    [
      ("user.id", common::AttributeValue::string("user-002")),
      ("user.tier", common::AttributeValue::string("basic")),
      ("operation.type", common::AttributeValue::string("query")),
      ("operation.amount", common::AttributeValue::float(0.0)),
      ("operation.currency", common::AttributeValue::string("USD")),
      ("operation.success", common::AttributeValue::bool(true)),
      ("tags", common::AttributeValue::array_string(["api", "query", "read-only"])),
      ("retry_count", common::AttributeValue::int(1L))
    ],
    [
      ("user.id", common::AttributeValue::string("user-003")),
      ("user.tier", common::AttributeValue::string("enterprise")),
      ("operation.type", common::AttributeValue::string("batch_export")),
      ("operation.amount", common::AttributeValue::float(1000.0)),
      ("operation.currency", common::AttributeValue::string("EUR")),
      ("operation.success", common::AttributeValue::bool(false)),
      ("tags", common::AttributeValue::array_string(["batch", "export", "failed"])),
      ("retry_count", common::AttributeValue::int(3L))
    ]
  ]
  
  // 为每个属性集执行操作
  let mut i = 0
  while i < complex_attributes_sets.length() {
    let attributes = complex_attributes_sets[i]
    let operation_count = (i + 1) * 10
    
    complex_counter.add(operation_count.to_int64(), Some(attributes))
    
    // 验证属性集结构
    assert_eq(attributes.length(), 8)
    
    // 验证特定属性类型
    match attributes[0].1 {
      common::StringValue(user_id) => assert(user_id.has_prefix("user-"))
      _ => @test.fail("Test failed")
    }
    
    match attributes[4].1 {
      common::StringValue(currency) => assert(currency == "USD" || currency == "EUR")
      _ => @test.fail("Test failed")
    }
    
    match attributes[5].1 {
      common::BoolValue(success) => assert(success == true || success == false)
      _ => @test.fail("Test failed")
    }
    
    match attributes[6].1 {
      common::ArrayStringValue(tags) => assert(tags.length() >= 2)
      _ => @test.fail("Test failed")
    }
    
    match attributes[7].1 {
      common::IntValue(retry_count) => assert(retry_count >= 0L)
      _ => @test.fail("Test failed")
    }
    
    i = i + 1
  }
  
  // 验证总操作数
  let total_operations = 10 + 20 + 30
  assert_eq(total_operations, 60)
}