// 仪表板数据聚合测试用例
// 测试遥测数据的聚合和仪表板展示功能

test "dashboard_metrics_aggregation" {
  // 测试仪表板指标聚合
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "dashboard-meter")
  
  // 创建多个指标
  let request_counter = Meter::create_counter(meter, "http.requests.total")
  let error_counter = Meter::create_counter(meter, "http.errors.total")
  let response_histogram = Meter::create_histogram(meter, "http.response.duration")
  let active_connections = Meter::create_gauge(meter, "http.active.connections")
  
  // 模拟不同服务的指标数据
  // 服务A的指标
  Counter::add(request_counter, 1000.0)
  Counter::add(error_counter, 50.0)
  Histogram::record(response_histogram, 120.0)
  Histogram::record(response_histogram, 150.0)
  Histogram::record(response_histogram, 180.0)
  
  // 服务B的指标
  Counter::add(request_counter, 800.0)
  Counter::add(error_counter, 30.0)
  Histogram::record(response_histogram, 90.0)
  Histogram::record(response_histogram, 110.0)
  
  // 服务C的指标
  Counter::add(request_counter, 1200.0)
  Counter::add(error_counter, 70.0)
  Histogram::record(response_histogram, 200.0)
  Histogram::record(response_histogram, 220.0)
  Histogram::record(response_histogram, 250.0)
  
  // 验证指标名称
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(error_counter.name, "http.errors.total")
  assert_eq(response_histogram.name, "http.response.duration")
  assert_eq(active_connections.name, "http.active.connections")
  
  // 验证instrument类型转换
  let request_instrument = Counter(request_counter.name, request_counter.description, request_counter.unit)
  let error_instrument = Counter(error_counter.name, error_counter.description, error_counter.unit)
  let response_instrument = Histogram(response_histogram.name, response_histogram.description, response_histogram.unit)
  let gauge_instrument = Gauge(active_connections.name, active_connections.description, active_connections.unit)
  
  assert_eq(Instrument::name(request_instrument), "http.requests.total")
  assert_eq(Instrument::name(error_instrument), "http.errors.total")
  assert_eq(Instrument::name(response_instrument), "http.response.duration")
  assert_eq(Instrument::name(gauge_instrument), "http.active.connections")
}

test "dashboard_log_aggregation" {
  // 测试仪表板日志聚合
  let clock = Clock::system()
  let base_timestamp = Clock::now_unix_nanos(clock)
  
  // 创建不同级别的日志记录
  let trace_logs = [
    LogRecord::new_with_context(Trace, Some("Trace message 1"), None, Some(base_timestamp), None, None, None, None),
    LogRecord::new_with_context(Trace, Some("Trace message 2"), None, Some(base_timestamp + 1000000L), None, None, None, None)
  ]
  
  let debug_logs = [
    LogRecord::new_with_context(Debug, Some("Debug message 1"), None, Some(base_timestamp + 2000000L), None, None, None, None),
    LogRecord::new_with_context(Debug, Some("Debug message 2"), None, Some(base_timestamp + 3000000L), None, None, None, None),
    LogRecord::new_with_context(Debug, Some("Debug message 3"), None, Some(base_timestamp + 4000000L), None, None, None, None)
  ]
  
  let info_logs = [
    LogRecord::new_with_context(Info, Some("Info message 1"), None, Some(base_timestamp + 5000000L), None, None, None, None),
    LogRecord::new_with_context(Info, Some("Info message 2"), None, Some(base_timestamp + 6000000L), None, None, None, None),
    LogRecord::new_with_context(Info, Some("Info message 3"), None, Some(base_timestamp + 7000000L), None, None, None, None),
    LogRecord::new_with_context(Info, Some("Info message 4"), None, Some(base_timestamp + 8000000L), None, None, None, None)
  ]
  
  let warn_logs = [
    LogRecord::new_with_context(Warn, Some("Warning message 1"), None, Some(base_timestamp + 9000000L), None, None, None, None),
    LogRecord::new_with_context(Warn, Some("Warning message 2"), None, Some(base_timestamp + 10000000L), None, None, None, None)
  ]
  
  let error_logs = [
    LogRecord::new_with_context(Error, Some("Error message 1"), None, Some(base_timestamp + 11000000L), None, None, None, None)
  ]
  
  // 验证日志数量
  assert_eq(trace_logs.length(), 2)
  assert_eq(debug_logs.length(), 3)
  assert_eq(info_logs.length(), 4)
  assert_eq(warn_logs.length(), 2)
  assert_eq(error_logs.length(), 1)
  
  // 验证日志级别
  for log in trace_logs {
    assert_eq(LogRecord::severity_number(log), Trace)
  }
  
  for log in debug_logs {
    assert_eq(LogRecord::severity_number(log), Debug)
  }
  
  for log in info_logs {
    assert_eq(LogRecord::severity_number(log), Info)
  }
  
  for log in warn_logs {
    assert_eq(LogRecord::severity_number(log), Warn)
  }
  
  for log in error_logs {
    assert_eq(LogRecord::severity_number(log), Error)
  }
  
  // 验证时间顺序
  for i in 0..trace_logs.length() - 1 {
    assert_true(trace_logs[i].timestamp < trace_logs[i + 1].timestamp)
  }
}

test "dashboard_trace_aggregation" {
  // 测试仪表板追踪聚合
  let trace_id_base = "dashboard_trace"
  
  // 创建多个追踪
  let traces = [
    ("dashboard_trace_001", "service_A", "operation_A", 1000L),
    ("dashboard_trace_002", "service_B", "operation_B", 1500L),
    ("dashboard_trace_003", "service_C", "operation_C", 2000L),
    ("dashboard_trace_004", "service_A", "operation_D", 1200L),
    ("dashboard_trace_005", "service_B", "operation_E", 1800L)
  ]
  
  let spans = []
  
  for (trace_id, service_name, operation_name, duration) in traces {
    let span_ctx = SpanContext::new(trace_id, "span_" + trace_id, true, "")
    let span = Span::new(operation_name, Internal, span_ctx)
    
    // 验证span属性
    assert_eq(SpanContext::trace_id(span_ctx), trace_id)
    assert_eq(Span::name(span), operation_name)
    assert_true(SpanContext::is_valid(span_ctx))
    assert_true(SpanContext::is_sampled(span_ctx))
    
    spans.push(span)
  }
  
  // 验证span数量
  assert_eq(spans.length(), 5)
  
  // 验证不同服务的span分布
  let service_a_count = 0
  let service_b_count = 0
  let service_c_count = 0
  
  for span in spans {
    let span_name = Span::name(span)
    if span_name.contains("operation_A") || span_name.contains("operation_D") {
      service_a_count = service_a_count + 1
    } else if span_name.contains("operation_B") || span_name.contains("operation_E") {
      service_b_count = service_b_count + 1
    } else if span_name.contains("operation_C") {
      service_c_count = service_c_count + 1
    }
  }
  
  assert_eq(service_a_count, 2)
  assert_eq(service_b_count, 2)
  assert_eq(service_c_count, 1)
}

test "dashboard_real_time_streaming" {
  // 测试仪表板实时数据流
  let clock = Clock::system()
  let start_time = Clock::now_unix_nanos(clock)
  
  // 模拟实时数据流
  let stream_data = []
  
  for i in 0..10 {
    let timestamp = start_time + (i * 1000000L).to_int64()  // 每秒一个数据点
    
    // 创建实时指标
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, "realtime-meter")
    let realtime_counter = Meter::create_counter(meter, "realtime.requests")
    
    // 记录实时数据
    Counter::add(realtime_counter, (i + 1).to_double())
    
    // 创建实时日志
    let log = LogRecord::new_with_context(
      Info,
      Some("Realtime data point " + i.to_string()),
      Some(Attributes::{ values: [
        ("data.point", IntValue(i)),
        ("timestamp", IntValue(timestamp.to_int()))
      ]}),
      Some(timestamp),
      None,
      Some("realtime_trace_" + i.to_string()),
      Some("realtime_span_" + i.to_string()),
      None
    )
    
    stream_data.push(log)
  }
  
  // 验证实时数据流
  assert_eq(stream_data.length(), 10)
  
  for i in 0..stream_data.length() {
    let log = stream_data[i]
    let expected_timestamp = start_time + (i * 1000000L).to_int64()
    
    assert_eq(log.timestamp, Some(expected_timestamp))
    assert_eq(LogRecord::body(log), Some("Realtime data point " + i.to_string()))
    
    match log.attributes {
      Some(attrs) => {
        let data_point = Attributes::get(attrs, "data.point")
        match data_point {
          Some(IntValue(value)) => assert_eq(value, i)
          _ => assert_false(true, "Expected IntValue for data.point")
        }
      }
      None => assert_false(true, "Expected attributes in realtime log")
    }
  }
  
  // 验证时间顺序
  for i in 0..stream_data.length() - 1 {
    assert_true(stream_data[i].timestamp < stream_data[i + 1].timestamp)
  }
}

test "dashboard_error_rate_calculation" {
  // 测试仪表板错误率计算
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "error-rate-meter")
  
  let total_requests = Meter::create_counter(meter, "total.requests")
  let error_requests = Meter::create_counter(meter, "error.requests")
  
  // 模拟请求数据
  let total_request_counts = [100, 200, 150, 300, 250]
  let error_request_counts = [5, 15, 10, 25, 20]
  
  let total_requests_sum = 0
  let error_requests_sum = 0
  
  for i in 0..total_request_counts.length() {
    Counter::add(total_requests, total_request_counts[i].to_double())
    Counter::add(error_requests, error_request_counts[i].to_double())
    
    total_requests_sum = total_requests_sum + total_request_counts[i]
    error_requests_sum = error_requests_sum + error_request_counts[i]
  }
  
  // 计算错误率
  let error_rate = (error_requests_sum.to_double() / total_requests_sum.to_double()) * 100.0
  
  // 验证计算结果
  assert_eq(total_requests_sum, 1000)
  assert_eq(error_requests_sum, 75)
  assert_true(error_rate > 7.0 && error_rate < 8.0)  // 应该是7.5%
  
  // 验证指标名称
  assert_eq(total_requests.name, "total.requests")
  assert_eq(error_requests.name, "error.requests")
}

test "dashboard_performance_metrics" {
  // 测试仪表板性能指标
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "performance-meter")
  
  // 创建性能相关的指标
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage")
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage")
  let disk_io = Meter::create_counter(meter, "system.disk.io")
  let network_io = Meter::create_counter(meter, "system.network.io")
  let response_time = Meter::create_histogram(meter, "system.response.time")
  
  // 模拟性能数据
  let cpu_values = [45.5, 67.2, 78.9, 56.3, 89.1]
  let memory_values = [1024.0, 1536.0, 2048.0, 1280.0, 2560.0]
  let disk_io_values = [100.0, 150.0, 200.0, 120.0, 180.0]
  let network_io_values = [500.0, 750.0, 1000.0, 600.0, 900.0]
  let response_time_values = [120.0, 150.0, 180.0, 90.0, 210.0]
  
  // 记录性能数据
  for i in 0..cpu_values.length() {
    // Gauge指标（当前值）
    // 注意：在实际实现中，Gauge会有set方法，这里简化处理
    
    // Counter指标（累计值）
    Counter::add(disk_io, disk_io_values[i])
    Counter::add(network_io, network_io_values[i])
    
    // Histogram指标（分布）
    Histogram::record(response_time, response_time_values[i])
  }
  
  // 验证指标名称
  assert_eq(cpu_usage.name, "system.cpu.usage")
  assert_eq(memory_usage.name, "system.memory.usage")
  assert_eq(disk_io.name, "system.disk.io")
  assert_eq(network_io.name, "system.network.io")
  assert_eq(response_time.name, "system.response.time")
  
  // 计算平均值
  let avg_cpu = cpu_values.reduce(0.0, fn(acc, x) { acc + x }) / cpu_values.length().to_double()
  let avg_memory = memory_values.reduce(0.0, fn(acc, x) { acc + x }) / memory_values.length().to_double()
  let total_disk_io = disk_io_values.reduce(0.0, fn(acc, x) { acc + x })
  let total_network_io = network_io_values.reduce(0.0, fn(acc, x) { acc + x })
  let avg_response_time = response_time_values.reduce(0.0, fn(acc, x) { acc + x }) / response_time_values.length().to_double()
  
  // 验证计算结果
  assert_true(avg_cpu > 60.0 && avg_cpu < 80.0)
  assert_true(avg_memory > 1500.0 && avg_memory < 2000.0)
  assert_eq(total_disk_io, 750.0)
  assert_eq(total_network_io, 3750.0)
  assert_true(avg_response_time > 140.0 && avg_response_time < 160.0)
}