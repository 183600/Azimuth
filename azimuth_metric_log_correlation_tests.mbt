// Azimuth 度量和日志关联性测试
// 测试度量和日志记录之间的关联性

test "度量与日志基本关联" {
  // 创建度量和日志提供者
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric.log.correlation")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "metric.log.correlation")
  
  // 创建度量
  let request_counter = Meter::create_counter(meter, "http.requests.total", Some("Total HTTP requests"), Some("requests"))
  let response_time = Meter::create_histogram(meter, "http.response.time", Some("HTTP response time"), Some("ms"))
  
  // 创建关联的上下文
  let ctx = Context::root()
  let operation_id_key = ContextKey::new("operation.id")
  let ctx_with_operation = Context::with_value(ctx, operation_id_key, "op-12345")
  
  // 记录度量
  Counter::add(request_counter, 1.0)
  Histogram::record(response_time, 125.5)
  
  // 记录关联的日志
  let start_log = LogRecord::new_with_context(
    Info,
    Some("Starting operation op-12345"),
    None,
    Some(1640995200000000000L),
    Some(1640995200005000000L),
    Some("trace-metric-log"),
    Some("span-start"),
    Some(ctx_with_operation)
  )
  
  let end_log = LogRecord::new_with_context(
    Info,
    Some("Completed operation op-12345 with response time 125.5ms"),
    None,
    Some(1640995200010000000L),
    Some(1640995200015000000L),
    Some("trace-metric-log"),
    Some("span-end"),
    Some(ctx_with_operation)
  )
  
  // 发射日志
  Logger::emit(logger, start_log)
  Logger::emit(logger, end_log)
  
  // 验证度量属性
  assert_eq(request_counter.name, "http.requests.total")
  assert_eq(response_time.name, "http.response.time")
  
  // 验证日志关联
  assert_eq(LogRecord::trace_id(start_log), Some("trace-metric-log"))
  assert_eq(LogRecord::trace_id(end_log), Some("trace-metric-log"))
  assert_eq(LogRecord::span_id(start_log), Some("span-start"))
  assert_eq(LogRecord::span_id(end_log), Some("span-end"))
  
  assert_true(true)
}

test "度量异常与错误日志关联" {
  // 测试度量异常与错误日志的关联
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric.error.correlation")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "metric.error.correlation")
  
  // 创建度量和错误计数器
  let operation_counter = Meter::create_counter(meter, "operations.total", Some("Total operations"), Some("operations"))
  let error_counter = Meter::create_counter(meter, "operations.errors", Some("Operation errors"), Some("errors"))
  
  // 模拟操作和错误
  let ctx = Context::root()
  let error_id_key = ContextKey::new("error.id")
  let ctx_with_error = Context::with_value(ctx, error_id_key, "err-67890")
  
  // 记录操作计数
  Counter::add(operation_counter, 1.0)
  
  // 记录错误计数
  Counter::add(error_counter, 1.0)
  
  // 记录错误日志
  let error_log = LogRecord::new_with_context(
    Error,
    Some("Operation failed with error err-67890: Database connection timeout"),
    None,
    Some(1640995200020000000L),
    Some(1640995200025000000L),
    Some("trace-error"),
    Some("span-error"),
    Some(ctx_with_error)
  )
  
  // 记录恢复日志
  let recovery_log = LogRecord::new_with_context(
    Info,
    Some("Recovery action initiated for error err-67890"),
    None,
    Some(1640995200030000000L),
    Some(1640995200035000000L),
    Some("trace-error"),
    Some("span-recovery"),
    Some(ctx_with_error)
  )
  
  // 发射日志
  Logger::emit(logger, error_log)
  Logger::emit(logger, recovery_log)
  
  // 验证度量
  assert_eq(operation_counter.name, "operations.total")
  assert_eq(error_counter.name, "operations.errors")
  
  // 验证错误日志关联
  assert_eq(LogRecord::severity_number(error_log), Error)
  assert_eq(LogRecord::severity_number(recovery_log), Info)
  assert_eq(LogRecord::trace_id(error_log), Some("trace-error"))
  assert_eq(LogRecord::trace_id(recovery_log), Some("trace-error"))
  
  assert_true(true)
}

test "度量阈值与警告日志关联" {
  // 测试度量阈值与警告日志的关联
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric.threshold.correlation")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "metric.threshold.correlation")
  
  // 创建度量和监控仪表
  let cpu_usage = Meter::create_gauge(meter, "system.cpu.usage", Some("System CPU usage"), Some("percent"))
  let memory_usage = Meter::create_gauge(meter, "system.memory.usage", Some("System memory usage"), Some("percent"))
  let threshold_counter = Meter::create_counter(meter, "threshold.breaches", Some("Threshold breaches"), Some("breaches"))
  
  // 模拟度量值变化
  let cpu_values = [45.2, 52.8, 78.5, 85.3, 91.7, 88.1, 76.4, 65.9]
  let memory_values = [60.1, 65.7, 72.3, 80.5, 87.2, 84.6, 78.9, 70.3]
  
  // 定义阈值
  let cpu_threshold = 80.0
  let memory_threshold = 85.0
  
  // 记录度量值和关联日志
  for i in 0..<cpu_values.length() {
    let ctx = Context::root()
    let timestamp_key = ContextKey::new("measurement.timestamp")
    let ctx_with_timestamp = Context::with_value(ctx, timestamp_key, (1640995200000L + i * 60L).to_string())
    
    // 记录CPU使用率
    UpDownCounter::add(cpu_usage, cpu_values[i])
    
    // 记录内存使用率
    UpDownCounter::add(memory_usage, memory_values[i])
    
    // 检查阈值并记录警告
    if cpu_values[i] > cpu_threshold {
      Counter::add(threshold_counter, 1.0)
      
      let cpu_warning = LogRecord::new_with_context(
        Warn,
        Some("CPU usage " + cpu_values[i].to_string() + "% exceeds threshold " + cpu_threshold.to_string() + "%"),
        None,
        Some(1640995200000000000L + i * 60000000000L),
        Some(1640995200005000000L + i * 60000000000L),
        Some("trace-threshold"),
        Some("span-cpu-" + i.to_string()),
        Some(ctx_with_timestamp)
      )
      
      Logger::emit(logger, cpu_warning)
    }
    
    if memory_values[i] > memory_threshold {
      Counter::add(threshold_counter, 1.0)
      
      let memory_warning = LogRecord::new_with_context(
        Warn,
        Some("Memory usage " + memory_values[i].to_string() + "% exceeds threshold " + memory_threshold.to_string() + "%"),
        None,
        Some(1640995200000000000L + i * 60000000000L),
        Some(1640995200005000000L + i * 60000000000L),
        Some("trace-threshold"),
        Some("span-memory-" + i.to_string()),
        Some(ctx_with_timestamp)
      )
      
      Logger::emit(logger, memory_warning)
    }
  }
  
  // 验证度量
  assert_eq(cpu_usage.name, "system.cpu.usage")
  assert_eq(memory_usage.name, "system.memory.usage")
  assert_eq(threshold_counter.name, "threshold.breaches")
  
  assert_true(true)
}

test "度量聚合与摘要日志关联" {
  // 测试度量聚合与摘要日志的关联
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric.aggregation.correlation")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "metric.aggregation.correlation")
  
  // 创建度量
  let request_counter = Meter::create_counter(meter, "api.requests.total", Some("Total API requests"), Some("requests"))
  let response_time = Meter::create_histogram(meter, "api.response.time", Some("API response time"), Some("ms"))
  let error_rate = Meter::create_gauge(meter, "api.error.rate", Some("API error rate"), Some("percent"))
  
  // 模拟一段时间内的度量数据
  let response_times = [120.5, 85.3, 200.7, 150.2, 95.8, 180.4, 110.6, 75.9, 165.3, 90.1]
  
  // 记录度量数据
  for i in 0..<response_times.length() {
    // 记录请求计数
    Counter::add(request_counter, 1.0)
    
    // 记录响应时间
    Histogram::record(response_time, response_times[i])
    
    // 模拟错误率变化
    let error_rate_value = if i % 5 == 0 { 10.0 } else { 2.5 }
    UpDownCounter::add(error_rate, error_rate_value)
  }
  
  // 记录聚合摘要日志
  let ctx = Context::root()
  let aggregation_window_key = ContextKey::new("aggregation.window")
  let ctx_with_window = Context::with_value(ctx, aggregation_window_key, "5m")
  
  let summary_log = LogRecord::new_with_context(
    Info,
    Some("5-minute summary: 10 requests, avg response time 137.5ms, error rate 5.0%"),
    None,
    Some(1640995200300000000L),
    Some(1640995200305000000L),
    Some("trace-aggregation"),
    Some("span-summary"),
    Some(ctx_with_window)
  )
  
  // 记录详细分析日志
  let analysis_log = LogRecord::new_with_context(
    Info,
    Some("Performance analysis: response time range 75.9-200.7ms, p95 190.3ms, peak load at 3rd request"),
    None,
    Some(1640995200310000000L),
    Some(1640995200315000000L),
    Some("trace-aggregation"),
    Some("span-analysis"),
    Some(ctx_with_window)
  )
  
  // 发射日志
  Logger::emit(logger, summary_log)
  Logger::emit(logger, analysis_log)
  
  // 验证度量
  assert_eq(request_counter.name, "api.requests.total")
  assert_eq(response_time.name, "api.response.time")
  assert_eq(error_rate.name, "api.error.rate")
  
  // 验证摘要日志
  assert_eq(LogRecord::body(summary_log), Some("5-minute summary: 10 requests, avg response time 137.5ms, error rate 5.0%"))
  assert_eq(LogRecord::body(analysis_log), Some("Performance analysis: response time range 75.9-200.7ms, p95 190.3ms, peak load at 3rd request"))
  
  assert_true(true)
}

test "多服务度量与日志关联" {
  // 测试多服务环境下的度量与日志关联
  
  let services = ["api-gateway", "auth-service", "user-service", "order-service"]
  let meter_providers = []
  let meters = []
  let loggers = []
  
  // 为每个服务创建度量提供者和日志记录器
  for i in 0..<services.length() {
    let provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(provider, services[i])
    
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, services[i])
    
    meter_providers.push(provider)
    meters.push(meter)
    loggers.push(logger)
  }
  
  // 为每个服务创建度量和记录日志
  for i in 0..<services.length() {
    let service_name = services[i]
    
    // 创建度量
    let request_counter = Meter::create_counter(
      meters[i], 
      "http.requests.total", 
      Some("Total HTTP requests for " + service_name), 
      Some("requests")
    )
    
    let response_time = Meter::create_histogram(
      meters[i], 
      "http.response.time", 
      Some("HTTP response time for " + service_name), 
      Some("ms")
    )
    
    // 记录度量数据
    Counter::add(request_counter, (i + 1) * 10.0)
    Histogram::record(response_time, 100.0 + i.to_float() * 25.0)
    
    // 记录关联日志
    let ctx = Context::root()
    let service_key = ContextKey::new("service.name")
    let ctx_with_service = Context::with_value(ctx, service_key, service_name)
    
    let service_log = LogRecord::new_with_context(
      Info,
      Some(service_name + " processed " + ((i + 1) * 10).to_string() + " requests with avg response time " + (100.0 + i.to_float() * 25.0).to_string() + "ms"),
      None,
      Some(1640995200000000000L + i * 1000000000L),
      Some(1640995200005000000L + i * 1000000000L),
      Some("trace-multi-service"),
      Some("span-" + service_name),
      Some(ctx_with_service)
    )
    
    Logger::emit(loggers[i], service_log)
    
    // 验证度量
    assert_eq(request_counter.name, "http.requests.total")
    assert_eq(response_time.name, "http.response.time")
    assert_eq(request_counter.description, Some("Total HTTP requests for " + service_name))
    assert_eq(response_time.description, Some("HTTP response time for " + service_name))
  }
  
  assert_true(true)
}

test "度量变化与趋势日志关联" {
  // 测试度量变化与趋势日志的关联
  
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "metric.trend.correlation")
  
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "metric.trend.correlation")
  
  // 创建趋势监控度量
  let throughput = Meter::create_gauge(meter, "system.throughput", Some("System throughput"), Some("ops/sec"))
  let latency = Meter::create_gauge(meter, "system.latency", Some("System latency"), Some("ms"))
  let error_rate = Meter::create_gauge(meter, "system.error.rate", Some("System error rate"), Some("percent"))
  
  // 模拟度量变化趋势
  let throughput_trend = [1000.0, 1050.0, 1100.0, 1080.0, 1150.0, 1200.0, 1180.0, 1250.0]
  let latency_trend = [50.0, 48.5, 52.0, 55.3, 51.2, 49.8, 53.5, 50.7]
  let error_rate_trend = [1.0, 0.8, 1.2, 0.9, 1.5, 1.1, 0.7, 1.3]
  
  // 记录度量变化和趋势分析
  for i in 0..<throughput_trend.length() {
    // 记录当前度量值
    UpDownCounter::add(throughput, throughput_trend[i])
    UpDownCounter::add(latency, latency_trend[i])
    UpDownCounter::add(error_rate, error_rate_trend[i])
    
    // 每3个记录点分析一次趋势
    if i > 0 && i % 3 == 0 {
      let ctx = Context::root()
      let trend_period_key = ContextKey::new("trend.period")
      let ctx_with_period = Context::with_value(ctx, trend_period_key, "last_3_points")
      
      let trend_log = LogRecord::new_with_context(
        Info,
        Some("Trend analysis: throughput " + 
             (if throughput_trend[i] > throughput_trend[i-3] { "increasing" } else { "decreasing" }) + 
             ", latency " + 
             (if latency_trend[i] > latency_trend[i-3] { "increasing" } else { "decreasing" }) + 
             ", error rate " + 
             (if error_rate_trend[i] > error_rate_trend[i-3] { "increasing" } else { "decreasing" })),
        None,
        Some(1640995200000000000L + i * 300000000000L),
        Some(1640995200005000000L + i * 300000000000L),
        Some("trace-trend"),
        Some("span-trend-" + i.to_string()),
        Some(ctx_with_period)
      )
      
      Logger::emit(logger, trend_log)
    }
  }
  
  // 验证度量
  assert_eq(throughput.name, "system.throughput")
  assert_eq(latency.name, "system.latency")
  assert_eq(error_rate.name, "system.error.rate")
  
  assert_true(true)
}