// Azimuth 遥测核心功能测试用例
// 专注于遥测系统的核心功能和关键特性

// 测试1: 遥测上下文传播
test "遥测上下文传播机制" {
  // 创建根上下文
  let root_context = TraceContext::new(
    "trace-12345", 
    "span-67890", 
    true, 
    "baggage-key=value,debug=true"
  )
  
  // 测试上下文序列化
  let serialized = TraceContext::serialize(root_context)
  assert_true(serialized.contains("trace-12345"))
  assert_true(serialized.contains("span-67890"))
  assert_true(serialized.contains("baggage-key=value"))
  
  // 测试上下文反序列化
  let deserialized = TraceContext::deserialize(serialized)
  assert_eq(deserialized.trace_id, "trace-12345")
  assert_eq(deserialized.span_id, "span-67890")
  assert_true(deserialized.sampled)
  assert_true(deserialized.baggage.contains("baggage-key=value"))
  
  // 测试子上下文创建
  let child_context = TraceContext::create_child(root_context, "child-span-11111")
  assert_eq(child_context.trace_id, "trace-12345")  // 继承trace_id
  assert_eq(child_context.span_id, "child-span-11111")  // 新的span_id
  assert_eq(child_context.parent_span_id, Some("span-67890"))  // 设置父span
  assert_eq(child_context.sampled, root_context.sampled)  // 继承采样标志
  assert_eq(child_context.baggage, root_context.baggage)  // 继承baggage
}

// 测试2: Span生命周期管理
test "Span生命周期管理" {
  // 创建Span
  let span = Span::builder()
    .name("database.query")
    .kind(SpanKind::Client)
    .start_time(1640995200000)
    .build()
  
  // 验证初始状态
  assert_eq(span.name, "database.query")
  assert_eq(span.kind, SpanKind::Client)
  assert_eq(span.status, SpanStatus::Unset)
  assert_true(span.start_time > 0)
  assert_eq(span.end_time, 0)  // 未结束
  
  // 添加属性
  Span::add_attribute(span, "db.system", "postgresql")
  Span::add_attribute(span, "db.statement", "SELECT * FROM users")
  Span::add_attribute(span, "db.operation", "select")
  
  // 添加事件
  Span::add_event(span, "query.start", 1640995200100, [("query", "SELECT * FROM users")])
  Span::add_event(span, "query.execute", 1640995200200, [])
  
  // 结束Span
  Span::end(span, 1640995250000, SpanStatus::Ok)
  
  // 验证结束状态
  assert_eq(span.status, SpanStatus::Ok)
  assert_eq(span.end_time, 1640995250000)
  assert_eq(span.duration(), 50000)  // 50秒
  
  // 验证属性
  assert_eq(Span::get_attribute(span, "db.system"), Some("postgresql"))
  assert_eq(Span::get_attribute(span, "db.operation"), Some("select"))
  assert_eq(Span::get_attribute(span, "nonexistent"), None)
  
  // 验证事件
  let events = Span::get_events(span)
  assert_eq(events.length(), 2)
  assert_eq(events[0].name, "query.start")
  assert_eq(events[1].name, "query.execute")
}

// 测试3: 指标数据收集
test "指标数据收集和聚合" {
  // 创建指标收集器
  let metric_collector = MetricCollector::new()
  
  // 创建计数器指标
  let request_counter = Metric::counter("http.requests.total", [
    ("service", "api-gateway"),
    ("method", "GET")
  ])
  
  // 记录指标值
  MetricCollector::record(metric_collector, request_counter, 1)
  MetricCollector::record(metric_collector, request_counter, 1)
  MetricCollector::record(metric_collector, request_counter, 1)
  
  // 创建直方图指标
  let response_time = Metric::histogram("http.response.duration", [
    ("service", "api-gateway"),
    ("status", "200")
  ])
  
  // 记录响应时间
  MetricCollector::record(metric_collector, response_time, 120.5)
  MetricCollector::record(metric_collector, response_time, 85.3)
  MetricCollector::record(metric_collector, response_time, 200.1)
  MetricCollector::record(metric_collector, response_time, 95.7)
  
  // 创建仪表指标
  let active_connections = Metric::gauge("db.connections.active", [
    ("service", "api-gateway"),
    ("database", "primary")
  ])
  
  // 设置仪表值
  MetricCollector::set(metric_collector, active_connections, 25)
  MetricCollector::set(metric_collector, active_connections, 32)
  MetricCollector::set(metric_collector, active_connections, 28)
  
  // 获取指标数据
  let counter_data = MetricCollector::get_metric(metric_collector, "http.requests.total")
  match counter_data {
    Some(MetricData::Counter(value)) => assert_eq(value, 3)
    _ => assert_true(false)
  }
  
  let histogram_data = MetricCollector::get_metric(metric_collector, "http.response.duration")
  match histogram_data {
    Some(MetricData::Histogram(stats)) => {
      assert_eq(stats.count, 4)
      assert_eq(stats.sum, 501.6)
      assert_true(stats.min <= 85.3)
      assert_true(stats.max >= 200.1)
    }
    _ => assert_true(false)
  }
  
  let gauge_data = MetricCollector::get_metric(metric_collector, "db.connections.active")
  match gauge_data {
    Some(MetricData::Gauge(value)) => assert_eq(value, 28)
    _ => assert_true(false)
  }
}

// 测试4: 日志关联和相关性
test "日志关联和相关性分析" {
  // 创建日志记录器
  let logger = Logger::new("service.api")
  
  // 创建Span上下文
  let span_context = SpanContext::new(
    "trace-abc123", 
    "span-def456", 
    true, 
    ""
  )
  
  // 关联日志与Span
  Logger::with_span_context(logger, span_context, fn() {
    Logger::info("Processing request started")
    Logger::debug("Validating input parameters")
    Logger::warn("Rate limit approaching threshold")
    Logger::error("Database connection failed", [("error.code", "CONN_TIMEOUT")])
  })
  
  // 获取关联的日志
  let logs = Logger::get_logs_by_trace(logger, "trace-abc123")
  assert_eq(logs.length(), 4)
  
  // 验证日志关联
  for log in logs {
    assert_eq(log.trace_id, "trace-abc123")
    assert_eq(log.span_id, "span-def456")
    assert_eq(log.service, "service.api")
  }
  
  // 验证日志内容
  assert_eq(logs[0].level, LogLevel::Info)
  assert_eq(logs[0].message, "Processing request started")
  
  assert_eq(logs[1].level, LogLevel::Debug)
  assert_eq(logs[1].message, "Validating input parameters")
  
  assert_eq(logs[2].level, LogLevel::Warn)
  assert_eq(logs[2].message, "Rate limit approaching threshold")
  
  assert_eq(logs[3].level, LogLevel::Error)
  assert_eq(logs[3].message, "Database connection failed")
  assert_eq(logs[3].attributes.get("error.code"), Some("CONN_TIMEOUT"))
  
  // 测试跨服务日志关联
  let downstream_logger = Logger::new("service.database")
  let downstream_context = SpanContext::new(
    "trace-abc123",  // 相同trace_id
    "span-ghi789",   // 不同span_id
    true, 
    ""
  )
  
  Logger::with_span_context(downstream_logger, downstream_context, fn() {
    Logger::info("Executing query: SELECT * FROM users")
    Logger::error("Query execution timeout", [("timeout.ms", "5000")])
  })
  
  // 获取完整trace的所有日志
  let all_trace_logs = Logger::get_logs_by_trace(logger, "trace-abc123") + 
                       Logger::get_logs_by_trace(downstream_logger, "trace-abc123")
  
  assert_eq(all_trace_logs.length(), 6)
  
  // 按时间排序验证执行顺序
  let sorted_logs = all_trace_logs.sort_by(fn(a, b) { a.timestamp - b.timestamp })
  assert_true(sorted_logs[0].timestamp <= sorted_logs[1].timestamp)
  assert_true(sorted_logs[1].timestamp <= sorted_logs[2].timestamp)
}

// 测试5: 遥测数据导出
test "遥测数据导出格式" {
  // 创建导出器
  let exporter = Exporter::new(ExportFormat::OTLP)
  
  // 创建测试数据
  let span = Span::builder()
    .name("http.request")
    .trace_id("trace-123")
    .span_id("span-456")
    .parent_span_id(Some("span-789"))
    .kind(SpanKind::Server)
    .status(SpanStatus::Ok)
    .start_time(1640995200000)
    .end_time(1640995250000)
    .add_attribute("http.method", "GET")
    .add_attribute("http.url", "/api/users")
    .add_attribute("http.status_code", "200")
    .add_event("request.start", 1640995200000, [])
    .add_event("request.end", 1640995250000, [("duration", "5000")])
    .build()
  
  // 导出Span数据
  let span_export = Exporter::export_span(exporter, span)
  
  // 验证导出格式
  assert_true(span_export.contains("trace-123"))
  assert_true(span_export.contains("span-456"))
  assert_true(span_export.contains("span-789"))
  assert_true(span_export.contains("http.request"))
  assert_true(span_export.contains("GET"))
  assert_true(span_export.contains("/api/users"))
  assert_true(span_export.contains("200"))
  
  // 创建指标数据
  let metric = Metric::histogram("http.request.duration", [
    ("method", "GET"),
    ("status", "200")
  ])
  metric.record(120.5)
  metric.record(85.3)
  metric.record(200.1)
  
  // 导出指标数据
  let metric_export = Exporter::export_metric(exporter, metric)
  
  // 验证指标导出格式
  assert_true(metric_export.contains("http.request.duration"))
  assert_true(metric_export.contains("GET"))
  assert_true(metric_export.contains("200"))
  assert_true(metric_export.contains("120.5"))
  assert_true(metric_export.contains("85.3"))
  assert_true(metric_export.contains("200.1"))
  
  // 创建日志数据
  let log = LogEntry::new(
    1640995200000,
    LogLevel::Info,
    "service.api",
    "Request processed successfully",
    [("trace.id", "trace-123"), ("span.id", "span-456")]
  )
  
  // 导出日志数据
  let log_export = Exporter::export_log(exporter, log)
  
  // 验证日志导出格式
  assert_true(log_export.contains("Request processed successfully"))
  assert_true(log_export.contains("service.api"))
  assert_true(log_export.contains("trace-123"))
  assert_true(log_export.contains("span-456"))
  assert_true(log_export.contains("INFO"))
}

// 测试6: 资源属性管理
test "资源属性管理和传播" {
  // 创建资源
  let resource = Resource::builder()
    .add_attribute("service.name", "payment-service")
    .add_attribute("service.version", "1.2.3")
    .add_attribute("service.instance.id", "instance-12345")
    .add_attribute("deployment.environment", "production")
    .add_attribute("host.name", "web-server-01")
    .add_attribute("os.type", "linux")
    .add_attribute("process.pid", "1234")
    .build()
  
  // 验证资源属性
  assert_eq(Resource::get_attribute(resource, "service.name"), Some("payment-service"))
  assert_eq(Resource::get_attribute(resource, "service.version"), Some("1.2.3"))
  assert_eq(Resource::get_attribute(resource, "deployment.environment"), Some("production"))
  assert_eq(Resource::get_attribute(resource, "nonexistent"), None)
  
  // 创建Span并关联资源
  let span = Span::builder()
    .name("payment.process")
    .resource(resource)
    .build()
  
  // 验证Span继承资源属性
  assert_eq(Span::get_resource_attribute(span, "service.name"), Some("payment-service"))
  assert_eq(Span::get_resource_attribute(span, "service.version"), Some("1.2.3"))
  
  // 创建指标并关联资源
  let metric = Metric::counter("payment.transactions.total", [])
    .resource(resource)
  
  // 验证指标继承资源属性
  assert_eq(Metric::get_resource_attribute(metric, "service.name"), Some("payment-service"))
  assert_eq(Metric::get_resource_attribute(metric, "deployment.environment"), Some("production"))
  
  // 测试资源合并
  let additional_resource = Resource::builder()
    .add_attribute("service.name", "payment-service")  // 重复属性
    .add_attribute("service.namespace", "finance")     // 新属性
    .add_attribute("cloud.provider", "aws")           // 新属性
    .add_attribute("cloud.region", "us-west-2")       // 新属性
    .build()
  
  let merged_resource = Resource::merge(resource, additional_resource)
  
  // 验证合并结果
  assert_eq(Resource::get_attribute(merged_resource, "service.name"), Some("payment-service"))
  assert_eq(Resource::get_attribute(merged_resource, "service.version"), Some("1.2.3"))
  assert_eq(Resource::get_attribute(merged_resource, "service.namespace"), Some("finance"))
  assert_eq(Resource::get_attribute(merged_resource, "cloud.provider"), Some("aws"))
  assert_eq(Resource::get_attribute(merged_resource, "cloud.region"), Some("us-west-2"))
  
  // 测试资源序列化
  let serialized = Resource::serialize(merged_resource)
  assert_true(serialized.contains("payment-service"))
  assert_true(serialized.contains("finance"))
  assert_true(serialized.contains("aws"))
  assert_true(serialized.contains("us-west-2"))
}

// 测试7: 异常处理和错误传播
test "异常处理和错误传播" {
  // 创建错误处理器
  let error_handler = ErrorHandler::new()
  
  // 定义错误类型
  enum TelemetryError {
    SpanCreationFailed(String)
    MetricRecordingFailed(String)
    LogCorruption(String)
    ExportTimeout(String)
  }
  
  // 测试Span创建错误处理
  let span_result = ErrorHandler::handle_span_creation(error_handler, fn() {
    // 模拟Span创建失败
    Err(TelemetryError::SpanCreationFailed("Invalid trace ID format"))
  })
  
  match span_result {
    Ok(_) => assert_true(false)  // 不应该成功
    Err(error) => {
      match error {
        TelemetryError::SpanCreationFailed(message) => {
          assert_eq(message, "Invalid trace ID format")
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 测试指标记录错误处理
  let metric_result = ErrorHandler::handle_metric_recording(error_handler, fn() {
    // 模拟指标记录失败
    Err(TelemetryError::MetricRecordingFailed("Histogram bucket overflow"))
  })
  
  match metric_result {
    Ok(_) => assert_true(false)  // 不应该成功
    Err(error) => {
      match error {
        TelemetryError::MetricRecordingFailed(message) => {
          assert_eq(message, "Histogram bucket overflow")
        }
        _ => assert_true(false)
      }
    }
  }
  
  // 测试错误恢复机制
  let recovered_span = ErrorHandler::recover_with_default_span(error_handler, "fallback.operation")
  assert_eq(recovered_span.name, "fallback.operation")
  assert_eq(recovered_span.status, SpanStatus::Error)
  assert_true(recovered_span.start_time > 0)
  
  // 测试错误聚合和统计
  ErrorHandler::record_error(error_handler, TelemetryError::SpanCreationFailed("Invalid trace ID"))
  ErrorHandler::record_error(error_handler, TelemetryError::SpanCreationFailed("Invalid span ID"))
  ErrorHandler::record_error(error_handler, TelemetryError::MetricRecordingFailed("Histogram overflow"))
  ErrorHandler::record_error(error_handler, TelemetryError::ExportTimeout("Network unreachable"))
  
  let error_stats = ErrorHandler::get_error_statistics(error_handler)
  assert_eq(error_stats.total_errors, 4)
  assert_eq(error_stats.error_counts.get("SpanCreationFailed"), Some(2))
  assert_eq(error_stats.error_counts.get("MetricRecordingFailed"), Some(1))
  assert_eq(error_stats.error_counts.get("ExportTimeout"), Some(1))
  
  // 测试错误阈值和熔断
  ErrorHandler::set_error_threshold(error_handler, "SpanCreationFailed", 2)
  
  let should_trip = ErrorHandler::should_trip_circuit_breaker(error_handler, "SpanCreationFailed")
  assert_true(should_trip)  // 超过阈值，应该触发熔断
  
  let should_not_trip = ErrorHandler::should_trip_circuit_breaker(error_handler, "MetricRecordingFailed")
  assert_false(should_not_trip)  // 未超过阈值，不应该触发熔断
}

// 测试8: 性能基准测试
test "遥测系统性能基准" {
  // 创建性能测试器
  let benchmark = Benchmark::new()
  
  // 测试Span创建性能
  let span_creation_time = Benchmark::measure(benchmark, fn() {
    for i in 0..=1000 {
      let span = Span::builder()
        .name("test.span." + i.to_string())
        .trace_id("trace-" + i.to_string())
        .span_id("span-" + i.to_string())
        .build()
      // 简单操作确保Span被创建
      assert_true(span.name.length() > 0)
    }
  })
  
  // 验证性能在合理范围内（假设1000个Span创建应在100ms内完成）
  assert_true(span_creation_time < 100)
  
  // 测试属性设置性能
  let span = Span::builder().name("performance.test").build()
  let attribute_time = Benchmark::measure(benchmark, fn() {
    for i in 0..=1000 {
      Span::add_attribute(span, "key." + i.to_string(), "value." + i.to_string())
    }
  })
  
  // 验证属性设置性能
  assert_true(attribute_time < 50)
  
  // 测试指标记录性能
  let metric_collector = MetricCollector::new()
  let counter = Metric::counter("performance.counter", [])
  
  let metric_time = Benchmark::measure(benchmark, fn() {
    for i in 0..=1000 {
      MetricCollector::record(metric_collector, counter, 1)
    }
  })
  
  // 验证指标记录性能
  assert_true(metric_time < 30)
  
  // 测试日志记录性能
  let logger = Logger::new("performance.test")
  
  let log_time = Benchmark::measure(benchmark, fn() {
    for i in 0..=1000 {
      Logger::info("Log message " + i.to_string())
    }
  })
  
  // 验证日志记录性能
  assert_true(log_time < 40)
  
  // 测试序列化性能
  let large_span = Span::builder()
    .name("large.span")
    .add_attribute("key1", "value1")
    .add_attribute("key2", "value2")
    .add_attribute("key3", "value3")
    .add_attribute("key4", "value4")
    .add_attribute("key5", "value5")
    .add_event("event1", 0, [])
    .add_event("event2", 1000, [])
    .add_event("event3", 2000, [])
    .build()
  
  let serialization_time = Benchmark::measure(benchmark, fn() {
    for i in 0..=100 {
      let serialized = Span::serialize(large_span)
      assert_true(serialized.length() > 0)
    }
  })
  
  // 验证序列化性能
  assert_true(serialization_time < 100)
  
  // 获取性能报告
  let performance_report = Benchmark::generate_report(benchmark)
  assert_true(performance_report.contains("span_creation"))
  assert_true(performance_report.contains("attribute"))
  assert_true(performance_report.contains("metric"))
  assert_true(performance_report.contains("log"))
  assert_true(performance_report.contains("serialization"))
}

// 测试9: 内存使用和资源管理
test "内存使用和资源管理" {
  // 创建资源监控器
  let resource_monitor = ResourceMonitor::new()
  
  // 记录初始内存使用
  let initial_memory = ResourceMonitor::get_memory_usage(resource_monitor)
  
  // 创建大量Span对象
  let spans = []
  for i in 0..=1000 {
    let span = Span::builder()
      .name("memory.test.span." + i.to_string())
      .trace_id("trace-" + i.to_string())
      .span_id("span-" + i.to_string())
      .add_attribute("index", i.to_string())
      .add_event("created", 0, [])
      .build()
    spans = spans.push(span)
  }
  
  // 记录创建Span后的内存使用
  let after_spans_memory = ResourceMonitor::get_memory_usage(resource_monitor)
  
  // 验证内存增长在合理范围内
  let memory_growth = after_spans_memory - initial_memory
  assert_true(memory_growth > 0)  // 内存应该增长
  assert_true(memory_growth < 50000)  // 但不应过度增长（50MB限制）
  
  // 创建大量指标对象
  let metrics = []
  for i in 0..=1000 {
    let metric = Metric::histogram("memory.test.metric." + i.to_string(), [])
      .add_attribute("index", i.to_string())
    metrics = metrics.push(metric)
  }
  
  // 记录创建指标后的内存使用
  let after_metrics_memory = ResourceMonitor::get_memory_usage(resource_monitor)
  
  // 验证指标内存使用
  let metrics_memory_growth = after_metrics_memory - after_spans_memory
  assert_true(metrics_memory_growth > 0)
  assert_true(metrics_memory_growth < 30000)  // 30MB限制
  
  // 测试内存释放
  let spans_before_gc = spans.length()
  spans = []  // 清理引用
  
  // 强制垃圾回收（如果支持）
  ResourceMonitor::force_gc(resource_monitor)
  
  // 记录GC后的内存使用
  let after_gc_memory = ResourceMonitor::get_memory_usage(resource_monitor)
  
  // 验证内存回收
  let memory_reclaimed = after_metrics_memory - after_gc_memory
  assert_true(memory_reclaimed > 0)  // 应该回收一些内存
  
  // 测试资源池
  let span_pool = SpanPool::new(100)  // 最大100个Span的池
  
  // 从池中获取Span
  let pooled_spans = []
  for i in 0..=50 {
    let span = SpanPool::acquire(span_pool, "pooled.span." + i.to_string())
    pooled_spans = pooled_spans.push(span)
  }
  
  // 验证池状态
  assert_eq(SpanPool::active_count(span_pool), 50)
  assert_eq(SpanPool::available_count(span_pool), 50)
  
  // 归还Span到池
  for span in pooled_spans {
    SpanPool::release(span_pool, span)
  }
  
  // 验证归还后的池状态
  assert_eq(SpanPool::active_count(span_pool), 0)
  assert_eq(SpanPool::available_count(span_pool), 100)
  
  // 测试池化性能
  let pooled_time = ResourceMonitor::measure_pool_performance(resource_monitor, span_pool, 1000)
  let non_pooled_time = ResourceMonitor::measure_non_pool_performance(resource_monitor, 1000)
  
  // 验证池化性能优势
  assert_true(pooled_time < non_pooled_time)
  
  // 获取资源使用报告
  let resource_report = ResourceMonitor::generate_report(resource_monitor)
  assert_true(resource_report.contains("memory"))
  assert_true(resource_report.contains("pool"))
  assert_true(resource_report.contains("gc"))
}

// 测试10: 并发安全和线程安全
test "并发安全和线程安全" {
  // 创建并发测试器
  let concurrent_tester = ConcurrentTester::new()
  
  // 创建共享的Span处理器
  let span_processor = SpanProcessor::new()
  
  // 测试并发Span创建
  let span_creation_results = ConcurrentTester::run_concurrent(concurrent_tester, 10, fn(thread_id) {
    let mut created_spans = 0
    for i in 0..=100 {
      let span = Span::builder()
        .name("concurrent.span." + thread_id.to_string() + "." + i.to_string())
        .trace_id("trace-" + thread_id.to_string())
        .span_id("span-" + thread_id.to_string() + "-" + i.to_string())
        .build()
      
      SpanProcessor::process(span_processor, span)
      created_spans = created_spans + 1
    }
    created_spans
  })
  
  // 验证所有线程都成功创建了Span
  let total_created = span_creation_results.reduce(fn(acc, count) { acc + count }, 0)
  assert_eq(total_created, 1000)  // 10个线程 × 100个Span
  
  // 验证Span处理器状态一致性
  let processed_spans = SpanProcessor::get_processed_count(span_processor)
  assert_eq(processed_spans, 1000)
  
  // 测试并发指标记录
  let metric_collector = MetricCollector::new()
  let counter = Metric::counter("concurrent.counter", [])
  
  let metric_recording_results = ConcurrentTester::run_concurrent(concurrent_tester, 10, fn(thread_id) {
    let mut recorded_metrics = 0
    for i in 0..=100 {
      MetricCollector::record(metric_collector, counter, 1)
      recorded_metrics = recorded_metrics + 1
    }
    recorded_metrics
  })
  
  // 验证所有指标都被正确记录
  let total_recorded = metric_recording_results.reduce(fn(acc, count) { acc + count }, 0)
  assert_eq(total_recorded, 1000)
  
  let counter_value = MetricCollector::get_metric_value(metric_collector, "concurrent.counter")
  assert_eq(counter_value, Some(1000))
  
  // 测试并发日志记录
  let logger = Logger::new("concurrent.test")
  
  let log_recording_results = ConcurrentTester::run_concurrent(concurrent_tester, 10, fn(thread_id) {
    let mut recorded_logs = 0
    for i in 0..=100 {
      Logger::info("Thread " + thread_id.to_string() + " message " + i.to_string())
      recorded_logs = recorded_logs + 1
    }
    recorded_logs
  })
  
  // 验证所有日志都被正确记录
  let total_logs = log_recording_results.reduce(fn(acc, count) { acc + count }, 0)
  assert_eq(total_logs, 1000)
  
  let log_count = Logger::get_log_count(logger)
  assert_eq(log_count, 1000)
  
  // 测试并发上下文传播
  let context_propagator = ContextPropagator::new()
  
  let context_propagation_results = ConcurrentTester::run_concurrent(concurrent_tester, 10, fn(thread_id) {
    let root_context = TraceContext::new(
      "trace-" + thread_id.to_string(),
      "root-" + thread_id.to_string(),
      true,
      ""
    )
    
    let mut propagated_contexts = 0
    for i in 0..=10 {
      let child_context = ContextPropagator::create_child(context_propagator, root_context, "child-" + i.to_string())
      
      // 验证上下文传播正确性
      assert_eq(child_context.trace_id, "trace-" + thread_id.to_string())
      assert_eq(child_context.parent_span_id, Some("root-" + thread_id.to_string()))
      
      propagated_contexts = propagated_contexts + 1
    }
    propagated_contexts
  })
  
  // 验证上下文传播
  let total_propagated = context_propagation_results.reduce(fn(acc, count) { acc + count }, 0)
  assert_eq(total_propagated, 110)  // 10个线程 × 11个上下文
  
  // 测试竞态条件检测
  let race_condition_detector = RaceConditionDetector::new()
  let shared_resource = SharedResource::new()
  
  ConcurrentTester::run_concurrent(concurrent_tester, 10, fn(thread_id) {
    for i in 0..=100 {
      RaceConditionDetector::access_start(race_condition_detector, "shared_resource", thread_id)
      SharedResource::update(shared_resource, thread_id, i)
      RaceConditionDetector::access_end(race_condition_detector, "shared_resource", thread_id)
    }
  })
  
  // 验证无竞态条件
  let race_conditions = RaceConditionDetector::detect_race_conditions(race_condition_detector)
  assert_eq(race_conditions.length(), 0)  // 应该没有竞态条件
  
  // 验证共享资源状态一致性
  let final_value = SharedResource::get_value(shared_resource)
  assert_true(final_value > 0)  // 应该有更新
}