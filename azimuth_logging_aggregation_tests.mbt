// Azimuth Logging and Aggregation Tests
// 日志记录和聚合测试用例 - 专注于日志记录、聚合分析和日志处理

// Test 1: 基础日志记录测试
test "basic logging functionality" {
  // 创建日志提供者
  let provider = LoggerProvider::new()
  
  // 创建日志记录器
  let logger = LoggerProvider::get_logger(provider, "test-logger")
  
  // 创建内存日志处理器
  let memory_handler = MemoryLogHandler::new(1000)
  Logger::add_handler(logger, memory_handler)
  
  // 测试不同级别的日志记录
  Logger::debug(logger, "Debug message", Some([("component", StringValue("test"))]))
  Logger::info(logger, "Info message", Some([("component", StringValue("test"))]))
  Logger::warn(logger, "Warning message", Some([("component", StringValue("test"))]))
  Logger::error(logger, "Error message", Some([("component", StringValue("test"))]))
  
  // 获取日志记录
  let log_records = MemoryLogHandler::get_records(memory_handler)
  
  // 验证日志记录
  assert_eq(log_records.length(), 4)
  
  match log_records[0] {
    LogRecord => {
      assert_eq(LogRecord::severity_number(log_records[0]), Debug)
      match LogRecord::body(log_records[0]) {
        Some(body) => assert_eq(body, "Debug message")
        None => assert_true(false)
      }
    }
  }
  
  match log_records[1] {
    LogRecord => {
      assert_eq(LogRecord::severity_number(log_records[1]), Info)
      match LogRecord::body(log_records[1]) {
        Some(body) => assert_eq(body, "Info message")
        None => assert_true(false)
      }
    }
  }
  
  match log_records[2] {
    LogRecord => {
      assert_eq(LogRecord::severity_number(log_records[2]), Warn)
      match LogRecord::body(log_records[2]) {
        Some(body) => assert_eq(body, "Warning message")
        None => assert_true(false)
      }
    }
  }
  
  match log_records[3] {
    LogRecord => {
      assert_eq(LogRecord::severity_number(log_records[3]), Error)
      match LogRecord::body(log_records[3]) {
        Some(body) => assert_eq(body, "Error message")
        None => assert_true(false)
      }
    }
  }
  
  // 验证日志属性
  for record in log_records {
    let attrs = LogRecord::attributes(record)
    match Attributes::get(attrs, "component") {
      Some(StringValue(value)) => assert_eq(value, "test")
      _ => assert_true(false)
    }
  }
  
  // 测试日志级别过滤
  Logger::set_level(logger, Warn)
  
  Logger::debug(logger, "Filtered debug message")
  Logger::info(logger, "Filtered info message")
  Logger::warn(logger, "Passed warning message")
  Logger::error(logger, "Passed error message")
  
  let filtered_records = MemoryLogHandler::get_records(memory_handler)
  
  // 验证过滤后的日志（只有Warn和Error级别的日志应该被记录）
  assert_eq(filtered_records.length(), 6) // 4条原始日志 + 2条过滤后的日志
  
  assert_eq(LogRecord::severity_number(filtered_records[4]), Warn)
  assert_eq(LogRecord::severity_number(filtered_records[5]), Error)
}

// Test 2: 结构化日志记录测试
test "structured logging" {
  // 创建日志提供者
  let provider = LoggerProvider::new()
  
  // 创建结构化日志记录器
  let logger = LoggerProvider::get_logger(provider, "structured-logger")
  
  // 创建JSON日志处理器
  let json_handler = JsonLogHandler::new()
  Logger::add_handler(logger, json_handler)
  
  // 创建内存处理器用于验证
  let memory_handler = MemoryLogHandler::new(1000)
  Logger::add_handler(logger, memory_handler)
  
  // 记录结构化日志
  let request_attrs = [
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/users/123")),
    ("http.status_code", IntValue(200)),
    ("user.id", StringValue("user-456")),
    ("request.duration_ms", IntValue(150)),
    ("trace.id", StringValue("trace-789"))
  ]
  
  Logger::info(logger, "HTTP request completed", Some(request_attrs))
  
  // 记录错误日志
  let error_attrs = [
    ("error.type", StringValue("ValidationError")),
    ("error.message", StringValue("Invalid user input")),
    ("error.code", StringValue("ERR-001")),
    ("user.id", StringValue("user-456")),
    ("request.id", StringValue("req-123"))
  ]
  
  Logger::error(logger, "Request processing failed", Some(error_attrs))
  
  // 获取JSON日志输出
  let json_logs = JsonLogHandler::get_output(json_handler)
  
  // 验证JSON格式
  assert_eq(json_logs.length(), 2)
  
  let first_log = Json::parse(json_logs[0])
  assert_eq(first_log["level"], "INFO")
  assert_eq(first_log["message"], "HTTP request completed")
  assert_eq(first_log["logger_name"], "structured-logger")
  assert_eq(first_log["http.method"], "GET")
  assert_eq(first_log["http.url"], "/api/users/123")
  assert_eq(first_log["http.status_code"], 200)
  assert_eq(first_log["user.id"], "user-456")
  assert_eq(first_log["request.duration_ms"], 150)
  assert_eq(first_log["trace.id"], "trace-789")
  
  let second_log = Json::parse(json_logs[1])
  assert_eq(second_log["level"], "ERROR")
  assert_eq(second_log["message"], "Request processing failed")
  assert_eq(second_log["error.type"], "ValidationError")
  assert_eq(second_log["error.message"], "Invalid user input")
  assert_eq(second_log["error.code"], "ERR-001")
  
  // 验证时间戳
  assert_true(first_log.contains("timestamp"))
  assert_true(second_log.contains("timestamp"))
  
  // 测试日志模板
  let template_logger = LoggerProvider::get_logger(provider, "template-logger")
  let template_memory_handler = MemoryLogHandler::new(1000)
  Logger::add_handler(template_logger, template_memory_handler)
  
  // 使用模板记录日志
  Logger::info_template(
    template_logger,
    "User {user_id} performed {action} on {resource}",
    [("user_id", StringValue("user-123")), ("action", StringValue("create")), ("resource", StringValue("document"))]
  )
  
  let template_records = MemoryLogHandler::get_records(template_memory_handler)
  assert_eq(template_records.length(), 1)
  
  match LogRecord::body(template_records[0]) {
    Some(body) => assert_eq(body, "User user-123 performed create on document")
    None => assert_true(false)
  }
}

// Test 3: 日志聚合和统计分析测试
test "log aggregation and statistical analysis" {
  // 创建日志聚合器
  let aggregator = LogAggregator::new()
  
  // 添加日志记录
  for i in 0..<100 {
    let severity = if i % 20 == 0 { Error } 
                   else if i % 10 == 0 { Warn } 
                   else if i % 5 == 0 { Info } 
                   else { Debug }
    
    let component = "component_" + (i % 5).to_string()
    let message = "Log message " + i.to_string()
    
    let attrs = [
      ("component", StringValue(component)),
      ("iteration", IntValue(i))
    ]
    
    let log_record = LogRecord::new(severity, message)
    LogRecord::add_attributes(log_record, attrs)
    
    LogAggregator::add_record(aggregator, log_record)
  }
  
  // 获取按级别统计
  let severity_stats = LogAggregator::get_statistics_by_severity(aggregator)
  
  assert_eq(severity_stats.get(Debug), Some(80))  // 80条Debug日志
  assert_eq(severity_stats.get(Info), Some(10))   // 10条Info日志
  assert_eq(severity_stats.get(Warn), Some(5))    // 5条Warn日志
  assert_eq(severity_stats.get(Error), Some(5))   // 5条Error日志
  
  // 获取按组件统计
  let component_stats = LogAggregator::get_statistics_by_component(aggregator)
  
  for i in 0..<5 {
    let component = "component_" + i.to_string()
    assert_eq(component_stats.get(component), Some(20)) // 每个组件20条日志
  }
  
  // 获取时间窗口统计
  let start_time = Time::now() - 3600000 // 1小时前
  let end_time = Time::now()
  
  let time_window_stats = LogAggregator::get_statistics_by_time_window(aggregator, start_time, end_time)
  assert_eq(time_window_stats.total_records, 100)
  
  // 获取错误模式分析
  let error_patterns = LogAggregator::analyze_error_patterns(aggregator)
  
  assert_true(error_patterns.length() > 0)
  
  // 添加更多错误日志进行模式分析
  for i in 0..<20 {
    let error_message = if i % 4 == 0 { "Database connection failed" }
                       else if i % 4 == 1 { "Authentication failed for user" }
                       else if i % 4 == 2 { "Validation error: Invalid input" }
                       else { "Network timeout occurred" }
    
    let error_attrs = [
      ("error.code", StringValue("ERR-" + (i % 4).to_string())),
      ("component", StringValue("error_component"))
    ]
    
    let error_record = LogRecord::new(Error, error_message)
    LogRecord::add_attributes(error_record, error_attrs)
    
    LogAggregator::add_record(aggregator, error_record)
  }
  
  // 重新分析错误模式
  let updated_error_patterns = LogAggregator::analyze_error_patterns(aggregator)
  
  assert_true(updated_error_patterns.length() >= 4)
  
  // 验证错误模式
  let db_errors = updated_error_patterns.find(fn(pattern) { pattern.message.contains("Database") })
  match db_errors {
    Some(pattern) => assert_eq(pattern.count, 5) // 原有1条 + 新增4条
    None => assert_true(false)
  }
  
  // 获取日志热力图数据
  let heatmap_data = LogAggregator::generate_activity_heatmap(aggregator, TimeGranularity::Minute)
  assert_true(heatmap_data.length() > 0)
  
  // 获取日志趋势分析
  let trend_analysis = LogAggregator::analyze_trends(aggregator, TimeGranularity::Minute, 10) // 10分钟窗口
  assert_true(trend_analysis.length() > 0)
}

// Test 4: 日志关联和上下文传播测试
test "log correlation and context propagation" {
  // 创建日志提供者
  let provider = LoggerProvider::new()
  
  // 创建带上下文的日志记录器
  let logger = LoggerProvider::get_logger(provider, "correlation-logger")
  
  // 创建内存处理器
  let memory_handler = MemoryLogHandler::new(1000)
  Logger::add_handler(logger, memory_handler)
  
  // 创建上下文
  let trace_id = "trace-123456"
  let span_id = "span-789012"
  
  let context = Context::new()
  Context::set_value(context, "trace_id", trace_id)
  Context::set_value(context, "span_id", span_id)
  Context::set_value(context, "user_id", "user-456")
  
  // 在上下文中记录日志
  Logger::info_with_context(logger, "Operation started", context, Some([("operation", StringValue("data_processing"))]))
  
  // 创建子上下文
  let child_context = Context::create_child(context)
  Context::set_value(child_context, "step", "validation")
  
  Logger::debug_with_context(logger, "Validation step", child_context, Some([("status", StringValue("in_progress"))]))
  
  // 更新子上下文
  Context::set_value(child_context, "step", "processing")
  Logger::info_with_context(logger, "Processing step", child_context, Some([("records_processed", IntValue(1000))]))
  
  // 完成操作
  Context::set_value(child_context, "step", "completed")
  Logger::info_with_context(logger, "Operation completed", context, Some([("duration_ms", IntValue(500))]))
  
  // 获取日志记录
  let log_records = MemoryLogHandler::get_records(memory_handler)
  
  // 验证日志关联
  assert_eq(log_records.length(), 4)
  
  for record in log_records {
    let attrs = LogRecord::attributes(record)
    
    // 验证trace_id和span_id在所有日志中一致
    match Attributes::get(attrs, "trace_id") {
      Some(StringValue(value)) => assert_eq(value, trace_id)
      _ => assert_true(false)
    }
    
    match Attributes::get(attrs, "span_id") {
      Some(StringValue(value)) => assert_eq(value, span_id)
      _ => assert_true(false)
    }
    
    // 验证user_id在所有日志中一致
    match Attributes::get(attrs, "user_id") {
      Some(StringValue(value)) => assert_eq(value, "user-456")
      _ => assert_true(false)
    }
  }
  
  // 验证特定上下文属性
  let validation_attrs = LogRecord::attributes(log_records[1])
  match Attributes::get(validation_attrs, "step") {
    Some(StringValue(value)) => assert_eq(value, "validation")
    _ => assert_true(false)
  }
  
  let processing_attrs = LogRecord::attributes(log_records[2])
  match Attributes::get(processing_attrs, "step") {
    Some(StringValue(value)) => assert_eq(value, "processing")
    _ => assert_true(false)
  }
  
  // 测试跨服务日志关联
  let cross_service_logger = LoggerProvider::get_logger(provider, "cross-service-logger")
  let cross_service_handler = MemoryLogHandler::new(1000)
  Logger::add_handler(cross_service_logger, cross_service_handler)
  
  // 模拟跨服务调用
  let correlation_id = "corr-789123"
  let service_context = Context::new()
  Context::set_value(service_context, "correlation_id", correlation_id)
  Context::set_value(service_context, "calling_service", "service-a")
  Context::set_value(service_context, "called_service", "service-b")
  
  // 服务A记录请求
  Logger::info_with_context(
    cross_service_logger,
    "Calling service B",
    service_context,
    Some([("endpoint", StringValue("/api/process")), ("request_id", StringValue("req-123"))])
  )
  
  // 服务B记录接收
  let service_b_context = Context::new()
  Context::set_value(service_b_context, "correlation_id", correlation_id)
  Context::set_value(service_b_context, "service", "service-b")
  
  Logger::info_with_context(
    cross_service_logger,
    "Received request from service A",
    service_b_context,
    Some([("endpoint", StringValue("/api/process")), ("request_id", StringValue("req-123"))])
  )
  
  // 服务B记录响应
  Logger::info_with_context(
    cross_service_logger,
    "Sending response to service A",
    service_b_context,
    Some([("status", StringValue("success")), ("response_time_ms", IntValue(200))])
  )
  
  // 服务A记录响应
  Logger::info_with_context(
    cross_service_logger,
    "Received response from service B",
    service_context,
    Some([("status", StringValue("success")), ("total_time_ms", IntValue(250))])
  )
  
  // 获取跨服务日志
  let cross_service_logs = MemoryLogHandler::get_records(cross_service_handler)
  assert_eq(cross_service_logs.length(), 4)
  
  // 验证所有日志都有相同的correlation_id
  for record in cross_service_logs {
    let attrs = LogRecord::attributes(record)
    match Attributes::get(attrs, "correlation_id") {
      Some(StringValue(value)) => assert_eq(value, correlation_id)
      _ => assert_true(false)
    }
  }
  
  // 测试日志关联查询
  let correlated_logs = LogCorrelator::find_by_correlation_id(memory_handler, trace_id)
  assert_eq(correlated_logs.length(), 4)
  
  let cross_service_correlated_logs = LogCorrelator::find_by_correlation_id(cross_service_handler, correlation_id)
  assert_eq(cross_service_correlated_logs.length(), 4)
}

// Test 5: 日志性能和吞吐量测试
test "logging performance and throughput" {
  // 创建高性能日志提供者
  let provider = LoggerProvider::new()
  LoggerProvider::set_async_mode(provider, true) // 启用异步模式
  LoggerProvider::set_buffer_size(provider, 10000) // 设置缓冲区大小
  
  // 创建高性能日志记录器
  let logger = LoggerProvider::get_logger(provider, "performance-logger")
  
  // 创建丢弃策略处理器（避免内存溢出）
  let drop_handler = DropLogHandler::new(1000) // 最多保留1000条日志
  Logger::add_handler(logger, drop_handler)
  
  // 测试单线程日志记录性能
  let single_thread_start = Time::now()
  
  for i in 0..<10000 {
    Logger::info(logger, "Performance test message " + i.to_string(), Some([("iteration", IntValue(i))]))
  }
  
  // 等待异步处理完成
  LoggerProvider::flush(provider)
  
  let single_thread_time = Time::now() - single_thread_start
  let single_thread_throughput = 10000.0 / (single_thread_time / 1000.0) // 每秒日志数
  
  // 验证性能（应该至少每秒1000条日志）
  assert_true(single_thread_throughput >= 1000.0)
  
  // 测试多线程日志记录性能
  let multi_thread_start = Time::now()
  
  let tasks = []
  for thread_id in 0..<4 {
    let task = ConcurrentTask::new(fn(tid: Int) {
      let thread_logger = LoggerProvider::get_logger(provider, "thread-" + tid.to_string())
      
      for i in 0..<2500 {
        Logger::info(
          thread_logger,
          "Multi-thread message " + tid.to_string() + "-" + i.to_string(),
          Some([("thread", IntValue(tid)), ("iteration", IntValue(i))])
        )
      }
    }, thread_id)
    
    tasks = tasks.push(task)
  }
  
  // 执行所有任务
  ConcurrentTask::execute_all(tasks)
  
  // 等待异步处理完成
  LoggerProvider::flush(provider)
  
  let multi_thread_time = Time::now() - multi_thread_start
  let multi_thread_throughput = 10000.0 / (multi_thread_time / 1000.0) // 每秒日志数
  
  // 验证多线程性能（应该至少每秒2000条日志）
  assert_true(multi_thread_throughput >= 2000.0)
  
  // 测试大批量日志记录
  let batch_start = Time::now()
  
  let batch_logger = LoggerProvider::get_logger(provider, "batch-logger")
  let batch_handler = BatchLogHandler::new(1000, 100) // 批量处理，每100条或每100ms刷新
  Logger::add_handler(batch_logger, batch_handler)
  
  for i in 0..<50000 {
    Logger::debug(batch_logger, "Batch test message " + i.to_string())
  }
  
  // 等待批量处理完成
  LoggerProvider::flush(provider)
  
  let batch_time = Time::now() - batch_start
  let batch_throughput = 50000.0 / (batch_time / 1000.0) // 每秒日志数
  
  // 验证批量处理性能（应该至少每秒5000条日志）
  assert_true(batch_throughput >= 5000.0)
  
  // 获取性能指标
  let metrics = LoggerProvider::get_metrics(provider)
  
  // 验证指标
  assert_true(metrics.total_logs_logged >= 70000) // 10000 + 10000 + 50000
  assert_true(metrics.average_log_time > 0)
  assert_true(metrics.max_log_time >= metrics.min_log_time)
  assert_true(metrics.async_queue_size >= 0)
  assert_true(metrics.dropped_logs_count >= 0)
  
  // 验证吞吐量指标
  assert_true(metrics.overall_throughput > 0)
  assert_true(metrics.peak_throughput >= metrics.overall_throughput)
}

// Test 6: 日志过滤和路由测试
test "log filtering and routing" {
  // 创建日志提供者
  let provider = LoggerProvider::new()
  
  // 创建日志记录器
  let logger = LoggerProvider::get_logger(provider, "filter-logger")
  
  // 创建不同类型的处理器
  let error_handler = MemoryLogHandler::new(1000)
  let warn_handler = MemoryLogHandler::new(1000)
  let info_handler = MemoryLogHandler::new(1000)
  let debug_handler = MemoryLogHandler::new(1000)
  
  // 创建级别过滤器
  let error_filter = LevelFilter::new([Error])
  let warn_filter = LevelFilter::new([Warn, Error])
  let info_filter = LevelFilter::new([Info, Warn, Error])
  let debug_filter = LevelFilter::new([Debug, Info, Warn, Error])
  
  // 创建过滤处理器
  let filtered_error_handler = FilteredLogHandler::new(error_handler, error_filter)
  let filtered_warn_handler = FilteredLogHandler::new(warn_handler, warn_filter)
  let filtered_info_handler = FilteredLogHandler::new(info_handler, info_filter)
  let filtered_debug_handler = FilteredLogHandler::new(debug_handler, debug_filter)
  
  // 添加过滤处理器
  Logger::add_handler(logger, filtered_error_handler)
  Logger::add_handler(logger, filtered_warn_handler)
  Logger::add_handler(logger, filtered_info_handler)
  Logger::add_handler(logger, filtered_debug_handler)
  
  // 记录不同级别的日志
  Logger::debug(logger, "Debug message 1")
  Logger::info(logger, "Info message 1")
  Logger::warn(logger, "Warning message 1")
  Logger::error(logger, "Error message 1")
  
  Logger::debug(logger, "Debug message 2")
  Logger::info(logger, "Info message 2")
  Logger::warn(logger, "Warning message 2")
  Logger::error(logger, "Error message 2")
  
  // 验证过滤结果
  let error_records = MemoryLogHandler::get_records(error_handler)
  let warn_records = MemoryLogHandler::get_records(warn_handler)
  let info_records = MemoryLogHandler::get_records(info_handler)
  let debug_records = MemoryLogHandler::get_records(debug_handler)
  
  // 错误处理器应该只有Error级别的日志
  assert_eq(error_records.length(), 2)
  for record in error_records {
    assert_eq(LogRecord::severity_number(record), Error)
  }
  
  // 警告处理器应该有Warn和Error级别的日志
  assert_eq(warn_records.length(), 4)
  for record in warn_records {
    let severity = LogRecord::severity_number(record)
    assert_true(severity == Warn || severity == Error)
  }
  
  // 信息处理器应该有Info、Warn和Error级别的日志
  assert_eq(info_records.length(), 6)
  for record in info_records {
    let severity = LogRecord::severity_number(record)
    assert_true(severity == Info || severity == Warn || severity == Error)
  }
  
  // 调试处理器应该有所有级别的日志
  assert_eq(debug_records.length(), 8)
  
  // 测试属性过滤器
  let attr_handler = MemoryLogHandler::new(1000)
  
  let component_filter = AttributeFilter::new("component", "critical")
  let filtered_attr_handler = FilteredLogHandler::new(attr_handler, component_filter)
  
  let attr_logger = LoggerProvider::get_logger(provider, "attr-filter-logger")
  Logger::add_handler(attr_logger, filtered_attr_handler)
  
  // 记录带不同属性的日志
  Logger::info(attr_logger, "Critical component message", Some([("component", StringValue("critical"))]))
  Logger::info(attr_logger, "Normal component message", Some([("component", StringValue("normal"))]))
  Logger::info(attr_logger, "Another critical message", Some([("component", StringValue("critical"))]))
  
  let attr_records = MemoryLogHandler::get_records(attr_handler)
  
  // 应该只有component为critical的日志
  assert_eq(attr_records.length(), 2)
  for record in attr_records {
    let attrs = LogRecord::attributes(record)
    match Attributes::get(attrs, "component") {
      Some(StringValue(value)) => assert_eq(value, "critical")
      _ => assert_true(false)
    }
  }
  
  // 测试日志路由
  let router = LogRouter::new()
  
  let db_handler = MemoryLogHandler::new(1000)
  let api_handler = MemoryLogHandler::new(1000)
  let default_handler = MemoryLogHandler::new(1000)
  
  // 添加路由规则
  LogRouter::add_route(router, "component", "database", db_handler)
  LogRouter::add_route(router, "component", "api", api_handler)
  LogRouter::set_default_route(router, default_handler)
  
  let routing_logger = LoggerProvider::get_logger(provider, "routing-logger")
  let routing_handler = RoutingLogHandler::new(router)
  Logger::add_handler(routing_logger, routing_handler)
  
  // 记录不同组件的日志
  Logger::info(routing_logger, "Database query executed", Some([("component", StringValue("database"))]))
  Logger::info(routing_logger, "API request processed", Some([("component", StringValue("api"))]))
  Logger::info(routing_logger, "General system message", Some([("component", StringValue("system"))]))
  
  // 验证路由结果
  let db_records = MemoryLogHandler::get_records(db_handler)
  let api_records = MemoryLogHandler::get_records(api_handler)
  let default_records = MemoryLogHandler::get_records(default_handler)
  
  assert_eq(db_records.length(), 1)
  assert_eq(api_records.length(), 1)
  assert_eq(default_records.length(), 1)
  
  match LogRecord::body(db_records[0]) {
    Some(body) => assert_eq(body, "Database query executed")
    None => assert_true(false)
  }
  
  match LogRecord::body(api_records[0]) {
    Some(body) => assert_eq(body, "API request processed")
    None => assert_true(false)
  }
  
  match LogRecord::body(default_records[0]) {
    Some(body) => assert_eq(body, "General system message")
    None => assert_true(false)
  }
}

// Test 7: 日志持久化和存储测试
test "log persistence and storage" {
  // 创建日志提供者
  let provider = LoggerProvider::new()
  
  // 创建文件日志处理器
  let log_file_path = "/tmp/test_log.jsonl"
  let file_handler = FileLogHandler::new(log_file_path, JsonFormat)
  
  let logger = LoggerProvider::get_logger(provider, "file-logger")
  Logger::add_handler(logger, file_handler)
  
  // 记录日志
  for i in 0..<100 {
    let severity = if i % 4 == 0 { Error } 
                   else if i % 4 == 1 { Warn } 
                   else if i % 4 == 2 { Info } 
                   else { Debug }
    
    let message = "File log message " + i.to_string()
    let attrs = [
      ("iteration", IntValue(i)),
      ("component", StringValue("file-test"))
    ]
    
    match severity {
      Error => Logger::error(logger, message, Some(attrs))
      Warn => Logger::warn(logger, message, Some(attrs))
      Info => Logger::info(logger, message, Some(attrs))
      Debug => Logger::debug(logger, message, Some(attrs))
    }
  }
  
  // 强制刷新文件
  LogHandler::flush(file_handler)
  
  // 验证文件存在
  assert_true(File::exists(log_file_path))
  
  // 读取并验证文件内容
  let file_content = File::read(log_file_path)
  let lines = file_content.split("\n")
  
  // 移除空行
  let non_empty_lines = lines.filter(fn(line) { line.length() > 0 })
  
  assert_eq(non_empty_lines.length(), 100)
  
  // 验证每行都是有效的JSON
  for line in non_empty_lines {
    let json_obj = Json::parse(line)
    assert_true(json_obj.contains("timestamp"))
    assert_true(json_obj.contains("level"))
    assert_true(json_obj.contains("message"))
    assert_true(json_obj.contains("logger_name"))
    assert_true(json_obj.contains("iteration"))
    assert_true(json_obj.contains("component"))
  }
  
  // 测试日志轮转
  let rotate_handler = RotatingFileLogHandler::new(
    "/tmp/rotating_log.jsonl",
    JsonFormat,
    1024, // 1KB大小限制
    5     // 保留5个文件
  )
  
  let rotate_logger = LoggerProvider::get_logger(provider, "rotate-logger")
  Logger::add_handler(rotate_logger, rotate_handler)
  
  // 记录大量日志以触发轮转
  for i in 0..<200 {
    let long_message = "This is a very long log message that will help trigger log rotation. " * 5 + i.to_string()
    Logger::info(rotate_logger, long_message, Some([("iteration", IntValue(i))]))
  }
  
  // 强制刷新
  LogHandler::flush(rotate_handler)
  
  // 验证轮转文件
  assert_true(File::exists("/tmp/rotating_log.jsonl"))
  assert_true(File::exists("/tmp/rotating_log.jsonl.1"))
  
  // 测试日志压缩存储
  let compress_handler = CompressedFileLogHandler::new(
    "/tmp/compressed_log.jsonl.gz",
    JsonFormat
  )
  
  let compress_logger = LoggerProvider::get_logger(provider, "compress-logger")
  Logger::add_handler(compress_logger, compress_handler)
  
  // 记录日志
  for i in 0..<50 {
    Logger::info(compress_logger, "Compressed log message " + i.to_string(), Some([("index", IntValue(i))]))
  }
  
  // 强制刷新
  LogHandler::flush(compress_handler)
  
  // 验证压缩文件存在
  assert_true(File::exists("/tmp/compressed_log.jsonl.gz"))
  
  // 读取并验证压缩文件
  let compressed_content = Gzip::decompress_file("/tmp/compressed_log.jsonl.gz")
  let compressed_lines = compressed_content.split("\n")
  let non_empty_compressed_lines = compressed_lines.filter(fn(line) { line.length() > 0 })
  
  assert_eq(non_empty_compressed_lines.length(), 50)
  
  // 验证压缩文件中的每行都是有效的JSON
  for line in non_empty_compressed_lines {
    let json_obj = Json::parse(line)
    assert_true(json_obj.contains("message"))
    assert_true(json_obj.contains("index"))
  }
  
  // 测试日志查询
  let log_storage = FileLogStorage::new(log_file_path)
  
  // 按级别查询
  let error_logs = LogStorage::query_by_level(log_storage, Error)
  assert_eq(error_logs.length(), 25) // 100条日志中25%是Error级别
  
  // 按时间范围查询
  let now = Time::now()
  let one_hour_ago = now - 3600000
  
  let recent_logs = LogStorage::query_by_time_range(log_storage, one_hour_ago, now)
  assert_eq(recent_logs.length(), 100) // 所有日志都是最近一小时内的
  
  // 按属性查询
  let component_logs = LogStorage::query_by_attribute(log_storage, "component", "file-test")
  assert_eq(component_logs.length(), 100) // 所有日志都有这个属性
  
  // 按消息内容查询
  let message_logs = LogStorage::query_by_message(log_storage, "File log message")
  assert_eq(message_logs.length(), 100) // 所有日志都包含这个消息模式
  
  // 复合查询
  let composite_logs = LogStorage::query_composite(
    log_storage,
    [("level", Error), ("component", "file-test")]
  )
  assert_eq(composite_logs.length(), 25) // 25条Error级别的日志
}

// Test 8: 日志监控和告警测试
test "log monitoring and alerting" {
  // 创建日志提供者
  let provider = LoggerProvider::new()
  
  // 创建日志记录器
  let logger = LoggerProvider::get_logger(provider, "monitoring-logger")
  
  // 创建内存处理器
  let memory_handler = MemoryLogHandler::new(1000)
  Logger::add_handler(logger, memory_handler)
  
  // 创建日志监控器
  let monitor = LogMonitor::new(memory_handler)
  
  // 创建告警规则
  let error_rate_rule = AlertRule::new(
    "error_rate_high",
    fn(logs: Array[LogRecord]) -> Bool {
      let mut error_count = 0
      for log in logs {
        if LogRecord::severity_number(log) == Error {
          error_count = error_count + 1
        }
      }
      let error_rate = error_count.to_float() / logs.length().to_float()
      error_rate > 0.2 // 错误率超过20%
    },
    Warning,
    "Error rate is above 20%"
  )
  
  let error_spike_rule = AlertRule::new(
    "error_spike",
    fn(logs: Array[LogRecord]) -> Bool {
      let mut error_count = 0
      for log in logs {
        if LogRecord::severity_number(log) == Error {
          error_count = error_count + 1
        }
      }
      error_count > 10 // 错误数量超过10
    },
    Critical,
    "Error spike detected"
  )
  
  let specific_error_rule = AlertRule::new(
    "database_connection_error",
    fn(logs: Array[LogRecord]) -> Bool {
      for log in logs {
        if LogRecord::severity_number(log) == Error {
          match LogRecord::body(log) {
            Some(body) => {
              if body.contains("Database connection failed") {
                return true
              }
            }
            None => ()
          }
        }
      }
      false
    },
    Critical,
    "Database connection error detected"
  )
  
  // 添加告警规则
  LogMonitor::add_rule(monitor, error_rate_rule)
  LogMonitor::add_rule(monitor, error_spike_rule)
  LogMonitor::add_rule(monitor, specific_error_rule)
  
  // 创建告警处理器
  let mut triggered_alerts = []
  
  let alert_handler = AlertHandler::new(fn(alert: Alert) {
    triggered_alerts = triggered_alerts.push(alert)
  })
  
  LogMonitor::set_alert_handler(monitor, alert_handler)
  
  // 启动监控
  LogMonitor::start(monitor, 5000) // 每5秒检查一次
  
  // 记录正常日志（不触发告警）
  for i in 0::<50 {
    Logger::info(logger, "Normal operation " + i.to_string())
  }
  
  // 等待监控检查
  Time::sleep(6000)
  
  // 验证没有告警触发
  assert_eq(triggered_alerts.length(), 0)
  
  // 记录一些错误日志（触发错误率告警）
  for i in 0::<20 {
    Logger::error(logger, "Error occurred " + i.to_string())
  }
  
  // 等待监控检查
  Time::sleep(6000)
  
  // 验证错误率告警触发
  assert_true(triggered_alerts.length() >= 1)
  
  let error_rate_alert = triggered_alerts.find(fn(alert) { alert.rule_id == "error_rate_high" })
  match error_rate_alert {
    Some(alert) => {
      assert_eq(alert.severity, Warning)
      assert_eq(alert.message, "Error rate is above 20%")
    }
    None => assert_true(false)
  }
  
  // 重置告警
  triggered_alerts = []
  
  // 记录大量错误日志（触发错误峰值告警）
  for i in 0::<15 {
    Logger::error(logger, "Critical error " + i.to_string())
  }
  
  // 等待监控检查
  Time::sleep(6000)
  
  // 验证错误峰值告警触发
  assert_true(triggered_alerts.length() >= 1)
  
  let error_spike_alert = triggered_alerts.find(fn(alert) { alert.rule_id == "error_spike" })
  match error_spike_alert {
    Some(alert) => {
      assert_eq(alert.severity, Critical)
      assert_eq(alert.message, "Error spike detected")
    }
    None => assert_true(false)
  }
  
  // 重置告警
  triggered_alerts = []
  
  // 记录特定错误日志（触发特定错误告警）
  Logger::error(logger, "Database connection failed")
  Logger::error(logger, "Database connection failed")
  
  // 等待监控检查
  Time::sleep(6000)
  
  // 验证特定错误告警触发
  assert_true(triggered_alerts.length() >= 1)
  
  let specific_error_alert = triggered_alerts.find(fn(alert) { alert.rule_id == "database_connection_error" })
  match specific_error_alert {
    Some(alert) => {
      assert_eq(alert.severity, Critical)
      assert_eq(alert.message, "Database connection error detected")
    }
    None => assert_true(false)
  }
  
  // 测试告警抑制
  LogMonitor::set_suppression(monitor, "error_rate_high", 30000) // 抑制30秒
  
  // 再次记录错误日志
  for i in 0::<25 {
    Logger::error(logger, "Suppressed error " + i.to_string())
  }
  
  // 等待监控检查
  Time::sleep(6000)
  
  // 验证错误率告警被抑制（没有新的错误率告警）
  let new_error_rate_alerts = triggered_alerts.filter(fn(alert) { alert.rule_id == "error_rate_high" })
  assert_eq(new_error_rate_alerts.length(), 0)
  
  // 测试告警恢复
  LogMonitor::clear_suppression(monitor, "error_rate_high")
  
  // 记录正常日志，降低错误率
  for i in 0::<100 {
    Logger::info(logger, "Recovery operation " + i.to_string())
  }
  
  // 等待监控检查
  Time::sleep(6000)
  
  // 获取监控统计
  let stats = LogMonitor::get_statistics(monitor)
  
  // 验证统计信息
  assert_true(stats.total_checks > 0)
  assert_true(stats.total_alerts_triggered > 0)
  assert_true(stats.suppressed_alerts > 0)
  assert_true(stats.error_rate > 0.0)
  
  // 停止监控
  LogMonitor::stop(monitor)
}