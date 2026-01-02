// Azimuth 日志关联性增强测试用例
// 测试日志关联性功能，包括追踪关联、上下文传播和日志聚合

// 测试1: 追踪与日志关联
test "追踪与日志关联功能" {
  // 创建日志记录器
  let logger = Logger::new("correlation.test")
  Logger::enable_correlation(logger, true)
  
  // 创建追踪上下文
  let tracer = Tracer::new("correlation.test")
  let span = Tracer::start_span(tracer, "operation.with.logs")
  
  // 设置追踪上下文到日志
  let span_context = Span::span_context(span)
  let trace_id = SpanContext::trace_id(span_context)
  let span_id = SpanContext::span_id(span_context)
  
  Logger::set_trace_context(logger, trace_id, span_id)
  
  // 记录关联日志
  Logger::info(logger, "操作开始", [("operation", "data.processing"), ("user.id", "12345")])
  Logger::debug(logger, "处理数据块", [("block.id", "block001"), ("size", "1024")])
  Logger::warn(logger, "处理时间较长", [("duration", "2500"), ("threshold", "2000")])
  Logger::info(logger, "操作完成", [("result", "success"), ("processed.count", "150")])
  
  // 结束span
  Span::end(span)
  
  // 检查日志关联性
  let correlated_logs = Logger::get_logs_by_trace_id(logger, trace_id)
  assert_eq(correlated_logs.length(), 4)
  
  // 验证每个日志都有正确的追踪上下文
  for log in correlated_logs {
    assert_eq(LogRecord::trace_id(log), trace_id)
    assert_eq(LogRecord::span_id(log), span_id)
  }
  
  // 按日志级别过滤
  let info_logs = Logger::get_logs_by_trace_id_and_level(logger, trace_id, LogLevel::Info)
  let warn_logs = Logger::get_logs_by_trace_id_and_level(logger, trace_id, LogLevel::Warn)
  let debug_logs = Logger::get_logs_by_trace_id_and_level(logger, trace_id, LogLevel::Debug)
  
  assert_eq(info_logs.length(), 2)
  assert_eq(warn_logs.length(), 1)
  assert_eq(debug_logs.length(), 1)
}

// 测试2: 跨服务日志关联
test "跨服务日志关联功能" {
  // 创建多个服务的日志记录器
  let service_a_logger = Logger::new("service.a")
  let service_b_logger = Logger::new("service.b")
  let service_c_logger = Logger::new("service.c")
  
  // 启用日志关联
  Logger::enable_correlation(service_a_logger, true)
  Logger::enable_correlation(service_b_logger, true)
  Logger::enable_correlation(service_c_logger, true)
  
  // 创建全局追踪ID
  let global_trace_id = "trace123456789"
  let correlation_id = "corr987654321"
  
  // 在服务A中记录日志
  Logger::set_trace_context(service_a_logger, global_trace_id, "spanA001")
  Logger::set_correlation_id(service_a_logger, correlation_id)
  
  Logger::info(service_a_logger, "请求接收", [("service", "A"), ("request.id", "req001")])
  Logger::info(service_a_logger, "调用服务B", [("service", "A"), ("target.service", "B")])
  
  // 在服务B中记录日志
  Logger::set_trace_context(service_b_logger, global_trace_id, "spanB001")
  Logger::set_correlation_id(service_b_logger, correlation_id)
  
  Logger::info(service_b_logger, "请求处理", [("service", "B"), ("request.id", "req001")])
  Logger::info(service_b_logger, "调用服务C", [("service", "B"), ("target.service", "C")])
  
  // 在服务C中记录日志
  Logger::set_trace_context(service_c_logger, global_trace_id, "spanC001")
  Logger::set_correlation_id(service_c_logger, correlation_id)
  
  Logger::info(service_c_logger, "数据处理", [("service", "C"), ("request.id", "req001")])
  Logger::info(service_c_logger, "处理完成", [("service", "C"), ("result", "success")])
  
  // 创建日志关联器
  let log_correlator = LogCorrelator::new()
  LogCorrelator::add_logger(log_correlator, service_a_logger)
  LogCorrelator::add_logger(log_correlator, service_b_logger)
  LogCorrelator::add_logger(log_correlator, service_c_logger)
  
  // 按追踪ID获取所有相关日志
  let trace_logs = LogCorrelator::get_logs_by_trace_id(log_correlator, global_trace_id)
  assert_eq(trace_logs.length(), 6)
  
  // 按关联ID获取所有相关日志
  let correlation_logs = LogCorrelator::get_logs_by_correlation_id(log_correlator, correlation_id)
  assert_eq(correlation_logs.length(), 6)
  
  // 按服务分组日志
  let logs_by_service = LogCorrelator::group_logs_by_service(log_correlator, global_trace_id)
  assert_eq(logs_by_service.get("service.a").length(), 2)
  assert_eq(logs_by_service.get("service.b").length(), 2)
  assert_eq(logs_by_service.get("service.c").length(), 2)
  
  // 构建调用链
  let call_chain = LogCorrelator::build_call_chain(log_correlator, global_trace_id)
  assert_eq(call_chain.length(), 3)
  assert_eq(call_chain[0], "service.a")
  assert_eq(call_chain[1], "service.b")
  assert_eq(call_chain[2], "service.c")
}

// 测试3: 日志上下文自动传播
test "日志上下文自动传播功能" {
  // 创建上下文管理器
  let context_manager = LoggingContextManager::new()
  
  // 设置根上下文
  LoggingContextManager::set_trace_id(context_manager, "trace001")
  LoggingContextManager::set_correlation_id(context_manager, "corr001")
  LoggingContextManager::set_user_id(context_manager, "user123")
  LoggingContextManager::set_session_id(context_manager, "session456")
  
  // 创建子上下文
  let child_context = LoggingContextManager::create_child_context(context_manager)
  LoggingContextManager::set_operation(child_context, "data.processing")
  
  // 创建日志记录器
  let logger = Logger::new("context.propagation.test")
  Logger::set_context_manager(logger, context_manager)
  
  // 记录日志（自动使用当前上下文）
  Logger::info(logger, "操作开始", [])
  Logger::debug(logger, "处理数据", [("block.id", "block001")])
  
  // 切换到子上下文
  LoggingContextManager::set_current_context(context_manager, child_context)
  
  Logger::info(logger, "子操作开始", [])
  Logger::debug(logger, "处理子数据", [("sub.block.id", "subblock001")])
  
  // 恢复父上下文
  LoggingContextManager::restore_parent_context(context_manager)
  
  Logger::info(logger, "操作完成", [])
  
  // 获取所有日志
  let all_logs = Logger::get_all_logs(logger)
  assert_eq(all_logs.length(), 5)
  
  // 验证上下文传播
  let root_context_logs = all_logs.filter(fn(l) { 
    LogRecord::get_attribute(l, "operation") == None 
  })
  let child_context_logs = all_logs.filter(fn(l) { 
    LogRecord::get_attribute(l, "operation") == Some("data.processing") 
  })
  
  assert_eq(root_context_logs.length(), 3)
  assert_eq(child_context_logs.length(), 2)
  
  // 验证所有日志都有相同的追踪ID和关联ID
  for log in all_logs {
    assert_eq(LogRecord::trace_id(log), "trace001")
    assert_eq(LogRecord::correlation_id(log), "corr001")
    assert_eq(LogRecord::get_attribute(log, "user.id"), Some("user123"))
    assert_eq(LogRecord::get_attribute(log, "session.id"), Some("session456"))
  }
}

// 测试4: 日志模式匹配和关联
test "日志模式匹配和关联功能" {
  // 创建日志记录器
  let logger = Logger::new("pattern.matching.test")
  
  // 记录不同模式的日志
  Logger::error(logger, "数据库连接失败", [("error.code", "DB001"), ("retry.count", "3")])
  Logger::warn(logger, "数据库连接缓慢", [("response.time", "2500"), ("threshold", "1000")])
  Logger::info(logger, "数据库连接成功", [("connection.id", "conn123")])
  
  Logger::error(logger, "API调用失败", [("error.code", "API001"), ("endpoint", "/api/data")])
  Logger::warn(logger, "API调用缓慢", [("response.time", "3500"), ("endpoint", "/api/data")])
  Logger::info(logger, "API调用成功", [("endpoint", "/api/data"), ("response.code", "200")])
  
  // 创建模式匹配器
  let pattern_matcher = LogPatternMatcher::new()
  
  // 定义错误模式
  PatternMatcher::add_error_pattern(pattern_matcher, "database.error", 
    ["数据库连接失败", "数据库查询失败"], ["DB"])
  PatternMatcher::add_error_pattern(pattern_matcher, "api.error", 
    ["API调用失败", "API响应错误"], ["API"])
  
  // 定义性能模式
  PatternMatcher::add_performance_pattern(pattern_matcher, "slow.database", 
    ["数据库连接缓慢", "数据库查询缓慢"], [("response.time", fn(v) { v.to_int() > 2000 })])
  PatternMatcher::add_performance_pattern(pattern_matcher, "slow.api", 
    ["API调用缓慢", "API响应缓慢"], [("response.time", fn(v) { v.to_int() > 3000 })])
  
  // 匹配错误日志
  let db_errors = PatternMatcher::match_errors(pattern_matcher, Logger::get_all_logs(logger), "database.error")
  let api_errors = PatternMatcher::match_errors(pattern_matcher, Logger::get_all_logs(logger), "api.error")
  
  assert_eq(db_errors.length(), 1)
  assert_eq(api_errors.length(), 1)
  
  // 匹配性能问题
  let db_performance_issues = PatternMatcher::match_performance(pattern_matcher, Logger::get_all_logs(logger), "slow.database")
  let api_performance_issues = PatternMatcher::match_performance(pattern_matcher, Logger::get_all_logs(logger), "slow.api")
  
  assert_eq(db_performance_issues.length(), 1)
  assert_eq(api_performance_issues.length(), 1)
  
  // 关联相关日志
  let db_related_logs = PatternMatcher::find_related_logs(pattern_matcher, db_errors[0], Logger::get_all_logs(logger))
  let api_related_logs = PatternMatcher::find_related_logs(pattern_matcher, api_errors[0], Logger::get_all_logs(logger))
  
  assert_eq(db_related_logs.length(), 3) // 错误、警告、成功
  assert_eq(api_related_logs.length(), 3) // 错误、警告、成功
  
  // 生成关联报告
  let correlation_report = PatternMatcher::generate_correlation_report(pattern_matcher, Logger::get_all_logs(logger))
  assert_true(correlation_report.contains("database.error"))
  assert_true(correlation_report.contains("api.error"))
  assert_true(correlation_report.contains("slow.database"))
  assert_true(correlation_report.contains("slow.api"))
}

// 测试5: 日志聚合和统计
test "日志聚合和统计功能" {
  // 创建日志记录器
  let logger = Logger::new("aggregation.test")
  
  // 记录不同级别和类型的日志
  for i = 0; i < 10; i = i + 1 {
    Logger::info(logger, "信息日志 " + i.to_string(), [("module", "module" + (i % 3).to_string())])
  }
  
  for i = 0; i < 5; i = i + 1 {
    Logger::warn(logger, "警告日志 " + i.to_string(), [("module", "module" + (i % 2).to_string())])
  }
  
  for i = 0; i < 3; i = i + 1 {
    Logger::error(logger, "错误日志 " + i.to_string(), [("module", "module" + (i % 3).to_string())])
  }
  
  // 创建日志聚合器
  let log_aggregator = LogAggregator::new()
  LogAggregator::add_logs(log_aggregator, Logger::get_all_logs(logger))
  
  // 按级别聚合
  let level_counts = LogAggregator::count_by_level(log_aggregator)
  assert_eq(level_counts.get(LogLevel::Info), 10)
  assert_eq(level_counts.get(LogLevel::Warn), 5)
  assert_eq(level_counts.get(LogLevel::Error), 3)
  
  // 按模块聚合
  let module_counts = LogAggregator::count_by_attribute(log_aggregator, "module")
  assert_eq(module_counts.get("module0"), 5) // 3个info + 1个warn + 1个error
  assert_eq(module_counts.get("module1"), 5) // 3个info + 2个warn + 0个error
  assert_eq(module_counts.get("module2"), 8) // 4个info + 2个warn + 2个error
  
  // 按时间聚合
  let time_buckets = LogAggregator::count_by_time_bucket(log_aggregator, TimeBucket::Hour)
  assert_true(time_buckets.length() >= 1)
  
  // 计算错误率
  let error_rate = LogAggregator::calculate_error_rate(log_aggregator)
  let expected_error_rate = 3.0 / 18.0 // 3个错误 / 18个总日志
  assert_true((error_rate - expected_error_rate).abs() < 0.001)
  
  // 按模块计算错误率
  let module_error_rates = LogAggregator::calculate_error_rate_by_attribute(log_aggregator, "module")
  let module0_error_rate = module_error_rates.get("module0") // 1个错误 / 5个总日志
  let module1_error_rate = module_error_rates.get("module1") // 0个错误 / 5个总日志
  let module2_error_rate = module_error_rates.get("module2") // 2个错误 / 8个总日志
  
  assert_true((module0_error_rate - 0.2).abs() < 0.001)
  assert_eq(module1_error_rate, 0.0)
  assert_true((module2_error_rate - 0.25).abs() < 0.001)
  
  // 生成聚合报告
  let aggregation_report = LogAggregator::generate_report(log_aggregator)
  assert_true(aggregation_report.contains("总日志数: 18"))
  assert_true(aggregation_report.contains("信息: 10"))
  assert_true(aggregation_report.contains("警告: 5"))
  assert_true(aggregation_report.contains("错误: 3"))
  assert_true(aggregation_report.contains("错误率:"))
}

// 测试6: 异常堆栈关联
test "异常堆栈关联功能" {
  // 创建日志记录器
  let logger = Logger::new("exception.correlation.test")
  Logger::enable_exception_correlation(logger, true)
  
  // 模拟异常堆栈
  let exception_stack1 = [
    "at service.processData (service.js:45:15)",
    "at controller.handleRequest (controller.js:23:10)",
    "at router.route (router.js:67:8)"
  ]
  
  let exception_stack2 = [
    "at service.processData (service.js:45:15)",
    "at controller.handleRequest (controller.js:23:10)",
    "at router.route (router.js:67:8)"
  ]
  
  let exception_stack3 = [
    "at database.query (database.js:78:12)",
    "at service.processData (service.js:42:20)",
    "at controller.handleRequest (controller.js:23:10)"
  ]
  
  // 记录异常日志
  Logger::error(logger, "数据处理异常", [
    ("exception.type", "DataProcessingError"),
    ("exception.message", "Invalid data format"),
    ("exception.stack", exception_stack1.join("\n"))
  ])
  
  Logger::error(logger, "数据处理异常", [
    ("exception.type", "DataProcessingError"),
    ("exception.message", "Invalid data format"),
    ("exception.stack", exception_stack2.join("\n"))
  ])
  
  Logger::error(logger, "数据库查询异常", [
    ("exception.type", "DatabaseError"),
    ("exception.message", "Connection timeout"),
    ("exception.stack", exception_stack3.join("\n"))
  ])
  
  // 创建异常关联器
  let exception_correlator = ExceptionCorrelator::new()
  ExceptionCorrelator::add_logs(exception_correlator, Logger::get_all_logs(logger))
  
  // 按异常类型分组
  let exceptions_by_type = ExceptionCorrelator::group_by_exception_type(exception_correlator)
  assert_eq(exceptions_by_type.get("DataProcessingError").length(), 2)
  assert_eq(exceptions_by_type.get("DatabaseError").length(), 1)
  
  // 按堆栈相似性分组
  let exceptions_by_stack = ExceptionCorrelator::group_by_stack_similarity(exception_correlator, 0.8) // 80%相似度
  assert_eq(exceptions_by_stack.length(), 2) // 两个不同的堆栈模式
  
  // 找到根本原因
  let root_causes = ExceptionCorrelator::find_root_causes(exception_correlator)
  assert_true(root_causes.length() >= 1)
  
  // 分析异常传播路径
  let propagation_paths = ExceptionCorrelator::analyze_propagation_paths(exception_correlator)
  assert_true(propagation_paths.length() >= 1)
  
  // 生成异常关联报告
  let exception_report = ExceptionCorrelator::generate_report(exception_correlator)
  assert_true(exception_report.contains("DataProcessingError"))
  assert_true(exception_report.contains("DatabaseError"))
  assert_true(exception_report.contains("异常传播路径"))
}