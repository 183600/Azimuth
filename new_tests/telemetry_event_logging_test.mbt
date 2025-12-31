// 遥测事件日志测试用例

test "basic_event_logging" {
  // 测试基本事件日志记录
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("test-logger")
  
  // 记录简单事件
  let event1 = logger.info("Application started")
  assert_eq(event1.get_level(), "INFO")
  assert_eq(event1.get_message(), "Application started")
  assert_eq(event1.get_timestamp() > 0, true)
  
  // 记录带属性的事件
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("12345"))
    .with("action", azimuth::telemetry::api::common::AttributeValue::String("login"))
    .with("success", azimuth::telemetry::api::common::AttributeValue::Bool(true))
  
  let event2 = logger.warn("User login attempt", attributes)
  assert_eq(event2.get_level(), "WARN")
  assert_eq(event2.get_message(), "User login attempt")
  assert_eq(event2.get_attributes().size(), 3)
  assert_eq(event2.get_attributes().get("user.id").unwrap().to_string(), "12345")
  
  // 记录错误事件
  let error_attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("error.code", azimuth::telemetry::api::common::AttributeValue::Int64(500))
    .with("error.message", azimuth::telemetry::api::common::AttributeValue::String("Internal server error"))
  
  let event3 = logger.error("Database connection failed", error_attributes)
  assert_eq(event3.get_level(), "ERROR")
  assert_eq(event3.get_message(), "Database connection failed")
  assert_eq(event3.get_attributes().get("error.code").unwrap().to_string(), "500")
}

test "event_logging_with_different_levels" {
  // 测试不同级别的事件日志
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("level-test-logger")
  
  // 测试所有日志级别
  let trace_event = logger.trace("Detailed trace information")
  assert_eq(trace_event.get_level(), "TRACE")
  
  let debug_event = logger.debug("Debug information")
  assert_eq(debug_event.get_level(), "DEBUG")
  
  let info_event = logger.info("General information")
  assert_eq(info_event.get_level(), "INFO")
  
  let warn_event = logger.warn("Warning message")
  assert_eq(warn_event.get_level(), "WARN")
  
  let error_event = logger.error("Error occurred")
  assert_eq(error_event.get_level(), "ERROR")
  
  let fatal_event = logger.fatal("Fatal error")
  assert_eq(fatal_event.get_level(), "FATAL")
  
  // 验证时间戳递增
  assert_eq(trace_event.get_timestamp() <= debug_event.get_timestamp(), true)
  assert_eq(debug_event.get_timestamp() <= info_event.get_timestamp(), true)
  assert_eq(info_event.get_timestamp() <= warn_event.get_timestamp(), true)
  assert_eq(warn_event.get_timestamp() <= error_event.get_timestamp(), true)
  assert_eq(error_event.get_timestamp() <= fatal_event.get_timestamp(), true)
}

test "event_logging_with_exceptions" {
  // 测试带异常的事件日志
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("exception-logger")
  
  // 创建模拟异常
  let exception = azimuth::telemetry::api::logging::Exception::new(
    "DatabaseException",
    "Connection timeout",
    "at db.connect() line 42\nat service.init() line 123"
  )
  
  // 记录带异常的事件
  let event = logger.error_with_exception("Database operation failed", exception)
  
  assert_eq(event.get_level(), "ERROR")
  assert_eq(event.get_message(), "Database operation failed")
  assert_eq(event.get_exception().is_some(), true)
  
  let logged_exception = event.get_exception().unwrap()
  assert_eq(logged_exception.get_type(), "DatabaseException")
  assert_eq(logged_exception.get_message(), "Connection timeout")
  assert_eq(logged_exception.get_stack_trace().contains("db.connect()"), true)
  assert_eq(logged_exception.get_stack_trace().contains("service.init()"), true)
}

test "structured_event_logging" {
  // 测试结构化事件日志
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("structured-logger")
  
  // 创建结构化数据
  let user_data = azimuth::telemetry::api::logging::StructuredData::new("user")
    .with_field("id", "12345")
    .with_field("name", "张三")
    .with_field("email", "zhangsan@example.com")
    .with_field("age", 25)
    .with_field("premium", true)
  
  let request_data = azimuth::telemetry::api::logging::StructuredData::new("request")
    .with_field("id", "req-abc-123")
    .with_field("method", "POST")
    .with_field("path", "/api/users")
    .with_field("duration_ms", 150)
    .with_field("status_code", 201)
  
  // 记录结构化事件
  let event = logger.info_structured("User created successfully", [user_data, request_data])
  
  assert_eq(event.get_level(), "INFO")
  assert_eq(event.get_message(), "User created successfully")
  assert_eq(event.get_structured_data().length(), 2)
  
  // 验证结构化数据
  let user_struct = event.get_structured_data()[0]
  assert_eq(user_struct.get_type(), "user")
  assert_eq(user_struct.get_field("id").unwrap(), "12345")
  assert_eq(user_struct.get_field("name").unwrap(), "张三")
  assert_eq(user_struct.get_field("email").unwrap(), "zhangsan@example.com")
  assert_eq(user_struct.get_field("age").unwrap().to_int(), 25)
  assert_eq(user_struct.get_field("premium").unwrap().to_bool(), true)
  
  let request_struct = event.get_structured_data()[1]
  assert_eq(request_struct.get_type(), "request")
  assert_eq(request_struct.get_field("method").unwrap(), "POST")
  assert_eq(request_struct.get_field("path").unwrap(), "/api/users")
  assert_eq(request_struct.get_field("duration_ms").unwrap().to_int(), 150)
}

test "event_logging_with_context" {
  // 测试带上下文的事件日志
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("context-logger")
  
  // 创建上下文
  let context = azimuth::telemetry::api::context::Context::current()
    .with("correlation.id", "corr-12345")
    .with("session.id", "session-67890")
    .with("user.id", "user-99999")
    .with("request.id", "req-def-456")
  
  // 在上下文中记录事件
  let event = logger.info_with_context("User action performed", context)
  
  assert_eq(event.get_level(), "INFO")
  assert_eq(event.get_message(), "User action performed")
  
  // 验证上下文信息被包含在事件中
  let event_context = event.get_context()
  assert_eq(event_context.get("correlation.id").unwrap(), "corr-12345")
  assert_eq(event_context.get("session.id").unwrap(), "session-67890")
  assert_eq(event_context.get("user.id").unwrap(), "user-99999")
  assert_eq(event_context.get("request.id").unwrap(), "req-def-456")
}

test "event_logging_filters_and_sampling" {
  // 测试事件日志过滤和采样
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("filtered-logger")
  
  // 配置过滤器（只记录WARN及以上级别）
  logger.set_level_filter("WARN")
  
  // 记录不同级别的事件
  let trace_event = logger.trace("This should be filtered")
  let debug_event = logger.debug("This should be filtered")
  let info_event = logger.info("This should be filtered")
  let warn_event = logger.warn("This should be logged")
  let error_event = logger.error("This should be logged")
  
  // 验证过滤结果
  assert_eq(trace_event.is_none(), true)   // 被过滤
  assert_eq(debug_event.is_none(), true)   // 被过滤
  assert_eq(info_event.is_none(), true)    // 被过滤
  assert_eq(warn_event.is_some(), true)    // 通过过滤
  assert_eq(error_event.is_some(), true)   // 通过过滤
  
  // 配置采样（10%采样率）
  logger.set_sampling_rate(0.1)
  
  let sampled_count = 0
  for i in range(0, 100) {
    let event = logger.info("Sampled event " + i.to_string())
    if event.is_some() {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证采样率大约为10%
  let actual_rate = sampled_count.to_float() / 100.0
  assert_eq(actual_rate > 0.05 && actual_rate < 0.15, true)
}

test "event_logging_serialization" {
  // 测试事件日志序列化
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("serialization-logger")
  
  // 创建复杂事件
  let attributes = azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("12345"))
    .with("action", azimuth::telemetry::api::common::AttributeValue::String("purchase"))
    .with("amount", azimuth::telemetry::api::common::AttributeValue::Float64(99.99))
    .with("success", azimuth::telemetry::api::common::AttributeValue::Bool(true))
  
  let context = azimuth::telemetry::api::context::Context::current()
    .with("request.id", "req-xyz-789")
    .with("session.id", "session-456")
  
  let event = logger.info_with_attributes_and_context("Purchase completed", attributes, context)
  
  // 序列化为JSON
  let json_event = event.to_json()
  assert_eq(json_event.length() > 0, true)
  assert_eq(json_event.contains("Purchase completed"), true)
  assert_eq(json_event.contains("INFO"), true)
  assert_eq(json_event.contains("user.id"), true)
  assert_eq(json_event.contains("request.id"), true)
  
  // 从JSON恢复事件
  let restored_event = azimuth::telemetry::api::logging::Event::from_json(json_event)
  assert_eq(restored_event.get_level(), event.get_level())
  assert_eq(restored_event.get_message(), event.get_message())
  assert_eq(restored_event.get_attributes().size(), event.get_attributes().size())
  assert_eq(restored_event.get_context().size(), event.get_context().size())
  
  // 序列化为文本格式
  let text_event = event.to_text()
  assert_eq(text_event.length() > 0, true)
  assert_eq(text_event.contains("INFO"), true)
  assert_eq(text_event.contains("Purchase completed"), true)
}

test "event_logging_performance" {
  // 测试事件日志性能
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("performance-logger")
  
  // 测试大量事件记录的性能
  let start_time = azimuth::telemetry::api::common::time::now()
  
  for i in range(0, 10000) {
    let attributes = azimuth::telemetry::api::common::Attributes::empty()
      .with("iteration", azimuth::telemetry::api::common::AttributeValue::Int64(i))
      .with("batch", azimuth::telemetry::api::common::AttributeValue::String("batch-1"))
    
    logger.info_with_attributes("Performance test event", attributes)
  }
  
  let end_time = azimuth::telemetry::api::common::time::now()
  let duration = end_time - start_time
  
  // 验证性能（应该在合理时间内完成）
  assert_eq(duration < 5000000, true)  // 小于5秒（微秒单位）
  
  // 计算吞吐量
  let events_per_second = 10000.0 / (duration.to_float() / 1000000.0)
  assert_eq(events_per_second > 1000.0, true)  // 至少每秒1000个事件
}

test "event_logging_aggregation_and_analytics" {
  // 测试事件日志聚合和分析
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("analytics-logger")
  
  // 记录不同类型的事件
  logger.info_with_attributes("User login", azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user1"))
    .with("action", azimuth::telemetry::api::common::AttributeValue::String("login"))
    .with("success", azimuth::telemetry::api::common::AttributeValue::Bool(true)))
  
  logger.error_with_attributes("Login failed", azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user2"))
    .with("action", azimuth::telemetry::api::common::AttributeValue::String("login"))
    .with("success", azimuth::telemetry::api::common::AttributeValue::Bool(false)))
  
  logger.info_with_attributes("User logout", azimuth::telemetry::api::common::Attributes::empty()
    .with("user.id", azimuth::telemetry::api::common::AttributeValue::String("user1"))
    .with("action", azimuth::telemetry::api::common::AttributeValue::String("logout"))
    .with("success", azimuth::telemetry::api::common::AttributeValue::Bool(true)))
  
  // 获取事件统计
  let analytics = logger.get_analytics()
  
  // 验证统计信息
  assert_eq(analytics.get_total_events(), 3)
  assert_eq(analytics.get_events_by_level("INFO"), 2)
  assert_eq(analytics.get_events_by_level("ERROR"), 1)
  assert_eq(analytics.get_events_by_attribute("action", "login"), 2)
  assert_eq(analytics.get_events_by_attribute("action", "logout"), 1)
  assert_eq(analytics.get_events_by_attribute("success", "true"), 2)
  assert_eq(analytics.get_events_by_attribute("success", "false"), 1)
  
  // 获取错误率
  let error_rate = analytics.get_error_rate()
  assert_eq(error_rate, 1.0 / 3.0)
  
  // 获取最活跃的用户
  let active_users = analytics.get_top_users(5)
  assert_eq(active_users.length(), 2)
  assert_eq(active_users[0], "user1")  // user1有2个事件
  assert_eq(active_users[1], "user2")  // user2有1个事件
}

test "event_logging_export_and_integration" {
  // 测试事件日志导出和集成
  
  let logger = azimuth::telemetry::api::logging::EventLogger::new("export-logger")
  
  // 记录一些测试事件
  logger.info("System startup")
  logger.warn("High memory usage detected")
  logger.error("Database connection lost")
  logger.info("System recovery completed")
  
  // 导出为不同格式
  let json_export = logger.export_json()
  assert_eq(json_export.length() > 0, true)
  assert_eq(json_export.contains("System startup"), true)
  assert_eq(json_export.contains("High memory usage"), true)
  
  let csv_export = logger.export_csv()
  assert_eq(csv_export.length() > 0, true)
  assert_eq(csv_export.contains("timestamp,level,message"), true)  // CSV头部
  
  let text_export = logger.export_text()
  assert_eq(text_export.length() > 0, true)
  assert_eq(text_export.contains("INFO"), true)
  assert_eq(text_export.contains("WARN"), true)
  assert_eq(text_export.contains("ERROR"), true)
  
  // 测试与外部系统集成
  let external_system = azimuth::telemetry::api::logging::ExternalLogSystem::new("elasticsearch")
  let export_result = logger.export_to_external(external_system)
  assert_eq(export_result.is_success(), true)
  assert_eq(export_result.get_exported_count(), 4)
  
  // 测试日志轮转
  logger.configure_rotation(1000000, 10)  // 1MB大小，10个文件
  logger.rotate_if_needed()
  assert_eq(logger.get_current_file_size() < 1000000, true)
}