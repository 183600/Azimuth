// Azimuth Telemetry System Additional Comprehensive Test Suite
// This file contains additional comprehensive test cases focusing on advanced scenarios
// These tests complement the existing basic and advanced test suites

// Test 1: 错误处理和异常恢复测试
test "错误处理和异常恢复测试" {
  // 创建带有错误处理的追踪提供者
  let tracer_provider = TracerProvider::with_error_handling(TracerProvider::default())
  let tracer = TracerProvider::get_tracer(tracer_provider, "error-handling-test")
  
  // 测试在span操作中的错误处理
  let span = Tracer::start_span(tracer, "error.test.span", Some([
    ("operation.type", StringValue("error.test"))
  ]))
  
  // 模拟错误情况
  let result = ErrorSimulator::simulate_span_error(span, "test.error")
  assert_true(result.is_error())
  
  // 验证错误被正确记录
  let error_events = Span::get_events(span, { |event|
    Event::name(event) == "error"
  })
  assert_true(error_events.length() > 0)
  
  // 测试错误恢复机制
  let recovered_span = ErrorRecovery::recover_span(span)
  assert_true(recovered_span.is_some())
  
  let recovered = recovered_span.unwrap()
  Span::add_event(recovered, "recovery.completed", Some([
    ("recovery.time", IntValue(Clock::now_unix_nanos(Clock::system()))),
    ("recovery.success", BoolValue(true))
  ]))
  
  Span::end(span)
  Span::end(recovered)
  
  // 测试度量系统的错误处理
  let meter_provider = MeterProvider::with_error_handling(MeterProvider::default())
  let meter = MeterProvider::get_meter(meter_provider, "error-metrics-test")
  
  let counter = Meter::create_counter(meter, "error.counter")
  
  // 模拟度量错误
  for i in 1..=10 {
    let result = Counter::add_with_error_handling(counter, i.to_double())
    if result.is_error() {
      // 验证错误度量被记录
      let error_metrics = Meter::get_error_metrics(meter)
      assert_true(error_metrics.length() > 0)
    }
  }
  
  // 测试日志系统的错误处理
  let logger_provider = LoggerProvider::with_error_handling(LoggerProvider::default())
  let logger = LoggerProvider::get_logger(logger_provider, "error-logging-test")
  
  let error_log = LogRecord::new(Error, "Test error message", Some([
    ("error.code", StringValue("TEST_ERROR")),
    ("error.severity", StringValue("high"))
  ]))
  
  let log_result = Logger::emit_with_error_handling(logger, error_log)
  assert_true(log_result.is_ok())
  
  // 验证错误日志被正确处理
  let error_logs = LoggerProvider::get_error_logs(logger_provider)
  assert_true(error_logs.length() > 0)
}

// Test 2: 并发安全性测试
test "并发安全性测试" {
  // 创建并发安全的追踪提供者
  let tracer_provider = TracerProvider::with_concurrency_safety(TracerProvider::default())
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  // 创建多个并发span
  let concurrent_spans = []
  let span_count = 100
  
  // 使用线程池创建并发span
  let thread_pool = ThreadPool::new(10)
  let futures = []
  
  for i in 1..=span_count {
    let future = ThreadPool::submit(thread_pool, { || 
      let span = Tracer::start_span(tracer, "concurrent.span-" + i.to_string(), Some([
        ("thread.id", IntValue(Thread::current_id())),
        ("span.index", IntValue(i))
      ]))
      
      // 添加事件
      Span::add_event(span, "concurrent.event", Some([
        ("event.timestamp", IntValue(Clock::now_unix_nanos(Clock::system()))),
        ("event.data", StringValue("concurrent-data-" + i.to_string()))
      ]))
      
      // 模拟一些工作
      Thread::sleep(10)  // 10ms
      
      Span::end(span)
      span
    })
    futures.push(future)
  }
  
  // 等待所有任务完成
  let completed_spans = []
  for future in futures {
    let span = Future::get(future)
    completed_spans.push(span)
  }
  
  // 验证所有span都被正确创建和结束
  assert_eq(completed_spans.length(), span_count)
  
  // 验证并发安全性 - 没有数据竞争或损坏
  for span in completed_spans {
    let context = Span::span_context(span)
    assert_true(SpanContext::is_valid(context))
    
    let events = Span::get_events(span)
    assert_true(events.length() > 0)
  }
  
  // 测试并发度量操作
  let meter_provider = MeterProvider::with_concurrency_safety(MeterProvider::default())
  let meter = MeterProvider::get_meter(meter_provider, "concurrency-metrics-test")
  
  let counter = Meter::create_counter(meter, "concurrent.counter")
  let histogram = Meter::create_histogram(meter, "concurrent.histogram")
  
  // 并发更新度量
  let metric_futures = []
  for i in 1..=1000 {
    let future = ThreadPool::submit(thread_pool, { ||
      Counter::add(counter, i.to_double())
      Histogram::record(histogram, i.to_double())
    })
    metric_futures.push(future)
  }
  
  // 等待所有度量操作完成
  for future in metric_futures {
    Future::get(future)
  }
  
  // 验证度量数据的完整性
  let metrics = Meter::collect(meter)
  assert_true(metrics.length() > 0)
  
  // 验证计数器总和
  let counter_sum = 0.0
  for metric in metrics {
    if Metric::name(metric) == "concurrent.counter" {
      counter_sum = counter_sum + Metric::value(metric)
    }
  }
  
  // 1+2+...+1000 = 500500
  assert_eq(counter_sum, 500500.0)
  
  ThreadPool::shutdown(thread_pool)
}

// Test 3: 性能基准测试
test "性能基准测试" {
  // 创建性能监控器
  let performance_monitor = PerformanceMonitor::new()
  
  // 测试span创建性能
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "performance-test")
  
  let span_count = 10000
  let start_time = Clock::now_unix_nanos(Clock::system())
  
  // 创建大量span
  let spans = []
  for i in 1..=span_count {
    let span = Tracer::start_span(tracer, "performance.span-" + i.to_string(), Some([
      ("index", IntValue(i)),
      ("batch.id", StringValue("performance-test"))
    ]))
    spans.push(span)
  }
  
  let creation_time = Clock::now_unix_nanos(Clock::system())
  
  // 结束所有span
  for span in spans {
    Span::end(span)
  }
  
  let end_time = Clock::now_unix_nanos(Clock::system())
  
  // 计算性能指标
  let creation_duration = creation_time - start_time
  let end_duration = end_time - creation_time
  let total_duration = end_time - start_time
  
  // 记录性能指标
  PerformanceMonitor::record_metric(performance_monitor, "span.creation.time", creation_duration.to_double())
  PerformanceMonitor::record_metric(performance_monitor, "span.end.time", end_duration.to_double())
  PerformanceMonitor::record_metric(performance_monitor, "span.total.time", total_duration.to_double())
  
  // 验证性能基准
  let avg_creation_time = creation_duration.to_double() / span_count.to_double()
  let avg_end_time = end_duration.to_double() / span_count.to_double()
  
  // 验证平均创建时间不超过100微秒
  assert_true(avg_creation_time < 100000.0)  // 100微秒
  
  // 验证平均结束时间不超过50微秒
  assert_true(avg_end_time < 50000.0)  // 50微秒
  
  // 测试度量系统性能
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "performance-metrics-test")
  
  let counter = Meter::create_counter(meter, "performance.counter")
  let histogram = Meter::create_histogram(meter, "performance.histogram")
  
  let metric_start_time = Clock::now_unix_nanos(Clock::system())
  
  // 执行大量度量操作
  for i in 1..=50000 {
    Counter::add(counter, i.to_double())
    Histogram::record(histogram, i.to_double())
  }
  
  let metric_end_time = Clock::now_unix_nanos(Clock::system())
  let metric_duration = metric_end_time - metric_start_time
  
  // 记录度量性能
  PerformanceMonitor::record_metric(performance_monitor, "metric.operation.time", metric_duration.to_double())
  
  // 验证度量操作性能
  let avg_metric_time = metric_duration.to_double() / 100000.0  // 总共100000次操作
  assert_true(avg_metric_time < 10000.0)  // 10微秒
  
  // 测试日志系统性能
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "performance-logging-test")
  
  let log_start_time = Clock::now_unix_nanos(Clock::system())
  
  // 发送大量日志
  for i in 1..=10000 {
    let log = LogRecord::new(Info, "Performance test log message " + i.to_string(), Some([
      ("log.index", IntValue(i)),
      ("log.batch", StringValue("performance-test"))
    ]))
    Logger::emit(logger, log)
  }
  
  let log_end_time = Clock::now_unix_nanos(Clock::system())
  let log_duration = log_end_time - log_start_time
  
  // 记录日志性能
  PerformanceMonitor::record_metric(performance_monitor, "log.operation.time", log_duration.to_double())
  
  // 验证日志操作性能
  let avg_log_time = log_duration.to_double() / 10000.0
  assert_true(avg_log_time < 50000.0)  // 50微秒
  
  // 生成性能报告
  let performance_report = PerformanceMonitor::generate_report(performance_monitor)
  assert_true(performance_report.length() > 0)
  assert_true(performance_report.contains("span.creation.time"))
  assert_true(performance_report.contains("metric.operation.time"))
  assert_true(performance_report.contains("log.operation.time"))
}

// Test 4: 跨平台兼容性测试
test "跨平台兼容性测试" {
  // 检测当前平台
  let platform_info = PlatformDetector::detect()
  let platform_name = PlatformInfo::name(platform_info)
  let platform_arch = PlatformInfo::architecture(platform_info)
  
  // 创建平台特定的配置
  let platform_config = PlatformConfig::new(platform_info)
  
  // 测试在不同平台上的span创建
  let tracer_provider = TracerProvider::with_platform_config(TracerProvider::default(), platform_config)
  let tracer = TracerProvider::get_tracer(tracer_provider, "platform-test")
  
  let span = Tracer::start_span(tracer, "platform.test.span", Some([
    ("platform.name", StringValue(platform_name)),
    ("platform.arch", StringValue(platform_arch)),
    ("platform.version", StringValue(PlatformInfo::version(platform_info)))
  ]))
  
  // 验证平台特定属性
  let platform_attrs = Span::get_attributes(span)
  assert_true(Attributes::contains(platform_attrs, "platform.name"))
  assert_true(Attributes::contains(platform_attrs, "platform.arch"))
  
  // 测试平台特定的度量
  let meter_provider = MeterProvider::with_platform_config(MeterProvider::default(), platform_config)
  let meter = MeterProvider::get_meter(meter_provider, "platform-metrics-test")
  
  // 创建平台特定的度量
  let memory_metric = PlatformMetrics::create_memory_metric(meter)
  let cpu_metric = PlatformMetrics::create_cpu_metric(meter)
  
  // 记录平台特定的度量值
  let memory_usage = PlatformMonitor::get_memory_usage()
  let cpu_usage = PlatformMonitor::get_cpu_usage()
  
  PlatformMetric::record(memory_metric, memory_usage)
  PlatformMetric::record(cpu_metric, cpu_usage)
  
  // 验证度量数据
  let metrics = Meter::collect(meter)
  assert_true(metrics.length() > 0)
  
  // 测试平台特定的日志
  let logger_provider = LoggerProvider::with_platform_config(LoggerProvider::default(), platform_config)
  let logger = LoggerProvider::get_logger(logger_provider, "platform-logging-test")
  
  let platform_log = LogRecord::new(Info, "Platform-specific log message", Some([
    ("platform.name", StringValue(platform_name)),
    ("platformlog.level", StringValue("INFO")),
    ("platform.timestamp", IntValue(Clock::now_unix_nanos(Clock::system())))
  ]))
  
  Logger::emit(logger, platform_log)
  
  // 验证平台特定的日志格式
  let logs = LoggerProvider::get_logs(logger_provider)
  assert_true(logs.length() > 0)
  
  // 测试跨平台序列化兼容性
  let serialized_span = Span::serialize(span)
  
  // 在当前平台反序列化
  let deserialized_span = Span::deserialize(serialized_span)
  assert_true(deserialized_span.is_some())
  
  // 验证反序列化后的数据完整性
  let deserialized = deserialized_span.unwrap()
  let original_context = Span::span_context(span)
  let deserialized_context = Span::span_context(deserialized)
  
  assert_eq(SpanContext::trace_id(original_context), SpanContext::trace_id(deserialized_context))
  assert_eq(SpanContext::span_id(original_context), SpanContext::span_id(deserialized_context))
  
  Span::end(span)
  Span::end(deserialized)
  
  // 测试平台特定的错误处理
  let platform_errors = PlatformErrorHandler::get_platform_specific_errors(platform_info)
  assert_true(platform_errors.length() > 0)
  
  // 模拟平台特定的错误
  for error_type in platform_errors {
    let error_result = PlatformErrorSimulator::simulate_error(error_type)
    assert_true(error_result.is_error())
    
    // 验证错误被正确处理
    let handled_error = PlatformErrorHandler::handle_platform_error(error_result.unwrap_error())
    assert_true(handled_error.is_ok())
  }
}

// Test 5: 数据完整性验证测试
test "数据完整性验证测试" {
  // 创建数据完整性验证器
  let integrity_validator = DataIntegrityValidator::new()
  
  // 测试span数据完整性
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "integrity-test")
  
  let span = Tracer::start_span(tracer, "integrity.test.span", Some([
    ("test.data", StringValue("important.data")),
    ("checksum", StringValue("initial.checksum"))
  ]))
  
  // 添加事件
  Span::add_event(span, "integrity.test.event", Some([
    ("event.data", StringValue("critical.event.data")),
    ("event.timestamp", IntValue(Clock::now_unix_nanos(Clock::system())))
  ]))
  
  // 计算数据校验和
  let initial_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, span)
  
  // 验证初始数据完整性
  assert_true(DataIntegrityValidator::validate_span(integrity_validator, span, initial_checksum))
  
  // 序列化和反序列化数据
  let serialized_data = Span::serialize(span)
  let deserialized_span = Span::deserialize(serialized_data)
  
  // 验证反序列化后的数据完整性
  assert_true(deserialized_span.is_some())
  let deserialized = deserialized_span.unwrap()
  
  let deserialized_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, deserialized)
  assert_eq(initial_checksum, deserialized_checksum)
  
  // 测试数据损坏检测
  let corrupted_data = DataCorruptionSimulator::corrupt_data(serialized_data, 0.1)  // 10%损坏率
  let corrupted_span = Span::deserialize(corrupted_data)
  
  if corrupted_span.is_some() {
    let corrupted = corrupted_span.unwrap()
    let corrupted_checksum = DataIntegrityValidator::calculate_checksum(integrity_validator, corrupted)
    
    // 验证损坏的数据被检测到
    assert_false(DataIntegrityValidator::validate_span(integrity_validator, corrupted, initial_checksum))
    assert_true(corrupted_checksum != initial_checksum)
  }
  
  // 测试度量数据完整性
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "integrity-metrics-test")
  
  let counter = Meter::create_counter(meter, "integrity.counter")
  
  // 记录度量数据
  for i in 1..=100 {
    Counter::add(counter, i.to_double())
  }
  
  // 计算度量数据校验和
  let metrics = Meter::collect(meter)
  let metrics_checksum = DataIntegrityValidator::calculate_metrics_checksum(integrity_validator, metrics)
  
  // 验证度量数据完整性
  assert_true(DataIntegrityValidator::validate_metrics(integrity_validator, metrics, metrics_checksum))
  
  // 测试日志数据完整性
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "integrity-logging-test")
  
  // 创建重要日志记录
  let critical_log = LogRecord::new(Error, "Critical system error", Some([
    ("error.code", StringValue("CRITICAL_ERROR")),
    ("error.severity", StringValue("critical")),
    ("system.component", StringValue("database")),
    ("error.timestamp", IntValue(Clock::now_unix_nanos(Clock::system())))
  ]))
  
  Logger::emit(logger, critical_log)
  
  // 获取日志并验证完整性
  let logs = LoggerProvider::get_logs(logger_provider)
  assert_true(logs.length() > 0)
  
  let logs_checksum = DataIntegrityValidator::calculate_logs_checksum(integrity_validator, logs)
  assert_true(DataIntegrityValidator::validate_logs(integrity_validator, logs, logs_checksum))
  
  // 测试端到端数据完整性
  let end_to_end_data = EndToEndDataCollector::collect(tracer_provider, meter_provider, logger_provider)
  let e2e_checksum = DataIntegrityValidator::calculate_e2e_checksum(integrity_validator, end_to_end_data)
  
  // 验证端到端数据完整性
  assert_true(DataIntegrityValidator::validate_e2e_data(integrity_validator, end_to_end_data, e2e_checksum))
  
  // 测试数据恢复机制
  let backup_data = DataBackupManager::create_backup(end_to_end_data)
  let recovered_data = DataBackupManager::restore_from_backup(backup_data)
  
  // 验证恢复的数据完整性
  let recovered_checksum = DataIntegrityValidator::calculate_e2e_checksum(integrity_validator, recovered_data)
  assert_eq(e2e_checksum, recovered_checksum)
  
  Span::end(span)
  Span::end(deserialized)
}

// Test 6: 安全性和权限测试
test "安全性和权限测试" {
  // 创建安全管理器
  let security_manager = SecurityManager::new()
  
  // 设置安全策略
  SecurityManager::set_policy(security_manager, "data.encryption", true)
  SecurityManager::set_policy(security_manager, "access.control", true)
  SecurityManager::set_policy(security_manager, "audit.logging", true)
  
  // 创建具有安全功能的追踪提供者
  let tracer_provider = TracerProvider::with_security(TracerProvider::default(), security_manager)
  let tracer = TracerProvider::get_tracer(tracer_provider, "security-test")
  
  // 测试敏感数据加密
  let sensitive_span = Tracer::start_span(tracer, "security.sensitive.span", Some([
    ("user.id", StringValue("user123")),
    ("sensitive.data", EncryptedValue("secret.information")),
    ("access.level", StringValue("confidential"))
  ]))
  
  // 验证敏感数据被加密
  let sensitive_attrs = Span::get_attributes(sensitive_span)
  let encrypted_data = Attributes::get(sensitive_attrs, "sensitive.data")
  
  assert_true(encrypted_data.is_some())
  match encrypted_data.unwrap() {
    EncryptedValue(data) => {
      // 验证数据是加密的（不是原始值）
      assert_false(data.contains("secret.information"))
    }
    _ => assert_true(false)
  }
  
  // 测试访问控制
  let access_context = AccessContext::new("user123", ["read", "write"])
  
  // 测试有权限的操作
  let authorized_result = SecurityManager::check_access(security_manager, access_context, "span.read")
  assert_true(authorized_result.is_authorized())
  
  // 测试无权限的操作
  let unauthorized_result = SecurityManager::check_access(security_manager, access_context, "admin.delete")
  assert_false(unauthorized_result.is_authorized())
  
  // 测试审计日志
  let audit_logger = SecurityManager::get_audit_logger(security_manager)
  
  // 记录安全事件
  AuditLogger::log_security_event(audit_logger, "access.attempt", Some([
    ("user.id", StringValue("user123")),
    ("resource", StringValue("span.data")),
    ("action", StringValue("read")),
    ("result", StringValue("authorized")),
    ("timestamp", IntValue(Clock::now_unix_nanos(Clock::system())))
  ]))
  
  // 验证审计日志
  let audit_logs = AuditLogger::get_security_events(audit_logger)
  assert_true(audit_logs.length() > 0)
  
  // 测试数据脱敏
  let pii_data = PersonallyIdentifiableInformation::new("John Doe", "john.doe@example.com", "555-123-4567")
  let masked_data = DataMasking::mask_pii(pii_data)
  
  // 验证PII数据被正确脱敏
  assert_false(masked_data.contains("John Doe"))
  assert_false(masked_data.contains("john.doe@example.com"))
  assert_false(masked_data.contains("555-123-4567"))
  assert_true(masked_data.contains("***"))  // 脱敏标记
  
  // 测试安全度量
  let meter_provider = MeterProvider::with_security(MeterProvider::default(), security_manager)
  let meter = MeterProvider::get_meter(meter_provider, "security-metrics-test")
  
  let security_counter = Meter::create_security_counter(meter, "security.events")
  
  // 记录安全事件
  SecurityCounter::record_event(security_counter, "authentication.success")
  SecurityCounter::record_event(security_counter, "authorization.failure")
  SecurityCounter::record_event(security_counter, "data.encryption")
  
  // 验证安全度量
  let security_metrics = Meter::collect(meter)
  assert_true(security_metrics.length() > 0)
  
  // 测试安全配置验证
  let security_config = SecurityConfig::new([
    ("encryption.enabled", true),
    ("access.control.enabled", true),
    ("audit.logging.enabled", true),
    ("data.retention.days", 90)
  ])
  
  let validation_result = SecurityConfig::validate(security_config)
  assert_true(validation_result.is_valid())
  
  // 测试无效安全配置
  let invalid_security_config = SecurityConfig::new([
    ("encryption.enabled", false),  // 违反安全策略
    ("access.control.enabled", true),
    ("audit.logging.enabled", true)
  ])
  
  let invalid_validation_result = SecurityConfig::validate(invalid_security_config)
  assert_false(invalid_validation_result.is_valid())
  
  Span::end(sensitive_span)
}

// Test 7: 国际化和本地化测试
test "国际化和本地化测试" {
  // 创建国际化管理器
  let i18n_manager = I18nManager::new()
  
  // 加载不同语言资源
  I18nManager::load_language(i18n_manager, "en", {
    "span.name.test": "Test Span",
    "log.message.error": "Error occurred: {0}",
    "metric.name.counter": "Request Counter",
    "attribute.user.id": "User ID"
  })
  
  I18nManager::load_language(i18n_manager, "zh", {
    "span.name.test": "测试跨度",
    "log.message.error": "发生错误: {0}",
    "metric.name.counter": "请求计数器",
    "attribute.user.id": "用户ID"
  })
  
  I18nManager::load_language(i18n_manager, "ja", {
    "span.name.test": "テストスパン",
    "log.message.error": "エラーが発生しました: {0}",
    "metric.name.counter": "リクエストカウンター",
    "attribute.user.id": "ユーザーID"
  })
  
  // 测试英文本地化
  I18nManager::set_locale(i18n_manager, "en")
  
  let tracer_provider = TracerProvider::with_i18n(TracerProvider::default(), i18n_manager)
  let tracer = TracerProvider::get_tracer(tracer_provider, "i18n-test")
  
  let en_span = Tracer::start_span(tracer, I18nManager::translate(i18n_manager, "span.name.test"), Some([
    (I18nManager::translate(i18n_manager, "attribute.user.id"), StringValue("user123"))
  ]))
  
  // 验证英文本地化
  assert_eq(Span::name(en_span), "Test Span")
  
  // 测试中文本地化
  I18nManager::set_locale(i18n_manager, "zh")
  
  let zh_span = Tracer::start_span(tracer, I18nManager::translate(i18n_manager, "span.name.test"), Some([
    (I18nManager::translate(i18n_manager, "attribute.user.id"), StringValue("用户123"))
  ]))
  
  // 验证中文本地化
  assert_eq(Span::name(zh_span), "测试跨度")
  
  // 测试日文本地化
  I18nManager::set_locale(i18n_manager, "ja")
  
  let ja_span = Tracer::start_span(tracer, I18nManager::translate(i18n_manager, "span.name.test"), Some([
    (I18nManager::translate(i18n_manager, "attribute.user.id"), StringValue("ユーザー123"))
  ]))
  
  // 验证日文本地化
  assert_eq(Span::name(ja_span), "テストスパン")
  
  // 测试日志消息本地化
  let logger_provider = LoggerProvider::with_i18n(LoggerProvider::default(), i18n_manager)
  let logger = LoggerProvider::get_logger(logger_provider, "i18n-logging-test")
  
  // 测试英文错误消息
  I18nManager::set_locale(i18n_manager, "en")
  let en_error_message = I18nManager::format_message(i18n_manager, "log.message.error", ["Database connection failed"])
  
  let en_log = LogRecord::new(Error, en_error_message, Some([
    ("locale", StringValue("en"))
  ]))
  
  Logger::emit(logger, en_log)
  assert_eq(en_error_message, "Error occurred: Database connection failed")
  
  // 测试中文错误消息
  I18nManager::set_locale(i18n_manager, "zh")
  let zh_error_message = I18nManager::format_message(i18n_manager, "log.message.error", ["数据库连接失败"])
  
  let zh_log = LogRecord::new(Error, zh_error_message, Some([
    ("locale", StringValue("zh"))
  ]))
  
  Logger::emit(logger, zh_log)
  assert_eq(zh_error_message, "发生错误: 数据库连接失败")
  
  // 测试日文错误消息
  I18nManager::set_locale(i18n_manager, "ja")
  let ja_error_message = I18nManager::format_message(i18n_manager, "log.message.error", ["データベース接続に失敗しました"])
  
  let ja_log = LogRecord::new(Error, ja_error_message, Some([
    ("locale", StringValue("ja"))
  ]))
  
  Logger::emit(logger, ja_log)
  assert_eq(ja_error_message, "エラーが発生しました: データベース接続に失敗しました")
  
  // 测试度量名称本地化
  let meter_provider = MeterProvider::with_i18n(MeterProvider::default(), i18n_manager)
  let meter = MeterProvider::get_meter(meter_provider, "i18n-metrics-test")
  
  // 测试英文度量名称
  I18nManager::set_locale(i18n_manager, "en")
  let en_counter = Meter::create_counter(meter, I18nManager::translate(i18n_manager, "metric.name.counter"))
  Counter::add(en_counter, 100.0)
  
  // 测试中文度量名称
  I18nManager::set_locale(i18n_manager, "zh")
  let zh_counter = Meter::create_counter(meter, I18nManager::translate(i18n_manager, "metric.name.counter"))
  Counter::add(zh_counter, 200.0)
  
  // 测试日文度量名称
  I18nManager::set_locale(i18n_manager, "ja")
  let ja_counter = Meter::create_counter(meter, I18nManager::translate(i18n_manager, "metric.name.counter"))
  Counter::add(ja_counter, 300.0)
  
  // 验证本地化度量数据
  let metrics = Meter::collect(meter)
  assert_true(metrics.length() > 0)
  
  // 测试时区和日期格式本地化
  let timestamp = Clock::now_unix_nanos(Clock::system())
  
  // 测试英文日期格式
  I18nManager::set_locale(i18n_manager, "en")
  let en_date_format = I18nManager::format_datetime(i18n_manager, timestamp, "en-US")
  
  // 测试中文日期格式
  I18nManager::set_locale(i18n_manager, "zh")
  let zh_date_format = I18nManager::format_datetime(i18n_manager, timestamp, "zh-CN")
  
  // 测试日文日期格式
  I18nManager::set_locale(i18n_manager, "ja")
  let ja_date_format = I18nManager::format_datetime(i18n_manager, timestamp, "ja-JP")
  
  // 验证不同地区的日期格式
  assert_true(en_date_format != zh_date_format)
  assert_true(zh_date_format != ja_date_format)
  assert_true(en_date_format != ja_date_format)
  
  // 测试数字格式本地化
  let number = 1234567.89
  
  I18nManager::set_locale(i18n_manager, "en")
  let en_number_format = I18nManager::format_number(i18n_manager, number)
  
  I18nManager::set_locale(i18n_manager, "zh")
  let zh_number_format = I18nManager::format_number(i18n_manager, number)
  
  I18nManager::set_locale(i18n_manager, "ja")
  let ja_number_format = I18nManager::format_number(i18n_manager, number)
  
  // 验证不同地区的数字格式
  assert_true(en_number_format != zh_number_format)
  assert_true(zh_number_format != ja_number_format)
  assert_true(en_number_format != ja_number_format)
  
  // 验证日志包含不同语言的内容
  let logs = LoggerProvider::get_logs(logger_provider)
  assert_true(logs.length() >= 3)
  
  let en_log_found = false
  let zh_log_found = false
  let ja_log_found = false
  
  for log in logs {
    let locale_attr = LogRecord::get_attribute(log, "locale")
    if locale_attr.is_some() {
      match locale_attr.unwrap() {
        StringValue(locale) => {
          match locale {
            "en" => en_log_found = true
            "zh" => zh_log_found = true
            "ja" => ja_log_found = true
            _ => ()
          }
        }
        _ => ()
      }
    }
  }
  
  assert_true(en_log_found)
  assert_true(zh_log_found)
  assert_true(ja_log_found)
  
  Span::end(en_span)
  Span::end(zh_span)
  Span::end(ja_span)
}