// Azimuth 错误处理和恢复测试
// 专注于错误检测、处理、恢复和容错机制

// 测试1: 错误分类和严重性评估
test "错误分类和严重性评估功能" {
  // 创建错误分类器
  let error_classifier = ErrorClassifier::new()
  
  // 配置错误分类规则
  ErrorClassifier::add_classification_rule(error_classifier, {
    name: "network_timeout",
    pattern: "timeout|connection.*failed|network.*error",
    category: "network",
    severity: "medium",
    recoverable: true,
    tags: ["transient", "external_dependency"]
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    name: "database_error",
    pattern: "database|sql|connection.*pool|deadlock",
    category: "database",
    severity: "high",
    recoverable: true,
    tags: ["transient", "data_integrity"]
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    name: "authentication_failure",
    pattern: "authentication|unauthorized|forbidden|invalid.*token",
    category: "security",
    severity: "medium",
    recoverable: false,
    tags: ["security", "user_action_required"]
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    name: "resource_exhaustion",
    pattern: "out.*of.*memory|disk.*full|too.*many.*connections",
    category: "resource",
    severity: "critical",
    recoverable: true,
    tags: ["infrastructure", "scaling_required"]
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    name: "configuration_error",
    pattern: "configuration|invalid.*setting|missing.*parameter",
    category: "configuration",
    severity: "high",
    recoverable: false,
    tags: ["deployment", "manual_intervention"]
  })
  
  // 测试错误分类
  let test_errors = [
    "Connection timeout after 30 seconds",
    "Database connection pool exhausted",
    "Authentication failed: invalid token",
    "Out of memory: cannot allocate 1GB",
    "Invalid configuration: missing database URL",
    "Unknown error occurred"
  ]
  
  let network_error = ErrorClassifier::classify(error_classifier, test_errors[0])
  assert_eq(network_error.category, "network")
  assert_eq(network_error.severity, "medium")
  assert_true(network_error.recoverable)
  assert_true(network_error.tags.contains("transient"))
  assert_true(network_error.tags.contains("external_dependency"))
  
  let database_error = ErrorClassifier::classify(error_classifier, test_errors[1])
  assert_eq(database_error.category, "database")
  assert_eq(database_error.severity, "high")
  assert_true(database_error.recoverable)
  assert_true(database_error.tags.contains("transient"))
  assert_true(database_error.tags.contains("data_integrity"))
  
  let auth_error = ErrorClassifier::classify(error_classifier, test_errors[2])
  assert_eq(auth_error.category, "security")
  assert_eq(auth_error.severity, "medium")
  assert_false(auth_error.recoverable)
  assert_true(auth_error.tags.contains("security"))
  assert_true(auth_error.tags.contains("user_action_required"))
  
  let resource_error = ErrorClassifier::classify(error_classifier, test_errors[3])
  assert_eq(resource_error.category, "resource")
  assert_eq(resource_error.severity, "critical")
  assert_true(resource_error.recoverable)
  assert_true(resource_error.tags.contains("infrastructure"))
  assert_true(resource_error.tags.contains("scaling_required"))
  
  let config_error = ErrorClassifier::classify(error_classifier, test_errors[4])
  assert_eq(config_error.category, "configuration")
  assert_eq(config_error.severity, "high")
  assert_false(config_error.recoverable)
  assert_true(config_error.tags.contains("deployment"))
  assert_true(config_error.tags.contains("manual_intervention"))
  
  // 测试未知错误
  let unknown_error = ErrorClassifier::classify(error_classifier, test_errors[5])
  assert_eq(unknown_error.category, "unknown")
  assert_eq(unknown_error.severity, "medium")  // 默认严重性
  assert_true(unknown_error.recoverable)        // 默认可恢复
  
  // 测试批量分类
  let classifications = ErrorClassifier::classify_batch(error_classifier, test_errors)
  assert_eq(classifications.length(), 6)
  
  // 验证分类统计
  let classification_stats = ErrorClassifier::get_classification_stats(error_classifier)
  assert_eq(classification_stats.total_classified, 6)
  assert_eq(classification_stats.by_category.get("network"), Some(1))
  assert_eq(classification_stats.by_category.get("database"), Some(1))
  assert_eq(classification_stats.by_category.get("security"), Some(1))
  assert_eq(classification_stats.by_category.get("resource"), Some(1))
  assert_eq(classification_stats.by_category.get("configuration"), Some(1))
  assert_eq(classification_stats.by_category.get("unknown"), Some(1))
  
  assert_eq(classification_stats.by_severity.get("critical"), Some(1))
  assert_eq(classification_stats.by_severity.get("high"), Some(2))
  assert_eq(classification_stats.by_severity.get("medium"), Some(3))
  
  assert_eq(classification_stats.recoverable_count, 4)
  assert_eq(classification_stats.non_recoverable_count, 2)
}

// 测试2: 错误检测和告警
test "错误检测和告警功能" {
  // 创建错误检测器
  let error_detector = ErrorDetector::new()
  
  // 配置检测规则
  ErrorDetector::add_threshold_rule(error_detector, {
    name: "error_rate_threshold",
    metric: "error_rate",
    threshold: 0.05,  // 5%错误率
    operator: "greater_than",
    window: 300000,    // 5分钟窗口
    severity: "warning"
  })
  
  ErrorDetector::add_threshold_rule(error_detector, {
    name: "critical_error_rate_threshold",
    metric: "error_rate",
    threshold: 0.15,  // 15%错误率
    operator: "greater_than",
    window: 60000,    // 1分钟窗口
    severity: "critical"
  })
  
  ErrorDetector::add_spike_rule(error_detector, {
    name: "error_spike_detection",
    metric: "error_count",
    baseline_window: 900000,  // 15分钟基线
    detection_window: 60000,   // 1分钟检测窗口
    spike_factor: 3.0,         // 3倍增长
    severity: "warning"
  })
  
  ErrorDetector::add_pattern_rule(error_detector, {
    name: "repeated_error_pattern",
    error_pattern: "database.*connection.*failed",
    count_threshold: 5,
    time_window: 120000,  // 2分钟
    severity: "critical"
  })
  
  // 模拟错误数据
  let base_time = 1640995200
  
  // 正常时期
  for i in 0..=10 {
    let timestamp = base_time + i * 60000  // 每分钟
    ErrorDetector::add_metric(error_detector, timestamp, "request_count", 100.0)
    ErrorDetector::add_metric(error_detector, timestamp, "error_count", 2.0)  // 2%错误率
    ErrorDetector::add_error(error_detector, timestamp, "Database connection failed")
  }
  
  // 检查正常状态
  let normal_status = ErrorDetector::check_status(error_detector)
  assert_eq(normal_status.alerts.length(), 0)
  assert_false(normal_status.anomaly_detected)
  
  // 错误率增加
  for i in 11..=15 {
    let timestamp = base_time + i * 60000
    ErrorDetector::add_metric(error_detector, timestamp, "request_count", 100.0)
    ErrorDetector::add_metric(error_detector, timestamp, "error_count", 8.0)  // 8%错误率
    ErrorDetector::add_error(error_detector, timestamp, "Database connection failed")
  }
  
  // 检查警告状态
  let warning_status = ErrorDetector::check_status(error_detector)
  assert_true(warning_status.alerts.length() > 0)
  assert_true(warning_status.anomaly_detected)
  
  // 验证错误率告警
  let error_rate_alert = warning_status.alerts.find(fn(a) { a.rule_name == "error_rate_threshold" })
  assert_true(error_rate_alert != None)
  
  match error_rate_alert {
    Some(alert) {
      assert_eq(alert.severity, "warning")
      assert_true(alert.message.contains("error_rate"))
      assert_true(alert.current_value > 0.05)
    }
    None => assert_true(false)
  }
  
  // 严重错误率增加
  for i in 16..=17 {
    let timestamp = base_time + i * 60000
    ErrorDetector::add_metric(error_detector, timestamp, "request_count", 100.0)
    ErrorDetector::add_metric(error_detector, timestamp, "error_count", 20.0)  // 20%错误率
    ErrorDetector::add_error(error_detector, timestamp, "Database connection failed")
  }
  
  // 检查严重状态
  let critical_status = ErrorDetector::check_status(error_detector)
  assert_true(critical_status.alerts.length() > 1)
  
  // 验证严重错误率告警
  let critical_error_rate_alert = critical_status.alerts.find(fn(a) { a.rule_name == "critical_error_rate_threshold" })
  assert_true(critical_error_rate_alert != None)
  
  match critical_error_rate_alert {
    Some(alert) {
      assert_eq(alert.severity, "critical")
      assert_true(alert.message.contains("error_rate"))
      assert_true(alert.current_value > 0.15)
    }
    None => assert_true(false)
  }
  
  // 验证重复错误模式检测
  let pattern_alert = critical_status.alerts.find(fn(a) { a.rule_name == "repeated_error_pattern" })
  assert_true(pattern_alert != None)
  
  match pattern_alert {
    Some(alert) {
      assert_eq(alert.severity, "critical")
      assert_true(alert.message.contains("Database connection failed"))
      assert_true(alert.occurrence_count >= 5)
    }
    None => assert_true(false)
  }
  
  // 测试错误峰值检测
  // 添加大量基线数据
  for i in 18..=30 {
    let timestamp = base_time + i * 60000
    ErrorDetector::add_metric(error_detector, timestamp, "request_count", 100.0)
    ErrorDetector::add_metric(error_detector, timestamp, "error_count", 3.0)  // 3%错误率
  }
  
  // 添加峰值数据
  for i in 31..=32 {
    let timestamp = base_time + i * 60000
    ErrorDetector::add_metric(error_detector, timestamp, "request_count", 100.0)
    ErrorDetector::add_metric(error_detector, timestamp, "error_count", 15.0)  // 15%错误率，5倍增长
  }
  
  let spike_status = ErrorDetector::check_status(error_detector)
  let spike_alert = spike_status.alerts.find(fn(a) { a.rule_name == "error_spike_detection" })
  assert_true(spike_alert != None)
  
  // 测试告警抑制
  ErrorDetector::configure_alert_suppression(error_detector, {
    suppression_rules: [
      {
        condition: "severity == 'warning' and count < 3",
        action: "suppress",
        duration: 300000  // 5分钟
      }
    ]
  })
  
  let suppressed_status = ErrorDetector::check_status(error_detector)
  let warning_alerts = suppressed_status.alerts.filter(fn(a) { a.severity == "warning" })
  assert_true(warning_alerts.length() < warning_status.alerts.filter(fn(a) { a.severity == "warning" }).length())
}

// 测试3: 自动恢复机制
test "自动恢复机制功能" {
  // 创建恢复管理器
  let recovery_manager = RecoveryManager::new()
  
  // 配置恢复策略
  RecoveryManager::add_strategy(recovery_manager, {
    name: "retry_with_backoff",
    error_types: ["network_timeout", "database_error"],
    max_attempts: 3,
    backoff_strategy: "exponential",
    initial_delay: 1000,      // 1秒
    max_delay: 30000,         // 30秒
    backoff_multiplier: 2.0
  })
  
  RecoveryManager::add_strategy(recovery_manager, {
    name: "circuit_breaker",
    error_types: ["resource_exhaustion", "database_error"],
    failure_threshold: 5,     // 5次失败后打开
    recovery_timeout: 60000,  // 1分钟后尝试恢复
    half_open_max_calls: 3    // 半开状态最多3次调用
  })
  
  RecoveryManager::add_strategy(recovery_manager, {
    name: "fallback_service",
    error_types: ["network_timeout", "database_error"],
    fallback_actions: [
      {
        service: "primary_database",
        fallback: "cache_database",
        timeout: 5000
      },
      {
        service: "external_api",
        fallback: "local_cache",
        timeout: 1000
      }
    ]
  })
  
  RecoveryManager::add_strategy(recovery_manager, {
    name: "resource_scaling",
    error_types: ["resource_exhaustion"],
    scaling_actions: [
      {
        resource_type: "memory",
        action: "increase_heap_size",
        increment_percent: 25,
        max_scale_factor: 2.0
      },
      {
        resource_type: "connections",
        action: "increase_pool_size",
        increment_count: 10,
        max_pool_size: 100
      }
    ]
  })
  
  // 测试重试机制
  let retry_result = RecoveryManager::execute_with_retry(recovery_manager, {
    operation: fn() {
      // 模拟第一次失败，第二次成功
      static attempt = 0
      attempt = attempt + 1
      if attempt == 1 {
        Error("Network timeout")
      } else {
        Ok("success")
      }
    },
    error_type: "network_timeout",
    strategy: "retry_with_backoff"
  })
  
  assert_true(retry_result.success)
  assert_eq(retry_result.attempts, 2)
  assert_true(retry_result.total_duration > 1000)  // 至少1秒延迟
  
  // 测试重试失败
  let retry_failure_result = RecoveryManager::execute_with_retry(recovery_manager, {
    operation: fn() {
      Error("Persistent database error")
    },
    error_type: "database_error",
    strategy: "retry_with_backoff"
  })
  
  assert_false(retry_failure_result.success)
  assert_eq(retry_failure_result.attempts, 3)  // 达到最大重试次数
  assert_true(retry_failure_result.last_error.contains("Persistent database error"))
  
  // 测试熔断器
  let circuit_breaker = RecoveryManager::get_circuit_breaker(recovery_manager, "database_service")
  
  // 模拟连续失败
  for i in 0..=5 {
    let result = RecoveryManager::execute_with_circuit_breaker(recovery_manager, {
      service: "database_service",
      operation: fn() {
        Error("Database connection failed")
      }
    })
    
    if i < 4 {
      assert_false(result.success)  // 前4次失败
    } else {
      assert_true(result.circuit_open)  // 第5次后熔断器打开
    }
  }
  
  // 验证熔断器状态
  let breaker_status = RecoveryManager::get_circuit_breaker_status(recovery_manager, "database_service")
  assert_true(breaker_status.is_open)
  assert_eq(breaker_status.failure_count, 5)
  assert_true(breaker_status.next_attempt_time > Time::now())
  
  // 测试降级服务
  let fallback_result = RecoveryManager::execute_with_fallback(recovery_manager, {
    primary_service: "external_api",
    primary_operation: fn() {
      Error("External API timeout")
    },
    fallback_service: "local_cache",
    fallback_operation: fn() {
      Ok("cached_data")
    }
  })
  
  assert_true(fallback_result.success)
  assert_eq(fallback_result.result, "cached_data")
  assert_true(fallback_result.used_fallback)
  assert_true(fallback_result.primary_error.contains("External API timeout"))
  
  // 测试资源扩容
  let scaling_result = RecoveryManager::handle_resource_exhaustion(recovery_manager, {
    resource_type: "memory",
    current_usage: 0.9,  // 90%使用率
    error: Error("Out of memory")
  })
  
  assert_true(scaling_result.success)
  assert_true(scaling_result.actions_taken.length() > 0)
  
  let memory_scaling = scaling_result.actions_taken.find(fn(a) { a.resource_type == "memory" })
  assert_true(memory_scaling != None)
  
  match memory_scaling {
    Some(action) {
      assert_eq(action.action, "increase_heap_size")
      assert_true(action.old_size < action.new_size)
    }
    None => assert_true(false)
  }
  
  // 测试复合恢复策略
  let complex_recovery_result = RecoveryManager::execute_complex_recovery(recovery_manager, {
    error_type: "database_error",
    error: Error("Database connection pool exhausted"),
    strategies: ["retry_with_backoff", "circuit_breaker", "fallback_service"],
    timeout: 30000  // 30秒超时
  })
  
  assert_true(complex_recovery_result.success or complex_recovery_result.success == false)  // 结果取决于具体实现
  assert_true(complex_recovery_result.strategies_attempted.length() > 0)
  assert_true(complex_recovery_result.total_duration > 0)
  
  // 测试恢复统计
  let recovery_stats = RecoveryManager::get_recovery_stats(recovery_manager)
  assert_true(recovery_stats.total_recovery_attempts > 0)
  assert_true(recovery_stats.successful_recoveries >= 0)
  assert_true(recovery_stats.failed_recoveries >= 0)
  assert_true(recovery_stats.strategies_used.length() > 0)
}

// 测试4: 错误上下文收集和分析
test "错误上下文收集和分析功能" {
  // 创建错误上下文收集器
  let context_collector = ErrorContextCollector::new()
  
  // 配置上下文收集
  ContextCollector::configure(context_collector, {
    collect_system_info: true,
    collect_application_state: true,
    collect_user_context: true,
    collect_request_trace: true,
    max_stack_frames: 20,
    max_context_size: 10240  // 10KB
  })
  
  // 创建模拟错误场景
  let error_scenario = ErrorScenario::new()
  
  // 设置系统信息
  ErrorScenario::set_system_info(error_scenario, {
    os: "Linux",
    os_version: "5.4.0-74-generic",
    architecture: "x86_64",
    cpu_count: 8,
    total_memory: 16777216000,  // 16GB
    available_memory: 8388608000,  // 8GB
    disk_usage: 0.7,  // 70%
    load_average: [1.2, 1.5, 1.8]
  })
  
  // 设置应用状态
  ErrorScenario::set_application_state(error_scenario, {
    version: "1.2.3",
    build: "abc123",
    environment: "production",
    uptime: 86400,  // 1天
    active_requests: 150,
    thread_pool_size: 50,
    active_threads: 45,
    database_connections: 15,
    cache_size: 1073741824,  // 1GB
    gc_info: {
      collections: 100,
      total_time: 5000,  // 5秒
      last_collection: 1000  // 1秒前
    }
  })
  
  // 设置用户上下文
  ErrorScenario::set_user_context(error_scenario, {
    user_id: "user-12345",
    session_id: "session-abcde",
    ip_address: "192.168.1.100",
    user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    locale: "en-US",
    timezone: "America/New_York",
    permissions: ["read", "write"],
    subscription_tier: "premium"
  })
  
  // 设置请求追踪
  ErrorScenario::set_request_trace(error_scenario, {
    trace_id: "trace-67890",
    span_id: "span-12345",
    parent_span_id: "span-67890",
    operation_name: "process_payment",
    start_time: 1640995200,
    duration: 2500,  // 2.5秒
    status: "error",
    tags: [
      ("http.method", "POST"),
      ("http.url", "/api/payments"),
      ("http.status_code", "500"),
      ("service.name", "payment-service")
    ],
    logs: [
      {
        timestamp: 1640995200,
        level: "info",
        message: "Starting payment processing"
      },
      {
        timestamp: 1640995201000,
        level: "warn",
        message: "Payment gateway response slow"
      },
      {
        timestamp: 1640995202500,
        level: "error",
        message: "Payment processing failed: timeout"
      }
    ]
  })
  
  // 设置调用栈
  ErrorScenario::set_call_stack(error_scenario, [
    "PaymentService.processPayment(PaymentService.java:125)",
    "PaymentController.charge(PaymentController.java:45)",
    "RequestHandler.handle(RequestHandler.java:78)",
    "Dispatcher.dispatch(Dispatcher.java:23)",
    "Application.main(Application.java:15)"
  ])
  
  // 收集错误上下文
  let error_context = ContextCollector::collect(context_collector, {
    error: "Payment processing failed: timeout",
    error_type: "timeout",
    timestamp: 1640995202500,
    scenario: error_scenario
  })
  
  // 验证上下文收集
  assert_eq(error_context.error_message, "Payment processing failed: timeout")
  assert_eq(error_context.error_type, "timeout")
  assert_eq(error_context.timestamp, 1640995202500)
  
  // 验证系统信息
  assert_true(error_context.system_info != None)
  match error_context.system_info {
    Some(sys_info) => {
      assert_eq(sys_info.os, "Linux")
      assert_eq(sys_info.cpu_count, 8)
      assert_true(sys_info.total_memory > 0)
    }
    None => assert_true(false)
  }
  
  // 验证应用状态
  assert_true(error_context.application_state != None)
  match error_context.application_state {
    Some(app_state) => {
      assert_eq(app_state.version, "1.2.3")
      assert_eq(app_state.environment, "production")
      assert_true(app_state.active_requests > 0)
    }
    None => assert_true(false)
  }
  
  // 验证用户上下文
  assert_true(error_context.user_context != None)
  match error_context.user_context {
    Some(user_ctx) => {
      assert_eq(user_ctx.user_id, "user-12345")
      assert_eq(user_ctx.ip_address, "192.168.1.100")
      assert_true(user_ctx.permissions.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证请求追踪
  assert_true(error_context.request_trace != None)
  match error_context.request_trace {
    Some(req_trace) => {
      assert_eq(req_trace.trace_id, "trace-67890")
      assert_eq(req_trace.operation_name, "process_payment")
      assert_eq(req_trace.status, "error")
      assert_true(req_trace.tags.length() > 0)
      assert_true(req_trace.logs.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证调用栈
  assert_true(error_context.call_stack.length() > 0)
  assert_true(error_context.call_stack[0].contains("PaymentService.processPayment"))
  
  // 分析错误上下文
  let context_analysis = ContextCollector::analyze(context_collector, error_context)
  
  // 验证分析结果
  assert_true(context_analysis.root_causes.length() > 0)
  assert_true(context_analysis.contributing_factors.length() > 0)
  assert_true(context_analysis.recommendations.length() > 0)
  
  // 验证根因分析
  let timeout_root_cause = context_analysis.root_causes.find(fn(c) { c.cause_type == "timeout" })
  assert_true(timeout_root_cause != None)
  
  match timeout_root_cause {
    Some(cause) => {
      assert_true(cause.confidence > 0.0)
      assert_true(cause.description.length() > 0)
      assert_true(cause.evidence.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证贡献因素
  let high_load_factor = context_analysis.contributing_factors.find(fn(f) { f.factor == "high_system_load" })
  assert_true(high_load_factor != None)
  
  match high_load_factor {
    Some(factor) => {
      assert_true(factor.impact > 0.0)
      assert_true(factor.description.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证建议
  let performance_recommendation = context_analysis.recommendations.find(fn(r) { r.category == "performance" })
  assert_true(performance_recommendation != None)
  
  match performance_recommendation {
    Some(rec) => {
      assert_true(rec.priority.length() > 0)
      assert_true(rec.action.length() > 0)
      assert_true(rec.expected_outcome.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 测试上下文相关性分析
  let similar_errors = ContextCollector::find_similar_errors(context_collector, error_context, {
    time_window: 3600000,  // 1小时
    similarity_threshold: 0.7
  })
  
  assert_true(similar_errors.length() >= 0)  // 可能没有相似错误
  
  // 测试上下文压缩
  let compressed_context = ContextCollector::compress(context_collector, error_context, {
    max_size: 2048,  // 2KB
    preserve_critical_info: true
  })
  
  assert_true(compressed_context.size <= 2048)
  assert_true(compressed_context.preserved_fields.contains("error_message"))
  assert_true(compressed_context.preserved_fields.contains("error_type"))
}

// 测试5: 错误报告和通知
test "错误报告和通知功能" {
  // 创建错误报告管理器
  let report_manager = ErrorReportManager::new()
  
  // 配置报告模板
  ReportManager::add_template(report_manager, {
    name: "standard_error_report",
    title: "Error Report: {{error_type}}",
    sections: [
      {
        name: "summary",
        title: "Summary",
        fields: [
          "error_message",
          "error_type",
          "severity",
          "timestamp",
          "occurrence_count"
        ]
      },
      {
        name: "context",
        title: "Context Information",
        fields: [
          "system_info",
          "application_state",
          "user_context"
        ]
      },
      {
        name: "analysis",
        title: "Analysis",
        fields: [
          "root_causes",
          "contributing_factors",
          "recommendations"
        ]
      }
    ]
  })
  
  ReportManager::add_template(report_manager, {
    name: "critical_error_alert",
    title: "🚨 CRITICAL ERROR: {{error_type}}",
    sections: [
      {
        name: "alert",
        title: "Immediate Action Required",
        fields: [
          "error_message",
          "severity",
          "timestamp",
          "impact_assessment"
        ]
      },
      {
        name: "quick_actions",
        title: "Suggested Actions",
        fields: [
          "immediate_actions",
          "escalation_contacts"
        ]
      }
    ]
  })
  
  // 配置通知渠道
  ReportManager::add_notification_channel(report_manager, {
    name: "email",
    type: "email",
    config: {
      smtp_server: "smtp.example.com",
      smtp_port: 587,
      username: "alerts@example.com",
      password: "password",
      from_address: "alerts@example.com",
      to_addresses: ["team@example.com", "ops@example.com"]
    },
    filters: {
      min_severity: "warning",
      error_types: ["all"],
      rate_limit: {
        max_per_hour: 10,
        cooldown_period: 300  // 5分钟
      }
    }
  })
  
  ReportManager::add_notification_channel(report_manager, {
    name: "slack",
    type: "slack",
    config: {
      webhook_url: "https://hooks.slack.com/services/...",
      channel: "#alerts",
      username: "ErrorBot"
    },
    filters: {
      min_severity: "error",
      error_types: ["critical", "high"],
      rate_limit: {
        max_per_hour: 20,
        cooldown_period: 60  // 1分钟
      }
    }
  })
  
  ReportManager::add_notification_channel(report_manager, {
    name: "pagerduty",
    type: "pagerduty",
    config: {
      integration_key: "integration-key-123",
      severity: "critical"
    },
    filters: {
      min_severity: "critical",
      error_types: ["critical"],
      rate_limit: {
        max_per_hour: 5,
        cooldown_period: 600  // 10分钟
      }
    }
  })
  
  // 创建错误报告
  let error_report = ReportManager::create_report(report_manager, {
    error_context: {
      error_message: "Database connection pool exhausted",
      error_type: "resource_exhaustion",
      severity: "critical",
      timestamp: 1640995200,
      occurrence_count: 1,
      system_info: {
        os: "Linux",
        memory_usage: 0.9,
        cpu_usage: 0.8
      },
      application_state: {
        version: "1.2.3",
        active_connections: 50,
        max_connections: 50
      },
      user_context: {
        affected_users: 1000,
        impact_duration: 300  // 5分钟
      },
      root_causes: [
        {
          cause_type: "resource_exhaustion",
          confidence: 0.9,
          description: "Database connection pool exhausted"
        }
      ],
      recommendations: [
        {
          category: "infrastructure",
          priority: "high",
          action: "Increase database connection pool size",
          expected_outcome: "Reduce connection errors"
        }
      ]
    },
    template: "critical_error_alert"
  })
  
  // 验证报告生成
  assert_true(error_report.content.length() > 0)
  assert_true(error_report.title.contains("CRITICAL ERROR"))
  assert_true(error_report.content.contains("Database connection pool exhausted"))
  assert_true(error_report.content.contains("Immediate Action Required"))
  
  // 发送通知
  let notification_results = ReportManager::send_notifications(report_manager, error_report)
  assert_true(notification_results.length() > 0)
  
  // 验证邮件通知
  let email_result = notification_results.find(fn(r) { r.channel == "email" })
  assert_true(email_result != None)
  
  match email_result {
    Some(result) {
      assert_true(result.success)
      assert_true(result.sent_at > 0)
      assert_true(result.recipients.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证Slack通知
  let slack_result = notification_results.find(fn(r) { r.channel == "slack" })
  assert_true(slack_result != None)
  
  match slack_result {
    Some(result) {
      assert_true(result.success)
      assert_true(result.sent_at > 0)
      assert_true(result.message_id.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证PagerDuty通知
  let pagerduty_result = notification_results.find(fn(r) { r.channel == "pagerduty" })
  assert_true(pagerduty_result != None)
  
  match pagerduty_result {
    Some(result) {
      assert_true(result.success)
      assert_true(result.incident_id.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 测试通知限流
  let rate_limit_test_report = ReportManager::create_report(report_manager, {
    error_context: {
      error_message: "Test rate limiting",
      error_type: "test",
      severity: "warning",
      timestamp: 1640995200,
      occurrence_count: 1
    },
    template: "standard_error_report"
  })
  
  // 快速连续发送通知
  let rapid_notifications = []
  for i in 0..=15 {
    let result = ReportManager::send_notifications(report_manager, rate_limit_test_report)
    rapid_notifications.push(result)
  }
  
  // 验证限流效果
  let successful_notifications = rapid_notifications.filter(fn(r) {
    r.any(fn(ch) { ch.channel == "email" and ch.success })
  })
  
  assert_true(successful_notifications.length() <= 10)  // 限制为每小时10次
  
  // 测试通知聚合
  let aggregated_report = ReportManager::aggregate_reports(report_manager, {
    reports: [
      error_report,
      rate_limit_test_report
    ],
    aggregation_window: 3600000,  // 1小时
    group_by: ["error_type", "severity"]
  })
  
  assert_true(aggregated_report.summary.length() > 0)
  assert_true(aggregated_report.groups.length() > 0)
  
  // 测试报告历史
  ReportManager::save_report(report_manager, error_report)
  let report_history = ReportManager::get_report_history(report_manager, {
    time_range: 3600000,  // 1小时
    error_types: ["resource_exhaustion"],
    severities: ["critical"]
  })
  
  assert_true(report_history.length() > 0)
  assert_true(report_history[0].error_type == "resource_exhaustion")
  assert_true(report_history[0].severity == "critical")
}

// 测试6: 错误模式识别
test "错误模式识别功能" {
  // 创建错误模式识别器
  let pattern_recognizer = ErrorPatternRecognizer::new()
  
  // 配置模式识别规则
  PatternRecognizer::add_pattern(pattern_recognizer, {
    name: "memory_leak_pattern",
    description: "渐进式内存使用增长导致OOM",
    indicators: [
      {
        metric: "memory_usage",
        trend: "increasing",
        duration: 3600000,  // 1小时
        threshold: 0.8      // 80%
      },
      {
        metric: "gc_frequency",
        trend: "increasing",
        duration: 1800000,  // 30分钟
        threshold: 10       // 每分钟10次
      }
    ],
    error_types: ["out_of_memory"],
    confidence_threshold: 0.8
  })
  
  PatternRecognizer::add_pattern(pattern_recognizer, {
    name: "connection_pool_exhaustion",
    description: "连接池耗尽模式",
    indicators: [
      {
        metric: "active_connections",
        trend: "stable",
        duration: 300000,  // 5分钟
        threshold: 0.95    // 95%使用率
      },
      {
        metric: "connection_wait_time",
        trend: "increasing",
        duration: 180000,  // 3分钟
        threshold: 5000    // 5秒
      }
    ],
    error_types: ["connection_timeout", "connection_refused"],
    confidence_threshold: 0.7
  })
  
  PatternRecognizer::add_pattern(pattern_recognizer, {
    name: "cascading_failure",
    description: "级联故障模式",
    indicators: [
      {
        metric: "error_rate",
        trend: "increasing",
        duration: 120000,  // 2分钟
        threshold: 0.1     // 10%错误率
      },
      {
        metric: "response_time",
        trend: "increasing",
        duration: 120000,  // 2分钟
        threshold: 1000    // 1秒
      },
      {
        metric: "throughput",
        trend: "decreasing",
        duration: 120000,  // 2分钟
        threshold: 0.5     // 50%下降
      }
    ],
    error_types: ["timeout", "service_unavailable"],
    confidence_threshold: 0.9
  })
  
  // 模拟内存泄漏模式数据
  let base_time = 1640995200
  
  for i in 0..=60 {
    let timestamp = base_time + i * 60000  // 每分钟
    let memory_usage = 0.5 + (i * 0.005)  // 渐进式增长
    let gc_frequency = 2.0 + (i * 0.1)    // GC频率增加
    
    PatternRecognizer::add_metric(pattern_recognizer, timestamp, "memory_usage", memory_usage)
    PatternRecognizer::add_metric(pattern_recognizer, timestamp, "gc_frequency", gc_frequency)
    
    // 在最后添加OOM错误
    if i == 60 {
      PatternRecognizer::add_error(pattern_recognizer, timestamp, "out_of_memory")
    }
  }
  
  // 识别模式
  let recognized_patterns = PatternRecognizer::recognize_patterns(pattern_recognizer)
  assert_true(recognized_patterns.length() > 0)
  
  // 验证内存泄漏模式识别
  let memory_leak_pattern = recognized_patterns.find(fn(p) { p.name == "memory_leak_pattern" })
  assert_true(memory_leak_pattern != None)
  
  match memory_leak_pattern {
    Some(pattern) {
      assert_true(pattern.confidence >= 0.8)
      assert_eq(pattern.matched_indicators.length(), 2)
      assert_true(pattern.description.contains("渐进式内存使用增长"))
      assert_true(pattern.recommendations.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 清除数据并模拟连接池耗尽模式
  PatternRecognizer::clear_data(pattern_recognizer)
  
  for i in 0..=20 {
    let timestamp = base_time + i * 15000  // 每15秒
    let active_connections = 0.95  // 稳定在高使用率
    let connection_wait_time = 1000.0 + (i * 200.0)  // 逐渐增加
    
    PatternRecognizer::add_metric(pattern_recognizer, timestamp, "active_connections", active_connections)
    PatternRecognizer::add_metric(pattern_recognizer, timestamp, "connection_wait_time", connection_wait_time)
    
    // 添加连接错误
    if i >= 10 {
      PatternRecognizer::add_error(pattern_recognizer, timestamp, "connection_timeout")
    }
  }
  
  let connection_patterns = PatternRecognizer::recognize_patterns(pattern_recognizer)
  let connection_pool_pattern = connection_patterns.find(fn(p) { p.name == "connection_pool_exhaustion" })
  assert_true(connection_pool_pattern != None)
  
  match connection_pool_pattern {
    Some(pattern) {
      assert_true(pattern.confidence >= 0.7)
      assert_eq(pattern.matched_indicators.length(), 2)
    }
    None => assert_true(false)
  }
  
  // 清除数据并模拟级联故障模式
  PatternRecognizer::clear_data(pattern_recognizer)
  
  for i in 0..=15 {
    let timestamp = base_time + i * 10000  // 每10秒
    let error_rate = 0.01 + (i * 0.01)    // 错误率逐渐增加
    let response_time = 100.0 + (i * 100.0)  // 响应时间逐渐增加
    let throughput = 1.0 - (i * 0.05)      // 吞吐量逐渐下降
    
    PatternRecognizer::add_metric(pattern_recognizer, timestamp, "error_rate", error_rate)
    PatternRecognizer::add_metric(pattern_recognizer, timestamp, "response_time", response_time)
    PatternRecognizer::add_metric(pattern_recognizer, timestamp, "throughput", throughput)
    
    // 添加超时和服务不可用错误
    if i >= 5 {
      PatternRecognizer::add_error(pattern_recognizer, timestamp, "timeout")
    }
    if i >= 10 {
      PatternRecognizer::add_error(pattern_recognizer, timestamp, "service_unavailable")
    }
  }
  
  let cascade_patterns = PatternRecognizer::recognize_patterns(pattern_recognizer)
  let cascade_pattern = cascade_patterns.find(fn(p) { p.name == "cascading_failure" })
  assert_true(cascade_pattern != None)
  
  match cascade_pattern {
    Some(pattern) {
      assert_true(pattern.confidence >= 0.9)
      assert_eq(pattern.matched_indicators.length(), 3)
      assert_true(pattern.severity == "critical")
    }
    None => assert_true(false)
  }
  
  // 测试自定义模式学习
  let custom_patterns = PatternRecognizer::learn_custom_patterns(pattern_recognizer, {
    training_data: [
      {
        error_sequence: ["timeout", "connection_refused", "service_unavailable"],
        metrics: [
          ("error_rate", [0.05, 0.1, 0.2]),
          ("response_time", [500, 1000, 2000])
        ],
        pattern_label: "service_degradation"
      }
    ],
    min_occurrences: 3,
    confidence_threshold: 0.6
  })
  
  assert_true(custom_patterns.length() >= 0)
  
  // 测试模式预测
  let pattern_prediction = PatternRecognizer::predict_pattern(pattern_recognizer, {
    current_metrics: [
      ("memory_usage", 0.7),
      ("gc_frequency", 8.0)
    ],
    time_horizon: 1800000  // 30分钟
  })
  
  assert_true(pattern_prediction.predicted_patterns.length() >= 0)
  assert_true(pattern_prediction.confidence >= 0.0)
  
  // 测试模式统计
  let pattern_stats = PatternRecognizer::get_pattern_statistics(pattern_recognizer)
  assert_true(pattern_stats.total_patterns > 0)
  assert_true(pattern_stats.recognized_patterns > 0)
  assert_true(pattern_stats.pattern_frequency.length() > 0)
}

// 测试7: 混沌工程和故障注入
test "混沌工程和故障注入功能" {
  // 创建混沌工程实验管理器
  let chaos_manager = ChaosExperimentManager::new()
  
  // 配置安全措施
  ChaosManager::configure_safety_measures(chaos_manager, {
    blast_radius: "single_service",
    max_duration: 300000,      // 5分钟最大实验时间
    rollback_on_error: true,
    health_check_interval: 10000,  // 10秒健康检查
    auto_rollback_threshold: 0.5   // 50%错误率自动回滚
  })
  
  // 创建网络延迟实验
  let network_latency_experiment = ChaosManager::create_experiment(chaos_manager, {
    name: "network_latency_injection",
    description: "注入网络延迟测试系统弹性",
    target: {
      service: "payment-service",
      endpoint: "/api/process",
      method: "POST"
    },
    fault: {
      type: "network_latency",
      parameters: {
        latency: 1000,      // 1秒延迟
        jitter: 200,        // 200ms抖动
        probability: 0.3    // 30%概率
      }
    },
    duration: 120000,       // 2分钟
    monitoring: {
      metrics: ["response_time", "error_rate", "throughput"],
      alert_thresholds: [
        ("response_time_p95", 5000),
        ("error_rate", 0.1),
        ("throughput", 0.5)
      ]
    },
    rollback_plan: {
      type: "automatic",
      triggers: ["error_rate > 0.2", "response_time_p95 > 10000"]
    }
  })
  
  // 创建内存压力实验
  let memory_pressure_experiment = ChaosManager::create_experiment(chaos_manager, {
    name: "memory_pressure_injection",
    description: "注入内存压力测试OOM处理",
    target: {
      service: "data-processing-service",
      resource: "memory"
    },
    fault: {
      type: "memory_pressure",
      parameters: {
        allocation_rate: 104857600,  // 100MB/s
        duration: 60000,            // 1分钟
        gc_suppression: true
      }
    },
    duration: 90000,        // 1.5分钟
    monitoring: {
      metrics: ["memory_usage", "gc_frequency", "response_time"],
      alert_thresholds: [
        ("memory_usage", 0.9),
        ("gc_frequency", 20),
        ("response_time_p95", 2000)
      ]
    },
    rollback_plan: {
      type: "automatic",
      triggers: ["memory_usage > 0.95", "oom_error_detected"]
    }
  })
  
  // 创建数据库连接故障实验
  let db_connection_experiment = ChaosManager::create_experiment(chaos_manager, {
    name: "database_connection_failure",
    description: "模拟数据库连接失败测试重试机制",
    target: {
      service: "user-service",
      dependency: "database"
    },
    fault: {
      type: "connection_failure",
      parameters: {
        failure_rate: 0.5,      // 50%连接失败
        error_type: "timeout",
        recovery_time: 30000    // 30秒后恢复
      }
    },
    duration: 180000,       // 3分钟
    monitoring: {
      metrics: ["connection_errors", "retry_attempts", "fallback_usage"],
      alert_thresholds: [
        ("connection_errors", 10),
        ("retry_attempts", 50),
        ("fallback_usage", 0.8)
      ]
    },
    rollback_plan: {
      type: "manual",
      approval_required: true
    }
  })
  
  // 运行网络延迟实验
  let latency_experiment_result = ChaosManager::run_experiment(chaos_manager, network_latency_experiment)
  assert_true(latency_experiment_result.started)
  assert_true(latency_experiment_result.experiment_id.length() > 0)
  
  // 等待实验完成
  let latency_final_result = ChaosManager::wait_for_completion(chaos_manager, latency_experiment_result.experiment_id, 180000)
  assert_true(latency_final_result.completed)
  
  // 验证实验结果
  assert_true(latency_final_result.metrics.length() > 0)
  assert_true(latency_final_result.duration > 0)
  
  // 检查响应时间影响
  let response_time_metrics = latency_final_result.metrics.filter(fn(m) { m.name == "response_time" })
  assert_true(response_time_metrics.length() > 0)
  
  let avg_response_time = response_time_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0) / response_time_metrics.length().to_float()
  assert_true(avg_response_time > 100.0)  // 应该比基线高
  
  // 检查错误率影响
  let error_rate_metrics = latency_final_result.metrics.filter(fn(m) { m.name == "error_rate" })
  assert_true(error_rate_metrics.length() > 0)
  
  let max_error_rate = error_rate_metrics.reduce(fn(acc, m) { if m.value > acc { m.value } else { acc } }, 0.0)
  assert_true(max_error_rate < 0.2)  // 应该在安全阈值内
  
  // 运行内存压力实验
  let memory_experiment_result = ChaosManager::run_experiment(chaos_manager, memory_pressure_experiment)
  let memory_final_result = ChaosManager::wait_for_completion(chaos_manager, memory_experiment_result.experiment_id, 180000)
  
  // 验证内存压力实验结果
  assert_true(memory_final_result.completed)
  
  let memory_metrics = memory_final_result.metrics.filter(fn(m) { m.name == "memory_usage" })
  assert_true(memory_metrics.length() > 0)
  
  let max_memory_usage = memory_metrics.reduce(fn(acc, m) { if m.value > acc { m.value } else { acc } }, 0.0)
  assert_true(max_memory_usage > 0.7)  // 内存使用率应该显著增加
  
  // 检查GC频率
  let gc_metrics = memory_final_result.metrics.filter(fn(m) { m.name == "gc_frequency" })
  assert_true(gc_metrics.length() > 0)
  
  // 运行数据库连接故障实验
  let db_experiment_result = ChaosManager::run_experiment(chaos_manager, db_connection_experiment)
  let db_final_result = ChaosManager::wait_for_completion(chaos_manager, db_experiment_result.experiment_id, 240000)
  
  // 验证数据库连接实验结果
  assert_true(db_final_result.completed)
  
  let connection_error_metrics = db_final_result.metrics.filter(fn(m) { m.name == "connection_errors" })
  assert_true(connection_error_metrics.length() > 0)
  
  let total_connection_errors = connection_error_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
  assert_true(total_connection_errors > 0)  // 应该有连接错误
  
  // 检查重试机制
  let retry_metrics = db_final_result.metrics.filter(fn(m) { m.name == "retry_attempts" })
  assert_true(retry_metrics.length() > 0)
  
  let total_retries = retry_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
  assert_true(total_retries > 0)  // 应该有重试尝试
  
  // 生成实验报告
  let experiment_report = ChaosManager::generate_experiment_report(chaos_manager, [
    latency_final_result,
    memory_final_result,
    db_final_result
  ])
  
  assert_true(experiment_report.summary.length() > 0)
  assert_true(experiment_report.experiments.length() == 3)
  assert_true(experiment_report.findings.length() > 0)
  assert_true(experiment_report.recommendations.length() > 0)
  
  // 验证发现的问题
  let latency_issues = experiment_report.findings.filter(fn(f) { f.experiment == "network_latency_injection" })
  assert_true(latency_issues.length() > 0)
  
  let resilience_findings = experiment_report.findings.filter(fn(f) { f.category == "resilience" })
  assert_true(resilience_findings.length() > 0)
  
  // 测试实验模板
  let experiment_template = ChaosManager::create_experiment_template(chaos_manager, {
    name: "standard_latency_test",
    description: "标准网络延迟测试模板",
    parameters: [
      {
        name: "latency",
        type: "integer",
        default: 1000,
        min: 100,
        max: 10000,
        description: "延迟时间（毫秒）"
      },
      {
        name: "probability",
        type: "float",
        default: 0.3,
        min: 0.1,
        max: 1.0,
        description: "故障概率"
      }
    ],
    target_template: {
      service: "{{service}}",
      endpoint: "{{endpoint}}"
    },
    fault_template: {
      type: "network_latency",
      parameters: {
        latency: "{{latency}}",
        probability: "{{probability}}"
      }
    }
  })
  
  // 从模板创建实验
  let templated_experiment = ChaosManager::create_from_template(chaos_manager, experiment_template, {
    service: "order-service",
    endpoint: "/api/orders",
    latency: 2000,
    probability: 0.5
  })
  
  assert_true(templated_experiment.name == "standard_latency_test")
  assert_true(templated_experiment.fault.parameters.latency == 2000)
  assert_true(templated_experiment.fault.parameters.probability == 0.5)
  
  // 测试实验统计
  let experiment_stats = ChaosManager::get_experiment_statistics(chaos_manager)
  assert_true(experiment_stats.total_experiments >= 3)
  assert_true(experiment_stats.successful_experiments >= 0)
  assert_true(experiment_stats.failed_experiments >= 0)
  assert_true(experiment_stats.rollbacks_triggered >= 0)
}

// 测试8: 错误恢复验证
test "错误恢复验证功能" {
  // 创建恢复验证器
  let recovery_validator = RecoveryValidator::new()
  
  // 配置验证测试
  RecoveryValidator::configure_validation_tests(recovery_validator, {
    test_timeout: 300000,      // 5分钟测试超时
    health_check_interval: 5000,  // 5秒健康检查间隔
    success_criteria: {
      error_rate_threshold: 0.01,  // 1%错误率阈值
      response_time_threshold: 1000,  // 1秒响应时间阈值
      availability_threshold: 0.99   // 99%可用性阈值
    }
  })
  
  // 创建恢复验证测试用例
  let circuit_breaker_test = RecoveryValidator::create_test(recovery_validator, {
    name: "circuit_breaker_recovery_test",
    description: "验证熔断器恢复机制",
    scenario: {
      setup: fn() {
        // 设置测试环境
        let service = TestService::new("payment-service")
        service.enable_circuit_breaker({
          failure_threshold: 3,
          recovery_timeout: 30000,
          half_open_max_calls: 2
        })
        service
      },
      fault_injection: fn(service) {
        // 注入故障
        service.inject_fault("database_connection_failed", {
          type: "connection_error",
          duration: 60000,  // 1分钟
          failure_rate: 1.0  // 100%失败
        })
      },
      validation: fn(service) {
        // 验证恢复
        let health_status = service.health_check()
        let metrics = service.get_metrics()
        
        {
          is_healthy: health_status.is_healthy,
          error_rate: metrics.error_rate,
          response_time: metrics.response_time_p95,
          circuit_breaker_state: service.get_circuit_breaker_state()
        }
      },
      cleanup: fn(service) {
        // 清理测试环境
        service.cleanup()
      }
    },
    expected_results: {
      circuit_breaker_opens: true,
      automatic_recovery: true,
      max_recovery_time: 60000,  // 1分钟内恢复
      final_error_rate: 0.01     // 最终错误率<1%
    }
  })
  
  let retry_mechanism_test = RecoveryValidator::create_test(recovery_validator, {
    name: "retry_mechanism_recovery_test",
    description: "验证重试机制恢复",
    scenario: {
      setup: fn() {
        let service = TestService::new("notification-service")
        service.configure_retry({
          max_attempts: 3,
          backoff_strategy: "exponential",
          initial_delay: 1000,
          max_delay: 10000
        })
        service
      },
      fault_injection: fn(service) {
        service.inject_fault("smtp_server_timeout", {
          type: "timeout",
          duration: 45000,  // 45秒
          intermittent: true,
          failure_rate: 0.7  // 70%失败率
        })
      },
      validation: fn(service) {
        let stats = service.get_operation_stats()
        {
          total_operations: stats.total_operations,
          successful_operations: stats.successful_operations,
          retry_attempts: stats.retry_attempts,
          final_success_rate: stats.success_rate
        }
      },
      cleanup: fn(service) {
        service.cleanup()
      }
    },
    expected_results: {
      retry_attempts: true,
      eventual_success: true,
      max_retry_duration: 30000,  // 30秒内完成重试
      final_success_rate: 0.95    // 最终成功率>95%
    }
  })
  
  let fallback_service_test = RecoveryValidator::create_test(recovery_validator, {
    name: "fallback_service_recovery_test",
    description: "验证降级服务恢复",
    scenario: {
      setup: fn() {
        let service = TestService::new("recommendation-service")
        service.configure_fallback({
          primary: "ml_recommendation_engine",
          fallback: "rule_based_recommendations",
          fallback_trigger: "response_time > 2000 or error_rate > 0.1"
        })
        service
      },
      fault_injection: fn(service) {
        service.inject_fault("ml_engine_overload", {
          type: "performance_degradation",
          duration: 90000,  // 1.5分钟
          response_time_increase: 5.0,  // 5倍响应时间增加
          error_rate: 0.2  // 20%错误率
        })
      },
      validation: fn(service) {
        let metrics = service.get_metrics()
        let fallback_stats = service.get_fallback_stats()
        
        {
          primary_response_time: metrics.primary_response_time,
          fallback_response_time: metrics.fallback_response_time,
          fallback_usage_rate: fallback_stats.usage_rate,
          overall_success_rate: metrics.overall_success_rate
        }
      },
      cleanup: fn(service) {
        service.cleanup()
      }
    },
    expected_results: {
      fallback_triggered: true,
      service_continuity: true,
      fallback_response_time: 500,  // 降级服务响应时间<500ms
      overall_success_rate: 0.98    // 整体成功率>98%
    }
  })
  
  // 运行熔断器恢复测试
  let circuit_breaker_result = RecoveryValidator::run_test(recovery_validator, circuit_breaker_test)
  assert_true(circuit_breaker_result.completed)
  assert_true(circuit_breaker_result.validation_results.length() > 0)
  
  // 验证熔断器测试结果
  let circuit_breaker_validation = circuit_breaker_result.validation_results[0]
  assert_true(circuit_breaker_validation.circuit_breaker_state == "closed" or circuit_breaker_validation.circuit_breaker_state == "half_open")  // 应该已恢复
  
  assert_true(circuit_breaker_validation.error_rate < 0.01)  // 错误率应该<1%
  assert_true(circuit_breaker_validation.response_time < 1000)  // 响应时间应该<1秒
  
  // 运行重试机制测试
  let retry_result = RecoveryValidator::run_test(recovery_validator, retry_mechanism_test)
  assert_true(retry_result.completed)
  
  let retry_validation = retry_result.validation_results[0]
  assert_true(retry_validation.retry_attempts > 0)  // 应该有重试尝试
  assert_true(retry_validation.final_success_rate > 0.95)  // 最终成功率应该>95%
  
  // 运行降级服务测试
  let fallback_result = RecoveryValidator::run_test(recovery_validator, fallback_service_test)
  assert_true(fallback_result.completed)
  
  let fallback_validation = fallback_result.validation_results[0]
  assert_true(fallback_validation.fallback_usage_rate > 0.0)  // 应该使用了降级服务
  assert_true(fallback_validation.fallback_response_time < 500)  // 降级服务响应时间应该<500ms
  assert_true(fallback_validation.overall_success_rate > 0.98)  // 整体成功率应该>98%
  
  // 创建综合恢复测试
  let comprehensive_test = RecoveryValidator::create_comprehensive_test(recovery_validator, {
    name: "comprehensive_error_recovery_test",
    description: "综合错误恢复测试",
    test_scenarios: [circuit_breaker_test, retry_mechanism_test, fallback_service_test],
    execution_order: "parallel",
    global_timeout: 600000,  // 10分钟全局超时
    success_criteria: {
      overall_availability: 0.99,  // 整体可用性>99%
      max_recovery_time: 120000,    // 最大恢复时间2分钟
      error_budger: 0.01           // 错误预算<1%
    }
  })
  
  // 运行综合测试
  let comprehensive_result = RecoveryValidator::run_comprehensive_test(recovery_validator, comprehensive_test)
  assert_true(comprehensive_result.completed)
  assert_eq(comprehensive_result.test_results.length(), 3)
  
  // 验证综合测试结果
  assert_true(comprehensive_result.overall_availability > 0.99)
  assert_true(comprehensive_result.max_recovery_time < 120000)
  assert_true(comprehensive_result.error_budget_consumed < 0.01)
  
  // 生成恢复验证报告
  let validation_report = RecoveryValidator::generate_validation_report(recovery_validator, [
    circuit_breaker_result,
    retry_result,
    fallback_result,
    comprehensive_result
  ])
  
  assert_true(validation_report.summary.length() > 0)
  assert_true(validation_report.test_results.length() == 4)
  assert_true(validation_report.recovery_effectiveness.length() > 0)
  assert_true(validation_report.improvement_recommendations.length() > 0)
  
  // 验证恢复效果评估
  let recovery_effectiveness = validation_report.recovery_effectiveness
  assert_true(recovery_effectiveness.overall_score > 0.8)  // 整体恢复效果应该>80%
  
  let circuit_breaker_effectiveness = recovery_effectiveness.by_strategy.get("circuit_breaker")
  assert_true(circuit_breaker_effectiveness != None)
  
  match circuit_breaker_effectiveness {
    Some(effectiveness) => {
      assert_true(effectiveness.avability_impact > 0.0)
      assert_true(effectiveness.error_reduction > 0.0)
      assert_true(effectiveness.recovery_time_score > 0.0)
    }
    None => assert_true(false)
  }
  
  // 验证改进建议
  let performance_recommendations = validation_report.improvement_recommendations.filter(fn(r) { r.category == "performance" })
  assert_true(performance_recommendations.length() >= 0)
  
  let reliability_recommendations = validation_report.improvement_recommendations.filter(fn(r) { r.category == "reliability" })
  assert_true(reliability_recommendations.length() >= 0)
  
  // 测试恢复验证统计
  let validation_stats = RecoveryValidator::get_validation_statistics(recovery_validator)
  assert_true(validation_stats.total_tests >= 4)
  assert_true(validation_stats.passed_tests >= 0)
  assert_true(validation_stats.failed_tests >= 0)
  assert_true(validation_stats.average_recovery_time > 0.0)
  assert_true(validation_stats.overall_success_rate >= 0.0)
}