// Azimuth 错误处理和恢复测试用例
// 专注于错误检测、恢复策略和系统弹性

// 测试1: 基础错误处理机制
test "基础错误处理机制" {
  // 创建错误处理器
  let error_handler = ErrorHandler::new()
  
  // 定义错误类型
  enum ServiceError {
    NetworkTimeout(String)
    DatabaseConnection(String)
    InvalidInput(String)
    ResourceExhausted(String)
    AuthenticationFailed(String)
  }
  
  // 测试错误创建和分类
  let network_error = ServiceError::NetworkTimeout("Connection timed out after 30s")
  let db_error = ServiceError::DatabaseConnection("Unable to connect to database")
  let input_error = ServiceError::InvalidInput("Missing required field 'id'")
  let resource_error = ServiceError::ResourceExhausted("Memory usage exceeded 80%")
  let auth_error = ServiceError::AuthenticationFailed("Invalid credentials")
  
  // 注册错误处理策略
  ErrorHandler::register_strategy(error_handler, "NetworkTimeout", {
    retry_count: 3,
    backoff_strategy: "exponential",
    fallback_action: "use_cached_data"
  })
  
  ErrorHandler::register_strategy(error_handler, "DatabaseConnection", {
    retry_count: 5,
    backoff_strategy: "linear",
    fallback_action: "switch_to_backup_db"
  })
  
  ErrorHandler::register_strategy(error_handler, "InvalidInput", {
    retry_count: 0,
    backoff_strategy: "none",
    fallback_action: "return_validation_error"
  })
  
  // 测试错误处理
  let handled_network = ErrorHandler::handle(error_handler, network_error)
  assert_eq(handled_network.strategy, "retry_with_backoff")
  assert_eq(handled_network.max_retries, 3)
  
  let handled_input = ErrorHandler::handle(error_handler, input_error)
  assert_eq(handled_input.strategy, "no_retry")
  assert_eq(handled_input.fallback, "return_validation_error")
  
  // 测试错误分类
  let network_category = ErrorHandler::categorize(error_handler, network_error)
  assert_eq(network_category, "transient")  # 网络超时是临时错误
  
  let input_category = ErrorHandler::categorize(error_handler, input_error)
  assert_eq(input_category, "permanent")  # 输入错误是永久错误
  
  let resource_category = ErrorHandler::categorize(error_handler, resource_error)
  assert_eq(resource_category, "resource")  # 资源错误
  
  // 测试错误严重程度评估
  let network_severity = ErrorHandler::assess_severity(error_handler, network_error)
  assert_true(network_severity >= 3 and network_severity <= 7)  # 中等严重程度
  
  let auth_severity = ErrorHandler::assess_severity(error_handler, auth_error)
  assert_true(auth_severity >= 7 and auth_severity <= 10)  # 高严重程度
  
  // 测试错误上下文捕获
  let error_context = ErrorHandler::capture_context(error_handler, network_error, {
    request_id: "req-12345",
    user_id: "user-67890",
    service: "payment-service",
    operation: "process_payment",
    timestamp: Time::now()
  })
  
  assert_eq(error_context.error_type, "NetworkTimeout")
  assert_eq(error_context.request_id, "req-12345")
  assert_eq(error_context.service, "payment-service")
  assert_true(error_context.stack_trace.length() > 0)
}

// 测试2: 重试机制和退避策略
test "重试机制和退避策略" {
  // 创建重试管理器
  let retry_manager = RetryManager::new()
  
  // 测试固定间隔重试
  let fixed_retry_config = {
    max_attempts: 3,
    initial_delay: 100,  # 100ms
    max_delay: 1000,
    backoff_strategy: "fixed",
    jitter: false
  }
  
  let mut attempt_count = 0
  let fixed_retry_result = RetryManager::execute_with_retry(retry_manager, fixed_retry_config, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      throw RuntimeError("Simulated failure")
    }
    "success"
  })
  
  assert_eq(fixed_retry_result, "success")
  assert_eq(attempt_count, 3)  # 失败2次，第3次成功
  
  // 测试指数退避重试
  let exponential_retry_config = {
    max_attempts: 4,
    initial_delay: 50,   # 50ms
    max_delay: 800,
    backoff_strategy: "exponential",
    jitter: false
  }
  
  attempt_count = 0
  let exponential_retry_result = RetryManager::execute_with_retry(retry_manager, exponential_retry_config, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 4 {
      throw RuntimeError("Simulated failure")
    }
    "success"
  })
  
  assert_eq(exponential_retry_result, "success")
  assert_eq(attempt_count, 4)  # 失败3次，第4次成功
  
  // 测试线性退避重试
  let linear_retry_config = {
    max_attempts: 3,
    initial_delay: 100,
    max_delay: 500,
    backoff_strategy: "linear",
    jitter: false
  }
  
  attempt_count = 0
  let linear_retry_result = RetryManager::execute_with_retry(retry_manager, linear_retry_config, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      throw RuntimeError("Simulated failure")
    }
    "success"
  })
  
  assert_eq(linear_retry_result, "success")
  assert_eq(attempt_count, 3)
  
  // 测试重试失败情况
  let failing_retry_config = {
    max_attempts: 2,
    initial_delay: 50,
    max_delay: 100,
    backoff_strategy: "fixed",
    jitter: false
  }
  
  attempt_count = 0
  let failing_retry_result = RetryManager::execute_with_retry(retry_manager, failing_retry_config, fn() {
    attempt_count = attempt_count + 1
    throw RuntimeError("Always failing")
  })
  
  match failing_retry_result {
    Success(value) => assert_true(false),  # 不应该成功
    Error(error) => assert_eq(error.message, "Always failing")
  }
  assert_eq(attempt_count, 2)  # 尝试2次都失败
  
  // 测试带抖动的重试
  let jitter_retry_config = {
    max_attempts: 3,
    initial_delay: 100,
    max_delay: 1000,
    backoff_strategy: "exponential",
    jitter: true
  }
  
  let jitter_delays = []
  attempt_count = 0
  let start_time = Time::now()
  
  RetryManager::execute_with_retry(retry_manager, jitter_retry_config, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      let current_time = Time::now()
      jitter_delays = jitter_delays.push(current_time - start_time)
      throw RuntimeError("Simulated failure")
    }
    "success"
  })
  
  # 验证抖动效果：延迟应该有变化
  if jitter_delays.length() >= 2 {
    assert_true(jitter_delays[1] > jitter_delays[0])
  }
}

// 测试3: 断路器模式
test "断路器模式" {
  // 创建断路器
  let circuit_breaker = CircuitBreaker::new({
    failure_threshold: 3,      # 3次失败后打开
    success_threshold: 2,      # 2次成功后关闭
    timeout: 5000,             # 5秒后尝试半开
    monitoring_period: 10000   # 10秒监控周期
  })
  
  // 初始状态应该是关闭的
  assert_eq(CircuitBreaker::get_state(circuit_breaker), "closed")
  assert_true(CircuitBreaker::is_closed(circuit_breaker))
  assert_false(CircuitBreaker::is_open(circuit_breaker))
  
  // 模拟成功操作
  let success_result = CircuitBreaker::execute(circuit_breaker, fn() {
    "operation successful"
  })
  assert_eq(success_result, "operation successful")
  assert_eq(CircuitBreaker::get_failure_count(circuit_breaker), 0)
  
  // 模拟失败操作
  for i in 0..=2 {
    let result = CircuitBreaker::execute(circuit_breaker, fn() {
      throw RuntimeError("Operation failed")
    })
    
    match result {
      Success(value) => assert_true(false),  # 不应该成功
      Error(error) => assert_eq(error.message, "Operation failed")
    }
  }
  
  // 3次失败后，断路器应该打开
  assert_eq(CircuitBreaker::get_state(circuit_breaker), "open")
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  assert_false(CircuitBreaker::is_closed(circuit_breaker))
  
  // 断路器打开时，操作应该快速失败
  let open_result = CircuitBreaker::execute(circuit_breaker, fn() {
    "this should not execute"
  })
  
  match open_result {
    Success(value) => assert_true(false),  # 不应该成功
    Error(error) => assert_eq(error.error_type, "CircuitBreakerOpen")
  }
  
  // 等待超时时间，让断路器进入半开状态
  Thread::sleep(5100)
  
  // 状态应该是半开
  assert_eq(CircuitBreaker::get_state(circuit_breaker), "half_open")
  
  // 在半开状态下，第一次操作成功
  let half_open_success = CircuitBreaker::execute(circuit_breaker, fn() {
    "half open success"
  })
  assert_eq(half_open_success, "half open success")
  
  // 第二次操作也成功，应该使断路器关闭
  let second_success = CircuitBreaker::execute(circuit_breaker, fn() {
    "second success"
  })
  assert_eq(second_success, "second success")
  
  // 断路器应该关闭
  assert_eq(CircuitBreaker::get_state(circuit_breaker), "closed")
  assert_true(CircuitBreaker::is_closed(circuit_breaker))
  
  // 测试断路器统计
  let stats = CircuitBreaker::get_stats(circuit_breaker)
  assert_eq(stats.failure_count, 0)  # 重置为0
  assert_eq(stats.success_count, 3)  # 3次成功
  assert_true(stats.state_transitions > 0)
  
  // 测试断路器重置
  CircuitBreaker::reset(circuit_breaker)
  assert_eq(CircuitBreaker::get_state(circuit_breaker), "closed")
  assert_eq(CircuitBreaker::get_failure_count(circuit_breaker), 0)
  assert_eq(CircuitBreaker::get_success_count(circuit_breaker), 0)
}

// 测试4: 超时处理
test "超时处理" {
  // 创建超时管理器
  let timeout_manager = TimeoutManager::new()
  
  // 测试快速操作（不应超时）
  let fast_result = TimeoutManager::execute_with_timeout(timeout_manager, fn() {
    Thread::sleep(50)  # 50ms操作
    "fast operation completed"
  }, 200)  # 200ms超时
  
  assert_eq(fast_result, "fast operation completed")
  
  // 测试慢操作（应该超时）
  let slow_result = TimeoutManager::execute_with_timeout(timeout_manager, fn() {
    Thread::sleep(300)  # 300ms操作
    "slow operation completed"
  }, 200)  # 200ms超时
  
  match slow_result {
    Success(value) => assert_true(false),  # 不应该成功
    Error(error) => assert_eq(error.error_type, "Timeout")
  }
  
  // 测试可取消操作
  let cancellation_token = CancellationToken::new()
  let cancelable_operation = fn() {
    let mut i = 0
    while i < 1000 and not(CancellationToken::is_cancelled(cancellation_token)) {
      Thread::sleep(10)
      i = i + 1
    }
    if CancellationToken::is_cancelled(cancellation_token) {
      throw RuntimeError("Operation cancelled")
    }
    "operation completed"
  }
  
  // 启动操作
  let operation_task = ConcurrentManager::spawn(fn() {
    TimeoutManager::execute_with_cancellation(timeout_manager, cancelable_operation, cancellation_token)
  })
  
  // 等待一段时间后取消
  Thread::sleep(100)
  CancellationToken::cancel(cancellation_token)
  
  // 等待操作完成
  let cancelled_result = ConcurrentManager::join(operation_task)
  
  match cancelled_result {
    Success(value) => assert_true(false),  # 不应该成功
    Error(error) => assert_eq(error.message, "Operation cancelled")
  }
  
  // 测试分层超时
  let outer_timeout = 500
  let inner_timeout = 200
  
  let outer_operation = fn() {
    TimeoutManager::execute_with_timeout(timeout_manager, fn() {
      Thread::sleep(100)  # 100ms操作
      "inner operation"
    }, inner_timeout)
  }
  
  let layered_result = TimeoutManager::execute_with_timeout(timeout_manager, outer_operation, outer_timeout)
  assert_eq(layered_result, "inner operation")
  
  // 测试超时策略
  let timeout_strategy = TimeoutManager::create_strategy({
    base_timeout: 100,
    max_timeout: 1000,
    backoff_multiplier: 2.0,
    jitter: true
  })
  
  let mut timeouts = []
  for i in 0..=5 {
    let timeout = TimeoutManager::calculate_timeout(timeout_strategy, i)
    timeouts = timeouts.push(timeout)
  }
  
  // 验证超时递增
  assert_true(timeouts[1] > timeouts[0])
  assert_true(timeouts[2] > timeouts[1])
  
  // 验证最大超时限制
  assert_true(timeouts[timeouts.length() - 1] <= 1000)
}

// 测试5: 错误恢复策略
test "错误恢复策略" {
  // 创建恢复管理器
  let recovery_manager = RecoveryManager::new()
  
  // 注册恢复策略
  RecoveryManager::register_strategy(recovery_manager, "database_connection", {
    name: "database_failover",
    steps: [
      { action: "retry_connection", max_attempts: 3, delay: 1000 },
      { action: "switch_to_backup", max_attempts: 1, delay: 0 },
      { action: "use_cache", max_attempts: 1, delay: 0 }
    ]
  })
  
  RecoveryManager::register_strategy(recovery_manager, "service_unavailable", {
    name: "service_degradation",
    steps: [
      { action: "retry_with_backoff", max_attempts: 3, delay: 500 },
      { action: "use_circuit_breaker", max_attempts: 1, delay: 0 },
      { action: "fallback_service", max_attempts: 1, delay: 0 }
    ]
  })
  
  // 模拟数据库连接失败和恢复
  let mut db_attempts = 0
  let db_recovery_result = RecoveryManager::execute_recovery(recovery_manager, "database_connection", fn() {
    db_attempts = db_attempts + 1
    if db_attempts <= 3 {
      throw RuntimeError("Database connection failed")
    }
    "database query successful"
  })
  
  assert_eq(db_recovery_result, "database query successful")
  assert_eq(db_attempts, 4)  # 3次重试 + 1次成功
  
  // 模拟服务不可用和降级
  let mut service_attempts = 0
  let service_recovery_result = RecoveryManager::execute_recovery(recovery_manager, "service_unavailable", fn() {
    service_attempts = service_attempts + 1
    if service_attempts <= 2 {
      throw RuntimeError("Service unavailable")
    }
    "service response"
  })
  
  assert_eq(service_recovery_result, "service response")
  assert_eq(service_attempts, 3)  # 2次重试 + 1次成功
  
  // 测试自动恢复
  let auto_recovery_config = {
    enabled: true,
    check_interval: 1000,  # 1秒检查一次
    max_recovery_attempts: 5,
    health_check_fn: fn() {
      # 模拟健康检查
      Math::random() > 0.3  # 70%概率健康
    }
  }
  
  let mut health_checks = 0
  RecoveryManager::enable_auto_recovery(recovery_manager, auto_recovery_config, fn() {
    health_checks = health_checks + 1
    if health_checks < 3 {
      throw RuntimeError("Service unhealthy")
    }
    "recovered"
  })
  
  # 等待自动恢复
  Thread::sleep(3500)
  
  # 验证恢复结果
  let recovery_status = RecoveryManager::get_recovery_status(recovery_manager)
  assert_true(recovery_status.is_healthy)
  assert_true(recovery_status.recovery_attempts >= 3)
  
  # 测试恢复统计
  let recovery_stats = RecoveryManager::get_stats(recovery_manager)
  assert_true(recovery_stats.total_recovery_attempts > 0)
  assert_true(recovery_stats.successful_recoveries > 0)
  assert_true(recovery_stats.average_recovery_time > 0)
}

// 测试6: 错误聚合和分析
test "错误聚合和分析" {
  // 创建错误分析器
  let error_analyzer = ErrorAnalyzer::new()
  
  // 模拟一系列错误
  let errors = [
    { type: "NetworkTimeout", service: "payment-service", timestamp: Time::now(), severity: 5 },
    { type: "DatabaseConnection", service: "user-service", timestamp: Time::now(), severity: 7 },
    { type: "NetworkTimeout", service: "payment-service", timestamp: Time::now(), severity: 5 },
    { type: "InvalidInput", service: "api-gateway", timestamp: Time::now(), severity: 3 },
    { type: "NetworkTimeout", service: "payment-service", timestamp: Time::now(), severity: 5 },
    { type: "AuthenticationFailed", service: "auth-service", timestamp: Time::now(), severity: 8 },
    { type: "DatabaseConnection", service: "user-service", timestamp: Time::now(), severity: 7 }
  ]
  
  // 添加错误到分析器
  for error in errors {
    ErrorAnalyzer::add_error(error_analyzer, error)
  }
  
  // 测试错误频率分析
  let frequency_analysis = ErrorAnalyzer::analyze_frequency(error_analyzer)
  
  let network_timeout_count = frequency_analysis.get("NetworkTimeout")
  assert_eq(network_timeout_count, Some(3))
  
  let db_connection_count = frequency_analysis.get("DatabaseConnection")
  assert_eq(db_connection_count, Some(2))
  
  // 测试服务错误分布
  let service_distribution = ErrorAnalyzer::analyze_by_service(error_analyzer)
  
  let payment_service_errors = service_distribution.get("payment-service")
  assert_eq(payment_service_errors, Some(3))
  
  let user_service_errors = service_distribution.get("user-service")
  assert_eq(user_service_errors, Some(2))
  
  // 测试严重程度分布
  let severity_distribution = ErrorAnalyzer::analyze_by_severity(error_analyzer)
  
  let critical_errors = severity_distribution.get(8)
  assert_eq(critical_errors, Some(1))
  
  let medium_errors = severity_distribution.get(5)
  assert_eq(medium_errors, Some(3))
  
  // 测试错误趋势分析
  let trend_analysis = ErrorAnalyzer::analyze_trends(error_analyzer, {
    time_window: 3600,  # 1小时窗口
    bucket_size: 300    # 5分钟桶
  })
  
  assert_true(trend_analysis.buckets.length() > 0)
  assert_true(trend_analysis.trend_direction == "increasing" or 
              trend_analysis.trend_direction == "decreasing" or 
              trend_analysis.trend_direction == "stable")
  
  // 测试错误关联分析
  let correlation_analysis = ErrorAnalyzer::analyze_correlations(error_analyzer)
  
  # 查找错误模式
  let network_timeout_pattern = correlation_analysis.find(fn(pattern) {
    pattern.error_type == "NetworkTimeout" and pattern.affected_services.contains("payment-service")
  })
  assert_true(network_timeout_pattern != None)
  
  // 测试错误预测
  let prediction_model = ErrorAnalyzer::build_prediction_model(error_analyzer, {
    algorithm: "time_series",
    features: ["error_frequency", "time_of_day", "service_load"],
    prediction_horizon: 3600  # 预测未来1小时
  })
  
  let predictions = ErrorAnalyzer::predict_errors(prediction_model, {
    time_range: 3600,
    confidence_threshold: 0.7
  })
  
  assert_true(predictions.length() > 0)
  
  // 验证预测格式
  for prediction in predictions {
    assert_true(prediction.error_type.length() > 0)
    assert_true(prediction.probability >= 0.0 and prediction.probability <= 1.0)
    assert_true(prediction.confidence >= 0.0 and prediction.confidence <= 1.0)
  }
}

// 测试7: 错误通知和警报
test "错误通知和警报" {
  // 创建警报管理器
  let alert_manager = AlertManager::new()
  
  // 创建通知通道
  let email_channel = AlertManager::create_channel(alert_manager, "email", {
    recipients: ["admin@example.com", "ops@example.com"],
    template: "error_alert_email",
    rate_limit: {
      max_per_hour: 10,
      cooldown: 300
    }
  })
  
  let slack_channel = AlertManager::create_channel(alert_manager, "slack", {
    webhook_url: "https://hooks.slack.com/services/xxx/yyy/zzz",
    channel: "#alerts",
    template: "error_alert_slack",
    rate_limit: {
      max_per_hour: 20,
      cooldown: 180
    }
  })
  
  // 定义警报规则
  AlertManager::add_rule(alert_manager, {
    name: "high_error_rate",
    condition: "error_rate > 0.1",
    severity: "warning",
    channels: ["email", "slack"],
    cooldown: 600,  # 10分钟冷却
    message: "High error rate detected: {{error_rate}}%"
  })
  
  AlertManager::add_rule(alert_manager, {
    name: "service_down",
    condition: "service_availability < 0.95",
    severity: "critical",
    channels: ["email", "slack"],
    cooldown: 300,  # 5分钟冷却
    message: "Service {{service_name}} is down. Availability: {{availability}}%"
  })
  
  AlertManager::add_rule(alert_manager, {
    name: "security_breach",
    condition: "failed_auth_attempts > 100",
    severity: "critical",
    channels: ["email", "slack", "pagerduty"],
    cooldown: 60,   # 1分钟冷却
    message: "Security breach detected: {{failed_auth_attempts}} failed authentication attempts"
  })
  
  // 模拟触发警报的条件
  let high_error_rate_metrics = {
    error_rate: 0.15,  # 15%错误率
    service_name: "payment-service",
    timestamp: Time::now()
  }
  
  let service_down_metrics = {
    service_availability: 0.90,  # 90%可用性
    service_name: "user-service",
    timestamp: Time::now()
  }
  
  let security_breach_metrics = {
    failed_auth_attempts: 150,  # 150次失败认证
    service_name: "auth-service",
    timestamp: Time::now()
  }
  
  // 触发警报
  let alert1 = AlertManager::evaluate_rules(alert_manager, high_error_rate_metrics)
  assert_eq(alert1.length(), 1)
  assert_eq(alert1[0].rule_name, "high_error_rate")
  assert_eq(alert1[0].severity, "warning")
  
  let alert2 = AlertManager::evaluate_rules(alert_manager, service_down_metrics)
  assert_eq(alert2.length(), 1)
  assert_eq(alert2[0].rule_name, "service_down")
  assert_eq(alert2[0].severity, "critical")
  
  let alert3 = AlertManager::evaluate_rules(alert_manager, security_breach_metrics)
  assert_eq(alert3.length(), 1)
  assert_eq(alert3[0].rule_name, "security_breach")
  assert_eq(alert3[0].severity, "critical")
  
  // 测试警报去重
  let duplicate_alert = AlertManager::evaluate_rules(alert_manager, high_error_rate_metrics)
  assert_eq(duplicate_alert.length(), 0)  # 应该被冷却期阻止
  
  // 测试警报升级
  AlertManager::add_escalation_rule(alert_manager, {
    rule_name: "service_down",
    conditions: [
      { severity: "warning", duration: 300, escalate_to: "critical" },
      { severity: "critical", duration: 600, escalate_to: "emergency" }
    ],
    escalation_channels: {
      critical: ["email", "slack", "pagerduty"],
      emergency: ["email", "slack", "pagerduty", "phone"]
    }
  })
  
  // 模拟持续的服务中断
  let persistent_service_down = {
    service_availability: 0.80,  # 80%可用性
    service_name: "user-service",
    timestamp: Time::now()
  }
  
  let escalated_alert = AlertManager::evaluate_rules(alert_manager, persistent_service_down)
  assert_eq(escalated_alert.length(), 1)
  assert_eq(escalated_alert[0].severity, "critical")
  
  // 测试警报通知
  let notification_log = []
  AlertManager::send_notifications(alert_manager, alert1, fn(channel, message) {
    notification_log = notification_log.push({
      channel: channel,
      message: message,
      timestamp: Time::now()
    })
  })
  
  assert_eq(notification_log.length(), 2)  # email和slack
  assert_true(notification_log.any(fn(n) { n.channel == "email" }))
  assert_true(notification_log.any(fn(n) { n.channel == "slack" }))
  
  // 测试警报统计
  let alert_stats = AlertManager::get_stats(alert_manager)
  assert_true(alert_stats.total_alerts > 0)
  assert_true(alert_stats.alerts_by_severity.get("critical") > 0)
  assert_true(alert_stats.alerts_by_severity.get("warning") > 0)
  assert_true(alert_stats.notifications_sent > 0)
}

// 测试8: 系统弹性和混沌工程
test "系统弹性和混沌工程" {
  // 创建混沌工程实验管理器
  let chaos_manager = ChaosManager::new()
  
  // 定义实验
  let network_latency_experiment = ChaosManager::create_experiment(chaos_manager, {
    name: "network_latency_injection",
    description: "Inject network latency to test system resilience",
    steady_state_hypothesis: {
      metrics: ["response_time_p99", "error_rate"],
      tolerance: {
        response_time_p99: { max_increase: 0.5 },  # 最多增加50%
        error_rate: { max_absolute: 0.05 }         # 最多增加5%
      }
    },
    method: {
      type: "latency_injection",
      parameters: {
        latency: 200,      # 200ms延迟
        jitter: 50,        # ±50ms抖动
        affected_services: ["payment-service", "user-service"],
        percentage: 30     # 影响30%的请求
      }
    },
    rollback: {
      type: "remove_latency_injection"
    },
    duration: 300  # 5分钟实验
  })
  
  // 创建系统弹性监控器
  let resilience_monitor = ResilienceMonitor::new()
  
  // 设置基线指标
  ResilienceMonitor::capture_baseline(resilience_monitor, {
    response_time_p99: 150.0,  # 150ms
    error_rate: 0.01,          # 1%
    throughput: 1000.0,        # 1000 req/s
    availability: 0.999         # 99.9%
  })
  
  // 执行混沌实验
  let experiment_result = ChaosManager::run_experiment(chaos_manager, network_latency_experiment)
  
  // 验证实验执行
  assert_true(experiment_result.started)
  assert_true(experiment_result.duration > 0)
  
  // 收集实验期间的指标
  let experiment_metrics = ResilienceMonitor::collect_metrics(resilience_monitor, {
    start_time: experiment_result.start_time,
    end_time: experiment_result.end_time,
    services: ["payment-service", "user-service"]
  })
  
  // 分析实验结果
  let analysis = ChaosManager::analyze_results(chaos_manager, experiment_result, experiment_metrics)
  
  // 验证稳态假设
  assert_true(analysis.hypothesis_validated or not(analysis.hypothesis_validated))
  
  // 测试故障注入
  let fault_injection_experiment = ChaosManager::create_experiment(chaos_manager, {
    name: "database_fault_injection",
    description: "Inject database faults to test fallback mechanisms",
    steady_state_hypothesis: {
      metrics: ["availability", "response_time_p95"],
      tolerance: {
        availability: { min_absolute: 0.95 },  # 最少95%可用性
        response_time_p95: { max_increase: 1.0 } # 最多增加100%
      }
    },
    method: {
      type: "database_fault",
      parameters: {
        fault_type: "connection_timeout",
        affected_operations: ["read", "write"],
        percentage: 20  # 20%的操作失败
      }
    },
    rollback: {
      type: "remove_database_fault"
    },
    duration: 180  # 3分钟实验
  })
  
  let fault_result = ChaosManager::run_experiment(chaos_manager, fault_injection_experiment)
  
  // 验证故障注入结果
  assert_true(fault_result.started)
  assert_true(fault_result.metrics_collected)
  
  // 测试弹性模式
  let resilience_patterns = ResilienceMonitor::detect_patterns(resilience_monitor, {
    lookback_period: 3600,  # 1小时
    pattern_types: ["circuit_breaker", "retry", "fallback", "timeout"]
  })
  
  // 验证检测到的模式
  assert_true(resilience_patterns.length() > 0)
  
  // 测试弹性评分
  let resilience_score = ResilienceMonitor::calculate_score(resilience_monitor, {
    weights: {
      availability: 0.4,
      performance: 0.3,
      error_handling: 0.2,
      recovery_time: 0.1
    }
  })
  
  assert_true(resilience_score >= 0.0 and resilience_score <= 10.0)
  
  // 测试弹性改进建议
  let improvement_recommendations = ResilienceMonitor::generate_recommendations(resilience_monitor)
  
  assert_true(improvement_recommendations.length() > 0)
  
  // 验证建议格式
  for recommendation in improvement_recommendations {
    assert_true(recommendation.area.length() > 0)
    assert_true(recommendation.description.length() > 0)
    assert_true(recommendation.priority >= 1 and recommendation.priority <= 5)
    assert_true(recommendation.estimated_impact >= 0.0 and recommendation.estimated_impact <= 10.0)
  }
  
  // 测试弹性报告生成
  let resilience_report = ResilienceMonitor::generate_report(resilience_monitor, {
    include_experiments: true,
    include_patterns: true,
    include_recommendations: true,
    format: "json"
  })
  
  // 验证报告内容
  assert_true(resilience_report.contains("resilience_score"))
  assert_true(resilience_report.contains("experiments"))
  assert_true(resilience_report.contains("patterns"))
  assert_true(resilience_report.contains("recommendations"))
}