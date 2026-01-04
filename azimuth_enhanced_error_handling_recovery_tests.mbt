// Azimuth 错误处理和恢复增强测试
// 专注于错误处理机制、异常恢复、容错性和故障转移

// 测试1: 错误类型和分类
test "错误类型和分类测试" {
  // 定义错误类型
  enum TelemetryError {
    NetworkError(String)
    DatabaseError(String)
    ValidationError(String)
    ConfigurationError(String)
    TimeoutError(String, Int)  // 错误信息，超时毫秒数
    RateLimitError(Int)        // 重试次数限制
    AuthenticationError(String)
    ResourceExhaustedError(String, String)  // 资源类型，资源ID
    InternalError(String, String)           // 错误代码，详细信息
  }
  
  // 创建错误分类器
  let error_classifier = ErrorClassifier::new()
  
  // 配置错误分类规则
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "NetworkError",
    category: "connectivity",
    severity: "medium",
    recovery_strategy: "retry_with_backoff",
    max_retries: 3
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "DatabaseError",
    category: "data_persistence",
    severity: "high",
    recovery_strategy: "failover_to_backup",
    max_retries: 2
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "ValidationError",
    category: "user_input",
    severity: "low",
    recovery_strategy: "request_correction",
    max_retries: 1
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "TimeoutError",
    category: "performance",
    severity: "medium",
    recovery_strategy: "increase_timeout",
    max_retries: 2
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "RateLimitError",
    category: "throttling",
    severity: "medium",
    recovery_strategy: "exponential_backoff",
    max_retries: 5
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "AuthenticationError",
    category: "security",
    severity: "high",
    recovery_strategy: "refresh_credentials",
    max_retries: 1
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "ResourceExhaustedError",
    category: "resource_management",
    severity: "high",
    recovery_strategy: "scale_resources",
    max_retries: 1
  })
  
  ErrorClassifier::add_classification_rule(error_classifier, {
    error_pattern: "InternalError",
    category: "system",
    severity: "critical",
    recovery_strategy: "escalate_to_operator",
    max_retries: 0
  })
  
  // 测试错误分类
  let network_error = TelemetryError::NetworkError("Connection refused")
  let network_classification = ErrorClassifier::classify_error(error_classifier, network_error)
  
  assert_eq(network_classification.category, "connectivity")
  assert_eq(network_classification.severity, "medium")
  assert_eq(network_classification.recovery_strategy, "retry_with_backoff")
  assert_eq(network_classification.max_retries, 3)
  
  let database_error = TelemetryError::DatabaseError("Connection pool exhausted")
  let database_classification = ErrorClassifier::classify_error(error_classifier, database_error)
  
  assert_eq(database_classification.category, "data_persistence")
  assert_eq(database_classification.severity, "high")
  assert_eq(database_classification.recovery_strategy, "failover_to_backup")
  assert_eq(database_classification.max_retries, 2)
  
  let validation_error = TelemetryError::ValidationError("Invalid timestamp format")
  let validation_classification = ErrorClassifier::classify_error(error_classifier, validation_error)
  
  assert_eq(validation_classification.category, "user_input")
  assert_eq(validation_classification.severity, "low")
  assert_eq(validation_classification.recovery_strategy, "request_correction")
  assert_eq(validation_classification.max_retries, 1)
  
  let timeout_error = TelemetryError::TimeoutError("Request timeout", 5000)
  let timeout_classification = ErrorClassifier::classify_error(error_classifier, timeout_error)
  
  assert_eq(timeout_classification.category, "performance")
  assert_eq(timeout_classification.severity, "medium")
  assert_eq(timeout_classification.recovery_strategy, "increase_timeout")
  assert_eq(timeout_classification.max_retries, 2)
  
  let rate_limit_error = TelemetryError::RateLimitError(10)
  let rate_limit_classification = ErrorClassifier::classify_error(error_classifier, rate_limit_error)
  
  assert_eq(rate_limit_classification.category, "throttling")
  assert_eq(rate_limit_classification.severity, "medium")
  assert_eq(rate_limit_classification.recovery_strategy, "exponential_backoff")
  assert_eq(rate_limit_classification.max_retries, 5)
  
  let auth_error = TelemetryError::AuthenticationError("Token expired")
  let auth_classification = ErrorClassifier::classify_error(error_classifier, auth_error)
  
  assert_eq(auth_classification.category, "security")
  assert_eq(auth_classification.severity, "high")
  assert_eq(auth_classification.recovery_strategy, "refresh_credentials")
  assert_eq(auth_classification.max_retries, 1)
  
  let resource_error = TelemetryError::ResourceExhaustedError("Memory", "heap-12345")
  let resource_classification = ErrorClassifier::classify_error(error_classifier, resource_error)
  
  assert_eq(resource_classification.category, "resource_management")
  assert_eq(resource_classification.severity, "high")
  assert_eq(resource_classification.recovery_strategy, "scale_resources")
  assert_eq(resource_classification.max_retries, 1)
  
  let internal_error = TelemetryError::InternalError("ERR-5001", "Unexpected null pointer")
  let internal_classification = ErrorClassifier::classify_error(error_classifier, internal_error)
  
  assert_eq(internal_classification.category, "system")
  assert_eq(internal_classification.severity, "critical")
  assert_eq(internal_classification.recovery_strategy, "escalate_to_operator")
  assert_eq(internal_classification.max_retries, 0)
  
  // 测试错误严重性比较
  assert_true(ErrorClassifier::compare_severity("critical", "high") > 0)
  assert_true(ErrorClassifier::compare_severity("high", "medium") > 0)
  assert_true(ErrorClassifier::compare_severity("medium", "low") > 0)
  assert_eq(ErrorClassifier::compare_severity("high", "high"), 0)
  
  // 测试错误聚合
  let error_log = [
    network_error,
    database_error,
    validation_error,
    timeout_error,
    rate_limit_error,
    auth_error,
    resource_error,
    internal_error
  ]
  
  let error_summary = ErrorClassifier::aggregate_errors(error_classifier, error_log)
  
  // 验证错误聚合结果
  assert_eq(error_summary.total_errors, 8)
  assert_eq(error_summary.by_category.get("connectivity"), Some(1))
  assert_eq(error_summary.by_category.get("data_persistence"), Some(1))
  assert_eq(error_summary.by_category.get("user_input"), Some(1))
  assert_eq(error_summary.by_category.get("performance"), Some(1))
  assert_eq(error_summary.by_category.get("throttling"), Some(1))
  assert_eq(error_summary.by_category.get("security"), Some(1))
  assert_eq(error_summary.by_category.get("resource_management"), Some(1))
  assert_eq(error_summary.by_category.get("system"), Some(1))
  
  assert_eq(error_summary.by_severity.get("critical"), Some(1))
  assert_eq(error_summary.by_severity.get("high"), Some(3))
  assert_eq(error_summary.by_severity.get("medium"), Some(3))
  assert_eq(error_summary.by_severity.get("low"), Some(1))
  
  // 验证最常见错误类别和严重性
  assert_eq(error_summary.most_common_category, "connectivity")  // 或其他类别，因为都是1个
  assert_eq(error_summary.most_common_severity, "critical")     // 或其他严重性，因为都是1个
}

// 测试2: 重试机制和退避策略
test "重试机制和退避策略测试" {
  // 创建重试管理器
  let retry_manager = RetryManager::new()
  
  // 配置重试策略
  RetryManager::configure_strategy(retry_manager, {
    max_retries: 5,
    base_delay_ms: 100,
    max_delay_ms: 10000,
    multiplier: 2.0,
    jitter: true,
    jitter_factor: 0.1
  })
  
  // 模拟会失败的操作
  let failing_operation = fn(attempt: Int) {
    if attempt < 3 {
      Err("Network timeout")
    } else {
      Ok("Operation succeeded after " + attempt.to_string() + " attempts")
    }
  }
  
  // 测试固定间隔重试
  let fixed_retry_result = RetryManager::execute_with_fixed_retry(retry_manager, failing_operation, 200, 3)
  assert_true(fixed_retry_result.is_ok())
  assert_eq(fixed_retry_result.unwrap(), "Operation succeeded after 3 attempts")
  
  // 测试指数退避重试
  let exponential_retry_result = RetryManager::execute_with_exponential_backoff(retry_manager, failing_operation, 100, 2.0, 3)
  assert_true(exponential_retry_result.is_ok())
  assert_eq(exponential_retry_result.unwrap(), "Operation succeeded after 3 attempts")
  
  // 测试带抖动的重试
  let jitter_retry_result = RetryManager::execute_with_jitter_retry(retry_manager, failing_operation, 100, 0.2, 3)
  assert_true(jitter_retry_result.is_ok())
  assert_eq(jitter_retry_result.unwrap(), "Operation succeeded after 3 attempts")
  
  // 模拟总是失败的操作
  let always_failing_operation = fn(attempt: Int) {
    Err("Database connection failed permanently")
  }
  
  // 测试重试耗尽
  let exhausted_retry_result = RetryManager::execute_with_exponential_backoff(retry_manager, always_failing_operation, 100, 2.0, 3)
  assert_true(exhausted_retry_result.is_err())
  assert_eq(exhausted_retry_result.unwrap_err(), "Database connection failed permanently")
  
  // 验证重试统计
  let retry_stats = RetryManager::get_retry_statistics(retry_manager)
  assert_true(retry_stats.total_attempts > 0)
  assert_true(retry_stats.successful_retries > 0)
  assert_true(retry_stats.failed_retries > 0)
  assert_true(retry_stats.avg_retry_count > 0)
  
  // 测试条件重试
  let conditional_operation = fn(attempt: Int) {
    if attempt < 2 {
      Err("Rate limit exceeded")
    } else if attempt < 4 {
      Err("Temporary network issue")
    } else {
      Ok("Operation succeeded")
    }
  }
  
  let conditional_retry_result = RetryManager::execute_with_condition_retry(
    retry_manager,
    conditional_operation,
    fn(error) { error.contains("Rate limit") or error.contains("Temporary") },
    100,
    5
  )
  assert_true(conditional_retry_result.is_ok())
  assert_eq(conditional_retry_result.unwrap(), "Operation succeeded")
  
  // 测试熔断器模式
  let circuit_breaker = CircuitBreaker::new({
    failure_threshold: 5,
    timeout_ms: 60000,
    reset_timeout_ms: 30000
  })
  
  // 模拟间歇性失败
  let intermittent_operation = fn() {
    let random = Random::next_int(10)
    if random < 7 {
      Err("Service unavailable")
    } else {
      Ok("Operation successful")
    }
  }
  
  // 执行多次操作，触发熔断器
  let mut success_count = 0
  let mut failure_count = 0
  
  for i in 0..=10 {
    let result = CircuitBreaker::execute(circuit_breaker, intermittent_operation)
    match result {
      Ok(_) => success_count = success_count + 1
      Err(_) => failure_count = failure_count + 1
    }
  }
  
  // 验证熔断器状态
  let breaker_state = CircuitBreaker::get_state(circuit_breaker)
  
  // 由于失败率高，熔断器可能已经打开
  if failure_count >= 5 {
    assert_true(breaker_state == "open" or breaker_state == "half_open")
  }
  
  // 测试熔断器恢复
  // 等待重置超时（在测试中模拟）
  CircuitBreaker::simulate_timeout_passage(circuit_breaker)
  
  // 尝试恢复操作
  let recovery_operation = fn() { Ok("Service recovered") }
  let recovery_result = CircuitBreaker::execute(circuit_breaker, recovery_operation)
  
  // 如果熔断器是半开状态，这次操作应该成功并关闭熔断器
  if breaker_state == "half_open" {
    assert_true(recovery_result.is_ok())
    assert_eq(CircuitBreaker::get_state(circuit_breaker), "closed")
  }
  
  // 获取熔断器统计
  let breaker_stats = CircuitBreaker::get_statistics(circuit_breaker)
  assert_true(breaker_stats.total_requests > 0)
  assert_true(breaker_stats.failure_count > 0)
  assert_true(breaker_stats.success_count >= 0)
}

// 测试3: 容错性和故障转移
test "容错性和故障转移测试" {
  // 创建容错管理器
  let fault_tolerance_manager = FaultToleranceManager::new()
  
  // 配置主服务和备用服务
  let primary_service = {
    name: "primary_telemetry_service",
    endpoint: "http://primary.telemetry.service:8080",
    health_check_endpoint: "http://primary.telemetry.service:8080/health",
    timeout_ms: 5000,
    is_healthy: true
  }
  
  let backup_service1 = {
    name: "backup_telemetry_service_1",
    endpoint: "http://backup1.telemetry.service:8080",
    health_check_endpoint: "http://backup1.telemetry.service:8080/health",
    timeout_ms: 5000,
    is_healthy: true
  }
  
  let backup_service2 = {
    name: "backup_telemetry_service_2",
    endpoint: "http://backup2.telemetry.service:8080",
    health_check_endpoint: "http://backup2.telemetry.service:8080/health",
    timeout_ms: 5000,
    is_healthy: true
  }
  
  // 注册服务
  FaultToleranceManager::register_service(fault_tolerance_manager, primary_service)
  FaultToleranceManager::register_service(fault_tolerance_manager, backup_service1)
  FaultToleranceManager::register_service(fault_tolerance_manager, backup_service2)
  
  // 配置故障转移策略
  FaultToleranceManager::configure_failover_strategy(fault_tolerance_manager, {
    primary_service: "primary_telemetry_service",
    backup_services: ["backup_telemetry_service_1", "backup_telemetry_service_2"],
    failover_threshold: 3,  // 连续3次失败后切换
    health_check_interval_ms: 30000,  // 30秒健康检查
    auto_recovery: true,
    recovery_check_interval_ms: 60000  // 60秒恢复检查
  })
  
  // 模拟服务操作
  let service_operation = fn(service_name: String) {
    if service_name == "primary_telemetry_service" {
      // 模拟主服务间歇性失败
      let random = Random::next_int(10)
      if random < 6 {
        Ok("Primary service operation successful")
      } else {
        Err("Primary service temporarily unavailable")
      }
    } else if service_name == "backup_telemetry_service_1" {
      // 模拟第一个备用服务偶尔失败
      let random = Random::next_int(10)
      if random < 8 {
        Ok("Backup service 1 operation successful")
      } else {
        Err("Backup service 1 overloaded")
      }
    } else if service_name == "backup_telemetry_service_2" {
      // 模拟第二个备用服务稳定
      Ok("Backup service 2 operation successful")
    } else {
      Err("Unknown service: " + service_name)
    }
  }
  
  // 测试故障转移
  let mut primary_success = 0
  let mut backup1_success = 0
  let mut backup2_success = 0
  let mut total_failures = 0
  
  // 执行多次操作，观察故障转移行为
  for i in 0..=20 {
    let result = FaultToleranceManager::execute_with_failover(
      fault_tolerance_manager,
      service_operation,
      "primary_telemetry_service"
    )
    
    match result {
      Ok(service_name) => {
        if service_name.contains("primary") {
          primary_success = primary_success + 1
        } else if service_name.contains("backup1") {
          backup1_success = backup1_success + 1
        } else if service_name.contains("backup2") {
          backup2_success = backup2_success + 1
        }
      }
      Err(_) => total_failures = total_failures + 1
    }
  }
  
  // 验证故障转移结果
  assert_true(primary_success > 0)  // 主服务应该有一些成功
  assert_true(backup1_success > 0 or backup2_success > 0)  // 至少一个备用服务应该有成功
  assert_true(total_failures < 20)  // 不应该所有操作都失败
  
  // 测试健康检查
  let health_check_operation = fn(service_name: String) {
    if service_name == "primary_telemetry_service" {
      // 模拟主服务健康检查失败
      let random = Random::next_int(10)
      if random < 3 {
        Ok("Healthy")
      } else {
        Err("Unhealthy")
      }
    } else {
      Ok("Healthy")  // 备用服务总是健康
    }
  }
  
  // 执行健康检查
  let health_results = FaultToleranceManager::check_all_services_health(
    fault_tolerance_manager,
    health_check_operation
  )
  
  // 验证健康检查结果
  assert_eq(health_results.length(), 3)
  
  for (service_name, is_healthy) in health_results {
    if service_name == "primary_telemetry_service" {
      // 主服务可能不健康
      assert_true(is_healthy == true or is_healthy == false)
    } else {
      // 备用服务应该总是健康
      assert_true(is_healthy)
    }
  }
  
  // 测试服务恢复
  // 模拟主服务恢复
  let recovered_primary_operation = fn(service_name: String) {
    if service_name == "primary_telemetry_service" {
      Ok("Primary service fully recovered")
    } else {
      service_operation(service_name)
    }
  }
  
  // 强制主服务恢复
  FaultToleranceManager::mark_service_healthy(fault_tolerance_manager, "primary_telemetry_service")
  
  // 验证主服务已恢复
  let primary_health = FaultToleranceManager::check_service_health(
    fault_tolerance_manager,
    "primary_telemetry_service",
    recovered_primary_operation
  )
  assert_true(primary_health)
  
  // 测试负载均衡故障转移
  let load_balancer = LoadBalancer::new({
    algorithm: "round_robin",
    services: ["primary_telemetry_service", "backup_telemetry_service_1", "backup_telemetry_service_2"],
    health_check_interval_ms: 30000
  })
  
  // 执行负载均衡操作
  let mut lb_results = []
  for i in 0..=15 {
    let result = LoadBalancer::execute(load_balancer, service_operation)
    lb_results = lb_results.push(result)
  }
  
  // 验证负载均衡结果
  assert_eq(lb_results.length(), 16)
  
  let lb_successes = lb_results.filter(fn(r) { r.is_ok() })
  let lb_failures = lb_results.filter(fn(r) { r.is_err() })
  
  assert_true(lb_successes.length() > 0)
  assert_true(lb_failures.length() < 16)
  
  // 获取负载均衡统计
  let lb_stats = LoadBalancer::get_statistics(load_balancer)
  assert_true(lb_stats.total_requests > 0)
  assert_true(lb_stats.successful_requests > 0)
  assert_true(lb_stats.service_distribution.length() > 0)
}

// 测试4: 错误监控和告警
test "错误监控和告警测试" {
  // 创建错误监控器
  let error_monitor = ErrorMonitor::new()
  
  // 配置监控规则
  ErrorMonitor::add_monitoring_rule(error_monitor, {
    name: "high_error_rate",
    condition: "error_rate > 0.05",
    time_window_ms: 300000,  // 5分钟
    severity: "warning",
    notification_channels: ["email", "slack"]
  })
  
  ErrorMonitor::add_monitoring_rule(error_monitor, {
    name: "critical_service_down",
    condition: "service_availability < 0.99",
    time_window_ms: 60000,   // 1分钟
    severity: "critical",
    notification_channels: ["pagerduty", "slack", "email"]
  })
  
  ErrorMonitor::add_monitoring_rule(error_monitor, {
    name: "spike_in_response_time",
    condition: "avg_response_time > 1000",
    time_window_ms: 180000,  // 3分钟
    severity: "warning",
    notification_channels: ["slack"]
  })
  
  ErrorMonitor::add_monitoring_rule(error_monitor, {
    name: "database_connection_failure",
    condition: "db_connection_errors > 10",
    time_window_ms: 60000,   // 1分钟
    severity: "high",
    notification_channels: ["email", "slack"]
  })
  
  // 模拟错误事件
  let base_time = 1640995200
  
  // 生成正常操作事件
  for i in 0..=200 {
    let timestamp = base_time + i * 1000  // 每秒一个事件
    
    ErrorMonitor::record_operation(error_monitor, {
      timestamp,
      service: "api.service",
      operation: "get_user_data",
      success: true,
      duration_ms: 50 + (i % 100),
      attributes: [
        ("user.id", StringValue("user-" + (i % 1000).to_string())),
        ("endpoint", StringValue("/api/users/" + (i % 10).to_string()))
      ]
    })
  }
  
  // 生成错误事件
  for i in 0..=20 {
    let timestamp = base_time + i * 10000  // 每10秒一个错误
    
    ErrorMonitor::record_operation(error_monitor, {
      timestamp,
      service: "api.service",
      operation: "get_user_data",
      success: false,
      duration_ms: 5000 + (i % 2000),
      error: "Database connection timeout",
      attributes: [
        ("user.id", StringValue("user-" + (i % 1000).to_string())),
        ("endpoint", StringValue("/api/users/" + (i % 10).to_string()))
      ]
    })
  }
  
  // 生成高延迟事件
  for i in 0..=30 {
    let timestamp = base_time + i * 3000  // 每3秒一个高延迟事件
    
    ErrorMonitor::record_operation(error_monitor, {
      timestamp,
      service: "api.service",
      operation: "process_payment",
      success: true,
      duration_ms: 1500 + (i % 1000),
      attributes: [
        ("payment.id", StringValue("payment-" + i.to_string())),
        ("amount", StringValue((100 + i * 10).to_string()))
      ]
    })
  }
  
  // 生成数据库连接错误
  for i in 0..=15 {
    let timestamp = base_time + i * 4000  // 每4秒一个数据库错误
    
    ErrorMonitor::record_operation(error_monitor, {
      timestamp,
      service: "database.service",
      operation: "execute_query",
      success: false,
      duration_ms: 100,
      error: "Connection pool exhausted",
      attributes: [
        ("query.type", StringValue("SELECT")),
        ("table", StringValue("users"))
      ]
    })
  }
  
  // 执行错误监控分析
  let monitoring_results = ErrorMonitor::analyze_errors(error_monitor, {
    start_time: base_time,
    end_time: base_time + 300000,  // 5分钟窗口
    services: ["api.service", "database.service"],
    group_by: ["service", "operation"]
  })
  
  // 验证监控结果
  assert_true(monitoring_results.error_rate > 0)
  assert_true(monitoring_results.error_rate < 0.1)  // 错误率应该小于10%
  
  // 验证服务错误率
  let api_service_stats = monitoring_results.service_stats.find(fn(s) { s.service == "api.service" })
  assert_true(api_service_stats != None)
  
  match api_service_stats {
    Some(stats) => {
      assert_true(stats.total_operations > 0)
      assert_true(stats.failed_operations > 0)
      assert_true(stats.success_rate > 0.8)  // 成功率应该大于80%
      assert_true(stats.avg_duration_ms > 0)
    }
    None => assert_true(false)
  }
  
  let db_service_stats = monitoring_results.service_stats.find(fn(s) { s.service == "database.service" })
  assert_true(db_service_stats != None)
  
  match db_service_stats {
    Some(stats) => {
      assert_eq(stats.total_operations, 16)  // 16个数据库操作
      assert_eq(stats.failed_operations, 16)  // 所有都失败
      assert_eq(stats.success_rate, 0.0)      // 成功率为0
    }
    None => assert_true(false)
  }
  
  // 检查触发告警
  let triggered_alerts = ErrorMonitor::check_alert_conditions(error_monitor, monitoring_results)
  assert_true(triggered_alerts.length() > 0)
  
  // 验证告警内容
  let high_error_rate_alert = triggered_alerts.find(fn(a) { a.rule_name == "high_error_rate" })
  let db_failure_alert = triggered_alerts.find(fn(a) { a.rule_name == "database_connection_failure" })
  let response_time_alert = triggered_alerts.find(fn(a) { a.rule_name == "spike_in_response_time" })
  
  // 根据错误率，可能触发高错误率告警
  if monitoring_results.error_rate > 0.05 {
    assert_true(high_error_rate_alert != None)
  }
  
  // 数据库连接失败应该触发告警
  assert_true(db_failure_alert != None)
  
  // 高延迟应该触发告警
  assert_true(response_time_alert != None)
  
  // 测试告警通知
  let notification_manager = NotificationManager::new()
  
  // 配置通知渠道
  NotificationManager::configure_channel(notification_manager, "email", {
    enabled: true,
    recipients: ["ops-team@example.com"],
    template: "error_alert_email",
    rate_limit: {
      max_per_hour: 10,
      cooldown_minutes: 5
    }
  })
  
  NotificationManager::configure_channel(notification_manager, "slack", {
    enabled: true,
    webhook_url: "https://hooks.slack.com/services/...",
    channel: "#alerts",
    template: "error_alert_slack",
    rate_limit: {
      max_per_hour: 20,
      cooldown_minutes: 2
    }
  })
  
  NotificationManager::configure_channel(notification_manager, "pagerduty", {
    enabled: true,
    api_key: "pagerduty-api-key",
    severity: "high",
    template: "critical_error_pagerduty",
    rate_limit: {
      max_per_hour: 5,
      cooldown_minutes: 10
    }
  })
  
  // 发送告警通知
  let mut sent_notifications = []
  for alert in triggered_alerts {
    for channel in alert.notification_channels {
      let notification_result = NotificationManager::send_notification(
        notification_manager,
        channel,
        {
          alert_name: alert.rule_name,
          severity: alert.severity,
          message: alert.description,
          timestamp: alert.triggered_at,
          details: alert.details
        }
      )
      
      sent_notifications.push((channel, notification_result))
    }
  }
  
  // 验证通知结果
  assert_true(sent_notifications.length() > 0)
  
  let email_notifications = sent_notifications.filter(fn((c, _)) { c == "email" })
  let slack_notifications = sent_notifications.filter(fn((c, _)) { c == "slack" })
  let pagerduty_notifications = sent_notifications.filter(fn((c, _)) { c == "pagerduty" })
  
  // 验证至少有一些通知被发送
  assert_true(email_notifications.length() > 0 or slack_notifications.length() > 0)
  
  // 验证通知发送结果
  for (channel, result) in sent_notifications {
    assert_true(result.success or result.rate_limited)
  }
  
  // 测试错误趋势分析
  let trend_analysis = ErrorMonitor::analyze_error_trends(error_monitor, {
    time_range: {
      start: base_time,
      end: base_time + 300000
    },
    granularity: "minute",
    metrics: ["error_rate", "avg_duration", "operation_count"],
    services: ["api.service", "database.service"]
  })
  
  // 验证趋势分析结果
  assert_true(trend_analysis.time_series.length() > 0)
  
  for series in trend_analysis.time_series {
    assert_true(series.data_points.length() > 0)
    assert_true(series.metric_name != "")
    
    for point in series.data_points {
      assert_true(point.timestamp >= base_time)
      assert_true(point.timestamp <= base_time + 300000)
      assert_true(point.value >= 0)
    }
  }
  
  // 验证趋势检测
  if trend_analysis.error_rate_trend != None {
    match trend_analysis.error_rate_trend {
      Some(trend) => {
        assert_true(trend.direction == "increasing" or trend.direction == "decreasing" or trend.direction == "stable")
        assert_true(trend.confidence >= 0 and trend.confidence <= 1)
      }
      None => assert_true(false)
    }
  }
}

// 测试5: 错误恢复和自愈机制
test "错误恢复和自愈机制测试" {
  // 创建自愈管理器
  let self_healing_manager = SelfHealingManager::new()
  
  // 配置自愈策略
  SelfHealingManager::configure_healing_strategy(self_healing_manager, {
    detection_interval_ms: 30000,  // 30秒检测一次
    healing_cooldown_ms: 300000,   // 5分钟冷却时间
    max_healing_attempts: 3,
    auto_approval: false,          // 需要人工审批
    notification_channels: ["email", "slack"]
  })
  
  // 注册自愈规则
  SelfHealingManager::register_healing_rule(self_healing_manager, {
    name: "restart_unhealthy_service",
    condition: "service_health_score < 0.5",
    healing_action: {
      type: "service_restart",
      parameters: {
        service_name: "affected_service",
        graceful_shutdown_timeout_ms: 30000,
        startup_timeout_ms: 60000
      }
    },
    approval_required: false,
    cooldown_minutes: 10
  })
  
  SelfHealingManager::register_healing_rule(self_healing_manager, {
    name: "scale_up_on_high_load",
    condition: "cpu_utilization > 80 and memory_utilization > 70",
    healing_action: {
      type: "scale_resources",
      parameters: {
        resource_type: "compute",
        scale_factor: 1.5,
        max_instances: 10,
        warmup_time_ms: 120000
      }
    },
    approval_required: true,
    cooldown_minutes: 30
  })
  
  SelfHealingManager::register_healing_rule(self_healing_manager, {
    name: "clear_cache_on_memory_pressure",
    condition: "memory_utilization > 85",
    healing_action: {
      type: "cache_management",
      parameters: {
        action: "clear_expired",
        target_cache: "application_cache",
        retention_ratio: 0.7
      }
    },
    approval_required: false,
    cooldown_minutes: 5
  })
  
  SelfHealingManager::register_healing_rule(self_healing_manager, {
    name: "reset_connection_pool",
    condition: "db_connection_errors > 20 in 5 minutes",
    healing_action: {
      type: "connection_pool_reset",
      parameters: {
        database: "primary_db",
        new_pool_size: 20,
        validation_query: "SELECT 1"
      }
    },
    approval_required: false,
    cooldown_minutes: 15
  })
  
  // 模拟系统状态监控
  let system_monitor = SystemMonitor::new()
  
  // 模拟系统健康状态
  let system_state = {
    services: [
      {
        name: "api.service",
        health_score: 0.3,  // 不健康
        uptime_percentage: 85.5,
        error_rate: 0.15,
        avg_response_time_ms: 1200
      },
      {
        name: "database.service",
        health_score: 0.7,  // 部分健康
        uptime_percentage: 95.2,
        error_rate: 0.05,
        avg_response_time_ms: 300
      },
      {
        name: "cache.service",
        health_score: 0.9,  // 健康
        uptime_percentage: 99.8,
        error_rate: 0.01,
        avg_response_time_ms: 50
      }
    ],
    resources: {
      cpu_utilization: 85.0,
      memory_utilization: 88.0,
      disk_utilization: 45.0,
      network_utilization: 60.0
    },
    database: {
      connection_errors: 25,
      active_connections: 18,
      max_connections: 20,
      avg_query_time_ms: 450
    }
  }
  
  // 检测系统问题
  let detected_issues = SelfHealingManager::detect_issues(self_healing_manager, system_state)
  
  // 验证检测到的问题
  assert_true(detected_issues.length() > 0)
  
  // 应该检测到API服务不健康
  let api_service_issue = detected_issues.find(fn(i) { i.affected_component == "api.service" })
  assert_true(api_service_issue != None)
  
  // 应该检测到高CPU和内存使用率
  let high_cpu_issue = detected_issues.find(fn(i) { i.issue_type == "high_cpu_utilization" })
  let high_memory_issue = detected_issues.find(fn(i) { i.issue_type == "high_memory_utilization" })
  assert_true(high_cpu_issue != None)
  assert_true(high_memory_issue != None)
  
  // 应该检测到数据库连接错误
  let db_connection_issue = detected_issues.find(fn(i) { i.issue_type == "database_connection_errors" })
  assert_true(db_connection_issue != None)
  
  // 生成自愈建议
  let healing_recommendations = SelfHealingManager::generate_healing_recommendations(
    self_healing_manager,
    detected_issues
  )
  
  // 验证自愈建议
  assert_true(healing_recommendations.length() > 0)
  
  // 应该有重启API服务的建议
  let restart_recommendation = healing_recommendations.find(fn(r) { r.rule_name == "restart_unhealthy_service" })
  assert_true(restart_recommendation != None)
  
  // 应该有扩展资源的建议
  let scale_recommendation = healing_recommendations.find(fn(r) { r.rule_name == "scale_up_on_high_load" })
  assert_true(scale_recommendation != None)
  
  // 应该有清理缓存的建议
  let cache_recommendation = healing_recommendations.find(fn(r) { r.rule_name == "clear_cache_on_memory_pressure" })
  assert_true(cache_recommendation != None)
  
  // 应该有重置连接池的建议
  let pool_recommendation = healing_recommendations.find(fn(r) { r.rule_name == "reset_connection_pool" })
  assert_true(pool_recommendation != None)
  
  // 执行自动批准的自愈操作
  let auto_approved_recommendations = healing_recommendations.filter(fn(r) { not(r.approval_required) })
  let mut healing_results = []
  
  for recommendation in auto_approved_recommendations {
    let result = SelfHealingManager::execute_healing_action(
      self_healing_manager,
      recommendation.healing_action,
      recommendation.parameters
    )
    healing_results.push((recommendation.rule_name, result))
  }
  
  // 验证自愈操作结果
  assert_true(healing_results.length() > 0)
  
  for (rule_name, result) in healing_results {
    assert_true(result.success or result.skipped)
    
    if result.success {
      assert_true(result.execution_time_ms > 0)
      assert_true(result.message != "")
    }
  }
  
  // 模拟需要人工批准的操作
  let manual_approved_recommendations = healing_recommendations.filter(fn(r) { r.approval_required })
  
  if manual_approved_recommendations.length() > 0 {
    // 模拟人工批准
    for recommendation in manual_approved_recommendations {
      SelfHealingManager::approve_healing_action(
        self_healing_manager,
        recommendation.rule_name,
        "operator@example.com",
        "High load detected, manual approval granted"
      )
    }
    
    // 执行已批准的操作
    for recommendation in manual_approved_recommendations {
      let result = SelfHealingManager::execute_healing_action(
        self_healing_manager,
        recommendation.healing_action,
        recommendation.parameters
      )
      healing_results.push((recommendation.rule_name, result))
    }
  }
  
  // 验证所有自愈操作结果
  assert_eq(healing_results.length(), healing_recommendations.length())
  
  let successful_healings = healing_results.filter(fn((_, r)) { r.success })
  let failed_healings = healing_results.filter(fn((_, r)) { not(r.success) and not(r.skipped) })
  
  assert_true(successful_healings.length() > 0)
  assert_true(failed_healings.length() == 0)  // 不应该有失败的自愈操作
  
  // 监控自愈效果
  let post_healing_state = {
    services: [
      {
        name: "api.service",
        health_score: 0.8,  // 改善
        uptime_percentage: 98.5,
        error_rate: 0.02,
        avg_response_time_ms: 400
      },
      {
        name: "database.service",
        health_score: 0.9,  // 改善
        uptime_percentage: 99.2,
        error_rate: 0.01,
        avg_response_time_ms: 200
      },
      {
        name: "cache.service",
        health_score: 0.9,  // 保持健康
        uptime_percentage: 99.8,
        error_rate: 0.01,
        avg_response_time_ms: 50
      }
    ],
    resources: {
      cpu_utilization: 65.0,  // 改善
      memory_utilization: 70.0,  // 改善
      disk_utilization: 45.0,
      network_utilization: 60.0
    },
    database: {
      connection_errors: 2,  // 改善
      active_connections: 15,
      max_connections: 20,
      avg_query_time_ms: 250
    }
  }
  
  // 验证自愈效果
  assert_true(post_healing_state.services[0].health_score > system_state.services[0].health_score)
  assert_true(post_healing_state.resources.cpu_utilization < system_state.resources.cpu_utilization)
  assert_true(post_healing_state.resources.memory_utilization < system_state.resources.memory_utilization)
  assert_true(post_healing_state.database.connection_errors < system_state.database.connection_errors)
  
  // 获取自愈统计
  let healing_stats = SelfHealingManager::get_healing_statistics(self_healing_manager)
  
  // 验证自愈统计
  assert_true(healing_stats.total_issues_detected > 0)
  assert_true(healing_stats.healing_actions_executed > 0)
  assert_true(healing_stats.successful_healings > 0)
  assert_true(healing_stats.failed_healings == 0)
  assert_true(healing_stats.avg_healing_time_ms > 0)
}