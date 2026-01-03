// Azimuth 错误恢复机制测试用例
// 测试系统的错误检测、恢复策略和容错能力

test "错误分类和严重性评估" {
  // 创建错误分类器
  let error_classifier = azimuth::ErrorClassifier::new()
  
  // 注册错误类型
  error_classifier.register_error_type("NetworkTimeout", azimuth::ErrorSeverity::Medium, azimuth::ErrorCategory::Transient)
  error_classifier.register_error_type("DatabaseConnectionFailed", azimuth::ErrorSeverity::High, azimuth::ErrorCategory::Persistent)
  error_classifier.register_error_type("InvalidInputData", azimuth::ErrorSeverity::Low, azimuth::ErrorCategory::User)
  error_classifier.register_error_type("SystemOutOfMemory", azimuth::ErrorSeverity::Critical, azimuth::ErrorCategory::System)
  
  // 创建错误实例
  let network_error = azimuth::Error::new("NetworkTimeout", "Request timed out after 30 seconds")
  network_error.set_context("service", "user-service")
  network_error.set_context("endpoint", "/api/users")
  network_error.set_context("timeout_ms", "30000")
  
  let db_error = azimuth::Error::new("DatabaseConnectionFailed", "Cannot connect to database server")
  db_error.set_context("database", "user_db")
  db_error.set_context("host", "db-01.example.com")
  db_error.set_context("port", "5432")
  
  let input_error = azimuth::Error::new("InvalidInputData", "Missing required field: user_id")
  input_error.set_context("request_id", "req-12345")
  input_error.set_context("field", "user_id")
  
  let memory_error = azimuth::Error::new("SystemOutOfMemory", "JVM heap space exhausted")
  memory_error.set_context("available_memory", "128MB")
  memory_error.set_context("required_memory", "512MB")
  
  // 分类错误
  let network_classification = error_classifier.classify(network_error)
  let db_classification = error_classifier.classify(db_error)
  let input_classification = error_classifier.classify(input_error)
  let memory_classification = error_classifier.classify(memory_error)
  
  // 验证分类结果
  assert_eq(network_classification.error_type, "NetworkTimeout")
  assert_eq(network_classification.severity, azimuth::ErrorSeverity::Medium)
  assert_eq(network_classification.category, azimuth::ErrorCategory::Transient)
  
  assert_eq(db_classification.error_type, "DatabaseConnectionFailed")
  assert_eq(db_classification.severity, azimuth::ErrorSeverity::High)
  assert_eq(db_classification.category, azimuth::ErrorCategory::Persistent)
  
  assert_eq(input_classification.error_type, "InvalidInputData")
  assert_eq(input_classification.severity, azimuth::ErrorSeverity::Low)
  assert_eq(input_classification.category, azimuth::ErrorCategory::User)
  
  assert_eq(memory_classification.error_type, "SystemOutOfMemory")
  assert_eq(memory_classification.severity, azimuth::ErrorSeverity::Critical)
  assert_eq(memory_classification.category, azimuth::ErrorCategory::System)
  
  // 验证错误上下文
  assert_eq(network_error.get_context("service"), Some("user-service"))
  assert_eq(network_error.get_context("timeout_ms"), Some("30000"))
  
  assert_eq(db_error.get_context("database"), Some("user_db"))
  assert_eq(db_error.get_context("host"), Some("db-01.example.com"))
}

test "自动重试机制" {
  // 创建重试策略
  let fixed_retry_policy = azimuth::RetryPolicy::FixedDelay {
    max_attempts: 5,
    delay_ms: 1000
  }
  
  let exponential_retry_policy = azimuth::RetryPolicy::ExponentialBackoff {
    max_attempts: 4,
    initial_delay_ms: 500,
    max_delay_ms: 10000,
    multiplier: 2.0
  }
  
  let custom_retry_policy = azimuth::RetryPolicy::Custom {
    retry_condition: fn(error, attempt) {
      match error.get_type() {
        "NetworkTimeout" => attempt < 3,
        "DatabaseConnectionFailed" => attempt < 5,
        _ => false
      }
    },
    delay_calculator: fn(attempt) {
      if attempt == 1 { 1000 }
      else if attempt == 2 { 2000 }
      else if attempt == 3 { 5000 }
      else { 10000 }
    }
  }
  
  // 创建重试管理器
  let retry_manager = azimuth::RetryManager::new()
  
  // 测试固定延迟重试
  let mut attempt_count = 0
  let fixed_result = retry_manager.execute_with_retry(fixed_retry_policy, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      return azimuth::Result::Error(azimuth::Error::new("NetworkTimeout", "Simulated failure"))
    } else {
      return azimuth::Result::Ok("Success after retries")
    }
  })
  
  match fixed_result {
    azimuth::Result::Ok(value) => assert_eq(value, "Success after retries"),
    azimuth::Result::Error(_) => assert_true(false)
  }
  assert_eq(attempt_count, 3)
  
  // 重置计数器
  attempt_count = 0
  
  // 测试指数退避重试
  let exponential_result = retry_manager.execute_with_retry(exponential_retry_policy, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 4 {
      return azimuth::Result::Error(azimuth::Error::new("DatabaseConnectionFailed", "Simulated failure"))
    } else {
      return azimuth::Result::Ok("Success after exponential backoff")
    }
  })
  
  match exponential_result {
    azimuth::Result::Ok(value) => assert_eq(value, "Success after exponential backoff"),
    azimuth::Result::Error(_) => assert_true(false)
  }
  assert_eq(attempt_count, 4)
  
  // 重置计数器
  attempt_count = 0
  
  // 测试自定义重试策略
  let custom_result = retry_manager.execute_with_retry(custom_retry_policy, fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      return azimuth::Result::Error(azimuth::Error::new("NetworkTimeout", "Simulated failure"))
    } else {
      return azimuth::Result::Ok("Success with custom policy")
    }
  })
  
  match custom_result {
    azimuth::Result::Ok(value) => assert_eq(value, "Success with custom policy"),
    azimuth::Result::Error(_) => assert_true(false)
  }
  assert_eq(attempt_count, 3)
  
  // 测试重试失败的情况
  attempt_count = 0
  let failure_result = retry_manager.execute_with_retry(fixed_retry_policy, fn() {
    attempt_count = attempt_count + 1
    return azimuth::Result::Error(azimuth::Error::new("PersistentError", "Always fails"))
  })
  
  match failure_result {
    azimuth::Result::Ok(_) => assert_true(false),
    azimuth::Result::Error(error) => {
      assert_eq(error.get_type(), "PersistentError")
      assert_eq(attempt_count, 5) // 达到最大重试次数
    }
  }
}

test "断路器模式" {
  // 创建断路器配置
  let circuit_breaker_config = azimuth::CircuitBreakerConfig {
    failure_threshold: 5, // 5次失败后打开断路器
    recovery_timeout_ms: 30000, // 30秒后尝试半开状态
    expected_exception_types: ["NetworkTimeout", "DatabaseConnectionFailed"],
    success_threshold: 3, // 半开状态下需要3次成功才能关闭断路器
    monitoring_period_ms: 10000 // 监控窗口10秒
  }
  
  // 创建断路器
  let circuit_breaker = azimuth::CircuitBreaker::new("user-service-circuit", circuit_breaker_config)
  
  // 验证初始状态
  assert_eq(circuit_breaker.get_state(), azimuth::CircuitState::Closed)
  assert_eq(circuit_breaker.get_failure_count(), 0)
  assert_eq(circuit_breaker.get_success_count(), 0)
  
  // 模拟成功调用
  for i in 0..=2 {
    let result = circuit_breaker.execute(fn() {
      return azimuth::Result::Ok("Success " + i.to_string())
    })
    
    match result {
      azimuth::Result::Ok(_) => assert_true(true),
      azimuth::Result::Error(_) => assert_true(false)
    }
  }
  
  assert_eq(circuit_breaker.get_state(), azimuth::CircuitState::Closed)
  assert_eq(circuit_breaker.get_failure_count(), 0)
  assert_eq(circuit_breaker.get_success_count(), 3)
  
  // 模拟失败调用
  for i in 0..=4 {
    let result = circuit_breaker.execute(fn() {
      return azimuth::Result::Error(azimuth::Error::new("NetworkTimeout", "Simulated network failure"))
    })
    
    match result {
      azimuth::Result::Ok(_) => assert_true(false),
      azimuth::Result::Error(_) => assert_true(true)
    }
  }
  
  // 验证断路器已打开
  assert_eq(circuit_breaker.get_state(), azimuth::CircuitState::Open)
  assert_eq(circuit_breaker.get_failure_count(), 5)
  
  // 断路器打开时，调用应该直接失败
  let open_result = circuit_breaker.execute(fn() {
    return azimuth::Result::Ok("Should not execute")
  })
  
  match open_result {
    azimuth::Result::Ok(_) => assert_true(false),
    azimuth::Result::Error(error) => {
      assert_eq(error.get_type(), "CircuitBreakerOpen")
    }
  }
  
  // 模拟恢复超时（将时间快进）
  circuit_breaker.advance_time(30000L)
  
  // 验证断路器进入半开状态
  assert_eq(circuit_breaker.get_state(), azimuth::CircuitState::HalfOpen)
  
  // 在半开状态下模拟成功调用
  for i in 0..=2 {
    let result = circuit_breaker.execute(fn() {
      return azimuth::Result::Ok("Recovery success " + i.to_string())
    })
    
    match result {
      azimuth::Result::Ok(_) => assert_true(true),
      azimuth::Result::Error(_) => assert_true(false)
    }
  }
  
  // 验证断路器已关闭
  assert_eq(circuit_breaker.get_state(), azimuth::CircuitState::Closed)
  assert_eq(circuit_breaker.get_failure_count(), 0) // 失败计数器重置
  assert_eq(circuit_breaker.get_success_count(), 0) // 成功计数器重置
}

test "故障转移机制" {
  // 创建故障转移配置
  let failover_config = azimuth::FailoverConfig {
    primary_service: "user-service-primary",
    backup_services: ["user-service-backup-1", "user-service-backup-2"],
    health_check_interval_ms: 5000,
    failover_threshold: 2, // 连续2次健康检查失败后切换
    recovery_check_interval_ms: 30000, // 每30秒检查主服务是否恢复
    auto_failback: true
  }
  
  // 创建故障转移管理器
  let failover_manager = azimuth::FailoverManager::new(failover_config)
  
  // 注册服务实例
  failover_manager.register_service("user-service-primary", "http://primary.example.com:8080")
  failover_manager.register_service("user-service-backup-1", "http://backup1.example.com:8080")
  failover_manager.register_service("user-service-backup-2", "http://backup2.example.com:8080")
  
  // 验证初始状态
  assert_eq(failover_manager.get_active_service(), Some("user-service-primary"))
  assert_eq(failover_manager.get_service_health("user-service-primary"), azimuth::ServiceHealth::Healthy)
  
  // 模拟主服务故障
  failover_manager.simulate_service_failure("user-service-primary")
  failover_manager.simulate_service_failure("user-service-primary") // 第二次失败触发故障转移
  
  // 验证故障转移到备份服务
  assert_eq(failover_manager.get_active_service(), Some("user-service-backup-1"))
  assert_eq(failover_manager.get_service_health("user-service-primary"), azimuth::ServiceHealth::Unhealthy)
  
  // 通过故障转移管理器执行调用
  let failover_result = failover_manager.execute(fn(service_url) {
    return azimuth::Result::Ok("Response from " + service_url)
  })
  
  match failover_result {
    azimuth::Result::Ok(response) => {
      assert_true(response.contains("backup1.example.com"))
    }
    azimuth::Result::Error(_) => assert_true(false)
  }
  
  // 模拟备份服务也故障
  failover_manager.simulate_service_failure("user-service-backup-1")
  failover_manager.simulate_service_failure("user-service-backup-1")
  
  // 验证故障转移到第二个备份服务
  assert_eq(failover_manager.get_active_service(), Some("user-service-backup-2"))
  
  // 模拟主服务恢复
  failover_manager.simulate_service_recovery("user-service-primary")
  
  // 由于启用了自动故障恢复，等待恢复检查
  failover_manager.advance_time(30000L)
  
  // 验证故障恢复到主服务
  assert_eq(failover_manager.get_active_service(), Some("user-service-primary"))
  assert_eq(failover_manager.get_service_health("user-service-primary"), azimuth::ServiceHealth::Healthy)
}

test "数据一致性恢复" {
  // 创建一致性管理器
  let consistency_manager = azimuth::ConsistencyManager::new()
  
  // 创建事务
  let transaction = consistency_manager.begin_transaction("order-processing")
  
  // 执行操作
  let operation1 = azimuth::Operation::DatabaseInsert {
    table: "orders",
    data: [("order_id", "ORD-001"), ("user_id", "USER-001"), ("amount", "100.00")]
  }
  
  let operation2 = azimuth::Operation::DatabaseInsert {
    table: "order_items",
    data: [("item_id", "ITEM-001"), ("order_id", "ORD-001"), ("product_id", "PROD-001")]
  }
  
  let operation3 = azimuth::Operation::MessagePublish {
    topic: "order_events",
    message: [("event_type", "order_created"), ("order_id", "ORD-001")]
  }
  
  // 执行操作
  let result1 = consistency_manager.execute_operation(transaction, operation1)
  let result2 = consistency_manager.execute_operation(transaction, operation2)
  
  // 模拟第三个操作失败
  let result3 = consistency_manager.execute_operation(transaction, operation3, fn() {
    return azimuth::Result::Error(azimuth::Error::new("MessagePublishFailed", "Broker unavailable"))
  })
  
  // 验证前两个操作成功
  match result1 {
    azimuth::Result::Ok(_) => assert_true(true),
    azimuth::Result::Error(_) => assert_true(false)
  }
  
  match result2 {
    azimuth::Result::Ok(_) => assert_true(true),
    azimuth::Result::Error(_) => assert_true(false)
  }
  
  // 验证第三个操作失败
  match result3 {
    azimuth::Result::Ok(_) => assert_true(false),
    azimuth::Result::Error(_) => assert_true(true)
  }
  
  // 由于事务失败，执行回滚
  let rollback_result = consistency_manager.rollback_transaction(transaction)
  assert_true(rollback_result)
  
  // 验证回滚后的状态
  let order_exists = consistency_manager.check_data_exists("orders", "order_id", "ORD-001")
  assert_false(order_exists)
  
  let item_exists = consistency_manager.check_data_exists("order_items", "item_id", "ITEM-001")
  assert_false(item_exists)
  
  // 创建补偿事务
  let compensation_transaction = consistency_manager.begin_compensation_transaction("order-compensation")
  
  // 执行补偿操作
  let compensation_ops = [
    azimuth::Operation::DatabaseDelete {
      table: "orders",
      condition: [("order_id", "ORD-001")]
    },
    azimuth::Operation::DatabaseDelete {
      table: "order_items",
      condition: [("order_id", "ORD-001")]
    },
    azimuth::Operation::MessagePublish {
      topic: "compensation_events",
      message: [("event_type", "order_compensated"), ("order_id", "ORD-001")]
    }
  ]
  
  for op in compensation_ops {
    let result = consistency_manager.execute_operation(compensation_transaction, op)
    match result {
      azimuth::Result::Ok(_) => assert_true(true),
      azimuth::Result::Error(_) => assert_true(false)
    }
  }
  
  // 提交补偿事务
  let commit_result = consistency_manager.commit_transaction(compensation_transaction)
  assert_true(commit_result)
  
  // 验证补偿事务状态
  assert_eq(consistency_manager.get_transaction_status(compensation_transaction), azimuth::TransactionStatus::Committed)
}

test "系统级故障恢复" {
  // 创建系统恢复管理器
  let recovery_manager = azimuth::SystemRecoveryManager::new()
  
  // 注册系统组件
  recovery_manager.register_component("database", azimuth::ComponentType::Database)
  recovery_manager.register_component("message_queue", azimuth::ComponentType::MessageQueue)
  recovery_manager.register_component("cache", azimuth::ComponentType::Cache)
  recovery_manager.register_component("web_server", azimuth::ComponentType::WebServer)
  
  // 配置恢复策略
  let database_recovery = azimuth::RecoveryStrategy::RestartWithRetry {
    max_restart_attempts: 3,
    restart_delay_ms: 5000,
    health_check_timeout_ms: 10000
  }
  
  let cache_recovery = azimuth::RecoveryStrategy::FailoverWithRebuild {
    backup_nodes: ["cache-backup-01", "cache-backup-02"],
    rebuild_timeout_ms: 60000
  }
  
  let web_server_recovery = azimuth::RecoveryStrategy::ScaleOut {
    min_instances: 2,
    max_instances: 10,
    scale_up_threshold: 80.0, // CPU使用率超过80%时扩容
    scale_down_threshold: 30.0 // CPU使用率低于30%时缩容
  }
  
  recovery_manager.set_recovery_strategy("database", database_recovery)
  recovery_manager.set_recovery_strategy("cache", cache_recovery)
  recovery_manager.set_recovery_strategy("web_server", web_server_recovery)
  
  // 模拟系统故障
  recovery_manager.simulate_component_failure("database")
  
  // 验证故障检测
  let system_status = recovery_manager.get_system_status()
  assert_eq(system_status.overall_health, azimuth::SystemHealth::Degraded)
  assert_eq(system_status.component_status["database"], azimuth::ComponentHealth::Failed)
  
  // 触发自动恢复
  let recovery_result = recovery_manager.initiate_recovery("database")
  assert_true(recovery_result)
  
  // 验证恢复过程
  let recovery_status = recovery_manager.get_recovery_status("database")
  assert_eq(recovery_status.state, azimuth::RecoveryState::InProgress)
  assert_eq(recovery_status.attempt_count, 1)
  
  // 模拟恢复成功
  recovery_manager.simulate_recovery_success("database")
  
  // 验证恢复完成
  let final_status = recovery_manager.get_system_status()
  assert_eq(final_status.overall_health, azimuth::SystemHealth::Healthy)
  assert_eq(final_status.component_status["database"], azimuth::ComponentHealth::Healthy)
  
  // 测试级联故障恢复
  recovery_manager.simulate_component_failure("cache")
  recovery_manager.simulate_component_failure("message_queue")
  
  // 验证级联故障检测
  let cascade_status = recovery_manager.get_system_status()
  assert_eq(cascade_status.overall_health, azimuth::SystemHealth::Critical)
  
  // 触发系统级恢复
  let system_recovery_result = recovery_manager.initiate_system_recovery()
  assert_true(system_recovery_result)
  
  // 模拟所有组件恢复
  recovery_manager.simulate_recovery_success("cache")
  recovery_manager.simulate_recovery_success("message_queue")
  
  // 验证系统完全恢复
  let fully_recovered_status = recovery_manager.get_system_status()
  assert_eq(fully_recovered_status.overall_health, azimuth::SystemHealth::Healthy)
  
  // 验证恢复历史
  let recovery_history = recovery_manager.get_recovery_history()
  assert_true(recovery_history.length() > 0)
  
  let database_recovery = recovery_history.find(fn(entry) { 
    entry.component == "database" 
  })
  match database_recovery {
    Some(entry) => {
      assert_eq(entry.outcome, azimuth::RecoveryOutcome::Success)
      assert_true(entry.duration_ms > 0)
    }
    None => assert_true(false)
  }
}

test "错误监控和告警" {
  // 创建错误监控器
  let error_monitor = azimuth::ErrorMonitor::new()
  
  // 配置告警规则
  let error_rate_rule = azimuth::AlertRule::ErrorRate {
    name: "high_error_rate",
    threshold: 0.05, // 5%错误率
    window_minutes: 5,
    severity: azimuth::AlertSeverity::Warning
  }
  
  let critical_error_rule = azimuth::AlertRule::CriticalError {
    name: "critical_error_detected",
    error_types: ["SystemOutOfMemory", "DatabaseConnectionFailed"],
    count_threshold: 1,
    severity: azimuth::AlertSeverity::Critical
  }
  
  let error_spike_rule = azimuth::AlertRule::ErrorSpike {
    name: "error_spike_detected",
    baseline_window_minutes: 60,
    current_window_minutes: 5,
    spike_multiplier: 3.0, // 当前错误率是基线的3倍
    severity: azimuth::AlertSeverity::Warning
  }
  
  error_monitor.add_alert_rule(error_rate_rule)
  error_monitor.add_alert_rule(critical_error_rule)
  error_monitor.add_alert_rule(error_spike_rule)
  
  // 配置告警通道
  let email_channel = azimuth::AlertChannel::Email {
    name: "ops-team-email",
    recipients: ["ops@example.com"],
    template: "error_alert_template"
  }
  
  let slack_channel = azimuth::AlertChannel::Slack {
    name: "ops-slack",
    webhook_url: "https://hooks.slack.com/services/...",
    channel: "#alerts"
  }
  
  error_monitor.add_alert_channel(email_channel)
  error_monitor.add_alert_channel(slack_channel)
  
  // 模拟正常操作
  for i in 0..=94 {
    let operation = azimuth::Operation::new("operation-" + i.to_string())
    operation.set_success(true)
    error_monitor.record_operation(operation)
  }
  
  // 模拟5%的错误率（触发告警）
  for i in 0..=4 {
    let operation = azimuth::Operation::new("error-operation-" + i.to_string())
    operation.set_success(false)
    operation.set_error(azimuth::Error::new("NetworkTimeout", "Request timed out"))
    error_monitor.record_operation(operation)
  }
  
  // 验证错误率告警
  let alerts = error_monitor.get_active_alerts()
  let error_rate_alert = alerts.find(fn(alert) { alert.rule_name == "high_error_rate" })
  match error_rate_alert {
    Some(alert) => {
      assert_eq(alert.severity, azimuth::AlertSeverity::Warning)
      assert_true(alert.message.contains("error rate"))
    }
    None => assert_true(false)
  }
  
  // 模拟关键错误
  let critical_operation = azimuth::Operation::new("critical-operation")
  critical_operation.set_success(false)
  critical_operation.set_error(azimuth::Error::new("SystemOutOfMemory", "JVM heap exhausted"))
  error_monitor.record_operation(critical_operation)
  
  // 验证关键错误告警
  let updated_alerts = error_monitor.get_active_alerts()
  let critical_alert = updated_alerts.find(fn(alert) { alert.rule_name == "critical_error_detected" })
  match critical_alert {
    Some(alert) => {
      assert_eq(alert.severity, azimuth::AlertSeverity::Critical)
      assert_true(alert.message.contains("SystemOutOfMemory"))
    }
    None => assert_true(false)
  }
  
  // 验证告警通知发送
  let notifications = error_monitor.get_sent_notifications()
  assert_true(notifications.length() > 0)
  
  // 测试告警解决
  // 模拟正常操作，降低错误率
  for i in 0..=99 {
    let operation = azimuth::Operation::new("recovery-operation-" + i.to_string())
    operation.set_success(true)
    error_monitor.record_operation(operation)
  }
  
  // 验证告警自动解决
  let resolved_alerts = error_monitor.get_resolved_alerts()
  assert_true(resolved_alerts.length() > 0)
  
  let resolved_error_rate = resolved_alerts.find(fn(alert) { alert.rule_name == "high_error_rate" })
  match resolved_error_rate {
    Some(alert) => {
      assert_eq(alert.status, azimuth::AlertStatus::Resolved)
    }
    None => assert_true(false)
  }
}