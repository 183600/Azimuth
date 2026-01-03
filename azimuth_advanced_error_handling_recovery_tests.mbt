// Azimuth 高级错误处理和恢复测试
// 专注于测试系统的错误处理机制、恢复策略和容错能力

// 测试1: 错误分类和优先级处理
test "错误分类和优先级处理测试" {
  // 1. 创建不同类型的错误
  let network_error = Error({
    error_type: NetworkError,
    code: "CONNECTION_TIMEOUT",
    message: "Connection to database server timed out",
    severity: High,
    retryable: true,
    context: [("server", "db-01"), ("timeout", "30s")]
  })
  
  let validation_error = Error({
    error_type: ValidationError,
    code: "INVALID_INPUT",
    message: "User input validation failed",
    severity: Medium,
    retryable: false,
    context: [("field", "email"), ("value", "invalid-email")]
  })
  
  let system_error = Error({
    error_type: SystemError,
    code: "OUT_OF_MEMORY",
    message: "System out of memory",
    severity: Critical,
    retryable: false,
    context: [("available_memory", "128MB"), ("required_memory", "512MB")]
  })
  
  let business_error = Error({
    error_type: BusinessError,
    code: "INSUFFICIENT_BALANCE",
    message: "Account has insufficient balance",
    severity: Low,
    retryable: false,
    context: [("account_id", "12345"), ("balance", "100.00"), ("required", "150.00")]
  })
  
  // 2. 验证错误属性
  match network_error.error_type {
    NetworkError => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(network_error.code, "CONNECTION_TIMEOUT")
  assert_eq(network_error.severity, High)
  assert_true(network_error.retryable)
  
  match validation_error.error_type {
    ValidationError => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(validation_error.code, "INVALID_INPUT")
  assert_eq(validation_error.severity, Medium)
  assert_false(validation_error.retryable)
  
  match system_error.error_type {
    SystemError => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(system_error.code, "OUT_OF_MEMORY")
  assert_eq(system_error.severity, Critical)
  assert_false(system_error.retryable)
  
  match business_error.error_type {
    BusinessError => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(business_error.code, "INSUFFICIENT_BALANCE")
  assert_eq(business_error.severity, Low)
  assert_false(business_error.retryable)
  
  // 3. 创建错误集合并按优先级排序
  let errors = [network_error, validation_error, system_error, business_error]
  let sorted_errors = sort_errors_by_priority(errors)
  
  // 4. 验证排序结果（Critical > High > Medium > Low）
  assert_eq(sorted_errors[0].severity, Critical)
  assert_eq(sorted_errors[1].severity, High)
  assert_eq(sorted_errors[2].severity, Medium)
  assert_eq(sorted_errors[3].severity, Low)
  
  // 5. 验证排序后的错误类型
  match sorted_errors[0].error_type {
    SystemError => assert_true(true)
    _ => assert_true(false)
  }
  
  match sorted_errors[1].error_type {
    NetworkError => assert_true(true)
    _ => assert_true(false)
  }
  
  match sorted_errors[2].error_type {
    ValidationError => assert_true(true)
    _ => assert_true(false)
  }
  
  match sorted_errors[3].error_type {
    BusinessError => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试2: 错误重试机制
test "错误重试机制测试" {
  // 1. 创建重试策略配置
  let retry_config = RetryConfig({
    max_attempts: 3,
    initial_delay: 1000, // 1秒
    max_delay: 10000, // 10秒
    backoff_multiplier: 2.0,
    retryable_errors: [
      "CONNECTION_TIMEOUT",
      "SERVICE_UNAVAILABLE",
      "RATE_LIMIT_EXCEEDED"
    ]
  })
  
  // 2. 验证重试配置
  assert_eq(retry_config.max_attempts, 3)
  assert_eq(retry_config.initial_delay, 1000)
  assert_eq(retry_config.max_delay, 10000)
  assert_eq(retry_config.backoff_multiplier, 2.0)
  assert_eq(retry_config.retryable_errors.length(), 3)
  
  // 3. 测试可重试错误的延迟计算
  let retryable_error = Error({
    error_type: NetworkError,
    code: "CONNECTION_TIMEOUT",
    message: "Connection timeout",
    severity: High,
    retryable: true,
    context: []
  })
  
  let delay1 = calculate_retry_delay(1, retry_config) // 第一次重试
  let delay2 = calculate_retry_delay(2, retry_config) // 第二次重试
  let delay3 = calculate_retry_delay(3, retry_config) // 第三次重试
  
  // 4. 验证延迟计算
  assert_eq(delay1, 1000) // 初始延迟
  assert_eq(delay2, 2000) // 初始延迟 * 2
  assert_eq(delay3, 4000) // 上次延迟 * 2
  
  // 5. 测试不可重试错误
  let non_retryable_error = Error({
    error_type: ValidationError,
    code: "INVALID_INPUT",
    message: "Invalid input",
    severity: Medium,
    retryable: false,
    context: []
  })
  
  let should_retry = should_retry_error(non_retryable_error, retry_config)
  assert_false(should_retry)
  
  // 6. 测试可重试错误
  let should_retry_retryable = should_retry_error(retryable_error, retry_config)
  assert_true(should_retry_retryable)
  
  // 7. 测试重试执行
  let mut attempt_count = 0
  let operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err(retryable_error) // 前两次尝试失败
    } else {
      Ok("success") // 第三次尝试成功
    }
  }
  
  let result = execute_with_retry(operation, retry_config)
  
  // 8. 验证重试结果
  match result {
    Ok(value) => assert_eq(value, "success")
    Err(_) => assert_true(false)
  }
  assert_eq(attempt_count, 3) // 应该尝试了3次
}

// 测试3: 断路器模式
test "断路器模式测试" {
  // 1. 创建断路器配置
  let circuit_breaker_config = CircuitBreakerConfig({
    failure_threshold: 5, // 5次失败后打开断路器
    success_threshold: 3, // 3次成功后关闭断路器
    timeout: 30000, // 30秒后尝试半开状态
    monitoring_period: 60000 // 1分钟监控周期
  })
  
  // 2. 创建断路器
  let mut circuit_breaker = CircuitBreaker({
    state: Closed, // 初始状态为关闭
    failure_count: 0,
    success_count: 0,
    last_failure_time: None,
    last_state_change: 1640997000000,
    config: circuit_breaker_config
  })
  
  // 3. 验证初始状态
  match circuit_breaker.state {
    Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(circuit_breaker.failure_count, 0)
  assert_eq(circuit_breaker.success_count, 0)
  match circuit_breaker.last_failure_time {
    None => assert_true(true)
    Some(_) => assert_true(false)
  }
  
  // 4. 模拟失败操作，达到失败阈值
  let failure_error = Error({
    error_type: NetworkError,
    code: "SERVICE_UNAVAILABLE",
    message: "Service unavailable",
    severity: High,
    retryable: true,
    context: []
  })
  
  for i in 1..=5 {
    let result = execute_with_circuit_breaker(fn() { Err(failure_error) }, circuit_breaker)
    match result {
      Err(_) => assert_true(true)
      Ok(_) => assert_true(false)
    }
    
    // 更新断路器状态
    circuit_breaker = update_circuit_breaker_state(circuit_breaker, false)
  }
  
  // 5. 验证断路器已打开
  match circuit_breaker.state {
    Open => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(circuit_breaker.failure_count, 5)
  
  // 6. 测试断路器打开时的操作执行
  let result_when_open = execute_with_circuit_breaker(fn() { Ok("success") }, circuit_breaker)
  match result_when_open {
    Err(error) => {
      match error.error_type {
        CircuitBreakerOpenError => assert_true(true)
        _ => assert_true(false)
      }
    }
    Ok(_) => assert_true(false)
  }
  
  // 7. 模拟超时后进入半开状态
  let current_time = circuit_breaker.last_state_change + circuit_breaker.config.timeout + 1000
  circuit_breaker = transition_to_half_open_if_needed(circuit_breaker, current_time)
  
  // 8. 验证半开状态
  match circuit_breaker.state {
    HalfOpen => assert_true(true)
    _ => assert_true(false)
  }
  
  // 9. 模拟半开状态下的成功操作，达到成功阈值
  for i in 1..=3 {
    let result = execute_with_circuit_breaker(fn() { Ok("success") }, circuit_breaker)
    match result {
      Ok(value) => assert_eq(value, "success")
      Err(_) => assert_true(false)
    }
    
    // 更新断路器状态
    circuit_breaker = update_circuit_breaker_state(circuit_breaker, true)
  }
  
  // 10. 验证断路器已关闭
  match circuit_breaker.state {
    Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(circuit_breaker.success_count, 3)
  assert_eq(circuit_breaker.failure_count, 0) // 失败计数应重置
}

// 测试4: 错误恢复策略
test "错误恢复策略测试" {
  // 1. 创建不同的恢复策略
  let retry_strategy = RecoveryStrategy({
    name: "retry",
    config: RetryStrategyConfig({
      max_attempts: 3,
      delay: 1000
    })
  })
  
  let fallback_strategy = RecoveryStrategy({
    name: "fallback",
    config: FallbackStrategyConfig({
      fallback_function: fn() { "fallback_result" }
    })
  })
  
  let cache_strategy = RecoveryStrategy({
    name: "cache",
    config: CacheStrategyConfig({
      cache_key: "user.profile.12345",
      ttl: 300 // 5分钟
    })
  })
  
  // 2. 验证恢复策略配置
  assert_eq(retry_strategy.name, "retry")
  assert_eq(fallback_strategy.name, "fallback")
  assert_eq(cache_strategy.name, "cache")
  
  // 3. 测试重试策略
  let mut attempt_count = 0
  let failing_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Err(Error({
        error_type: NetworkError,
        code: "CONNECTION_TIMEOUT",
        message: "Connection timeout",
        severity: High,
        retryable: true,
        context: []
      }))
    } else {
      Ok("operation_success")
    }
  }
  
  let retry_result = apply_recovery_strategy(failing_operation, retry_strategy)
  match retry_result {
    Ok(value) => assert_eq(value, "operation_success")
    Err(_) => assert_true(false)
  }
  assert_eq(attempt_count, 3)
  
  // 4. 测试回退策略
  let always_failing_operation = fn() {
    Err(Error({
      error_type: ServiceUnavailableError,
      code: "SERVICE_DOWN",
      message: "Service is down",
      severity: Critical,
      retryable: false,
      context: []
    }))
  }
  
  let fallback_result = apply_recovery_strategy(always_failing_operation, fallback_strategy)
  match fallback_result {
    Ok(value) => assert_eq(value, "fallback_result")
    Err(_) => assert_true(false)
  }
  
  // 5. 测试缓存策略
  let cache_unavailable_operation = fn() {
    Err(Error({
      error_type: DatabaseError,
      code: "DB_CONNECTION_FAILED",
      message: "Database connection failed",
      severity: High,
      retryable: true,
      context: []
    }))
  }
  
  let cache_result = apply_recovery_strategy(cache_unavailable_operation, cache_strategy)
  match cache_result {
    Ok(value) => assert_eq(value, "cached_value") // 假设缓存中有值
    Err(_) => assert_true(false)
  }
}

// 测试5: 错误聚合和分析
test "错误聚合和分析测试" {
  // 1. 创建错误事件集合
  let error_events = [
    ErrorEvent({
      timestamp: 1640998000000,
      error: Error({
        error_type: NetworkError,
        code: "CONNECTION_TIMEOUT",
        message: "Connection timeout",
        severity: High,
        retryable: true,
        context: [("service", "user-service")]
      }),
      service: "api-gateway",
      request_id: "req-001"
    }),
    ErrorEvent({
      timestamp: 1640998001000,
      error: Error({
        error_type: ValidationError,
        code: "INVALID_INPUT",
        message: "Invalid input",
        severity: Medium,
        retryable: false,
        context: [("field", "email")]
      }),
      service: "user-service",
      request_id: "req-002"
    }),
    ErrorEvent({
      timestamp: 1640998002000,
      error: Error({
        error_type: NetworkError,
        code: "CONNECTION_TIMEOUT",
        message: "Connection timeout",
        severity: High,
        retryable: true,
        context: [("service", "order-service")]
      }),
      service: "api-gateway",
      request_id: "req-003"
    }),
    ErrorEvent({
      timestamp: 1640998003000,
      error: Error({
        error_type: SystemError,
        code: "OUT_OF_MEMORY",
        message: "Out of memory",
        severity: Critical,
        retryable: false,
        context: [("process", "worker-1")]
      }),
      service: "order-service",
      request_id: "req-004"
    }),
    ErrorEvent({
      timestamp: 1640998004000,
      error: Error({
        error_type: NetworkError,
        code: "CONNECTION_TIMEOUT",
        message: "Connection timeout",
        severity: High,
        retryable: true,
        context: [("service", "payment-service")]
      }),
      service: "api-gateway",
      request_id: "req-005"
    })
  ]
  
  // 2. 验证错误事件集合
  assert_eq(error_events.length(), 5)
  
  // 3. 按错误类型分组
  let errors_by_type = group_errors_by_type(error_events)
  
  assert_eq(errors_by_type.length(), 3) // NetworkError, ValidationError, SystemError
  
  let network_errors = errors_by_type.get("NetworkError")
  match network_errors {
    Some(errors) => assert_eq(errors.length(), 3)
    None => assert_true(false)
  }
  
  let validation_errors = errors_by_type.get("ValidationError")
  match validation_errors {
    Some(errors) => assert_eq(errors.length(), 1)
    None => assert_true(false)
  }
  
  let system_errors = errors_by_type.get("SystemError")
  match system_errors {
    Some(errors) => assert_eq(errors.length(), 1)
    None => assert_true(false)
  }
  
  // 4. 按服务分组
  let errors_by_service = group_errors_by_service(error_events)
  
  assert_eq(errors_by_service.length(), 3) // api-gateway, user-service, order-service
  
  let gateway_errors = errors_by_service.get("api-gateway")
  match gateway_errors {
    Some(errors) => assert_eq(errors.length(), 3)
    None => assert_true(false)
  }
  
  let user_service_errors = errors_by_service.get("user-service")
  match user_service_errors {
    Some(errors) => assert_eq(errors.length(), 1)
    None => assert_true(false)
  }
  
  let order_service_errors = errors_by_service.get("order-service")
  match order_service_errors {
    Some(errors) => assert_eq(errors.length(), 2)
    None => assert_true(false)
  }
  
  // 5. 计算错误统计
  let error_stats = calculate_error_statistics(error_events)
  
  assert_eq(error_stats.total_errors, 5)
  assert_eq(error_stats.critical_errors, 1)
  assert_eq(error_stats.high_errors, 3)
  assert_eq(error_stats.medium_errors, 1)
  assert_eq(error_stats.low_errors, 0)
  assert_eq(error_stats.retryable_errors, 3)
  assert_eq(error_stats.non_retryable_errors, 2)
  
  // 6. 识别错误模式
  let error_patterns = identify_error_patterns(error_events)
  
  assert_eq(error_patterns.length(), 1) // 识别出连接超时模式
  
  let timeout_pattern = error_patterns[0]
  assert_eq(timeout_pattern.error_code, "CONNECTION_TIMEOUT")
  assert_eq(timeout_pattern.occurrence_count, 3)
  assert_eq(timeout_pattern.affected_services.length(), 1)
  assert_true(timeout_pattern.affected_services.contains("api-gateway"))
}

// 测试6: 错误通知和报告
test "错误通知和报告测试" {
  // 1. 创建错误通知配置
  let notification_config = NotificationConfig({
    channels: [
      NotificationChannel({
        type: Email,
        recipients: ["admin@example.com", "ops@example.com"],
        severity_threshold: Critical
      }),
      NotificationChannel({
        type: Slack,
        webhook_url: "https://hooks.slack.com/services/xxx/yyy/zzz",
        severity_threshold: High
      }),
      NotificationChannel({
        type: PagerDuty,
        service_key: "pd-service-key",
        severity_threshold: Critical
      })
    ],
    rate_limit: {
      max_notifications_per_minute: 10,
      cooldown_period: 300 // 5分钟
    }
  })
  
  // 2. 验证通知配置
  assert_eq(notification_config.channels.length(), 3)
  assert_eq(notification_config.rate_limit.max_notifications_per_minute, 10)
  assert_eq(notification_config.rate_limit.cooldown_period, 300)
  
  // 3. 创建严重错误事件
  let critical_error = ErrorEvent({
    timestamp: 1640999000000,
    error: Error({
      error_type: SystemError,
      code: "DATABASE_DOWN",
      message: "Primary database is down",
      severity: Critical,
      retryable: false,
      context: [("database", "primary-db"), ("region", "us-west")]
    }),
    service: "order-service",
    request_id: "req-critical-001"
  })
  
  // 4. 创建高严重性错误事件
  let high_error = ErrorEvent({
    timestamp: 1640999001000,
    error: Error({
      error_type: NetworkError,
      code: "CONNECTION_TIMEOUT",
      message: "Connection timeout",
      severity: High,
      retryable: true,
      context: [("service", "user-service"), ("timeout", "30s")]
    }),
    service: "api-gateway",
    request_id: "req-high-001"
  })
  
  // 5. 创建中等严重性错误事件
  let medium_error = ErrorEvent({
    timestamp: 1640999002000,
    error: Error({
      error_type: ValidationError,
      code: "INVALID_INPUT",
      message: "Invalid input",
      severity: Medium,
      retryable: false,
      context: [("field", "email"), ("value", "invalid")]
    }),
    service: "user-service",
    request_id: "req-medium-001"
  })
  
  // 6. 确定通知渠道
  let critical_channels = determine_notification_channels(critical_error, notification_config)
  let high_channels = determine_notification_channels(high_error, notification_config)
  let medium_channels = determine_notification_channels(medium_error, notification_config)
  
  // 7. 验证通知渠道选择
  assert_eq(critical_channels.length(), 3) // 严重错误通知所有渠道
  assert_true(critical_channels.contains("Email"))
  assert_true(critical_channels.contains("Slack"))
  assert_true(critical_channels.contains("PagerDuty"))
  
  assert_eq(high_channels.length(), 1) // 高严重性错误只通知Slack
  assert_true(high_channels.contains("Slack"))
  assert_false(high_channels.contains("Email"))
  assert_false(high_channels.contains("PagerDuty"))
  
  assert_eq(medium_channels.length(), 0) // 中等严重性错误不通知
  
  // 8. 生成错误报告
  let error_report = generate_error_report([critical_error, high_error, medium_error])
  
  // 9. 验证错误报告
  assert_eq(error_report.total_errors, 3)
  assert_eq(error_report.critical_errors, 1)
  assert_eq(error_report.high_errors, 1)
  assert_eq(error_report.medium_errors, 1)
  assert_eq(error_report.affected_services.length(), 3)
  assert_true(error_report.affected_services.contains("order-service"))
  assert_true(error_report.affected_services.contains("api-gateway"))
  assert_true(error_report.affected_services.contains("user-service"))
  
  // 10. 验证报告时间范围
  assert_eq(error_report.start_time, 1640999000000)
  assert_eq(error_report.end_time, 1640999002000)
  assert_eq(error_report.duration, 2000) // 2秒
}

// 测试7: 错误恢复自动化
test "错误恢复自动化测试" {
  // 1. 创建自动恢复配置
  let auto_recovery_config = AutoRecoveryConfig({
    recovery_actions: [
      RecoveryAction({
        trigger_error_code: "CONNECTION_TIMEOUT",
        action_type: RestartService,
        target_service: "user-service",
        max_attempts: 2,
        cooldown_period: 300 // 5分钟
      }),
      RecoveryAction({
        trigger_error_code: "OUT_OF_MEMORY",
        action_type: ScaleUp,
        target_service: "order-service",
        max_attempts: 3,
        cooldown_period: 600 // 10分钟
      }),
      RecoveryAction({
        trigger_error_code: "DATABASE_DOWN",
        action_type: Failover,
        target_service: "database",
        max_attempts: 1,
        cooldown_period: 1800 // 30分钟
      })
    ]
  })
  
  // 2. 验证自动恢复配置
  assert_eq(auto_recovery_config.recovery_actions.length(), 3)
  
  // 3. 创建触发自动恢复的错误事件
  let connection_timeout_error = ErrorEvent({
    timestamp: 1641000000000,
    error: Error({
      error_type: NetworkError,
      code: "CONNECTION_TIMEOUT",
      message: "Connection timeout",
      severity: High,
      retryable: true,
      context: [("service", "user-service")]
    }),
    service: "api-gateway",
    request_id: "req-recovery-001"
  })
  
  let out_of_memory_error = ErrorEvent({
    timestamp: 1641000001000,
    error: Error({
      error_type: SystemError,
      code: "OUT_OF_MEMORY",
      message: "Out of memory",
      severity: Critical,
      retryable: false,
      context: [("service", "order-service"), ("memory_usage", "95%")]
    }),
    service: "order-service",
    request_id: "req-recovery-002"
  })
  
  let database_down_error = ErrorEvent({
    timestamp: 1641000002000,
    error: Error({
      error_type: DatabaseError,
      code: "DATABASE_DOWN",
      message: "Database is down",
      severity: Critical,
      retryable: false,
      context: [("database", "primary-db"), ("region", "us-west")]
    }),
    service: "order-service",
    request_id: "req-recovery-003"
  })
  
  // 4. 确定恢复动作
  let timeout_recovery_action = determine_recovery_action(connection_timeout_error, auto_recovery_config)
  let memory_recovery_action = determine_recovery_action(out_of_memory_error, auto_recovery_config)
  let db_recovery_action = determine_recovery_action(database_down_error, auto_recovery_config)
  
  // 5. 验证恢复动作
  match timeout_recovery_action {
    Some(action) => {
      assert_eq(action.action_type, RestartService)
      assert_eq(action.target_service, "user-service")
    }
    None => assert_true(false)
  }
  
  match memory_recovery_action {
    Some(action) => {
      assert_eq(action.action_type, ScaleUp)
      assert_eq(action.target_service, "order-service")
    }
    None => assert_true(false)
  }
  
  match db_recovery_action {
    Some(action) => {
      assert_eq(action.action_type, Failover)
      assert_eq(action.target_service, "database")
    }
    None => assert_true(false)
  }
  
  // 6. 执行恢复动作
  let timeout_recovery_result = execute_recovery_action(timeout_recovery_action.unwrap())
  let memory_recovery_result = execute_recovery_action(memory_recovery_action.unwrap())
  let db_recovery_result = execute_recovery_action(db_recovery_action.unwrap())
  
  // 7. 验证恢复结果
  match timeout_recovery_result {
    RecoveryResult(success: true, message: _) => assert_true(true)
    RecoveryResult(success: false, message: _) => assert_true(false)
  }
  
  match memory_recovery_result {
    RecoveryResult(success: true, message: _) => assert_true(true)
    RecoveryResult(success: false, message: _) => assert_true(false)
  }
  
  match db_recovery_result {
    RecoveryResult(success: true, message: _) => assert_true(true)
    RecoveryResult(success: false, message: _) => assert_true(false)
  }
}

// 测试8: 错误处理性能影响评估
test "错误处理性能影响评估测试" {
  // 1. 创建性能基准数据（无错误情况）
  let baseline_metrics = PerformanceMetrics({
    request_count: 10000,
    successful_requests: 10000,
    failed_requests: 0,
    avg_response_time: 50.0, // 毫秒
    p95_response_time: 100.0,
    p99_response_time: 200.0,
    throughput: 200.0, // 请求/秒
    cpu_usage: 60.0, // 百分比
    memory_usage: 70.0, // 百分比
    error_rate: 0.0
  })
  
  // 2. 创建错误情况下的性能数据
  let error_metrics = PerformanceMetrics({
    request_count: 10000,
    successful_requests: 9500,
    failed_requests: 500,
    avg_response_time: 75.0, // 毫秒
    p95_response_time: 150.0,
    p99_response_time: 300.0,
    throughput: 180.0, // 请求/秒
    cpu_usage: 75.0, // 百分比
    memory_usage: 80.0, // 百分比
    error_rate: 0.05 // 5%
  })
  
  // 3. 验证性能数据
  assert_eq(baseline_metrics.request_count, error_metrics.request_count)
  assert_eq(baseline_metrics.successful_requests, 10000)
  assert_eq(error_metrics.successful_requests, 9500)
  assert_eq(baseline_metrics.failed_requests, 0)
  assert_eq(error_metrics.failed_requests, 500)
  
  // 4. 计算性能影响
  let performance_impact = calculate_performance_impact(baseline_metrics, error_metrics)
  
  // 5. 验证性能影响计算
  assert_eq(performance_impact.success_rate_impact, -0.05) // 成功率下降5%
  assert_eq(performance_impact.response_time_impact, 0.5) // 响应时间增加50%
  assert_eq(performance_impact.throughput_impact, -0.1) // 吞吐量下降10%
  assert_eq(performance_impact.cpu_impact, 0.25) // CPU使用率增加25%
  assert_eq(performance_impact.memory_impact, 0.14285714285714285) // 内存使用率增加约14.29%
  
  // 6. 评估错误处理开销
  let error_handling_overhead = calculate_error_handling_overhead(baseline_metrics, error_metrics)
  
  // 7. 验证错误处理开销
  assert_true(error_handling_overhead.total_overhead > 0)
  assert_eq(error_handling_overhead.response_time_overhead, 25.0) // 75 - 50
  assert_eq(error_handling_overhead.throughput_overhead, 20.0) // 200 - 180
  
  // 8. 分析不同错误类型的性能影响
  let error_type_impacts = [
    ErrorTypeImpact({
      error_type: NetworkError,
      frequency: 0.6, // 60%的错误是网络错误
      avg_handling_time: 100.0, // 平均处理时间100毫秒
      resource_overhead: 0.3 // 30%资源开销
    }),
    ErrorTypeImpact({
      error_type: ValidationError,
      frequency: 0.3, // 30%的错误是验证错误
      avg_handling_time: 20.0, // 平均处理时间20毫秒
      resource_overhead: 0.1 // 10%资源开销
    }),
    ErrorTypeImpact({
      error_type: SystemError,
      frequency: 0.1, // 10%的错误是系统错误
      avg_handling_time: 500.0, // 平均处理时间500毫秒
      resource_overhead: 0.8 // 80%资源开销
    })
  ]
  
  // 9. 计算加权平均性能影响
  let weighted_avg_handling_time = error_type_impacts.reduce(fn(acc, impact) {
    acc + impact.frequency * impact.avg_handling_time
  }, 0.0)
  
  let weighted_avg_resource_overhead = error_type_impacts.reduce(fn(acc, impact) {
    acc + impact.frequency * impact.resource_overhead
  }, 0.0)
  
  // 10. 验证加权平均值
  assert_eq(weighted_avg_handling_time, 116.0) // 0.6*100 + 0.3*20 + 0.1*500
  assert_eq(weighted_avg_resource_overhead, 0.29) // 0.6*0.3 + 0.3*0.1 + 0.1*0.8
  
  // 11. 识别性能瓶颈
  let performance_bottlenecks = identify_performance_bottlenecks(error_type_impacts)
  
  // 12. 验证性能瓶颈识别
  assert_eq(performance_bottlenecks.length(), 1) // 系统错误是主要瓶颈
  
  let bottleneck = performance_bottlenecks[0]
  assert_eq(bottleneck.error_type, SystemError)
  assert_eq(bottleneck.reason, "Highest handling time and resource overhead")
}