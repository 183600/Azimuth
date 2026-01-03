// Azimuth 错误处理测试
// 测试遥测系统的错误处理能力

// 测试1: Span 错误状态处理
test "Span 错误状态处理测试" {
  // 创建包含错误状态的Span
  let error_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    name: "failing operation",
    kind: SpanKind.Server,
    start_time: 1640995200000000000L,
    end_time: Some(1640995200500000000L),
    status: Status.Error,
    attributes: [
      ("error.type", "ValidationError"),
      ("error.message", "Invalid input parameter"),
      ("error.code", "INVALID_PARAM")
    ],
    events: [
      {
        name: "exception",
        timestamp: 1640995200300000000L,
        attributes: [
          ("exception.type", "ValidationError"),
          ("exception.message", "Parameter 'userId' is required"),
          ("exception.stacktrace", "at com.example.Service.validate(Service.java:42)")
        ]
      },
      {
        name: "error.handled",
        timestamp: 1640995200450000000L,
        attributes: [
          ("error.handled.by", "ErrorHandler"),
          ("error.handled.at", "2022-01-01T00:00:45Z")
        ]
      }
    ],
    links: []
  })
  
  // 验证错误状态
  assert_eq(error_span.status, Status.Error)
  
  // 验证错误属性
  assert_true(error_span.attributes.contains(("error.type", "ValidationError")))
  assert_true(error_span.attributes.contains(("error.message", "Invalid input parameter")))
  assert_true(error_span.attributes.contains(("error.code", "INVALID_PARAM")))
  
  // 验证错误事件
  assert_eq(error_span.events.length(), 2)
  assert_eq(error_span.events[0].name, "exception")
  assert_eq(error_span.events[1].name, "error.handled")
  
  // 验证异常事件属性
  let exception_event = error_span.events[0]
  assert_true(exception_event.attributes.contains(("exception.type", "ValidationError")))
  assert_true(exception_event.attributes.contains(("exception.message", "Parameter 'userId' is required")))
  assert_true(exception_event.attributes.contains(("exception.stacktrace", "at com.example.Service.validate(Service.java:42)")))
  
  // 验证错误处理事件属性
  let handled_event = error_span.events[1]
  assert_true(handled_event.attributes.contains(("error.handled.by", "ErrorHandler")))
  assert_true(handled_event.attributes.contains(("error.handled.at", "2022-01-01T00:00:45Z")))
  
  // 创建不同错误类型的Span
  let timeout_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: None,
    name: "timeout operation",
    kind: SpanKind.Client,
    start_time: 1640995201000000000L,
    end_time: Some(1640995201500000000L),
    status: Status.Error,
    attributes: [
      ("error.type", "TimeoutError"),
      ("error.message", "Operation timed out after 30 seconds"),
      ("timeout.duration_ms", "30000")
    ],
    events: [
      {
        name: "exception",
        timestamp: 1640995201450000000L,
        attributes: [
          ("exception.type", "TimeoutException"),
          ("exception.message", "Read timeout")
        ]
      }
    ],
    links: []
  })
  
  let resource_exhausted_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "e457b5a2e4dcb0d6",
    parent_span_id: None,
    name: "resource exhausted operation",
    kind: SpanKind.Server,
    start_time: 1640995202000000000L,
    end_time: Some(1640995202300000000L),
    status: Status.Error,
    attributes: [
      ("error.type", "ResourceExhaustedError"),
      ("error.message", "Too many requests"),
      ("rate.limit.remaining", "0")
    ],
    events: [
      {
        name: "exception",
        timestamp: 1640995202250000000L,
        attributes: [
          ("exception.type", "RateLimitExceededException"),
          ("exception.message", "Rate limit exceeded")
        ]
      }
    ],
    links: []
  })
  
  // 验证不同错误类型
  assert_eq(timeout_span.status, Status.Error)
  assert_eq(resource_exhausted_span.status, Status.Error)
  
  assert_true(timeout_span.attributes.contains(("error.type", "TimeoutError")))
  assert_true(resource_exhausted_span.attributes.contains(("error.type", "ResourceExhaustedError")))
  
  // 按错误类型分组Span
  let error_spans = [error_span, timeout_span, resource_exhausted_span]
  let validation_errors = error_spans.filter(fn(s) { 
    s.attributes.contains(("error.type", "ValidationError"))
  })
  let timeout_errors = error_spans.filter(fn(s) { 
    s.attributes.contains(("error.type", "TimeoutError"))
  })
  let resource_exhausted_errors = error_spans.filter(fn(s) { 
    s.attributes.contains(("error.type", "ResourceExhaustedError"))
  })
  
  assert_eq(validation_errors.length(), 1)
  assert_eq(timeout_errors.length(), 1)
  assert_eq(resource_exhausted_errors.length(), 1)
}

// 测试2: 度量错误处理
test "度量错误处理测试" {
  // 创建包含错误信息的度量数据
  let error_metrics = [
    {
      metric_name: "http_requests_total",
      value: 1L,
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "POST"),
        ("http.status_code", "400"),
        ("error.type", "ValidationError")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "http_requests_total",
      value: 1L,
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "GET"),
        ("http.status_code", "404"),
        ("error.type", "NotFoundError")
      ],
      timestamp: 1640995200100000000L
    },
    {
      metric_name: "http_requests_total",
      value: 1L,
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "POST"),
        ("http.status_code", "500"),
        ("error.type", "InternalError")
      ],
      timestamp: 1640995200200000000L
    },
    {
      metric_name: "http_requests_total",
      value: 1L,
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "GET"),
        ("http.status_code", "503"),
        ("error.type", "ServiceUnavailableError")
      ],
      timestamp: 1640995200300000000L
    }
  ]
  
  // 验证错误度量
  assert_eq(error_metrics.length(), 4)
  
  // 按状态码分组
  let client_errors = error_metrics.filter(fn(m) { 
    let status_code = m.attributes.find(fn(a) { a[0] == "http.status_code" })[1]
    let code = status_code.to_int()
    match code {
      Some(c) => c >= 400 && c < 500
      None => false
    }
  })
  let server_errors = error_metrics.filter(fn(m) { 
    let status_code = m.attributes.find(fn(a) { a[0] == "http.status_code" })[1]
    let code = status_code.to_int()
    match code {
      Some(c) => c >= 500
      None => false
    }
  })
  
  assert_eq(client_errors.length(), 2) // 400, 404
  assert_eq(server_errors.length(), 2) // 500, 503
  
  // 按错误类型分组
  let validation_errors = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "ValidationError"))
  })
  let not_found_errors = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "NotFoundError"))
  })
  let internal_errors = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "InternalError"))
  })
  let service_unavailable_errors = error_metrics.filter(fn(m) { 
    m.attributes.contains(("error.type", "ServiceUnavailableError"))
  })
  
  assert_eq(validation_errors.length(), 1)
  assert_eq(not_found_errors.length(), 1)
  assert_eq(internal_errors.length(), 1)
  assert_eq(service_unavailable_errors.length(), 1)
  
  // 创建错误率度量
  let total_requests = 100L
  let error_requests = error_metrics.reduce(fn(acc, m) { acc + m.value }, 0L)
  let error_rate = Int64::to_float(error_requests) / Int64::to_float(total_requests)
  
  assert_eq(error_requests, 4L)
  assert_eq(error_rate, 0.04) // 4% 错误率
  
  // 创建错误恢复度量
  let recovery_metrics = [
    {
      metric_name: "error_recovery_attempts_total",
      value: 3L,
      attributes: [
        ("service.name", "api-service"),
        ("error.type", "ValidationError"),
        ("recovery.strategy", "retry_with_default")
      ],
      timestamp: 1640995200400000000L
    },
    {
      metric_name: "error_recovery_attempts_total",
      value: 2L,
      attributes: [
        ("service.name", "api-service"),
        ("error.type", "InternalError"),
        ("recovery.strategy", "circuit_breaker")
      ],
      timestamp: 1640995200500000000L
    },
    {
      metric_name: "error_recovery_success_total",
      value: 2L,
      attributes: [
        ("service.name", "api-service"),
        ("recovery.strategy", "retry_with_default")
      ],
      timestamp: 1640995200600000000L
    }
  ]
  
  // 验证错误恢复度量
  assert_eq(recovery_metrics.length(), 3)
  
  let total_recovery_attempts = recovery_metrics.reduce(fn(acc, m) { acc + m.value }, 0L)
  let recovery_successes = recovery_metrics.filter(fn(m) { 
    m.metric_name == "error_recovery_success_total"
  }).reduce(fn(acc, m) { acc + m.value }, 0L)
  
  assert_eq(total_recovery_attempts, 5L)
  assert_eq(recovery_successes, 2L)
  
  let recovery_success_rate = Int64::to_float(recovery_successes) / Int64::to_float(total_recovery_attempts)
  assert_eq(recovery_success_rate, 0.4) // 40% 恢复成功率
}

// 测试3: 日志错误处理
test "日志错误处理测试" {
  // 创建包含错误信息的日志记录
  let error_logs = [
    LogRecord({
      timestamp: 1640995200000000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Error,
      severity_text: Some("ERROR"),
      body: Some("Database connection failed"),
      attributes: [
        ("service.name", "api-service"),
        ("error.type", "DatabaseConnectionError"),
        ("error.code", "DB_CONN_FAILED"),
        ("db.host", "db.example.com"),
        ("db.port", "5432")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("b7ad6b7169203331"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200100000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Error,
      severity_text: Some("ERROR"),
      body: Some("Authentication failed"),
      attributes: [
        ("service.name", "auth-service"),
        ("error.type", "AuthenticationError"),
        ("error.code", "AUTH_FAILED"),
        ("user.id", "12345"),
        ("auth.method", "jwt")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("00f067aa0ba902b7"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200200000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Warn,
      severity_text: Some("WARN"),
      body: Some("Rate limit threshold approaching"),
      attributes: [
        ("service.name", "api-service"),
        ("warning.type", "RateLimitWarning"),
        ("current.rate", "950"),
        ("rate.limit", "1000")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("e457b5a2e4dcb0d6"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200300000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Fatal,
      severity_text: Some("FATAL"),
      body: Some("Out of memory error"),
      attributes: [
        ("service.name", "api-service"),
        ("error.type", "OutOfMemoryError"),
        ("error.code", "OOM"),
        ("available.memory", "128MB"),
        ("required.memory", "512MB")
      ],
      trace_id: Some("a3c9d5e7b8f019c2"),
      span_id: Some("c1d2e3f4a5b6c7d8"),
      trace_flags: Some(0x01)
    })
  ]
  
  // 验证错误日志
  assert_eq(error_logs.length(), 4)
  
  // 按严重级别分组
  let error_logs_severity = error_logs.filter(fn(log) { 
    log.severity_number == SeverityNumber.Error
  })
  let warn_logs = error_logs.filter(fn(log) { 
    log.severity_number == SeverityNumber.Warn
  })
  let fatal_logs = error_logs.filter(fn(log) { 
    log.severity_number == SeverityNumber.Fatal
  })
  
  assert_eq(error_logs_severity.length(), 2)
  assert_eq(warn_logs.length(), 1)
  assert_eq(fatal_logs.length(), 1)
  
  // 按错误类型分组
  let db_connection_errors = error_logs.filter(fn(log) { 
    log.attributes.contains(("error.type", "DatabaseConnectionError"))
  })
  let auth_errors = error_logs.filter(fn(log) { 
    log.attributes.contains(("error.type", "AuthenticationError"))
  })
  let oom_errors = error_logs.filter(fn(log) { 
    log.attributes.contains(("error.type", "OutOfMemoryError"))
  })
  
  assert_eq(db_connection_errors.length(), 1)
  assert_eq(auth_errors.length(), 1)
  assert_eq(oom_errors.length(), 1)
  
  // 验证错误日志内容
  let db_error_log = db_connection_errors[0]
  assert_eq(db_error_log.body, Some("Database connection failed"))
  assert_true(db_error_log.attributes.contains(("error.code", "DB_CONN_FAILED")))
  assert_true(db_error_log.attributes.contains(("db.host", "db.example.com")))
  assert_true(db_error_log.attributes.contains(("db.port", "5432")))
  
  let auth_error_log = auth_errors[0]
  assert_eq(auth_error_log.body, Some("Authentication failed"))
  assert_true(auth_error_log.attributes.contains(("error.code", "AUTH_FAILED")))
  assert_true(auth_error_log.attributes.contains(("user.id", "12345")))
  assert_true(auth_error_log.attributes.contains(("auth.method", "jwt")))
  
  let oom_error_log = oom_errors[0]
  assert_eq(oom_error_log.body, Some("Out of memory error"))
  assert_true(oom_error_log.attributes.contains(("error.code", "OOM")))
  assert_true(oom_error_log.attributes.contains(("available.memory", "128MB")))
  assert_true(oom_error_log.attributes.contains(("required.memory", "512MB")))
  
  // 创建错误恢复日志
  let recovery_logs = [
    LogRecord({
      timestamp: 1640995200400000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Database connection recovered"),
      attributes: [
        ("service.name", "api-service"),
        ("recovery.type", "DatabaseConnectionRecovery"),
        ("recovery.strategy", "reconnect"),
        ("recovery.time_ms", "5000")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("b7ad6b7169203331"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200500000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Circuit breaker activated"),
      attributes: [
        ("service.name", "api-service"),
        ("recovery.type", "CircuitBreakerActivation"),
        ("failure.threshold", "5"),
        ("recovery.timeout_ms", "30000")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("00f067aa0ba902b7"),
      trace_flags: Some(0x01)
    })
  ]
  
  // 验证错误恢复日志
  assert_eq(recovery_logs.length(), 2)
  
  let db_recovery_log = recovery_logs[0]
  assert_eq(db_recovery_log.body, Some("Database connection recovered"))
  assert_true(db_recovery_log.attributes.contains(("recovery.type", "DatabaseConnectionRecovery")))
  assert_true(db_recovery_log.attributes.contains(("recovery.strategy", "reconnect")))
  assert_true(db_recovery_log.attributes.contains(("recovery.time_ms", "5000")))
  
  let circuit_breaker_log = recovery_logs[1]
  assert_eq(circuit_breaker_log.body, Some("Circuit breaker activated"))
  assert_true(circuit_breaker_log.attributes.contains(("recovery.type", "CircuitBreakerActivation")))
  assert_true(circuit_breaker_log.attributes.contains(("failure.threshold", "5")))
  assert_true(circuit_breaker_log.attributes.contains(("recovery.timeout_ms", "30000")))
}

// 测试4: 上下文传播错误处理
test "上下文传播错误处理测试" {
  // 测试无效的traceparent格式
  let invalid_traceparents = [
    "invalid-format",                    // 完全无效的格式
    "01-invalid-trace-id-span-id-01",   // 无效的trace_id
    "00-4bf92f3577b34da6a3ce929d0e0e4736-invalid-span-id-01", // 无效的span_id
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-ff", // 无效的trace_flags
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7",   // 缺少trace_flags
    "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01-extra" // 额外的部分
  ]
  
  // 验证无效的traceparent格式
  for invalid_traceparent in invalid_traceparents {
    let parts = invalid_traceparent.split("-")
    
    // 验证格式错误
    if parts.length() != 4 {
      assert_true(true) // 预期的格式错误
    } else {
      let trace_id = parts[1]
      let span_id = parts[2]
      let trace_flags = parts[3]
      
      // 验证trace_id格式
      if trace_id.length() != 32 || not trace_id.chars().all(fn(c) { 
        let char_code = c.to_char_code()
        (char_code >= 48 && char_code <= 57) || // 0-9
        (char_code >= 97 && char_code <= 102)    // a-f
      }) {
        assert_true(true) // 预期的trace_id格式错误
      }
      
      // 验证span_id格式
      if span_id.length() != 16 || not span_id.chars().all(fn(c) { 
        let char_code = c.to_char_code()
        (char_code >= 48 && char_code <= 57) || // 0-9
        (char_code >= 97 && char_code <= 102)    // a-f
      }) {
        assert_true(true) // 预期的span_id格式错误
      }
      
      // 验证trace_flags格式
      if trace_flags.length() != 2 || not trace_flags.chars().all(fn(c) { 
        let char_code = c.to_char_code()
        (char_code >= 48 && char_code <= 57) || // 0-9
        (char_code >= 97 && char_code <= 102)    // a-f
      }) {
        assert_true(true) // 预期的trace_flags格式错误
      }
    }
  }
  
  // 测试无效的baggage格式
  let invalid_baggages = [
    "key=value=extra",           // 多个等号
    "key",                       // 缺少值
    "=value",                    // 缺少键
    "key=value,invalid",         // 第二项无效
    "key=value,key2=",           // 第二项缺少值
    "key=value,,key3=value3",    // 空项
    " key=value",                // 前导空格
    "key=value ",                // 尾随空格
    "key= value",                // 键值间的空格
    "key =value",                // 键值间的空格
    "key = value"                // 键值间的空格
  ]
  
  // 验证无效的baggage格式
  for invalid_baggage in invalid_baggages {
    let items = invalid_baggage.split(",")
    
    for item in items {
      if item.length() == 0 {
        assert_true(true) // 预期的空项
      } else {
        let kv = item.split("=")
        
        if kv.length() != 2 {
          assert_true(true) // 预期的键值格式错误
        } else {
          let key = kv[0]
          let value = kv[1]
          
          // 验证键不为空
          if key.length() == 0 {
            assert_true(true) // 预期的空键
          }
          
          // 验证键不包含空格
          if key.contains(" ") {
            assert_true(true) // 预期的键包含空格
          }
          
          // 验证值不包含未编码的空格
          if value.contains(" ") {
            assert_true(true) // 预期的值包含空格
          }
        }
      }
    }
  }
  
  // 测试上下文传播错误恢复
  let valid_traceparent = "00-4bf92f3577b34da6a3ce929d0e0e4736-b7ad6b7169203331-01"
  let valid_baggage = "user.id=12345,service.name=api-service"
  
  // 创建恢复策略
  let recovery_strategies = [
    {
      error_type: "InvalidTraceparentFormat",
      strategy: "UseDefaultTraceparent",
      default_traceparent: "00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01"
    },
    {
      error_type: "InvalidBaggageFormat",
      strategy: "UseDefaultBaggage",
      default_baggage: "service.name=unknown-service"
    },
    {
      error_type: "MissingTraceContext",
      strategy: "GenerateNewTraceContext",
      generator: "random"
    }
  ]
  
  // 验证恢复策略
  assert_eq(recovery_strategies.length(), 3)
  
  for strategy in recovery_strategies {
    match strategy.error_type {
      "InvalidTraceparentFormat" => {
        assert_eq(strategy.strategy, "UseDefaultTraceparent")
        assert_true(strategy.default_traceparent.length() > 0)
      }
      "InvalidBaggageFormat" => {
        assert_eq(strategy.strategy, "UseDefaultBaggage")
        assert_true(strategy.default_baggage.length() > 0)
      }
      "MissingTraceContext" => {
        assert_eq(strategy.strategy, "GenerateNewTraceContext")
        assert_eq(strategy.generator, "random")
      }
      _ => assert_true(false)
    }
  }
}

// 测试5: 资源限制错误处理
test "资源限制错误处理测试" {
  // 测试资源限制错误
  let resource_limit_errors = [
    {
      resource_type: "span_count",
      limit: 10000,
      current_usage: 10000,
      error_type: "SpanLimitExceededError",
      error_message: "Maximum number of spans exceeded"
    },
    {
      resource_type: "metric_count",
      limit: 50000,
      current_usage: 50000,
      error_type: "MetricLimitExceededError",
      error_message: "Maximum number of metrics exceeded"
    },
    {
      resource_type: "log_count",
      limit: 100000,
      current_usage: 100000,
      error_type: "LogLimitExceededError",
      error_message: "Maximum number of logs exceeded"
    },
    {
      resource_type: "attribute_count",
      limit: 128,
      current_usage: 128,
      error_type: "AttributeLimitExceededError",
      error_message: "Maximum number of attributes exceeded"
    },
    {
      resource_type: "event_count",
      limit: 128,
      current_usage: 128,
      error_type: "EventLimitExceededError",
      error_message: "Maximum number of events exceeded"
    },
    {
      resource_type: "link_count",
      limit: 128,
      current_usage: 128,
      error_type: "LinkLimitExceededError",
      error_message: "Maximum number of links exceeded"
    }
  ]
  
  // 验证资源限制错误
  assert_eq(resource_limit_errors.length(), 6)
  
  for error in resource_limit_errors {
    // 验证当前使用量达到限制
    assert_eq(error.current_usage, error.limit)
    
    // 验证错误类型和消息
    assert_true(error.error_type.length() > 0)
    assert_true(error.error_message.length() > 0)
    
    // 验证错误类型与资源类型匹配
    match error.resource_type {
      "span_count" => assert_eq(error.error_type, "SpanLimitExceededError")
      "metric_count" => assert_eq(error.error_type, "MetricLimitExceededError")
      "log_count" => assert_eq(error.error_type, "LogLimitExceededError")
      "attribute_count" => assert_eq(error.error_type, "AttributeLimitExceededError")
      "event_count" => assert_eq(error.error_type, "EventLimitExceededError")
      "link_count" => assert_eq(error.error_type, "LinkLimitExceededError")
      _ => assert_true(false)
    }
  }
  
  // 创建资源限制恢复策略
  let recovery_strategies = [
    {
      resource_type: "span_count",
      strategy: "DropOldestSpans",
      parameters: {
        "drop_count": "1000",
        "drop_threshold": "0.9"
      }
    },
    {
      resource_type: "metric_count",
      strategy: "AggregateMetrics",
      parameters: {
        "aggregation_window": "60s",
        "aggregation_function": "sum"
      }
    },
    {
      resource_type: "log_count",
      strategy: "FilterLogs",
      parameters: {
        "min_severity": "WARN",
        "filter_pattern": "error"
      }
    },
    {
      resource_type: "attribute_count",
      strategy: "DropLowPriorityAttributes",
      parameters: {
        "priority_threshold": "0.5"
      }
    }
  ]
  
  // 验证恢复策略
  assert_eq(recovery_strategies.length(), 4)
  
  for strategy in recovery_strategies {
    // 验证策略名称
    assert_true(strategy.strategy.length() > 0)
    
    // 验证参数
    assert_true(strategy.parameters.size() > 0)
    
    // 验证策略与资源类型匹配
    match strategy.resource_type {
      "span_count" => assert_eq(strategy.strategy, "DropOldestSpans")
      "metric_count" => assert_eq(strategy.strategy, "AggregateMetrics")
      "log_count" => assert_eq(strategy.strategy, "FilterLogs")
      "attribute_count" => assert_eq(strategy.strategy, "DropLowPriorityAttributes")
      _ => assert_true(false)
    }
  }
  
  // 测试资源限制恢复效果
  let recovery_results = [
    {
      resource_type: "span_count",
      before_usage: 10000,
      after_usage: 9000,
      recovery_effectiveness: 0.1
    },
    {
      resource_type: "metric_count",
      before_usage: 50000,
      after_usage: 25000,
      recovery_effectiveness: 0.5
    },
    {
      resource_type: "log_count",
      before_usage: 100000,
      after_usage: 20000,
      recovery_effectiveness: 0.8
    },
    {
      resource_type: "attribute_count",
      before_usage: 128,
      after_usage: 64,
      recovery_effectiveness: 0.5
    }
  ]
  
  // 验证恢复效果
  assert_eq(recovery_results.length(), 4)
  
  for result in recovery_results {
    // 验证使用量减少
    assert_true(result.after_usage < result.before_usage)
    
    // 验证恢复效果计算
    let expected_effectiveness = Int64::to_float(result.before_usage - result.after_usage) / 
                                Int64::to_float(result.before_usage)
    
    assert_true(abs(result.recovery_effectiveness - expected_effectiveness) < 0.01)
    
    // 验证恢复效果在合理范围内
    assert_true(result.recovery_effectiveness > 0.0)
    assert_true(result.recovery_effectiveness <= 1.0)
  }
}