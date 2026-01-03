// Azimuth 日志遥测测试
// 测试日志记录与遥测系统的集成

// 测试1: 基本日志记录
test "基本日志记录测试" {
  // 创建日志记录器
  let logger = Logger({
    name: "azimuth.logger",
    version: Some("1.0.0"),
    schema_url: Some("https://opentelemetry.io/schemas/1.20.0")
  })
  
  // 验证日志记录器属性
  assert_eq(logger.name, "azimuth.logger")
  match logger.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  match logger.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://opentelemetry.io/schemas/1.20.0")
    None => assert_true(false)
  }
  
  // 创建日志记录
  let log_record = LogRecord({
    timestamp: 1640995200000000000L, // 2022-01-01 00:00:00 UTC in nanos
    observed_timestamp: Some(1640995200000100000L),
    severity_number: SeverityNumber.Info,
    severity_text: Some("INFO"),
    body: Some("User login successful"),
    attributes: [
      ("user.id", "12345"),
      ("user.name", "john.doe"),
      ("ip.address", "192.168.1.100")
    ],
    trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
    span_id: Some("b7ad6b7169203331"),
    trace_flags: Some(0x01)
  })
  
  // 验证日志记录
  assert_eq(log_record.timestamp, 1640995200000000000L)
  match log_record.observed_timestamp {
    Some(observed_timestamp) => assert_eq(observed_timestamp, 1640995200000100000L)
    None => assert_true(false)
  }
  assert_eq(log_record.severity_number, SeverityNumber.Info)
  match log_record.severity_text {
    Some(severity_text) => assert_eq(severity_text, "INFO")
    None => assert_true(false)
  }
  match log_record.body {
    Some(body) => assert_eq(body, "User login successful")
    None => assert_true(false)
  }
  assert_eq(log_record.attributes.length(), 3)
  assert_true(log_record.attributes.contains(("user.id", "12345")))
  assert_true(log_record.attributes.contains(("user.name", "john.doe")))
  assert_true(log_record.attributes.contains(("ip.address", "192.168.1.100")))
  
  // 验证追踪关联
  match log_record.trace_id {
    Some(trace_id) => assert_eq(trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
    None => assert_true(false)
  }
  match log_record.span_id {
    Some(span_id) => assert_eq(span_id, "b7ad6b7169203331")
    None => assert_true(false)
  }
  match log_record.trace_flags {
    Some(trace_flags) => assert_eq(trace_flags, 0x01)
    None => assert_true(false)
  }
}

// 测试2: 不同严重级别的日志
test "不同严重级别的日志测试" {
  // 创建不同严重级别的日志记录
  let trace_log = LogRecord({
    timestamp: 1640995200000000000L,
    observed_timestamp: None,
    severity_number: SeverityNumber.Trace,
    severity_text: Some("TRACE"),
    body: Some("Entering function process_request"),
    attributes: [
      ("function.name", "process_request"),
      ("line.number", "42")
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None
  })
  
  let debug_log = LogRecord({
    timestamp: 1640995200100000000L,
    observed_timestamp: None,
    severity_number: SeverityNumber.Debug,
    severity_text: Some("DEBUG"),
    body: Some("Database connection established"),
    attributes: [
      ("db.connection.id", "conn-12345"),
      ("db.host", "localhost")
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None
  })
  
  let info_log = LogRecord({
    timestamp: 1640995200200000000L,
    observed_timestamp: None,
    severity_number: SeverityNumber.Info,
    severity_text: Some("INFO"),
    body: Some("Request processed successfully"),
    attributes: [
      ("request.id", "req-67890"),
      ("response.time_ms", "150")
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None
  })
  
  let warn_log = LogRecord({
    timestamp: 1640995200300000000L,
    observed_timestamp: None,
    severity_number: SeverityNumber.Warn,
    severity_text: Some("WARN"),
    body: Some("Rate limit threshold approaching"),
    attributes: [
      ("current.rate", "950"),
      ("limit", "1000")
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None
  })
  
  let error_log = LogRecord({
    timestamp: 1640995200400000000L,
    observed_timestamp: None,
    severity_number: SeverityNumber.Error,
    severity_text: Some("ERROR"),
    body: Some("Database query failed"),
    attributes: [
      ("error.code", "DB_TIMEOUT"),
      ("query", "SELECT * FROM users WHERE id = $1")
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None
  })
  
  let fatal_log = LogRecord({
    timestamp: 1640995200500000000L,
    observed_timestamp: None,
    severity_number: SeverityNumber.Fatal,
    severity_text: Some("FATAL"),
    body: Some("Application cannot start"),
    attributes: [
      ("error.type", "OutOfMemoryError"),
      ("available.memory", "128MB")
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None
  })
  
  // 验证严重级别
  assert_eq(trace_log.severity_number, SeverityNumber.Trace)
  assert_eq(debug_log.severity_number, SeverityNumber.Debug)
  assert_eq(info_log.severity_number, SeverityNumber.Info)
  assert_eq(warn_log.severity_number, SeverityNumber.Warn)
  assert_eq(error_log.severity_number, SeverityNumber.Error)
  assert_eq(fatal_log.severity_number, SeverityNumber.Fatal)
  
  // 验证严重级别文本
  match trace_log.severity_text {
    Some(severity_text) => assert_eq(severity_text, "TRACE")
    None => assert_true(false)
  }
  match debug_log.severity_text {
    Some(severity_text) => assert_eq(severity_text, "DEBUG")
    None => assert_true(false)
  }
  match info_log.severity_text {
    Some(severity_text) => assert_eq(severity_text, "INFO")
    None => assert_true(false)
  }
  match warn_log.severity_text {
    Some(severity_text) => assert_eq(severity_text, "WARN")
    None => assert_true(false)
  }
  match error_log.severity_text {
    Some(severity_text) => assert_eq(severity_text, "ERROR")
    None => assert_true(false)
  }
  match fatal_log.severity_text {
    Some(severity_text) => assert_eq(severity_text, "FATAL")
    None => assert_true(false)
  }
  
  // 按严重级别过滤日志
  let logs = [trace_log, debug_log, info_log, warn_log, error_log, fatal_log]
  
  let error_and_above = logs.filter(fn(log) { 
    log.severity_number >= SeverityNumber.Error 
  })
  assert_eq(error_and_above.length(), 2) // ERROR 和 FATAL
  
  let warn_and_above = logs.filter(fn(log) { 
    log.severity_number >= SeverityNumber.Warn 
  })
  assert_eq(warn_and_above.length(), 3) // WARN, ERROR 和 FATAL
  
  let info_and_below = logs.filter(fn(log) { 
    log.severity_number <= SeverityNumber.Info 
  })
  assert_eq(info_and_below.length(), 3) // TRACE, DEBUG 和 INFO
}

// 测试3: 日志与追踪关联
test "日志与追踪关联测试" {
  // 创建与追踪关联的日志记录
  let trace_related_logs = [
    LogRecord({
      timestamp: 1640995200000000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Starting request processing"),
      attributes: [
        ("http.method", "GET"),
        ("http.url", "/api/users")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("b7ad6b7169203331"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200100000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Debug,
      severity_text: Some("DEBUG"),
      body: Some("Querying database"),
      attributes: [
        ("db.query", "SELECT * FROM users")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("00f067aa0ba902b7"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200200000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Database query completed"),
      attributes: [
        ("db.rows.returned", "25")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("00f067aa0ba902b7"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200300000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Request processed successfully"),
      attributes: [
        ("http.status_code", "200")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("b7ad6b7169203331"),
      trace_flags: Some(0x01)
    })
  ]
  
  // 验证所有日志记录都关联到同一个 trace
  for log in trace_related_logs {
    match log.trace_id {
      Some(trace_id) => assert_eq(trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
      None => assert_true(false)
    }
    match log.trace_flags {
      Some(trace_flags) => assert_eq(trace_flags, 0x01)
      None => assert_true(false)
    }
  }
  
  // 按 span_id 分组日志
  let root_span_logs = trace_related_logs.filter(fn(log) {
    match log.span_id {
      Some(span_id) => span_id == "b7ad6b7169203331"
      None => false
    }
  })
  let child_span_logs = trace_related_logs.filter(fn(log) {
    match log.span_id {
      Some(span_id) => span_id == "00f067aa0ba902b7"
      None => false
    }
  })
  
  assert_eq(root_span_logs.length(), 2)
  assert_eq(child_span_logs.length(), 2)
  
  // 验证 root span 日志
  assert_eq(root_span_logs[0].body, Some("Starting request processing"))
  assert_eq(root_span_logs[1].body, Some("Request processed successfully"))
  
  // 验证 child span 日志
  assert_eq(child_span_logs[0].body, Some("Querying database"))
  assert_eq(child_span_logs[1].body, Some("Database query completed"))
}

// 测试4: 结构化日志记录
test "结构化日志记录测试" {
  // 创建结构化日志记录
  let structured_logs = [
    LogRecord({
      timestamp: 1640995200000000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("HTTP request processed"),
      attributes: [
        ("event.name", "http.request"),
        ("http.method", "POST"),
        ("http.url", "/api/orders"),
        ("http.status_code", "201"),
        ("http.response_time_ms", "250"),
        ("user.id", "12345"),
        ("order.id", "order-67890"),
        ("order.amount", "99.99"),
        ("order.currency", "USD")
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
      body: Some("Payment processing failed"),
      attributes: [
        ("event.name", "payment.error"),
        ("error.type", "PaymentDeclined"),
        ("error.message", "Insufficient funds"),
        ("payment.id", "pay-11111"),
        ("payment.amount", "199.99"),
        ("payment.currency", "USD"),
        ("user.id", "67890"),
        ("card.last4", "1234")
      ],
      trace_id: Some("a3c9d5e7b8f019c2"),
      span_id: Some("e457b5a2e4dcb0d6"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200200000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Warn,
      severity_text: Some("WARN"),
      body: Some("Cache miss rate high"),
      attributes: [
        ("event.name", "cache.performance"),
        ("cache.name", "user_sessions"),
        ("cache.hit_rate", "0.65"),
        ("cache.miss_rate", "0.35"),
        ("cache.total_requests", "10000"),
        ("cache.misses", "3500"),
        ("threshold", "0.3")
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None
    })
  ]
  
  // 验证结构化日志属性
  let http_log = structured_logs[0]
  assert_eq(http_log.attributes.length(), 9)
  assert_true(http_log.attributes.contains(("event.name", "http.request")))
  assert_true(http_log.attributes.contains(("http.method", "POST")))
  assert_true(http_log.attributes.contains(("http.url", "/api/orders")))
  assert_true(http_log.attributes.contains(("http.status_code", "201")))
  assert_true(http_log.attributes.contains(("http.response_time_ms", "250")))
  assert_true(http_log.attributes.contains(("user.id", "12345")))
  assert_true(http_log.attributes.contains(("order.id", "order-67890")))
  assert_true(http_log.attributes.contains(("order.amount", "99.99")))
  assert_true(http_log.attributes.contains(("order.currency", "USD")))
  
  let payment_log = structured_logs[1]
  assert_eq(payment_log.attributes.length(), 8)
  assert_true(payment_log.attributes.contains(("event.name", "payment.error")))
  assert_true(payment_log.attributes.contains(("error.type", "PaymentDeclined")))
  assert_true(payment_log.attributes.contains(("error.message", "Insufficient funds")))
  assert_true(payment_log.attributes.contains(("payment.id", "pay-11111")))
  assert_true(payment_log.attributes.contains(("payment.amount", "199.99")))
  assert_true(payment_log.attributes.contains(("payment.currency", "USD")))
  assert_true(payment_log.attributes.contains(("user.id", "67890")))
  assert_true(payment_log.attributes.contains(("card.last4", "1234")))
  
  let cache_log = structured_logs[2]
  assert_eq(cache_log.attributes.length(), 7)
  assert_true(cache_log.attributes.contains(("event.name", "cache.performance")))
  assert_true(cache_log.attributes.contains(("cache.name", "user_sessions")))
  assert_true(cache_log.attributes.contains(("cache.hit_rate", "0.65")))
  assert_true(cache_log.attributes.contains(("cache.miss_rate", "0.35")))
  assert_true(cache_log.attributes.contains(("cache.total_requests", "10000")))
  assert_true(cache_log.attributes.contains(("cache.misses", "3500")))
  assert_true(cache_log.attributes.contains(("threshold", "0.3")))
  
  // 按事件名称过滤日志
  let http_request_logs = structured_logs.filter(fn(log) {
    log.attributes.contains(("event.name", "http.request"))
  })
  let payment_error_logs = structured_logs.filter(fn(log) {
    log.attributes.contains(("event.name", "payment.error"))
  })
  let cache_performance_logs = structured_logs.filter(fn(log) {
    log.attributes.contains(("event.name", "cache.performance"))
  })
  
  assert_eq(http_request_logs.length(), 1)
  assert_eq(payment_error_logs.length(), 1)
  assert_eq(cache_performance_logs.length(), 1)
}

// 测试5: 日志批处理和导出
test "日志批处理和导出测试" {
  // 创建日志记录集合
  let log_records = [
    LogRecord({
      timestamp: 1640995200000000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Application started"),
      attributes: [
        ("app.name", "azimuth-service"),
        ("app.version", "1.0.0")
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None
    }),
    LogRecord({
      timestamp: 1640995200100000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Database connected"),
      attributes: [
        ("db.type", "postgresql"),
        ("db.host", "db.example.com"),
        ("db.port", "5432")
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None
    }),
    LogRecord({
      timestamp: 1640995200200000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Server listening on port 8080"),
      attributes: [
        ("server.port", "8080"),
        ("server.protocol", "http")
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None
    })
  ]
  
  // 创建日志资源
  let resource = Resource({
    attributes: [
      ("service.name", "azimuth-service"),
      ("service.version", "1.0.0"),
      ("service.instance.id", "instance-12345"),
      ("host.name", "server-1"),
      ("os.type", "linux"),
      ("os.version", "5.4.0")
    ]
  })
  
  // 创建日志作用域
  let scope = InstrumentationScope({
    name: "azimuth.logger",
    version: Some("1.0.0"),
    attributes: [
      ("logger.type", "structured")
    ]
  })
  
  // 创建日志批处理请求
  let log_batch = ExportLogsServiceRequest({
    resource_logs: [
      {
        resource: resource,
        scope_logs: [
          {
            scope: scope,
            log_records: log_records
          }
        ]
      }
    ]
  })
  
  // 验证日志批处理请求
  assert_eq(log_batch.resource_logs.length(), 1)
  
  let resource_log = log_batch.resource_logs[0]
  assert_eq(resource_log.resource.attributes.length(), 6)
  assert_true(resource_log.resource.attributes.contains(("service.name", "azimuth-service")))
  assert_true(resource_log.resource.attributes.contains(("service.version", "1.0.0")))
  assert_true(resource_log.resource.attributes.contains(("service.instance.id", "instance-12345")))
  assert_true(resource_log.resource.attributes.contains(("host.name", "server-1")))
  assert_true(resource_log.resource.attributes.contains(("os.type", "linux")))
  assert_true(resource_log.resource.attributes.contains(("os.version", "5.4.0")))
  
  assert_eq(resource_log.scope_logs.length(), 1)
  
  let scope_log = resource_log.scope_logs[0]
  assert_eq(scope_log.scope.name, "azimuth.logger")
  match scope_log.scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  assert_true(scope_log.scope.attributes.contains(("logger.type", "structured")))
  
  assert_eq(scope_log.log_records.length(), 3)
  assert_eq(scope_log.log_records[0].body, Some("Application started"))
  assert_eq(scope_log.log_records[1].body, Some("Database connected"))
  assert_eq(scope_log.log_records[2].body, Some("Server listening on port 8080"))
}