// Azimuth 跨服务遥测测试
// 测试跨服务的遥测数据收集和传播

// 测试1: 跨服务请求追踪
test "跨服务请求追踪测试" {
  // 创建API网关服务的Span
  let gateway_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    name: "HTTP GET /api/orders",
    kind: SpanKind.Server,
    start_time: 1640995200000000000L,
    end_time: Some(1640995200500000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "api-gateway"),
      ("http.method", "GET"),
      ("http.url", "/api/orders"),
      ("http.host", "api.example.com"),
      ("http.scheme", "https"),
      ("http.status_code", "200")
    ],
    events: [],
    links: []
  })
  
  // 创建订单服务的Span
  let order_service_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    name: "HTTP GET /orders",
    kind: SpanKind.Server,
    start_time: 1640995200050000000L,
    end_time: Some(1640995200450000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "order-service"),
      ("http.method", "GET"),
      ("http.url", "/orders"),
      ("http.host", "order-service.example.com"),
      ("http.scheme", "http"),
      ("http.status_code", "200")
    ],
    events: [],
    links: []
  })
  
  // 创建数据库服务的Span
  let database_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "e457b5a2e4dcb0d6",
    parent_span_id: Some("00f067aa0ba902b7"),
    name: "SQL SELECT",
    kind: SpanKind.Client,
    start_time: 1640995200100000000L,
    end_time: Some(1640995200400000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "order-service"),
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM orders WHERE user_id = $1"),
      ("db.connection_string", "postgresql://db.example.com:5432/orders"),
      ("db.user", "order_service")
    ],
    events: [],
    links: []
  })
  
  // 验证所有Span属于同一个Trace
  assert_eq(gateway_span.trace_id, order_service_span.trace_id)
  assert_eq(gateway_span.trace_id, database_span.trace_id)
  
  // 验证Span父子关系
  assert_eq(gateway_span.parent_span_id, None) // 根Span
  match order_service_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, gateway_span.span_id)
    None => assert_true(false)
  }
  match database_span.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, order_service_span.span_id)
    None => assert_true(false)
  }
  
  // 验证服务名称
  assert_eq(gateway_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "api-gateway")
  assert_eq(order_service_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "order-service")
  assert_eq(database_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "order-service")
  
  // 验证时间顺序
  assert_true(gateway_span.start_time <= order_service_span.start_time)
  assert_true(order_service_span.start_time <= database_span.start_time)
  assert_true(database_span.start_time <= database_span.end_time)
  assert_true(database_span.end_time <= order_service_span.end_time)
  assert_true(order_service_span.end_time <= gateway_span.end_time)
  
  // 计算总请求时间和各服务处理时间
  let total_request_time = match gateway_span.end_time {
    Some(end_time) => end_time - gateway_span.start_time
    None => 0L
  }
  let gateway_processing_time = order_service_span.start_time - gateway_span.start_time
  let order_service_processing_time = match order_service_span.end_time {
    Some(end_time) => end_time - order_service_span.start_time
    None => 0L
  }
  let database_processing_time = match database_span.end_time {
    Some(end_time) => end_time - database_span.start_time
    None => 0L
  }
  
  assert_eq(total_request_time, 50000000L) // 50ms
  assert_eq(gateway_processing_time, 5000000L) // 5ms
  assert_eq(order_service_processing_time, 40000000L) // 40ms
  assert_eq(database_processing_time, 30000000L) // 30ms
}

// 测试2: 跨服务度量传播
test "跨服务度量传播测试" {
  // 创建不同服务的度量数据
  let gateway_metrics = [
    {
      metric_name: "http_requests_total",
      value: 1250L,
      attributes: [
        ("service.name", "api-gateway"),
        ("http.method", "GET"),
        ("http.status_code", "200")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "http_request_duration_seconds",
      value: 0.075,
      attributes: [
        ("service.name", "api-gateway"),
        ("http.method", "GET"),
        ("http.status_code", "200")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "active_connections",
      value: 45L,
      attributes: [
        ("service.name", "api-gateway"),
        ("connection.type", "http")
      ],
      timestamp: 1640995200000000000L
    }
  ]
  
  let order_service_metrics = [
    {
      metric_name: "http_requests_total",
      value: 1180L,
      attributes: [
        ("service.name", "order-service"),
        ("http.method", "GET"),
        ("http.status_code", "200")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "http_request_duration_seconds",
      value: 0.125,
      attributes: [
        ("service.name", "order-service"),
        ("http.method", "GET"),
        ("http.status_code", "200")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "database_connections_active",
      value: 8L,
      attributes: [
        ("service.name", "order-service"),
        ("db.system", "postgresql")
      ],
      timestamp: 1640995200000000000L
    }
  ]
  
  let database_metrics = [
    {
      metric_name: "connections_active",
      value: 25L,
      attributes: [
        ("service.name", "postgresql"),
        ("database.name", "orders")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "query_duration_seconds",
      value: 0.025,
      attributes: [
        ("service.name", "postgresql"),
        ("query.type", "SELECT")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "rows_returned_total",
      value: 15000L,
      attributes: [
        ("service.name", "postgresql"),
        ("table.name", "orders")
      ],
      timestamp: 1640995200000000000L
    }
  ]
  
  // 合并所有度量数据
  let all_metrics = gateway_metrics.concat(order_service_metrics).concat(database_metrics)
  
  // 按服务分组度量
  let gateway_service_metrics = all_metrics.filter(fn(m) { 
    m.attributes.contains(("service.name", "api-gateway"))
  })
  let order_service_service_metrics = all_metrics.filter(fn(m) { 
    m.attributes.contains(("service.name", "order-service"))
  })
  let database_service_metrics = all_metrics.filter(fn(m) { 
    m.attributes.contains(("service.name", "postgresql"))
  })
  
  assert_eq(gateway_service_metrics.length(), 3)
  assert_eq(order_service_service_metrics.length(), 3)
  assert_eq(database_service_metrics.length(), 3)
  
  // 按度量名称分组
  let http_requests_metrics = all_metrics.filter(fn(m) { 
    m.metric_name == "http_requests_total"
  })
  let duration_metrics = all_metrics.filter(fn(m) { 
    m.metric_name.contains("duration")
  })
  let connection_metrics = all_metrics.filter(fn(m) { 
    m.metric_name.contains("connection")
  })
  
  assert_eq(http_requests_metrics.length(), 2) // api-gateway 和 order-service
  assert_eq(duration_metrics.length(), 2)     // api-gateway 和 order-service
  assert_eq(connection_metrics.length(), 3)   // api-gateway, order-service 和 postgresql
  
  // 计算总请求数
  let total_requests = http_requests_metrics.reduce(fn(acc, m) { acc + m.value }, 0L)
  assert_eq(total_requests, 2430L) // 1250 + 1180
  
  // 计算平均响应时间
  let total_duration = duration_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0)
  let avg_duration = total_duration / Int64::from_int(duration_metrics.length())
  assert_true(avg_duration > 0.09 && avg_duration < 0.11) // (0.075 + 0.125) / 2 = 0.1
}

// 测试3: 跨服务日志关联
test "跨服务日志关联测试" {
  // 创建不同服务的日志记录
  let gateway_logs = [
    LogRecord({
      timestamp: 1640995200000000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Incoming request: GET /api/orders"),
      attributes: [
        ("service.name", "api-gateway"),
        ("http.method", "GET"),
        ("http.url", "/api/orders"),
        ("user.id", "12345")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("b7ad6b7169203331"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200450000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Response sent to client"),
      attributes: [
        ("service.name", "api-gateway"),
        ("http.status_code", "200"),
        ("response.size.bytes", "1024")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("b7ad6b7169203331"),
      trace_flags: Some(0x01)
    })
  ]
  
  let order_service_logs = [
    LogRecord({
      timestamp: 1640995200050000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Forwarding request to order service"),
      attributes: [
        ("service.name", "order-service"),
        ("http.method", "GET"),
        ("http.url", "/orders")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("00f067aa0ba902b7"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200150000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Debug,
      severity_text: Some("DEBUG"),
      body: Some("Querying database for orders"),
      attributes: [
        ("service.name", "order-service"),
        ("db.query", "SELECT * FROM orders WHERE user_id = $1"),
        ("db.param.user_id", "12345")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("e457b5a2e4dcb0d6"),
      trace_flags: Some(0x01)
    }),
    LogRecord({
      timestamp: 1640995200400000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Returning order data"),
      attributes: [
        ("service.name", "order-service"),
        ("orders.count", "5"),
        ("response.size.bytes", "2048")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("00f067aa0ba902b7"),
      trace_flags: Some(0x01)
    })
  ]
  
  // 合并所有日志记录
  let all_logs = gateway_logs.concat(order_service_logs)
  
  // 验证所有日志记录都关联到同一个trace
  for log in all_logs {
    match log.trace_id {
      Some(trace_id) => assert_eq(trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
      None => assert_true(false)
    }
    match log.trace_flags {
      Some(trace_flags) => assert_eq(trace_flags, 0x01)
      None => assert_true(false)
    }
  }
  
  // 按服务分组日志
  let gateway_service_logs = all_logs.filter(fn(log) { 
    log.attributes.contains(("service.name", "api-gateway"))
  })
  let order_service_service_logs = all_logs.filter(fn(log) { 
    log.attributes.contains(("service.name", "order-service"))
  })
  
  assert_eq(gateway_service_logs.length(), 2)
  assert_eq(order_service_service_logs.length(), 3)
  
  // 按span_id分组日志
  let gateway_span_logs = all_logs.filter(fn(log) {
    match log.span_id {
      Some(span_id) => span_id == "b7ad6b7169203331"
      None => false
    }
  })
  let order_service_span_logs = all_logs.filter(fn(log) {
    match log.span_id {
      Some(span_id) => span_id == "00f067aa0ba902b7"
      None => false
    }
  })
  let database_span_logs = all_logs.filter(fn(log) {
    match log.span_id {
      Some(span_id) => span_id == "e457b5a2e4dcb0d6"
      None => false
    }
  })
  
  assert_eq(gateway_span_logs.length(), 2)
  assert_eq(order_service_span_logs.length(), 2)
  assert_eq(database_span_logs.length(), 1)
  
  // 按时间顺序验证日志
  let sorted_logs = all_logs.sort(fn(a, b) { a.timestamp <= b.timestamp })
  assert_eq(sorted_logs[0].body, Some("Incoming request: GET /api/orders"))
  assert_eq(sorted_logs[1].body, Some("Forwarding request to order service"))
  assert_eq(sorted_logs[2].body, Some("Querying database for orders"))
  assert_eq(sorted_logs[3].body, Some("Returning order data"))
  assert_eq(sorted_logs[4].body, Some("Response sent to client"))
}

// 测试4: 跨服务错误传播
test "跨服务错误传播测试" {
  // 创建包含错误的跨服务调用链
  let api_gateway_error_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "b7ad6b7169203331",
    parent_span_id: None,
    name: "HTTP POST /api/payments",
    kind: SpanKind.Server,
    start_time: 1640995200000000000L,
    end_time: Some(1640995200500000000L),
    status: Status.Error,
    attributes: [
      ("service.name", "api-gateway"),
      ("http.method", "POST"),
      ("http.url", "/api/payments"),
      ("http.status_code", "502"),
      ("error.message", "Payment service unavailable")
    ],
    events: [
      {
        name: "exception",
        timestamp: 1640995200450000000L,
        attributes: [
          ("exception.type", "ServiceUnavailableException"),
          ("exception.message", "Payment service returned 503 Service Unavailable")
        ]
      }
    ],
    links: []
  })
  
  let payment_service_error_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("b7ad6b7169203331"),
    name: "HTTP POST /payments",
    kind: SpanKind.Server,
    start_time: 1640995200050000000L,
    end_time: Some(1640995200400000000L),
    status: Status.Error,
    attributes: [
      ("service.name", "payment-service"),
      ("http.method", "POST"),
      ("http.url", "/payments"),
      ("http.status_code", "503"),
      ("error.message", "Database connection failed")
    ],
    events: [
      {
        name: "exception",
        timestamp: 1640995200350000000L,
        attributes: [
          ("exception.type", "DatabaseConnectionException"),
          ("exception.message", "Failed to connect to payment database")
        ]
      }
    ],
    links: []
  })
  
  let database_error_span = Span({
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "e457b5a2e4dcb0d6",
    parent_span_id: Some("00f067aa0ba902b7"),
    name: "DB Connection",
    kind: SpanKind.Client,
    start_time: 1640995200100000000L,
    end_time: Some(1640995200300000000L),
    status: Status.Error,
    attributes: [
      ("service.name", "payment-service"),
      ("db.system", "postgresql"),
      ("db.connection_string", "postgresql://db.example.com:5432/payments"),
      ("error.code", "CONNECTION_TIMEOUT")
    ],
    events: [
      {
        name: "exception",
        timestamp: 1640995200250000000L,
        attributes: [
          ("exception.type", "ConnectionTimeoutException"),
          ("exception.message", "Connection timeout after 15 seconds")
        ]
      }
    ],
    links: []
  })
  
  // 验证错误状态
  assert_eq(api_gateway_error_span.status, Status.Error)
  assert_eq(payment_service_error_span.status, Status.Error)
  assert_eq(database_error_span.status, Status.Error)
  
  // 验证错误事件
  assert_eq(api_gateway_error_span.events.length(), 1)
  assert_eq(payment_service_error_span.events.length(), 1)
  assert_eq(database_error_span.length(), 1)
  
  // 验证错误类型传播
  let api_gateway_exception = api_gateway_error_span.events[0]
  assert_true(api_gateway_exception.attributes.contains(("exception.type", "ServiceUnavailableException")))
  
  let payment_service_exception = payment_service_error_span.events[0]
  assert_true(payment_service_exception.attributes.contains(("exception.type", "DatabaseConnectionException")))
  
  let database_exception = database_error_span.events[0]
  assert_true(database_exception.attributes.contains(("exception.type", "ConnectionTimeoutException")))
  
  // 验证错误消息传播
  assert_true(api_gateway_error_span.attributes.contains(("error.message", "Payment service unavailable")))
  assert_true(payment_service_error_span.attributes.contains(("error.message", "Database connection failed")))
  assert_true(database_error_span.attributes.contains(("error.code", "CONNECTION_TIMEOUT")))
  
  // 创建错误相关的度量数据
  let error_metrics = [
    {
      metric_name: "http_requests_total",
      value: 1L,
      attributes: [
        ("service.name", "api-gateway"),
        ("http.method", "POST"),
        ("http.status_code", "502")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "http_requests_total",
      value: 1L,
      attributes: [
        ("service.name", "payment-service"),
        ("http.method", "POST"),
        ("http.status_code", "503")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "database_connection_errors_total",
      value: 1L,
      attributes: [
        ("service.name", "payment-service"),
        ("db.system", "postgresql"),
        ("error.code", "CONNECTION_TIMEOUT")
      ],
      timestamp: 1640995200000000000L
    }
  ]
  
  // 验证错误度量
  assert_eq(error_metrics.length(), 3)
  
  let gateway_error_metrics = error_metrics.filter(fn(m) { 
    m.attributes.contains(("service.name", "api-gateway"))
  })
  let payment_service_error_metrics = error_metrics.filter(fn(m) { 
    m.attributes.contains(("service.name", "payment-service"))
  })
  
  assert_eq(gateway_error_metrics.length(), 1)
  assert_eq(payment_service_error_metrics.length(), 2)
}

// 测试5: 跨服务性能分析
test "跨服务性能分析测试" {
  // 创建多个服务的性能数据
  let service_performance_data = [
    {
      service_name: "api-gateway",
      spans: [
        {
          name: "HTTP GET /api/products",
          start_time: 1640995200000000000L,
          end_time: 1640995200200000000L,
          attributes: [
            ("http.method", "GET"),
            ("http.status_code", "200")
          ]
        },
        {
          name: "HTTP POST /api/orders",
          start_time: 1640995200300000000L,
          end_time: 1640995200600000000L,
          attributes: [
            ("http.method", "POST"),
            ("http.status_code", "201")
          ]
        },
        {
          name: "HTTP PUT /api/users/12345",
          start_time: 1640995200700000000L,
          end_time: 1640995200900000000L,
          attributes: [
            ("http.method", "PUT"),
            ("http.status_code", "200")
          ]
        }
      ]
    },
    {
      service_name: "product-service",
      spans: [
        {
          name: "HTTP GET /products",
          start_time: 1640995200050000000L,
          end_time: 1640995200180000000L,
          attributes: [
            ("http.method", "GET"),
            ("http.status_code", "200")
          ]
        }
      ]
    },
    {
      service_name: "order-service",
      spans: [
        {
          name: "HTTP POST /orders",
          start_time: 1640995200350000000L,
          end_time: 1640995200550000000L,
          attributes: [
            ("http.method", "POST"),
            ("http.status_code", "201")
          ]
        }
      ]
    },
    {
      service_name: "user-service",
      spans: [
        {
          name: "HTTP PUT /users/12345",
          start_time: 1640995200750000000L,
          end_time: 1640995200880000000L,
          attributes: [
            ("http.method", "PUT"),
            ("http.status_code", "200")
          ]
        }
      ]
    }
  ]
  
  // 计算每个服务的性能指标
  let service_performance_metrics = [] : Array[(String, (Int64, Float64))]
  
  for service_data in service_performance_data {
    let service_name = service_data.service_name
    let spans = service_data.spans
    
    // 计算总请求数和平均响应时间
    let total_requests = Int64::from_int(spans.length())
    let total_duration = spans.reduce(fn(acc, span) { 
      acc + (span.end_time - span.start_time) 
    }, 0L)
    let avg_duration = (Int64::to_float(total_duration) / Int64::to_float(total_requests)) / 1000000.0 // 转换为毫秒
    
    service_performance_metrics = service_performance_metrics.push((service_name, (total_requests, avg_duration)))
  }
  
  // 验证性能指标
  assert_eq(service_performance_metrics.length(), 4)
  
  // 查找API网关的性能指标
  let gateway_metrics = service_performance_metrics.find(fn(m) { m[0] == "api-gateway" })
  match gateway_metrics {
    Some(metrics) => {
      assert_eq(metrics[1][0], 3L) // 3个请求
      assert_true(metrics[1][1] > 15.0 && metrics[1][1] < 25.0) // 平均响应时间在15-25ms之间
    }
    None => assert_true(false)
  }
  
  // 查找产品服务的性能指标
  let product_service_metrics = service_performance_metrics.find(fn(m) { m[0] == "product-service" })
  match product_service_metrics {
    Some(metrics) => {
      assert_eq(metrics[1][0], 1L) // 1个请求
      assert_true(metrics[1][1] > 10.0 && metrics[1][1] < 15.0) // 平均响应时间在10-15ms之间
    }
    None => assert_true(false)
  }
  
  // 按平均响应时间排序服务
  let sorted_by_duration = service_performance_metrics.sort(fn(a, b) { a[1][1] <= b[1][1] })
  
  // 验证排序结果
  assert_true(sorted_by_duration[0][1][1] <= sorted_by_duration[1][1][1])
  assert_true(sorted_by_duration[1][1][1] <= sorted_by_duration[2][1][1])
  assert_true(sorted_by_duration[2][1][1] <= sorted_by_duration[3][1][1])
  
  // 计算跨服务调用延迟
  let cross_service_delays = [
    {
      from_service: "api-gateway",
      to_service: "product-service",
      delay_ms: 5.0 // API网关到产品服务的延迟
    },
    {
      from_service: "api-gateway",
      to_service: "order-service",
      delay_ms: 5.0 // API网关到订单服务的延迟
    },
    {
      from_service: "api-gateway",
      to_service: "user-service",
      delay_ms: 5.0 // API网关到用户服务的延迟
    }
  ]
  
  // 验证跨服务延迟
  for delay in cross_service_delays {
    assert_true(delay.delay_ms > 0.0)
    assert_true(delay.delay_ms < 10.0)
  }
  
  // 计算平均跨服务延迟
  let total_delay = cross_service_delays.reduce(fn(acc, delay) { acc + delay.delay_ms }, 0.0)
  let avg_delay = total_delay / Int64::from_int(cross_service_delays.length())
  
  assert_eq(avg_delay, 5.0)
}