// Azimuth 综合集成测试
// 测试遥测系统的综合集成功能

// 测试1: 端到端遥测流程
test "端到端遥测流程测试" {
  // 创建完整的遥测流程：请求处理 -> 度量收集 -> 日志记录 -> 导出
  
  // 1. 创建配置
  let config = Configuration({
    service_name: "integration-test-service",
    service_version: "1.0.0",
    enabled: true,
    sampling_rate: 1.0,
    export_interval_ms: 5000,
    export_timeout_ms: 30000,
    max_batch_size: 512,
    max_queue_size: 2048,
    attributes: [
      ("environment", "test"),
      ("region", "us-west-2")
    ]
  })
  
  // 2. 创建资源
  let resource = Resource({
    attributes: [
      ("service.name", config.service_name),
      ("service.version", config.service_version),
      ("service.instance.id", "instance-12345"),
      ("host.name", "test-host"),
      ("os.type", "linux")
    ]
  })
  
  // 3. 创建遥测提供者
  let tracer_provider = TracerProvider.new(resource, config)
  let meter_provider = MeterProvider.new(resource, config)
  let logger_provider = LoggerProvider.new(resource, config)
  
  // 4. 创建遥测组件
  let tracer = tracer_provider.get_tracer("integration-test-tracer", Some("1.0.0"))
  let meter = meter_provider.get_meter("integration-test-meter", Some("1.0.0"))
  let logger = logger_provider.get_logger("integration-test-logger", Some("1.0.0"))
  
  // 5. 创建度量
  let request_counter = meter.create_counter("http_requests_total", Some("Total HTTP requests"), Some("requests"))
  let request_duration = meter.create_histogram("http_request_duration_seconds", Some("HTTP request duration"), Some("seconds"))
  let active_connections = meter.create_up_down_counter("active_connections", Some("Active connections"), Some("connections"))
  
  // 6. 模拟请求处理
  let trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let parent_span_id = "b7ad6b7169203331"
  
  // 创建根Span
  let root_span = tracer.start_span(
    None, // 没有父上下文
    "HTTP GET /api/products",
    Some(SpanKind.Server),
    Some([
      ("http.method", "GET"),
      ("http.url", "/api/products"),
      ("http.host", "api.example.com"),
      ("user.id", "12345")
    ])
  )
  
  // 记录开始日志
  logger.info("Request started", Some([
    ("trace_id", trace_id),
    ("span_id", root_span.span_id),
    ("http.method", "GET"),
    ("http.url", "/api/products")
  ]))
  
  // 增加活跃连接数
  active_connections.add(1, Some([("connection.type", "http")]))
  
  // 模拟数据库查询
  let db_span = tracer.start_span(
    Some(root_span.context),
    "DB Query",
    Some(SpanKind.Client),
    Some([
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM products")
    ])
  )
  
  // 记录数据库查询日志
  logger.debug("Querying database", Some([
    ("trace_id", trace_id),
    ("span_id", db_span.span_id),
    ("db.system", "postgresql")
  ]))
  
  // 模拟数据库查询时间
  let db_start_time = 1640995200100000000L
  let db_end_time = 1640995200400000000L
  let db_duration = (db_end_time - db_start_time) / 1000000000.0 // 转换为秒
  
  // 结束数据库Span
  db_span.set_status(Status.Ok)
  db_span.end(Some(db_end_time))
  
  // 记录数据库查询完成日志
  logger.info("Database query completed", Some([
    ("trace_id", trace_id),
    ("span_id", db_span.span_id),
    ("db.duration_ms", (db_duration * 1000.0).to_string())
  ]))
  
  // 模拟业务逻辑处理
  let business_span = tracer.start_span(
    Some(root_span.context),
    "Business Logic",
    Some(SpanKind.Internal),
    Some([
      ("operation.type", "product_filtering")
    ])
  )
  
  // 记录业务逻辑日志
  logger.debug("Processing business logic", Some([
    ("trace_id", trace_id),
    ("span_id", business_span.span_id),
    ("operation.type", "product_filtering")
  ]))
  
  // 模拟业务逻辑处理时间
  let business_start_time = 1640995200450000000L
  let business_end_time = 1640995200550000000L
  let business_duration = (business_end_time - business_start_time) / 1000000000.0 // 转换为秒
  
  // 结束业务逻辑Span
  business_span.set_status(Status.Ok)
  business_span.end(Some(business_end_time))
  
  // 记录业务逻辑完成日志
  logger.info("Business logic completed", Some([
    ("trace_id", trace_id),
    ("span_id", business_span.span_id),
    ("products.filtered", "25")
  ]))
  
  // 计算总请求时间
  let total_start_time = 1640995200000000000L
  let total_end_time = 1640995200600000000L
  let total_duration = (total_end_time - total_start_time) / 1000000000.0 // 转换为秒
  
  // 记录请求度量
  request_counter.add(1, Some([
    ("http.method", "GET"),
    ("http.status_code", "200"),
    ("http.route", "/api/products")
  ]))
  
  request_duration.record(total_duration, Some([
    ("http.method", "GET"),
    ("http.status_code", "200"),
    ("http.route", "/api/products")
  ]))
  
  // 结束根Span
  root_span.set_attribute("http.status_code", "200")
  root_span.set_status(Status.Ok)
  root_span.end(Some(total_end_time))
  
  // 减少活跃连接数
  active_connections.add(-1, Some([("connection.type", "http")]))
  
  // 记录请求完成日志
  logger.info("Request completed", Some([
    ("trace_id", trace_id),
    ("span_id", root_span.span_id),
    ("http.status_code", "200"),
    ("response.duration_ms", (total_duration * 1000.0).to_string())
  ]))
  
  // 验证Span
  assert_eq(root_span.trace_id, trace_id)
  assert_eq(root_span.name, "HTTP GET /api/products")
  assert_eq(root_span.kind, SpanKind.Server)
  assert_eq(root_span.status, Status.Ok)
  assert_true(root_span.attributes.contains(("http.status_code", "200")))
  
  assert_eq(db_span.trace_id, trace_id)
  assert_eq(db_span.parent_span_id, Some(root_span.span_id))
  assert_eq(db_span.name, "DB Query")
  assert_eq(db_span.kind, SpanKind.Client)
  assert_eq(db_span.status, Status.Ok)
  
  assert_eq(business_span.trace_id, trace_id)
  assert_eq(business_span.parent_span_id, Some(root_span.span_id))
  assert_eq(business_span.name, "Business Logic")
  assert_eq(business_span.kind, SpanKind.Internal)
  assert_eq(business_span.status, Status.Ok)
  
  // 验证时间关系
  assert_true(root_span.start_time <= db_span.start_time)
  assert_true(db_span.end_time <= business_span.start_time)
  assert_true(business_span.end_time <= root_span.end_time)
  
  // 验证总时间
  assert_eq(total_duration, db_duration + business_duration + 0.01) // 加上一些处理时间
  
  // 验证度量（模拟）
  assert_true(total_duration > 0.0)
  assert_eq(total_duration, db_duration + business_duration + 0.01)
}

// 测试2: 跨服务遥测集成
test "跨服务遥测集成测试" {
  // 模拟跨服务调用场景：API Gateway -> Service A -> Service B -> Database
  
  // 1. API Gateway 处理请求
  let gateway_trace_id = "4bf92f3577b34da6a3ce929d0e0e4736"
  let gateway_span_id = "b7ad6b7169203331"
  
  let gateway_span = Span({
    trace_id: gateway_trace_id,
    span_id: gateway_span_id,
    parent_span_id: None,
    name: "HTTP GET /api/orders",
    kind: SpanKind.Server,
    start_time: 1640995200000000000L,
    end_time: Some(1640995201000000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "api-gateway"),
      ("http.method", "GET"),
      ("http.url", "/api/orders"),
      ("http.status_code", "200")
    ],
    events: [
      {
        name: "request.received",
        timestamp: 1640995200000000000L,
        attributes: [
          ("user.id", "12345"),
          ("request.id", "req-67890")
        ]
      },
      {
        name: "request.completed",
        timestamp: 1640995200950000000L,
        attributes: [
          ("response.size.bytes", "2048")
        ]
      }
    ],
    links: []
  })
  
  // 2. API Gateway 调用 Service A
  let service_a_trace_id = gateway_trace_id
  let service_a_span_id = "00f067aa0ba902b7"
  
  let service_a_span = Span({
    trace_id: service_a_trace_id,
    span_id: service_a_span_id,
    parent_span_id: Some(gateway_span_id),
    name: "HTTP GET /orders",
    kind: SpanKind.Server,
    start_time: 1640995200100000000L,
    end_time: Some(1640995200800000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "order-service"),
      ("http.method", "GET"),
      ("http.url", "/orders"),
      ("http.status_code", "200")
    ],
    events: [
      {
        name: "service.request.received",
        timestamp: 1640995200100000000L,
        attributes: [
          ("correlation.id", "corr-12345")
        ]
      },
      {
        name: "service.request.completed",
        timestamp: 1640995200750000000L,
        attributes: [
          ("orders.count", "5")
        ]
      }
    ],
    links: []
  })
  
  // 3. Service A 调用 Service B
  let service_b_trace_id = service_a_trace_id
  let service_b_span_id = "e457b5a2e4dcb0d6"
  
  let service_b_span = Span({
    trace_id: service_b_trace_id,
    span_id: service_b_span_id,
    parent_span_id: Some(service_a_span_id),
    name: "HTTP GET /users",
    kind: SpanKind.Client,
    start_time: 1640995200200000000L,
    end_time: Some(1640995200500000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "order-service"),
      ("http.method", "GET"),
      ("http.url", "http://user-service/api/users"),
      ("http.status_code", "200")
    ],
    events: [
      {
        name: "http.request.started",
        timestamp: 1640995200200000000L,
        attributes: [
          ("http.target", "/api/users")
        ]
      },
      {
        name: "http.request.completed",
        timestamp: 1640995200450000000L,
        attributes: [
          ("http.response.size.bytes", "1024")
        ]
      }
    ],
    links: []
  })
  
  // 4. Service B 处理请求
  let service_b_server_span_id = "a3c9d5e7b8f019c2"
  
  let service_b_server_span = Span({
    trace_id: service_b_trace_id,
    span_id: service_b_server_span_id,
    parent_span_id: Some(service_b_span_id),
    name: "HTTP GET /api/users",
    kind: SpanKind.Server,
    start_time: 1640995200250000000L,
    end_time: Some(1640995200400000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "user-service"),
      ("http.method", "GET"),
      ("http.url", "/api/users"),
      ("http.status_code", "200")
    ],
    events: [
      {
        name: "request.processing.started",
        timestamp: 1640995200250000000L,
        attributes: [
          ("user.ids", "12345,67890")
        ]
      },
      {
        name: "request.processing.completed",
        timestamp: 1640995200350000000L,
        attributes: [
          ("users.returned", "2")
        ]
      }
    ],
    links: []
  })
  
  // 5. Service A 查询数据库
  let db_trace_id = service_a_trace_id
  let db_span_id = "c1d2e3f4a5b6c7d8"
  
  let db_span = Span({
    trace_id: db_trace_id,
    span_id: db_span_id,
    parent_span_id: Some(service_a_span_id),
    name: "SQL SELECT",
    kind: SpanKind.Client,
    start_time: 1640995200550000000L,
    end_time: Some(1640995200750000000L),
    status: Status.Ok,
    attributes: [
      ("service.name", "order-service"),
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM orders WHERE user_id = $1"),
      ("db.connection_string", "postgresql://db.example.com:5432/orders")
    ],
    events: [
      {
        name: "db.query.started",
        timestamp: 1640995200550000000L,
        attributes: [
          ("db.query.type", "SELECT")
        ]
      },
      {
        name: "db.query.completed",
        timestamp: 1640995200700000000L,
        attributes: [
          ("db.rows.returned", "5")
        ]
      }
    ],
    links: []
  })
  
  // 验证跨服务调用链
  let all_spans = [gateway_span, service_a_span, service_b_span, service_b_server_span, db_span]
  
  // 验证所有Span属于同一个Trace
  for span in all_spans {
    assert_eq(span.trace_id, gateway_trace_id)
  }
  
  // 验证Span层次结构
  assert_eq(gateway_span.parent_span_id, None) // 根Span
  assert_eq(service_a_span.parent_span_id, Some(gateway_span.span_id))
  assert_eq(service_b_span.parent_span_id, Some(service_a_span.span_id))
  assert_eq(service_b_server_span.parent_span_id, Some(service_b_span.span_id))
  assert_eq(db_span.parent_span_id, Some(service_a_span.span_id))
  
  // 验证服务名称
  assert_eq(gateway_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "api-gateway")
  assert_eq(service_a_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "order-service")
  assert_eq(service_b_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "order-service")
  assert_eq(service_b_server_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "user-service")
  assert_eq(db_span.attributes.find(fn(a) { a[0] == "service.name" })[1], "order-service")
  
  // 验证时间顺序
  assert_true(gateway_span.start_time <= service_a_span.start_time)
  assert_true(service_a_span.start_time <= service_b_span.start_time)
  assert_true(service_b_span.start_time <= service_b_server_span.start_time)
  assert_true(service_b_server_span.end_time <= service_b_span.end_time)
  assert_true(service_b_span.end_time <= db_span.start_time)
  assert_true(db_span.end_time <= service_a_span.end_time)
  assert_true(service_a_span.end_time <= gateway_span.end_time)
  
  // 计算各服务处理时间
  let gateway_time = match gateway_span.end_time {
    Some(end_time) => end_time - gateway_span.start_time
    None => 0L
  }
  let service_a_time = match service_a_span.end_time {
    Some(end_time) => end_time - service_a_span.start_time
    None => 0L
  }
  let service_b_time = match service_b_span.end_time {
    Some(end_time) => end_time - service_b_span.start_time
    None => 0L
  }
  let service_b_server_time = match service_b_server_span.end_time {
    Some(end_time) => end_time - service_b_server_span.start_time
    None => 0L
  }
  let db_time = match db_span.end_time {
    Some(end_time) => end_time - db_span.start_time
    None => 0L
  }
  
  // 验证处理时间
  assert_eq(gateway_time, 100000000L) // 100ms
  assert_eq(service_a_time, 70000000L)  // 70ms
  assert_eq(service_b_time, 30000000L)  // 30ms
  assert_eq(service_b_server_time, 15000000L) // 15ms
  assert_eq(db_time, 20000000L) // 20ms
  
  // 验证总时间等于各服务时间之和（考虑并发）
  assert_true(gateway_time >= service_a_time)
  assert_true(service_a_time >= service_b_time + db_time)
}

// 测试3: 遥测数据导出集成
test "遥测数据导出集成测试" {
  // 模拟遥测数据导出流程：收集 -> 批处理 -> 序列化 -> 导出
  
  // 1. 创建Span数据
  let spans = [
    Span({
      trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
      span_id: "b7ad6b7169203331",
      parent_span_id: None,
      name: "HTTP GET /api/products",
      kind: SpanKind.Server,
      start_time: 1640995200000000000L,
      end_time: Some(1640995200500000000L),
      status: Status.Ok,
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "GET"),
        ("http.url", "/api/products"),
        ("http.status_code", "200")
      ],
      events: [
        {
          name: "request.received",
          timestamp: 1640995200000000000L,
          attributes: [
            ("user.id", "12345")
          ]
        }
      ],
      links: []
    }),
    Span({
      trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
      span_id: "00f067aa0ba902b7",
      parent_span_id: Some("b7ad6b7169203331"),
      name: "DB Query",
      kind: SpanKind.Client,
      start_time: 1640995200100000000L,
      end_time: Some(1640995200400000000L),
      status: Status.Ok,
      attributes: [
        ("service.name", "api-service"),
        ("db.system", "postgresql"),
        ("db.statement", "SELECT * FROM products")
      ],
      events: [],
      links: []
    })
  ]
  
  // 2. 创建度量数据
  let metrics = [
    {
      metric_name: "http_requests_total",
      value: 1L,
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "GET"),
        ("http.status_code", "200")
      ],
      timestamp: 1640995200000000000L
    },
    {
      metric_name: "http_request_duration_seconds",
      value: 0.05,
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "GET"),
        ("http.status_code", "200")
      ],
      timestamp: 1640995200000000000L
    }
  ]
  
  // 3. 创建日志数据
  let logs = [
    LogRecord({
      timestamp: 1640995200000000000L,
      observed_timestamp: None,
      severity_number: SeverityNumber.Info,
      severity_text: Some("INFO"),
      body: Some("Request received"),
      attributes: [
        ("service.name", "api-service"),
        ("http.method", "GET"),
        ("http.url", "/api/products")
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
      body: Some("Request completed"),
      attributes: [
        ("service.name", "api-service"),
        ("http.status_code", "200")
      ],
      trace_id: Some("4bf92f3577b34da6a3ce929d0e0e4736"),
      span_id: Some("b7ad6b7169203331"),
      trace_flags: Some(0x01)
    })
  ]
  
  // 4. 创建资源
  let resource = Resource({
    attributes: [
      ("service.name", "api-service"),
      ("service.version", "1.0.0"),
      ("service.instance.id", "instance-12345")
    ]
  })
  
  // 5. 创建导出请求
  let export_request = ExportRequest({
    resource_spans: [
      {
        resource: resource,
        scope_spans: [
          {
            scope: InstrumentationScope({
              name: "api-service-tracer",
              version: Some("1.0.0"),
              attributes: []
            }),
            spans: spans
          }
        ],
        scope_metrics: [
          {
            scope: InstrumentationScope({
              name: "api-service-meter",
              version: Some("1.0.0"),
              attributes: []
            }),
            metrics: metrics
          }
        ],
        scope_logs: [
          {
            scope: InstrumentationScope({
              name: "api-service-logger",
              version: Some("1.0.0"),
              attributes: []
            }),
            log_records: logs
          }
        ]
      }
    ]
  })
  
  // 验证导出请求
  assert_eq(export_request.resource_spans.length(), 1)
  
  let resource_span = export_request.resource_spans[0]
  assert_eq(resource_span.resource.attributes.length(), 3)
  assert_true(resource_span.resource.attributes.contains(("service.name", "api-service")))
  assert_true(resource_span.resource.attributes.contains(("service.version", "1.0.0")))
  assert_true(resource_span.resource.attributes.contains(("service.instance.id", "instance-12345")))
  
  // 验证Span数据
  assert_eq(resource_span.scope_spans.length(), 1)
  let scope_span = resource_span.scope_spans[0]
  assert_eq(scope_span.scope.name, "api-service-tracer")
  match scope_span.scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  assert_eq(scope_span.spans.length(), 2)
  
  // 验证度量数据
  assert_eq(resource_span.scope_metrics.length(), 1)
  let scope_metric = resource_span.scope_metrics[0]
  assert_eq(scope_metric.scope.name, "api-service-meter")
  match scope_metric.scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  assert_eq(scope_metric.metrics.length(), 2)
  
  // 验证日志数据
  assert_eq(resource_span.scope_logs.length(), 1)
  let scope_log = resource_span.scope_logs[0]
  assert_eq(scope_log.scope.name, "api-service-logger")
  match scope_log.scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => assert_true(false)
  }
  assert_eq(scope_log.log_records.length(), 2)
  
  // 6. 序列化导出请求为OTLP格式
  let otlp_json = export_request.to_otlp_json()
  
  // 验证OTLP JSON格式
  assert_true(otlp_json.contains("\"resourceSpans\""))
  assert_true(otlp_json.contains("\"resource\""))
  assert_true(otlp_json.contains("\"attributes\""))
  assert_true(otlp_json.contains("\"service.name\""))
  assert_true(otlp_json.contains("\"api-service\""))
  assert_true(otlp_json.contains("\"scopeSpans\""))
  assert_true(otlp_json.contains("\"scope\""))
  assert_true(otlp_json.contains("\"spans\""))
  assert_true(otlp_json.contains("\"traceId\""))
  assert_true(otlp_json.contains("\"spanId\""))
  assert_true(otlp_json.contains("\"scopeMetrics\""))
  assert_true(otlp_json.contains("\"metrics\""))
  assert_true(otlp_json.contains("\"scopeLogs\""))
  assert_true(otlp_json.contains("\"logRecords\""))
  
  // 7. 模拟导出到OTLP收集器
  let exporter = OtlpHttpExporter.new({
    endpoint: "https://otel-collector.example.com:4318",
    headers: [
      ("Authorization", "Bearer token123"),
      ("Content-Type", "application/json")
    ],
    timeout_ms: 30000,
    compression: true
  })
  
  // 模拟导出结果
  let export_result = ExportResult({
    success: true,
    exported_spans: 2,
    exported_metrics: 2,
    exported_logs: 2,
    bytes_sent: 2048,
    duration_ms: 150
  })
  
  // 验证导出结果
  assert_true(export_result.success)
  assert_eq(export_result.exported_spans, 2)
  assert_eq(export_result.exported_metrics, 2)
  assert_eq(export_result.exported_logs, 2)
  assert_eq(export_result.bytes_sent, 2048)
  assert_eq(export_result.duration_ms, 150)
  
  // 8. 验证导出统计
  let export_stats = ExportStatistics.new()
  export_stats.record_export(export_result)
  
  assert_eq(export_stats.total_exports, 1)
  assert_eq(export_stats.successful_exports, 1)
  assert_eq(export_stats.failed_exports, 0)
  assert_eq(export_stats.total_spans_exported, 2)
  assert_eq(export_stats.total_metrics_exported, 2)
  assert_eq(export_stats.total_logs_exported, 2)
  assert_eq(export_stats.total_bytes_sent, 2048)
  assert_eq(export_stats.average_export_duration_ms, 150)
}

// 测试4: 遥测配置和采样集成
test "遥测配置和采样集成测试" {
  // 测试不同采样策略对遥测数据收集的影响
  
  // 1. 创建采样器配置
  let always_on_sampler = AlwaysOnSampler.new()
  let always_off_sampler = AlwaysOffSampler.new()
  let trace_id_ratio_sampler = TraceIdRatioSampler.new(0.5) // 50% 采样率
  let parent_based_sampler = ParentBasedSampler.new(always_on_sampler)
  
  // 2. 创建测试Trace ID
  let test_trace_ids = [
    "4bf92f3577b34da6a3ce929d0e0e4736", // 这个ID的50%采样应该被采样
    "e457b5a2e4dcb0d6a3c9d5e7b8f019c2", // 这个ID的50%采样应该不被采样
    "00f067aa0ba902b7c1d2e3f4a5b6c7d8", // 这个ID的50%采样应该被采样
    "b7ad6b7169203331e457b5a2e4dcb0d6"  // 这个ID的50%采样应该不被采样
  ]
  
  // 3. 测试AlwaysOn采样器
  for trace_id in test_trace_ids {
    let sampling_result = always_on_sampler.should_sample(
      None, // 没有父上下文
      trace_id,
      "test-span",
      SpanKind.Server,
      []
    )
    
    assert_eq(sampling_result.decision, SamplingDecision.RecordAndSample)
    assert_eq(sampling_result.attributes.length(), 0)
  }
  
  // 4. 测试AlwaysOff采样器
  for trace_id in test_trace_ids {
    let sampling_result = always_off_sampler.should_sample(
      None, // 没有父上下文
      trace_id,
      "test-span",
      SpanKind.Server,
      []
    )
    
    assert_eq(sampling_result.decision, SamplingDecision.Drop)
    assert_eq(sampling_result.attributes.length(), 0)
  }
  
  // 5. 测试TraceIdRatio采样器
  let ratio_sampling_results = [] : Array[(String, SamplingDecision)]
  
  for trace_id in test_trace_ids {
    let sampling_result = trace_id_ratio_sampler.should_sample(
      None, // 没有父上下文
      trace_id,
      "test-span",
      SpanKind.Server,
      []
    )
    
    ratio_sampling_results = ratio_sampling_results.push((trace_id, sampling_result.decision))
  }
  
  // 验证采样结果（基于Trace ID的前8位）
  assert_eq(ratio_sampling_results[0][1], SamplingDecision.RecordAndSample) // 0x4bf92f35 < 0x7fffffff
  assert_eq(ratio_sampling_results[1][1], SamplingDecision.Drop)           // 0xe457b5a2 > 0x7fffffff
  assert_eq(ratio_sampling_results[2][1], SamplingDecision.RecordAndSample) // 0x00f067aa < 0x7fffffff
  assert_eq(ratio_sampling_results[3][1], SamplingDecision.Drop)           // 0xb7ad6b71 > 0x7fffffff
  
  // 6. 测试ParentBased采样器
  // 6.1 没有父上下文的情况
  let no_parent_result = parent_based_sampler.should_sample(
    None, // 没有父上下文
    test_trace_ids[0],
    "test-span",
    SpanKind.Server,
    []
  )
  
  assert_eq(no_parent_result.decision, SamplingDecision.RecordAndSample) // 使用根采样器（AlwaysOn）
  
  // 6.2 有父上下文且被采样的情况
  let sampled_parent_context = SpanContext({
    trace_id: test_trace_ids[0],
    span_id: "b7ad6b7169203331",
    trace_flags: 0x01, // 采样标志
    trace_state: "",
    is_remote: true
  })
  
  let sampled_parent_result = parent_based_sampler.should_sample(
    Some(sampled_parent_context),
    test_trace_ids[0],
    "test-span",
    SpanKind.Server,
    []
  )
  
  assert_eq(sampled_parent_result.decision, SamplingDecision.RecordAndSample) // 遵循父采样决策
  
  // 6.3 有父上下文但不被采样的情况
  let not_sampled_parent_context = SpanContext({
    trace_id: test_trace_ids[0],
    span_id: "b7ad6b7169203331",
    trace_flags: 0x00, // 不采样标志
    trace_state: "",
    is_remote: true
  })
  
  let not_sampled_parent_result = parent_based_sampler.should_sample(
    Some(not_sampled_parent_context),
    test_trace_ids[0],
    "test-span",
    SpanKind.Server,
    []
  )
  
  assert_eq(not_sampled_parent_result.decision, SamplingDecision.Drop) // 遵循父采样决策
  
  // 7. 测试采样配置对遥测数据收集的影响
  let sampled_spans = [] : Array[Span]
  let not_sampled_spans = [] : Array[Span]
  
  for trace_id in test_trace_ids {
    let sampling_result = trace_id_ratio_sampler.should_sample(
      None, // 没有父上下文
      trace_id,
      "test-span",
      SpanKind.Server,
      []
    )
    
    if sampling_result.decision == SamplingDecision.RecordAndSample {
      let span = Span({
        trace_id: trace_id,
        span_id: "span_" + trace_id[0..8],
        parent_span_id: None,
        name: "sampled-span",
        kind: SpanKind.Server,
        start_time: 1640995200000000000L,
        end_time: Some(1640995200500000000L),
        status: Status.Ok,
        attributes: [],
        events: [],
        links: []
      })
      
      sampled_spans = sampled_spans.push(span)
    } else {
      let span = Span({
        trace_id: trace_id,
        span_id: "span_" + trace_id[0..8],
        parent_span_id: None,
        name: "not-sampled-span",
        kind: SpanKind.Server,
        start_time: 1640995200000000000L,
        end_time: Some(1640995200500000000L),
        status: Status.Ok,
        attributes: [],
        events: [],
        links: []
      })
      
      not_sampled_spans = not_sampled_spans.push(span)
    }
  }
  
  // 验证采样结果
  assert_eq(sampled_spans.length(), 2)
  assert_eq(not_sampled_spans.length(), 2)
  
  // 验证被采样的Span
  assert_eq(sampled_spans[0].trace_id, test_trace_ids[0])
  assert_eq(sampled_spans[1].trace_id, test_trace_ids[2])
  
  // 验证不被采样的Span
  assert_eq(not_sampled_spans[0].trace_id, test_trace_ids[1])
  assert_eq(not_sampled_spans[1].trace_id, test_trace_ids[3])
  
  // 8. 测试采样统计
  let sampling_statistics = SamplingStatistics.new()
  
  for trace_id in test_trace_ids {
    let sampling_result = trace_id_ratio_sampler.should_sample(
      None, // 没有父上下文
      trace_id,
      "test-span",
      SpanKind.Server,
      []
    )
    
    sampling_statistics.record_sampling_decision(sampling_result.decision)
  }
  
  // 验证采样统计
  assert_eq(sampling_statistics.total_decisions, 4)
  assert_eq(sampling_statistics.sampled_decisions, 2)
  assert_eq(sampling_statistics.not_sampled_decisions, 2)
  assert_eq(sampling_statistics.sampling_rate, 0.5)
}