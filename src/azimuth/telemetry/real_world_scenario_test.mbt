// 端到端场景测试 - 模拟真实使用场景

test "web_request_processing_scenario" {
  // 模拟Web请求处理的完整场景
  
  // 1. 初始化遥测系统
  let resource = common::Resource::default("web-service")
  let ctx = context::Context::empty()
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("web-tracer", "1.0.0")
  let meter = meter_provider.get_meter("web-meter", "1.0.0")
  let logger = logger_provider.get_logger("web-logger", "1.0.0")
  
  // 2. 创建指标
  let request_counter = meter.create_counter("http.requests.total", "count", "Total HTTP requests")
  let request_duration = meter.create_histogram("http.request.duration", "ms", "HTTP request duration")
  let active_connections = meter.create_gauge("http.active_connections", "count", "Active HTTP connections")
  let error_counter = meter.create_counter("http.errors.total", "count", "Total HTTP errors")
  
  // 3. 模拟HTTP请求处理
  let (request_ctx, request_span) = tracer.start_span(
    ctx,
    "HTTP GET /api/users",
    trace::Server,
    [
      ("http.method", AttributeValue::string("GET")),
      ("http.url", AttributeValue::string("/api/users")),
      ("http.scheme", AttributeValue::string("https")),
      ("http.host", AttributeValue::string("api.example.com")),
      ("user.id", AttributeValue::string("user123"))
    ],
    Some(1640995200000000000L)  // 2022-01-01 00:00:00 UTC
  )
  
  // 4. 记录请求开始
  request_counter.add(1L, [
    ("http.method", AttributeValue::string("GET")),
    ("http.route", AttributeValue::string("/api/users")),
    ("http.status_code", AttributeValue::int(200L))
  ])
  
  active_connections.record(1.0, [
    ("service.name", AttributeValue::string("web-service"))
  ])
  
  // 5. 记录请求开始日志
  let start_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Started processing HTTP request")
    .with_attribute("http.method", AttributeValue::string("GET"))
    .with_attribute("http.url", AttributeValue::string("/api/users"))
    .with_attribute("user.id", AttributeValue::string("user123"))
    .with_attribute("request.id", AttributeValue::string("req-abc123"))
    .build()
  
  // 6. 模拟数据库查询
  let (db_ctx, db_span) = tracer.start_span(
    request_ctx,
    "Database Query: SELECT * FROM users WHERE id = ?",
    trace::Client,
    [
      ("db.system", AttributeValue::string("postgresql")),
      ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = ?")),
      ("db.operation", AttributeValue::string("SELECT")),
      ("db.user", AttributeValue::string("app_user"))
    ]
  )
  
  // 记录数据库查询指标
  let db_query_counter = meter.create_counter("db.queries.total", "count", "Total database queries")
  let db_query_duration = meter.create_histogram("db.query.duration", "ms", "Database query duration")
  
  db_query_counter.add(1L, [
    ("db.system", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT"))
  ])
  
  db_query_duration.record(25.5, [
    ("db.system", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT"))
  ])
  
  // 7. 记录数据库查询日志
  let db_log = logs::LogRecord::builder()
    .timestamp(1640995200001000000L)
    .severity(logs::Debug)
    .body("Executed database query successfully")
    .with_attribute("db.system", AttributeValue::string("postgresql"))
    .with_attribute("db.operation", AttributeValue::string("SELECT"))
    .with_attribute("db.duration_ms", AttributeValue::float(25.5))
    .build()
  
  // 8. 模拟缓存操作
  let (cache_ctx, cache_span) = tracer.start_span(
    request_ctx,
    "Cache GET: user:user123",
    trace::Client,
    [
      ("cache.system", AttributeValue::string("redis")),
      ("cache.operation", AttributeValue::string("GET")),
      ("cache.key", AttributeValue::string("user:user123"))
    ]
  )
  
  // 记录缓存指标
  let cache_counter = meter.create_counter("cache.operations.total", "count", "Total cache operations")
  let cache_hit_ratio = meter.create_gauge("cache.hit_ratio", "percent", "Cache hit ratio")
  
  cache_counter.add(1L, [
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.result", AttributeValue::string("hit"))
  ])
  
  cache_hit_ratio.record(85.5, [
    ("cache.system", AttributeValue::string("redis"))
  ])
  
  // 9. 模拟外部API调用
  let (api_ctx, api_span) = tracer.start_span(
    request_ctx,
    "External API Call: GET https://api.payment.com/charges/charge123",
    trace::Client,
    [
      ("http.method", AttributeValue::string("GET")),
      ("http.url", AttributeValue::string("https://api.payment.com/charges/charge123")),
      ("http.scheme", AttributeValue::string("https")),
      ("http.host", AttributeValue::string("api.payment.com")),
      ("peer.service", AttributeValue::string("payment-api"))
    ]
  )
  
  // 记录外部API指标
  let external_api_counter = meter.create_counter("external.api.calls.total", "count", "Total external API calls")
  let external_api_duration = meter.create_histogram("external.api.duration", "ms", "External API call duration")
  
  external_api_counter.add(1L, [
    ("http.method", AttributeValue::string("GET")),
    ("peer.service", AttributeValue::string("payment-api")),
    ("http.status_code", AttributeValue::int(200L))
  ])
  
  external_api_duration.record(150.0, [
    ("http.method", AttributeValue::string("GET")),
    ("peer.service", AttributeValue::string("payment-api"))
  ])
  
  // 10. 记录外部API日志
  let api_log = logs::LogRecord::builder()
    .timestamp(1640995200002000000L)
    .severity(logs::Info)
    .body("External API call completed successfully")
    .with_attribute("http.method", AttributeValue::string("GET"))
    .with_attribute("http.url", AttributeValue::string("https://api.payment.com/charges/charge123"))
    .with_attribute("http.status_code", AttributeValue::int(200L))
    .with_attribute("external.api.duration_ms", AttributeValue::float(150.0))
    .build()
  
  // 11. 完成请求处理
  request_duration.record(200.0, [
    ("http.method", AttributeValue::string("GET")),
    ("http.route", AttributeValue::string("/api/users")),
    ("http.status_code", AttributeValue::int(200L))
  ])
  
  active_connections.record(0.0, [
    ("service.name", AttributeValue::string("web-service"))
  ])
  
  // 12. 记录请求完成日志
  let end_log = logs::LogRecord::builder()
    .timestamp(1640995200002000000L)
    .severity(logs::Info)
    .body("HTTP request completed successfully")
    .with_attribute("http.method", AttributeValue::string("GET"))
    .with_attribute("http.url", AttributeValue::string("/api/users"))
    .with_attribute("http.status_code", AttributeValue::int(200L))
    .with_attribute("request.duration_ms", AttributeValue::float(200.0))
    .with_attribute("user.id", AttributeValue::string("user123"))
    .build()
  
  // 13. 验证所有遥测数据都正确创建
  assert_eq(request_span.name, "HTTP GET /api/users")
  assert_eq(db_span.name, "Database Query: SELECT * FROM users WHERE id = ?")
  assert_eq(cache_span.name, "Cache GET: user:user123")
  assert_eq(api_span.name, "External API Call: GET https://api.payment.com/charges/charge123")
  
  match start_log.body {
    Some(body) => assert_eq(body, "Started processing HTTP request")
    None => @test.fail("Expected log body")
  }
  
  match end_log.body {
    Some(body) => assert_eq(body, "HTTP request completed successfully")
    None => @test.fail("Expected log body")
  }
}

test "microservice_communication_scenario" {
  // 模拟微服务间通信的场景
  
  // 1. 初始化服务A的遥测
  let service_a_resource = common::Resource::default("service-a")
  let ctx = context::Context::empty()
  
  let tracer_provider_a = trace::NoopTracerProvider::{}
  let meter_provider_a = metrics::NoopMeterProvider::{}
  let logger_provider_a = logs::NoopLoggerProvider::{}
  
  let tracer_a = tracer_provider_a.get_tracer("service-a-tracer")
  let meter_a = meter_provider_a.get_meter("service-a-meter")
  let logger_a = logger_provider_a.get_logger("service-a-logger")
  
  // 2. 服务A开始处理请求
  let (service_a_ctx, service_a_span) = tracer_a.start_span(
    ctx,
    "Process Order",
    trace::Server,
    [
      ("service.name", AttributeValue::string("service-a")),
      ("operation.type", AttributeValue::string("process_order")),
      ("order.id", AttributeValue::string("order-123")),
      ("customer.id", AttributeValue::string("customer-456"))
    ]
  )
  
  // 3. 服务A调用服务B
  let (call_b_ctx, call_b_span) = tracer_a.start_span(
    service_a_ctx,
    "Call Service B: Validate Inventory",
    trace::Client,
    [
      ("peer.service", AttributeValue::string("service-b")),
      ("operation.type", AttributeValue::string("validate_inventory")),
      ("product.id", AttributeValue::string("product-789")),
      ("quantity", AttributeValue::int(5L))
    ]
  )
  
  // 4. 设置传播头部
  let carrier = propagation::MapCarrier::new()
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  trace_propagator.inject(call_b_ctx, carrier)
  baggage_propagator.inject(call_b_ctx, carrier)
  
  // 5. 模拟服务B接收请求
  let service_b_resource = common::Resource::default("service-b")
  let tracer_provider_b = trace::NoopTracerProvider::{}
  let tracer_b = tracer_provider_b.get_tracer("service-b-tracer")
  
  // 提取传播的上下文
  let service_b_ctx = trace_propagator.extract(context::Context::empty(), carrier)
  let service_b_ctx_with_baggage = baggage_propagator.extract(service_b_ctx, carrier)
  
  let (service_b_span_ctx, service_b_span) = tracer_b.start_span(
    service_b_ctx_with_baggage,
    "Validate Inventory",
    trace::Server,
    [
      ("service.name", AttributeValue::string("service-b")),
      ("operation.type", AttributeValue::string("validate_inventory")),
      ("product.id", AttributeValue::string("product-789")),
      ("quantity", AttributeValue::int(5L))
    ]
  )
  
  // 6. 服务B调用数据库
  let (db_ctx, db_span) = tracer_b.start_span(
    service_b_span_ctx,
    "Check Inventory DB",
    trace::Client,
    [
      ("db.system", AttributeValue::string("mysql")),
      ("db.operation", AttributeValue::string("SELECT")),
      ("db.table", AttributeValue::string("inventory"))
    ]
  )
  
  // 7. 服务B调用服务C
  let (call_c_ctx, call_c_span) = tracer_b.start_span(
    service_b_span_ctx,
    "Call Service C: Check Reservations",
    trace::Client,
    [
      ("peer.service", AttributeValue::string("service-c")),
      ("operation.type", AttributeValue::string("check_reservations")),
      ("product.id", AttributeValue::string("product-789"))
    ]
  )
  
  // 8. 模拟服务C处理
  let service_c_resource = common::Resource::default("service-c")
  let tracer_provider_c = trace::NoopTracerProvider::{}
  let tracer_c = tracer_provider_c.get_tracer("service-c-tracer")
  
  let service_c_ctx = trace_propagator.extract(context::Context::empty(), carrier)
  let (service_c_span_ctx, service_c_span) = tracer_c.start_span(
    service_c_ctx,
    "Check Reservations",
    trace::Server,
    [
      ("service.name", AttributeValue::string("service-c")),
      ("operation.type", AttributeValue::string("check_reservations")),
      ("product.id", AttributeValue::string("product-789"))
    ]
  )
  
  // 9. 记录各种指标
  let meter_b = metrics::NoopMeterProvider::{}.get_meter("service-b-meter")
  let inventory_checks = meter_b.create_counter("inventory.checks.total", "count", "Total inventory checks")
  let inventory_check_duration = meter_b.create_histogram("inventory.check.duration", "ms", "Inventory check duration")
  
  inventory_checks.add(1L, [
    ("product.id", AttributeValue::string("product-789")),
    ("result", AttributeValue::string("available"))
  ])
  
  inventory_check_duration.record(45.0, [
    ("product.id", AttributeValue::string("product-789"))
  ])
  
  // 10. 记录日志
  let service_b_log = logs::LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::Info)
    .body("Inventory validation completed successfully")
    .with_attribute("service.name", AttributeValue::string("service-b"))
    .with_attribute("product.id", AttributeValue::string("product-789"))
    .with_attribute("quantity", AttributeValue::int(5L))
    .with_attribute("result", AttributeValue::string("available"))
    .build()
  
  let service_c_log = logs::LogRecord::builder()
    .timestamp(1640995200000500000L)
    .severity(logs::Info)
    .body("Reservation check completed")
    .with_attribute("service.name", AttributeValue::string("service-c"))
    .with_attribute("product.id", AttributeValue::string("product-789"))
    .with_attribute("reserved_quantity", AttributeValue::int(2L))
    .build()
  
  // 11. 验证微服务通信链路
  assert_eq(service_a_span.name, "Process Order")
  assert_eq(call_b_span.name, "Call Service B: Validate Inventory")
  assert_eq(service_b_span.name, "Validate Inventory")
  assert_eq(db_span.name, "Check Inventory DB")
  assert_eq(call_c_span.name, "Call Service C: Check Reservations")
  assert_eq(service_c_span.name, "Check Reservations")
  
  match service_b_log.body {
    Some(body) => assert_eq(body, "Inventory validation completed successfully")
    None => @test.fail("Expected service B log body")
  }
  
  match service_c_log.body {
    Some(body) => assert_eq(body, "Reservation check completed")
    None => @test.fail("Expected service C log body")
  }
}

test "background_job_processing_scenario" {
  // 模拟后台作业处理的场景
  
  // 1. 初始化后台作业服务的遥测
  let job_resource = common::Resource::default("job-processor")
  let ctx = context::Context::empty()
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("job-tracer")
  let meter = meter_provider.get_meter("job-meter")
  let logger = logger_provider.get_logger("job-logger")
  
  // 2. 创建作业处理指标
  let job_counter = meter.create_counter("jobs.processed.total", "count", "Total jobs processed")
  let job_duration = meter.create_histogram("job.duration", "ms", "Job processing duration")
  let job_queue_size = meter.create_gauge("job.queue.size", "count", "Job queue size")
  let job_failure_counter = meter.create_counter("jobs.failed.total", "count", "Total failed jobs")
  
  // 3. 模拟不同类型的作业处理
  let job_types = ["email", "report", "cleanup", "backup", "notification"]
  let mut i = 0
  while i < job_types.length() {
    let job_type = job_types[i]
    
    // 4. 开始作业处理
    let (job_ctx, job_span) = tracer.start_span(
      ctx,
      "Process Job: " + job_type,
      trace::Internal,
      [
        ("job.type", AttributeValue::string(job_type)),
        ("job.id", AttributeValue::string("job-" + i.to_string())),
        ("job.queue", AttributeValue::string("high-priority"))
      ]
    )
    
    // 5. 记录作业开始指标
    job_queue_size.record((10 - i).to_double(), [
      ("job.queue", AttributeValue::string("high-priority"))
    ])
    
    // 6. 模拟作业处理步骤
    let (step1_ctx, step1_span) = tracer.start_span(
      job_ctx,
      "Validate Job Parameters",
      trace::Internal,
      [
        ("job.step", AttributeValue::string("validate")),
        ("job.type", AttributeValue::string(job_type))
      ]
    )
    
    let (step2_ctx, step2_span) = tracer.start_span(
      job_ctx,
      "Execute Job Logic",
      trace::Internal,
      [
        ("job.step", AttributeValue::string("execute")),
        ("job.type", AttributeValue::string(job_type))
      ]
    )
    
    // 7. 根据作业类型执行不同的操作
    match job_type {
      "email" => {
        // 邮件作业特有操作
        let (email_ctx, email_span) = tracer.start_span(
          step2_ctx,
          "Send Email",
          trace::Client,
          [
            ("operation.type", AttributeValue::string("send_email")),
            ("email.provider", AttributeValue::string("smtp")),
            ("email.recipients", AttributeValue::int(5L))
          ]
        )
        
        // 记录邮件发送指标
        let email_counter = meter.create_counter("emails.sent.total", "count", "Total emails sent")
        email_counter.add(1L, [
          ("email.provider", AttributeValue::string("smtp")),
          ("email.status", AttributeValue::string("sent"))
        ])
      }
      "report" => {
        // 报告作业特有操作
        let (report_ctx, report_span) = tracer.start_span(
          step2_ctx,
          "Generate Report",
          trace::Internal,
          [
            ("operation.type", AttributeValue::string("generate_report")),
            ("report.type", AttributeValue::string("monthly")),
            ("report.format", AttributeValue::string("pdf"))
          ]
        )
        
        // 记录报告生成指标
        let report_counter = meter.create_counter("reports.generated.total", "count", "Total reports generated")
        report_counter.add(1L, [
          ("report.type", AttributeValue::string("monthly")),
          ("report.format", AttributeValue::string("pdf"))
        ])
      }
      _ => {
        // 其他作业类型的通用处理
        let (generic_ctx, generic_span) = tracer.start_span(
          step2_ctx,
          "Generic Job Processing",
          trace::Internal,
          [
            ("operation.type", AttributeValue::string("generic_process")),
            ("job.type", AttributeValue::string(job_type))
          ]
        )
      }
    }
    
    // 8. 模拟作业完成
    let job_success = i % 5 != 0  // 每5个作业有1个失败
    
    if job_success {
      job_counter.add(1L, [
        ("job.type", AttributeValue::string(job_type)),
        ("job.status", AttributeValue::string("success"))
      ])
      
      job_duration.record((100.0 + i.to_double() * 10.0), [
        ("job.type", AttributeValue::string(job_type))
      ])
      
      // 记录成功日志
      let success_log = logs::LogRecord::builder()
        .timestamp(1640995200000000000L + i.to_int64() * 1000000L)
        .severity(logs::Info)
        .body("Job completed successfully")
        .with_attribute("job.type", AttributeValue::string(job_type))
        .with_attribute("job.id", AttributeValue::string("job-" + i.to_string()))
        .with_attribute("job.duration_ms", AttributeValue::float(100.0 + i.to_double() * 10.0))
        .build()
    } else {
      job_failure_counter.add(1L, [
        ("job.type", AttributeValue::string(job_type)),
        ("failure.reason", AttributeValue::string("timeout"))
      ])
      
      // 记录失败日志
      let failure_log = logs::LogRecord::builder()
        .timestamp(1640995200000000000L + i.to_int64() * 1000000L)
        .severity(logs::Error)
        .body("Job failed due to timeout")
        .with_attribute("job.type", AttributeValue::string(job_type))
        .with_attribute("job.id", AttributeValue::string("job-" + i.to_string()))
        .with_attribute("failure.reason", AttributeValue::string("timeout"))
        .build()
    }
    
    i = i + 1
  }
  
  // 9. 验证作业处理完成
  assert_eq(i, job_types.length())
  assert_eq(job_span.name, "Process Job: cleanup")  // 最后一个作业类型
}

test "real_time_data_streaming_scenario" {
  // 模拟实时数据流处理的场景
  
  // 1. 初始化流处理服务的遥测
  let stream_resource = common::Resource::default("stream-processor")
  let ctx = context::Context::empty()
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("stream-tracer")
  let meter = meter_provider.get_meter("stream-meter")
  let logger = logger_provider.get_logger("stream-logger")
  
  // 2. 创建流处理指标
  let messages_processed = meter.create_counter("messages.processed.total", "count", "Total messages processed")
  let message_processing_duration = meter.create_histogram("message.processing.duration", "ms", "Message processing duration")
  let stream_lag = meter.create_gauge("stream.lag", "ms", "Stream processing lag")
  let error_rate = meter.create_gauge("stream.error.rate", "percent", "Stream error rate")
  
  // 3. 模拟消息流处理
  let message_types = ["user_action", "system_event", "metric_update", "log_entry"]
  let mut i = 0
  while i < message_types.length() {
    let message_type = message_types[i]
    
    // 4. 开始处理消息
    let (message_ctx, message_span) = tracer.start_span(
      ctx,
      "Process Message: " + message_type,
      trace::Internal,
      [
        ("message.type", AttributeValue::string(message_type)),
        ("message.id", AttributeValue::string("msg-" + i.to_string())),
        ("stream.topic", AttributeValue::string("events")),
        ("partition", AttributeValue::int(0L))
      ]
    )
    
    // 5. 记录消息处理开始
    stream_lag.record((50.0 + i.to_double() * 5.0), [
      ("stream.topic", AttributeValue::string("events"))
    ])
    
    // 6. 根据消息类型进行不同的处理
    match message_type {
      "user_action" => {
        // 用户行为分析
        let (analytics_ctx, analytics_span) = tracer.start_span(
          message_ctx,
          "Analyze User Action",
          trace::Internal,
          [
            ("operation.type", AttributeValue::string("analytics")),
            ("action.type", AttributeValue::string("click"))
          ]
        )
        
        // 记录用户行为指标
        let user_actions = meter.create_counter("user.actions.total", "count", "Total user actions")
        user_actions.add(1L, [
          ("action.type", AttributeValue::string("click")),
          ("user.segment", AttributeValue::string("premium"))
        ])
      }
      "system_event" => {
        // 系统事件处理
        let (event_ctx, event_span) = tracer.start_span(
          message_ctx,
          "Process System Event",
          trace::Internal,
          [
            ("operation.type", AttributeValue::string("system_event")),
            ("event.severity", AttributeValue::string("warning"))
          ]
        )
        
        // 记录系统事件指标
        let system_events = meter.create_counter("system.events.total", "count", "Total system events")
        system_events.add(1L, [
          ("event.severity", AttributeValue::string("warning")),
          ("event.source", AttributeValue::string("auth-service"))
        ])
      }
      "metric_update" => {
        // 指标更新处理
        let (metric_ctx, metric_span) = tracer.start_span(
          message_ctx,
          "Update Metrics",
          trace::Internal,
          [
            ("operation.type", AttributeValue::string("metric_update")),
            ("metric.source", AttributeValue::string("application"))
          ]
        )
        
        // 记录指标更新
        let metrics_updated = meter.create_counter("metrics.updated.total", "count", "Total metrics updated")
        metrics_updated.add(10L, [
          ("metric.source", AttributeValue::string("application"))
        ])
      }
      "log_entry" => {
        // 日志条目处理
        let (log_ctx, log_span) = tracer.start_span(
          message_ctx,
          "Process Log Entry",
          trace::Internal,
          [
            ("operation.type", AttributeValue::string("log_processing")),
            ("log.level", AttributeValue::string("INFO"))
          ]
        )
        
        // 记录日志处理指标
        let logs_processed = meter.create_counter("logs.processed.total", "count", "Total logs processed")
        logs_processed.add(1L, [
          ("log.level", AttributeValue::string("INFO")),
          ("log.source", AttributeValue::string("web-server"))
        ])
      }
    }
    
    // 7. 完成消息处理
    let processing_success = i % 4 != 0  // 每4个消息有1个处理失败
    
    if processing_success {
      messages_processed.add(1L, [
        ("message.type", AttributeValue::string(message_type)),
        ("processing.status", AttributeValue::string("success"))
      ])
      
      message_processing_duration.record((10.0 + i.to_double() * 2.0), [
        ("message.type", AttributeValue::string(message_type))
      ])
      
      // 记录成功日志
      let success_log = logs::LogRecord::builder()
        .timestamp(1640995200000000000L + i.to_int64() * 1000000L)
        .severity(logs::Debug)
        .body("Message processed successfully")
        .with_attribute("message.type", AttributeValue::string(message_type))
        .with_attribute("message.id", AttributeValue::string("msg-" + i.to_string()))
        .with_attribute("processing.duration_ms", AttributeValue::float(10.0 + i.to_double() * 2.0))
        .build()
    } else {
      // 记录失败指标
      let message_errors = meter.create_counter("messages.errors.total", "count", "Total message processing errors")
      message_errors.add(1L, [
        ("message.type", AttributeValue::string(message_type)),
        ("error.type", AttributeValue::string("parsing_error"))
      ])
      
      // 记录失败日志
      let error_log = logs::LogRecord::builder()
        .timestamp(1640995200000000000L + i.to_int64() * 1000000L)
        .severity(logs::Error)
        .body("Failed to process message")
        .with_attribute("message.type", AttributeValue::string(message_type))
        .with_attribute("message.id", AttributeValue::string("msg-" + i.to_string()))
        .with_attribute("error.type", AttributeValue::string("parsing_error"))
        .build()
    }
    
    // 8. 更新错误率
    error_rate.record(25.0, [
      ("stream.topic", AttributeValue::string("events"))
    ])
    
    i = i + 1
  }
  
  // 9. 验证流处理完成
  assert_eq(i, message_types.length())
  assert_eq(message_span.name, "Process Message: log_entry")  // 最后一个消息类型
}