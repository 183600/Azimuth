// Trace模块SpanKind和StatusCode枚举详细测试用例
test "span_kind_enum_comprehensive" {
  // 测试所有SpanKind枚举值的创建和比较
  let internal_kind = Internal
  let server_kind = Server
  let client_kind = Client
  let producer_kind = Producer
  let consumer_kind = Consumer
  
  // 验证枚举值可以正确赋值和使用
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  let (_, internal_span) = tracer.start_span(ctx, "internal", internal_kind)
  let (_, server_span) = tracer.start_span(ctx, "server", server_kind)
  let (_, client_span) = tracer.start_span(ctx, "client", client_kind)
  let (_, producer_span) = tracer.start_span(ctx, "producer", producer_kind)
  let (_, consumer_span) = tracer.start_span(ctx, "consumer", consumer_kind)
  
  // 验证每种SpanKind都能正确设置
  assert_eq(internal_span.kind, Internal)
  assert_eq(server_span.kind, Server)
  assert_eq(client_span.kind, Client)
  assert_eq(producer_span.kind, Producer)
  assert_eq(consumer_span.kind, Consumer)
  
  // 测试SpanKind在复杂场景中的使用
  let complex_attributes = [
    ("service.name", AttributeValue::string("api-gateway")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("http.method", AttributeValue::string("POST")),
    ("http.target", AttributeValue::string("/api/v1/payments")),
    ("user.id", AttributeValue::string("user-12345"))
  ]
  
  // 不同类型的业务场景span
  let (_, api_server_span) = tracer.start_span(
    ctx,
    "api.request",
    Server,
    complex_attributes
  )
  
  let (_, db_client_span) = tracer.start_span(
    ctx,
    "database.query",
    Client,
    [("db.statement", AttributeValue::string("SELECT * FROM payments"))]
  )
  
  let (_, message_producer_span) = tracer.start_span(
    ctx,
    "message.publish",
    Producer,
    [("topic", AttributeValue::string("payments.processed"))]
  )
  
  let (_, message_consumer_span) = tracer.start_span(
    ctx,
    "message.consume",
    Consumer,
    [("topic", AttributeValue::string("user.notifications"))]
  )
  
  let (_, internal_processing_span) = tracer.start_span(
    ctx,
    "data.transform",
    Internal,
    [("operation", AttributeValue::string("currency_conversion"))]
  )
  
  // 验证业务场景span的kind
  assert_eq(api_server_span.kind, Server)
  assert_eq(db_client_span.kind, Client)
  assert_eq(message_producer_span.kind, Producer)
  assert_eq(message_consumer_span.kind, Consumer)
  assert_eq(internal_processing_span.kind, Internal)
}

test "status_code_enum_comprehensive" {
  // 测试所有StatusCode枚举值
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // 创建带有不同状态的span
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // Unset状态span（默认状态）
  let unset_span = Span::{
    name: "unset.span",
    context: SpanContext::{
      trace_id: Array::make(16, 0_byte),
      span_id: Array::make(8, 0_byte),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: unset_status,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // Ok状态span
  let ok_span = Span::{
    name: "ok.span",
    context: SpanContext::{
      trace_id: Array::make(16, 1_byte),
      span_id: Array::make(8, 1_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: ok_status,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("result", AttributeValue::string("success")),
      ("duration_ms", AttributeValue::int(1000L))
    ],
    events: [],
    links: []
  }
  
  // Error状态span
  let error_span = Span::{
    name: "error.span",
    context: SpanContext::{
      trace_id: Array::make(16, 2_byte),
      span_id: Array::make(8, 2_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1500L),
    status: error_status,
    status_description: Some("Database connection timeout"),
    attributes: [
      ("error.type", AttributeValue::string("timeout")),
      ("error.code", AttributeValue::string("DB_TIMEOUT")),
      ("error.retries", AttributeValue::int(3L)),
      ("error.timeout_ms", AttributeValue::int(30000L))
    ],
    events: [
      SpanEvent::{
        name: "error",
        timestamp_unix_nanos: 1200L,
        attributes: [
          ("error.message", AttributeValue::string("Connection timeout after 30 seconds")),
          ("error.severity", AttributeValue::string("high"))
        ]
      }
    ],
    links: []
  }
  
  // 验证不同状态的span
  assert_eq(unset_span.status, Unset)
  assert_eq(ok_span.status, Ok)
  assert_eq(error_span.status, Error)
  
  // 验证状态描述
  match unset_span.status_description {
    Some(_) => @test.fail("Expected None for unset status description")
    None => assert_eq(true, true)
  }
  
  match ok_span.status_description {
    Some(description) => assert_eq(description, "Operation completed successfully")
    None => @test.fail("Expected Some(status_description) for ok status")
  }
  
  match error_span.status_description {
    Some(description) => assert_eq(description, "Database connection timeout")
    None => @test.fail("Expected Some(status_description) for error status")
  }
  
  // 验证不同状态span的属性
  assert_eq(unset_span.attributes.length(), 0)
  assert_eq(ok_span.attributes.length(), 2)
  assert_eq(error_span.attributes.length(), 4)
  
  // 验证错误span的事件
  assert_eq(error_span.events.length(), 1)
  assert_eq(error_span.events[0].name, "error")
}

test "span_status_lifecycle_and_transitions" {
  // 测试span状态的生命周期和转换
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // 模拟span生命周期的不同阶段
  let initial_span = Span::{
    name: "lifecycle.test",
    context: SpanContext::{
      trace_id: Array::make(16, 0x10_byte),
      span_id: Array::make(8, 0x20_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  // 阶段1：span开始，状态为Unset
  assert_eq(initial_span.status, Unset)
  match initial_span.status_description {
    Some(_) => @test.fail("Expected None for initial status description")
    None => assert_eq(true, true)
  }
  match initial_span.end_time_unix_nanos {
    Some(_) => @test.fail("Expected None for initial end_time")
    None => assert_eq(true, true)
  }
  
  // 阶段2：span处理中，添加事件
  let processing_span = Span::{
    ..initial_span,
    events: [
      SpanEvent::{
        name: "processing.started",
        timestamp_unix_nanos: 1640995200000000100L,
        attributes: [
          ("operation", AttributeValue::string("data.validation"))
        ]
      }
    ]
  }
  
  assert_eq(processing_span.status, Unset)
  assert_eq(processing_span.events.length(), 1)
  
  // 阶段3：span成功完成
  let success_span = Span::{
    ..processing_span,
    end_time_unix_nanos: Some(1640995200000000500L),
    status: Ok,
    status_description: Some("Request processed successfully"),
    attributes: [
      ("http.status_code", AttributeValue::int(200L)),
      ("response.size", AttributeValue::int(1024L)),
      ("processing.duration_ms", AttributeValue::int(400L))
    ],
    events: processing_span.events + [
      SpanEvent::{
        name: "processing.completed",
        timestamp_unix_nanos: 1640995200000000450L,
        attributes: [
          ("result", AttributeValue::string("success")),
          ("records.processed", AttributeValue::int(50L))
        ]
      }
    ]
  }
  
  assert_eq(success_span.status, Ok)
  match success_span.status_description {
    Some(description) => assert_eq(description, "Request processed successfully")
    None => @test.fail("Expected Some(status_description)")
  }
  match success_span.end_time_unix_nanos {
    Some(end_time) => assert_eq(end_time, 1640995200000000500L)
    None => @test.fail("Expected Some(end_time)")
  }
  
  // 阶段4：span失败场景
  let error_span = Span::{
    ..processing_span,
    end_time_unix_nanos: Some(1640995200000000300L),
    status: Error,
    status_description: Some("Validation failed: invalid input data"),
    attributes: [
      ("error.type", AttributeValue::string("validation_error")),
      ("error.code", AttributeValue::string("INVALID_INPUT")),
      ("error.field", AttributeValue::string("email.address"))
    ],
    events: processing_span.events + [
      SpanEvent::{
        name: "validation.error",
        timestamp_unix_nanos: 1640995200000000200L,
        attributes: [
          ("error.message", AttributeValue::string("Invalid email format")),
          ("error.field", AttributeValue::string("email")),
          ("input.value", AttributeValue::string("invalid-email"))
        ]
      }
    ]
  }
  
  assert_eq(error_span.status, Error)
  match error_span.status_description {
    Some(description) => assert_eq(description, "Validation failed: invalid input data")
    None => @test.fail("Expected Some(status_description)")
  }
  assert_eq(error_span.attributes.length(), 3)
  assert_eq(error_span.events.length(), 2)
}

test "span_kind_and_status_combinations" {
  // 测试不同SpanKind和StatusCode的组合
  
  let ctx = Context::empty()
  let tracer = NoopTracer::{}
  
  // Server + Ok
  let server_ok = Span::{
    name: "server.success",
    context: SpanContext::{
      trace_id: Array::make(16, 0x01_byte),
      span_id: Array::make(8, 0x01_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: Ok,
    status_description: Some("HTTP request completed successfully"),
    attributes: [
      ("http.method", AttributeValue::string("GET")),
      ("http.status_code", AttributeValue::int(200L)),
      ("http.route", AttributeValue::string("/api/users"))
    ],
    events: [],
    links: []
  }
  
  // Client + Error
  let client_error = Span::{
    name: "client.failure",
    context: SpanContext::{
      trace_id: Array::make(16, 0x02_byte),
      span_id: Array::make(8, 0x02_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Client,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1500L),
    status: Error,
    status_description: Some("Database query failed"),
    attributes: [
      ("db.statement", AttributeValue::string("SELECT * FROM users")),
      ("db.error", AttributeValue::string("Connection refused")),
      ("db.retries", AttributeValue::int(3L))
    ],
    events: [],
    links: []
  }
  
  // Producer + Unset (仍在进行中)
  let producer_unset = Span::{
    name: "producer.active",
    context: SpanContext::{
      trace_id: Array::make(16, 0x03_byte),
      span_id: Array::make(8, 0x03_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Producer,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [
      ("message.topic", AttributeValue::string("user.events")),
      ("message.type", AttributeValue::string("user.created"))
    ],
    events: [],
    links: []
  }
  
  // Consumer + Ok
  let consumer_ok = Span::{
    name: "consumer.success",
    context: SpanContext::{
      trace_id: Array::make(16, 0x04_byte),
      span_id: Array::make(8, 0x04_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Consumer,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(3000L),
    status: Ok,
    status_description: Some("Message processed successfully"),
    attributes: [
      ("message.topic", AttributeValue::string("payment.events")),
      ("messages.processed", AttributeValue::int(100L)),
      ("processing.duration_ms", AttributeValue::int(2000L))
    ],
    events: [],
    links: []
  }
  
  // Internal + Error
  let internal_error = Span::{
    name: "internal.failure",
    context: SpanContext::{
      trace_id: Array::make(16, 0x05_byte),
      span_id: Array::make(8, 0x05_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1200L),
    status: Error,
    status_description: Some("Data transformation failed"),
    attributes: [
      ("operation", AttributeValue::string("json.serialization")),
      ("error.type", AttributeValue::string("serialization_error")),
      ("data.size", AttributeValue::int(1048576L))  // 1MB
    ],
    events: [],
    links: []
  }
  
  // 验证所有组合
  assert_eq(server_ok.kind, Server)
  assert_eq(server_ok.status, Ok)
  
  assert_eq(client_error.kind, Client)
  assert_eq(client_error.status, Error)
  
  assert_eq(producer_unset.kind, Producer)
  assert_eq(producer_unset.status, Unset)
  
  assert_eq(consumer_ok.kind, Consumer)
  assert_eq(consumer_ok.status, Ok)
  
  assert_eq(internal_error.kind, Internal)
  assert_eq(internal_error.status, Error)
  
  // 验证每种组合的特定属性
  match server_ok.status_description {
    Some(desc) => assert_eq(desc, "HTTP request completed successfully")
    None => @test.fail("Expected status description for server_ok")
  }
  
  match client_error.status_description {
    Some(desc) => assert_eq(desc, "Database query failed")
    None => @test.fail("Expected status description for client_error")
  }
  
  match producer_unset.status_description {
    Some(_) => @test.fail("Expected None for producer_unset status description")
    None => assert_eq(true, true)
  }
  
  match producer_unset.end_time_unix_nanos {
    Some(_) => @test.fail("Expected None for producer_unset end_time")
    None => assert_eq(true, true)
  }
}

test "span_status_with_complex_attributes_and_events" {
  // 测试带有复杂属性和事件的span状态
  
  let base_attributes = [
    ("service.name", AttributeValue::string("payment-service")),
    ("service.version", AttributeValue::string("3.2.1")),
    ("service.namespace", AttributeValue::string("production")),
    ("host.name", AttributeValue::string("payment-server-01")),
    ("host.ip", AttributeValue::string("10.0.1.100")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.executable", AttributeValue::string("payment-service")),
    ("http.method", AttributeValue::string("POST")),
    ("http.scheme", AttributeValue::string("https")),
    ("http.host", AttributeValue::string("api.example.com")),
    ("http.target", AttributeValue::string("/api/v3/payments")),
    ("http.user_agent", AttributeValue::string("PaymentClient/3.2.1")),
    ("user.id", AttributeValue::string("user-12345")),
    ("user.tier", AttributeValue::string("premium")),
    ("payment.amount", AttributeValue::float(299.99)),
    ("payment.currency", AttributeValue::string("USD")),
    ("payment.method", AttributeValue::string("credit_card")),
    ("correlation.id", AttributeValue::string("corr-abcdef-123456")),
    ("request.id", AttributeValue::string("req-789012-345678")),
    ("trace.id", AttributeValue::string("trace-111111-222222"))
  ]
  
  let base_events = [
    SpanEvent::{
      name: "validation.started",
      timestamp_unix_nanos: 1640995200000000000L,
      attributes: [
        ("validation.type", AttributeValue::string("payment.request")),
        ("validator.version", AttributeValue::string("2.1.0"))
      ]
    },
    SpanEvent::{
      name: "validation.completed",
      timestamp_unix_nanos: 1640995200000000100L,
      attributes: [
        ("validation.result", AttributeValue::string("passed")),
        ("validation.duration_ms", AttributeValue::int(100L))
      ]
    },
    SpanEvent::{
      name: "database.query.started",
      timestamp_unix_nanos: 1640995200000000200L,
      attributes: [
        ("db.operation", AttributeValue::string("SELECT")),
        ("db.table", AttributeValue::string("payments")),
        ("db.query", AttributeValue::string("SELECT * FROM payments WHERE user_id = ?"))
      ]
    }
  ]
  
  // 成功场景
  let success_events = base_events + [
    SpanEvent::{
      name: "database.query.completed",
      timestamp_unix_nanos: 1640995200000000300L,
      attributes: [
        ("db.rows_returned", AttributeValue::int(1L)),
        ("db.duration_ms", AttributeValue::int(80L)),
        ("db.index_used", AttributeValue::string("idx_user_id"))
      ]
    },
    SpanEvent::{
      name: "payment.processing.started",
      timestamp_unix_nanos: 1640995200000000400L,
      attributes: [
        ("processor.id", AttributeValue::string("processor-01")),
        ("payment.gateway", AttributeValue::string("stripe"))
      ]
    },
    SpanEvent::{
      name: "payment.processing.completed",
      timestamp_unix_nanos: 1640995200000000500L,
      attributes: [
        ("payment.status", AttributeValue::string("completed")),
        ("transaction.id", AttributeValue::string("txn_1234567890")),
        ("processing.duration_ms", AttributeValue::int(90L)),
        ("fee.charged", AttributeValue::float(8.99))
      ]
    }
  ]
  
  let success_span = Span::{
    name: "payment.process.success",
    context: SpanContext::{
      trace_id: Array::make(16, 0xa1_byte),
      span_id: Array::make(8, 0xb2_byte),
      trace_flags: 1_byte,
      trace_state: "vendor1=value1,vendor2=value2"
    },
    kind: Server,
    parent_span_id: Some(Array::make(8, 0xc3_byte)),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000000600L),
    status: Ok,
    status_description: Some("Payment processed successfully"),
    attributes: base_attributes + [
      ("http.status_code", AttributeValue::int(201L)),
      ("response.size", AttributeValue::int(512L)),
      ("total.duration_ms", AttributeValue::int(600L)),
      ("payment.id", AttributeValue::string("pay_1234567890")),
      ("auth.transaction_id", AttributeValue::string("auth_0987654321"))
    ],
    events: success_events,
    links: [
      SpanLink::{
        context: SpanContext::{
          trace_id: Array::make(16, 0xd4_byte),
          span_id: Array::make(8, 0xe5_byte),
          trace_flags: 1_byte,
          trace_state: ""
        },
        attributes: [
          ("link.type", AttributeValue::string("parent")),
          ("parent.service", AttributeValue::string("api-gateway"))
        ]
      }
    ]
  }
  
  // 失败场景
  let failure_events = base_events + [
    SpanEvent::{
      name: "database.query.failed",
      timestamp_unix_nanos: 1640995200000000250L,
      attributes: [
        ("db.error", AttributeValue::string("Connection timeout")),
        ("db.timeout_ms", AttributeValue::int(30000L)),
        ("db.retries.attempted", AttributeValue::int(3L))
      ]
    },
    SpanEvent::{
      name: "circuit.breaker.triggered",
      timestamp_unix_nanos: 1640995200000000300L,
      attributes: [
        ("circuit.name", AttributeValue::string("database.connection")),
        ("circuit.state", AttributeValue::string("open")),
        ("circuit.failure.threshold", AttributeValue::int(5L)),
        ("circuit.recovery.timeout_ms", AttributeValue::int(60000L))
      ]
    }
  ]
  
  let failure_span = Span::{
    name: "payment.process.failure",
    context: SpanContext::{
      trace_id: Array::make(16, 0xa2_byte),
      span_id: Array::make(8, 0xb3_byte),
      trace_flags: 1_byte,
      trace_state: ""
    },
    kind: Server,
    parent_span_id: Some(Array::make(8, 0xc4_byte)),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000000350L),
    status: Error,
    status_description: Some("Payment processing failed: Database unavailable"),
    attributes: base_attributes + [
      ("http.status_code", AttributeValue::int(503L)),
      ("error.type", AttributeValue::string("database_error")),
      ("error.code", AttributeValue::string("DB_UNAVAILABLE")),
      ("error.retryable", AttributeValue::bool(true)),
      ("error.retries.remaining", AttributeValue::int(2L))
    ],
    events: failure_events,
    links: []
  }
  
  // 验证成功span
  assert_eq(success_span.status, Ok)
  match success_span.status_description {
    Some(desc) => assert_eq(desc, "Payment processed successfully")
    None => @test.fail("Expected success status description")
  }
  assert_eq(success_span.attributes.length(), 26)  // base + additional
  assert_eq(success_span.events.length(), 6)
  assert_eq(success_span.links.length(), 1)
  
  // 验证失败span
  assert_eq(failure_span.status, Error)
  match failure_span.status_description {
    Some(desc) => assert_eq(desc, "Payment processing failed: Database unavailable")
    None => @test.fail("Expected failure status description")
  }
  assert_eq(failure_span.attributes.length(), 23)  // base + error attributes
  assert_eq(failure_span.events.length(), 5)
  assert_eq(failure_span.links.length(), 0)
  
  // 验证特定事件
  assert_eq(success_span.events[4].name, "payment.processing.completed")
  assert_eq(failure_span.events[3].name, "circuit.breaker.triggered")
}