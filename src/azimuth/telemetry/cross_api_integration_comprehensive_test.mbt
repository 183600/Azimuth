// 跨API集成测试 - 测试trace、metrics、logs三个API的协作
// 验证Azimuth Telemetry各组件之间的无缝集成和数据一致性

test "trace_metrics_logs_integration" {
  // 测试trace、metrics、logs三个API的完整集成
  
  // 1. 设置基础上下文和资源
  let ctx = context::Context::empty()
  let resource = common::Resource::default("integrated-service")
  let correlation_key = context::create_key("correlation.id")
  let ctx_with_correlation = ctx.with_value(correlation_key, "corr-integration-001")
  
  // 2. 创建Tracer和Meter
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("integration-test", "1.0.0")
  let meter = meter_provider.get_meter("integration-test", "1.0.0")
  let logger = logger_provider.get_logger("integration-test", "1.0.0")
  
  // 3. 启动一个span进行分布式追踪
  let (ctx_with_span, span) = tracer.start_span(
    ctx_with_correlation,
    "integrated_operation",
    trace::Server,
    [
      ("operation.type", common::AttributeValue::string("user_request")),
      ("service.version", common::AttributeValue::string("2.1.0"))
    ],
    1640995200000000000L  // 2022-01-01 00:00:00 UTC in nanoseconds
  )
  
  // 验证span创建
  assert_eq(span.name, "integrated_operation")
  assert_eq(span.kind, trace::Server)
  assert_eq(span.attributes.length(), 2)
  assert_eq(span.start_time_unix_nanos, 1640995200000000000L)
  
  // 4. 在span上下文中创建和更新metrics
  let request_counter = meter.create_counter("http_requests_total", "count", "Total HTTP requests")
  let response_histogram = meter.create_histogram("http_response_duration_ms", "ms", "HTTP response duration")
  let active_connections = meter.create_up_down_counter("active_connections", "count", "Active connections")
  
  // 记录metrics
  request_counter.add(1L, [
    ("method", common::AttributeValue::string("GET")),
    ("endpoint", common::AttributeValue::string("/api/users")),
    ("status_code", common::AttributeValue::int(200L))
  ])
  
  response_histogram.record(125.5, [
    ("method", common::AttributeValue::string("GET")),
    ("endpoint", common::AttributeValue::string("/api/users"))
  ])
  
  active_connections.add(1L)
  
  // 验证metrics创建成功（No-op实现）
  assert_eq(true, true)  // 如果没有异常，说明metrics操作成功
  
  // 5. 在span上下文中记录结构化日志
  let log_record = logs::LogRecord::builder()
    .timestamp(1640995200123456789L)
    .severity(logs::Info)
    .body("HTTP request processed successfully")
    .with_attribute("http.method", common::AttributeValue::string("GET"))
    .with_attribute("http.url", common::AttributeValue::string("/api/users"))
    .with_attribute("http.status_code", common::AttributeValue::int(200L))
    .with_attribute("duration_ms", common::AttributeValue::float(125.5))
    .with_attribute("user.id", common::AttributeValue::string("user-12345"))
    .with_attribute("trace.id", common::AttributeValue::array_string([
      "0af7651916cd43dd8448eb211c80319c"
    ]))
    .build()
  
  logger.emit(log_record)
  logger.info("User operation completed", [
    ("operation", common::AttributeValue::string("get_user")),
    ("user.id", common::AttributeValue::string("user-12345")),
    ("success", common::AttributeValue::bool(true))
  ])
  
  // 验证日志记录创建
  assert_eq(log_record.severity_number, logs::Info)
  assert_eq(log_record.body.unwrap(), "HTTP request processed successfully")
  assert_eq(log_record.attributes.length(), 7)
  
  // 6. 验证上下文传播和数据一致性
  let propagated_correlation = ctx_with_span.get(correlation_key)
  assert_eq(propagated_correlation.unwrap(), "corr-integration-001")
  
  // 7. 完成span
  let completed_span = trace::Span::{
    ..span,
    end_time_unix_nanos: Some(1640995200125456789L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully"),
    events: [
      trace::SpanEvent::{
        name: "request_received",
        timestamp_unix_nanos: 1640995200000000000L,
        attributes: [
          ("event.type", common::AttributeValue::string("request_start"))
        ]
      },
      trace::SpanEvent::{
        name: "response_sent",
        timestamp_unix_nanos: 1640995200123456789L,
        attributes: [
          ("event.type", common::AttributeValue::string("request_end")),
          ("response.status", common::AttributeValue::int(200L))
        ]
      }
    ]
  }
  
  // 验证span完成
  assert_eq(completed_span.end_time_unix_nanos.unwrap(), 1640995200125456789L)
  assert_eq(completed_span.status, trace::Ok)
  assert_eq(completed_span.events.length(), 2)
  
  // 8. 清理连接
  active_connections.add(-1L)
}

test "cross_api_context_propagation" {
  // 测试跨API的上下文传播
  
  // 创建初始上下文
  let ctx = context::Context::empty()
  let trace_id_key = context::create_key("trace.id")
  let user_id_key = context::create_key("user.id")
  let request_id_key = context::create_key("request.id")
  
  // 设置上下文值
  let ctx_enriched = ctx
    .with_value(trace_id_key, "trace-123456789")
    .with_value(user_id_key, "user-987654321")
    .with_value(request_id_key, "req-abcdef")
  
  // 创建tracer和logger
  let tracer_provider = trace::NoopTracerProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  let tracer = tracer_provider.get_tracer("context-test")
  let logger = logger_provider.get_logger("context-test")
  
  // 在 enriched 上下文中创建span
  let (ctx_with_span, span) = tracer.start_span(
    ctx_enriched,
    "context_propagation_test",
    trace::Internal,
    [
      ("user.id", common::AttributeValue::string("user-987654321")),
      ("request.id", common::AttributeValue::string("req-abcdef"))
    ]
  )
  
  // 验证上下文传播
  assert_eq(ctx_with_span.get(trace_id_key).unwrap(), "trace-123456789")
  assert_eq(ctx_with_span.get(user_id_key).unwrap(), "user-987654321")
  assert_eq(ctx_with_span.get(request_id_key).unwrap(), "req-abcdef")
  
  // 记录包含上下文信息的日志
  logger.info("Operation with propagated context", [
    ("trace.id", common::AttributeValue::string("trace-123456789")),
    ("user.id", common::AttributeValue::string("user-987654321")),
    ("request.id", common::AttributeValue::string("req-abcdef")),
    ("span.name", common::AttributeValue::string("context_propagation_test"))
  ])
  
  // 验证span属性与上下文一致性
  let mut found_user_id = false
  let mut found_request_id = false
  
  let mut i = 0
  while i < span.attributes.length() {
    let (key, value) = span.attributes[i]
    if key == "user.id" {
      match value {
        common::StringValue(uid) => {
          assert_eq(uid, "user-987654321")
          found_user_id = true
        }
        _ => assert_eq(false, true, "Expected string value for user.id")
      }
    }
    if key == "request.id" {
      match value {
        common::StringValue(rid) => {
          assert_eq(rid, "req-abcdef")
          found_request_id = true
        }
        _ => assert_eq(false, true, "Expected string value for request.id")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_user_id, true)
  assert_eq(found_request_id, true)
}

test "cross_api_error_handling_integration" {
  // 测试跨API的错误处理集成
  
  let ctx = context::Context::empty()
  let error_key = context::create_key("error.context")
  let ctx_with_error = ctx.with_value(error_key, "database_connection_failed")
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("error-test")
  let meter = meter_provider.get_meter("error-test")
  let logger = logger_provider.get_logger("error-test")
  
  // 创建错误span
  let (ctx_with_error_span, error_span) = tracer.start_span(
    ctx_with_error,
    "failing_operation",
    trace::Internal,
    [
      ("error.type", common::AttributeValue::string("database_error")),
      ("error.retry_count", common::AttributeValue::int(3L))
    ]
  )
  
  // 记录错误metrics
  let error_counter = meter.create_counter("errors_total", "count", "Total errors")
  let retry_histogram = meter.create_histogram("error_retry_attempts", "count", "Error retry attempts")
  
  error_counter.add(1L, [
    ("error.type", common::AttributeValue::string("database_error")),
    ("operation", common::AttributeValue::string("failing_operation"))
  ])
  
  retry_histogram.record(3.0, [
    ("error.type", common::AttributeValue::string("database_error"))
  ])
  
  // 记录错误日志
  logger.error("Database operation failed after 3 retries", [
    ("error.type", common::AttributeValue::string("database_error")),
    ("error.message", common::AttributeValue::string("Connection timeout")),
    ("retry.count", common::AttributeValue::int(3L)),
    ("operation.duration_ms", common::AttributeValue::float(5000.0))
  ])
  
  // 创建错误状态的span
  let failed_span = trace::Span::{
    ..error_span,
    end_time_unix_nanos: Some(1640995205000000000L),
    status: trace::Error,
    status_description: Some("Database connection failed after 3 retries"),
    events: [
      trace::SpanEvent::{
        name: "error",
        timestamp_unix_nanos: 1640995205000000000L,
        attributes: [
          ("error.type", common::AttributeValue::string("database_error")),
          ("error.message", common::AttributeValue::string("Connection timeout")),
          ("exception.stacktrace", common::AttributeValue::string("TimeoutException: Connection timeout after 30s"))
        ]
      }
    ]
  }
  
  // 验证错误处理集成
  assert_eq(failed_span.status, trace::Error)
  assert_eq(failed_span.status_description.unwrap(), "Database connection failed after 3 retries")
  assert_eq(failed_span.events.length(), 1)
  
  let error_event = failed_span.events[0]
  assert_eq(error_event.name, "error")
  assert_eq(error_event.attributes.length(), 3)
  
  // 验证上下文中的错误信息
  assert_eq(ctx_with_error_span.get(error_key).unwrap(), "database_connection_failed")
}

test "cross_api_performance_correlation" {
  // 测试跨API的性能数据关联
  
  let ctx = context::Context::empty()
  let operation_start = 1640995200000000000L  // 纳秒时间戳
  let operation_end = 1640995200250000000L    // 250ms后
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("performance-test")
  let meter = meter_provider.get_meter("performance-test")
  let logger = logger_provider.get_logger("performance-test")
  
  // 创建性能追踪span
  let (ctx_with_perf, perf_span) = tracer.start_span(
    ctx,
    "performance_operation",
    trace::Internal,
    [
      ("operation.type", common::AttributeValue::string("database_query")),
      ("db.statement", common::AttributeValue::string("SELECT * FROM users WHERE id = ?"))
    ],
    operation_start
  )
  
  // 记录性能metrics
  let duration_histogram = meter.create_histogram("operation_duration_ms", "ms", "Operation duration")
  let throughput_counter = meter.create_counter("operations_total", "count", "Total operations")
  
  let duration_ms = (operation_end - operation_start).to_double() / 1000000.0  // 转换为毫秒
  duration_histogram.record(duration_ms, [
    ("operation.type", common::AttributeValue::string("database_query")),
    ("db.table", common::AttributeValue::string("users"))
  ])
  
  throughput_counter.add(1L, [
    ("operation.type", common::AttributeValue::string("database_query")),
    ("status", common::AttributeValue::string("success"))
  ])
  
  // 记录性能日志
  logger.info("Performance operation completed", [
    ("operation.type", common::AttributeValue::string("database_query")),
    ("duration_ms", common::AttributeValue::float(duration_ms)),
    ("rows.processed", common::AttributeValue::int(150L)),
    ("cache.hit", common::AttributeValue::bool(true))
  ])
  
  // 完成性能span
  let completed_perf_span = trace::Span::{
    ..perf_span,
    end_time_unix_nanos: Some(operation_end),
    status: trace::Ok,
    events: [
      trace::SpanEvent::{
        name: "performance_metrics",
        timestamp_unix_nanos: operation_end,
        attributes: [
          ("duration_ms", common::AttributeValue::float(duration_ms)),
          ("throughput_ops_per_sec", common::AttributeValue::float(4.0)),  // 1op/250ms = 4ops/sec
          ("memory_usage_mb", common::AttributeValue::float(12.5))
        ]
      }
    ]
  }
  
  // 验证性能数据关联
  assert_eq(completed_perf_span.end_time_unix_nanos.unwrap(), operation_end)
  assert_eq(completed_perf_span.status, trace::Ok)
  
  let perf_event = completed_perf_span.events[0]
  assert_eq(perf_event.name, "performance_metrics")
  
  // 验证性能指标一致性
  let mut found_duration = false
  let mut found_throughput = false
  
  let mut i = 0
  while i < perf_event.attributes.length() {
    let (key, value) = perf_event.attributes[i]
    if key == "duration_ms" {
      match value {
        common::FloatValue(d) => {
          assert_eq(d, duration_ms)
          found_duration = true
        }
        _ => assert_eq(false, true, "Expected float value for duration_ms")
      }
    }
    if key == "throughput_ops_per_sec" {
      match value {
        common::FloatValue(t) => {
          assert_eq(t, 4.0)
          found_throughput = true
        }
        _ => assert_eq(false, true, "Expected float value for throughput")
      }
    }
    i = i + 1
  }
  
  assert_eq(found_duration, true)
  assert_eq(found_throughput, true)
  assert_eq(duration_ms, 250.0)
}