// 错误恢复测试 - 测试系统从错误中恢复的能力
// 验证Azimuth Telemetry在异常情况下的韧性和恢复机制

test "network_connectivity_error_recovery" {
  // 测试网络连接错误的恢复能力
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("resilience-test", "1.0.0")
  let meter = meter_provider.get_meter("resilience-test", "1.0.0")
  let logger = logger_provider.get_logger("resilience-test", "1.0.0")
  
  // 1. 模拟网络连接失败场景
  let (ctx_network_error, network_span) = tracer.start_span(
    context::Context::empty(),
    "network_operation_with_failure",
    trace::Client,
    [
      ("operation.type", common::AttributeValue::string("http_request")),
      ("target.service", common::AttributeValue::string("external-api")),
      ("attempt.count", common::AttributeValue::int(1L))
    ],
    1640995200000000000L
  )
  
  // 记录网络错误
  logger.error("Network connection failed", [
    ("error.type", common::AttributeValue::string("connection_timeout")),
    ("error.code", common::AttributeValue::int(503L)),
    ("target.host", common::AttributeValue::string("api.example.com")),
    ("target.port", common::AttributeValue::int(443L)),
    ("timeout.ms", common::AttributeValue::int(30000L))
  ])
  
  // 记录错误指标
  let error_counter = meter.create_counter("network_errors_total", "count", "Total network errors")
  error_counter.add(1L, [
    ("error.type", common::AttributeValue::string("connection_timeout")),
    ("target.service", common::AttributeValue::string("external-api"))
  ])
  
  // 2. 创建错误状态的span
  let failed_network_span = trace::Span::{
    ..network_span,
    end_time_unix_nanos: Some(1640995200050000000L),
    status: trace::Error,
    status_description: Some("Network connection timeout after 30 seconds"),
    events: [
      trace::SpanEvent::{
        name: "connection_failed",
        timestamp_unix_nanos: 1640995200050000000L,
        attributes: [
          ("error.type", common::AttributeValue::string("connection_timeout")),
          ("error.code", common::AttributeValue::int(503L)),
          ("retry.attempt", common::AttributeValue::int(0L))
        ]
      }
    ]
  }
  
  // 验证错误状态
  assert_eq(failed_network_span.status, trace::Error)
  assert_eq(failed_network_span.status_description.unwrap(), "Network connection timeout after 30 seconds")
  assert_eq(failed_network_span.events.length(), 1)
  
  // 3. 模拟重试机制
  let retry_attempts = 3
  let mut retry_count = 0
  
  while retry_count < retry_attempts {
    retry_count = retry_count + 1
    
    let (ctx_retry, retry_span) = tracer.start_span(
      ctx_network_error,
      "network_operation_retry",
      trace::Client,
      [
        ("operation.type", common::AttributeValue::string("http_request")),
        ("target.service", common::AttributeValue::string("external-api")),
        ("attempt.count", common::AttributeValue::int((retry_count + 1).to_int64())),
        ("retry.reason", common::AttributeValue::string("connection_timeout"))
      ],
      1640995200100000000L + retry_count.to_int64()
    )
    
    // 记录重试尝试
    logger.warn("Network retry attempt", [
      ("retry.count", common::AttributeValue::int(retry_count.to_int64())),
      ("max.retries", common::AttributeValue::int(retry_attempts.to_int64())),
      ("backoff.ms", common::AttributeValue::int((retry_count * 1000).to_int64()))
    ])
    
    // 记录重试指标
    let retry_counter = meter.create_counter("network_retries_total", "count", "Total network retries")
    retry_counter.add(1L, [
      ("retry.attempt", common::AttributeValue::int(retry_count.to_int64())),
      ("target.service", common::AttributeValue::string("external-api"))
    ])
    
    // 前两次重试失败，最后一次成功
    if retry_count < retry_attempts {
      // 重试失败
      let failed_retry_span = trace::Span::{
        ..retry_span,
        end_time_unix_nanos: Some(1640995200100000000L + retry_count.to_int64() + 2000000L),
        status: trace::Error,
        status_description: Some("Retry attempt " + retry_count.to_string() + " failed"),
        events: [
          trace::SpanEvent::{
            name: "retry_failed",
            timestamp_unix_nanos: 1640995200100000000L + retry_count.to_int64() + 2000000L,
            attributes: [
              ("retry.count", common::AttributeValue::int(retry_count.to_int64())),
              ("error.type", common::AttributeValue::string("connection_timeout"))
            ]
          }
        ]
      }
      
      assert_eq(failed_retry_span.status, trace::Error)
    } else {
      // 重试成功
      let success_retry_span = trace::Span::{
        ..retry_span,
        end_time_unix_nanos: Some(1640995200100000000L + retry_count.to_int64() + 1000000L),
        status: trace::Ok,
        status_description: Some("Retry attempt " + retry_count.to_string() + " succeeded"),
        events: [
          trace::SpanEvent::{
            name: "retry_succeeded",
            timestamp_unix_nanos: 1640995200100000000L + retry_count.to_int64() + 1000000L,
            attributes: [
              ("retry.count", common::AttributeValue::int(retry_count.to_int64())),
              ("response.time.ms", common::AttributeValue::int(150L))
            ]
          }
        ]
      }
      
      assert_eq(success_retry_span.status, trace::Ok)
      
      // 记录成功恢复
      logger.info("Network operation recovered after retries", [
        ("total.retries", common::AttributeValue::int(retry_count.to_int64())),
        ("total.duration.ms", common::AttributeValue::int((retry_count * 1000 + 150).to_int64()))
      ])
      
      // 记录恢复指标
      let recovery_counter = meter.create_counter("network_recoveries_total", "count", "Total network recoveries")
      recovery_counter.add(1L, [
        ("target.service", common::AttributeValue::string("external-api")),
        ("retries.required", common::AttributeValue::int(retry_count.to_int64()))
      ])
      
      break
    }
  }
  
  // 验证重试次数
  assert_eq(retry_count, retry_attempts)
}

test "memory_pressure_error_recovery" {
  // 测试内存压力错误的恢复能力
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("memory-resilience", "1.0.0")
  let meter = meter_provider.get_meter("memory-resilience", "1.0.0")
  let logger = logger_provider.get_logger("memory-resilience", "1.0.0")
  
  // 1. 模拟内存压力检测
  let (ctx_memory_pressure, memory_span) = tracer.start_span(
    context::Context::empty(),
    "memory_intensive_operation",
    trace::Internal,
    [
      ("operation.type", common::AttributeValue::string("batch_processing")),
      ("batch.size", common::AttributeValue::int(10000L)),
      ("memory.threshold.mb", common::AttributeValue::int(512L))
    ],
    1640995200000000000L
  )
  
  // 模拟内存使用监控
  let memory_gauge = meter.create_gauge("memory_usage_mb", "megabytes", "Memory usage in MB")
  memory_gauge.record(480.0, [
    ("operation.type", common::AttributeValue::string("batch_processing"))
  ])
  
  // 2. 检测到内存压力
  logger.warn("High memory usage detected", [
    ("memory.usage.mb", common::AttributeValue::float(480.0)),
    ("memory.threshold.mb", common::AttributeValue::int(512L)),
    ("memory.percentage", common::AttributeValue::float(93.75))
  ])
  
  // 记录内存压力指标
  let pressure_counter = meter.create_counter("memory_pressure_events", "count", "Memory pressure events")
  pressure_counter.add(1L, [
    ("pressure.level", common::AttributeValue::string("high")),
    ("operation.type", common::AttributeValue::string("batch_processing"))
  ])
  
  // 3. 触发内存恢复机制
  let (ctx_recovery, recovery_span) = tracer.start_span(
    ctx_memory_pressure,
    "memory_recovery_procedure",
    trace::Internal,
    [
      ("recovery.type", common::AttributeValue::string("garbage_collection")),
      ("recovery.trigger", common::AttributeValue::string("memory_pressure"))
    ],
    1640995200050000000L
  )
  
  // 模拟内存清理操作
  logger.info("Initiating memory cleanup", [
    ("cleanup.strategy", common::AttributeValue::string("incremental_gc")),
    ("target.reduction.mb", common::AttributeValue::int(100L))
  ])
  
  // 记录清理指标
  let cleanup_counter = meter.create_counter("memory_cleanup_operations", "count", "Memory cleanup operations")
  cleanup_counter.add(1L, [
    ("cleanup.strategy", common::AttributeValue::string("incremental_gc"))
  ])
  
  // 4. 验证内存恢复效果
  let recovered_memory_usage = 380.0  // 清理后的内存使用
  memory_gauge.record(recovered_memory_usage, [
    ("operation.type", common::AttributeValue::string("batch_processing")),
    ("post.cleanup", common::AttributeValue::bool(true))
  ])
  
  logger.info("Memory cleanup completed", [
    ("memory.before.mb", common::AttributeValue::float(480.0)),
    ("memory.after.mb", common::AttributeValue::float(recovered_memory_usage)),
    ("memory.reduced.mb", common::AttributeValue::float(100.0))
  ])
  
  // 5. 创建恢复成功的span
  let successful_recovery_span = trace::Span::{
    ..recovery_span,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: trace::Ok,
    status_description: Some("Memory recovery completed successfully"),
    events: [
      trace::SpanEvent::{
        name: "memory_recovered",
        timestamp_unix_nanos: 1640995200100000000L,
        attributes: [
          ("memory.before.mb", common::AttributeValue::float(480.0)),
          ("memory.after.mb", common::AttributeValue::float(recovered_memory_usage)),
          ("recovery.duration.ms", common::AttributeValue::int(5000L))
        ]
      }
    ]
  }
  
  // 验证恢复效果
  assert_eq(successful_recovery_span.status, trace::Ok)
  assert_eq(recovered_memory_usage < 480.0, true)
  assert_eq(480.0 - recovered_memory_usage >= 100.0, true)
  
  // 6. 继续原始操作（降低批处理大小）
  let (ctx_continued, continued_span) = tracer.start_span(
    ctx_recovery,
    "memory_intensive_operation_continued",
    trace::Internal,
    [
      ("operation.type", common::AttributeValue::string("batch_processing")),
      ("batch.size", common::AttributeValue::int(5000L)),  // 减半批处理大小
      ("memory.optimized", common::AttributeValue::bool(true))
    ],
    1640995200150000000L
  )
  
  logger.info("Operation continued with optimized memory usage", [
    ("new.batch.size", common::AttributeValue::int(5000L)),
    ("memory.strategy", common::AttributeValue::string("optimized"))
  ])
  
  // 验证操作继续
  let completed_continued_span = trace::Span::{
    ..continued_span,
    end_time_unix_nanos: Some(1640995200250000000L),
    status: trace::Ok,
    status_description: Some("Operation completed successfully after memory recovery")
  }
  
  assert_eq(completed_continued_span.status, trace::Ok)
}

test "database_connection_error_recovery" {
  // 测试数据库连接错误的恢复能力
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("db-resilience", "1.0.0")
  let meter = meter_provider.get_meter("db-resilience", "1.0.0")
  let logger = logger_provider.get_logger("db-resilience", "1.0.0")
  
  // 1. 模拟数据库连接失败
  let (ctx_db_error, db_span) = tracer.start_span(
    context::Context::empty(),
    "database_query_execution",
    trace::Client,
    [
      ("db.operation", common::AttributeValue::string("SELECT")),
      ("db.table", common::AttributeValue::string("users")),
      ("db.connection.pool", common::AttributeValue::string("primary"))
    ],
    1640995200000000000L
  )
  
  // 记录数据库连接错误
  logger.error("Database connection failed", [
    ("error.type", common::AttributeValue::string("connection_lost")),
    ("error.code", common::AttributeValue::string("08006")),
    ("db.vendor", common::AttributeValue::string("postgresql")),
    ("db.host", common::AttributeValue::string("db-primary.example.com")),
    ("db.port", common::AttributeValue::int(5432L))
  ])
  
  // 记录数据库错误指标
  let db_error_counter = meter.create_counter("database_errors_total", "count", "Total database errors")
  db_error_counter.add(1L, [
    ("error.type", common::AttributeValue::string("connection_lost")),
    ("db.operation", common::AttributeValue::string("SELECT")),
    ("db.pool", common::AttributeValue::string("primary"))
  ])
  
  // 2. 创建数据库错误span
  let failed_db_span = trace::Span::{
    ..db_span,
    end_time_unix_nanos: Some(1640995200030000000L),
    status: trace::Error,
    status_description: Some("Database connection lost"),
    events: [
      trace::SpanEvent::{
        name: "connection_lost",
        timestamp_unix_nanos: 1640995200030000000L,
        attributes: [
          ("error.code", common::AttributeValue::string("08006")),
          ("connection.pool", common::AttributeValue::string("primary")),
          ("active.connections", common::AttributeValue::int(10L))
        ]
      }
    ]
  }
  
  assert_eq(failed_db_span.status, trace::Error)
  
  // 3. 触发连接池重建
  let (ctx_pool_rebuild, pool_span) = tracer.start_span(
    ctx_db_error,
    "database_connection_pool_rebuild",
    trace::Internal,
    [
      ("rebuild.reason", common::AttributeValue::string("connection_lost")),
      ("pool.type", common::AttributeValue::string("primary")),
      ("new.pool.size", common::AttributeValue::int(20L))
    ],
    1640995200050000000L
  )
  
  logger.info("Rebuilding database connection pool", [
    ("old.pool.size", common::AttributeValue::int(10L)),
    ("new.pool.size", common::AttributeValue::int(20L)),
    ("rebuild.strategy", common::AttributeValue::string("graceful_rebuild"))
  ])
  
  // 记录连接池指标
  let pool_gauge = meter.create_gauge("db_pool_size", "connections", "Database connection pool size")
  pool_gauge.record(20.0, [
    ("pool.type", common::AttributeValue::string("primary")),
    ("pool.status", common::AttributeValue::string("rebuilt"))
  ])
  
  let rebuild_counter = meter.create_counter("db_pool_rebuilds", "count", "Database pool rebuilds")
  rebuild_counter.add(1L, [
    ("pool.type", common::AttributeValue::string("primary")),
    ("rebuild.reason", common::AttributeValue::string("connection_lost"))
  ])
  
  // 4. 验证连接池重建效果
  let successful_rebuild_span = trace::Span::{
    ..pool_span,
    end_time_unix_nanos: Some(1640995200150000000L),
    status: trace::Ok,
    status_description: Some("Connection pool rebuilt successfully"),
    events: [
      trace::SpanEvent::{
        name: "pool_rebuilt",
        timestamp_unix_nanos: 1640995200150000000L,
        attributes: [
          ("new.pool.size", common::AttributeValue::int(20L)),
          ("rebuild.duration.ms", common::AttributeValue::int(10000L)),
          ("healthy.connections", common::AttributeValue::int(20L))
        ]
      }
    ]
  }
  
  assert_eq(successful_rebuild_span.status, trace::Ok)
  
  // 5. 使用新连接池重试数据库操作
  let (ctx_db_retry, retry_db_span) = tracer.start_span(
    ctx_pool_rebuild,
    "database_query_retry",
    trace::Client,
    [
      ("db.operation", common::AttributeValue::string("SELECT")),
      ("db.table", common::AttributeValue::string("users")),
      ("db.connection.pool", common::AttributeValue::string("primary")),
      ("retry.reason", common::AttributeValue::string("pool_rebuilt"))
    ],
    1640995200200000000L
  )
  
  logger.info("Database query retry with new connection pool", [
    ("query.retry", common::AttributeValue::bool(true)),
    ("pool.status", common::AttributeValue::string("healthy"))
  ])
  
  // 记录成功的数据库操作指标
  let db_success_counter = meter.create_counter("database_operations_total", "count", "Total database operations")
  db_success_counter.add(1L, [
    ("operation.type", common::AttributeValue::string("SELECT")),
    ("operation.status", common::AttributeValue::string("success")),
    ("pool.type", common::AttributeValue::string("primary"))
  ])
  
  let db_latency_histogram = meter.create_histogram("db_operation_latency_ms", "ms", "Database operation latency")
  db_latency_histogram.record(45.5, [
    ("operation.type", common::AttributeValue::string("SELECT")),
    ("table.name", common::AttributeValue::string("users"))
  ])
  
  // 6. 创建成功的重试span
  let successful_retry_span = trace::Span::{
    ..retry_db_span,
    end_time_unix_nanos: Some(1640995200200455000L),
    status: trace::Ok,
    status_description: Some("Database query completed successfully after pool rebuild"),
    events: [
      trace::SpanEvent::{
        name: "query_completed",
        timestamp_unix_nanos: 1640995200200455000L,
        attributes: [
          ("rows.returned", common::AttributeValue::int(100L)),
          ("execution.time.ms", common::AttributeValue::float(45.5))
        ]
      }
    ]
  }
  
  assert_eq(successful_retry_span.status, trace::Ok)
  
  // 7. 记录整体恢复情况
  logger.info("Database operation recovered successfully", [
    ("total.recovery.time.ms", common::AttributeValue::int(20045L)),
    ("recovery.strategy", common::AttributeValue::string("pool_rebuild")),
    ("final.status", common::AttributeValue::string("success"))
  ])
  
  let recovery_histogram = meter.create_histogram("db_error_recovery_time_ms", "ms", "Database error recovery time")
  recovery_histogram.record(20045.0, [
    ("error.type", common::AttributeValue::string("connection_lost")),
    ("recovery.strategy", common::AttributeValue::string("pool_rebuild"))
  ])
}

test "circuit_breaker_error_recovery" {
  // 测试断路器模式的错误恢复
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  let logger_provider = logs::NoopLoggerProvider::{}
  
  let tracer = tracer_provider.get_tracer("circuit-breaker", "1.0.0")
  let meter = meter_provider.get_meter("circuit-breaker", "1.0.0")
  let logger = logger_provider.get_logger("circuit-breaker", "1.0.0")
  
  // 1. 模拟连续失败导致断路器打开
  let failure_threshold = 5
  let mut failure_count = 0
  
  while failure_count < failure_threshold {
    failure_count = failure_count + 1
    
    let (ctx_failure, failure_span) = tracer.start_span(
      context::Context::empty(),
      "failing_operation",
      trace::Client,
      [
        ("operation.type", common::AttributeValue::string("external_api_call")),
        ("failure.count", common::AttributeValue::int(failure_count.to_int64())),
        ("circuit.state", common::AttributeValue::string("closed"))
      ],
      1640995200000000000L + failure_count.to_int64()
    )
    
    // 记录失败
    logger.error("External API call failed", [
      ("failure.count", common::AttributeValue::int(failure_count.to_int64())),
      ("error.code", common::AttributeValue::int(503L)),
      ("circuit.state", common::AttributeValue::string("closed"))
    ])
    
    // 记录失败指标
    let failure_counter = meter.create_counter("circuit_breaker_failures", "count", "Circuit breaker failures")
    failure_counter.add(1L, [
      ("operation.type", common::AttributeValue::string("external_api_call")),
      ("failure.count", common::AttributeValue::int(failure_count.to_int64()))
    ])
    
    // 创建失败span
    let failed_span = trace::Span::{
      ..failure_span,
      end_time_unix_nanos: Some(1640995200000000000L + failure_count.to_int64() + 1000000L),
      status: trace::Error,
      status_description: Some("External API unavailable"),
      events: [
        trace::SpanEvent::{
          name: "api_failure",
          timestamp_unix_nanos: 1640995200000000000L + failure_count.to_int64() + 1000000L,
          attributes: [
            ("error.code", common::AttributeValue::int(503L)),
            ("failure.count", common::AttributeValue::int(failure_count.to_int64()))
          ]
        }
      ]
    }
    
    assert_eq(failed_span.status, trace::Error)
  }
  
  // 2. 触发断路器打开
  let (ctx_circuit_open, circuit_span) = tracer.start_span(
    context::Context::empty(),
    "circuit_breaker_opened",
    trace::Internal,
    [
      ("circuit.state", common::AttributeValue::string("open")),
      ("failure.threshold", common::AttributeValue::int(failure_threshold.to_int64())),
      ("total.failures", common::AttributeValue::int(failure_count.to_int64()))
    ],
    1640995200000000000L + failure_count.to_int64() + 2000000L
  )
  
  logger.warn("Circuit breaker opened due to consecutive failures", [
    ("failure.threshold", common::AttributeValue::int(failure_threshold.to_int64())),
    ("total.failures", common::AttributeValue::int(failure_count.to_int64())),
    ("circuit.open.duration.ms", common::AttributeValue::int(30000L))
  ])
  
  // 记录断路器指标
  let circuit_gauge = meter.create_gauge("circuit_breaker_state", "state", "Circuit breaker state (0=closed, 1=open, 2=half_open)")
  circuit_gauge.record(1.0, [  // 1 = open
    ("service.name", common::AttributeValue::string("external-api"))
  ])
  
  let circuit_open_counter = meter.create_counter("circuit_breaker_opens", "count", "Circuit breaker openings")
  circuit_open_counter.add(1L, [
    ("service.name", common::AttributeValue::string("external-api")),
    ("failure.count", common::AttributeValue::int(failure_count.to_int64()))
  ])
  
  let open_circuit_span = trace::Span::{
    ..circuit_span,
    end_time_unix_nanos: Some(1640995200000000000L + failure_count.to_int64() + 3000000L),
    status: trace::Ok,
    status_description: Some("Circuit breaker opened to prevent cascading failures")
  }
  
  assert_eq(open_circuit_span.status, trace::Ok)
  
  // 3. 断路器打开期间的快速失败
  let (ctx_fast_fail, fast_fail_span) = tracer.start_span(
    context::Context::empty(),
    "operation_rejected_by_circuit_breaker",
    trace::Internal,
    [
      ("operation.type", common::AttributeValue::string("external_api_call")),
      ("rejection.reason", common::AttributeValue::string("circuit_breaker_open")),
      ("circuit.state", common::AttributeValue::string("open"))
    ],
    1640995200000000000L + failure_count.to_int64() + 5000000L
  )
  
  logger.info("Operation rejected by circuit breaker", [
    ("rejection.reason", common::AttributeValue::string("circuit_breaker_open")),
    ("fallback.used", common::AttributeValue::bool(true))
  ])
  
  // 记录快速失败指标
  let rejection_counter = meter.create_counter("circuit_breaker_rejections", "count", "Circuit breaker rejections")
  rejection_counter.add(1L, [
    ("operation.type", common::AttributeValue::string("external_api_call")),
    ("circuit.state", common::AttributeValue::string("open"))
  ])
  
  // 使用fallback机制
  let (ctx_fallback, fallback_span) = tracer.start_span(
    ctx_fast_fail,
    "fallback_operation",
    trace::Internal,
    [
      ("fallback.type", common::AttributeValue::string("cache_response")),
      ("fallback.reason", common::AttributeValue::string("circuit_breaker_open"))
    ],
    1640995200000000000L + failure_count.to_int64() + 6000000L
  )
  
  logger.info("Fallback operation executed successfully", [
    ("fallback.type", common::AttributeValue::string("cache_response")),
    ("response.time.ms", common::AttributeValue::int(5L))
  ])
  
  // 记录fallback指标
  let fallback_counter = meter.create_counter("fallback_operations", "count", "Fallback operations")
  fallback_counter.add(1L, [
    ("fallback.type", common::AttributeValue::string("cache_response")),
    ("fallback.success", common::AttributeValue::bool(true))
  ])
  
  let successful_fallback_span = trace::Span::{
    ..fallback_span,
    end_time_unix_nanos: Some(1640995200000000000L + failure_count.to_int64() + 6005000L),
    status: trace::Ok,
    status_description: Some("Fallback provided cached response")
  }
  
  assert_eq(successful_fallback_span.status, trace::Ok)
  
  // 4. 模拟断路器半开状态和恢复
  let recovery_time = 1640995200000000000L + failure_count.to_int64() + 35000000L  // 30秒后
  
  let (ctx_half_open, half_open_span) = tracer.start_span(
    context::Context::empty(),
    "circuit_breaker_half_open",
    trace::Internal,
    [
      ("circuit.state", common::AttributeValue::string("half_open")),
      ("test.operation", common::AttributeValue::bool(true))
    ],
    recovery_time
  )
  
  logger.info("Circuit breaker entering half-open state", [
    ("recovery.time.ms", common::AttributeValue::int(30000L)),
    ("test.operation", common::AttributeValue::bool(true))
  ])
  
  // 更新断路器状态
  circuit_gauge.record(2.0, [  // 2 = half_open
    ("service.name", common::AttributeValue::string("external-api"))
  ])
  
  // 测试操作成功
  let (ctx_recovery_test, recovery_test_span) = tracer.start_span(
    ctx_half_open,
    "recovery_test_operation",
    trace::Client,
    [
      ("operation.type", common::AttributeValue::string("external_api_call")),
      ("circuit.state", common::AttributeValue::string("half_open")),
      ("test.operation", common::AttributeValue::bool(true))
    ],
    recovery_time + 1000000L
  )
  
  logger.info("Recovery test operation successful", [
    ("response.time.ms", common::AttributeValue::int(50L)),
    ("circuit.transition", common::AttributeValue::string("half_open_to_closed"))
  ])
  
  // 记录成功恢复指标
  let recovery_counter = meter.create_counter("circuit_breaker_recoveries", "count", "Circuit breaker recoveries")
  recovery_counter.add(1L, [
    ("service.name", common::AttributeValue::string("external-api")),
    ("recovery.time.ms", common::AttributeValue::int(30000L))
  ])
  
  // 断路器恢复到关闭状态
  circuit_gauge.record(0.0, [  // 0 = closed
    ("service.name", common::AttributeValue::string("external-api"))
  ])
  
  let successful_recovery_span = trace::Span::{
    ..recovery_test_span,
    end_time_unix_nanos: Some(recovery_time + 1000050L),
    status: trace::Ok,
    status_description: Some("Service recovered, circuit breaker closed"),
    events: [
      trace::SpanEvent::{
        name: "circuit_closed",
        timestamp_unix_nanos: recovery_time + 1000050L,
        attributes: [
          ("recovery.duration.ms", common::AttributeValue::int(30001L)),
          ("final.state", common::AttributeValue::string("closed"))
        ]
      }
    ]
  }
  
  assert_eq(successful_recovery_span.status, trace::Ok)
  
  // 5. 验证后续操作正常
  let (ctx_normal_operation, normal_span) = tracer.start_span(
    context::Context::empty(),
    "normal_operation_after_recovery",
    trace::Client,
    [
      ("operation.type", common::AttributeValue::string("external_api_call")),
      ("circuit.state", common::AttributeValue::string("closed"))
    ],
    recovery_time + 2000000L
  )
  
  logger.info("Normal operation resumed after circuit breaker recovery", [
    ("response.time.ms", common::AttributeValue::int(45L))
  ])
  
  let completed_normal_span = trace::Span::{
    ..normal_span,
    end_time_unix_nanos: Some(recovery_time + 2000045L),
    status: trace::Ok,
    status_description: Some("Normal operation completed successfully")
  }
  
  assert_eq(completed_normal_span.status, trace::Ok)
}