// 跨模块集成综合测试用例
// 测试不同telemetry模块之间的协作和数据流

test "trace_to_logs_integration" {
  // 测试Trace到Logs的集成
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("integration-test", "1.0.0")
  let ctx = Context::empty()
  
  // 创建一个span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "user-authentication",
    Server,
    [
      ("service.name", AttributeValue::string("auth-service")),
      ("operation.type", AttributeValue::string("authentication")),
      ("user.id", AttributeValue::int(12345L))
    ],
    1640995200000000000L
  )
  
  // 模拟从span信息创建log记录
  let log_record = LogRecord::builder()
    .timestamp(span.start_time_unix_nanos + 1000000000L)  // 1秒后
    .severity(Info)
    .body("Authentication completed successfully")
    .with_attribute("trace.id", AttributeValue::array_string([
      byte_array_to_hex_string(span.context.trace_id)
    ]))
    .with_attribute("span.id", AttributeValue::array_string([
      byte_array_to_hex_string(span.context.span_id)
    ]))
    .with_attribute("span.name", AttributeValue::string(span.name))
    .with_attribute("service.name", AttributeValue::string("auth-service"))
    .with_attribute("operation.type", AttributeValue::string("authentication"))
    .with_attribute("user.id", AttributeValue::int(12345L))
    .with_attribute("authentication.result", AttributeValue::string("success"))
    .with_attribute("authentication.duration_ms", AttributeValue::float(1000.0))
    .build()
  
  // 验证log记录包含了span的信息
  match log_record.body {
    Some(body) => assert_eq(body, "Authentication completed successfully")
    None => @test.fail("Expected Some(body)")
  }
  
  match log_record.severity_number {
    Info => assert_eq(true, true)
    _ => @test.fail("Expected Info severity")
  }
  
  assert_eq(log_record.attributes.length(), 9)
  
  // 验证trace和span ID被正确传递
  let mut found_trace_id = false
  let mut found_span_id = false
  let mut found_span_name = false
  
  let mut i = 0
  while i < log_record.attributes.length() {
    match log_record.attributes[i].0 {
      "trace.id" => {
        match log_record.attributes[i].1 {
          AttributeValue::ArrayStringValue(trace_ids) => {
            assert_eq(trace_ids.length(), 1)
            assert_eq(trace_ids[0].length(), 32)  // 16 bytes = 32 hex chars
            found_trace_id = true
          }
          _ => @test.fail("Expected ArrayStringValue")
        }
      }
      "span.id" => {
        match log_record.attributes[i].1 {
          AttributeValue::ArrayStringValue(span_ids) => {
            assert_eq(span_ids.length(), 1)
            assert_eq(span_ids[0].length(), 16)  // 8 bytes = 16 hex chars
            found_span_id = true
          }
          _ => @test.fail("Expected ArrayStringValue")
        }
      }
      "span.name" => {
        match log_record.attributes[i].1 {
          AttributeValue::StringValue(name) => {
            assert_eq(name, "user-authentication")
            found_span_name = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_trace_id, true)
  assert_eq(found_span_id, true)
  assert_eq(found_span_name, true)
}

test "trace_to_metrics_integration" {
  // 测试Trace到Metrics的集成
  
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("integration-test", "1.0.0")
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("integration-meter", "1.0.0")
  
  let ctx = Context::empty()
  
  // 创建多个span来模拟操作
  let spans = []
  let operations = ["database.query", "cache.lookup", "api.call", "validation.check"]
  let durations = [50000000L, 10000000L, 200000000L, 5000000L]  // 纳秒
  
  let mut i = 0
  while i < operations.length() {
    let (span_ctx, span) = tracer.start_span(
      ctx,
      operations[i],
      Internal,
      [
        ("operation.type", AttributeValue::string(operations[i])),
        ("service.name", AttributeValue::string("api-gateway"))
      ],
      1640995200000000000L + durations[i]
    )
    spans.push(span)
    i = i + 1
  }
  
  // 创建metrics来记录span统计
  let request_counter = meter.create_counter("http.requests.total", "requests", "Total HTTP requests")
  let duration_histogram = meter.create_histogram("http.request.duration", "seconds", "HTTP request duration")
  let error_counter = meter.create_counter("http.requests.errors", "requests", "Total HTTP request errors")
  
  // 模拟基于span信息记录metrics
  let mut j = 0
  while j < spans.length() {
    let span = spans[j]
    let duration_ms = (durations[j] as Double) / 1000000.0  // 转换为毫秒
    
    // 记录请求计数
    request_counter.add(1L, [
      ("operation.type", AttributeValue::string(operations[j])),
      ("service.name", AttributeValue::string("api-gateway"))
    ])
    
    // 记录请求持续时间
    duration_histogram.record(duration_ms, [
      ("operation.type", AttributeValue::string(operations[j])),
      ("service.name", AttributeValue::string("api-gateway"))
    ])
    
    j = j + 1
  }
  
  // 模拟错误情况
  let (error_span_ctx, error_span) = tracer.start_span(
    ctx,
    "database.error",
    Internal,
    [
      ("operation.type", AttributeValue::string("database.error")),
      ("service.name", AttributeValue::string("api-gateway")),
      ("error.type", AttributeValue::string("connection.timeout")),
      ("error.code", AttributeValue::int(5003L))
    ],
    1640995200000005000L
  )
  
  // 记录错误计数
  error_counter.add(1L, [
    ("operation.type", AttributeValue::string("database.error")),
    ("service.name", AttributeValue::string("api-gateway")),
    ("error.type", AttributeValue::string("connection.timeout"))
  ])
  
  // 验证span和metrics的一致性
  assert_eq(spans.length(), 4)
  assert_eq(operations.length(), 4)
  assert_eq(durations.length(), 4)
  
  // 验证错误span的属性
  assert_eq(error_span.name, "database.error")
  assert_eq(error_span.attributes.length(), 4)
}

test "context_propagation_integration" {
  // 测试Context传播的集成
  
  let ctx = Context::empty()
  
  // 创建包含telemetry数据的context
  let baggage_key = create_key("baggage")
  let trace_id_key = create_key("trace_id")
  let span_id_key = create_key("span_id")
  let user_id_key = create_key("user_id")
  let request_id_key = create_key("request_id")
  
  let enriched_ctx = ctx
    .with_value(baggage_key, "user.id=12345,request.id=req-67890,service.name=api-gateway")
    .with_value(trace_id_key, "0af7651916cd43dd8448eb211c80319c")
    .with_value(span_id_key, "b7ad6b7169203331")
    .with_value(user_id_key, "12345")
    .with_value(request_id_key, "req-67890")
  
  // 创建propagator
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 使用context创建span
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("propagation-test", "1.0.0")
  
  let (span_ctx, span) = tracer.start_span(
    enriched_ctx,
    "propagated-operation",
    Server,
    [
      ("service.name", AttributeValue::string("api-gateway")),
      ("operation.type", AttributeValue::string("user.request"))
    ],
    1640995200000000000L
  )
  
  // 使用context创建log记录
  let log_record = LogRecord::builder()
    .timestamp(1640995200000001000L)
    .severity(Info)
    .body("Processing user request")
    .with_attribute("service.name", AttributeValue::string("api-gateway"))
    .with_attribute("operation.type", AttributeValue::string("user.request"))
    .build()
  
  // 使用context创建metrics
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("propagation-meter", "1.0.0")
  let counter = meter.create_counter("user.requests", "requests", "User request count")
  
  // 从context中提取信息用于metrics
  match enriched_ctx.get(user_id_key) {
    Some(user_id) => {
      counter.add(1L, [
        ("user.id", AttributeValue::string(user_id)),
        ("service.name", AttributeValue::string("api-gateway"))
      ])
    }
    None => @test.fail("Expected user_id in context")
  }
  
  // 验证context中的所有值都存在
  match enriched_ctx.get(baggage_key) {
    Some(baggage) => assert_eq(baggage.contains("user.id=12345"), true)
    None => @test.fail("Expected baggage in context")
  }
  
  match enriched_ctx.get(trace_id_key) {
    Some(trace_id) => assert_eq(trace_id, "0af7651916cd43dd8448eb211c80319c")
    None => @test.fail("Expected trace_id in context")
  }
  
  match enriched_ctx.get(span_id_key) {
    Some(span_id) => assert_eq(span_id, "b7ad6b7169203331")
    None => @test.fail("Expected span_id in context")
  }
  
  match enriched_ctx.get(user_id_key) {
    Some(user_id) => assert_eq(user_id, "12345")
    None => @test.fail("Expected user_id in context")
  }
  
  match enriched_ctx.get(request_id_key) {
    Some(request_id) => assert_eq(request_id, "req-67890")
    None => @test.fail("Expected request_id in context")
  }
}

test "end_to_end_telemetry_flow" {
  // 测试端到端的telemetry流程
  
  // 1. 初始化context
  let ctx = Context::empty()
  
  // 2. 创建根span
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("e2e-test", "1.0.0")
  
  let (root_ctx, root_span) = tracer.start_span(
    ctx,
    "http.request",
    Server,
    [
      ("http.method", AttributeValue::string("GET")),
      ("http.url", AttributeValue::string("/api/users/12345")),
      ("http.scheme", AttributeValue::string("https")),
      ("http.host", AttributeValue::string("api.example.com")),
      ("http.user_agent", AttributeValue::string("Mozilla/5.0")),
      ("net.peer.ip", AttributeValue::string("192.168.1.100")),
      ("service.name", AttributeValue::string("api-gateway"))
    ],
    1640995200000000000L
  )
  
  // 3. 添加context信息
  let trace_id_key = create_key("trace_id")
  let span_id_key = create_key("span_id")
  let baggage_key = create_key("baggage")
  
  let enriched_ctx = root_ctx
    .with_value(trace_id_key, byte_array_to_hex_string(root_span.context.trace_id))
    .with_value(span_id_key, byte_array_to_hex_string(root_span.context.span_id))
    .with_value(baggage_key, "user.id=12345,request.id=req-abc123,tenant.id=tenant-001")
  
  // 4. 创建子span - 数据库查询
  let (db_ctx, db_span) = tracer.start_span(
    enriched_ctx,
    "database.query",
    Client,
    [
      ("db.system", AttributeValue::string("postgresql")),
      ("db.name", AttributeValue::string("userdb")),
      ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = $1")),
      ("db.operation", AttributeValue::string("SELECT")),
      ("service.name", AttributeValue::string("user-service"))
    ],
    1640995200000000100L
  )
  
  // 5. 记录数据库查询日志
  let db_log = LogRecord::builder()
    .timestamp(1640995200000000200L)
    .severity(Debug)
    .body("Executing database query")
    .with_attribute("trace.id", AttributeValue::string(byte_array_to_hex_string(db_span.context.trace_id)))
    .with_attribute("span.id", AttributeValue::string(byte_array_to_hex_string(db_span.context.span_id)))
    .with_attribute("db.system", AttributeValue::string("postgresql"))
    .with_attribute("db.operation", AttributeValue::string("SELECT"))
    .with_attribute("service.name", AttributeValue::string("user-service"))
    .build()
  
  // 6. 记录数据库metrics
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("db-meter", "1.0.0")
  
  let db_query_counter = meter.create_counter("db.queries.total", "queries", "Total database queries")
  let db_duration_histogram = meter.create_histogram("db.query.duration", "seconds", "Database query duration")
  
  db_query_counter.add(1L, [
    ("db.system", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("service.name", AttributeValue::string("user-service"))
  ])
  
  db_duration_histogram.record(50.5, [
    ("db.system", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("service.name", AttributeValue::string("user-service"))
  ])
  
  // 7. 创建子span - 缓存查询
  let (cache_ctx, cache_span) = tracer.start_span(
    enriched_ctx,
    "cache.get",
    Internal,
    [
      ("cache.system", AttributeValue::string("redis")),
      ("cache.operation", AttributeValue::string("GET")),
      ("cache.key", AttributeValue::string("user:12345")),
      ("service.name", AttributeValue::string("cache-service"))
    ],
    1640995200000000300L
  )
  
  // 8. 记录缓存日志
  let cache_log = LogRecord::builder()
    .timestamp(1640995200000000400L)
    .severity(Debug)
    .body("Cache lookup")
    .with_attribute("trace.id", AttributeValue::string(byte_array_to_hex_string(cache_span.context.trace_id)))
    .with_attribute("span.id", AttributeValue::string(byte_array_to_hex_string(cache_span.context.span_id)))
    .with_attribute("cache.system", AttributeValue::string("redis"))
    .with_attribute("cache.operation", AttributeValue::string("GET"))
    .with_attribute("cache.hit", AttributeValue::bool(false))
    .with_attribute("service.name", AttributeValue::string("cache-service"))
    .build()
  
  // 9. 记录缓存metrics
  let cache_counter = meter.create_counter("cache.operations.total", "operations", "Total cache operations")
  let cache_histogram = meter.create_histogram("cache.operation.duration", "seconds", "Cache operation duration")
  
  cache_counter.add(1L, [
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("cache.hit", AttributeValue::bool(false)),
    ("service.name", AttributeValue::string("cache-service"))
  ])
  
  cache_histogram.record(2.3, [
    ("cache.system", AttributeValue::string("redis")),
    ("cache.operation", AttributeValue::string("GET")),
    ("service.name", AttributeValue::string("cache-service"))
  ])
  
  // 10. 记录最终响应日志
  let response_log = LogRecord::builder()
    .timestamp(1640995200000001000L)
    .severity(Info)
    .body("Request completed successfully")
    .with_attribute("trace.id", AttributeValue::string(byte_array_to_hex_string(root_span.context.trace_id)))
    .with_attribute("span.id", AttributeValue::string(byte_array_to_hex_string(root_span.context.span_id)))
    .with_attribute("http.status_code", AttributeValue::int(200L))
    .with_attribute("http.response_content_length", AttributeValue::int(1024L))
    .with_attribute("service.name", AttributeValue::string("api-gateway"))
    .with_attribute("user.id", AttributeValue::string("12345"))
    .with_attribute("request.id", AttributeValue::string("req-abc123"))
    .with_attribute("tenant.id", AttributeValue::string("tenant-001"))
    .build()
  
  // 11. 记录最终metrics
  let http_counter = meter.create_counter("http.requests.total", "requests", "Total HTTP requests")
  let http_histogram = meter.create_histogram("http.request.duration", "seconds", "HTTP request duration")
  
  http_counter.add(1L, [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L)),
    ("service.name", AttributeValue::string("api-gateway"))
  ])
  
  http_histogram.record(100.0, [
    ("http.method", AttributeValue::string("GET")),
    ("http.status_code", AttributeValue::int(200L)),
    ("service.name", AttributeValue::string("api-gateway"))
  ])
  
  // 12. 验证端到端流程的完整性
  assert_eq(root_span.name, "http.request")
  assert_eq(db_span.name, "database.query")
  assert_eq(cache_span.name, "cache.get")
  
  assert_eq(root_span.attributes.length(), 7)
  assert_eq(db_span.attributes.length(), 6)
  assert_eq(cache_span.attributes.length(), 5)
  
  assert_eq(db_log.attributes.length(), 6)
  assert_eq(cache_log.attributes.length(), 7)
  assert_eq(response_log.attributes.length(), 9)
  
  // 验证trace ID的一致性
  let root_trace_id = byte_array_to_hex_string(root_span.context.trace_id)
  let db_trace_id = byte_array_to_hex_string(db_span.context.trace_id)
  let cache_trace_id = byte_array_to_hex_string(cache_span.context.trace_id)
  
  assert_eq(root_trace_id, db_trace_id)
  assert_eq(root_trace_id, cache_trace_id)
  
  // 验证context中的信息
  match enriched_ctx.get(trace_id_key) {
    Some(trace_id) => assert_eq(trace_id, root_trace_id)
    None => @test.fail("Expected trace_id in context")
  }
  
  match enriched_ctx.get(baggage_key) {
    Some(baggage) => {
      assert_eq(baggage.contains("user.id=12345"), true)
      assert_eq(baggage.contains("request.id=req-abc123"), true)
      assert_eq(baggage.contains("tenant.id=tenant-001"), true)
    }
    None => @test.fail("Expected baggage in context")
  }
}

test "telemetry_resource_integration" {
  // 测试telemetry资源的集成
  
  // 创建资源
  let resource = Resource::default("api-gateway")
  
  // 使用资源信息创建span
  let provider = NoopTracerProvider::{}
  let tracer = provider.get_tracer("resource-test", "1.0.0")
  let ctx = Context::empty()
  
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "resource-aware-operation",
    Server,
    [
      ("service.name", AttributeValue::string(resource.service_name)),
      ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
      ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),
      ("operation.type", AttributeValue::string("resource.test"))
    ],
    1640995200000000000L
  )
  
  // 使用资源信息创建log记录
  let log_record = LogRecord::builder()
    .timestamp(1640995200000000100L)
    .severity(Info)
    .body("Resource-aware log entry")
    .with_attribute("service.name", AttributeValue::string(resource.service_name))
    .with_attribute("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name))
    .with_attribute("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version))
    .with_attribute("operation.type", AttributeValue::string("resource.test"))
    .build()
  
  // 使用资源信息创建metrics
  let meter_provider = NoopMeterProvider::{}
  let meter = meter_provider.get_meter("resource-meter", "1.0.0")
  let counter = meter.create_counter("resource.operations", "operations", "Resource-aware operations")
  
  counter.add(1L, [
    ("service.name", AttributeValue::string(resource.service_name)),
    ("telemetry.sdk.name", AttributeValue::string(resource.telemetry_sdk_name)),
    ("telemetry.sdk.version", AttributeValue::string(resource.telemetry_sdk_version)),
    ("operation.type", AttributeValue::string("resource.test"))
  ])
  
  // 验证资源信息的一致性
  assert_eq(resource.service_name, "api-gateway")
  assert_eq(resource.telemetry_sdk_name, "azimuth")
  assert_eq(resource.telemetry_sdk_version, "0.1.0")
  
  // 验证span包含资源信息
  let mut found_service_name = false
  let mut found_sdk_name = false
  let mut found_sdk_version = false
  
  let mut i = 0
  while i < span.attributes.length() {
    match span.attributes[i].0 {
      "service.name" => {
        match span.attributes[i].1 {
          AttributeValue::StringValue(name) => {
            assert_eq(name, resource.service_name)
            found_service_name = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      "telemetry.sdk.name" => {
        match span.attributes[i].1 {
          AttributeValue::StringValue(name) => {
            assert_eq(name, resource.telemetry_sdk_name)
            found_sdk_name = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      "telemetry.sdk.version" => {
        match span.attributes[i].1 {
          AttributeValue::StringValue(version) => {
            assert_eq(version, resource.telemetry_sdk_version)
            found_sdk_version = true
          }
          _ => @test.fail("Expected StringValue")
        }
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_service_name, true)
  assert_eq(found_sdk_name, true)
  assert_eq(found_sdk_version, true)
  
  // 验证log记录包含资源信息
  assert_eq(log_record.attributes.length(), 5)
  
  // 验证metrics包含资源信息
  // 在Noop实现中，我们只能验证调用不会失败
  assert_eq(true, true)
}

// 辅助函数：将字节数组转换为十六进制字符串
fn byte_array_to_hex_string(bytes : Array[Byte]) -> String {
  let hex_chars = "0123456789abcdef"
  let mut result = ""
  let mut i = 0
  while i < bytes.length() {
    let byte = bytes[i]
    let high = (byte >> 4) & 0x0f
    let low = byte & 0x0f
    result = result + hex_chars.sub(high.to_int(), high.to_int() + 1) + hex_chars.sub(low.to_int(), low.to_int() + 1)
    i = i + 1
  }
  result
}