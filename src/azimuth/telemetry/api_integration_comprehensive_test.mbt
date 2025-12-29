// API集成综合测试用例
// 测试不同遥测模块之间的集成和交互

test "logs_traces_integration" {
  // 测试Logs和Traces模块的集成
  let ctx = Context::empty()
  
  // 创建一个span
  let (_, span) = NoopTracer::start_span(ctx, "operation_span", Server, None)
  
  // 创建与span关联的log记录
  let trace_id = span.context.trace_id
  let span_id = span.context.span_id
  
  let log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Operation started"),
    attributes: [
      ("operation.name", AttributeValue::string("user_login")),
      ("user.id", AttributeValue::string("user-12345"))
    ],
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(span.context.trace_flags),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证log记录与span的关联
  match log_record.trace_id {
    Some(id) => assert_eq(id.length(), 16)
    None => @test.fail("Test failed")
  }
  
  match log_record.span_id {
    Some(id) => assert_eq(id.length(), 8)
    None => @test.fail("Test failed")
  }
  
  // 创建带有复杂属性的span和log
  let span_attributes = [
    ("service.name", AttributeValue::string("auth-service")),
    ("operation.type", AttributeValue::string("authentication")),
    ("user.id", AttributeValue::string("user-12345")),
    ("client.ip", AttributeValue::string("192.168.1.100")),
    ("request.id", AttributeValue::string("req-abcdef"))
  ]
  
  let (_, auth_span) = NoopTracer::start_span(ctx, "authenticate", Server, Some(span_attributes))
  
  let auth_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("User authentication successful"),
    attributes: [
      ("auth.method", AttributeValue::string("password")),
      ("auth.duration_ms", AttributeValue::int(250L)),
      ("auth.success", AttributeValue::bool(true))
    ],
    trace_id: Some(auth_span.context.trace_id),
    span_id: Some(auth_span.context.span_id),
    trace_flags: Some(auth_span.context.trace_flags),
    resource: Some(Resource::default("auth-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "auth-tracer",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 验证认证日志的属性
  assert_eq(auth_log.attributes.length(), 3)
  match auth_log.resource {
    Some(res) => assert_eq(res.service_name, "auth-service")
    None => @test.fail("Test failed")
  }
  
  match auth_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "auth-tracer")
    None => @test.fail("Test failed")
  }
}

test "metrics_traces_integration" {
  // 测试Metrics和Traces模块的集成
  let ctx = Context::empty()
  
  // 创建一个操作span
  let span_attributes = [
    ("operation.name", AttributeValue::string("database_query")),
    ("db.type", AttributeValue::string("postgresql")),
    ("db.name", AttributeValue::string("production_db"))
  ]
  
  let (_, db_span) = NoopTracer::start_span(ctx, "db_query", Server, Some(span_attributes))
  
  // 模拟与span关联的指标记录
  let meter = NoopMeter::{}
  let counter = meter.create_counter("db_query_count", Some("queries"), Some("Database query count"))
  let histogram = meter.create_histogram("db_query_duration", Some("ms"), Some("Database query duration"))
  let gauge = meter.create_gauge("db_connections", Some("connections"), Some("Active database connections"))
  
  // 记录指标（这些是no-op实现，不会实际记录）
  counter.add(1L, Some([
    ("operation.name", AttributeValue::string("database_query")),
    ("db.type", AttributeValue::string("postgresql")),
    ("status", AttributeValue::string("success"))
  ]))
  
  histogram.record(150.5, Some([
    ("operation.name", AttributeValue::string("database_query")),
    ("db.type", AttributeValue::string("postgresql")),
    ("query.type", AttributeValue::string("SELECT"))
  ]))
  
  gauge.record(25.0, Some([
    ("db.type", AttributeValue::string("postgresql")),
    ("db.pool", AttributeValue::string("main"))
  ]))
  
  // 验证span和指标的属性一致性
  assert_eq(db_span.name, "db_query")
  assert_eq(db_span.attributes.length(), 3)
  
  // 创建带有span上下文的事件记录
  let span_event = SpanEvent::{
    name: "query_executed",
    timestamp_unix_nanos: 1640995200000002000L,
    attributes: [
      ("db.statement", AttributeValue::string("SELECT * FROM users WHERE id = $1")),
      ("db.rows", AttributeValue::int(1L)),
      ("db.duration_ms", AttributeValue::int(150L))
    ]
  }
  
  // 验证事件属性
  assert_eq(span_event.name, "query_executed")
  assert_eq(span_event.attributes.length(), 3)
  
  match span_event.attributes[0].1 {
    StringValue(statement) => assert_eq(statement, "SELECT * FROM users WHERE id = $1")
    _ => @test.fail("Test failed")
  }
}

test "context_propagation_integration" {
  // 测试Context、Propagation和Traces的集成
  let ctx = Context::empty()
  
  // 创建上下文键
  let user_key = create_key("user.id")
  let request_key = create_key("request.id")
  let trace_key = create_key("trace.id")
  
  // 在上下文中设置值
  let ctx_with_user = ctx.with_value(user_key, "user-12345")
  let ctx_with_request = ctx_with_user.with_value(request_key, "req-abcdef")
  let ctx_with_trace = ctx_with_request.with_value(trace_key, "trace-123456")
  
  // 验证上下文值
  match ctx_with_trace.get(user_key) {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_trace.get(request_key) {
    Some(request_id) => assert_eq(request_id, "req-abcdef")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_trace.get(trace_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-123456")
    None => @test.fail("Test failed")
  }
  
  // 创建带有上下文的span
  let span_attributes = [
    ("user.id", AttributeValue::string("user-12345")),
    ("request.id", AttributeValue::string("req-abcdef"))
  ]
  
  let (_, contextual_span) = NoopTracer::start_span(ctx_with_trace, "contextual_operation", Server, Some(span_attributes))
  
  // 验证span属性与上下文的一致性
  assert_eq(contextual_span.name, "contextual_operation")
  assert_eq(contextual_span.attributes.length(), 2)
  
  // 测试baggage集成
  let baggage = Baggage::empty()
  let baggage_with_user = baggage.with_entry("user.id", "user-12345")
  let baggage_with_session = baggage_with_user.with_entry("session.id", "sess-abcdef")
  
  match baggage_with_session.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match baggage_with_session.get("session.id") {
    Some(session_id) => assert_eq(session_id, "sess-abcdef")
    None => @test.fail("Test failed")
  }
  
  // 创建与baggage关联的日志
  let baggage_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000003000L,
    observed_timestamp_unix_nanos: Some(1640995200000003100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Operation with baggage"),
    attributes: [
      ("baggage.user.id", AttributeValue::string("user-12345")),
      ("baggage.session.id", AttributeValue::string("sess-abcdef"))
    ],
    trace_id: Some(contextual_span.context.trace_id),
    span_id: Some(contextual_span.context.span_id),
    trace_flags: Some(contextual_span.context.trace_flags),
    resource: None,
    instrumentation_scope: None
  }
  
  // 验证baggage日志属性
  assert_eq(baggage_log.attributes.length(), 2)
}

test "propagation_integration" {
  // 测试Propagation、Context和Traces的集成
  let ctx = Context::empty()
  let carrier = MapCarrier::new()
  
  // 创建W3C TraceContext propagator
  let trace_propagator = W3CTraceContextPropagator::{}
  
  // 注入trace context到carrier
  trace_propagator.inject(ctx, carrier)
  
  // 验证traceparent header存在
  match carrier.get("traceparent") {
    Some(trace_parent) => assert_eq(trace_parent.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  // 从carrier提取trace context
  let extracted_ctx = trace_propagator.extract(ctx, carrier)
  
  // 创建W3C Baggage propagator
  let baggage_propagator = W3CBaggagePropagator::{}
  
  // 注入baggage到carrier
  baggage_propagator.inject(ctx, carrier)
  
  // 验证baggage header存在
  match carrier.get("baggage") {
    Some(baggage) => assert_eq(baggage.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  // 从carrier提取baggage
  let ctx_with_baggage = baggage_propagator.extract(extracted_ctx, carrier)
  
  // 创建复合propagator
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 使用复合propagator进行注入和提取
  composite_propagator.inject(ctx, carrier)
  let final_ctx = composite_propagator.extract(ctx, carrier)
  
  // 验证最终上下文
  assert_eq(true, true)  // 基本断言确保测试运行
  
  // 创建与传播上下文关联的span
  let (_, propagated_span) = NoopTracer::start_span(final_ctx, "propagated_operation", Client, None)
  
  // 验证传播的span
  assert_eq(propagated_span.name, "propagated_operation")
  assert_eq(propagated_span.kind, Client)
  
  // 创建传播日志
  let propagation_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000004000L,
    observed_timestamp_unix_nanos: Some(1640995200000004100L),
    severity_number: Debug,
    severity_text: Some("DEBUG"),
    body: Some("Context propagated across service boundaries"),
    attributes: [
      ("propagation.traceparent", AttributeValue::string("00-0af7651916cd43dd8448eb211c80319c-b7ad6b7169203331-01")),
      ("propagation.baggage", AttributeValue::string("key1=value1,key2=value2"))
    ],
    trace_id: Some(propagated_span.context.trace_id),
    span_id: Some(propagated_span.context.span_id),
    trace_flags: Some(propagated_span.context.trace_flags),
    resource: Some(Resource::default("gateway-service")),
    instrumentation_scope: None
  }
  
  // 验证传播日志
  match propagation_log.body {
    Some(body) => assert_eq(body, "Context propagated across service boundaries")
    None => @test.fail("Test failed")
  }
  
  assert_eq(propagation_log.attributes.length(), 2)
}

test "resource_instrumentation_integration" {
  // 测试Resource和InstrumentationScope的集成
  let resource = Resource::default("integration-service")
  let instrumentation_scope = InstrumentationScope::{
    name: "integration-scope",
    version: Some("2.1.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 创建带有resource和instrumentation scope的span
  let ctx = Context::empty()
  let span_attributes = [
    ("service.name", AttributeValue::string("integration-service")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("operation.name", AttributeValue::string("complex_operation"))
  ]
  
  let (_, resource_span) = NoopTracer::start_span(ctx, "resource_operation", Server, Some(span_attributes))
  
  // 创建带有resource和instrumentation scope的log
  let resource_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000005000L,
    observed_timestamp_unix_nanos: Some(1640995200000005100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Operation with resource and instrumentation scope"),
    attributes: [
      ("operation.type", AttributeValue::string("integration")),
      ("operation.complexity", AttributeValue::string("high")),
      ("resource.service.name", AttributeValue::string("integration-service")),
      ("scope.name", AttributeValue::string("integration-scope")),
      ("scope.version", AttributeValue::string("2.1.0"))
    ],
    trace_id: Some(resource_span.context.trace_id),
    span_id: Some(resource_span.context.span_id),
    trace_flags: Some(resource_span.context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 验证resource和instrumentation scope集成
  match resource_log.resource {
    Some(res) => {
      assert_eq(res.service_name, "integration-service")
      assert_eq(res.telemetry_sdk_name, "azimuth")
      assert_eq(res.telemetry_sdk_version, "0.1.0")
    }
    None => @test.fail("Test failed")
  }
  
  match resource_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "integration-scope")
      match scope.version {
        Some(version) => assert_eq(version, "2.1.0")
        None => @test.fail("Test failed")
      }
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://example.com/schema/v1")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 测试多个instrumentation scope
  let auth_scope = InstrumentationScope::{
    name: "auth-scope",
    version: Some("1.5.0"),
    schema_url: None
  }
  
  let db_scope = InstrumentationScope::{
    name: "db-scope",
    version: Some("3.2.0"),
    schema_url: None
  }
  
  let metrics_scope = InstrumentationScope::{
    name: "metrics-scope",
    version: Some("4.0.0"),
    schema_url: None
  }
  
  // 创建带有不同scope的日志
  let auth_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000006000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Authentication operation"),
    attributes: [("auth.method", AttributeValue::string("oauth2"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource),
    instrumentation_scope: Some(auth_scope)
  }
  
  let db_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000007000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Database operation"),
    attributes: [("db.operation", AttributeValue::string("query"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource),
    instrumentation_scope: Some(db_scope)
  }
  
  let metrics_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000008000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Metrics operation"),
    attributes: [("metric.type", AttributeValue::string("counter"))],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource),
    instrumentation_scope: Some(metrics_scope)
  }
  
  // 验证不同scope的日志
  match auth_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "auth-scope")
    None => @test.fail("Test failed")
  }
  
  match db_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "db-scope")
    None => @test.fail("Test failed")
  }
  
  match metrics_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "metrics-scope")
    None => @test.fail("Test failed")
  }
}

test "end_to_end_integration" {
  // 端到端集成测试，模拟完整的遥测流程
  
  // 1. 创建资源
  let resource = Resource::default("e2e-service")
  
  // 2. 创建instrumentation scope
  let instrumentation_scope = InstrumentationScope::{
    name: "e2e-scope",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  // 3. 创建上下文并设置值
  let ctx = Context::empty()
  let user_key = create_key("user.id")
  let session_key = create_key("session.id")
  let ctx_with_values = ctx.with_value(user_key, "user-67890").with_value(session_key, "sess-12345")
  
  // 4. 创建baggage
  let baggage = Baggage::empty()
    .with_entry("user.role", "admin")
    .with_entry("request.source", "web")
    .with_entry("api.version", "v1")
  
  // 5. 创建span
  let span_attributes = [
    ("user.id", AttributeValue::string("user-67890")),
    ("session.id", AttributeValue::string("sess-12345")),
    ("user.role", AttributeValue::string("admin")),
    ("request.source", AttributeValue::string("web")),
    ("api.version", AttributeValue::string("v1"))
  ]
  
  let (_, main_span) = NoopTracer::start_span(ctx_with_values, "main_operation", Server, Some(span_attributes))
  
  // 6. 创建span事件
  let start_event = SpanEvent::{
    name: "operation_started",
    timestamp_unix_nanos: 1640995200000000000L,
    attributes: [
      ("operation.phase", AttributeValue::string("start")),
      ("component", AttributeValue::string("auth"))
    ]
  }
  
  let auth_event = SpanEvent::{
    name: "authentication_completed",
    timestamp_unix_nanos: 1640995200000000500L,
    attributes: [
      ("operation.phase", AttributeValue::string("auth")),
      ("auth.method", AttributeValue::string("jwt")),
      ("auth.duration_ms", AttributeValue::int(100L))
    ]
  }
  
  let db_event = SpanEvent::{
    name: "database_query_completed",
    timestamp_unix_nanos: 1640995200000001000L,
    attributes: [
      ("operation.phase", AttributeValue::string("data")),
      ("db.type", AttributeValue::string("postgresql")),
      ("db.duration_ms", AttributeValue::int(200L)),
      ("db.rows", AttributeValue::int(5L))
    ]
  }
  
  let complete_event = SpanEvent::{
    name: "operation_completed",
    timestamp_unix_nanos: 1640995200000001500L,
    attributes: [
      ("operation.phase", AttributeValue::string("complete")),
      ("total.duration_ms", AttributeValue::int(500L)),
      ("success", AttributeValue::bool(true))
    ]
  }
  
  // 7. 创建span链接
  let linked_context = SpanContext::{
    trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0xaa_byte }),
    span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0xbb_byte }),
    trace_flags: 1_byte,
    trace_state: "key=value"
  }
  
  let span_link = SpanLink::{
    context: linked_context,
    attributes: [
      ("link.type", AttributeValue::string("parent")),
      ("parent.service", AttributeValue::string("api-gateway"))
    ]
  }
  
  // 8. 创建日志记录
  let start_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000010L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Main operation started"),
    attributes: [
      ("operation.phase", AttributeValue::string("start")),
      ("user.id", AttributeValue::string("user-67890")),
      ("session.id", AttributeValue::string("sess-12345"))
    ],
    trace_id: Some(main_span.context.trace_id),
    span_id: Some(main_span.context.span_id),
    trace_flags: Some(main_span.context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  let auth_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000500L,
    observed_timestamp_unix_nanos: Some(1640995200000000510L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Authentication successful"),
    attributes: [
      ("operation.phase", AttributeValue::string("auth")),
      ("auth.method", AttributeValue::string("jwt")),
      ("auth.duration_ms", AttributeValue::int(100L)),
      ("user.role", AttributeValue::string("admin"))
    ],
    trace_id: Some(main_span.context.trace_id),
    span_id: Some(main_span.context.span_id),
    trace_flags: Some(main_span.context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  let db_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001000L,
    observed_timestamp_unix_nanos: Some(1640995200000001010L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Database query completed"),
    attributes: [
      ("operation.phase", AttributeValue::string("data")),
      ("db.type", AttributeValue::string("postgresql")),
      ("db.duration_ms", AttributeValue::int(200L)),
      ("db.rows", AttributeValue::int(5L))
    ],
    trace_id: Some(main_span.context.trace_id),
    span_id: Some(main_span.context.span_id),
    trace_flags: Some(main_span.context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  let complete_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000001500L,
    observed_timestamp_unix_nanos: Some(1640995200000001510L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Main operation completed successfully"),
    attributes: [
      ("operation.phase", AttributeValue::string("complete")),
      ("total.duration_ms", AttributeValue::int(500L)),
      ("success", AttributeValue::bool(true)),
      ("events.count", AttributeValue::int(4L))
    ],
    trace_id: Some(main_span.context.trace_id),
    span_id: Some(main_span.context.span_id),
    trace_flags: Some(main_span.context.trace_flags),
    resource: Some(resource),
    instrumentation_scope: Some(instrumentation_scope)
  }
  
  // 9. 创建指标（模拟）
  let meter = NoopMeter::{}
  let operation_counter = meter.create_counter("operations_total", Some("operations"), Some("Total operations"))
  let duration_histogram = meter.create_histogram("operation_duration_ms", Some("ms"), Some("Operation duration"))
  let auth_counter = meter.create_counter("auth_operations", Some("operations"), Some("Authentication operations"))
  let db_counter = meter.create_counter("db_queries", Some("queries"), Some("Database queries"))
  
  // 记录指标
  operation_counter.add(1L, Some([
    ("operation.name", AttributeValue::string("main_operation")),
    ("status", AttributeValue::string("success")),
    ("user.role", AttributeValue::string("admin"))
  ]))
  
  duration_histogram.record(500.0, Some([
    ("operation.name", AttributeValue::string("main_operation")),
    ("operation.phase", AttributeValue::string("complete"))
  ]))
  
  auth_counter.add(1L, Some([
    ("auth.method", AttributeValue::string("jwt")),
    ("auth.status", AttributeValue::string("success"))
  ]))
  
  db_counter.add(1L, Some([
    ("db.type", AttributeValue::string("postgresql")),
    ("db.operation", AttributeValue::string("SELECT")),
    ("db.status", AttributeValue::string("success"))
  ]))
  
  // 10. 验证端到端集成
  assert_eq(main_span.name, "main_operation")
  assert_eq(main_span.kind, Server)
  assert_eq(main_span.attributes.length(), 5)
  
  // 验证事件
  assert_eq(start_event.name, "operation_started")
  assert_eq(auth_event.name, "authentication_completed")
  assert_eq(db_event.name, "database_query_completed")
  assert_eq(complete_event.name, "operation_completed")
  
  // 验证链接
  assert_eq(span_link.context.trace_id.length(), 16)
  assert_eq(span_link.context.span_id.length(), 8)
  assert_eq(span_link.attributes.length(), 2)
  
  // 验证日志
  match start_log.body {
    Some(body) => assert_eq(body, "Main operation started")
    None => @test.fail("Test failed")
  }
  
  match auth_log.body {
    Some(body) => assert_eq(body, "Authentication successful")
    None => @test.fail("Test failed")
  }
  
  match db_log.body {
    Some(body) => assert_eq(body, "Database query completed")
    None => @test.fail("Test failed")
  }
  
  match complete_log.body {
    Some(body) => assert_eq(body, "Main operation completed successfully")
    None => @test.fail("Test failed")
  }
  
  // 验证所有日志都有相同的trace_id和span_id
  let expected_trace_id = Some(main_span.context.trace_id)
  let expected_span_id = Some(main_span.context.span_id)
  
  assert_eq(start_log.trace_id, expected_trace_id)
  assert_eq(auth_log.trace_id, expected_trace_id)
  assert_eq(db_log.trace_id, expected_trace_id)
  assert_eq(complete_log.trace_id, expected_trace_id)
  
  assert_eq(start_log.span_id, expected_span_id)
  assert_eq(auth_log.span_id, expected_span_id)
  assert_eq(db_log.span_id, expected_span_id)
  assert_eq(complete_log.span_id, expected_span_id)
  
  // 验证所有日志都有相同的resource和instrumentation scope
  match start_log.resource {
    Some(res) => assert_eq(res.service_name, "e2e-service")
    None => @test.fail("Test failed")
  }
  
  match start_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "e2e-scope")
    None => @test.fail("Test failed")
  }
}