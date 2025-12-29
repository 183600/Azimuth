// 资源生命周期管理测试用例
// 测试遥测系统资源的创建、使用和销毁

test "resource_lifecycle_management" {
  // 测试Resource的生命周期管理
  
  // 1. 资源创建阶段
  let created_resource = Resource::default("lifecycle-test-service")
  assert_eq(created_resource.service_name, "lifecycle-test-service")
  assert_eq(created_resource.service_version, None)
  assert_eq(created_resource.telemetry_sdk_name, "azimuth")
  assert_eq(created_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(created_resource.attributes.length(), 0)
  
  // 2. 资源初始化阶段
  let initialized_resource = Resource::{
    service_name: "lifecycle-test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("lifecycle.phase", AttributeValue::string("initialized")),
      ("lifecycle.timestamp", AttributeValue::int(1640995200000000000L)),
      ("lifecycle.status", AttributeValue::string("active"))
    ]
  }
  
  assert_eq(initialized_resource.service_version, Some("1.0.0"))
  assert_eq(initialized_resource.attributes.length(), 3)
  
  // 3. 资源使用阶段
  let active_resource = Resource::{
    service_name: "lifecycle-test-service",
    service_version: Some("1.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("lifecycle.phase", AttributeValue::string("active")),
      ("lifecycle.uptime", AttributeValue::int(3600L)),
      ("lifecycle.requests", AttributeValue::int(1000L)),
      ("lifecycle.errors", AttributeValue::int(5L))
    ]
  }
  
  assert_eq(active_resource.attributes.length(), 4)
  
  // 4. 资源更新阶段
  let updated_resource = Resource::{
    service_name: "lifecycle-test-service",
    service_version: Some("1.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("lifecycle.phase", AttributeValue::string("updated")),
      ("lifecycle.uptime", AttributeValue::int(7200L)),
      ("lifecycle.requests", AttributeValue::int(2500L)),
      ("lifecycle.errors", AttributeValue::int(12L)),
      ("lifecycle.last_update", AttributeValue::int(1640998800000000000L))
    ]
  }
  
  assert_eq(updated_resource.service_version, Some("1.1.0"))
  assert_eq(updated_resource.attributes.length(), 5)
  
  // 5. 资源停用阶段
  let deprecated_resource = Resource::{
    service_name: "lifecycle-test-service",
    service_version: Some("1.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("lifecycle.phase", AttributeValue::string("deprecated")),
      ("lifecycle.deprecation_date", AttributeValue::int(1641085200000000000L)),
      ("lifecycle.replacement", AttributeValue::string("new-service")),
      ("lifecycle.migration_required", AttributeValue::bool(true))
    ]
  }
  
  assert_eq(deprecated_resource.attributes.length(), 4)
  
  // 6. 资源销毁阶段（模拟）
  let destroyed_attributes = [
    ("lifecycle.phase", AttributeValue::string("destroyed")),
    ("lifecycle.destroy_date", AttributeValue::int(1641171600000000000L)),
    ("lifecycle.total_uptime", AttributeValue::int(86400L)),
    ("lifecycle.total_requests", AttributeValue::int(10000L))
  ]
  
  assert_eq(destroyed_attributes.length(), 4)
}

test "instrumentation_scope_lifecycle" {
  // 测试InstrumentationScope的生命周期管理
  
  // 1. 创建阶段
  let created_scope = InstrumentationScope::{
    name: "lifecycle-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema")
  }
  
  assert_eq(created_scope.name, "lifecycle-scope")
  assert_eq(created_scope.version, Some("1.0.0"))
  assert_eq(created_scope.schema_url, Some("https://example.com/schema"))
  
  // 2. 初始化阶段
  let initialized_scope = InstrumentationScope::{
    name: "lifecycle-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 3. 活跃阶段
  let active_scope = InstrumentationScope::{
    name: "lifecycle-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schema/v1")
  }
  
  // 4. 更新阶段
  let updated_scope = InstrumentationScope::{
    name: "lifecycle-scope",
    version: Some("1.1.0"),
    schema_url: Some("https://example.com/schema/v1.1")
  }
  
  assert_eq(updated_scope.version, Some("1.1.0"))
  assert_eq(updated_scope.schema_url, Some("https://example.com/schema/v1.1"))
  
  // 5. 停用阶段
  let deprecated_scope = InstrumentationScope::{
    name: "lifecycle-scope",
    version: Some("1.1.0"),
    schema_url: Some("https://example.com/schema/v1.1")
  }
  
  // 6. 多个scope的生命周期管理
  let scopes = [
    InstrumentationScope::{
      name: "scope-v1",
      version: Some("1.0.0"),
      schema_url: Some("https://example.com/schema/v1")
    },
    InstrumentationScope::{
      name: "scope-v2",
      version: Some("2.0.0"),
      schema_url: Some("https://example.com/schema/v2")
    },
    InstrumentationScope::{
      name: "scope-v3",
      version: Some("3.0.0"),
      schema_url: Some("https://example.com/schema/v3")
    }
  ]
  
  assert_eq(scopes.length(), 3)
  assert_eq(scopes[0].version, Some("1.0.0"))
  assert_eq(scopes[1].version, Some("2.0.0"))
  assert_eq(scopes[2].version, Some("3.0.0"))
}

test "span_lifecycle_management" {
  // 测试Span的生命周期管理
  
  // 1. Span创建阶段
  let ctx = Context::empty()
  let (ctx_with_span, created_span) = NoopTracer::start_span(
    ctx, 
    "lifecycle-operation", 
    Internal, 
    Some([("lifecycle.phase", AttributeValue::string("created"))])
  )
  
  assert_eq(created_span.name, "lifecycle-operation")
  assert_eq(created_span.kind, Internal)
  assert_eq(created_span.status, Unset)
  assert_eq(created_span.end_time_unix_nanos, None)
  
  // 2. Span活跃阶段
  let active_span = Span::{
    name: "lifecycle-operation",
    context: created_span.context,
    kind: Server,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: None,
    status: Unset,
    status_description: None,
    attributes: [
      ("lifecycle.phase", AttributeValue::string("active")),
      ("operation.progress", AttributeValue::int(50L))
    ],
    events: [
      SpanEvent::{
        name: "operation.started",
        timestamp_unix_nanos: 1640995200000000000L,
        attributes: [("lifecycle.event", AttributeValue::string("start"))]
      }
    ],
    links: []
  }
  
  assert_eq(active_span.attributes.length(), 2)
  assert_eq(active_span.events.length(), 1)
  assert_eq(active_span.end_time_unix_nanos, None)
  
  // 3. Span完成阶段
  let completed_span = Span::{
    name: "lifecycle-operation",
    context: active_span.context,
    kind: Server,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000001000L),
    status: Ok,
    status_description: Some("Operation completed successfully"),
    attributes: [
      ("lifecycle.phase", AttributeValue::string("completed")),
      ("operation.progress", AttributeValue::int(100L)),
      ("operation.duration", AttributeValue::int(1000L))
    ],
    events: [
      SpanEvent::{
        name: "operation.started",
        timestamp_unix_nanos: 1640995200000000000L,
        attributes: [("lifecycle.event", AttributeValue::string("start"))]
      },
      SpanEvent::{
        name: "operation.completed",
        timestamp_unix_nanos: 1640995200000001000L,
        attributes: [("lifecycle.event", AttributeValue::string("end"))]
      }
    ],
    links: []
  }
  
  assert_eq(completed_span.status, Ok)
  assert_eq(completed_span.end_time_unix_nanos, Some(1640995200000001000L))
  assert_eq(completed_span.events.length(), 2)
  
  // 4. Span错误阶段
  let error_span = Span::{
    name: "lifecycle-operation",
    context: created_span.context,
    kind: Server,
    parent_span_id: Some([for i = 0; i < 8; i = i + 1].map(fn(_) { 0x01_byte })),
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200000000500L),
    status: Error,
    status_description: Some("Operation failed due to timeout"),
    attributes: [
      ("lifecycle.phase", AttributeValue::string("error")),
      ("error.type", AttributeValue::string("timeout")),
      ("error.message", AttributeValue::string("Operation timed out"))
    ],
    events: [
      SpanEvent::{
        name: "error.occurred",
        timestamp_unix_nanos: 1640995200000000500L,
        attributes: [
          ("lifecycle.event", AttributeValue::string("error")),
          ("error.code", AttributeValue::string("TIMEOUT"))
        ]
      }
    ],
    links: []
  }
  
  assert_eq(error_span.status, Error)
  assert_eq(error_span.status_description, Some("Operation failed due to timeout"))
  assert_eq(error_span.events.length(), 1)
  
  // 5. 嵌套Span的生命周期
  let (parent_ctx, parent_span) = NoopTracer::start_span(
    ctx, 
    "parent-operation", 
    Server, 
    Some([("lifecycle.level", AttributeValue::string("parent"))])
  )
  
  let (child_ctx, child_span) = NoopTracer::start_span(
    parent_ctx, 
    "child-operation", 
    Client, 
    Some([("lifecycle.level", AttributeValue::string("child"))])
  )
  
  assert_eq(parent_span.name, "parent-operation")
  assert_eq(child_span.name, "child-operation")
  assert_eq(child_span.parent_span_id.is_some(), true)
}

test "log_record_lifecycle" {
  // 测试LogRecord的生命周期管理
  
  // 1. 日志创建阶段
  let created_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Log record created"),
    attributes: [
      ("lifecycle.phase", AttributeValue::string("created")),
      ("log.source", AttributeValue::string("test-module"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: None
  }
  
  assert_eq(created_log.body, Some("Log record created"))
  assert_eq(created_log.severity_number, Info)
  assert_eq(created_log.attributes.length(), 2)
  
  // 2. 日志处理阶段
  let processing_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Processing log record"),
    attributes: [
      ("lifecycle.phase", AttributeValue::string("processing")),
      ("log.processor", AttributeValue::string("log-processor-1")),
      ("log.queue.position", AttributeValue::int(5L))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("log-processor-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "log-processor",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  assert_eq(processing_log.body, Some("Processing log record"))
  assert_eq(processing_log.resource.is_some(), true)
  assert_eq(processing_log.instrumentation_scope.is_some(), true)
  
  // 3. 日志输出阶段
  let output_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Log record output to destination"),
    attributes: [
      ("lifecycle.phase", AttributeValue::string("output")),
      ("output.destination", AttributeValue::string("file")),
      ("output.format", AttributeValue::string("json")),
      ("output.size", AttributeValue::int(1024L))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("log-processor-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "log-processor",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  assert_eq(output_log.body, Some("Log record output to destination"))
  assert_eq(output_log.attributes.length(), 4)
  
  // 4. 错误日志的生命周期
  let error_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Error,
    severity_text: Some("ERROR"),
    body: Some("Error occurred during log processing"),
    attributes: [
      ("lifecycle.phase", AttributeValue::string("error")),
      ("error.type", AttributeValue::string("processing.error")),
      ("error.retry.count", AttributeValue::int(3L)),
      ("error.final", AttributeValue::bool(true))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("log-processor-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "log-processor",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  assert_eq(error_log.severity_number, Error)
  assert_eq(error_log.attributes.length(), 4)
  
  // 5. 使用LogRecordBuilder的生命周期
  let builder_log = LogRecord::builder()
    .timestamp(1640995200000000000L)
    .severity(logs::SeverityNumber::Warn)
    .body("Warning log using builder")
    .with_attribute("lifecycle.phase", AttributeValue::string("builder"))
    .with_attribute("builder.version", AttributeValue::string("1.0.0"))
    .build()
  
  assert_eq(builder_log.body, Some("Warning log using builder"))
  assert_eq(builder_log.severity_number, Warn)
  assert_eq(builder_log.attributes.length(), 2)
}

test "context_lifecycle_management" {
  // 测试Context的生命周期管理
  
  // 1. 上下文创建阶段
  let created_ctx = Context::empty()
  assert_eq(created_ctx.values.length(), 0)
  
  // 2. 上下文初始化阶段
  let init_key = create_key("lifecycle.phase")
  let init_ctx = created_ctx
    .with_value(init_key, "initialized")
    .with_value(create_key("lifecycle.timestamp"), "1640995200000000000")
  
  assert_eq(init_ctx.values.length(), 2)
  
  // 3. 上下文活跃阶段
  let active_ctx = init_ctx
    .with_value(create_key("lifecycle.phase"), "active")
    .with_value(create_key("lifecycle.uptime"), "3600")
    .with_value(create_key("lifecycle.operations"), "100")
  
  assert_eq(active_ctx.values.length(), 4)
  
  // 4. 上下文传播阶段
  let propagation_key = create_key("lifecycle.propagation")
  let propagated_ctx = active_ctx
    .with_value(propagation_key, "in-progress")
    .with_value(create_key("lifecycle.hops"), "3")
  
  assert_eq(propagated_ctx.values.length(), 6)
  
  // 5. 上下文清理阶段
  let cleanup_ctx = propagated_ctx
    .with_value(create_key("lifecycle.phase"), "cleanup")
    .with_value(create_key("lifecycle.cleanup.reason"), "shutdown")
  
  assert_eq(cleanup_ctx.values.length(), 8)
  
  // 6. 验证上下文状态
  match cleanup_ctx.get(init_key) {
    Some(phase) => assert_eq(phase, "cleanup")
    None => @test.fail("Test failed")
  }
  
  match cleanup_ctx.get(create_key("lifecycle.cleanup.reason")) {
    Some(reason) => assert_eq(reason, "shutdown")
    None => @test.fail("Test failed")
  }
  
  // 7. 上下文的层次化生命周期
  let root_ctx = Context::empty()
  let child_ctx = root_ctx
    .with_value(create_key("context.level"), "child")
    .with_value(create_key("context.parent"), "root")
  
  let grandchild_ctx = child_ctx
    .with_value(create_key("context.level"), "grandchild")
    .with_value(create_key("context.parent"), "child")
    .with_value(create_key("context.root"), "root")
  
  assert_eq(root_ctx.values.length(), 0)
  assert_eq(child_ctx.values.length(), 2)
  assert_eq(grandchild_ctx.values.length(), 3)
}

test "baggage_lifecycle_management" {
  // 测试Baggage的生命周期管理
  
  // 1. Baggage创建阶段
  let created_baggage = Baggage::empty()
  assert_eq(created_baggage.entries.length(), 0)
  
  // 2. Baggage初始化阶段
  let init_baggage = created_baggage
    .with_entry("lifecycle.phase", "created")
    .with_entry("lifecycle.timestamp", "1640995200000000000")
  
  assert_eq(init_baggage.entries.length(), 2)
  
  // 3. Baggage传播阶段
  let propagation_baggage = init_baggage
    .with_entry("lifecycle.phase", "propagation")
    .with_entry("lifecycle.hops", "1")
    .with_entry("lifecycle.service", "service-a")
  
  assert_eq(propagation_baggage.entries.length(), 4)
  
  // 4. Baggage更新阶段
  let updated_baggage = propagation_baggage
    .with_entry("lifecycle.phase", "updated")
    .with_entry("lifecycle.hops", "2")
    .with_entry("lifecycle.service", "service-b")
    .with_entry("lifecycle.updated.timestamp", "1640995200000001000")
  
  assert_eq(updated_baggage.entries.length(), 5)
  
  // 5. Baggage清理阶段
  let cleanup_baggage = updated_baggage
    .with_entry("lifecycle.phase", "cleanup")
    .with_entry("lifecycle.cleanup.reason", "end-of-request")
  
  assert_eq(cleanup_baggage.entries.length(), 6)
  
  // 6. 验证Baggage状态
  match cleanup_baggage.get("lifecycle.phase") {
    Some(phase) => assert_eq(phase, "cleanup")
    None => @test.fail("Test failed")
  }
  
  match cleanup_baggage.get("lifecycle.cleanup.reason") {
    Some(reason) => assert_eq(reason, "end-of-request")
    None => @test.fail("Test failed")
  }
  
  // 7. Baggage的跨服务传播生命周期
  let service_a_baggage = Baggage::empty()
    .with_entry("service.name", "service-a")
    .with_entry("request.id", "req-123")
    .with_entry("user.id", "user-456")
  
  let service_b_baggage = service_a_baggage
    .with_entry("service.name", "service-b")
    .with_entry("service.a.timestamp", "1640995200000000000")
    .with_entry("service.b.timestamp", "1640995200000000500")
  
  let service_c_baggage = service_b_baggage
    .with_entry("service.name", "service-c")
    .with_entry("service.b.timestamp", "1640995200000000500")
    .with_entry("service.c.timestamp", "1640995200000001000")
    .with_entry("total.hops", "2")
  
  assert_eq(service_a_baggage.entries.length(), 3)
  assert_eq(service_b_baggage.entries.length(), 5)
  assert_eq(service_c_baggage.entries.length(), 6)
}

test "metrics_lifecycle_management" {
  // 测试Metrics的生命周期管理
  
  // 1. Meter创建阶段
  let meter_provider = NoopMeterProvider::{}
  let created_meter = meter_provider.get_meter("lifecycle-meter", Some("1.0.0"), Some("https://example.com/schema"))
  
  // 2. Counter创建阶段
  let created_counter = created_meter.create_counter("lifecycle-counter", Some("operations"), Some("Lifecycle counter operations"))
  
  // 3. Counter活跃阶段
  let active_attributes = [
    ("lifecycle.phase", AttributeValue::string("active")),
    ("service.name", AttributeValue::string("lifecycle-service")),
    ("operation.type", AttributeValue::string("increment"))
  ]
  
  created_counter.add(1L, Some(active_attributes))
  created_counter.add(5L, Some(active_attributes))
  created_counter.add(10L, Some(active_attributes))
  
  // 4. Counter更新阶段
  let updated_attributes = [
    ("lifecycle.phase", AttributeValue::string("updated")),
    ("service.name", AttributeValue::string("lifecycle-service")),
    ("operation.type", AttributeValue::string("increment")),
    ("counter.version", AttributeValue::string("2.0.0"))
  ]
  
  created_counter.add(20L, Some(updated_attributes))
  
  // 5. Histogram创建和使用
  let created_histogram = created_meter.create_histogram("lifecycle-histogram", Some("ms"), Some("Lifecycle duration histogram"))
  
  let histogram_attributes = [
    ("lifecycle.phase", AttributeValue::string("measurement")),
    ("service.name", AttributeValue::string("lifecycle-service")),
    ("operation.type", AttributeValue::string("duration"))
  ]
  
  created_histogram.record(100.0, Some(histogram_attributes))
  created_histogram.record(250.0, Some(histogram_attributes))
  created_histogram.record(500.0, Some(histogram_attributes))
  
  // 6. Gauge创建和使用
  let created_gauge = created_meter.create_gauge("lifecycle-gauge", Some("units"), Some("Lifecycle gauge measurement"))
  
  let gauge_attributes = [
    ("lifecycle.phase", AttributeValue::string("monitoring")),
    ("service.name", AttributeValue::string("lifecycle-service")),
    ("metric.type", AttributeValue::string("gauge"))
  ]
  
  created_gauge.record(75.0, Some(gauge_attributes))
  created_gauge.record(80.0, Some(gauge_attributes))
  created_gauge.record(65.0, Some(gauge_attributes))
  
  // 7. UpDownCounter创建和使用
  let created_up_down_counter = created_meter.create_up_down_counter("lifecycle-up-down", Some("items"), Some("Lifecycle up-down counter"))
  
  let up_down_attributes = [
    ("lifecycle.phase", AttributeValue::string("adjustment")),
    ("service.name", AttributeValue::string("lifecycle-service")),
    ("operation.type", AttributeValue::string("increment_decrement"))
  ]
  
  created_up_down_counter.add(10L, Some(up_down_attributes))
  created_up_down_counter.add(-5L, Some(up_down_attributes))
  created_up_down_counter.add(15L, Some(up_down_attributes))
  created_up_down_counter.add(-3L, Some(up_down_attributes))
  
  // 8. 验证metric操作不会抛出异常
  assert_eq(true, true)  // 如果所有操作都成功执行，测试通过
}

test "propagation_lifecycle_management" {
  // 测试Propagation的生命周期管理
  
  // 1. Propagator创建阶段
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 2. Carrier创建阶段
  let created_carrier = MapCarrier::new()
  assert_eq(created_carrier.data.length(), 0)
  
  // 3. 注入阶段
  let ctx = Context::empty()
  let injection_ctx = ctx
    .with_value(create_key("lifecycle.phase"), "injection")
    .with_value(create_key("lifecycle.timestamp"), "1640995200000000000")
  
  composite_propagator.inject(injection_ctx, created_carrier)
  
  // 4. 验证注入结果
  let trace_parent = created_carrier.get("traceparent")
  let baggage_header = created_carrier.get("baggage")
  
  assert_eq(trace_parent.is_some(), true)
  assert_eq(baggage_header.is_some(), true)
  
  // 5. 提取阶段
  let extraction_ctx = composite_propagator.extract(ctx, created_carrier)
  
  // 6. 传播阶段
  let propagation_carrier = MapCarrier::new()
  let propagation_ctx = extraction_ctx
    .with_value(create_key("lifecycle.phase"), "propagation")
    .with_value(create_key("lifecycle.hops"), "1")
  
  composite_propagator.inject(propagation_ctx, propagation_carrier)
  
  // 7. 验证传播结果
  let propagated_trace_parent = propagation_carrier.get("traceparent")
  let propagated_baggage = propagation_carrier.get("baggage")
  
  assert_eq(propagated_trace_parent.is_some(), true)
  assert_eq(propagated_baggage.is_some(), true)
  
  // 8. 多跳传播生命周期
  let mut current_ctx = ctx
  let mut hop_count = 0
  
  while hop_count < 3 {
    let hop_carrier = MapCarrier::new()
    current_ctx = current_ctx
      .with_value(create_key("lifecycle.phase"), "hop-" + hop_count.to_string())
      .with_value(create_key("lifecycle.hops"), hop_count.to_string())
    
    composite_propagator.inject(current_ctx, hop_carrier)
    current_ctx = composite_propagator.extract(ctx, hop_carrier)
    
    hop_count = hop_count + 1
  }
  
  assert_eq(hop_count, 3)
  
  // 9. 传播清理阶段
  let cleanup_carrier = MapCarrier::new()
  let cleanup_ctx = ctx
    .with_value(create_key("lifecycle.phase"), "cleanup")
    .with_value(create_key("lifecycle.cleanup.reason"), "end-of-trace")
  
  composite_propagator.inject(cleanup_ctx, cleanup_carrier)
  
  // 验证清理状态
  let cleanup_trace_parent = cleanup_carrier.get("traceparent")
  let cleanup_baggage = cleanup_carrier.get("baggage")
  
  assert_eq(cleanup_trace_parent.is_some(), true)
  assert_eq(cleanup_baggage.is_some(), true)
}