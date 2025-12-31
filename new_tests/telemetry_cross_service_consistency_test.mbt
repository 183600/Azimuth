// 跨服务遥测数据一致性测试用例

test "cross_service_trace_propagation_consistency" {
  // 测试跨服务链路追踪传播的一致性
  
  // 模拟服务A创建初始trace
  let trace_id = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E, 0x0F, 0x10]
  let parent_span_id = [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]
  
  let service_a_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: trace_id,
    span_id: parent_span_id,
    trace_flags: 0x01,
    trace_state: "key1=value1,key2=value2"
  }
  
  // 服务A创建的Span
  let service_a_span = azimuth::telemetry::api::trace::Span::{
    name: "service-a-operation",
    context: service_a_context,
    kind: azimuth::telemetry::api::trace::SpanKind::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200100000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Ok,
    status_description: Some("Service A operation completed"),
    attributes: [
      ("service.name", azimuth::telemetry::api::common::AttributeValue::string("service-a")),
      ("service.version", azimuth::telemetry::api::common::AttributeValue::string("1.0.0")),
      ("operation.type", azimuth::telemetry::api::common::AttributeValue::string("business-process"))
    ],
    events: [],
    links: []
  }
  
  // 模拟传播到服务B
  let service_b_span_id = [0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28]
  let service_b_context = azimuth::telemetry::api::trace::SpanContext::{
    trace_id: trace_id,  // 保持相同的trace_id
    span_id: service_b_span_id,
    trace_flags: 0x01,
    trace_state: "key1=value1,key2=value2"  // 保持相同的trace_state
  }
  
  let service_b_span = azimuth::telemetry::api::trace::Span::{
    name: "service-b-operation",
    context: service_b_context,
    kind: azimuth::telemetry::api::trace::SpanKind::Client,
    parent_span_id: Some(parent_span_id),  // 引用父span
    start_time_unix_nanos: 1640995200050000000L,
    end_time_unix_nanos: Some(1640995200150000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Ok,
    status_description: Some("Service B operation completed"),
    attributes: [
      ("service.name", azimuth::telemetry::api::common::AttributeValue::string("service-b")),
      ("service.version", azimuth::telemetry::api::common::AttributeValue::string("2.1.0")),
      ("operation.type", azimuth::telemetry::api::common::AttributeValue::string("data-processing"))
    ],
    events: [],
    links: []
  }
  
  // 验证跨服务一致性
  assert_eq(service_a_span.context.trace_id, service_b_span.context.trace_id)
  assert_eq(service_b_span.parent_span_id.unwrap(), service_a_span.context.span_id)
  assert_eq(service_a_span.context.trace_flags, service_b_span.context.trace_flags)
  assert_eq(service_a_span.context.trace_state, service_b_span.context.trace_state)
  
  // 验证时间顺序
  assert_eq(service_b_span.start_time_unix_nanos >= service_a_span.start_time_unix_nanos, true)
  assert_eq(service_b_span.end_time_unix_nanos.unwrap() >= service_a_span.end_time_unix_nanos.unwrap(), true)
}

test "cross_service_metrics_consistency" {
  // 测试跨服务指标一致性
  
  // 模拟服务A和服务B的指标收集
  let common_metric_attributes = [
    ("trace.id", azimuth::telemetry::api::common::AttributeValue::string("trace-123-abc")),
    ("correlation.id", azimuth::telemetry::api::common::AttributeValue::string("corr-456-def")),
    ("business.process", azimuth::telemetry::api::common::AttributeValue::string("order-processing")),
    ("user.id", azimuth::telemetry::api::common::AttributeValue::string("user-789-ghi"))
  ]
  
  // 服务A特定的指标属性
  let service_a_attributes = [
    ("service.name", azimuth::telemetry::api::common::AttributeValue::string("order-service")),
    ("service.version", azimuth::telemetry::api::common::AttributeValue::string("1.5.2")),
    ("operation.name", azimuth::telemetry::api::common::AttributeValue::string("create-order"))
  ]
  
  // 服务B特定的指标属性
  let service_b_attributes = [
    ("service.name", azimuth::telemetry::api::common::AttributeValue::string("payment-service")),
    ("service.version", azimuth::telemetry::api::common::AttributeValue::string("2.3.1")),
    ("operation.name", azimuth::telemetry::api::common::AttributeValue::string("process-payment"))
  ]
  
  // 合并属性（模拟跨服务传播）
  let service_a_full_attributes = []
  for attr in common_metric_attributes {
    service_a_full_attributes.push(attr)
  }
  for attr in service_a_attributes {
    service_a_full_attributes.push(attr)
  }
  
  let service_b_full_attributes = []
  for attr in common_metric_attributes {
    service_b_full_attributes.push(attr)
  }
  for attr in service_b_attributes {
    service_b_full_attributes.push(attr)
  }
  
  // 验证跨服务指标属性一致性
  assert_eq(service_a_full_attributes.length(), 7)
  assert_eq(service_b_full_attributes.length(), 7)
  
  // 验证共同属性在两个服务中都存在
  let common_keys = ["trace.id", "correlation.id", "business.process", "user.id"]
  for key in common_keys {
    let mut found_in_a = false
    let mut found_in_b = false
    
    for attr in service_a_full_attributes {
      let (attr_key, _) = attr
      if attr_key == key {
        found_in_a = true
        break
      }
    }
    
    for attr in service_b_full_attributes {
      let (attr_key, _) = attr
      if attr_key == key {
        found_in_b = true
        break
      }
    }
    
    assert_eq(found_in_a, true)
    assert_eq(found_in_b, true)
  }
}

test "cross_service_context_consistency" {
  // 测试跨服务上下文一致性
  
  // 创建基础上下文
  let base_context = azimuth::telemetry::api::context::Context::empty()
  
  // 添加跨服务传播的上下文键
  let trace_key = azimuth::telemetry::api::context::create_key("trace_id")
  let correlation_key = azimuth::telemetry::api::context::create_key("correlation_id")
  let user_key = azimuth::telemetry::api::context::create_key("user_id")
  let tenant_key = azimuth::telemetry::api::context::create_key("tenant_id")
  let request_key = azimuth::telemetry::api::context::create_key("request_id")
  
  // 服务A创建的上下文
  let service_a_context = base_context
    .with_value(trace_key, "trace-abc-123-def-456")
    .with_value(correlation_key, "corr-789-ghi-012-jkl")
    .with_value(user_key, "user-345-mno-678-pqr")
    .with_value(tenant_key, "tenant-901-stu-234-vwx")
    .with_value(request_key, "req-567-yza-890-bcd")
  
  // 服务B接收并传播上下文
  let service_b_context = azimuth::telemetry::api::context::Context::empty()
  let service_b_enriched = service_b_context
    .with_value(trace_key, service_a_context.get(trace_key).unwrap())
    .with_value(correlation_key, service_a_context.get(correlation_key).unwrap())
    .with_value(user_key, service_a_context.get(user_key).unwrap())
    .with_value(tenant_key, service_a_context.get(tenant_key).unwrap())
    .with_value(request_key, service_a_context.get(request_key).unwrap())
    .with_value(azimuth::telemetry::api::context::create_key("service.name"), "service-b")
  
  // 服务C继续传播
  let service_c_context = azimuth::telemetry::api::context::Context::empty()
  let service_c_enriched = service_c_context
    .with_value(trace_key, service_b_enriched.get(trace_key).unwrap())
    .with_value(correlation_key, service_b_enriched.get(correlation_key).unwrap())
    .with_value(user_key, service_b_enriched.get(user_key).unwrap())
    .with_value(tenant_key, service_b_enriched.get(tenant_key).unwrap())
    .with_value(request_key, service_b_enriched.get(request_key).unwrap())
    .with_value(azimuth::telemetry::api::context::create_key("service.name"), "service-c")
  
  // 验证跨服务上下文一致性
  assert_eq(service_a_context.get(trace_key).unwrap(), service_b_enriched.get(trace_key).unwrap())
  assert_eq(service_b_enriched.get(trace_key).unwrap(), service_c_enriched.get(trace_key).unwrap())
  
  assert_eq(service_a_context.get(correlation_key).unwrap(), service_b_enriched.get(correlation_key).unwrap())
  assert_eq(service_b_enriched.get(correlation_key).unwrap(), service_c_enriched.get(correlation_key).unwrap())
  
  assert_eq(service_a_context.get(user_key).unwrap(), service_c_enriched.get(user_key).unwrap())
}

test "cross_service_baggage_consistency" {
  // 测试跨服务Baggage一致性
  
  // 服务A创建的Baggage
  let service_a_baggage = azimuth::telemetry::api::context::Baggage::empty()
  let service_a_enriched = service_a_baggage
    .with_entry("user.id", "12345")
    .with_entry("user.role", "premium")
    .with_entry("tenant.id", "tenant-001")
    .with_entry("request.source", "mobile-app")
    .with_entry("feature.flags", "new-ui,advanced-analytics")
    .with_entry("debug.mode", "false")
  
  // 服务B接收并添加Baggage
  let service_b_baggage = azimuth::telemetry::api::context::Baggage::empty()
  let service_b_enriched = service_b_baggage
    .with_entry("user.id", service_a_enriched.get("user.id").unwrap())
    .with_entry("user.role", service_a_enriched.get("user.role").unwrap())
    .with_entry("tenant.id", service_a_enriched.get("tenant.id").unwrap())
    .with_entry("request.source", service_a_enriched.get("request.source").unwrap())
    .with_entry("feature.flags", service_a_enriched.get("feature.flags").unwrap())
    .with_entry("debug.mode", service_a_enriched.get("debug.mode").unwrap())
    .with_entry("service.b.timestamp", "2023-12-01T10:30:00Z")
    .with_entry("service.b.operation", "data-validation")
  
  // 服务C继续传播Baggage
  let service_c_baggage = azimuth::telemetry::api::context::Baggage::empty()
  let service_c_enriched = service_c_baggage
    .with_entry("user.id", service_b_enriched.get("user.id").unwrap())
    .with_entry("user.role", service_b_enriched.get("user.role").unwrap())
    .with_entry("tenant.id", service_b_enriched.get("tenant.id").unwrap())
    .with_entry("request.source", service_b_enriched.get("request.source").unwrap())
    .with_entry("feature.flags", service_b_enriched.get("feature.flags").unwrap())
    .with_entry("debug.mode", service_b_enriched.get("debug.mode").unwrap())
    .with_entry("service.c.timestamp", "2023-12-01T10:30:05Z")
    .with_entry("service.c.operation", "business-logic")
  
  // 验证跨服务Baggage一致性
  assert_eq(service_a_enriched.get("user.id").unwrap(), service_b_enriched.get("user.id").unwrap())
  assert_eq(service_b_enriched.get("user.id").unwrap(), service_c_enriched.get("user.id").unwrap())
  
  assert_eq(service_a_enriched.get("user.role").unwrap(), service_c_enriched.get("user.role").unwrap())
  assert_eq(service_a_enriched.get("feature.flags").unwrap(), service_c_enriched.get("feature.flags").unwrap())
  
  // 验证每个服务都有适当的条目数
  assert_eq(service_a_enriched.entries.length(), 6)
  assert_eq(service_b_enriched.entries.length(), 8)
  assert_eq(service_c_enriched.entries.length(), 8)
}

test "cross_service_resource_consistency" {
  // 测试跨服务资源一致性
  
  // 创建通用的资源属性
  let common_resource_attributes = [
    ("telemetry.sdk.name", azimuth::telemetry::api::common::AttributeValue::string("azimuth")),
    ("telemetry.sdk.version", azimuth::telemetry::api::common::AttributeValue::string("0.1.0")),
    ("deployment.environment", azimuth::telemetry::api::common::AttributeValue::string("production")),
    ("cloud.provider", azimuth::telemetry::api::common::AttributeValue::string("aws")),
    ("cloud.region", azimuth::telemetry::api::common::AttributeValue::string("us-west-2"))
  ]
  
  // 服务A特定的资源
  let service_a_resource = azimuth::telemetry::api::common::Resource::{
    service_name: "auth-service",
    service_version: Some("1.2.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", azimuth::telemetry::api::common::AttributeValue::string("auth-instance-01")),
      ("host.name", azimuth::telemetry::api::common::AttributeValue::string("auth-server-01")),
      ("container.name", azimuth::telemetry::api::common::AttributeValue::string("auth-container"))
    ]
  }
  
  // 服务B特定的资源
  let service_b_resource = azimuth::telemetry::api::common::Resource::{
    service_name: "user-service",
    service_version: Some("2.1.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("service.instance.id", azimuth::telemetry::api::common::AttributeValue::string("user-instance-01")),
      ("host.name", azimuth::telemetry::api::common::AttributeValue::string("user-server-01")),
      ("container.name", azimuth::telemetry::api::common::AttributeValue::string("user-container"))
    ]
  }
  
  // 验证跨服务资源一致性
  assert_eq(service_a_resource.telemetry_sdk_name, service_b_resource.telemetry_sdk_name)
  assert_eq(service_a_resource.telemetry_sdk_version, service_b_resource.telemetry_sdk_version)
  
  // 验证服务特定属性
  assert_eq(service_a_resource.service_name, "auth-service")
  assert_eq(service_a_resource.service_version.unwrap(), "1.2.0")
  assert_eq(service_b_resource.service_name, "user-service")
  assert_eq(service_b_resource.service_version.unwrap(), "2.1.0")
}

test "cross_service_error_propagation_consistency" {
  // 测试跨服务错误传播一致性
  
  // 模拟服务A发生错误
  let trace_id = [0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88, 0x99, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF, 0x00]
  let service_a_span_id = [0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18]
  
  let service_a_error_span = azimuth::telemetry::api::trace::Span::{
    name: "service-a-failing-operation",
    context: azimuth::telemetry::api::trace::SpanContext::{
      trace_id: trace_id,
      span_id: service_a_span_id,
      trace_flags: 0x01,
      trace_state: ""
    },
    kind: azimuth::telemetry::api::trace::SpanKind::Server,
    parent_span_id: None,
    start_time_unix_nanos: 1640995200000000000L,
    end_time_unix_nanos: Some(1640995200050000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Error,
    status_description: Some("Database connection failed"),
    attributes: [
      ("service.name", azimuth::telemetry::api::common::AttributeValue::string("service-a")),
      ("error.type", azimuth::telemetry::api::common::AttributeValue::string("database.error")),
      ("error.code", azimuth::telemetry::api::common::AttributeValue::string("DB_CONN_FAILED"))
    ],
    events: [
      azimuth::telemetry::api::trace::SpanEvent::{
        name: "exception",
        timestamp_unix_nanos: 1640995200040000000L,
        attributes: [
          ("exception.type", azimuth::telemetry::api::common::AttributeValue::string("ConnectionException")),
          ("exception.message", azimuth::telemetry::api::common::AttributeValue::string("Unable to connect to database")),
          ("exception.stacktrace", azimuth::telemetry::api::common::AttributeValue::string("at service-a.handler:123"))
        ]
      }
    ],
    links: []
  }
  
  // 服务B接收错误并传播
  let service_b_span_id = [0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28]
  let service_b_error_span = azimuth::telemetry::api::trace::Span::{
    name: "service-b-error-handling",
    context: azimuth::telemetry::api::trace::SpanContext::{
      trace_id: trace_id,  // 保持相同的trace_id
      span_id: service_b_span_id,
      trace_flags: 0x01,
      trace_state: ""
    },
    kind: azimuth::telemetry::api::trace::SpanKind::Client,
    parent_span_id: Some(service_a_span_id),  // 链接到失败的span
    start_time_unix_nanos: 1640995200060000000L,
    end_time_unix_nanos: Some(1640995200080000000L),
    status: azimuth::telemetry::api::trace::StatusCode::Error,
    status_description: Some("Upstream service error"),
    attributes: [
      ("service.name", azimuth::telemetry::api::common::AttributeValue::string("service-b")),
      ("error.type", azimuth::telemetry::api::common::AttributeValue::string("upstream.error")),
      ("upstream.service", azimuth::telemetry::api::common::AttributeValue::string("service-a")),
      ("upstream.error.code", azimuth::telemetry::api::common::AttributeValue::string("DB_CONN_FAILED"))
    ],
    events: [
      azimuth::telemetry::api::trace::SpanEvent::{
        name: "error.received",
        timestamp_unix_nanos: 1640995200070000000L,
        attributes: [
          ("error.source", azimuth::telemetry::api::common::AttributeValue::string("service-a")),
          ("error.propagated", azimuth::telemetry::api::common::AttributeValue::bool(true))
        ]
      }
    ],
    links: []
  }
  
  // 验证错误传播一致性
  assert_eq(service_a_error_span.context.trace_id, service_b_error_span.context.trace_id)
  assert_eq(service_b_error_span.parent_span_id.unwrap(), service_a_error_span.context.span_id)
  assert_eq(service_a_error_span.status, azimuth::telemetry::api::trace::StatusCode::Error)
  assert_eq(service_b_error_span.status, azimuth::telemetry::api::trace::StatusCode::Error)
  
  // 验证错误信息传播
  let mut found_upstream_error = false
  for attr in service_b_error_span.attributes {
    let (key, value) = attr
    if key == "upstream.error.code" {
      match value {
        StringValue(code) => {
          assert_eq(code, "DB_CONN_FAILED")
          found_upstream_error = true
        }
        _ => assert_eq(false, true)
      }
    }
  }
  assert_eq(found_upstream_error, true)
}

test "cross_service_performance_consistency" {
  // 测试跨服务性能一致性
  
  // 模拟分布式请求的性能数据
  let distributed_request = {
    trace_id: "trace-perf-123-456",
    services: [
      {
        name: "gateway",
        start_time: 1640995200000L,
        end_time: 1640995200050L,
        operation: "route-request"
      },
      {
        name: "auth",
        start_time: 1640995200010L,
        end_time: 1640995200030L,
        operation: "authenticate"
      },
      {
        name: "user",
        start_time: 1640995200035L,
        end_time: 1640995200070L,
        operation: "get-profile"
      },
      {
        name: "business",
        start_time: 1640995200040L,
        end_time: 1640995200100L,
        operation: "process-logic"
      }
    ]
  }
  
  // 验证时间顺序一致性
  let services = distributed_request.services
  for i = 0; i < services.length() - 1; i = i + 1 {
    let current = services[i]
    let next = services[i + 1]
    
    // 验证时间顺序的合理性
    assert_eq(current.start_time <= current.end_time, true)
    assert_eq(next.start_time <= next.end_time, true)
    
    // 验证服务间的时间重叠是合理的（允许并发）
    let overlap = current.end_time >= next.start_time && next.end_time >= current.start_time
    let sequential = next.start_time >= current.end_time || current.start_time >= next.end_time
    assert_eq(overlap || sequential, true)
  }
  
  // 验证总请求时间
  let total_start = services[0].start_time
  let total_end = services[services.length() - 1].end_time
  let total_duration = total_end - total_start
  
  assert_eq(total_duration > 0L, true)
  
  // 验证每个服务的持续时间
  for service in services {
    let duration = service.end_time - service.start_time
    assert_eq(duration >= 0L, true)
    assert_eq(duration <= total_duration, true)  // 单个服务时间不应超过总时间
  }
}