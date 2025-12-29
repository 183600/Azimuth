// 跨模块集成增强测试 - 验证遥测系统各模块间的协同工作

use azimuth.telemetry.api.common.{AttributeValue, Attributes, Resource, InstrumentationScope}
use azimuth.telemetry.api.trace.{SpanKind, StatusCode, Span}
use azimuth.telemetry.api.metrics.{MetricType, Instrument, Measurement}
use azimuth.telemetry.api.logs.{LogRecord, Severity, Logger}
use azimuth.telemetry.api.context.{Context, Baggage, ContextKey}
use azimuth.telemetry.api.propagation.{TextMapPropagator, Propagator}

test "trace_metrics_logs_integration" {
  // 测试追踪、指标和日志三大模块的集成
  
  // 创建共享资源
  let resource = Resource::default("integration-test-service")
  assert_eq(resource.service_name, "integration-test-service")
  
  // 创建检测范围
  let scope = InstrumentationScope::{
    name: "integration-scope",
    version: Some("1.0.0"),
    schema_url: None
  }
  
  // 1. 创建Span并记录指标
  let trace_id = "trace_123456789"
  let span_id = "span_987654321"
  let parent_span = Span::{
    trace_id: trace_id,
    span_id: span_id,
    parent_span_id: None,
    name: "http_request",
    kind: SpanKind::Server,
    start_time: 1640995200L,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [
      ("http.method", AttributeValue::string("GET")),
      ("http.url", AttributeValue::string("/api/users")),
      ("http.scheme", AttributeValue::string("https"))
    ],
    events: [],
    links: []
  }
  
  // 2. 在Span执行过程中记录指标
  let request_duration = Instrument::counter(
    "http_request_duration_ms",
    "HTTP request duration in milliseconds",
    "ms"
  )
  
  let duration_measurement = Measurement::{
    instrument: request_duration,
    value: 125.5,
    attributes: [
      ("http.method", AttributeValue::string("GET")),
      ("http.status_code", AttributeValue::int(200L)),
      ("service.name", AttributeValue::string(resource.service_name))
    ],
    time: 1640995325L
  }
  
  // 3. 在Span执行过程中记录日志
  let log_record = LogRecord::{
    timestamp: 1640995280L,
    observed_timestamp: Some(1640995281L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("HTTP request processed successfully"),
    attributes: [
      ("trace_id", AttributeValue::string(trace_id)),
      ("span_id", AttributeValue::string(span_id)),
      ("user.id", AttributeValue::string("user_123")),
      ("request.duration_ms", AttributeValue::float(125.5))
    ],
    flags: 0,
    trace_id: Some(trace_id),
    span_id: Some(span_id),
    trace_flags: Some(1)
  }
  
  // 4. 验证集成数据一致性
  assert_eq(parent_span.trace_id, trace_id)
  assert_eq(log_record.trace_id, Some(trace_id))
  
  // 验证Span属性与日志属性的一致性
  let span_http_method = parent_span.attributes[0]
  match span_http_method.1 {
    StringValue(method) => assert_eq(method, "GET")
    _ => assert_eq(false, true)
  }
  
  // 验证指标属性与Span属性的关联
  let metric_method = duration_measurement.attributes[0]
  match metric_method.1 {
    StringValue(method) => assert_eq(method, "GET")
    _ => assert_eq(false, true)
  }
  
  // 验证时间戳的递增性
  assert_eq(parent_span.start_time < log_record.timestamp, true)
  assert_eq(log_record.timestamp < duration_measurement.time, true)
}

test "context_propagation_across_modules" {
  // 测试上下文在多个模块间的传播
  
  // 1. 创建根上下文
  let root_context = Context::current()
  let trace_id_key = ContextKey::new("trace_id")
  let user_id_key = ContextKey::new("user_id")
  
  // 2. 在上下文中设置追踪信息
  let trace_context = root_context.with_value(trace_id_key, "trace_ctx_12345")
  let user_context = trace_context.with_value(user_id_key, "user_ctx_67890")
  
  // 3. 创建Baggage并传播
  let baggage = Baggage::empty()
  let enriched_baggage = baggage
    .with("request.id", "req_123")
    .with("session.id", "sess_456")
    .with("tenant.id", "tenant_789")
  
  // 4. 在Span中使用传播的上下文
  let propagated_span = Span::{
    trace_id: "trace_ctx_12345",
    span_id: "span_propagated",
    parent_span_id: None,
    name: "propagated_operation",
    kind: SpanKind::Internal,
    start_time: 1640995400L,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [
      ("user.id", AttributeValue::string("user_ctx_67890")),
      ("request.id", AttributeValue::string("req_123")),
      ("session.id", AttributeValue::string("sess_456")),
      ("tenant.id", AttributeValue::string("tenant_789"))
    ],
    events: [],
    links: []
  }
  
  // 5. 在日志中使用传播的上下文
  let context_log = LogRecord::{
    timestamp: 1640995450L,
    observed_timestamp: Some(1640995451L),
    severity: Some(Severity::Debug),
    severity_text: Some("DEBUG"),
    body: Some("Context propagated successfully across modules"),
    attributes: [
      ("trace.id", AttributeValue::string("trace_ctx_12345")),
      ("user.id", AttributeValue::string("user_ctx_67890")),
      ("baggage.request.id", AttributeValue::string("req_123")),
      ("baggage.session.id", AttributeValue::string("sess_456")),
      ("baggage.tenant.id", AttributeValue::string("tenant_789"))
    ],
    flags: 0,
    trace_id: Some("trace_ctx_12345"),
    span_id: Some("span_propagated"),
    trace_flags: Some(1)
  }
  
  // 6. 验证上下文传播的一致性
  assert_eq(propagated_span.trace_id, "trace_ctx_12345")
  assert_eq(context_log.trace_id, Some("trace_ctx_12345"))
  
  // 验证Baggage信息在Span和日志中的一致性
  let span_tenant = propagated_span.attributes[3]
  match span_tenant.1 {
    StringValue(tenant) => assert_eq(tenant, "tenant_789")
    _ => assert_eq(false, true)
  }
  
  let log_tenant = context_log.attributes[4]
  match log_tenant.1 {
    StringValue(tenant) => assert_eq(tenant, "tenant_789")
    _ => assert_eq(false, true)
  }
}

test "error_propagation_across_modules" {
  // 测试错误在多个模块间的传播和处理
  
  // 1. 在Span中记录错误
  let error_span = Span::{
    trace_id: "error_trace_123",
    span_id: "error_span_456",
    parent_span_id: None,
    name: "failing_operation",
    kind: SpanKind::Client,
    start_time: 1640995500L,
    end_time: Some(1640995600L),
    status: StatusCode::Error,
    attributes: [
      ("error.type", AttributeValue::string("connection_timeout")),
      ("error.message", AttributeValue::string("Failed to connect to external service")),
      ("error.retry_count", AttributeValue::int(3L)),
      ("error.final_attempt", AttributeValue::bool(true))
    ],
    events: [
      ("exception", 1640995550L, [
        ("exception.type", AttributeValue::string("TimeoutException")),
        ("exception.message", AttributeValue::string("Connection timeout after 30 seconds")),
        ("exception.stacktrace", AttributeValue::string("at com.example.Service.connect(Service.java:123)"))
      ])
    ],
    links: []
  }
  
  // 2. 在指标中记录错误率
  let error_counter = Instrument::counter(
    "operation_errors_total",
    "Total number of operation errors",
    "count"
  )
  
  let error_measurement = Measurement::{
    instrument: error_counter,
    value: 1.0,
    attributes: [
      ("operation.name", AttributeValue::string("failing_operation")),
      ("error.type", AttributeValue::string("connection_timeout")),
      ("service.name", AttributeValue::string("error-prone-service"))
    ],
    time: 1640995600L
  }
  
  // 3. 在日志中记录错误详情
  let error_log = LogRecord::{
    timestamp: 1640995600L,
    observed_timestamp: Some(1640995601L),
    severity: Some(Severity::Error),
    severity_text: Some("ERROR"),
    body: Some("Operation failed after 3 retry attempts"),
    attributes: [
      ("trace_id", AttributeValue::string("error_trace_123")),
      ("span_id", AttributeValue::string("error_span_456")),
      ("error.type", AttributeValue::string("connection_timeout")),
      ("error.message", AttributeValue::string("Failed to connect to external service")),
      ("error.retry_count", AttributeValue::int(3L)),
      ("error.duration_ms", AttributeValue::float(100.0)),
      ("operation.status", AttributeValue::string("failed"))
    ],
    flags: 0,
    trace_id: Some("error_trace_123"),
    span_id: Some("error_span_456"),
    trace_flags: Some(1)
  }
  
  // 4. 验证错误信息在模块间的一致性
  assert_eq(error_span.status, StatusCode::Error)
  assert_eq(error_log.severity, Some(Severity::Error))
  
  // 验证错误类型的一致性
  let span_error_type = error_span.attributes[0]
  match span_error_type.1 {
    StringValue(error_type) => assert_eq(error_type, "connection_timeout")
    _ => assert_eq(false, true)
  }
  
  let metric_error_type = error_measurement.attributes[1]
  match metric_error_type.1 {
    StringValue(error_type) => assert_eq(error_type, "connection_timeout")
    _ => assert_eq(false, true)
  }
  
  let log_error_type = error_log.attributes[2]
  match log_error_type.1 {
    StringValue(error_type) => assert_eq(error_type, "connection_timeout")
    _ => assert_eq(false, true)
  }
  
  // 验证时间线的一致性
  assert_eq(error_span.start_time < error_span.end_time.unwrap(), true)
  assert_eq(error_span.end_time.unwrap() <= error_log.timestamp, true)
  assert_eq(error_log.timestamp, error_measurement.time)
}

test "resource_metadata_consistency" {
  // 测试资源元数据在各个模块间的一致性
  
  // 1. 创建带完整元数据的资源
  let detailed_resource = Resource::{
    service_name: "detailed-service",
    service_version: Some("2.1.0"),
    service_namespace: Some("production"),
    service_instance_id: Some("instance-abc123"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    telemetry_sdk_language: "moonbit",
    attributes: [
      ("service.environment", AttributeValue::string("production")),
      ("service.region", AttributeValue::string("us-west-2")),
      ("service.zone", AttributeValue::string("us-west-2a")),
      ("deployment.color", AttributeValue::string("blue")),
      ("k8s.pod.name", AttributeValue::string("detailed-service-7f8d9c2d")),
      ("k8s.namespace", AttributeValue::string("default"))
    ]
  }
  
  // 2. 在Span中使用资源元数据
  let resource_span = Span::{
    trace_id: "resource_trace_123",
    span_id: "resource_span_456",
    parent_span_id: None,
    name: "resource_aware_operation",
    kind: SpanKind::Server,
    start_time: 1640995700L,
    end_time: None,
    status: StatusCode::Unset,
    attributes: [
      ("service.name", AttributeValue::string(detailed_resource.service_name)),
      ("service.version", AttributeValue::string(detailed_resource.service_version.unwrap())),
      ("service.namespace", AttributeValue::string(detailed_resource.service_namespace.unwrap())),
      ("service.instance.id", AttributeValue::string(detailed_resource.service_instance_id.unwrap())),
      ("service.environment", AttributeValue::string("production")),
      ("service.region", AttributeValue::string("us-west-2"))
    ],
    events: [],
    links: []
  }
  
  // 3. 在指标中使用资源元数据
  let resource_metric = Measurement::{
    instrument: Instrument::counter("resource_operations_total", "Total operations with resource metadata", "count"),
    value: 1.0,
    attributes: [
      ("service.name", AttributeValue::string(detailed_resource.service_name)),
      ("service.version", AttributeValue::string(detailed_resource.service_version.unwrap())),
      ("service.namespace", AttributeValue::string(detailed_resource.service_namespace.unwrap())),
      ("service.instance.id", AttributeValue::string(detailed_resource.service_instance_id.unwrap())),
      ("k8s.pod.name", AttributeValue::string("detailed-service-7f8d9c2d"))
    ],
    time: 1640995750L
  }
  
  // 4. 在日志中使用资源元数据
  let resource_log = LogRecord::{
    timestamp: 1640995800L,
    observed_timestamp: Some(1640995801L),
    severity: Some(Severity::Info),
    severity_text: Some("INFO"),
    body: Some("Operation completed with full resource context"),
    attributes: [
      ("service.name", AttributeValue::string(detailed_resource.service_name)),
      ("service.version", AttributeValue::string(detailed_resource.service_version.unwrap())),
      ("service.namespace", AttributeValue::string(detailed_resource.service_namespace.unwrap())),
      ("service.instance.id", AttributeValue::string(detailed_resource.service_instance_id.unwrap())),
      ("k8s.namespace", AttributeValue::string("default")),
      ("deployment.color", AttributeValue::string("blue"))
    ],
    flags: 0,
    trace_id: Some("resource_trace_123"),
    span_id: Some("resource_span_456"),
    trace_flags: Some(1)
  }
  
  // 5. 验证资源元数据的一致性
  assert_eq(detailed_resource.service_name, "detailed-service")
  assert_eq(detailed_resource.service_version, Some("2.1.0"))
  
  // 验证服务名称在所有模块中的一致性
  let span_service_name = resource_span.attributes[0]
  match span_service_name.1 {
    StringValue(name) => assert_eq(name, "detailed-service")
    _ => assert_eq(false, true)
  }
  
  let metric_service_name = resource_metric.attributes[0]
  match metric_service_name.1 {
    StringValue(name) => assert_eq(name, "detailed-service")
    _ => assert_eq(false, true)
  }
  
  let log_service_name = resource_log.attributes[0]
  match log_service_name.1 {
    StringValue(name) => assert_eq(name, "detailed-service")
    _ => assert_eq(false, true)
  }
  
  // 验证K8s元数据的一致性
  let log_k8s_namespace = resource_log.attributes[5]
  match log_k8s_namespace.1 {
    StringValue(namespace) => assert_eq(namespace, "default")
    _ => assert_eq(false, true)
  }
}