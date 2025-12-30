// 配置管理综合测试 - 测试配置加载、验证和动态更新
test "configuration_resource_defaults" {
  // 测试资源配置的默认值
  
  let default_resource = common::Resource::default("test-service")
  
  // 验证默认配置值
  assert_eq(default_resource.service_name, "test-service")
  assert_eq(default_resource.service_version, None)
  assert_eq(default_resource.telemetry_sdk_name, "azimuth")
  assert_eq(default_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(default_resource.attributes.length(), 0)
  
  // 测试不同服务名的默认资源
  let web_service_resource = common::Resource::default("web-service")
  let api_service_resource = common::Resource::default("api-service")
  let db_service_resource = common::Resource::default("database-service")
  
  assert_eq(web_service_resource.service_name, "web-service")
  assert_eq(api_service_resource.service_name, "api-service")
  assert_eq(db_service_resource.service_name, "database-service")
  
  // 验证所有默认资源都有相同的SDK信息
  assert_eq(web_service_resource.telemetry_sdk_name, "azimuth")
  assert_eq(api_service_resource.telemetry_sdk_name, "azimuth")
  assert_eq(db_service_resource.telemetry_sdk_name, "azimuth")
  
  assert_eq(web_service_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(api_service_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(db_service_resource.telemetry_sdk_version, "0.1.0")
}

test "configuration_attribute_validation" {
  // 测试属性配置的验证
  
  // 测试有效的属性配置
  let valid_attributes = [
    ("service.name", common::AttributeValue::string("payment-service")),
    ("service.version", common::AttributeValue::string("2.1.0")),
    ("service.namespace", common::AttributeValue::string("production")),
    ("deployment.environment", common::AttributeValue::string("prod")),
    ("host.name", common::AttributeValue::string("web-server-01")),
    ("host.ip", common::AttributeValue::string("192.168.1.100")),
    ("process.id", common::AttributeValue::int(12345L)),
    ("process.name", common::AttributeValue::string("payment-processor")),
    ("process.command", common::AttributeValue::string("/usr/bin/payment-service")),
    ("process.command_args", common::AttributeValue::array_string(["--config", "/etc/payment/config.yaml"])),
    ("process.executable.name", common::AttributeValue::string("payment-service")),
    ("process.executable.path", common::AttributeValue::string("/usr/bin/payment-service")),
    ("process.runtime.name", common::AttributeValue::string("node")),
    ("process.runtime.version", common::AttributeValue::string("18.17.0")),
    ("process.runtime.description", common::AttributeValue::string("Node.js")),
    ("os.type", common::AttributeValue::string("linux")),
    ("os.name", common::AttributeValue::string("Ubuntu")),
    ("os.version", common::AttributeValue::string("22.04")),
    ("os.description", common::AttributeValue::string("Ubuntu 22.04 LTS")),
    ("cloud.provider", common::AttributeValue::string("aws")),
    ("cloud.account.id", common::AttributeValue::string("123456789012")),
    ("cloud.region", common::AttributeValue::string("us-west-2")),
    ("cloud.availability_zone", common::AttributeValue::string("us-west-2a")),
    ("cloud.platform", common::AttributeValue::string("aws_ec2"))
  ]
  
  // 验证有效属性配置
  assert_eq(valid_attributes.length(), 22)
  
  // 验证特定属性值
  match valid_attributes[0].1 {
    common::StringValue(service_name) => assert_eq(service_name, "payment-service")
    _ => @test.fail("Test failed")
  }
  
  match valid_attributes[6].1 {
    common::IntValue(process_id) => assert_eq(process_id, 12345L)
    _ => @test.fail("Test failed")
  }
  
  match valid_attributes[9].1 {
    common::ArrayStringValue(args) => {
      assert_eq(args.length(), 2)
      assert_eq(args[0], "--config")
      assert_eq(args[1], "/etc/payment/config.yaml")
    }
    _ => @test.fail("Test failed")
  }
}

test "configuration_span_kinds_and_status" {
  // 测试Span类型和状态配置
  
  // 测试所有SpanKind枚举值
  let internal_span_kind = trace::Internal
  let server_span_kind = trace::Server
  let client_span_kind = trace::Client
  let producer_span_kind = trace::Producer
  let consumer_span_kind = trace::Consumer
  
  // 创建不同类型的Span
  let ctx = context::Context::empty()
  
  let (_, internal_span) = trace::NoopTracer::start_span(ctx, "internal_operation", internal_span_kind)
  let (_, server_span) = trace::NoopTracer::start_span(ctx, "server_operation", server_span_kind)
  let (_, client_span) = trace::NoopTracer::start_span(ctx, "client_operation", client_span_kind)
  let (_, producer_span) = trace::NoopTracer::start_span(ctx, "producer_operation", producer_span_kind)
  let (_, consumer_span) = trace::NoopTracer::start_span(ctx, "consumer_operation", consumer_span_kind)
  
  // 验证Span类型配置
  assert_eq(internal_span.kind, trace::Internal)
  assert_eq(server_span.kind, trace::Server)
  assert_eq(client_span.kind, trace::Client)
  assert_eq(producer_span.kind, trace::Producer)
  assert_eq(consumer_span.kind, trace::Consumer)
  
  // 测试所有StatusCode枚举值
  let unset_status = trace::Unset
  let ok_status = trace::Ok
  let error_status = trace::Error
  
  // 创建不同状态的Span
  let unset_span = trace::Span::{
    name: "unset_status_span",
    context: trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: None,
    status: unset_status,
    status_description: None,
    attributes: [],
    events: [],
    links: []
  }
  
  let ok_span = trace::Span::{
    name: "ok_status_span",
    context: trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(2000L),
    status: ok_status,
    status_description: Some("Operation completed successfully"),
    attributes: [],
    events: [],
    links: []
  }
  
  let error_span = trace::Span::{
    name: "error_status_span",
    context: trace::SpanContext::{
      trace_id: [for i = 0; i < 16; i = i + 1].map(fn(_) { 0_byte }),
      span_id: [for i = 0; i < 8; i = i + 1].map(fn(_) { 0_byte }),
      trace_flags: 0_byte,
      trace_state: ""
    },
    kind: trace::Internal,
    parent_span_id: None,
    start_time_unix_nanos: 1000L,
    end_time_unix_nanos: Some(1500L),
    status: error_status,
    status_description: Some("Operation failed with timeout"),
    attributes: [],
    events: [],
    links: []
  }
  
  // 验证Span状态配置
  assert_eq(unset_span.status, trace::Unset)
  assert_eq(ok_span.status, trace::Ok)
  assert_eq(error_span.status, trace::Error)
  
  match ok_span.status_description {
    Some(description) => assert_eq(description, "Operation completed successfully")
    None => @test.fail("Test failed")
  }
  
  match error_span.status_description {
    Some(description) => assert_eq(description, "Operation failed with timeout")
    None => @test.fail("Test failed")
  }
}

test "configuration_metric_types" {
  // 测试指标类型配置
  
  // 测试所有InstrumentType枚举值
  let counter_type = metrics::Counter
  let histogram_type = metrics::Histogram
  let up_down_counter_type = metrics::UpDownCounter
  let gauge_type = metrics::Gauge
  let observable_counter_type = metrics::ObservableCounter
  let observable_gauge_type = metrics::ObservableGauge
  let observable_up_down_counter_type = metrics::ObservableUpDownCounter
  
  // 创建不同类型的指标仪器
  let meter_provider = metrics::NoopMeterProvider::{}
  let meter = meter_provider.get_meter("test_meter", Some("1.0.0"))
  
  let counter = meter.create_counter("request_count", Some("requests"), Some("Total number of requests"))
  let histogram = meter.create_histogram("request_duration", Some("ms"), Some("Request duration in milliseconds"))
  let up_down_counter = meter.create_up_down_counter("active_connections", Some("connections"), Some("Number of active connections"))
  let gauge = meter.create_gauge("cpu_usage", Some("percent"), Some("CPU usage percentage"))
  
  // 验证指标仪器创建成功
  // 注意：由于返回的是No-op实现，我们主要验证不会崩溃
  counter.add(1L, Some([("endpoint", common::AttributeValue::string("/api/test"))]))
  histogram.record(100.5, Some([("endpoint", common::AttributeValue::string("/api/test"))]))
  up_down_counter.add(1L, Some([("endpoint", common::AttributeValue::string("/api/test"))]))
  gauge.record(75.5, Some([("endpoint", common::AttributeValue::string("/api/test"))]))
  
  assert_eq(true, true)  // 如果没有崩溃，测试通过
}

test "configuration_propagation_headers" {
  // 测试传播头部配置
  
  // 测试标准W3C头部名称
  let trace_parent_header = propagation::TRACE_PARENT_HEADER
  let trace_state_header = propagation::TRACE_STATE_HEADER
  let baggage_header = propagation::BAGGAGE_HEADER
  
  assert_eq(trace_parent_header, "traceparent")
  assert_eq(trace_state_header, "tracestate")
  assert_eq(baggage_header, "baggage")
  
  // 测试自定义头部配置
  let custom_headers = [
    ("x-custom-trace-id", "custom-trace-123"),
    ("x-custom-span-id", "custom-span-456"),
    ("x-custom-baggage", "user.id=123,session.id=456"),
    ("x-correlation-id", "correlation-789")
  ]
  
  let carrier_with_custom_headers = propagation::MapCarrier::new(custom_headers)
  
  // 验证自定义头部
  match carrier_with_custom_headers.get("x-custom-trace-id") {
    Some(trace_id) => assert_eq(trace_id, "custom-trace-123")
    None => @test.fail("Test failed")
  }
  
  match carrier_with_custom_headers.get("x-custom-baggage") {
    Some(baggage) => assert_eq(baggage, "user.id=123,session.id=456")
    None => @test.fail("Test failed")
  }
  
  // 测试复合传播器配置
  let trace_propagator = propagation::W3CTraceContextPropagator::{}
  let baggage_propagator = propagation::W3CBaggagePropagator::{}
  
  let single_propagator = propagation::CompositePropagator::new([trace_propagator])
  let dual_propagator = propagation::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // 验证传播器配置
  let ctx = context::Context::empty()
  
  single_propagator.inject(ctx, carrier_with_custom_headers)
  dual_propagator.inject(ctx, carrier_with_custom_headers)
  
  let _extracted_ctx_single = single_propagator.extract(ctx, carrier_with_custom_headers)
  let _extracted_ctx_dual = dual_propagator.extract(ctx, carrier_with_custom_headers)
  
  assert_eq(true, true)  // 如果没有崩溃，测试通过
}

test "configuration_context_keys" {
  // 测试上下文键配置
  
  // 测试不同类型的上下文键
  let trace_id_key = context::create_key("trace_id")
  let span_id_key = context::create_key("span_id")
  let user_id_key = context::create_key("user_id")
  let session_id_key = context::create_key("session_id")
  let request_id_key = context::create_key("request_id")
  let tenant_id_key = context::create_key("tenant_id")
  let correlation_id_key = context::create_key("correlation_id")
  
  // 验证键创建
  assert_eq(trace_id_key.name, "trace_id")
  assert_eq(span_id_key.name, "span_id")
  assert_eq(user_id_key.name, "user_id")
  assert_eq(session_id_key.name, "session_id")
  assert_eq(request_id_key.name, "request_id")
  assert_eq(tenant_id_key.name, "tenant_id")
  assert_eq(correlation_id_key.name, "correlation_id")
  
  // 测试上下文键的使用
  let ctx = context::Context::empty()
  
  let enriched_ctx = ctx
    .with_value(trace_id_key, "trace-123456789")
    .with_value(span_id_key, "span-987654321")
    .with_value(user_id_key, "user-12345")
    .with_value(session_id_key, "session-67890")
    .with_value(request_id_key, "req-abcdef")
    .with_value(tenant_id_key, "tenant-001")
    .with_value(correlation_id_key, "corr-xyz")
  
  // 验证上下文值
  match enriched_ctx.get(trace_id_key) {
    Some(trace_id) => assert_eq(trace_id, "trace-123456789")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx.get(user_id_key) {
    Some(user_id) => assert_eq(user_id, "user-12345")
    None => @test.fail("Test failed")
  }
  
  match enriched_ctx.get(tenant_id_key) {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-001")
    None => @test.fail("Test failed")
  }
}

test "configuration_attribute_types" {
  // 测试属性类型配置
  
  // 测试所有属性值类型
  let string_attr = common::AttributeValue::string("string_value")
  let int_attr = common::AttributeValue::int(12345L)
  let float_attr = common::AttributeValue::float(3.14159)
  let bool_attr = common::AttributeValue::bool(true)
  
  let string_array_attr = common::AttributeValue::array_string(["a", "b", "c"])
  let int_array_attr = common::AttributeValue::array_int([1L, 2L, 3L])
  let float_array_attr = common::AttributeValue::array_float([1.1, 2.2, 3.3])
  let bool_array_attr = common::AttributeValue::array_bool([true, false, true])
  
  // 验证属性类型配置
  match string_attr {
    common::StringValue(value) => assert_eq(value, "string_value")
    _ => @test.fail("Test failed")
  }
  
  match int_attr {
    common::IntValue(value) => assert_eq(value, 12345L)
    _ => @test.fail("Test failed")
  }
  
  match float_attr {
    common::FloatValue(value) => assert_eq(value, 3.14159)
    _ => @test.fail("Test failed")
  }
  
  match bool_attr {
    common::BoolValue(value) => assert_eq(value, true)
    _ => @test.fail("Test failed")
  }
  
  match string_array_attr {
    common::ArrayStringValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], "a")
      assert_eq(values[1], "b")
      assert_eq(values[2], "c")
    }
    _ => @test.fail("Test failed")
  }
  
  match int_array_attr {
    common::ArrayIntValue(values) => {
      assert_eq(values.length(), 3)
      assert_eq(values[0], 1L)
      assert_eq(values[1], 2L)
      assert_eq(values[2], 3L)
    }
    _ => @test.fail("Test failed")
  }
}

test "configuration_complex_scenarios" {
  // 测试复杂配置场景
  
  // 测试多层级服务配置
  let microservice_resource = common::Resource::default("payment-service")
  
  let microservice_attributes = [
    // 服务层配置
    ("service.name", common::AttributeValue::string("payment-service")),
    ("service.version", common::AttributeValue::string("2.1.0")),
    ("service.namespace", common::AttributeValue::string("ecommerce")),
    ("service.instance.id", common::AttributeValue::string("payment-pod-123")),
    
    // 部署层配置
    ("deployment.environment", common::AttributeValue::string("production")),
    ("deployment.user", common::AttributeValue::string("deploy-bot")),
    ("deployment.timestamp", common::AttributeValue::int(1640995200L)),
    
    // 基础设施层配置
    ("k8s.pod.name", common::AttributeValue::string("payment-service-7d4f8c9b-xyz")),
    ("k8s.namespace", common::AttributeValue::string("ecommerce-prod")),
    ("k8s.node.name", common::AttributeValue::string("worker-node-3")),
    ("k8s.cluster.name", common::AttributeValue::string("prod-cluster-1")),
    
    // 云提供商配置
    ("cloud.provider", common::AttributeValue::string("gcp")),
    ("cloud.region", common::AttributeValue::string("us-central1")),
    ("cloud.zone", common::AttributeValue::string("us-central1-c")),
    ("cloud.project.id", common::AttributeValue::string("ecommerce-prod-123")),
    
    // 应用层配置
    ("app.framework", common::AttributeValue::string("spring-boot")),
    ("app.java.version", common::AttributeValue::string("17")),
    ("app.profile", common::AttributeValue::string("prod")),
    ("app.config.source", common::AttributeValue::string("config-server"))
  ]
  
  // 验证多层级配置
  assert_eq(microservice_attributes.length(), 20)
  
  // 验证不同层级的配置
  match microservice_attributes[0].1 {
    common::StringValue(name) => assert_eq(name, "payment-service")
    _ => @test.fail("Test failed")
  }
  
  match microservice_attributes[10].1 {
    common::StringValue(pod_name) => assert_eq(pod_name, "payment-service-7d4f8c9b-xyz")
    _ => @test.fail("Test failed")
  }
  
  match microservice_attributes[14].1 {
    common::StringValue(cloud_provider) => assert_eq(cloud_provider, "gcp")
    _ => @test.fail("Test failed")
  }
  
  match microservice_attributes[18].1 {
    common::StringValue(framework) => assert_eq(framework, "spring-boot")
    _ => @test.fail("Test failed")
  }
}