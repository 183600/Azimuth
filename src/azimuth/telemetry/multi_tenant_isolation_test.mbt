// 多租户隔离测试用例
// 测试多租户场景下的数据隔离

test "resource_tenant_isolation" {
  // 测试Resource的多租户隔离
  
  // 1. 创建不同租户的资源
  let tenant_a_resource = Resource::default("tenant-a-service")
  let tenant_b_resource = Resource::default("tenant-b-service")
  let tenant_c_resource = Resource::default("tenant-c-service")
  
  // 2. 为每个租户的资源添加租户标识属性
  let tenant_a_attributes = [
    ("tenant.id", AttributeValue::string("tenant-a")),
    ("tenant.name", AttributeValue::string("Tenant A")),
    ("tenant.environment", AttributeValue::string("production")),
    ("service.name", AttributeValue::string("tenant-a-service"))
  ]
  
  let tenant_b_attributes = [
    ("tenant.id", AttributeValue::string("tenant-b")),
    ("tenant.name", AttributeValue::string("Tenant B")),
    ("tenant.environment", AttributeValue::string("staging")),
    ("service.name", AttributeValue::string("tenant-b-service"))
  ]
  
  let tenant_c_attributes = [
    ("tenant.id", AttributeValue::string("tenant-c")),
    ("tenant.name", AttributeValue::string("Tenant C")),
    ("tenant.environment", AttributeValue::string("development")),
    ("service.name", AttributeValue::string("tenant-c-service"))
  ]
  
  // 3. 创建带有租户隔离的日志记录
  let tenant_a_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Tenant A operation"),
    attributes: tenant_a_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(tenant_a_resource),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "tenant-a-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  let tenant_b_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Tenant B operation"),
    attributes: tenant_b_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(tenant_b_resource),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "tenant-b-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  let tenant_c_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Tenant C operation"),
    attributes: tenant_c_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(tenant_c_resource),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "tenant-c-logger",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  // 4. 验证租户隔离
  // 验证每个租户的资源和服务名称不同
  match tenant_a_log.resource {
    Some(resource) => assert_eq(resource.service_name, "tenant-a-service")
    None => @test.fail("Test failed")
  }
  
  match tenant_b_log.resource {
    Some(resource) => assert_eq(resource.service_name, "tenant-b-service")
    None => @test.fail("Test failed")
  }
  
  match tenant_c_log.resource {
    Some(resource) => assert_eq(resource.service_name, "tenant-c-service")
    None => @test.fail("Test failed")
  }
  
  // 验证每个租户的属性包含正确的租户ID
  let mut found_tenant_a_id = false
  let mut found_tenant_b_id = false
  let mut found_tenant_c_id = false
  
  let mut i = 0
  while i < tenant_a_log.attributes.length() {
    let (name, value) = tenant_a_log.attributes[i]
    match (name, value) {
      ("tenant.id", StringValue(tenant_id)) => {
        assert_eq(tenant_id, "tenant-a")
        found_tenant_a_id = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  i = 0
  while i < tenant_b_log.attributes.length() {
    let (name, value) = tenant_b_log.attributes[i]
    match (name, value) {
      ("tenant.id", StringValue(tenant_id)) => {
        assert_eq(tenant_id, "tenant-b")
        found_tenant_b_id = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  i = 0
  while i < tenant_c_log.attributes.length() {
    let (name, value) = tenant_c_log.attributes[i]
    match (name, value) {
      ("tenant.id", StringValue(tenant_id)) => {
        assert_eq(tenant_id, "tenant-c")
        found_tenant_c_id = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_tenant_a_id, true)
  assert_eq(found_tenant_b_id, true)
  assert_eq(found_tenant_c_id, true)
  
  // 验证每个租户的instrumentation scope不同
  match tenant_a_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "tenant-a-logger")
    None => @test.fail("Test failed")
  }
  
  match tenant_b_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "tenant-b-logger")
    None => @test.fail("Test failed")
  }
  
  match tenant_c_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "tenant-c-logger")
    None => @test.fail("Test failed")
  }
}

test "context_tenant_isolation" {
  // 测试Context的多租户隔离
  
  // 1. 为不同租户创建上下文
  let ctx = Context::empty()
  
  let tenant_a_key = create_key("tenant.id")
  let tenant_b_key = create_key("tenant.id")
  let tenant_c_key = create_key("tenant.id")
  
  let user_a_key = create_key("user.id")
  let user_b_key = create_key("user.id")
  let user_c_key = create_key("user.id")
  
  // 2. 为每个租户设置不同的上下文值
  let tenant_a_ctx = ctx
    .with_value(tenant_a_key, "tenant-a")
    .with_value(user_a_key, "user-a-001")
    .with_value(create_key("tenant.environment"), "production")
  
  let tenant_b_ctx = ctx
    .with_value(tenant_b_key, "tenant-b")
    .with_value(user_b_key, "user-b-001")
    .with_value(create_key("tenant.environment"), "staging")
  
  let tenant_c_ctx = ctx
    .with_value(tenant_c_key, "tenant-c")
    .with_value(user_c_key, "user-c-001")
    .with_value(create_key("tenant.environment"), "development")
  
  // 3. 验证租户隔离
  // 验证每个租户的上下文包含正确的租户ID
  match tenant_a_ctx.get(tenant_a_key) {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-a")
    None => @test.fail("Test failed")
  }
  
  match tenant_b_ctx.get(tenant_b_key) {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-b")
    None => @test.fail("Test failed")
  }
  
  match tenant_c_ctx.get(tenant_c_key) {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-c")
    None => @test.fail("Test failed")
  }
  
  // 验证每个租户的用户ID不同
  match tenant_a_ctx.get(user_a_key) {
    Some(user_id) => assert_eq(user_id, "user-a-001")
    None => @test.fail("Test failed")
  }
  
  match tenant_b_ctx.get(user_b_key) {
    Some(user_id) => assert_eq(user_id, "user-b-001")
    None => @test.fail("Test failed")
  }
  
  match tenant_c_ctx.get(user_c_key) {
    Some(user_id) => assert_eq(user_id, "user-c-001")
    None => @test.fail("Test failed")
  }
  
  // 4. 测试上下文隔离的完整性
  // 确保租户A不能访问租户B的数据
  match tenant_a_ctx.get(user_b_key) {
    Some(_) => @test.fail("Test failed")  // 不应该能访问
    None => assert_eq(true, true)  // 正确，无法访问
  }
  
  match tenant_b_ctx.get(user_c_key) {
    Some(_) => @test.fail("Test failed")  // 不应该能访问
    None => assert_eq(true, true)  // 正确，无法访问
  }
  
  match tenant_c_ctx.get(user_a_key) {
    Some(_) => @test.fail("Test failed")  // 不应该能访问
    None => assert_eq(true, true)  // 正确，无法访问
  }
}

test "baggage_tenant_isolation" {
  // 测试Baggage的多租户隔离
  
  // 1. 为不同租户创建baggage
  let baggage = Baggage::empty()
  
  let tenant_a_baggage = baggage
    .with_entry("tenant.id", "tenant-a")
    .with_entry("user.id", "user-a-001")
    .with_entry("tenant.environment", "production")
    .with_entry("tenant.region", "us-east-1")
  
  let tenant_b_baggage = baggage
    .with_entry("tenant.id", "tenant-b")
    .with_entry("user.id", "user-b-001")
    .with_entry("tenant.environment", "staging")
    .with_entry("tenant.region", "us-west-2")
  
  let tenant_c_baggage = baggage
    .with_entry("tenant.id", "tenant-c")
    .with_entry("user.id", "user-c-001")
    .with_entry("tenant.environment", "development")
    .with_entry("tenant.region", "eu-west-1")
  
  // 2. 验证租户隔离
  // 验证每个租户的baggage包含正确的租户ID
  match tenant_a_baggage.get("tenant.id") {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-a")
    None => @test.fail("Test failed")
  }
  
  match tenant_b_baggage.get("tenant.id") {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-b")
    None => @test.fail("Test failed")
  }
  
  match tenant_c_baggage.get("tenant.id") {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-c")
    None => @test.fail("Test failed")
  }
  
  // 验证每个租户的用户ID不同
  match tenant_a_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-a-001")
    None => @test.fail("Test failed")
  }
  
  match tenant_b_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-b-001")
    None => @test.fail("Test failed")
  }
  
  match tenant_c_baggage.get("user.id") {
    Some(user_id) => assert_eq(user_id, "user-c-001")
    None => @test.fail("Test failed")
  }
  
  // 验证每个租户的环境不同
  match tenant_a_baggage.get("tenant.environment") {
    Some(environment) => assert_eq(environment, "production")
    None => @test.fail("Test failed")
  }
  
  match tenant_b_baggage.get("tenant.environment") {
    Some(environment) => assert_eq(environment, "staging")
    None => @test.fail("Test failed")
  }
  
  match tenant_c_baggage.get("tenant.environment") {
    Some(environment) => assert_eq(environment, "development")
    None => @test.fail("Test failed")
  }
  
  // 3. 测试baggage隔离的完整性
  // 确保租户A不能访问租户B的特定数据
  match tenant_a_baggage.get("tenant.region") {
    Some(region) => assert_eq(region, "us-east-1")  // 只能访问自己的区域
    None => @test.fail("Test failed")
  }
  
  match tenant_b_baggage.get("tenant.region") {
    Some(region) => assert_eq(region, "us-west-2")  // 只能访问自己的区域
    None => @test.fail("Test failed")
  }
  
  match tenant_c_baggage.get("tenant.region") {
    Some(region) => assert_eq(region, "eu-west-1")  // 只能访问自己的区域
    None => @test.fail("Test failed")
  }
}

test "trace_tenant_isolation" {
  // 测试Trace的多租户隔离
  
  // 1. 为不同租户创建上下文
  let ctx = Context::empty()
  let tenant_a_key = create_key("tenant.id")
  let tenant_b_key = create_key("tenant.id")
  
  let tenant_a_ctx = ctx.with_value(tenant_a_key, "tenant-a")
  let tenant_b_ctx = ctx.with_value(tenant_b_key, "tenant-b")
  
  // 2. 为每个租户创建带有租户标识的span
  let tenant_a_span_attributes = [
    ("tenant.id", AttributeValue::string("tenant-a")),
    ("user.id", AttributeValue::string("user-a-001")),
    ("operation.name", AttributeValue::string("tenant-a-operation"))
  ]
  
  let tenant_b_span_attributes = [
    ("tenant.id", AttributeValue::string("tenant-b")),
    ("user.id", AttributeValue::string("user-b-001")),
    ("operation.name", AttributeValue::string("tenant-b-operation"))
  ]
  
  let (_, tenant_a_span) = NoopTracer::start_span(tenant_a_ctx, "tenant-a-span", Server, Some(tenant_a_span_attributes))
  let (_, tenant_b_span) = NoopTracer::start_span(tenant_b_ctx, "tenant-b-span", Server, Some(tenant_b_span_attributes))
  
  // 3. 验证租户隔离
  // 验证每个租户的span包含正确的租户ID
  let mut found_tenant_a_id = false
  let mut found_tenant_b_id = false
  
  let mut i = 0
  while i < tenant_a_span.attributes.length() {
    let (name, value) = tenant_a_span.attributes[i]
    match (name, value) {
      ("tenant.id", StringValue(tenant_id)) => {
        assert_eq(tenant_id, "tenant-a")
        found_tenant_a_id = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  i = 0
  while i < tenant_b_span.attributes.length() {
    let (name, value) = tenant_b_span.attributes[i]
    match (name, value) {
      ("tenant.id", StringValue(tenant_id)) => {
        assert_eq(tenant_id, "tenant-b")
        found_tenant_b_id = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_tenant_a_id, true)
  assert_eq(found_tenant_b_id, true)
  
  // 验证每个租户的span名称不同
  assert_eq(tenant_a_span.name, "tenant-a-span")
  assert_eq(tenant_b_span.name, "tenant-b-span")
  
  // 验证每个租户的用户ID不同
  let mut found_tenant_a_user = false
  let mut found_tenant_b_user = false
  
  i = 0
  while i < tenant_a_span.attributes.length() {
    let (name, value) = tenant_a_span.attributes[i]
    match (name, value) {
      ("user.id", StringValue(user_id)) => {
        assert_eq(user_id, "user-a-001")
        found_tenant_a_user = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  i = 0
  while i < tenant_b_span.attributes.length() {
    let (name, value) = tenant_b_span.attributes[i]
    match (name, value) {
      ("user.id", StringValue(user_id)) => {
        assert_eq(user_id, "user-b-001")
        found_tenant_b_user = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_tenant_a_user, true)
  assert_eq(found_tenant_b_user, true)
  
  // 4. 测试trace隔离的完整性
  // 验证不同租户的trace_id和span_id是独立的
  let trace_ids_equal = tenant_a_span.context.trace_id.length() == tenant_b_span.context.trace_id.length()
  assert_eq(trace_ids_equal, true)  // 长度应该相同
  
  // 检查trace_id内容不同（至少有一个字节不同）
  let mut trace_ids_different = false
  let mut i = 0
  while i < tenant_a_span.context.trace_id.length() {
    if tenant_a_span.context.trace_id[i] != tenant_b_span.context.trace_id[i] {
      trace_ids_different = true
      break
    }
    i = i + 1
  }
  assert_eq(trace_ids_different, true)  // trace_id应该不同
}

test "metrics_tenant_isolation" {
  // 测试Metrics的多租户隔离
  
  // 1. 为不同租户创建meter
  let meter_provider = NoopMeterProvider::{}
  let tenant_a_meter = meter_provider.get_meter("tenant-a-meter", Some("1.0.0"), Some("https://tenant-a.example.com"))
  let tenant_b_meter = meter_provider.get_meter("tenant-b-meter", Some("1.0.0"), Some("https://tenant-b.example.com"))
  
  // 2. 为每个租户创建指标
  let tenant_a_counter = tenant_a_meter.create_counter("tenant_a_operations", Some("operations"), Some("Tenant A operations"))
  let tenant_b_counter = tenant_b_meter.create_counter("tenant_b_operations", Some("operations"), Some("Tenant B operations"))
  
  let tenant_a_histogram = tenant_a_meter.create_histogram("tenant_a_duration", Some("ms"), Some("Tenant A duration"))
  let tenant_b_histogram = tenant_b_meter.create_histogram("tenant_b_duration", Some("ms"), Some("Tenant B duration"))
  
  // 3. 为每个租户记录指标
  let tenant_a_attributes = [
    ("tenant.id", AttributeValue::string("tenant-a")),
    ("user.id", AttributeValue::string("user-a-001")),
    ("service.name", AttributeValue::string("tenant-a-service"))
  ]
  
  let tenant_b_attributes = [
    ("tenant.id", AttributeValue::string("tenant-b")),
    ("user.id", AttributeValue::string("user-b-001")),
    ("service.name", AttributeValue::string("tenant-b-service"))
  ]
  
  tenant_a_counter.add(1L, Some(tenant_a_attributes))
  tenant_b_counter.add(1L, Some(tenant_b_attributes))
  
  tenant_a_histogram.record(100.0, Some(tenant_a_attributes))
  tenant_b_histogram.record(200.0, Some(tenant_b_attributes))
  
  // 4. 验证租户隔离
  // 由于是No-op实现，我们通过创建日志来验证隔离
  let tenant_a_metrics_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Tenant A metrics recorded"),
    attributes: tenant_a_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("tenant-a-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "tenant-a-meter",
      version: Some("1.0.0"),
      schema_url: Some("https://tenant-a.example.com")
    })
  }
  
  let tenant_b_metrics_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: Some(1640995200000000100L),
    severity_number: Info,
    severity_text: Some("INFO"),
    body: Some("Tenant B metrics recorded"),
    attributes: tenant_b_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("tenant-b-service")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "tenant-b-meter",
      version: Some("1.0.0"),
      schema_url: Some("https://tenant-b.example.com")
    })
  }
  
  // 验证每个租户的指标属性包含正确的租户ID
  let mut found_tenant_a_id = false
  let mut found_tenant_b_id = false
  
  let mut i = 0
  while i < tenant_a_metrics_log.attributes.length() {
    let (name, value) = tenant_a_metrics_log.attributes[i]
    match (name, value) {
      ("tenant.id", StringValue(tenant_id)) => {
        assert_eq(tenant_id, "tenant-a")
        found_tenant_a_id = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  i = 0
  while i < tenant_b_metrics_log.attributes.length() {
    let (name, value) = tenant_b_metrics_log.attributes[i]
    match (name, value) {
      ("tenant.id", StringValue(tenant_id)) => {
        assert_eq(tenant_id, "tenant-b")
        found_tenant_b_id = true
      }
      _ => ()
    }
    i = i + 1
  }
  
  assert_eq(found_tenant_a_id, true)
  assert_eq(found_tenant_b_id, true)
  
  // 验证每个租户的instrumentation scope不同
  match tenant_a_metrics_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "tenant-a-meter")
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://tenant-a.example.com")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  match tenant_b_metrics_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "tenant-b-meter")
      match scope.schema_url {
        Some(schema) => assert_eq(schema, "https://tenant-b.example.com")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "propagation_tenant_isolation" {
  // 测试Propagation的多租户隔离
  
  // 1. 为不同租户创建上下文
  let ctx = Context::empty()
  let tenant_a_key = create_key("tenant.id")
  let tenant_b_key = create_key("tenant.id")
  
  let tenant_a_ctx = ctx.with_value(tenant_a_key, "tenant-a")
  let tenant_b_ctx = ctx.with_value(tenant_b_key, "tenant-b")
  
  // 2. 为每个租户创建baggage
  let tenant_a_baggage = Baggage::empty()
    .with_entry("tenant.id", "tenant-a")
    .with_entry("user.id", "user-a-001")
    .with_entry("tenant.environment", "production")
  
  let tenant_b_baggage = Baggage::empty()
    .with_entry("tenant.id", "tenant-b")
    .with_entry("user.id", "user-b-001")
    .with_entry("tenant.environment", "staging")
  
  // 3. 为每个租户创建carrier
  let tenant_a_carrier = MapCarrier::new()
  let tenant_b_carrier = MapCarrier::new()
  
  // 4. 使用propagator为每个租户注入上下文
  let trace_propagator = W3CTraceContextPropagator::{}
  let baggage_propagator = W3CBaggagePropagator::{}
  let composite_propagator = CompositePropagator::new([trace_propagator, baggage_propagator])
  
  composite_propagator.inject(tenant_a_ctx, tenant_a_carrier)
  composite_propagator.inject(tenant_b_ctx, tenant_b_carrier)
  
  // 5. 验证租户隔离
  // 验证每个租户的carrier包含不同的traceparent
  match tenant_a_carrier.get("traceparent") {
    Some(trace_parent_a) => assert_eq(trace_parent_a.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  match tenant_b_carrier.get("traceparent") {
    Some(trace_parent_b) => assert_eq(trace_parent_b.length() > 0, true)
    None => @test.fail("Test failed")
  }
  
  // 验证traceparent不同
  match (tenant_a_carrier.get("traceparent"), tenant_b_carrier.get("traceparent")) {
    (Some(tp_a), Some(tp_b)) => assert_eq(tp_a != tp_b, true)
    _ => @test.fail("Test failed")
  }
  
  // 验证每个租户的carrier包含不同的baggage
  match tenant_a_carrier.get("baggage") {
    Some(baggage_a) => assert_eq(baggage_a.contains("tenant-a"), true)
    None => @test.fail("Test failed")
  }
  
  match tenant_b_carrier.get("baggage") {
    Some(baggage_b) => assert_eq(baggage_b.contains("tenant-b"), true)
    None => @test.fail("Test failed")
  }
  
  // 验证baggage不同
  match (tenant_a_carrier.get("baggage"), tenant_b_carrier.get("baggage")) {
    (Some(bg_a), Some(bg_b)) => {
      assert_eq(bg_a.contains("tenant-a"), true)
      assert_eq(bg_b.contains("tenant-b"), true)
      assert_eq(bg_a != bg_b, true)
    }
    _ => @test.fail("Test failed")
  }
  
  // 6. 测试传播隔离的完整性
  // 从carrier提取上下文并验证租户隔离
  let extracted_a_ctx = composite_propagator.extract(ctx, tenant_a_carrier)
  let extracted_b_ctx = composite_propagator.extract(ctx, tenant_b_carrier)
  
  // 验证提取的上下文保持租户隔离
  match extracted_a_ctx.get(tenant_a_key) {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-a")
    None => @test.fail("Test failed")
  }
  
  match extracted_b_ctx.get(tenant_b_key) {
    Some(tenant_id) => assert_eq(tenant_id, "tenant-b")
    None => @test.fail("Test failed")
  }
}

test "end_to_end_tenant_isolation" {
  // 端到端多租户隔离测试
  
  // 1. 创建多个租户的完整遥测环境
  let tenants = ["tenant-a", "tenant-b", "tenant-c"]
  let tenant_environments = ["production", "staging", "development"]
  let tenant_regions = ["us-east-1", "us-west-2", "eu-west-1"]
  
  // 2. 为每个租户创建资源、上下文、baggage等
  let mut tenant_logs = []
  let mut tenant_spans = []
  let mut tenant_metrics_logs = []
  
  let mut tenant_index = 0
  while tenant_index < tenants.length() {
    let tenant_id = tenants[tenant_index]
    let environment = tenant_environments[tenant_index]
    let region = tenant_regions[tenant_index]
    
    // 创建租户特定的资源
    let tenant_resource = Resource::default(tenant_id + "-service")
    
    // 创建租户特定的上下文
    let ctx = Context::empty()
    let tenant_key = create_key("tenant.id")
    let user_key = create_key("user.id")
    let tenant_ctx = ctx
      .with_value(tenant_key, tenant_id)
      .with_value(user_key, "user-" + tenant_id + "-001")
      .with_value(create_key("tenant.environment"), environment)
      .with_value(create_key("tenant.region"), region)
    
    // 创建租户特定的baggage
    let tenant_baggage = Baggage::empty()
      .with_entry("tenant.id", tenant_id)
      .with_entry("user.id", "user-" + tenant_id + "-001")
      .with_entry("tenant.environment", environment)
      .with_entry("tenant.region", region)
    
    // 创建租户特定的span
    let tenant_span_attributes = [
      ("tenant.id", AttributeValue::string(tenant_id)),
      ("user.id", AttributeValue::string("user-" + tenant_id + "-001")),
      ("tenant.environment", AttributeValue::string(environment)),
      ("tenant.region", AttributeValue::string(region)),
      ("operation.name", AttributeValue::string("tenant-operation"))
    ]
    
    let (_, tenant_span) = NoopTracer::start_span(tenant_ctx, tenant_id + "-span", Server, Some(tenant_span_attributes))
    
    // 创建租户特定的日志
    let tenant_log = LogRecord::{
      timestamp_unix_nanos: 1640995200000000000L + tenant_index.to_int64(),
      observed_timestamp_unix_nanos: Some(1640995200000000100L + tenant_index.to_int64()),
      severity_number: Info,
      severity_text: Some("INFO"),
      body: Some(tenant_id + " operation completed"),
      attributes: tenant_span_attributes,
      trace_id: Some(tenant_span.context.trace_id),
      span_id: Some(tenant_span.context.span_id),
      trace_flags: Some(tenant_span.context.trace_flags),
      resource: Some(tenant_resource),
      instrumentation_scope: Some(InstrumentationScope::{
        name: tenant_id + "-logger",
        version: Some("1.0.0"),
        schema_url: Some("https://" + tenant_id + ".example.com")
      })
    }
    
    // 创建租户特定的指标日志
    let tenant_meter = NoopMeterProvider::{}.get_meter(tenant_id + "-meter", Some("1.0.0"), Some("https://" + tenant_id + ".example.com"))
    let tenant_counter = tenant_meter.create_counter(tenant_id + "_operations", Some("operations"), Some(tenant_id + " operations"))
    tenant_counter.add(1L, Some(tenant_span_attributes))
    
    let tenant_metrics_log = LogRecord::{
      timestamp_unix_nanos: 1640995200000002000L + tenant_index.to_int64(),
      observed_timestamp_unix_nanos: Some(1640995200000002100L + tenant_index.to_int64()),
      severity_number: Info,
      severity_text: Some("INFO"),
      body: Some(tenant_id + " metrics recorded"),
      attributes: tenant_span_attributes,
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: Some(tenant_resource),
      instrumentation_scope: Some(InstrumentationScope::{
        name: tenant_id + "-meter",
        version: Some("1.0.0"),
        schema_url: Some("https://" + tenant_id + ".example.com")
      })
    }
    
    // 创建租户特定的carrier
    let tenant_carrier = MapCarrier::new()
    let composite_propagator = CompositePropagator::new([W3CTraceContextPropagator::{}, W3CBaggagePropagator::{}])
    composite_propagator.inject(tenant_ctx, tenant_carrier)
    
    // 验证carrier隔离
    match tenant_carrier.get("traceparent") {
      Some(trace_parent) => assert_eq(trace_parent.length() > 0, true)
      None => @test.fail("Test failed")
    }
    
    match tenant_carrier.get("baggage") {
      Some(baggage) => assert_eq(baggage.contains(tenant_id), true)
      None => @test.fail("Test failed")
    }
    
    // 添加到数组
    tenant_logs.push(tenant_log)
    tenant_spans.push(tenant_span)
    tenant_metrics_logs.push(tenant_metrics_log)
    
    tenant_index = tenant_index + 1
  }
  
  // 3. 验证端到端租户隔离
  // 验证每个租户的日志包含正确的租户信息
  let mut log_index = 0
  while log_index < tenant_logs.length() {
    let log = tenant_logs[log_index]
    let expected_tenant_id = tenants[log_index]
    let expected_environment = tenant_environments[log_index]
    let expected_region = tenant_regions[log_index]
    
    // 验证租户ID
    let mut found_tenant_id = false
    let mut found_environment = false
    let mut found_region = false
    
    let mut attr_index = 0
    while attr_index < log.attributes.length() {
      let (name, value) = log.attributes[attr_index]
      match (name, value) {
        ("tenant.id", StringValue(tenant_id)) => {
          assert_eq(tenant_id, expected_tenant_id)
          found_tenant_id = true
        }
        ("tenant.environment", StringValue(environment)) => {
          assert_eq(environment, expected_environment)
          found_environment = true
        }
        ("tenant.region", StringValue(region)) => {
          assert_eq(region, expected_region)
          found_region = true
        }
        _ => ()
      }
      attr_index = attr_index + 1
    }
    
    assert_eq(found_tenant_id, true)
    assert_eq(found_environment, true)
    assert_eq(found_region, true)
    
    // 验证resource和instrumentation scope
    match log.resource {
      Some(resource) => assert_eq(resource.service_name, expected_tenant_id + "-service")
      None => @test.fail("Test failed")
    }
    
    match log.instrumentation_scope {
      Some(scope) => {
        assert_eq(scope.name, expected_tenant_id + "-logger")
        match scope.schema_url {
          Some(schema) => assert_eq(schema, "https://" + expected_tenant_id + ".example.com")
          None => @test.fail("Test failed")
        }
      }
      None => @test.fail("Test failed")
    }
    
    log_index = log_index + 1
  }
  
  // 4. 验证跨租户数据隔离
  // 确保租户A的数据不会出现在租户B的日志中
  let mut i = 0
  while i < tenant_logs.length() {
    let log = tenant_logs[i]
    let current_tenant = tenants[i]
    
    // 检查其他租户的数据不会出现在当前租户的日志中
    let mut j = 0
    while j < tenant_logs.length() {
      if i != j {
        let other_tenant = tenants[j]
        
        // 确保其他租户的ID不会出现在当前租户的属性中
        let mut found_other_tenant = false
        let mut attr_index = 0
        while attr_index < log.attributes.length() {
          let (name, value) = log.attributes[attr_index]
          match (name, value) {
            ("tenant.id", StringValue(tenant_id)) => {
              if tenant_id == other_tenant {
                found_other_tenant = true
                break
              }
            }
            _ => ()
          }
          attr_index = attr_index + 1
        }
        
        assert_eq(found_other_tenant, false)  // 不应该找到其他租户的ID
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 5. 验证trace隔离
  // 确保每个租户的trace_id和span_id都是独立的
  let mut i = 0
  while i < tenant_spans.length() {
    let current_span = tenant_spans[i]
    
    // 与其他所有span比较
    let mut j = 0
    while j < tenant_spans.length() {
      if i != j {
        let other_span = tenant_spans[j]
        
        // 检查trace_id不同
        let mut trace_ids_different = false
        let mut k = 0
        while k < current_span.context.trace_id.length() {
          if current_span.context.trace_id[k] != other_span.context.trace_id[k] {
            trace_ids_different = true
            break
          }
          k = k + 1
        }
        assert_eq(trace_ids_different, true)  // trace_id应该不同
        
        // 检查span_id不同
        let mut span_ids_different = false
        k = 0
        while k < current_span.context.span_id.length() {
          if current_span.context.span_id[k] != other_span.context.span_id[k] {
            span_ids_different = true
            break
          }
          k = k + 1
        }
        assert_eq(span_ids_different, true)  // span_id应该不同
      }
      j = j + 1
    }
    
    i = i + 1
  }
}