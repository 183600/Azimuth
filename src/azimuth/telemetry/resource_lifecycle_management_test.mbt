// 资源管理测试 - 测试资源生命周期管理

test "resource_lifecycle_management" {
  // 测试Resource的完整生命周期管理
  
  // 1. Resource创建阶段
  let initial_resource = Resource::default("lifecycle-test-service")
  
  assert_eq(initial_resource.service_name, "lifecycle-test-service")
  assert_eq(initial_resource.service_version, None)
  assert_eq(initial_resource.telemetry_sdk_name, "azimuth")
  assert_eq(initial_resource.telemetry_sdk_version, "0.1.0")
  assert_eq(initial_resource.attributes.length(), 0)
  
  // 2. Resource配置阶段
  let resource_with_attrs = Resource::default("configured-service")
  let config_attributes = [
    ("service.namespace", AttributeValue::string("production")),
    ("service.version", AttributeValue::string("2.1.0")),
    ("deployment.environment", AttributeValue::string("prod")),
    ("host.name", AttributeValue::string("prod-server-01")),
    ("host.ip", AttributeValue::string("10.0.1.100")),
    ("process.pid", AttributeValue::int(12345L)),
    ("process.start.time", AttributeValue::string("2023-01-01T00:00:00Z")),
    ("cloud.provider", AttributeValue::string("aws")),
    ("cloud.region", AttributeValue::string("us-west-2")),
    ("cloud.availability_zone", AttributeValue::string("us-west-2a"))
  ]
  
  // 3. Resource使用阶段
  let ctx = Context::empty()
  let (_, usage_span) = NoopTracer::start_span(
    ctx,
    "resource_usage_test",
    Server,
    Some([
      ("service.name", AttributeValue::string("configured-service")),
      ("service.namespace", AttributeValue::string("production")),
      ("deployment.environment", AttributeValue::string("prod"))
    ])
  )
  
  let meter = NoopMeter::{}
  let resource_counter = meter.create_counter("resource_operations_total", Some("operations"), Some("Resource operations"))
  
  resource_counter.add(1L, Some([
    ("service.name", AttributeValue::string("configured-service")),
    ("operation.type", AttributeValue::string("resource_lifecycle_test")),
    ("resource.phase", AttributeValue::string("usage"))
  ]))
  
  let logger = NoopLogger::{}
  let resource_log_record = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("RESOURCE_LIFECYCLE"),
    body: Some("Resource lifecycle test - usage phase"),
    attributes: [
      ("service.name", AttributeValue::string("configured-service")),
      ("service.namespace", AttributeValue::string("production")),
      ("deployment.environment", AttributeValue::string("prod")),
      ("lifecycle.phase", AttributeValue::string("usage"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(resource_with_attrs),
    instrumentation_scope: None
  }
  
  logger.emit(resource_log_record)
  
  // 4. Resource更新阶段
  let updated_resource = Resource::default("updated-service")
  let update_attributes = [
    ("service.namespace", AttributeValue::string("staging")),
    ("service.version", AttributeValue::string("2.2.0")),
    ("deployment.environment", AttributeValue::string("staging")),
    ("host.name", AttributeValue::string("staging-server-01")),
    ("maintenance.mode", AttributeValue::bool(false))
  ]
  
  let (_, update_span) = NoopTracer::start_span(
    ctx,
    "resource_update_test",
    Server,
    Some([
      ("service.name", AttributeValue::string("updated-service")),
      ("service.namespace", AttributeValue::string("staging")),
      ("deployment.environment", AttributeValue::string("staging"))
    ])
  )
  
  // 5. Resource清理阶段
  let cleanup_resource = Resource::default("cleanup-service")
  let cleanup_attributes = [
    ("service.name", AttributeValue::string("cleanup-service")),
    ("lifecycle.phase", AttributeValue::string("cleanup")),
    ("cleanup.reason", AttributeValue::string("normal_shutdown")),
    ("cleanup.timestamp", AttributeValue::string("2023-12-31T23:59:59Z"))
  ]
  
  let cleanup_log_record = LogRecord::{
    timestamp_unix_nanos: 1672531199000000000L,  // 2023-12-31 23:59:59 UTC
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("RESOURCE_CLEANUP"),
    body: Some("Resource lifecycle test - cleanup phase"),
    attributes: cleanup_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(cleanup_resource),
    instrumentation_scope: None
  }
  
  logger.emit(cleanup_log_record)
  
  // 6. 验证生命周期各阶段
  assert_eq(initial_resource.service_name, "lifecycle-test-service")
  assert_eq(resource_with_attrs.service_name, "configured-service")
  assert_eq(updated_resource.service_name, "updated-service")
  assert_eq(cleanup_resource.service_name, "cleanup-service")
  
  // 验证使用阶段的Resource关联
  match resource_log_record.resource {
    Some(resource) => assert_eq(resource.service_name, "configured-service")
    None => @test.fail("Test failed")
  }
  
  // 验证清理阶段的Resource关联
  match cleanup_log_record.resource {
    Some(resource) => assert_eq(resource.service_name, "cleanup-service")
    None => @test.fail("Test failed")
  }
  
  // 验证Span名称反映生命周期阶段
  assert_eq(usage_span.name, "resource_usage_test")
  assert_eq(update_span.name, "resource_update_test")
}

test "instrumentation_scope_lifecycle" {
  // 测试InstrumentationScope的生命周期管理
  
  // 1. 创建阶段
  let initial_scope = InstrumentationScope::{
    name: "initial-scope",
    version: Some("1.0.0"),
    schema_url: Some("https://example.com/schemas/initial")
  }
  
  assert_eq(initial_scope.name, "initial-scope")
  match initial_scope.version {
    Some(version) => assert_eq(version, "1.0.0")
    None => @test.fail("Test failed")
  }
  match initial_scope.schema_url {
    Some(schema_url) => assert_eq(schema_url, "https://example.com/schemas/initial")
    None => @test.fail("Test failed")
  }
  
  // 2. 配置阶段
  let config_scope = InstrumentationScope::{
    name: "config-scope",
    version: Some("2.1.0"),
    schema_url: Some("https://example.com/schemas/config")
  }
  
  // 3. 使用阶段 - 在Trace中使用
  let ctx = Context::empty()
  let (_, trace_usage_span) = NoopTracer::start_span(
    ctx,
    "scope_trace_usage",
    Server,
    Some([
      ("instrumentation.scope.name", AttributeValue::string("config-scope")),
      ("instrumentation.scope.version", AttributeValue::string("2.1.0")),
      ("operation.phase", AttributeValue::string("usage"))
    ])
  )
  
  // 4. 使用阶段 - 在Metrics中使用
  let meter = NoopMeter::{}
  let scope_counter = meter.create_counter("scope_operations_total", Some("operations"), Some("Scope operations"))
  
  scope_counter.add(1L, Some([
    ("instrumentation.scope.name", AttributeValue::string("config-scope")),
    ("instrumentation.scope.version", AttributeValue::string("2.1.0")),
    ("operation.type", AttributeValue::string("metrics_usage"))
  ]))
  
  // 5. 使用阶段 - 在Logs中使用
  let logger = NoopLogger::{}
  let scope_usage_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("SCOPE_USAGE"),
    body: Some("Instrumentation scope usage test"),
    attributes: [
      ("instrumentation.scope.name", AttributeValue::string("config-scope")),
      ("instrumentation.scope.version", AttributeValue::string("2.1.0")),
      ("operation.phase", AttributeValue::string("usage"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: Some(config_scope)
  }
  
  logger.emit(scope_usage_log)
  
  // 6. 更新阶段
  let updated_scope = InstrumentationScope::{
    name: "updated-scope",
    version: Some("3.0.0"),
    schema_url: Some("https://example.com/schemas/updated")
  }
  
  let (_, update_span) = NoopTracer::start_span(
    ctx,
    "scope_update_test",
    Server,
    Some([
      ("instrumentation.scope.name", AttributeValue::string("updated-scope")),
      ("instrumentation.scope.version", AttributeValue::string("3.0.0")),
      ("operation.phase", AttributeValue::string("update"))
    ])
  )
  
  // 7. 清理阶段
  let cleanup_scope = InstrumentationScope::{
    name: "cleanup-scope",
    version: Some("3.1.0"),
    schema_url: Some("https://example.com/schemas/cleanup")
  }
  
  let cleanup_log = LogRecord::{
    timestamp_unix_nanos: 1672531199000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("SCOPE_CLEANUP"),
    body: Some("Instrumentation scope cleanup test"),
    attributes: [
      ("instrumentation.scope.name", AttributeValue::string("cleanup-scope")),
      ("instrumentation.scope.version", AttributeValue::string("3.1.0")),
      ("operation.phase", AttributeValue::string("cleanup")),
      ("cleanup.reason", AttributeValue::string("version_upgrade"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: None,
    instrumentation_scope: Some(cleanup_scope)
  }
  
  logger.emit(cleanup_log)
  
  // 8. 验证生命周期各阶段
  assert_eq(initial_scope.name, "initial-scope")
  assert_eq(config_scope.name, "config-scope")
  assert_eq(updated_scope.name, "updated-scope")
  assert_eq(cleanup_scope.name, "cleanup-scope")
  
  // 验证使用阶段的InstrumentationScope关联
  match scope_usage_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "config-scope")
      match scope.version {
        Some(version) => assert_eq(version, "2.1.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
  
  // 验证清理阶段的InstrumentationScope关联
  match cleanup_log.instrumentation_scope {
    Some(scope) => {
      assert_eq(scope.name, "cleanup-scope")
      match scope.version {
        Some(version) => assert_eq(version, "3.1.0")
        None => @test.fail("Test failed")
      }
    }
    None => @test.fail("Test failed")
  }
}

test "context_resource_management" {
  // 测试Context与Resource的综合管理
  
  // 1. 创建带有Resource信息的Context
  let service_resource = Resource::default("context-managed-service")
  let resource_attributes = [
    ("service.namespace", AttributeValue::string("finance")),
    ("service.version", AttributeValue::string("1.5.2")),
    ("deployment.environment", AttributeValue::string("production")),
    ("host.name", AttributeValue::string("finance-prod-01")),
    ("cloud.region", AttributeValue::string("eu-west-1"))
  ]
  
  let ctx_with_resource = Context::empty()
    .with_value(create_key("resource.service.name"), "context-managed-service")
    .with_value(create_key("resource.service.namespace"), "finance")
    .with_value(create_key("resource.deployment.environment"), "production")
    .with_value(create_key("resource.cloud.region"), "eu-west-1")
    .with_value(create_key("resource.lifecycle.phase"), "active")
  
  // 2. 在Trace中使用Context和Resource信息
  let trace_attributes = [
    ("service.name", AttributeValue::string("context-managed-service")),
    ("service.namespace", AttributeValue::string("finance")),
    ("deployment.environment", AttributeValue::string("production")),
    ("cloud.region", AttributeValue::string("eu-west-1")),
    ("resource.lifecycle.phase", AttributeValue::string("active"))
  ]
  
  let (_, resource_trace_span) = NoopTracer::start_span(
    ctx_with_resource,
    "context_resource_trace",
    Server,
    Some(trace_attributes)
  )
  
  // 3. 在Metrics中使用Context和Resource信息
  let meter = NoopMeter::{}
  let resource_metrics_counter = meter.create_counter("context_resource_operations", Some("operations"), Some("Context resource operations"))
  
  resource_metrics_counter.add(1L, Some(trace_attributes))
  
  // 4. 在Logs中使用Context和Resource信息
  let logger = NoopLogger::{}
  let context_resource_log = LogRecord::{
    timestamp_unix_nanos: 1640995200000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("CONTEXT_RESOURCE"),
    body: Some("Context and resource management test"),
    attributes: trace_attributes,
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(service_resource),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "context-resource-scope",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  logger.emit(context_resource_log)
  
  // 5. Resource状态变更
  let updated_ctx = ctx_with_resource
    .with_value(create_key("resource.lifecycle.phase"), "maintenance")
    .with_value(create_key("resource.maintenance.reason"), "scheduled_update")
  
  let (_, maintenance_span) = NoopTracer::start_span(
    updated_ctx,
    "resource_maintenance",
    Server,
    Some([
      ("service.name", AttributeValue::string("context-managed-service")),
      ("resource.lifecycle.phase", AttributeValue::string("maintenance")),
      ("resource.maintenance.reason", AttributeValue::string("scheduled_update"))
    ])
  )
  
  // 6. Resource清理
  let cleanup_ctx = updated_ctx
    .with_value(create_key("resource.lifecycle.phase"), "cleanup")
    .with_value(create_key("resource.cleanup.reason"), "decommission")
  
  let cleanup_log = LogRecord::{
    timestamp_unix_nanos: 1672531199000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Warn,
    severity_text: Some("RESOURCE_CLEANUP"),
    body: Some("Resource cleanup and decommission"),
    attributes: [
      ("service.name", AttributeValue::string("context-managed-service")),
      ("resource.lifecycle.phase", AttributeValue::string("cleanup")),
      ("resource.cleanup.reason", AttributeValue::string("decommission"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(service_resource),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "cleanup-scope",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  logger.emit(cleanup_log)
  
  // 7. 验证Context和Resource管理
  // 验证初始Context状态
  match ctx_with_resource.get(create_key("resource.service.name")) {
    Some(service_name) => assert_eq(service_name, "context-managed-service")
    None => @test.fail("Test failed")
  }
  
  match ctx_with_resource.get(create_key("resource.lifecycle.phase")) {
    Some(phase) => assert_eq(phase, "active")
    None => @test.fail("Test failed")
  }
  
  // 验证更新后的Context状态
  match updated_ctx.get(create_key("resource.lifecycle.phase")) {
    Some(phase) => assert_eq(phase, "maintenance")
    None => @test.fail("Test failed")
  }
  
  match updated_ctx.get(create_key("resource.maintenance.reason")) {
    Some(reason) => assert_eq(reason, "scheduled_update")
    None => @test.fail("Test failed")
  }
  
  // 验证清理后的Context状态
  match cleanup_ctx.get(create_key("resource.lifecycle.phase")) {
    Some(phase) => assert_eq(phase, "cleanup")
    None => @test.fail("Test failed")
  }
  
  match cleanup_ctx.get(create_key("resource.cleanup.reason")) {
    Some(reason) => assert_eq(reason, "decommission")
    None => @test.fail("Test failed")
  }
  
  // 验证Span中的Resource信息
  assert_eq(resource_trace_span.attributes.length(), 5)
  match resource_trace_span.attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, "context-managed-service")
    _ => @test.fail("Test failed")
  }
  
  // 验证LogRecord中的Resource和InstrumentationScope
  match context_resource_log.resource {
    Some(resource) => assert_eq(resource.service_name, "context-managed-service")
    None => @test.fail("Test failed")
  }
  
  match context_resource_log.instrumentation_scope {
    Some(scope) => assert_eq(scope.name, "context-resource-scope")
    None => @test.fail("Test failed")
  }
}

test "memory_efficient_resource_usage" {
  // 测试内存高效的资源使用
  
  // 1. 轻量级Resource创建
  let lightweight_resource = Resource::default("lightweight-service")
  
  // 2. 最小化属性使用
  let minimal_attributes = [
    ("service.name", AttributeValue::string("lightweight-service")),
    ("operation.type", AttributeValue::string("minimal"))
  ]
  
  let ctx = Context::empty()
  let (_, minimal_span) = NoopTracer::start_span(
    ctx,
    "minimal_operation",
    Internal,
    Some(minimal_attributes)
  )
  
  // 3. 批量操作以减少资源分配
  let meter = NoopMeter::{}
  let batch_counter = meter.create_counter("batch_operations", Some("operations"), Some("Batch operations"))
  
  // 批量记录Metrics
  let batch_size = 100
  let mut i = 0
  while i < batch_size {
    batch_counter.add(1L, Some([
      ("service.name", AttributeValue::string("lightweight-service")),
      ("batch.id", AttributeValue::int((i / 10).to_int64())),
      ("operation.index", AttributeValue::int(i.to_int64()))
    ]))
    i = i + 1
  }
  
  // 4. 复用LogRecord Builder
  let logger = NoopLogger::{}
  let log_builder = LogRecord::builder()
  
  // 批量创建日志
  let mut j = 0
  while j < 10 {  // 较少的日志数量以节省内存
    let batch_log = log_builder
      .timestamp(1640995200000000000L + j.to_int64() * 1000000L)
      .severity(Info)
      .body("Batch operation " + j.to_string())
      .with_attribute("service.name", AttributeValue::string("lightweight-service"))
      .with_attribute("batch.id", AttributeValue::int(j.to_int64()))
      .with_attribute("operations.count", AttributeValue::int(10L))
      .build()
    
    logger.emit(batch_log)
    j = j + 1
  }
  
  // 5. 资源清理验证
  let cleanup_log = LogRecord::builder()
    .timestamp(1672531199000000000L)
    .severity(Info)
    .body("Memory efficient resource cleanup")
    .with_attribute("service.name", AttributeValue::string("lightweight-service"))
    .with_attribute("cleanup.reason", AttributeValue::string("normal_completion"))
    .with_attribute("total.operations", AttributeValue::int(batch_size.to_int64()))
    .with_attribute("total.logs", AttributeValue::int(10L))
    .build()
  
  logger.emit(cleanup_log)
  
  // 6. 验证内存高效使用
  assert_eq(lightweight_resource.service_name, "lightweight-service")
  assert_eq(lightweight_resource.attributes.length(), 0)  // 最小化属性
  
  assert_eq(minimal_span.attributes.length(), 2)  // 最小化属性
  
  // 验证批量操作完成
  assert_eq(batch_size, 100)
  
  // 验证日志数量控制
  assert_eq(j, 10)  // 只创建了10个日志而不是100个
}

test "resource_pool_management" {
  // 测试资源池管理
  
  // 1. 创建资源池
  let resource_pool_size = 5
  let mut resource_pool = []
  let mut i = 0
  while i < resource_pool_size {
    let pool_resource = Resource::default("pooled-service-" + i.to_string())
    resource_pool.push(pool_resource)
    i = i + 1
  }
  
  // 2. 从池中获取资源
  let pooled_resource_0 = resource_pool[0]
  let pooled_resource_1 = resource_pool[1]
  
  assert_eq(pooled_resource_0.service_name, "pooled-service-0")
  assert_eq(pooled_resource_1.service_name, "pooled-service-1")
  
  // 3. 使用池化资源
  let ctx = Context::empty()
  let mut pool_spans = []
  
  let mut j = 0
  while j < resource_pool_size {
    let (_, pool_span) = NoopTracer::start_span(
      ctx,
      "pooled_operation_" + j.to_string(),
      Server,
      Some([
        ("service.name", AttributeValue::string("pooled-service-" + j.to_string())),
        ("pool.index", AttributeValue::int(j.to_int64())),
        ("resource.pool.id", AttributeValue::string("main_pool"))
      ])
    )
    pool_spans.push(pool_span)
    j = j + 1
  }
  
  // 4. 池化Metrics
  let meter = NoopMeter::{}
  let pool_counter = meter.create_counter("pool_operations", Some("operations"), Some("Pool operations"))
  
  let mut k = 0
  while k < resource_pool_size {
    pool_counter.add(1L, Some([
      ("service.name", AttributeValue::string("pooled-service-" + k.to_string())),
      ("pool.index", AttributeValue::int(k.to_int64())),
      ("operation.type", AttributeValue::string("pooled_access"))
    ]))
    k = k + 1
  }
  
  // 5. 池化日志
  let logger = NoopLogger::{}
  
  let mut l = 0
  while l < resource_pool_size {
    let pool_log = LogRecord::{
      timestamp_unix_nanos: 1640995200000000000L + l.to_int64() * 1000000L,
      observed_timestamp_unix_nanos: None,
      severity_number: Info,
      severity_text: Some("POOLED_OPERATION"),
      body: Some("Pooled resource operation " + l.to_string()),
      attributes: [
        ("service.name", AttributeValue::string("pooled-service-" + l.to_string())),
        ("pool.index", AttributeValue::int(l.to_int64())),
        ("resource.pool.id", AttributeValue::string("main_pool")),
        ("resource.pool.size", AttributeValue::int(resource_pool_size.to_int64()))
      ],
      trace_id: None,
      span_id: None,
      trace_flags: None,
      resource: Some(resource_pool[l]),
      instrumentation_scope: Some(InstrumentationScope::{
        name: "pool-scope",
        version: Some("1.0.0"),
        schema_url: None
      })
    }
    
    logger.emit(pool_log)
    l = l + 1
  }
  
  // 6. 资源池清理
  let pool_cleanup_log = LogRecord::{
    timestamp_unix_nanos: 1672531199000000000L,
    observed_timestamp_unix_nanos: None,
    severity_number: Info,
    severity_text: Some("POOL_CLEANUP"),
    body: Some("Resource pool cleanup"),
    attributes: [
      ("resource.pool.id", AttributeValue::string("main_pool")),
      ("resource.pool.size", AttributeValue::int(resource_pool_size.to_int64())),
      ("cleanup.reason", AttributeValue::string("pool_drain"))
    ],
    trace_id: None,
    span_id: None,
    trace_flags: None,
    resource: Some(Resource::default("pool-manager")),
    instrumentation_scope: Some(InstrumentationScope::{
      name: "pool-cleanup-scope",
      version: Some("1.0.0"),
      schema_url: None
    })
  }
  
  logger.emit(pool_cleanup_log)
  
  // 7. 验证资源池管理
  assert_eq(resource_pool.length(), resource_pool_size)
  assert_eq(pool_spans.length(), resource_pool_size)
  
  // 验证池化资源的正确性
  assert_eq(resource_pool[0].service_name, "pooled-service-0")
  assert_eq(resource_pool[resource_pool_size - 1].service_name, "pooled-service-" + (resource_pool_size - 1).to_string())
  
  // 验证池化操作的正确性
  assert_eq(pool_spans[0].name, "pooled_operation_0")
  assert_eq(pool_spans[resource_pool_size - 1].name, "pooled_operation_" + (resource_pool_size - 1).to_string())
  
  // 验证池化Span的属性
  match pool_spans[0].attributes[0].1 {
    StringValue(service_name) => assert_eq(service_name, "pooled-service-0")
    _ => @test.fail("Test failed")
  }
  
  match pool_spans[0].attributes[1].1 {
    IntValue(pool_index) => assert_eq(pool_index, 0L)
    _ => @test.fail("Test failed")
  }
  
  // 验证池化日志的Resource关联
  // 注意：这里我们只能验证最后一个日志，因为我们没有保存所有日志
  assert_eq(l, resource_pool_size)
}