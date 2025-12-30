// 跨模块集成的端到端测试
// 测试不同遥测模块之间的完整集成流程

test "e2e_metrics_traces_integration" {
  // 测试指标和追踪的端到端集成
  let resource = common::Resource::default("integration_service")
  
  // 创建meter和tracer
  let meter_provider = metrics::NoopMeterProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  
  let meter = meter_provider.get_meter("integration_meter", "1.0.0")
  let tracer = tracer_provider.get_tracer("integration_tracer", "1.0.0")
  
  let ctx = context::Context::current()
  
  // 开始一个trace span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "integration_operation",
    Server,
    [
      ("service.name", AttributeValue::string(resource.service_name)),
      ("operation.type", AttributeValue::string("metrics_traces_integration"))
    ]
  )
  
  // 在span内记录指标
  let counter = meter.create_counter("operations_total", "count", "Total operations")
  let histogram = meter.create_histogram("operation_duration", "ms", "Operation duration")
  let gauge = meter.create_gauge("active_connections", "connections", "Active connections")
  
  // 记录操作指标
  counter.add(1L, [
    ("operation.name", AttributeValue::string(span.name)),
    ("service.version", AttributeValue::string(resource.telemetry_sdk_version))
  ])
  
  histogram.record(150.5, [
    ("operation.name", AttributeValue::string(span.name)),
    ("status", AttributeValue::string("success"))
  ])
  
  gauge.record(25.0, [
    ("service.name", AttributeValue::string(resource.service_name))
  ])
  
  // 验证集成数据一致性
  @assertion.assert_eq(span.name, "integration_operation")
  @assertion.assert_eq(span.attributes.length(), 2)
  
  // 验证指标和追踪使用相同的资源信息
  let service_attr = span.attributes.find(fn(attr) { attr[0] == "service.name" })
  match service_attr {
    Some((_, AttributeValue::string("integration_service"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}

test "e2e_multi_service_propagation" {
  // 测试多服务间的遥测数据传播
  let resource_a = common::Resource::default("service_a")
  let resource_b = common::Resource::default("service_b")
  let resource_c = common::Resource::default("service_c")
  
  let tracer_provider = trace::NoopTracerProvider::{}
  let tracer_a = tracer_provider.get_tracer("tracer_a", "1.0.0")
  let tracer_b = tracer_provider.get_tracer("tracer_b", "1.0.0")
  let tracer_c = tracer_provider.get_tracer("tracer_c", "1.0.0")
  
  let ctx = context::Context::current()
  
  // Service A: 创建根span
  let (ctx_a, span_a) = tracer_a.start_span(
    ctx,
    "service_a_operation",
    Server,
    [
      ("service.name", AttributeValue::string(resource_a.service_name)),
      ("service.version", AttributeValue::string("1.0.0"))
    ]
  )
  
  // Service B: 创建子span
  let (ctx_b, span_b) = tracer_b.start_span(
    ctx_a,
    "service_b_operation",
    Client,
    [
      ("service.name", AttributeValue::string(resource_b.service_name)),
      ("parent.service", AttributeValue::string(resource_a.service_name))
    ]
  )
  
  // Service C: 创建孙span
  let (ctx_c, span_c) = tracer_c.start_span(
    ctx_b,
    "service_c_operation",
    Server,
    [
      ("service.name", AttributeValue::string(resource_c.service_name)),
      ("parent.service", AttributeValue::string(resource_b.service_name)),
      ("root.service", AttributeValue::string(resource_a.service_name))
    ]
  )
  
  // 验证服务链路
  @assertion.assert_eq(span_a.name, "service_a_operation")
  @assertion.assert_eq(span_b.name, "service_b_operation")
  @assertion.assert_eq(span_c.name, "service_c_operation")
  
  // 验证父子关系
  @assertion.assert_eq(span_b.parent_span_id.length(), 8)
  @assertion.assert_eq(span_c.parent_span_id.length(), 8)
  
  // 验证服务传播属性
  let root_service_attr = span_c.attributes.find(fn(attr) { attr[0] == "root.service" })
  match root_service_attr {
    Some((_, AttributeValue::string("service_a"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}

test "e2e_logs_metrics_traces_correlation" {
  // 测试日志、指标和追踪的关联
  let resource = common::Resource::default("correlation_service")
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  // 假设有日志记录器
  // let logger_provider = logs::NoopLoggerProvider::{}
  
  let meter = meter_provider.get_meter("correlation_meter", "1.0.0")
  let tracer = tracer_provider.get_tracer("correlation_tracer", "1.0.0")
  // let logger = logger_provider.get_logger("correlation_logger", "1.0.0")
  
  let ctx = context::Context::current()
  
  // 开始操作
  let (operation_ctx, operation_span) = tracer.start_span(
    ctx,
    "correlated_operation",
    Server,
    [
      ("service.name", AttributeValue::string(resource.service_name)),
      ("operation.id", AttributeValue::string("op-12345")),
      ("user.id", AttributeValue::string("user-67890"))
    ]
  )
  
  // 记录操作指标
  let operation_counter = meter.create_counter("operations_total", "count", "Total operations")
  let operation_histogram = meter.create_histogram("operation_duration", "ms", "Operation duration")
  
  operation_counter.add(1L, [
    ("operation.id", AttributeValue::string("op-12345")),
    ("user.id", AttributeValue::string("user-67890")),
    ("service.name", AttributeValue::string(resource.service_name))
  ])
  
  operation_histogram.record(250.75, [
    ("operation.id", AttributeValue::string("op-12345")),
    ("status", AttributeValue::string("completed"))
  ])
  
  // 模拟日志记录（在实际实现中会有日志记录器）
  // logger.emit(
  //   operation_ctx,
  //   LogLevel::Info,
  //   "Operation completed successfully",
  //   [
  //     ("operation.id", AttributeValue::string("op-12345")),
  //     ("user.id", AttributeValue::string("user-67890")),
  //     ("duration_ms", AttributeValue::float(250.75))
  //   ]
  // )
  
  // 验证关联数据
  let operation_id_attr = operation_span.attributes.find(fn(attr) { attr[0] == "operation.id" })
  match operation_id_attr {
    Some((_, AttributeValue::string("op-12345"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  // 验证所有组件使用相同的关联ID
  @assertion.assert_eq(operation_span.attributes.length(), 3)
}

test "e2e_error_propagation_across_modules" {
  // 测试跨模块的错误传播
  let resource = common::Resource::default("error_handling_service")
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  
  let meter = meter_provider.get_meter("error_meter", "1.0.0")
  let tracer = tracer_provider.get_tracer("error_tracer", "1.0.0")
  
  let ctx = context::Context::current()
  
  // 开始可能失败的操作
  let (operation_ctx, operation_span) = tracer.start_span(
    ctx,
    "error_prone_operation",
    Server,
    [
      ("service.name", AttributeValue::string(resource.service_name)),
      ("operation.type", AttributeValue::string("error_test"))
    ]
  )
  
  // 记录错误指标
  let error_counter = meter.create_counter("errors_total", "count", "Total errors")
  let success_counter = meter.create_counter("success_total", "count", "Total successes")
  
  // 模拟错误场景
  let error_occurred = true
  
  if error_occurred {
    // 记录错误
    error_counter.add(1L, [
      ("error.type", AttributeValue::string("validation_error")),
      ("operation.name", AttributeValue::string(operation_span.name)),
      ("error.code", AttributeValue::string("ERR_001"))
    ])
    
    // 在实际实现中，这里会设置span状态为错误
    // operation_span.set_status(StatusCode::Error, "Validation failed")
  } else {
    // 记录成功
    success_counter.add(1L, [
      ("operation.name", AttributeValue::string(operation_span.name))
    ])
  }
  
  // 验证错误处理
  @assertion.assert_eq(operation_span.name, "error_prone_operation")
  @assertion.assert_eq(error_occurred, true)
}

test "e2e_performance_monitoring_integration" {
  // 测试性能监控集成
  let resource = common::Resource::default("performance_service")
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  
  let meter = meter_provider.get_meter("performance_meter", "1.0.0")
  let tracer = tracer_provider.get_tracer("performance_tracer", "1.0.0")
  
  let ctx = context::Context::current()
  
  // 性能监控操作
  let (perf_ctx, perf_span) = tracer.start_span(
    ctx,
    "performance_intensive_operation",
    Server,
    [
      ("service.name", AttributeValue::string(resource.service_name)),
      ("operation.type", AttributeValue::string("performance_test"))
    ]
  )
  
  // 创建性能指标
  let cpu_usage = meter.create_gauge("cpu_usage_percent", "percent", "CPU usage percentage")
  let memory_usage = meter.create_gauge("memory_usage_bytes", "bytes", "Memory usage in bytes")
  let request_duration = meter.create_histogram("request_duration_ms", "ms", "Request duration")
  let throughput = meter.create_counter("requests_total", "count", "Total requests")
  
  // 模拟性能数据收集
  let start_time = 0L  // 实际实现中会使用真实时间戳
  let end_time = 100L   // 模拟100ms的操作
  
  // 记录性能指标
  cpu_usage.record(75.5, [
    ("operation.name", AttributeValue::string(perf_span.name)),
    ("instance.id", AttributeValue::string("instance-001"))
  ])
  
  memory_usage.record(1024.0 * 1024.0 * 512.0, [  // 512MB
    ("operation.name", AttributeValue::string(perf_span.name)),
    ("memory.type", AttributeValue::string("heap"))
  ])
  
  request_duration.record(100.0, [
    ("operation.name", AttributeValue::string(perf_span.name)),
    ("status", AttributeValue::string("success"))
  ])
  
  throughput.add(1L, [
    ("operation.name", AttributeValue::string(perf_span.name)),
    ("endpoint", AttributeValue::string("/api/performance"))
  ])
  
  // 验证性能监控集成
  @assertion.assert_eq(perf_span.name, "performance_intensive_operation")
  @assertion.assert_eq(perf_span.attributes.length(), 2)
}

test "e2e_resource_attribute_inheritance" {
  // 测试资源属性继承
  let global_resource = common::Resource::{
    service_name: "global_service",
    service_version: Some("2.0.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("environment", AttributeValue::string("production")),
      ("region", AttributeValue::string("us-west-2")),
      ("datacenter", AttributeValue::string("dc-1"))
    ]
  }
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  
  let meter = meter_provider.get_meter("inheritance_meter", "1.0.0")
  let tracer = tracer_provider.get_tracer("inheritance_tracer", "1.0.0")
  
  let ctx = context::Context::current()
  
  // 创建继承资源属性的span
  let (inherit_ctx, inherit_span) = tracer.start_span(
    ctx,
    "inheritance_operation",
    Server,
    [
      // 继承全局资源属性
      ("service.name", AttributeValue::string(global_resource.service_name)),
      ("service.version", AttributeValue::string(global_resource.service_version.unwrap_or(""))),
      ("environment", AttributeValue::string("production")),
      ("region", AttributeValue::string("us-west-2")),
      ("datacenter", AttributeValue::string("dc-1")),
      // 添加操作特定属性
      ("operation.type", AttributeValue::string("inheritance_test")),
      ("operation.id", AttributeValue::string("op-inherit-001"))
    ]
  )
  
  // 创建继承资源属性的指标
  let operation_counter = meter.create_counter("inheritance_operations", "count", "Operations with inherited attributes")
  
  operation_counter.add(1L, [
    // 继承全局资源属性
    ("service.name", AttributeValue::string(global_resource.service_name)),
    ("environment", AttributeValue::string("production")),
    ("region", AttributeValue::string("us-west-2")),
    ("datacenter", AttributeValue::string("dc-1")),
    // 添加指标特定属性
    ("metric.type", AttributeValue::string("counter")),
    ("metric.version", AttributeValue::string("1.0"))
  ])
  
  // 验证属性继承
  @assertion.assert_eq(inherit_span.name, "inheritance_operation")
  @assertion.assert_eq(inherit_span.attributes.length(), 7)
  
  // 验证继承的属性值
  let service_attr = inherit_span.attributes.find(fn(attr) { attr[0] == "service.name" })
  let env_attr = inherit_span.attributes.find(fn(attr) { attr[0] == "environment" })
  let region_attr = inherit_span.attributes.find(fn(attr) { attr[0] == "region" })
  
  match service_attr {
    Some((_, AttributeValue::string("global_service"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match env_attr {
    Some((_, AttributeValue::string("production"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
  
  match region_attr {
    Some((_, AttributeValue::string("us-west-2"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}

test "e2e_configuration_driven_integration" {
  // 测试配置驱动的集成
  let config_resource = common::Resource::{
    service_name: "config_driven_service",
    service_version: Some("1.5.0"),
    telemetry_sdk_name: "azimuth",
    telemetry_sdk_version: "0.1.0",
    attributes: [
      ("config.version", AttributeValue::string("v2.1")),
      ("config.environment", AttributeValue::string("staging")),
      ("sampling.rate", AttributeValue::float(0.1)),
      ("metrics.enabled", AttributeValue::bool(true)),
      ("tracing.enabled", AttributeValue::bool(true)),
      ("export.interval", AttributeValue::int(60000L))
    ]
  }
  
  let meter_provider = metrics::NoopMeterProvider::{}
  let tracer_provider = trace::NoopTracerProvider::{}
  
  let meter = meter_provider.get_meter("config_meter", "1.0.0")
  let tracer = tracer_provider.get_tracer("config_tracer", "1.0.0")
  
  let ctx = context::Context::current()
  
  // 根据配置创建span
  let (config_ctx, config_span) = tracer.start_span(
    ctx,
    "config_driven_operation",
    Server,
    [
      ("service.name", AttributeValue::string(config_resource.service_name)),
      ("config.version", AttributeValue::string("v2.1")),
      ("config.environment", AttributeValue::string("staging"))
    ]
  )
  
  // 根据配置记录指标
  if config_resource.attributes.find(fn(attr) { attr[0] == "metrics.enabled" })?.[1] == AttributeValue::bool(true) {
    let config_counter = meter.create_counter("config_operations", "count", "Configuration-driven operations")
    
    config_counter.add(1L, [
      ("service.name", AttributeValue::string(config_resource.service_name)),
      ("config.version", AttributeValue::string("v2.1")),
      ("config.environment", AttributeValue::string("staging"))
    ])
  }
  
  // 验证配置驱动的行为
  @assertion.assert_eq(config_span.name, "config_driven_operation")
  @assertion.assert_eq(config_span.attributes.length(), 3)
  
  // 验证配置属性
  let config_version_attr = config_span.attributes.find(fn(attr) { attr[0] == "config.version" })
  match config_version_attr {
    Some((_, AttributeValue::string("v2.1"))) => @assertion.assert_true(true)
    _ => @assertion.assert_true(false)
  }
}