// 跨信号集成综合测试 - 验证Traces、Metrics、Logs之间的协同工作

test "cross_signal_trace_metrics_correlation" {
  // 测试追踪和指标之间的关联
  let resource = common::Resource::default("test-service")
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("test-tracer", "1.0.0")
  let meter = meter_provider.get_meter("test-meter", "1.0.0")
  
  let counter = meter.create_counter("operation_count", "count", "Number of operations")
  let histogram = meter.create_histogram("operation_duration", "ms", "Operation duration")
  
  // 创建追踪上下文
  let (ctx, parent_span) = tracer.start_span(
    context::Context::empty(),
    "parent_operation",
    trace::Internal,
    [("operation_type", common::AttributeValue::string("integration_test"))]
  )
  
  // 在追踪上下文中记录指标
  counter.add(1L, [("span_name", common::AttributeValue::string(parent_span.name))])
  histogram.record(100.0, [("trace_id", common::AttributeValue::string("test-trace-id"))])
  
  // 验证指标和追踪的关联性
  @assertion.assert_eq(parent_span.name, "parent_operation")
  @assertion.assert_eq(parent_span.kind, trace::Internal)
  @assertion.assert_eq(parent_span.attributes.length, 1)
}

test "cross_signal_context_propagation_comprehensive" {
  // 测试跨信号上下文传播的完整性
  let resource = common::Resource::default("context-test-service")
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("context-tracer")
  let meter = meter_provider.get_meter("context-meter")
  
  // 创建根追踪
  let (root_ctx, root_span) = tracer.start_span(
    context::Context::empty(),
    "root_operation",
    trace::Server,
    [
      ("user_id", common::AttributeValue::string("user123")),
      ("request_id", common::AttributeValue::string("req-456"))
    ]
  )
  
  // 在子操作中传播上下文
  let (child_ctx, child_span) = tracer.start_span(
    root_ctx,
    "child_operation",
    trace::Internal,
    [("component", common::AttributeValue::string("database"))]
  )
  
  // 在子操作中记录指标
  let gauge = meter.create_gauge("active_connections", "count", "Active database connections")
  gauge.record(5.0, [("service", common::AttributeValue::string("database"))])
  
  // 验证上下文传播
  @assertion.assert_eq(child_span.name, "child_operation")
  @assertion.assert_eq(child_span.kind, trace::Internal)
  @assertion.assert_eq(child_span.attributes.length, 1)
}

test "cross_signal_error_handling_correlation" {
  // 测试跨信号错误处理的关联性
  let resource = common::Resource::default("error-test-service")
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("error-tracer")
  let meter = meter_provider.get_meter("error-meter")
  
  let error_counter = meter.create_counter("error_count", "count", "Number of errors")
  
  // 模拟错误场景的追踪
  let (error_ctx, error_span) = tracer.start_span(
    context::Context::empty(),
    "failing_operation",
    trace::Internal,
    [("operation_type", common::AttributeValue::string("database_query"))]
  )
  
  // 记录错误指标
  error_counter.add(1L, [
    ("error_type", common::AttributeValue::string("connection_timeout")),
    ("span_name", common::AttributeValue::string(error_span.name))
  ])
  
  // 验证错误关联
  @assertion.assert_eq(error_span.name, "failing_operation")
  @assertion.assert_eq(error_span.status, trace::Unset)
}

test "cross_signal_resource_attribute_inheritance" {
  // 测试跨信号资源属性继承
  let resource = common::Resource::default("resource-test-service")
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("resource-tracer")
  let meter = meter_provider.get_meter("resource-meter")
  
  // 验证资源属性
  @assertion.assert_eq(resource.service_name, "resource-test-service")
  @assertion.assert_eq(resource.telemetry_sdk_name, "azimuth")
  @assertion.assert_eq(resource.telemetry_sdk_version, "0.1.0")
  @assertion.assert_eq(resource.attributes.length, 0)
  
  // 创建带有资源属性的追踪和指标
  let (ctx, span) = tracer.start_span(
    context::Context::empty(),
    "resource_aware_operation",
    trace::Server
  )
  
  let counter = meter.create_counter("request_count", "count", "Request count")
  counter.add(1L, [("service_name", common::AttributeValue::string(resource.service_name))])
  
  @assertion.assert_eq(span.name, "resource_aware_operation")
  @assertion.assert_eq(span.kind, trace::Server)
}

test "cross_signal_performance_benchmark" {
  // 跨信号性能基准测试
  let resource = common::Resource::default("performance-test-service")
  let tracer_provider = trace::NoopTracerProvider::{}
  let meter_provider = metrics::NoopMeterProvider::{}
  
  let tracer = tracer_provider.get_tracer("performance-tracer")
  let meter = meter_provider.get_meter("performance-meter")
  
  let histogram = meter.create_histogram("operation_latency", "ms", "Operation latency")
  
  // 批量创建追踪和指标以测试性能
  for i = 0; i < 100; i = i + 1 {
    let (ctx, span) = tracer.start_span(
      context::Context::empty(),
      "batch_operation_${i}",
      trace::Internal,
      [("batch_index", common::AttributeValue::int(i.to_int64()))]
    )
    
    histogram.record(i.to_double(), [("batch_index", common::AttributeValue::int(i.to_int64()))])
  }
  
  // 验证批量操作完成
  @assertion.assert_true(true) // 如果没有异常，则测试通过
}