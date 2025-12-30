// 跨模块集成的端到端测试
// 测试不同遥测模块之间的完整集成流程

test "e2e_metrics_traces_integration" {
  // 测试指标和追踪的端到端集成
  let resource = @azimuth.telemetry.api.common.Resource::default("integration_service")
  
  // 创建meter和tracer
  let meter_provider = @azimuth.telemetry.api.metrics.NoopMeterProvider::{}
  let tracer_provider = @azimuth.telemetry.api.trace.NoopTracerProvider::{}
  
  let meter = meter_provider.get_meter("integration_meter", "1.0.0")
  let tracer = tracer_provider.get_tracer("integration_tracer", "1.0.0")
  
  let ctx = @azimuth.telemetry.api.context.Context::current()
  
  // 开始一个trace span
  let (span_ctx, span) = tracer.start_span(
    ctx,
    "integration_operation",
    @azimuth.telemetry.api.trace.Server,
    [
      ("service.name", @azimuth.telemetry.api.common.AttributeValue::string(resource.service_name)),
      ("operation.type", @azimuth.telemetry.api.common.AttributeValue::string("metrics_traces_integration"))
    ]
  )
  
  // 在span内记录指标
  let counter = meter.create_counter("operations_total", "count", "Total operations")
  
  // 记录操作指标
  counter.add(1L, [
    ("operation.name", @azimuth.telemetry.api.common.AttributeValue::string(span.name)),
    ("service.version", @azimuth.telemetry.api.common.AttributeValue::string(resource.telemetry_sdk_version))
  ])
  
  // 验证集成数据一致性
  assert_eq(span.name, "integration_operation")
  assert_eq(span.attributes.length(), 2)
}