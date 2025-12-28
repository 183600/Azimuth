// 简化集成测试
// Simplified integration tests for Azimuth telemetry system

test "基础遥测流程集成测试" {
  // 测试基础的遥测流程
  
  // 1. 初始化提供者
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  // 2. 创建仪器
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "integration.test.tracer")
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "integration.test.meter")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "integration.test.logger")
  
  // 3. 创建指标仪器
  let request_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "http.requests.total")
  let response_time = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "http.response.time")
  
  // 4. 开始span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (_, main_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "http.request.processing")
  
  // 5. 记录日志
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "开始处理HTTP请求")
  
  // 6. 更新指标
  @azimuth.telemetry.sdk.metrics.Counter.add(request_counter, 1.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(response_time, 25.5)
  
  // 7. 结束span
  @azimuth.telemetry.api.trace.Span.end(main_span)
}

test "上下文传播集成测试" {
  // 测试上下文传播
  
  let tracer_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(tracer_provider, "propagation.test")
  
  // 创建父span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  let (parent_ctx, parent_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, ctx, "parent.operation")
  
  // 创建子span
  let (_, child_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(tracer, parent_ctx, "child.operation")
  
  // 验证上下文传播
  let parent_span_ctx = @azimuth.telemetry.api.trace.Span.span_context(parent_span)
  let child_span_ctx = @azimuth.telemetry.api.trace.Span.span_context(child_span)
  
  assert_eq(@azimuth.telemetry.api.trace.SpanContext.trace_id(parent_span_ctx), 
           @azimuth.telemetry.api.trace.SpanContext.trace_id(child_span_ctx))
  
  // 结束spans
  @azimuth.telemetry.api.trace.Span.end(child_span)
  @azimuth.telemetry.api.trace.Span.end(parent_span)
}

test "指标和日志集成测试" {
  // 测试指标和日志的集成
  
  let metrics_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let logs_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(metrics_provider, "metrics.test")
  let logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(logs_provider, "logs.test")
  
  // 创建指标
  let counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "operations.total")
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(meter, "operation.duration")
  
  // 记录指标
  @azimuth.telemetry.sdk.metrics.Counter.add(counter, 1.0)
  @azimuth.telemetry.sdk.metrics.Histogram.record(histogram, 42.5)
  
  // 记录日志
  @azimuth.telemetry.sdk.logs.Logger.info(logger, "操作完成")
  @azimuth.telemetry.sdk.logs.Logger.error(logger, "操作失败")
}

test "属性和资源集成测试" {
  // 测试属性和资源的集成
  
  // 创建资源
  let resource = @azimuth.telemetry.api.common.Resource.new()
  let resource_with_attrs = @azimuth.telemetry.api.common.Resource.with_attributes(resource, [
    ("service.name", @azimuth.telemetry.api.common.StringValue("test-service")),
    ("service.version", @azimuth.telemetry.api.common.StringValue("1.0.0"))
  ])
  
  // 创建属性
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  @azimuth.telemetry.api.common.Attributes.set(attrs, "operation", @azimuth.telemetry.api.common.StringValue("test"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "user.id", @azimuth.telemetry.api.common.IntValue(12345))
  
  // 验证属性
  let operation = @azimuth.telemetry.api.common.Attributes.get(attrs, "operation")
  let user_id = @azimuth.telemetry.api.common.Attributes.get(attrs, "user.id")
  
  assert_eq(operation, Some(@azimuth.telemetry.api.common.StringValue("test")))
  assert_eq(user_id, Some(@azimuth.telemetry.api.common.IntValue(12345)))
}