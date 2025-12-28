// 简化配置测试
// Simplified configuration tests for Azimuth telemetry system

test "提供者配置测试" {
  // 测试提供者的基本配置
  
  // 测试默认TracerProvider
  let default_trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let default_tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(default_trace_provider, "default.test")
  assert_true(default_tracer != @azimuth.telemetry.sdk.trace.Tracer::{})
  
  // 测试MeterProvider
  let default_meter_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let default_meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(default_meter_provider, "config.meter")
  assert_true(default_meter != @azimuth.telemetry.sdk.metrics.Meter::{})
  
  // 测试LoggerProvider
  let default_logger_provider = @azimuth.telemetry.sdk.logs.LoggerProvider.default()
  let default_logger = @azimuth.telemetry.sdk.logs.LoggerProvider.get_logger(default_logger_provider, "config.logger")
  assert_true(default_logger != @azimuth.telemetry.sdk.logs.Logger::{})
}

test "仪器配置测试" {
  // 测试仪器的配置
  
  let meter_provider = @azimuth.telemetry.sdk.metrics.MeterProvider.default()
  let meter = @azimuth.telemetry.sdk.metrics.MeterProvider.get_meter(meter_provider, "instrument.config.test")
  
  // 测试Counter配置
  let simple_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(meter, "simple.counter")
  let detailed_counter = @azimuth.telemetry.sdk.metrics.Meter.create_counter(
    meter, 
    "detailed.counter", 
    Some("详细的计数器描述"), 
    Some("operations")
  )
  
  assert_eq(simple_counter.name, "simple.counter")
  assert_eq(simple_counter.description, None)
  assert_eq(simple_counter.unit, None)
  
  assert_eq(detailed_counter.name, "detailed.counter")
  assert_eq(detailed_counter.description, Some("详细的计数器描述"))
  assert_eq(detailed_counter.unit, Some("operations"))
  
  // 测试Histogram配置
  let histogram = @azimuth.telemetry.sdk.metrics.Meter.create_histogram(
    meter, 
    "response.time.histogram", 
    Some("响应时间分布"), 
    Some("ms")
  )
  
  assert_eq(histogram.name, "response.time.histogram")
  assert_eq(histogram.description, Some("响应时间分布"))
  assert_eq(histogram.unit, Some("ms"))
}

test "属性配置测试" {
  // 测试属性配置
  
  // 测试各种属性类型
  let attrs = @azimuth.telemetry.api.common.Attributes.new()
  
  @azimuth.telemetry.api.common.Attributes.set(attrs, "string.attr", @azimuth.telemetry.api.common.StringValue("test_value"))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "int.attr", @azimuth.telemetry.api.common.IntValue(42))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "float.attr", @azimuth.telemetry.api.common.FloatValue(3.14))
  @azimuth.telemetry.api.common.Attributes.set(attrs, "bool.attr", @azimuth.telemetry.api.common.BoolValue(true))
  
  // 验证属性
  let string_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "string.attr")
  let int_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "int.attr")
  let float_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "float.attr")
  let bool_val = @azimuth.telemetry.api.common.Attributes.get(attrs, "bool.attr")
  
  assert_eq(string_val, Some(@azimuth.telemetry.api.common.StringValue("test_value")))
  assert_eq(int_val, Some(@azimuth.telemetry.api.common.IntValue(42)))
  assert_eq(float_val, Some(@azimuth.telemetry.api.common.FloatValue(3.14)))
  assert_eq(bool_val, Some(@azimuth.telemetry.api.common.BoolValue(true)))
}

test "Span配置测试" {
  // 测试Span配置
  
  let trace_provider = @azimuth.telemetry.sdk.trace.TracerProvider.default()
  let tracer = @azimuth.telemetry.sdk.trace.TracerProvider.get_tracer(trace_provider, "span.config.test")
  
  // 创建不同类型的span
  let ctx = @azimuth.telemetry.api.context.Context.root()
  
  let (_, internal_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(
    tracer, ctx, "internal.operation"
  )
  
  let (_, server_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(
    tracer, ctx, "server.operation"
  )
  
  let (_, client_span) = @azimuth.telemetry.sdk.trace.Tracer.start_span(
    tracer, ctx, "client.operation"
  )
  
  // 验证span名称
  assert_eq(@azimuth.telemetry.api.trace.Span.name(internal_span), "internal.operation")
  assert_eq(@azimuth.telemetry.api.trace.Span.name(server_span), "server.operation")
  assert_eq(@azimuth.telemetry.api.trace.Span.name(client_span), "client.operation")
  
  // 结束spans
  @azimuth.telemetry.api.trace.Span.end(internal_span)
  @azimuth.telemetry.api.trace.Span.end(server_span)
  @azimuth.telemetry.api.trace.Span.end(client_span)
}