// Feather.mbt - OpenTelemetry风格的MoonBit Telemetry套件

// 导出所有公共API
pub use telemetry.api.common.{AttributeValue, Attributes, Resource, InstrumentationScope}
pub use telemetry.api.context.{Context, ContextKey, empty, with_value, get}
pub use telemetry.api.trace.{Span, Tracer, TracerProvider, SpanContext, SpanKind, StatusCode, Status, Event, Link}
pub use telemetry.api.propagation.{TextMapCarrier, TextMapPropagator, CompositePropagator, HttpHeadersCarrier}
pub use telemetry.api.propagation.w3c_trace_context.{W3CTraceContextPropagator, get_span_context, set_span_context}

// 导出SDK实现
pub use telemetry.sdk.trace.{SdkTracerProvider, SdkTracer, SpanProcessor, Sampler, SamplingResult, SamplingDecision}
pub use telemetry.sdk.trace.sdk_tracer_provider.{builder as sdk_tracer_provider_builder, AlwaysOnSampler}
pub use telemetry.sdk.trace.processor.{SimpleSpanProcessor, BatchSpanProcessor}
pub use telemetry.sdk.exporter.console.{ConsoleSpanExporter, ExportResult}

// 全局TracerProvider实例
let mut global_tracer_provider : Option[TracerProvider] = None

// 初始化全局TracerProvider
pub fn set_global_tracer_provider(provider : TracerProvider) -> Unit {
  global_tracer_provider = Some(provider)
}

// 获取全局TracerProvider
pub fn get_global_tracer_provider() -> TracerProvider {
  match global_tracer_provider {
    Some(provider) => provider
    None => NoopTracerProvider::new()
  }
}

// 获取全局Tracer
pub fn get_tracer(name : String, version? : String, schema_url? : String) -> Tracer {
  get_global_tracer_provider().get_tracer(name, version, schema_url)
}

// 便捷函数：创建一个基本的TracerProvider并设置为全局
pub fn init_basic_tracing() -> Unit {
  let exporter = ConsoleSpanExporter::new(true)
  let processor = SimpleSpanProcessor::new(exporter)
  let provider = sdk_tracer_provider_builder()
    .with_span_processor(processor)
    .build()
  set_global_tracer_provider(provider)
}

// 便捷函数：创建span并自动管理context
pub fn trace_span[T](
  name : String,
  operation : (@context.Context, Span) -> T,
  context? : @context.Context,
  kind? : SpanKind,
  attributes? : @common.Attributes
) -> T {
  let ctx = context.unwrap_or(@context::empty())
  let tracer = get_tracer("feather.telemetry")
  let (new_ctx, span) = tracer.start_span(name, kind, attributes, [], None, ctx)
  
  try {
    operation(new_ctx, span)
  } finally {
    span.end()
  }
}

// 示例使用函数
pub fn example_tracing() -> Unit {
  // 初始化基本追踪
  init_basic_tracing()
  
  // 创建一个span
  let tracer = get_tracer("example")
  let (ctx, span) = tracer.start_span("main_operation", Some(Server))
  
  // 设置属性
  span.set_attribute("service.name", "example-service")
  span.set_attribute("operation.type", "main")
  
  // 添加事件
  span.add_event("operation_started", [("init.time", @common::AttributeValue::Int64(get_current_time()))])
  
  // 模拟一些工作
  let (inner_ctx, inner_span) = tracer.start_span("inner_operation", Some(Internal), [], [], None, ctx)
  inner_span.set_attribute("work.type", "computation")
  inner_span.end()
  
  // 结束span
  span.set_status(Ok, "Operation completed successfully")
  span.end()
}

// 获取当前时间（占位符实现）
fn get_current_time() -> Int64 {
  // 在实际实现中，这应该使用平台特定的时间函数
  0L
}