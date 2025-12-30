// 链路追踪的并发安全性测试
// 测试多线程环境下的链路追踪安全性和一致性

test "trace_concurrent_span_creation" {
  let tracer_provider = @azimuth.telemetry.api.trace.NoopTracerProvider::{}
  let tracer = tracer_provider.get_tracer("test_tracer", "1.0.0")
  
  // 模拟并发创建多个span
  let num_spans = 10
  let spans = Array::range_with(0, num_spans, 1).map(fn(i) {
    let ctx = @azimuth.telemetry.api.context.Context::current()
    let (new_ctx, span) = tracer.start_span(
      ctx,
      "concurrent_span_" + i.to_string(),
      @azimuth.telemetry.api.trace.Server,
      [("iteration", @azimuth.telemetry.api.common.AttributeValue::int(i.to_int64()))]
    )
    span
  })
  
  // 验证所有span都被正确创建
  assert_eq(spans.length(), num_spans)
  
  // 验证每个span的唯一性
  let span_names = spans.map(fn(span) { span.name })
  let unique_names = span_names.to_set()
  assert_eq(unique_names.size(), num_spans)
}