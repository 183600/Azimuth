// 高并发遥测一致性测试
test "高并发遥测一致性测试" {
  // 模拟高并发的Span创建和操作
  let tracer_provider = azimuth::TracerProvider::default()
  let tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "concurrency-test")
  
  let spans = []
  let trace_ids = []
  
  // 创建大量并发Span（模拟并发场景）
  for i = 0; i < 50; i = i + 1 {
    let trace_id = "trace-" + i.to_string()
    let span_id = "span-" + i.to_string()
    
    trace_ids.push(trace_id)
    
    let span_ctx = azimuth::SpanContext::new(trace_id, span_id, true, "")
    let span = azimuth::Span::new("concurrent-operation-" + i.to_string(), azimuth::Internal, span_ctx)
    
    spans.push(span)
  }
  
  // 验证所有Span都被正确创建
  assert_eq(spans.length(), 50)
  assert_eq(trace_ids.length(), 50)
  
  // 验证Trace ID的唯一性
  let unique_trace_ids = []
  for span in spans {
    let span_ctx = azimuth::Span::span_context(span)
    let trace_id = azimuth::SpanContext::trace_id(span_ctx)
    
    if (!unique_trace_ids.contains(trace_id)) {
      unique_trace_ids.push(trace_id)
    }
  }
  
  assert_eq(unique_trace_ids.length(), 50)
}